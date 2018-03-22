#include "vm.h"
#include <cstring>
#include <cstdio>
#include <cctype>
#include <cstdlib>
#include <vector>
#include <cassert>
#include <map>
#include <string>

void *alloc(VM *vm, size_t size) {
    return malloc(size);
}

using std::vector;
using std::map;

enum TokenType {
    T_DOUBLE,
    T_SYMBOL,
    T_LEFT_BRACKET,
    T_RIGHT_BRACKET,
    T_LEFT_CURLY,
    T_RIGHT_CURLY,
    T_LEFT_PAREN,
    T_RIGHT_PAREN,
    T_QUOTE,
    T_QUASI_QUOTE,
    T_UNQUOTE,
    T_UNQUOTE_SPLICE,
    T_BOOLEAN,
    T_STRING,
    T_EOF,
    T_ERROR
};

struct Token {
    size_t start;
    size_t length;
    TokenType type;
    union {
        double d;
        bool b;
        char *str;
        struct {
            char *symstr;
            int symid;
        };
    };
};

struct LexState {
    char *prog;
    size_t pos;
    Token nextToken;
};

void eatWhiteSpace(LexState *state) {
    while ((isspace(state->prog[state->pos]) ||
            state->prog[state->pos] == ';') &&
           state->prog[state->pos]) {
        if (state->prog[state->pos] == ';') {
            state->pos += strcspn(state->prog+state->pos, "\n");
        } else {
            state->pos++;
        }
    }
}

bool isFloatStartChar(int c) {
    return isdigit(c) || c == '.';
}

map<std::string, char *> stringPool;
int symbolIdTop;
map<void *, int> symbolIds;

Token peekToken(LexState *state) {
    return state->nextToken;
}

Token nextToken(LexState *state) {
    eatWhiteSpace(state);
    size_t tokStart = state->pos;
    const char *tokenDelimiters = ")( \n'`,";
    size_t tokLength = strcspn(state->prog+state->pos,
                               tokenDelimiters);
    if (strchr(tokenDelimiters, state->prog[tokStart])) {
        tokLength += 1;
    }
    state->pos += tokLength;

    Token tok{tokStart, tokLength, T_ERROR};

    const char *t = "true";
    const char *f = "false";

    if (state->prog[tokStart] == 0) {
        tok.type = T_EOF;
    } else if (isFloatStartChar(state->prog[tokStart])) {
        // positive floats
        char *end = 0;
        double res = strtod(state->prog+tokStart, &end);
        if (end == state->prog+state->pos) {
            tok.type = T_DOUBLE;
            tok.d = res;
        }
    } else if (state->prog[tokStart] == '-' &&
               tokLength > 1 &&
               isFloatStartChar(state->prog[tokStart+1])) {
        // negative floats
        char *end = 0;
        double res = strtod(state->prog+tokStart, &end);
        if (end == state->prog+state->pos) {
            tok.type = T_DOUBLE;
            tok.d = res;
        }
    } else if (state->prog[tokStart] == ',') {
        if (state->prog[tokStart+1] == '@') {
            tokLength++;
            state->pos++;
            tok.length++;
            tok.type = T_UNQUOTE_SPLICE;
        } else {
            tok.type = T_UNQUOTE;
        }
    } else if (state->prog[tokStart] == '`') {
        tok.type = T_QUASI_QUOTE;
    } else if (state->prog[tokStart] == '\'') {
        tok.type = T_QUOTE;
    } else if (state->prog[tokStart] == '(') {
        tok.type = T_LEFT_PAREN;
    } else if (state->prog[tokStart] == ')') {
        tok.type = T_RIGHT_PAREN;
    } else if (state->prog[tokStart] == '[') {
        tok.type = T_LEFT_BRACKET;
    } else if (state->prog[tokStart] == ']') {
        tok.type = T_RIGHT_BRACKET;
    } else if (state->prog[tokStart] == '{') {
        tok.type = T_LEFT_CURLY;
    } else if (state->prog[tokStart] == '}') {
        tok.type = T_RIGHT_CURLY;
    } else if (tokLength == strlen(t) &&
               !memcmp(state->prog+tokStart, t, tokLength)) {
        // True
        tok.type = T_BOOLEAN;
        tok.b = true;
    } else if (tokLength == strlen(f) &&
               !memcmp(state->prog+tokStart, f, tokLength)) {
        // False
        tok.type = T_BOOLEAN;
        tok.b = false;
    } else {
        // symbols
        tok.type = T_SYMBOL;
        char temp_str[100];
        assert(tokLength < 99);
        memcpy(temp_str, state->prog+tokStart, tokLength);
        temp_str[tokLength] = 0;
        std::string s(temp_str);
        map<std::string, char *>::iterator it = stringPool.find(s);
        if (it != stringPool.end()) {
            tok.symstr = stringPool[s];
            tok.symid = symbolIds[tok.symstr];
        } else {
            tok.symstr = strdup(temp_str);
            tok.symid = symbolIdTop;
            symbolIdTop++;
            stringPool[s] = tok.symstr;
            symbolIds[tok.symstr] = tok.symid;
        }
    }
    Token ret = state->nextToken;
    state->nextToken = tok;
    return ret;
}

template <typename T>
void add(DynamicArray<T> *a, T v) {
    a->v.push_back(v);
}

template <typename T>
T pop(DynamicArray<T> *a) {
    T ret = a->v[size(a)-1];
    a->v.pop_back();
    return ret;
}

template <typename T>
void resize(DynamicArray<T> *a, size_t newSize) {
    a->v.resize(newSize);
}

template <typename T>
size_t size(DynamicArray<T> *a) {
    return a->v.size();
}

struct ParseState {
    LexState lexer;
    Value root;
};

ParseState initParseState(char *prog) {
    ParseState ret{};
    ret.lexer.prog = prog;
    nextToken(&ret.lexer);
    if (peekToken(&ret.lexer).type == T_EOF) {
        fprintf(stderr, "ERROR?: Empty 'files' are not legal\n");
        assert(false);
    }
    ret.root.type = V_CONS_PAIR;
    return ret;
}


void parseList(VM *vm, ParseState *ps, Value *parent) {

#define allocPair(vm, parent)                                   \
    do {                                                        \
        parent->pair = (ConsPair *)alloc(vm, sizeof(ConsPair)); \
        parent->pair->cdr.type = V_CONS_PAIR;                   \
        parent->pair->cdr.pair = 0;                             \
    } while(false)

    Token tok = nextToken(&ps->lexer);
    while (tok.type != T_RIGHT_PAREN && tok.type != T_EOF) {
        assert(parent->type == V_CONS_PAIR);
        assert(parent->pair == 0);
        switch (tok.type) {
            case T_DOUBLE: {
                allocPair(vm, parent);
                parent->pair->car.type = V_DOUBLE;
                parent->pair->car.doub = tok.d;
            } break;
            case T_BOOLEAN: {
                allocPair(vm, parent);
                parent->pair->car.type = V_BOOLEAN;
                parent->pair->car.boolean = tok.b;
            } break;
            case T_SYMBOL: {
                allocPair(vm, parent);
                parent->pair->car.type = V_SYMBOL;
                parent->pair->car.symstr = tok.symstr;
                parent->pair->car.symid = tok.symid;
            } break;
            case T_LEFT_PAREN: {
                if (peekToken(&ps->lexer).type != T_RIGHT_PAREN) {
                    allocPair(vm, parent);
                    parent->pair->car.type = V_CONS_PAIR;
                    parseList(vm, ps, &parent->pair->car);
                }
            } break;
            default: {
                fprintf(stderr, "ERROR: %d not recognized yet\n", tok.type);
                assert(false);
            } break;
        }
        tok = nextToken(&ps->lexer);
        if (tok.type != T_RIGHT_PAREN && tok.type != T_EOF) {
            parent->pair->cdr.pair = (ConsPair *)alloc(vm, sizeof(ConsPair));
            parent = &parent->pair->cdr;
            parent->type = V_CONS_PAIR;
            parent->pair = 0;
        }
    }
}

void parse(VM *vm, ParseState *ps) {
    parseList(vm, ps, &ps->root);
}

void add(Object *o, int symid, Value v) {
    o->table[symid] = v;
}

Value &get(Object *o, int symid) {
    return o->table[symid];
}

bool keyExists(Object *o, int symid) {
    return o->table.count(symid);
}

bool operator<(const Value a, const Value b) {
    if (a.type != b.type) {
        return a.type < b.type;
    }
    switch (a.type) {
        case V_FUNCTION: {
            return a.funcID < b.funcID;
        } break;
        case V_SYMBOL: {
            return a.symid < b.symid;
        } break;
        case V_DOUBLE: {
            return a.doub < b.doub;
        } break;
        case V_BOOLEAN: {
            return !a.boolean && b.boolean;
        } break;
        default:assert(false);
    }
}

size_t allocReg(Function *func) {
    size_t reg = func->numRegs;
    func->numRegs++;
    return reg;
}

size_t allocConstant(Function *func, Value val) {
    size_t k = size(&func->constants);
    add(&func->constants, val);
    return k;
}

#define bitsize(o) (sizeof(o)*8)

void addRRR(Function *func, OpCode op, uint8 a, uint8 b, uint8 c) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(a)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*2)) |
                                  (((OpCode)(b)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*3)) |
                                  (((OpCode)(c)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*4)));
    add(&func->code, assembledOp);
}

void addRI(Function *func, OpCode op, uint8 reg, uint32 immediateID) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(reg)) << (bitsize(OpCode) -
                                                       bitsize(uint8)*2)) |
                                  immediateID);
    add(&func->code, assembledOp);
}

void addRR(Function *func, OpCode op, uint8 a, uint8 b) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(a)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*2)) |
                                  (((OpCode)(b)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*3)));
    add(&func->code, assembledOp);
}

void addR(Function *func, OpCode op, uint8 reg) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(reg)) << (bitsize(OpCode) -
                                                       bitsize(uint8)*2)));
    add(&func->code, assembledOp);
}

void addN(Function *func, OpCode op) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))));
    add(&func->code, assembledOp);
}

// Do i really need this?
enum ASTNodeType {
    ASTT_BODY,
    ASTT_SYMBOL,
    ASTT_VARIABLE,
    ASTT_ARGLIST,
    ASTT_LAMBDA,
    ASTT_CALL,
    ASTT_DOUBLE,
    ASTT_BOOLEAN,
    ASTT_MAKE_OBJECT,
    ASTT_DEFINE,
};

struct ASTNode {
    ASTNodeType type;
    bool hasReg;
    ASTNode(ASTNodeType t, bool b) : type(t), hasReg(b) {}
    virtual void traverse() = 0;
    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) = 0;
    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) = 0;
    //virtual size_t getConstantId(VM *vm, Function *func,
    //map<int, size_t> *symToReg) = 0;
};

struct ASTBody : ASTNode {
    ASTBody() : ASTNode(ASTT_BODY, false) {}
        
    DynamicArray<ASTNode *> body;
    virtual void traverse() {
        for (size_t i = 0; i < size(&body); ++i) {
            body[i]->traverse();
        }
    }

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        for (size_t i = 0; i < size(&body); ++i) {
            body[i]->emit(vm, funcID, symToReg, valueToConstantSlot);
        }
        if (body[size(&body)-1]->hasReg) {
            size_t retReg = body[size(&body)-1]->getRegister(vm, funcID,
                                                             symToReg,
                                                             valueToConstantSlot);
            addR(&vm->funcs[funcID], OP_RETURN, retReg);
        } else {
            addN(&vm->funcs[funcID], OP_RETURN_UNDEF);
        }
    };

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        fprintf(stderr, "ICE: body does not have a register\n");
        assert(false);
    }
};

struct ASTSymbol : ASTNode {
    ASTSymbol() : ASTNode(ASTT_SYMBOL, true) {}
    char *str;
    int symid;
    size_t reg;
    virtual void traverse() {
        printf("%s %d\n", str, symid);
    };

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        Value v{V_SYMBOL};
        v.symid = symid;
        v.symstr = str;
        size_t k;
        if (valueToConstantSlot->count(v)) {
            k = valueToConstantSlot->at(v);
        } else {
            k = allocConstant(&vm->funcs[funcID], v);
            (*valueToConstantSlot)[v] = k;
        }
        reg = allocReg(&vm->funcs[funcID]);
        addRI(&vm->funcs[funcID], OP_LOADK, reg, k);
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        return reg;
    }
};

// Hmmmm since we have a register machine a-normal form
// would be *very* good, or until then an enum would work.
struct ASTVariable : ASTNode {
    ASTVariable() : ASTNode(ASTT_VARIABLE, true) {}
    ASTSymbol symbol;
    size_t reg;
    virtual void traverse() {
        printf("%s %d\n", symbol.str, symbol.symid);
    };

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        // NOOP
    }

    // Only (let var val) and arguments
    virtual void setRegister(VM *vm, size_t funcID,
                             map<int, size_t> *symToReg, size_t sreg) {
        assert(!symToReg->count(symbol.symid));
        (*symToReg)[symbol.symid] = sreg;
        reg = sreg;
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        assert(symToReg->count(symbol.symid));
        return reg;
    }
};

struct ASTArgList : ASTNode {
    ASTArgList() : ASTNode(ASTT_ARGLIST, true) {}
    DynamicArray<ASTVariable> args;
    virtual void traverse() {
        printf("ArgList (\n");
        for (size_t i = 0; i < size(&args); ++i) {
            args[i].traverse();
        }
        printf(")\n");
    };

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        for (size_t i = 0; i < size(&args); ++i) {
            args[i].setRegister(vm, funcID, symToReg,
                                allocReg(&vm->funcs[funcID]));
        }
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        fprintf(stderr, "ICE: argument list does not have a register\n");
        assert(false);
    }
};

struct ASTLambda : ASTNode {
    ASTLambda() : ASTNode(ASTT_LAMBDA, true) {}
    ASTArgList argList;
    ASTBody body;
    size_t reg;
    virtual void traverse() {
        printf("Lambda (\n");
        argList.traverse();
        body.traverse();
        printf(")\n");
    };

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        // create new func and stuffs, add the new func to the
        // constant table(?), load the constant.

        size_t newFuncID = size(&vm->funcs);
        add(&vm->funcs, Function());
        map<int, size_t> newSymToReg;
        map<Value, size_t> newValueToConstantSlot;
        argList.emit(vm, newFuncID, &newSymToReg,
                     &newValueToConstantSlot);
        body.emit(vm, newFuncID, &newSymToReg, &newValueToConstantSlot);
        reg = allocReg(&vm->funcs[funcID]);
        addRI(&vm->funcs[funcID], OP_LOAD_FUNC, reg, newFuncID);
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        return reg;
    }
};

struct ASTCall : ASTNode {
    ASTCall() : ASTNode(ASTT_CALL, true) {}
    ASTNode *callee;
    DynamicArray<ASTNode *> args;
    size_t returnReg;
    virtual void traverse() {
        printf("Call (\n");
        callee->traverse();
        for (size_t i = 0; i < size(&args); ++i) {
            args[i]->traverse();
        }
        printf(")\n");
    }

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        callee->emit(vm, funcID, symToReg, valueToConstantSlot);
        size_t calleeReg = callee->getRegister(vm, funcID,
                                               symToReg,
                                               valueToConstantSlot);
        DynamicArray<size_t> argRegs;
        for (size_t i = 0; i < size(&args); ++i) {
            args[i]->emit(vm, funcID, symToReg, valueToConstantSlot);
            add(&argRegs, args[i]->getRegister(vm, funcID,
                                               symToReg,
                                               valueToConstantSlot));
        }
        addR(&vm->funcs[funcID], OP_SETUP_CALL, calleeReg);
        for (size_t i = 0; i < size(&argRegs); ++i) {
            addR(&vm->funcs[funcID], OP_PUSH_ARG, argRegs[i]);
        }
        returnReg = allocReg(&vm->funcs[funcID]);
        addR(&vm->funcs[funcID], OP_CALL, returnReg);
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        return returnReg;
    }

};

struct ASTDouble : ASTNode {
    ASTDouble() : ASTNode(ASTT_DOUBLE, true) {}
    double value;
    size_t reg;
    virtual void traverse() {
        printf("%f\n", value);
    }

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        Value v{V_DOUBLE};
        v.doub = value;
        size_t k;
        if (valueToConstantSlot->count(v)) {
            k = valueToConstantSlot->at(v);
        } else {
            k = allocConstant(&vm->funcs[funcID], v);
            (*valueToConstantSlot)[v] = k;
        }
        reg = allocReg(&vm->funcs[funcID]);
        addRI(&vm->funcs[funcID], OP_LOADK, reg, k);
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        return reg;
    }
};

struct ASTBoolean : ASTNode {
    ASTBoolean() : ASTNode(ASTT_BOOLEAN, true) {}
    bool value;
    size_t reg;
    virtual void traverse() {
        printf("%s\n", value ? "true" : "false");
    }

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        Value v{V_BOOLEAN};
        v.boolean = value;
        size_t k;
        if (valueToConstantSlot->count(v)) {
            k = valueToConstantSlot->at(v);
        } else {
            k = allocConstant(&vm->funcs[funcID], v);
            (*valueToConstantSlot)[v] = k;
        }
        reg = allocReg(&vm->funcs[funcID]);
        addRI(&vm->funcs[funcID], OP_LOADK, reg, k);
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        return reg;
    }
};

struct MakeObjectSlot {
    ASTNode *key;
    ASTNode *value;
};

struct ASTMakeObject : ASTNode {
    ASTMakeObject() : ASTNode(ASTT_MAKE_OBJECT, true) {}
    DynamicArray<MakeObjectSlot> slots;
    size_t reg;
    virtual void traverse() {
        printf("(make-object\n(\n");
        for (size_t i = 0; i < size(&slots); ++i) {
            printf("(\n");
            slots[i].key->traverse();
            slots[i].value->traverse();
            printf(")\n");
        }
        printf(")\n)\n");
    }

    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        reg = allocReg(&vm->funcs[funcID]);
        addR(&vm->funcs[funcID], OP_MAKE_OBJECT, reg);
        for (size_t i = 0; i < size(&slots); ++i) {
            slots[i].key->emit(vm, funcID, symToReg, valueToConstantSlot);
            slots[i].value->emit(vm, funcID, symToReg,
                                 valueToConstantSlot);
            addRRR(&vm->funcs[funcID], OP_SET_OBJECT_SLOT, reg,
                   slots[i].key->getRegister(vm, funcID,
                                             symToReg,
                                             valueToConstantSlot),
                   slots[i].value->getRegister(vm, funcID,
                                               symToReg,
                                               valueToConstantSlot));
        }
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        return reg;
    }
};

struct ASTDefine : ASTNode {
    ASTSymbol var;
    ASTNode *expr;
    ASTDefine() : ASTNode(ASTT_DEFINE, false) {}
    virtual void traverse() {
        printf("(define\n");
        var.traverse();
        expr->traverse();
        printf(")\n");
    }
    virtual void emit(VM *vm, size_t funcID,
                      map<int, size_t> *symToReg,
                      map<Value, size_t> *valueToConstantSlot) {
        expr->emit(vm, funcID, symToReg, valueToConstantSlot);
        Value v{V_SYMBOL};
        v.symid = var.symid;
        v.symstr = var.str;
        size_t k;
        if (valueToConstantSlot->count(v)) {
            k = valueToConstantSlot->at(v);
        } else {
            k = allocConstant(&vm->funcs[funcID], v);
            (*valueToConstantSlot)[v] = k;
        }
        addRI(&vm->funcs[funcID], OP_DEFINE_GLOBAL,
              expr->getRegister(vm, funcID, symToReg,
                                valueToConstantSlot), k);
    }

    virtual size_t getRegister(VM *vm, size_t funcID,
                               map<int, size_t> *symToReg,
                               map<Value, size_t> *valueToConstantSlot) {
        fprintf(stderr, "ERROR: define can't be used as an expression\n");
        assert(false);
    }
};

ASTSymbol symbolToAST(Value tree) {
    assert(tree.type == V_SYMBOL);
    ASTSymbol ret;
    ret.str = tree.symstr;
    ret.symid = tree.symid;
    return ret;
}

ASTVariable variableToAST(Value tree) {
    assert(tree.type == V_SYMBOL);
    ASTVariable ret;
    ret.symbol = symbolToAST(tree);
    return ret;
}


ASTDouble doubleToAST(Value tree) {
    assert(tree.type == V_DOUBLE);
    ASTDouble ret;
    ret.value = tree.doub;
    return ret;
}

ASTBoolean booleanToAST(Value tree) {
    assert(tree.type == V_BOOLEAN);
    ASTBoolean ret;
    ret.value = tree.boolean;
    return ret;
}

ASTNode *exprToAST(Value tree);

size_t length(Value v) {
    assert(v.type == V_CONS_PAIR);
    size_t ret = 0;
    while (v.pair) {
        assert(v.type == V_CONS_PAIR);
        v = v.pair->cdr;
        ret++;
    }
    return ret;
}

ASTCall callToAST(Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTCall ret;
    if (length(tree) < 1) {
        fprintf(stderr, "ERROR: calling to nothing does not work!\n");
        assert(false);
    }
    ret.callee = exprToAST(tree.pair->car);
    assert(tree.pair->cdr.type == V_CONS_PAIR);
    tree = tree.pair->cdr;
    while (tree.pair) {
        add(&ret.args, exprToAST(tree.pair->car));
    }
    return ret;
}

ASTArgList argListToAST(Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTArgList ret;

    while (tree.pair) {
        if (tree.pair->car.type != V_SYMBOL) {
            fprintf(stderr, "ERROR: Arguments has to be symbols\n");
            assert(false);
        }
        add(&ret.args, variableToAST(tree.pair->car));
        tree = tree.pair->cdr;
    }
    return ret;
}

ASTBody bodyToAST(Value tree);

#define parseTreeSymCmp(tree, str) (!strcmp((tree).symstr, str))

#define firstInListIsType(tree, p_type) (((tree).pair &&               \
                                          (tree).pair->car.type ==     \
                                          (p_type)))


ASTLambda lambdaToAST(Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTLambda ret;
    if (length(tree) < 3) {
        fprintf(stderr, "ERROR: lambda needs both an argument list and a body!\n");
        assert(false);
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(parseTreeSymCmp(tree.pair->car, "lambda"));
    Value afterLambdaSym = tree.pair->cdr;
    ret.argList = argListToAST(afterLambdaSym.pair->car);
    ret.body = bodyToAST(afterLambdaSym.pair->cdr);
    return ret;
}

ASTNode *quotedToAST(Value tree) {
    ASTNode *ret = 0;
    switch (tree.type) {
        case V_DOUBLE: {
            ASTDouble *c = new ASTDouble;
            *c = doubleToAST(tree);
            ret = c;
        } break;
        case V_BOOLEAN: {
            ASTBoolean *c = new ASTBoolean;
            *c = booleanToAST(tree);
            ret = c;
        } break;
        case V_SYMBOL: {
            ASTSymbol *c = new ASTSymbol;
            *c = symbolToAST(tree);
            ret = c;
        } break;
        default: {
            fprintf(stderr, "ERROR: %d cant be quoted yet\n", tree.type);
            assert(false);
        } break;
    }
    return ret;
}

MakeObjectSlot makeObjectSlot(Value tree) {
    MakeObjectSlot ret;
    if (tree.type != V_CONS_PAIR || length(tree) != 2) {
        fprintf(stderr, "ERROR: make-object's argument has to be an associative list\n");
        assert(false);
    }
    ret.key = exprToAST(tree.pair->car);
    ret.value = exprToAST(tree.pair->cdr.pair->car);
    return ret;
}

ASTMakeObject makeObjectToAST(Value tree) {
    ASTMakeObject ret;
    if (tree.type != V_CONS_PAIR) {
        fprintf(stderr, "ERROR: make-object's argument has to be an associative list\n");
        assert(false);
    }
    resize(&ret.slots, length(tree));
    for (size_t i = 0; tree.pair; ++i, tree = tree.pair->cdr) {
        ret.slots[i] = makeObjectSlot(tree.pair->car);
    }
    return ret;
}

ASTDefine defineToAST(Value tree) {
    ASTDefine ret;
    assert(tree.type == V_CONS_PAIR);
    assert(tree.pair);
    assert(tree.pair->cdr.type == V_CONS_PAIR);
    assert(tree.pair->cdr.pair);
    if (tree.pair->car.type != V_SYMBOL) {
        fprintf(stderr, "ERROR: Can only define symbols as variables\n");
        assert(false);
    }
    ret.var = symbolToAST(tree.pair->car);
    ret.expr = exprToAST(tree.pair->cdr.pair->car);
    return ret;
}

ASTNode *exprToAST(Value tree) {
    ASTNode *ret = 0;
    switch (tree.type) {
        case V_DOUBLE: {
            ASTDouble *c = new ASTDouble;
            *c = doubleToAST(tree);
            ret = c;
        } break;
        case V_BOOLEAN: {
            ASTBoolean *c = new ASTBoolean;
            *c = booleanToAST(tree);
            ret = c;
        } break;
        case V_SYMBOL: {
            ASTVariable *c = new ASTVariable;
            *c = variableToAST(tree);
            ret = c;
        } break;
        case V_CONS_PAIR: {
            if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "lambda")) {
                ASTLambda *c = new ASTLambda;
                *c = lambdaToAST(tree);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "define")) {
                if (length(tree) != 3) {
                    fprintf(stderr, "ERROR: define takes three arguments!\n");
                    assert(false);
                }
                ASTDefine *c = new ASTDefine;
                *c = defineToAST(tree.pair->cdr);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "quote")) {
                if (length(tree) != 2) {
                    fprintf(stderr, "ERROR: quote takes only one argument!\n");
                    assert(false);
                }
                ret = quotedToAST(at(tree, 1));
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car,
                                       "make-object")) {
                if (length(tree) != 2) {
                    fprintf(stderr, "ERROR: make-object takes only one argument!\n");
                    assert(false);
                }
                ASTMakeObject *c = new ASTMakeObject;
                *c = makeObjectToAST(at(tree, 1));
                ret = c;
            } else {
                ASTCall *c = new ASTCall;
                *c = callToAST(tree);
                ret = c;
            }
        } break;
        default: {
            fprintf(stderr, "ERROR: %d cant be converted to ast yet\n", tree.type);
            assert(false);
        } break;
    }
    return ret;
}

ASTBody bodyToAST(Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTBody ret;
    while (tree.pair) {
        assert(tree.type == V_CONS_PAIR);
        add(&ret.body, exprToAST(tree.pair->car));
        tree = tree.pair->cdr;
    }
    return ret;
}

// TODO: symid to symbol string
void printValue(Value v) {
    switch(v.type) {
        case V_UNDEF: {
            printf("<undef>");
        } break;
        case V_OPAQUE_POINTER: {
            printf("<opaque: %p type: %llu>", v.opaque, v.opaqueType);
        } break;
        case V_FUNCTION: {
            printf("<func: %lx>", v.funcID);
        } break;
        case V_OBJECT: {
            printf("<object: %p>", v.object);
        } break;
        case V_CFUNCTION: {
            assert(false);
        } break;
        case V_BOOLEAN: {
            printf("%s", v.boolean ? "true" : "false");
        } break;
        case V_DOUBLE: {
            printf("%f", v.doub);
        } break;
        case V_SYMBOL: {
            printf("%s", v.symstr);
        } break;
        case V_CONS_PAIR: {
            printf("(");
            bool isFirst = true;
            while(v.pair) {
                if (!isFirst) {
                    printf(" ");
                } else {
                    isFirst = false;
                }
                printValue(v.pair->car);
                if (v.pair->cdr.type == V_CONS_PAIR) {
                    v = v.pair->cdr;
                } else {
                    printf(" . ");
                    printValue(v.pair->cdr);
                    break;
                }
            }
            printf(")");
        } break;
    }
}

ASTBody buildAST(VM *vm, char *prog) {
    ParseState ps = initParseState(prog);
    parse(vm, &ps);
    //printParseTree(&ps.root, ps.lexer.prog, 0);
    printValue(ps.root);
    printf("\n");
    ASTBody body = bodyToAST(ps.root);
    return body;
}

#define getOp(undecoded) ((OpCode)((undecoded) >>                       \
                                   (bitsize(OpCode) - bitsize(uint8))))

#define getRegA(undecoded) ((0x00FF000000000000 & (undecoded)) >>       \
                            (bitsize(OpCode) - bitsize(uint8)*2))

#define getRegB(undecoded) ((0x0000FF0000000000 & (undecoded)) >>       \
                            (bitsize(OpCode) - bitsize(uint8)*3))

#define getRegC(undecoded) ((0x000000FF00000000 & (undecoded)) >>       \
                            (bitsize(OpCode) - bitsize(uint8)*4))

#define getImm(undecoded) (0x00000000FFFFFFFF & (undecoded))

void printOpCode(OpCode undecoded) {
    OpCode op = getOp(undecoded);
    printf("%s ", opCodeStr[op]);
    switch (opCodeTypes[op]) {
        case OT_N: {
            printf("\n");
        } break;
        case OT_R: {
            uint8 reg = getRegA(undecoded);
            printf("r%x\n", (uint32)reg);
        } break;
        case OT_RR: {
            uint8 a = getRegA(undecoded);
            uint8 b = getRegB(undecoded);
            printf("r%x r%x\n", (uint32)a, (uint32)b);
        } break;
        case OT_RRR: {
            uint8 a = getRegA(undecoded);
            uint8 b = getRegB(undecoded);
            uint8 c = getRegC(undecoded);
            printf("r%x r%x r%x\n", (uint32)a, (uint32)b, (uint32)c);
        } break;
        case OT_RI: {
            uint8 reg = getRegA(undecoded);
            uint32 immId = getImm(undecoded);
            printf("r%x i%x\n", (uint32)reg, immId);
        } break;
    }
}

void printFuncCode(Function *func) {
    printf("CONSTANT TABLE:\n");
    for (size_t i = 0; i < size(&func->constants); ++i) {
        printf("i%lx ", i);
        printValue(func->constants[i]);
        printf("\n");
    }
    printf("CODE:\n");
    for (size_t i = 0; i < size(&func->code); ++i) {
        printOpCode(func->code[i]);
    }
}

void compileString(VM *vm, char *prog) {
    ASTBody body = buildAST(vm, prog);
    body.traverse();
    map<int, size_t> symToReg;
    map<Value, size_t> valueToConstantSlot;
    size_t funcId = size(&vm->funcs);
    add(&vm->funcs, Function());
    body.emit(vm, funcId, &symToReg, &valueToConstantSlot);
}

void initFrame(VM *vm, ActivationFrame *frame, Function *func) {
    *frame = ActivationFrame();
    frame->func = func; // Might this break?? or not??
    resize(&frame->registers, frame->func->numRegs);
}


void initFrame(VM *vm, ActivationFrame *frame, size_t funcID) {
    initFrame(vm, frame, &vm->funcs[funcID]);
}

Object *allocObject(VM *vm) {
    Object *ret = (Object *)alloc(vm, sizeof(Object));
    Object temp;
    memcpy(ret, &temp, sizeof(Object));
    *ret = temp;
    return ret;
}

Value runFunc(VM *vm, ActivationFrame *frame) {
    OpCode undecoded;
 loop:
    undecoded = frame->func->code[frame->pc];
    OpCode instr = getOp(undecoded);
    switch(instr) {
        case OP_LOADK: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            frame->registers[reg] = frame->func->constants[k];
            frame->pc++;
        } break;
        case OP_SETUP_CALL: {
            goto call;
        } break;
        case OP_RETURN: {
            uint8 reg = getRegA(undecoded);
            if (frame->caller) {
                frame->caller->registers[frame->retReg] =
                    frame->registers[reg];
                frame = frame->caller;
            } else {
                return frame->registers[reg];
            }
        } break;
        case OP_RETURN_UNDEF: {
            if (frame->caller) {
                frame->caller->registers[frame->retReg] = Value{V_UNDEF};
                frame = frame->caller;
            } else {
                return Value{V_UNDEF};
            }
        } break;
        case OP_LOAD_FUNC: {
            uint8 reg = getRegA(undecoded);
            uint32 funcID = getImm(undecoded);
            Value func = {V_FUNCTION};
            func.funcID = funcID;
            frame->registers[reg] = func;
            frame->pc++;
        } break;
        case OP_PUSH_ARG: {
            fprintf(stderr, "ICE: stray PUSH_ARG at %lu\n", frame->pc);
            assert(false);
        } break;
        case OP_CALL: {
            fprintf(stderr, "ICE: stray CALL at %lu\n", frame->pc);
            assert(false);
        } break;
        case OP_MAKE_OBJECT: {
            uint8 reg = getRegA(undecoded);
            Value o{V_OBJECT};
            o.object = allocObject(vm);
            frame->registers[reg] = o;
            frame->pc++;
        } break;
        case OP_SET_OBJECT_SLOT: {
            uint8 objReg = getRegA(undecoded);
            uint8 keyReg = getRegB(undecoded);
            uint8 valReg = getRegC(undecoded);
            Value obj = frame->registers[objReg];
            if (obj.type != V_OBJECT) {
                fprintf(stderr, "ERROR: Can only set slot on object\n");
                assert(false);
            }
            Value key = frame->registers[keyReg];
            if (key.type != V_SYMBOL) {
                fprintf(stderr, "ERROR: Object key can only be a symbol (so far)\n");
                assert(false);
            }
            Value val = frame->registers[valReg];
            add(obj.object, key.symid, val);
            frame->pc++;
        } break;
        case OP_DEFINE_GLOBAL: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            if (frame->func->constants[k].type != V_SYMBOL) {
                fprintf(stderr, "ICE: DEFINE_GLOBAL imm not a symbol\n");
                assert(false);
            }

            add(&vm->globals, frame->func->constants[k].symid, 
                frame->registers[reg]); 
            frame->pc++;
        } break;
        default: {
            printf("OP: ");
            printOpCode(undecoded);
            printf("is not yet implemented\n");
            assert(false);
        } break;
    }
    goto loop;
    {
    call:
        uint8 calleeReg = getRegA(undecoded);
        Value callee = frame->registers[calleeReg];
        assert(callee.type == V_FUNCTION);
        frame->pc++;
        ActivationFrame *calleeFrame = (ActivationFrame *)alloc(vm,
                                                                sizeof(ActivationFrame));
        initFrame(vm, calleeFrame, callee.funcID);
        calleeFrame->caller = frame;
        uint8 dstReg = 0;
    pushArgLoop:
        undecoded = frame->func->code[frame->pc];
        instr = getOp(undecoded);
        switch(instr) {
            case OP_PUSH_ARG: {
                uint8 srcReg = getRegA(undecoded);
                calleeFrame->registers[dstReg] =
                    frame->registers[srcReg];
                dstReg++;
                frame->pc++;
            } break;
            case OP_CALL: {
                frame->pc++;
                uint8 retReg = getRegA(undecoded);
                calleeFrame->retReg = retReg;
                frame = calleeFrame;
                goto loop;
            } break;
            default: {
                fprintf(stderr,
                        "ICE: Illegal %s between SETUP_CALL and CALL\n",
                        opCodeStr[instr]);
                assert(false);
            } break;
        }
        goto pushArgLoop;
    } 
    {
#if 0
    ccall:
        // TODO: similar to call, but build the apiStack then call.
#endif
    }
}


Value runFunc(VM *vm, size_t funcID) {
    ActivationFrame frame;
    initFrame(vm, &frame, funcID);
    return runFunc(vm, &frame);
}

void pushValue(VM *vm, Value v) {
    add(&vm->apiStack, v);
}

void call(VM *vm, uint8 numArgs) {
    ActivationFrame frame;
    Value callee = pop(&vm->apiStack);
    assert(callee.type == V_FUNCTION);
    initFrame(vm, &frame, callee.funcID);
    uint8 reg = 0;
    for (size_t i = size(&vm->apiStack)-numArgs-1;
         i < size(&vm->apiStack); ++i) {
        frame.registers[reg] = vm->apiStack[i];
        reg++;
    }
    resize(&vm->apiStack, size(&vm->apiStack)-numArgs);
    pushValue(vm, runFunc(vm, &frame));
}

Value peek(VM *vm, int idx) {
    if (idx < 1) {
        idx = size(&vm->apiStack) + idx;
        return vm->apiStack[idx];
    } else {
        return vm->apiStack[idx];
    }
}

void getGlobal(VM *vm, int symid) {
    add(&vm->apiStack, get(&vm->globals, symid));
}

int main(int argc, char *argv[]) {
    VM vm;
    compileString(&vm, (char *)"(define a 1) a");
    for (size_t i = 0; i < size(&vm.funcs); ++i) {
        printf("FUNC: %lu\n", i);
        printFuncCode(&vm.funcs[i]);
    }
    printf("RUNNING:\n");
    {
        Value f = {V_FUNCTION};
        f.funcID = 0;
        pushValue(&vm, f);
    }
    call(&vm, 0);
    getGlobal(&vm, 1);
    printValue(peek(&vm, -1));
    printf("\nDONE\n");
}