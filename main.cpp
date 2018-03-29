#include "vm.h"
#include <cstring>
#include <cstdio>
#include <cctype>
#include <cstdlib>
#include <cassert>
#include <string>
#include <climits>

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

Token peekToken(LexState *state) {
    return state->nextToken;
}

void printValue(Value value);

void printObject(Object *obj) {
    printf("{(*pointer* %p)", obj);
    for (size_t i = 0; i < size(&obj->slots); ++i) {
        if (obj->slots[i].key.type != V_UNDEF) {
            printf(" (");
            printValue(obj->slots[i].key);
            printf(" ");
            printValue(obj->slots[i].value);
            printf(")");
        }
    }
    printf("}");
}

Token nextToken(VM *vm, LexState *state) {
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
        Symbol symbol = intern(vm, temp_str);
        tok.symstr = symbol.str;
        tok.symid = symbol.id;
        printf("Interned symbol %s as %d\n", symbol.str, symbol.id);
    }
    Token ret = state->nextToken;
    state->nextToken = tok;
    return ret;
}

LexState initLexerState(VM *vm, char *prog) {
    LexState ret{};
    ret.prog = prog;
    nextToken(vm, &ret);
    if (peekToken(&ret).type == T_EOF) {
        fprintf(stderr, "ERROR?: Empty 'files' are not legal\n");
        assert(false);
    }
    return ret;
}

#define allocPair(vm, parent)                                           \
    do {                                                                \
        getC(vm, parent).cdr.type = V_CONS_PAIR;                        \
        getC(vm, parent).cdr.pair = 0;                                  \
        getC(vm, parent).car.type = V_CONS_PAIR;                        \
        getC(vm, parent).car.pair = 0;                                  \
    } while(false)

void parseList(VM *vm, LexState *lex, Handle parent);

void parseExpr(VM *vm, LexState *lex, Handle parent) {
    //assert(getC(vm, parent).type == V_CONS_PAIR);
    //assert(getC(vm, parent).pair == 0);
    Token tok = nextToken(vm, lex);
    switch (tok.type) {
        case T_ERROR: {
            fprintf(stderr, "ERROR: '%.*s'\n", (int)tok.length, tok.str);
            assert(false);
        } break;
        case T_DOUBLE: {
            allocPair(vm, parent);
            getC(vm, parent).car.type = V_DOUBLE;
            getC(vm, parent).car.doub = tok.d;
        } break;
        case T_BOOLEAN: {
            allocPair(vm, parent);
            getC(vm, parent).car.type = V_BOOLEAN;
            getC(vm, parent).car.boolean = tok.b;
        } break;
        case T_SYMBOL: {
            allocPair(vm, parent);
            getC(vm, parent).car.type = V_SYMBOL;
            getC(vm, parent).car.sym.str = tok.symstr;
            getC(vm, parent).car.sym.id = tok.symid;
        } break;
        case T_LEFT_PAREN: {
            allocPair(vm, parent);
            if (peekToken(lex).type != T_RIGHT_PAREN) {
                getC(vm, parent).car.type = V_CONS_PAIR;
                ConsPair *p = allocConsPair(vm);
                getC(vm, parent).car.pair = p;
                Handle car = reserve(vm, getC(vm, parent).car.pair);
                parseList(vm, lex, car);
                free(vm, car);
            }
            nextToken(vm, lex); // eat right parent
        } break;
        case T_RIGHT_PAREN: {
            fprintf(stderr, "ERROR: Mismatching parenthesis, more ) than (\n");
            assert(false);
        } break;
        case T_EOF: {
            fprintf(stderr, "ERROR: Mismatching parenthesis, more ( than )\n");
            assert(false);
        } break;
        default: {
            fprintf(stderr, "ERROR: %d not recognized yet\n", tok.type);
            assert(false);
        } break;
    }
}

void parseList(VM *vm, LexState *lex, Handle parent) {

    Token tok = peekToken(lex);
    Handle currParent = reserve(vm, &getC(vm, parent));
    while (tok.type != T_RIGHT_PAREN) {
        parseExpr(vm, lex, currParent);
        tok = peekToken(lex);
        if (tok.type != T_RIGHT_PAREN) {
            ConsPair *p = allocConsPair(vm);
            getC(vm, currParent).cdr.pair = p;
            Handle newCurrParent = reserve(vm,
                                           getC(vm,
                                                currParent).cdr.pair);
            free(vm, currParent);
            currParent = newCurrParent;
        }
    }
    free(vm, currParent);
}

struct Scope {
    size_t protoID;
    Scope *parent;
    Object *symToReg;
    DynamicArray<size_t> *freeRegisters;
};


Value allocReg(FunctionPrototype *func, Scope scope,
               bool nonLocal = false) {
    if (size(scope.freeRegisters) == 0) {
        size_t reg = func->numRegs;
        assert(reg <= UCHAR_MAX);
        func->numRegs++;
        Value ret{V_REG_OR_CONSTANT};
        ret.regOrConstant = reg;
        ret.nonLocal = nonLocal;
        return ret;
    } else {
        Value ret{V_REG_OR_CONSTANT};
        ret.regOrConstant = pop(scope.freeRegisters);
        ret.nonLocal = nonLocal;
        return ret;
    }
}

void freeReg(Scope scope, size_t reg) {
    add(scope.freeRegisters, reg);
}

Value allocConstant(FunctionPrototype *func, Value val) {
    size_t k = size(&func->constants);
    assert(k <= UCHAR_MAX);
    add(&func->constants, val);
    Value ret{V_REG_OR_CONSTANT};
    ret.regOrConstant = k;
    return ret;
}

#define bitsize(o) (sizeof(o)*8)

void addRRR(FunctionPrototype *func, OpCode op,
            uint8 a, uint8 b, uint8 c) {
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

void addRI(FunctionPrototype *func, OpCode op, uint8 reg, uint32 immediateID) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(reg)) << (bitsize(OpCode) -
                                                       bitsize(uint8)*2)) |
                                  immediateID);
    add(&func->code, assembledOp);
}

void addI(FunctionPrototype *func, OpCode op, uint32 immediateID) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  immediateID);
    add(&func->code, assembledOp);
}

void addRR(FunctionPrototype *func, OpCode op, uint8 a, uint8 b) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(a)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*2)) |
                                  (((OpCode)(b)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*3)));
    add(&func->code, assembledOp);
}

void addR(FunctionPrototype *func, OpCode op, uint8 reg) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(reg)) << (bitsize(OpCode) -
                                                       bitsize(uint8)*2)));
    add(&func->code, assembledOp);
}

void addN(FunctionPrototype *func, OpCode op) {
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

struct VarReg {
    bool nonLocal = true;
    size_t reg;
};

struct ASTNode {
    ASTNodeType type;
    bool hasReg;
    ASTNode(ASTNodeType t, bool b) : type(t), hasReg(b) {}
    virtual void traverse() = 0;
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) = 0;
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) = 0;
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

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        for (size_t i = 0; i < size(&body); ++i) {
            body[i]->emit(vm, scope, valueToConstantSlot);
        }
        if (body[size(&body)-1]->hasReg) {
            Value retReg =
                body[size(&body)-1]->getRegister(vm, scope,
                                                 valueToConstantSlot);
            addR(&vm->funcProtos[scope.protoID],
                 OP_RETURN, retReg.regOrConstant);
        } else {
            addN(&vm->funcProtos[scope.protoID], OP_RETURN_UNDEF);
        }
    };

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        fprintf(stderr, "ICE: body does not have a register?\n");
        assert(false);
    }
};

struct ASTSymbol : ASTNode {
    ASTSymbol() : ASTNode(ASTT_SYMBOL, true) {}
    char *str;
    int symid;
    Value reg;
    virtual void traverse() {
        printf("%s %d\n", str, symid);
    };

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        Value v{V_SYMBOL};
        v.sym.id = symid;
        v.sym.str = str;
        Value k;
        if (keyExists(valueToConstantSlot, v)) {
            k = get(valueToConstantSlot, v);
        } else {
            k = allocConstant(&vm->funcProtos[scope.protoID], v);
            set(vm, valueToConstantSlot, v, k);
        }
        reg = allocReg(&vm->funcProtos[scope.protoID], scope);
        addRI(&vm->funcProtos[scope.protoID], OP_LOADK, reg.regOrConstant,
              k.regOrConstant);
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        return reg;
    }
};

bool getUpvalue(VM *vm, Scope scope, Value variable, uint8 *upvalueIdx) {
    bool found = false;
    DynamicArray<Scope *> scopes;
    uint8 retUpvalueIdx;
    Scope *currScope = &scope;
    while (!found && currScope) {
        FunctionPrototype *proto = &vm->funcProtos[currScope->protoID];
        for (size_t i = 0; i < size(&proto->upvalues); ++i) {
            if (proto->upvalues[i].variable.id == variable.sym.id) {
                retUpvalueIdx = i;
                found = true;
            }
        }
        if (!found) {
            if(currScope->parent &&
               keyExists(currScope->parent->symToReg, variable)) {
                found = true;
                uint8 reg = get(currScope->parent->symToReg,
                                variable).regOrConstant;
                retUpvalueIdx = size(&proto->upvalues);
                add(&proto->upvalues,
                    UpvalueDesc({variable.sym, true,
                                reg}));
            } else {
                add(&scopes, currScope);
                currScope = currScope->parent;
            }
        }
    }
    if (found) {
        if (size(&scopes) >= 1) {
            // Safe since we know that size(&scopes) >= 1 and 1-1 == 0
            for (size_t i = size(&scopes)-1;
                 i >= 0; // Since we add an upvalue to i-1
                 --i) {
                FunctionPrototype *proto =
                    &vm->funcProtos[scopes[i]->protoID];
                uint8 newUpvalueIdx = (uint8)size(&proto->upvalues);
                add(&proto->upvalues,
                    UpvalueDesc({variable.sym, false, retUpvalueIdx}));
                retUpvalueIdx = newUpvalueIdx;
            }
        }
        *upvalueIdx = retUpvalueIdx;
        return true;
    }
    return false;
}

// Hmmmm since we have a register machine a-normal form
// would be *very* good, or until then an enum would work.
struct ASTVariable : ASTNode {
    ASTVariable() : ASTNode(ASTT_VARIABLE, true),
                    symbol(Value{V_SYMBOL}) {}
    Value symbol;
    bool upvalue;
    virtual void traverse() {
        printf("%s %d\n", symbol.sym.str, symbol.sym.id);
    };

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        if (!keyExists(scope.symToReg, symbol)) {
            set(vm, scope.symToReg, symbol,
                allocReg(&vm->funcProtos[scope.protoID], scope, true));
        }
        Value reg = get(scope.symToReg, symbol);
        if (reg.nonLocal) {
            uint8 upvalueIdx;
            if (getUpvalue(vm, scope, symbol, &upvalueIdx)) {
                addRI(&vm->funcProtos[scope.protoID], OP_GET_UPVALUE,
                      reg.regOrConstant, upvalueIdx);
            } else {
                Value k;
                if (keyExists(valueToConstantSlot, symbol)) {
                    k = get(valueToConstantSlot, symbol);
                } else {
                    k = allocConstant(&vm->funcProtos[scope.protoID],
                                      symbol);
                    set(vm, valueToConstantSlot, symbol, k);
                }
                addRI(&vm->funcProtos[scope.protoID], OP_GET_GLOBAL,
                      reg.regOrConstant, k.regOrConstant);
            }
        } else {
            printf("Local variable emmit NOOP ");
            printValue(reg);
            printf("\n");
                
            // NOOP
        }
    }

    // Only (let var val) and arguments
    virtual void setRegister(VM *vm, Scope scope) {
        assert(!keyExists(scope.symToReg, symbol));
        set(vm, scope.symToReg, symbol,
            allocReg(&vm->funcProtos[scope.protoID], scope));
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        assert(keyExists(scope.symToReg, symbol));
        return get(scope.symToReg, symbol);
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

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        vm->funcProtos[scope.protoID].numArgs = size(&args);
        for (size_t i = 0; i < size(&args); ++i) {
            args[i].setRegister(vm, scope);
        }
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        fprintf(stderr, "ICE: argument list does not have a register\n");
        assert(false);
    }
};

struct ASTLambda : ASTNode {
    ASTLambda() : ASTNode(ASTT_LAMBDA, true) {}
    ASTArgList argList;
    ASTBody body;
    Value reg;
    virtual void traverse() {
        printf("Lambda (\n");
        argList.traverse();
        body.traverse();
        printf(")\n");
    };

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        // create new func and stuffs, add the new func to the
        // constant table(?), load the constant.

        Object newSymToReg;
        DynamicArray<size_t> newFreeRegisters;
        Scope newScope = {size(&vm->funcProtos), &scope, &newSymToReg,
                          &newFreeRegisters};
        add(&vm->funcProtos, FunctionPrototype());
        size_t localProtoID =
            size(&vm->funcProtos[scope.protoID].subFuncProtoIDs);
        add(&vm->funcProtos[scope.protoID].subFuncProtoIDs,
            newScope.protoID);
        //newSymToReg.parent = symToReg;
        Object newValueToConstantSlot;
        argList.emit(vm, newScope, &newValueToConstantSlot);
        body.emit(vm, newScope, &newValueToConstantSlot);
        reg = allocReg(&vm->funcProtos[scope.protoID], scope);
        addRI(&vm->funcProtos[scope.protoID], OP_CREATE_FUNC,
              reg.regOrConstant,
              localProtoID);
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        return reg;
    }
};

struct ASTCall : ASTNode {
    ASTCall() : ASTNode(ASTT_CALL, true) {}
    ASTNode *callee;
    DynamicArray<ASTNode *> args;
    Value returnReg;
    virtual void traverse() {
        printf("Call (\n");
        callee->traverse();
        for (size_t i = 0; i < size(&args); ++i) {
            args[i]->traverse();
        }
        printf(")\n");
    }

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        callee->emit(vm, scope, valueToConstantSlot);
        Value calleeReg = callee->getRegister(vm, scope,
                                              valueToConstantSlot);
        DynamicArray<Value> argRegs;
        for (size_t i = 0; i < size(&args); ++i) {
            args[i]->emit(vm, scope, valueToConstantSlot);
            add(&argRegs, args[i]->getRegister(vm, scope,
                                               valueToConstantSlot));
        }
        addR(&vm->funcProtos[scope.protoID], OP_SETUP_CALL, calleeReg.regOrConstant);
        for (size_t i = 0; i < size(&argRegs); ++i) {
            addR(&vm->funcProtos[scope.protoID], OP_PUSH_ARG,
                 argRegs[i].regOrConstant);
            if (args[i]->type != ASTT_VARIABLE) {
                freeReg(scope, argRegs[i].regOrConstant);
            }
        }
        returnReg = allocReg(&vm->funcProtos[scope.protoID], scope);
        addR(&vm->funcProtos[scope.protoID], OP_CALL, returnReg.regOrConstant);
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        return returnReg;
    }

};

struct ASTDouble : ASTNode {
    ASTDouble() : ASTNode(ASTT_DOUBLE, true) {}
    double value;
    Value reg;
    virtual void traverse() {
        printf("%f\n", value);
    }

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        Value v{V_DOUBLE};
        v.doub = value;
        Value k;
        if (keyExists(valueToConstantSlot, v)) {
            k = get(valueToConstantSlot, v);
        } else {
            k = allocConstant(&vm->funcProtos[scope.protoID], v);
            set(vm, valueToConstantSlot, v, k);
        }
        reg = allocReg(&vm->funcProtos[scope.protoID], scope);
        addRI(&vm->funcProtos[scope.protoID], OP_LOADK, reg.regOrConstant,
              k.regOrConstant);
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        return reg;
    }
};

struct ASTBoolean : ASTNode {
    ASTBoolean() : ASTNode(ASTT_BOOLEAN, true) {}
    bool value;
    Value reg;
    virtual void traverse() {
        printf("%s\n", value ? "true" : "false");
    }

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        Value v{V_BOOLEAN};
        v.boolean = value;
        Value k;
        if (keyExists(valueToConstantSlot, v)) {
            k = get(valueToConstantSlot, v);
        } else {
            k = allocConstant(&vm->funcProtos[scope.protoID], v);
            set(vm, valueToConstantSlot, v, k);
        }
        reg = allocReg(&vm->funcProtos[scope.protoID], scope);
        addRI(&vm->funcProtos[scope.protoID], OP_LOADK, reg.regOrConstant,
              k.regOrConstant);
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
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
    Value reg;
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

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        reg = allocReg(&vm->funcProtos[scope.protoID], scope);
        addR(&vm->funcProtos[scope.protoID], OP_MAKE_OBJECT, reg.regOrConstant);
        DynamicArray<uint8> regsToFree;
        for (size_t i = 0; i < size(&slots); ++i) {
            slots[i].key->emit(vm, scope, valueToConstantSlot);
            slots[i].value->emit(vm, scope, valueToConstantSlot);
            Value keyReg = slots[i].key->getRegister(vm, scope,
                                                     valueToConstantSlot);
            Value valueReg =
                slots[i].value->getRegister(vm, scope,
                                            valueToConstantSlot);
            addRRR(&vm->funcProtos[scope.protoID], OP_SET_OBJECT_SLOT,
                   reg.regOrConstant,
                   keyReg.regOrConstant,
                   valueReg.regOrConstant);
            if (slots[i].key->type != ASTT_VARIABLE) {
                add(&regsToFree, keyReg.regOrConstant);
            }
            if (slots[i].value->type != ASTT_VARIABLE) {
                add(&regsToFree, valueReg.regOrConstant);
            }
        }
        for (size_t i = 0; i < size(&regsToFree); ++i) {
            freeReg(scope, regsToFree[i]);
        }
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        return reg;
    }
};

struct ASTDefine : ASTNode {
    Value var;
    ASTNode *expr;
    ASTDefine() : ASTNode(ASTT_DEFINE, false), var(Value{V_SYMBOL}) {}
    virtual void traverse() {
        printf("(define\n");
        printValue(var);
        printf("\n");
        expr->traverse();
        printf(")\n");
    }

    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
        expr->emit(vm, scope, valueToConstantSlot);
        Value k;
        if (keyExists(valueToConstantSlot, var)) {
            k = get(valueToConstantSlot, var);
        } else {
            k = allocConstant(&vm->funcProtos[scope.protoID], var);
            set(vm, valueToConstantSlot, var, k);
        }
        addRI(&vm->funcProtos[scope.protoID], OP_DEFINE_GLOBAL,
              expr->getRegister(vm, scope,
                                valueToConstantSlot).regOrConstant,
              k.regOrConstant);
        if (expr->type != ASTT_VARIABLE) {
            freeReg(scope,
                    expr->getRegister(vm, scope,
                                      valueToConstantSlot).regOrConstant);
        }
    }

    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
        fprintf(stderr, "ERROR: define can't be used as an expression\n");
        assert(false);
    }
};

struct ArenaAllocator {
    void *mem = 0;
    const size_t size = 512; // TODO: Tune me!!!
    size_t top = 0;
    ArenaAllocator *next = 0;
    ArenaAllocator *last = 0;
};

template <typename T>
T *alloc(ArenaAllocator *arena) {
    if (!arena->mem) {
        arena->mem = malloc(arena->size);
        arena->last = arena;
    }
    assert(sizeof(T) + sizeof(ArenaAllocator) < arena->size);
    if (arena->last->top + sizeof(T) +
        sizeof(ArenaAllocator) > arena->size) {
        arena->last->next = new(((char *)arena->last->mem) +
                                arena->last->top) ArenaAllocator;
        arena->last->top += sizeof(ArenaAllocator);
        arena->last = arena->last->next;
        arena->last->mem = malloc(arena->size);
    }
    T *ret = new(((char *)arena->last->mem) + arena->last->top) T;
    arena->last->top += sizeof(T);
    return ret;
}

void freeArena(ArenaAllocator *arena) {
    if (arena) {
        freeArena(arena->next);
        free(arena->mem);
        arena->mem = 0;
        arena->top = 0;
        arena->next = 0;
        arena->last = 0;
    }
}

ASTSymbol symbolToAST(Value tree) {
    assert(tree.type == V_SYMBOL);
    ASTSymbol ret;
    ret.str = tree.sym.str;
    ret.symid = tree.sym.id;
    return ret;
}

ASTVariable variableToAST(Value tree) {
    assert(tree.type == V_SYMBOL);
    ASTVariable ret;
    ret.symbol = tree;
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

ASTNode *exprToAST(ArenaAllocator *arena, Value tree);

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

ASTCall callToAST(ArenaAllocator *arena, Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTCall ret;
    if (length(tree) < 1) {
        fprintf(stderr, "ERROR: calling to nothing does not work!\n");
        assert(false);
    }
    ret.callee = exprToAST(arena, tree.pair->car);
    assert(tree.pair->cdr.type == V_CONS_PAIR);
    tree = tree.pair->cdr;
    while (tree.pair) {
        add(&ret.args, exprToAST(arena, tree.pair->car));
        tree = tree.pair->cdr;
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

ASTBody bodyToAST(ArenaAllocator *arena, Value tree);

// Fix me, Intern lambda, quote etc.
#define parseTreeSymCmp(tree, s) (!strcmp((tree).sym.str, s))

#define firstInListIsType(tree, p_type) (((tree).pair &&               \
                                          (tree).pair->car.type ==     \
                                          (p_type)))


ASTLambda lambdaToAST(ArenaAllocator *arena, Value tree) {
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
    ret.body = bodyToAST(arena, afterLambdaSym.pair->cdr);
    return ret;
}

ASTNode *quotedToAST(ArenaAllocator *arena, Value tree) {
    ASTNode *ret = 0;
    switch (tree.type) {
        case V_DOUBLE: {
            ASTDouble *c = alloc<ASTDouble>(arena);
            *c = doubleToAST(tree);
            ret = c;
        } break;
        case V_BOOLEAN: {
            ASTBoolean *c = alloc<ASTBoolean>(arena);
            *c = booleanToAST(tree);
            ret = c;
        } break;
        case V_SYMBOL: {
            ASTSymbol *c = alloc<ASTSymbol>(arena);
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

MakeObjectSlot makeObjectSlot(ArenaAllocator *arena, Value tree) {
    MakeObjectSlot ret;
    if (tree.type != V_CONS_PAIR || length(tree) != 2) {
        fprintf(stderr, "ERROR: make-object's argument has to be an associative list\n");
        assert(false);
    }
    ret.key = exprToAST(arena, tree.pair->car);
    ret.value = exprToAST(arena, tree.pair->cdr.pair->car);
    return ret;
}

ASTMakeObject makeObjectToAST(ArenaAllocator *arena, Value tree) {
    ASTMakeObject ret;
    if (tree.type != V_CONS_PAIR) {
        fprintf(stderr, "ERROR: make-object's argument has to be an associative list\n");
        assert(false);
    }
    resize(&ret.slots, length(tree));
    for (size_t i = 0; tree.pair; ++i, tree = tree.pair->cdr) {
        ret.slots[i] = makeObjectSlot(arena, tree.pair->car);
    }
    return ret;
}

ASTDefine defineToAST(ArenaAllocator *arena, Value tree) {
    ASTDefine ret;
    assert(tree.type == V_CONS_PAIR);
    assert(tree.pair);
    assert(tree.pair->cdr.type == V_CONS_PAIR);
    assert(tree.pair->cdr.pair);
    if (tree.pair->car.type != V_SYMBOL) {
        fprintf(stderr, "ERROR: Can only define symbols as variables\n");
        assert(false);
    }
    ret.var = tree.pair->car;
    ret.expr = exprToAST(arena, tree.pair->cdr.pair->car);
    return ret;
}

ASTNode *exprToAST(ArenaAllocator *arena, Value tree) {
    ASTNode *ret = 0;
    switch (tree.type) {
        case V_DOUBLE: {
            ASTDouble *c = alloc<ASTDouble>(arena);
            *c = doubleToAST(tree);
            ret = c;
        } break;
        case V_BOOLEAN: {
            ASTBoolean *c = alloc<ASTBoolean>(arena);
            *c = booleanToAST(tree);
            ret = c;
        } break;
        case V_SYMBOL: {
            ASTVariable *c = alloc<ASTVariable>(arena);
            *c = variableToAST(tree);
            ret = c;
        } break;
        case V_CONS_PAIR: {
            if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "lambda")) {
                ASTLambda *c = alloc<ASTLambda>(arena);
                *c = lambdaToAST(arena, tree);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "define")) {
                if (length(tree) != 3) {
                    fprintf(stderr, "ERROR: define takes two arguments!\n");
                    assert(false);
                }
                ASTDefine *c = alloc<ASTDefine>(arena);
                *c = defineToAST(arena, tree.pair->cdr);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "quote")) {
                if (length(tree) != 2) {
                    fprintf(stderr, "ERROR: quote takes only one argument!\n");
                    assert(false);
                }
                ret = quotedToAST(arena, at(tree, 1));
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car,
                                       "make-object")) {
                if (length(tree) != 2) {
                    fprintf(stderr, "ERROR: make-object takes only one argument!\n");
                    assert(false);
                }
                ASTMakeObject *c = alloc<ASTMakeObject>(arena);
                *c = makeObjectToAST(arena, at(tree, 1));
                ret = c;
            } else {
                ASTCall *c = alloc<ASTCall>(arena);
                *c = callToAST(arena, tree);
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

ASTBody bodyToAST(ArenaAllocator *arena, Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTBody ret;
    while (tree.pair) {
        assert(tree.type == V_CONS_PAIR);
        add(&ret.body, exprToAST(arena, tree.pair->car));
        tree = tree.pair->cdr;
    }
    return ret;
}

// TODO: symid to symbol string
void printValue(Value v) {
    switch(v.type) {
        case V_REG_OR_CONSTANT: {
            printf("<reg or constant: %d global: %s>",
                   v.regOrConstant, v.nonLocal ? "true" : "false");
        } break;
        case V_STRING: {
            printf("\"%s\"", v.str);
        } break;
        case V_UNDEF: {
            printf("<undef>");
        } break;
        case V_OPAQUE_POINTER: {
            printf("<opaque: %p type: %llu>", v.opaque, v.opaqueType);
        } break;
        case V_FUNCTION: {
            printf("<func: %p>", v.func);
        } break;
        case V_OBJECT: {
            printObject(v.object);
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
            printf("%s", v.sym.str);
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
        case OT_I: {
            uint32 immId = getImm(undecoded);
            printf("i%x\n", immId);
        } break;
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

void printFuncProtoCode(FunctionPrototype *func) {
    printf("NUM ARGS: %lu\n", func->numArgs);
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

Value compileString(VM *vm, char *prog) {
    LexState lex = initLexerState(vm, prog);
    Object symToReg;
    Object valueToConstantSlot;
    DynamicArray<size_t> freeRegisters;
    Scope topScope = {size(&vm->funcProtos), 0, &symToReg,
                      &freeRegisters};
    add(&vm->funcProtos, FunctionPrototype());
    ASTNode *node = 0;
    ArenaAllocator arena;
    {
        ConsPair *root = allocConsPair(vm);
        Handle handle = reserve(vm, root);
        Handle currTree = handle;
        while (peekToken(&lex).type != T_EOF) {
            freeArena(&arena);
            parseExpr(vm, &lex, currTree);
            printf("\n");
            printValue(getC(vm, currTree).car);
            printf("\n\n");
            node = exprToAST(&arena, getC(vm, currTree).car);
            node->traverse();
            node->emit(vm, topScope, &valueToConstantSlot);
            if (peekToken(&lex).type != T_EOF) {
                ConsPair *p = allocConsPair(vm);
                getC(vm, currTree).cdr.pair = p;
                Handle temp = reserve(vm, getC(vm, currTree).cdr.pair);
                free(vm, currTree);
                currTree = temp;
            } else {
                free(vm, currTree);
            }
        }
    }
    if (!node) {
        fprintf(stderr, "ERROR: Empty 'file' is not allowed\n");
        assert(false);
    }
    if (node->hasReg) {
        Value retReg = node->getRegister(vm, topScope,
                                         &valueToConstantSlot);
        addR(&vm->funcProtos[topScope.protoID],
             OP_RETURN, retReg.regOrConstant);
    } else {
        addN(&vm->funcProtos[topScope.protoID], OP_RETURN_UNDEF);
    }
    freeArena(&arena);
    Value ret = {V_FUNCTION};
    ret.func = allocFunction(vm, &vm->funcProtos[topScope.protoID]);
    return ret;
}

Value compileFile(VM *vm, const char *path) {
    FILE *file = fopen(path, "r");
    long int length;
    fseek(file, 0, SEEK_END);
    length = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *prog = (char *)malloc(length+1);
    prog[length] = 0;
    fread(prog, sizeof(char), length, file);
    fclose(file);
    return compileString(vm, prog);
}

size_t allocFrame(VM *vm, Function *func) {
    ActivationFrame *frame;
    if (vm->frameStackTop < size(&vm->frameStack)) {
        frame = &vm->frameStack[vm->frameStackTop];
    } else {
        ActivationFrame newFrame;
        add(&vm->frameStack, newFrame);
        frame = &vm->frameStack[size(&vm->frameStack)-1];
    }
    vm->frameStackTop++;
    *frame = ActivationFrame();
    frame->func = func; // Might this break?? or not??
    resize(&frame->registers, frame->func->prototype->numRegs);
    return vm->frameStackTop;
}

void popFrame(VM *vm) {
    vm->frameStackTop--;
    resize(&vm->frameStack[vm->frameStackTop].registers, 0);
}

void closeAllUpvalues(VM *vm, ActivationFrame *frame) {
    if (vm->openUpvalueHead) {
        void *start = &frame->registers[0];
        void *end = (&frame->registers[size(&frame->registers)-1])+1;
        while (vm->openUpvalueHead &&
               vm->openUpvalueHead->value >= start &&
               vm->openUpvalueHead->value < end) {
            vm->openUpvalueHead->closed = *vm->openUpvalueHead->value;
            vm->openUpvalueHead->value = &vm->openUpvalueHead->closed;
            vm->openUpvalueHead = vm->openUpvalueHead->next;
        }
        Upvalue *u = vm->openUpvalueHead;
        while (u) {
            if (!u->next || (u->next &&
                             u->next->value >= start &&
                             u->next->value < end)) {
                u = u->next;
            } else {
                u->next->closed = *u->next->value;
                u->next->value = &u->next->closed;
                Upvalue *t = u->next;
                u->next = u->next->next;
                t->next = 0;
            }
        }
    }
}

Value runFunc(VM *vm, size_t frameID) {
    OpCode undecoded;
    ActivationFrame *frame = &vm->frameStack[frameID-1];
 loop:
    undecoded = frame->func->prototype->code[frame->pc];
    OpCode instr = getOp(undecoded);
    switch(instr) {
        case OP_LOADK: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            frame->registers[reg] = frame->func->prototype->constants[k];
            frame->pc++;
        } break;
        case OP_SETUP_CALL: {
            goto call;
        } break;
        case OP_RETURN: {
            closeAllUpvalues(vm, frame);
            uint8 reg = getRegA(undecoded);
            if (!frame->calledFromCpp) {
                frameID -= 1;
                ActivationFrame *caller = &vm->frameStack[frameID-1];
                caller->registers[frame->retReg] = frame->registers[reg];
                frame = caller;
            } else {
                Value ret = frame->registers[reg];
                popFrame(vm);
                return ret;
            }
            popFrame(vm);
        } break;
        case OP_RETURN_UNDEF: {
            closeAllUpvalues(vm, frame);
            if (!frame->calledFromCpp) {
                frameID -= 1;
                ActivationFrame *caller = &vm->frameStack[frameID-1];
                caller->registers[frame->retReg] = Value{V_UNDEF};
                frame = caller;
            } else {
                popFrame(vm);
                return Value{V_UNDEF};
            }
            popFrame(vm);
        } break;
        case OP_CREATE_FUNC: {
            uint8 reg = getRegA(undecoded);
            uint32 localProtoID = getImm(undecoded);
            Value func = {V_FUNCTION};
            size_t protoID =
                frame->func->prototype->subFuncProtoIDs[localProtoID];
            func.func = allocFunction(vm, &vm->funcProtos[protoID]);
            frame->registers[reg] = func;
            for (size_t i = 0;
                 i < size(&frame->registers[reg].func->upvalues); ++i) {
                Upvalue *u;
                FunctionPrototype *proto = &vm->funcProtos[protoID];
                if (proto->upvalues[i].local) {
                    u = allocUpvalue(vm);
                    u->value =
                        &frame->registers[proto->upvalues[i].index];
                } else {
                    u = frame->func->upvalues[proto->upvalues[i].index];
                }
                frame->registers[reg].func->upvalues[i] = u;
            }
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
            set(vm, obj.object, key, val);
            frame->pc++;
        } break;
        case OP_DEFINE_GLOBAL: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            if (frame->func->prototype->constants[k].type != V_SYMBOL) {
                fprintf(stderr, "ICE: DEFINE_GLOBAL imm not a symbol\n");
                assert(false);
            }

            set(vm, &vm->globals, frame->func->prototype->constants[k], 
                frame->registers[reg]); 
            frame->pc++;
        } break;
        case OP_GET_GLOBAL: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            if (frame->func->prototype->constants[k].type != V_SYMBOL) {
                fprintf(stderr, "ICE: GET_GLOBAL imm not a symbol\n");
                assert(false);
            }
            frame->registers[reg] = get(&vm->globals,
                                        frame->func->prototype->constants[k]);
            frame->pc++;
        } break;
        case OP_GET_UPVALUE: {
            uint8 reg = getRegA(undecoded);
            uint32 upvalueIdx = getImm(undecoded);
            frame->registers[reg] =
                *frame->func->upvalues[upvalueIdx]->value;
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
        
        size_t calleeFrameID = allocFrame(vm, callee.func);
        frame = &vm->frameStack[frameID-1];
        ActivationFrame *calleeFrame = &vm->frameStack[calleeFrameID-1];
        calleeFrame->calledFromCpp = false;
        uint8 dstReg = 0;
    pushArgLoop:
        undecoded = frame->func->prototype->code[frame->pc];
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
                frameID = calleeFrameID;
                if (dstReg != frame->func->prototype->numArgs) {
                    fprintf(stderr, "Not correct amount of arguments!\n");
                    assert(false);
                }
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

void call(VM *vm, uint8 numArgs) {
    Value callee = pop(&vm->apiStack);
    assert(callee.type == V_FUNCTION);
    size_t frameID = allocFrame(vm, callee.func);
    ActivationFrame *frame = &vm->frameStack[frameID-1];
    if (numArgs != frame->func->prototype->numArgs) {
        fprintf(stderr, "Not correct amount of argument!\n");
        assert(false);
    }
    uint8 reg = 0;
    for (size_t i = size(&vm->apiStack)-numArgs;
         i < size(&vm->apiStack); ++i) {
        frame->registers[reg] = vm->apiStack[i];
        reg++;
    }
    resize(&vm->apiStack, size(&vm->apiStack)-numArgs);
    pushValue(vm, runFunc(vm, frameID));
}

void doString(VM *vm, char *prog, size_t numArgs = 0) {
    pushValue(vm, compileString(vm, prog));
    call(vm, numArgs);
}

void doFile(VM *vm, const char *path, size_t numArgs = 0) {
    pushValue(vm, compileFile(vm, path));
    for (size_t i = 0; i < size(&vm->funcProtos); ++i) {
        printFuncProtoCode(&vm->funcProtos[i]);
    }
    call(vm, numArgs);
}

int main(int argc, char *argv[]) {
    VM vm;
    doFile(&vm, "./basic.lsp");
    printf("\n");
    printValue(peek(&vm, -1));
    printf("\n");
    collect(&vm);
    pushValue(&vm, Value(V_UNDEF));
    setGlobal(&vm, intern(&vm, "testFunc2"));
    for (int i = 0; i < 10; ++i) {
        getGlobal(&vm, intern(&vm, "testFunc"));
        call(&vm, 0);
        //getGlobal(&vm, 1);
        printValue(peek(&vm, -1));
        printf("\n");
        clearStack(&vm);
        collect(&vm);
    }
    freeVM(&vm);
    
}