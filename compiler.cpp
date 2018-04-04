#include "compiler.h"
#include "parser.h"
#include <climits>
#include <cstdio>


// Fix me, Intern lambda, quote etc.
#define parseTreeSymCmp(tree, s) (!strcmp((tree).sym.str, s))

#define firstInListIsType(tree, p_type) (((tree).pair &&               \
                                          (tree).pair->car.type ==     \
                                          (p_type)))


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

void patchRS(FunctionPrototype *func, size_t pos, OpCode op, uint8 reg,
             int32 immediate) {

    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(reg)) << (bitsize(OpCode) -
                                                       bitsize(uint8)*2)) |
                                  *((uint32 *)&immediate));
    func->code[pos] = assembledOp;
}

void patchS(FunctionPrototype *func, size_t pos, OpCode op,
             int32 immediate) {

    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  *((uint32 *)&immediate));
    func->code[pos] = assembledOp;
}


size_t getPos(FunctionPrototype *func) {
    return size(&func->code);
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

ASTNode *exprToAST(VM *vm, ArenaAllocator *arena, Value tree);

ASTCall callToAST(VM *vm, ArenaAllocator *arena, Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTCall ret;
    if (length(tree) < 1) {
        fprintf(stderr, "ERROR: calling to nothing does not work!\n");
        assert(false);
    }
    ret.callee = exprToAST(vm, arena, tree.pair->car);
    assert(tree.pair->cdr.type == V_CONS_PAIR);
    tree = tree.pair->cdr;
    while (tree.pair) {
        add(&ret.args, exprToAST(vm, arena, tree.pair->car));
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

ASTIf ifToAST(VM *vm, ArenaAllocator *arena, Value tree) {
    assert(tree.type == V_CONS_PAIR);
    if (length(tree) < 3) {
        fprintf(stderr, "ERROR: if needs atleast predicate and a true branch\n");
        assert(false);
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(parseTreeSymCmp(tree.pair->car, "if"));
    ASTIf ret;
    ret.pred = exprToAST(vm, arena, at(tree, 1));
    ret.trueBranch = exprToAST(vm, arena, at(tree, 2));
    if (length(tree) == 4) {
        ret.falseBranch = exprToAST(vm, arena, at(tree, 3));
    } else {
        ret.falseBranch = 0;
    }
    return ret;
}

ASTBody bodyToAST(VM *vm, ArenaAllocator *arena, Value tree);

ASTLambda lambdaToAST(VM *vm, ArenaAllocator *arena, Value tree) {
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
    ret.body = bodyToAST(vm, arena, afterLambdaSym.pair->cdr);
    return ret;
}

ASTDefmacro defmacroToAST(VM *vm, ArenaAllocator *arena, Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTDefmacro ret;
    if (length(tree) < 4) {
        fprintf(stderr, "ERROR: defmacro needs a variable, an argument list and a body!\n");
        assert(false);
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(parseTreeSymCmp(tree.pair->car, "defmacro"));
    Value afterDefmacroSym = tree.pair->cdr;
    if (afterDefmacroSym.pair->car.type != V_SYMBOL) {
        fprintf(stderr, "defmacro variable not a symbol!");
        assert(false);
    }
    ret.variable = afterDefmacroSym.pair->car;
    ret.argList = argListToAST(afterDefmacroSym.pair->cdr.pair->car);
    ret.body = bodyToAST(vm, arena, afterDefmacroSym.pair->cdr.pair->cdr);
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
        case V_CONS_PAIR: {
            ASTList *c = alloc<ASTList>(arena);
            while (tree.pair) {
                add(&c->elems, quotedToAST(arena, tree.pair->car));
                tree = tree.pair->cdr;
            }
            ret = c;
        } break;
        default: {
            fprintf(stderr, "ERROR: %d cant be quoted yet\n", tree.type);
            assert(false);
        } break;
    }
    return ret;
}

MakeObjectSlot makeObjectSlot(VM *vm, ArenaAllocator *arena, Value tree) {
    MakeObjectSlot ret;
    if (tree.type != V_CONS_PAIR || length(tree) != 2) {
        fprintf(stderr, "ERROR: make-object's argument has to be an associative list\n");
        assert(false);
    }
    ret.key = exprToAST(vm, arena, tree.pair->car);
    ret.value = exprToAST(vm, arena, tree.pair->cdr.pair->car);
    return ret;
}

ASTMakeObject makeObjectToAST(VM *vm, ArenaAllocator *arena, Value tree) {
    ASTMakeObject ret;
    if (tree.type != V_CONS_PAIR) {
        fprintf(stderr, "ERROR: make-object's argument has to be an associative list\n");
        assert(false);
    }
    resize(&ret.slots, length(tree));
    for (size_t i = 0; tree.pair; ++i, tree = tree.pair->cdr) {
        ret.slots[i] = makeObjectSlot(vm, arena, tree.pair->car);
    }
    return ret;
}

ASTDefine defineToAST(VM *vm, ArenaAllocator *arena, Value tree) {
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
    ret.expr = exprToAST(vm, arena, tree.pair->cdr.pair->car);
    return ret;
}

ASTLet letToAST(VM *vm, ArenaAllocator *arena, Value tree) {
    ASTLet ret;
    assert(tree.type == V_CONS_PAIR);
    if (length(tree) != 3) {
        fprintf(stderr, "ERROR: let needs both a symbol and an expression!\n");
        assert(false);
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(parseTreeSymCmp(tree.pair->car, "let"));
    ret.var = variableToAST(at(tree, 1));
    ret.expr = exprToAST(vm, arena, at(tree, 2));
    return ret;
}

ASTSet setToAST(VM *vm, ArenaAllocator *arena, Value tree) {
    ASTSet ret;
    assert(tree.type == V_CONS_PAIR);
    if (length(tree) != 3) {
        fprintf(stderr, "ERROR: set! needs both a symbol and an expression!\n");
        assert(false);
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(parseTreeSymCmp(tree.pair->car, "set!"));
    ret.var = variableToAST(at(tree, 1));
    ret.expr = exprToAST(vm, arena, at(tree, 2));
    return ret;
}


ASTNode *exprToAST(VM *vm, ArenaAllocator *arena, Value tree) {
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
                *c = lambdaToAST(vm, arena, tree);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "if")) {
                ASTIf *c = alloc<ASTIf>(arena);
                *c = ifToAST(vm, arena, tree);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "defmacro")) {
                ASTDefmacro *c = alloc<ASTDefmacro>(arena);
                *c = defmacroToAST(vm, arena, tree);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "let")) {
                ASTLet *c = alloc<ASTLet>(arena);
                *c = letToAST(vm, arena, tree);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "set!")) {
                ASTSet *c = alloc<ASTSet>(arena);
                *c = setToAST(vm, arena, tree);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "scope")) {
                ASTBody *c = alloc<ASTBody>(arena);
                *c = bodyToAST(vm, arena, tree.pair->cdr);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       parseTreeSymCmp(tree.pair->car, "define")) {
                if (length(tree) != 3) {
                    fprintf(stderr, "ERROR: define takes two arguments!\n");
                    assert(false);
                }
                ASTDefine *c = alloc<ASTDefine>(arena);
                *c = defineToAST(vm, arena, tree.pair->cdr);
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
                *c = makeObjectToAST(vm, arena, at(tree, 1));
                ret = c;
            } else {
                ASTCall *c = alloc<ASTCall>(arena);
                *c = callToAST(vm, arena, tree);
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

ASTBody bodyToAST(VM *vm, ArenaAllocator *arena, Value tree) {
    assert(tree.type == V_CONS_PAIR);
    ASTBody ret;
    while (tree.pair) {
        assert(tree.type == V_CONS_PAIR);
        add(&ret.body, exprToAST(vm, arena, tree.pair->car));
        tree = tree.pair->cdr;
    }
    return ret;
}



Value compileString(VM *vm, char *prog, bool verbose) {
    LexState lex = initLexerState(vm, prog);
    Object symToReg;
    Object valueToConstantSlot;
    assert(size(&symToReg.slots) > 0);
    assert(size(&valueToConstantSlot.slots) > 0);
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
            if (verbose) {
                printf("\n");
                printValue(getC(vm, currTree).car);
                printf("\n\n");
            }
            node = exprToAST(vm, &arena, getC(vm, currTree).car);
            if (verbose) {
                node->traverse();
            }
            node->emit(vm, topScope, &valueToConstantSlot);
            if (peekToken(&lex).type != T_EOF) {
                ConsPair *p = allocConsPair(vm);
                getC(vm, currTree).cdr.pair = p;
                Handle temp = reserve(vm, getC(vm, currTree).cdr.pair);
                free(vm, currTree);
                currTree = temp;
                node->freeRegister(topScope);
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

Value compileFile(VM *vm, const char *path, bool verbose) {
    FILE *file = fopen(path, "r");
    long int length;
    fseek(file, 0, SEEK_END);
    length = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *prog = (char *)malloc(length+1);
    prog[length] = 0;
    fread(prog, sizeof(char), length, file);
    fclose(file);
    return compileString(vm, prog, verbose);
}


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


ASTNode::ASTNode(ASTNodeType t, bool b) : type(t), hasReg(b) {}

ASTBody::ASTBody() : ASTNode(ASTT_BODY, false) {}

void ASTBody::traverse() {
    for (size_t i = 0; i < size(&body); ++i) {
        body[i]->traverse();
    }
}

void ASTBody::emit(VM *vm, Scope scope,
                   Object *valueToConstantSlot) {
    for (size_t i = 0; i < size(&body); ++i) {
        body[i]->emit(vm, scope, valueToConstantSlot);
        body[i]->freeRegister(scope);
    }
    hasReg = body[size(&body)-1]->hasReg;
    //if (body[size(&body)-1]->hasReg) {
    //Value retReg =
    //body[size(&body)-1]->getRegister(vm, scope,
    //valueToConstantSlot);
    //addR(&vm->funcProtos[scope.protoID],
    //OP_RETURN, retReg.regOrConstant);
    //} else {
    //addN(&vm->funcProtos[scope.protoID], OP_RETURN_UNDEF);
    //}
};

Value ASTBody::getRegister(VM *vm, Scope scope,
                           Object *valueToConstantSlot) {
    return body[size(&body)-1]->getRegister(vm, scope,
                                            valueToConstantSlot);
}

void ASTBody::freeRegister(Scope scope) {}
ASTSymbol::ASTSymbol() : ASTNode(ASTT_SYMBOL, true) {}
void ASTSymbol::traverse() {
    printf("%s %d\n", str, symid);
}
void ASTSymbol::emit(VM *vm, Scope scope,
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
Value ASTSymbol::getRegister(VM *vm, Scope scope,
                             Object *valueToConstantSlot) {
    return reg;
}
void ASTSymbol::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

// Hmmmm since we have a register machine a-normal form
// would be *very* good, or until then an enum would work.
ASTVariable::ASTVariable() : ASTNode(ASTT_VARIABLE, true),
                             symbol(Value{V_SYMBOL}) {}
void ASTVariable::traverse() {
    printf("%s %d\n", symbol.sym.str, symbol.sym.id);
};

void ASTVariable::emit(VM *vm, Scope scope,
                       Object *valueToConstantSlot) {
    if (!keyExists(scope.symToReg, symbol)) {
        set(vm, scope.symToReg, symbol,
            allocReg(&vm->funcProtos[scope.protoID], scope, true));
    }
    Value reg = get(scope.symToReg, symbol);
    if (reg.nonLocal) {
        uint8 upvalueIdx;
        if (getUpvalue(vm, scope, symbol, &upvalueIdx)) {
            // Upval
            addRI(&vm->funcProtos[scope.protoID], OP_GET_UPVALUE,
                  reg.regOrConstant, upvalueIdx);
        } else {
            // Global
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
        // Local
        printf("Local variable emmit NOOP ");
        printValue(reg);
        printf("\n");
        
        // NOOP
    }
}

// Only (let var val) and arguments
void ASTVariable::setRegister(VM *vm, Scope scope) {
    //assert(!keyExists(scope.symToReg, symbol));
    set(vm, scope.symToReg, symbol,
        allocReg(&vm->funcProtos[scope.protoID], scope));
}

Value ASTVariable::getRegister(VM *vm, Scope scope,
                               Object *valueToConstantSlot) {
    assert(keyExists(scope.symToReg, symbol));
    return get(scope.symToReg, symbol);
}

void ASTVariable::freeRegister(Scope scope) {
}

ASTArgList::ASTArgList() : ASTNode(ASTT_ARGLIST, true) {}
void ASTArgList::traverse() {
    printf("ArgList (\n");
    for (size_t i = 0; i < size(&args); ++i) {
        args[i].traverse();
    }
    printf(")\n");
};

void ASTArgList::emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
    vm->funcProtos[scope.protoID].numArgs = size(&args);
    for (size_t i = 0; i < size(&args); ++i) {
        args[i].setRegister(vm, scope);
    }
}

Value ASTArgList::getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
    fprintf(stderr, "ICE: argument list does not have a register\n");
    assert(false);
}

void ASTArgList::freeRegister(Scope scope) {
    fprintf(stderr, "ICE: argument list does not have a register\n");
    assert(false);
}

ASTLambda::ASTLambda() : ASTNode(ASTT_LAMBDA, true) {}
void ASTLambda::traverse() {
    printf("Lambda (\n");
    argList.traverse();
    body.traverse();
    printf(")\n");
};

void ASTLambda::emit(VM *vm, Scope scope,
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
    if (body.hasReg) {
        Value retReg = body.getRegister(vm, newScope,
                                        valueToConstantSlot);
        addR(&vm->funcProtos[newScope.protoID],
             OP_RETURN, retReg.regOrConstant);
    } else {
        addN(&vm->funcProtos[newScope.protoID], OP_RETURN_UNDEF);
    }
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addRI(&vm->funcProtos[scope.protoID], OP_CREATE_FUNC,
          reg.regOrConstant,
          localProtoID);
}

Value ASTLambda::getRegister(VM *vm, Scope scope,
                             Object *valueToConstantSlot) {
    return reg;
}

void ASTLambda::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTCall::ASTCall() : ASTNode(ASTT_CALL, true) {}
void ASTCall::traverse() {
    printf("Call (\n");
    callee->traverse();
    for (size_t i = 0; i < size(&args); ++i) {
        args[i]->traverse();
    }
    printf(")\n");
}

void ASTCall::emit(VM *vm, Scope scope,
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
        args[i]->freeRegister(scope);
    }
    returnReg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addR(&vm->funcProtos[scope.protoID], OP_CALL, returnReg.regOrConstant);
}

Value ASTCall::getRegister(VM *vm, Scope scope,
                           Object *valueToConstantSlot) {
    return returnReg;
}

void ASTCall::freeRegister(Scope scope) {
    freeReg(scope, returnReg.regOrConstant);
}

ASTDouble::ASTDouble() : ASTNode(ASTT_DOUBLE, true) {}
void ASTDouble::traverse() {
    printf("%f\n", value);
}

void ASTDouble::emit(VM *vm, Scope scope,
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

Value ASTDouble::getRegister(VM *vm, Scope scope,
                             Object *valueToConstantSlot) {
    return reg;
}

void ASTDouble::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTBoolean::ASTBoolean() : ASTNode(ASTT_BOOLEAN, true) {}

void ASTBoolean::traverse() {
    printf("%s\n", value ? "true" : "false");
}

void ASTBoolean::emit(VM *vm, Scope scope,
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

Value ASTBoolean::getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) {
    return reg;
}

void ASTBoolean::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTMakeObject::ASTMakeObject() : ASTNode(ASTT_MAKE_OBJECT, true) {}

void ASTMakeObject::traverse() {
    printf("(make-object\n(\n");
    for (size_t i = 0; i < size(&slots); ++i) {
        printf("(\n");
        slots[i].key->traverse();
        slots[i].value->traverse();
        printf(")\n");
    }
    printf(")\n)\n");
}

void ASTMakeObject::emit(VM *vm, Scope scope,
                         Object *valueToConstantSlot) {
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addR(&vm->funcProtos[scope.protoID], OP_MAKE_OBJECT, reg.regOrConstant);
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
    }
    for (size_t i = 0; i < size(&slots); ++i) {
        slots[i].key->freeRegister(scope);
        slots[i].value->freeRegister(scope);
    }
}

Value ASTMakeObject::getRegister(VM *vm, Scope scope,
                                 Object *valueToConstantSlot) {
    return reg;
}

void ASTMakeObject::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTDefine::ASTDefine() : ASTNode(ASTT_DEFINE, false), var(Value{V_SYMBOL}) {}

void ASTDefine::traverse() {
    printf("(define\n");
    printValue(var);
    printf("\n");
    expr->traverse();
    printf(")\n");
}

void ASTDefine::emit(VM *vm, Scope scope,
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
    expr->freeRegister(scope);
}

Value ASTDefine::getRegister(VM *vm, Scope scope,
                             Object *valueToConstantSlot) {
    fprintf(stderr, "ERROR: define can't be used as an expression\n");
    assert(false);
}

void ASTDefine::freeRegister(Scope scope) {
}

ASTIf::ASTIf() : ASTNode(ASTT_IF, true) {}

void ASTIf::traverse() {
    printf("If (\n");
    pred->traverse();
    trueBranch->traverse();
    if (falseBranch) {
        falseBranch->traverse();
    }
    printf(")\n");
}

void ASTIf::emit(VM *vm, Scope scope,
                 Object *valueToConstantSlot) {
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    pred->emit(vm, scope, valueToConstantSlot);
    pred->freeRegister(scope);
    size_t ifBranchPos = getPos(&vm->funcProtos[scope.protoID]);
    addN(&vm->funcProtos[scope.protoID], OP_DUMMY);
    int64 trueStart = getPos(&vm->funcProtos[scope.protoID]);
    trueBranch->emit(vm, scope, valueToConstantSlot);
    trueBranch->freeRegister(scope);
    if (trueBranch->hasReg) {
        addRR(&vm->funcProtos[scope.protoID], OP_MOVE,
              reg.regOrConstant,
              trueBranch->getRegister(vm, scope,
                                      valueToConstantSlot).regOrConstant);
    } else {
        addR(&vm->funcProtos[scope.protoID], OP_LOAD_UNDEF,
             reg.regOrConstant);
    }
    size_t trueBranchPos = getPos(&vm->funcProtos[scope.protoID]);
    int64 trueBranchEnd = getPos(&vm->funcProtos[scope.protoID]);
    if (falseBranch) {
        addN(&vm->funcProtos[scope.protoID], OP_DUMMY);
        trueBranchEnd = getPos(&vm->funcProtos[scope.protoID]);
        falseBranch->emit(vm, scope, valueToConstantSlot);
        falseBranch->freeRegister(scope);
        if (falseBranch->hasReg) {
            addRR(&vm->funcProtos[scope.protoID], OP_MOVE,
                  reg.regOrConstant,
                  falseBranch->getRegister(vm, scope,
                                           valueToConstantSlot).regOrConstant);
        } else {
            addR(&vm->funcProtos[scope.protoID], OP_LOAD_UNDEF,
                 reg.regOrConstant);
        }
        int64 endPos = getPos(&vm->funcProtos[scope.protoID]);
        patchS(&vm->funcProtos[scope.protoID], trueBranchPos,
               OP_JMP,
               endPos - trueBranchEnd);
    }
    patchRS(&vm->funcProtos[scope.protoID], ifBranchPos,
            OP_JMP_IF_FALSE,
            pred->getRegister(vm, scope,
                              valueToConstantSlot).regOrConstant,
            trueBranchEnd - trueStart);
}

Value ASTIf::getRegister(VM *vm, Scope scope,
                         Object *valueToConstantSlot) {
    return reg;
}

void ASTIf::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTLet::ASTLet() : ASTNode(ASTT_LET, true) {}

void ASTLet::traverse() {
    printf("(let\n");
    var.traverse();
    printf("\n");
    expr->traverse();
    printf(")\n");
}

void ASTLet::emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
    var.setRegister(vm, scope);
    expr->emit(vm, scope, valueToConstantSlot);
    addRR(&vm->funcProtos[scope.protoID], OP_MOVE,
          var.getRegister(vm, scope, valueToConstantSlot).regOrConstant,
          expr->getRegister(vm, scope,
                            valueToConstantSlot).regOrConstant);
    expr->freeRegister(scope);
}

Value ASTLet::getRegister(VM *vm, Scope scope,
                          Object *valueToConstantSlot) {
    return var.getRegister(vm, scope, valueToConstantSlot);
}

void ASTLet::freeRegister(Scope scope) {
}

ASTSet::ASTSet() : ASTNode(ASTT_SET, true) {}

void ASTSet::traverse() {
    printf("(set!\n");
    var.traverse();
    printf("\n");
    expr->traverse();
    printf(")\n");
}

void ASTSet::emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
    expr->emit(vm, scope, valueToConstantSlot);
    uint8 exprReg = expr->getRegister(vm, scope,
                                  valueToConstantSlot).regOrConstant;

    if (!keyExists(scope.symToReg, var.symbol)) {
        set(vm, scope.symToReg, var.symbol,
            allocReg(&vm->funcProtos[scope.protoID], scope, true));
    }
    Value varReg = get(scope.symToReg, var.symbol);
    if (varReg.nonLocal) {
        uint8 upvalueIdx;
        if (getUpvalue(vm, scope, var.symbol, &upvalueIdx)) {
            // Upval
            addRI(&vm->funcProtos[scope.protoID], OP_SET_UPVALUE,
                  exprReg, upvalueIdx);
        } else {
            // Global
            Value k;
            if (keyExists(valueToConstantSlot, var.symbol)) {
                k = get(valueToConstantSlot, var.symbol);
            } else {
                k = allocConstant(&vm->funcProtos[scope.protoID],
                                  var.symbol);
                set(vm, valueToConstantSlot, var.symbol, k);
            }
            addRI(&vm->funcProtos[scope.protoID], OP_SET_GLOBAL,
                  exprReg, k.regOrConstant);
        }
    } else {
        // Local
        addRR(&vm->funcProtos[scope.protoID], OP_MOVE,
              varReg.regOrConstant, exprReg);
    }
    expr->freeRegister(scope);
}

Value ASTSet::getRegister(VM *vm, Scope scope,
                          Object *valueToConstantSlot) {
    return var.getRegister(vm, scope, valueToConstantSlot);
}

void ASTSet::freeRegister(Scope scope) {
}

ASTList::ASTList() : ASTNode(ASTT_LIST, true) {}

void ASTList::traverse() {
    printf("(list\n");
    for (size_t i = 0; i < size(&elems); ++i) {
        elems[i]->traverse();
    }
    printf(")");
}

void ASTList::emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) {
    // needs to be done in reverse, so ugly as fuck.
    if (size(&elems)) {
        size_t carReg;
        size_t cdrReg = allocReg(&vm->funcProtos[scope.protoID],
                                 scope).regOrConstant;
        size_t headReg = allocReg(&vm->funcProtos[scope.protoID],
                                  scope).regOrConstant;
        addR(&vm->funcProtos[scope.protoID], OP_LOAD_NULL,
             headReg);
        for (int i = size(&elems)-1; i >= 0; --i) {
            elems[i]->emit(vm, scope, valueToConstantSlot);
            freeReg(scope, cdrReg);
            cdrReg = headReg;
            carReg =
                elems[i]->getRegister(vm, scope,
                                      valueToConstantSlot).regOrConstant;
            headReg = allocReg(&vm->funcProtos[scope.protoID],
                               scope).regOrConstant;
            addRRR(&vm->funcProtos[scope.protoID], OP_CONS,
                   headReg,
                   carReg,
                   cdrReg);
            elems[i]->freeRegister(scope);
        }
        reg.type = V_REG_OR_CONSTANT;
        reg.regOrConstant = headReg;
    } else {
        reg = allocReg(&vm->funcProtos[scope.protoID], scope);
        addR(&vm->funcProtos[scope.protoID], OP_LOAD_NULL,
             reg.regOrConstant);
    }
}

Value ASTList::getRegister(VM *vm, Scope scope,
                          Object *valueToConstantSlot) {
    return reg;
}

void ASTList::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTDefmacro::ASTDefmacro() : ASTNode(ASTT_DEFMACRO, false) {}
void ASTDefmacro::traverse() {
    printf("Defmacro %s (\n", variable.sym.str);
    argList.traverse();
    body.traverse();
    printf(")\n");
};

void ASTDefmacro::emit(VM *vm, Scope scope,
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
    if (body.hasReg) {
        Value retReg = body.getRegister(vm, newScope,
                                        valueToConstantSlot);
        addR(&vm->funcProtos[newScope.protoID],
             OP_RETURN, retReg.regOrConstant);
    } else {
        fprintf(stderr, "ERROR: macro has to return a value\n");
        assert(false);
        //addN(&vm->funcProtos[newScope.protoID], OP_RETURN_UNDEF);
    }
    Value mac = {V_FUNCTION};
    mac.func = allocFunction(vm, &vm->funcProtos[newScope.protoID]);
    set(vm, &vm->macros, variable, mac);
}

Value ASTDefmacro::getRegister(VM *vm, Scope scope,
                             Object *valueToConstantSlot) {
    fprintf(stderr, "ERROR: defmacro can't be used as an expression\n");
    assert(false);
}

void ASTDefmacro::freeRegister(Scope scope) {
}