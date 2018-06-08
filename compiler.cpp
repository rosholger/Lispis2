#include "compiler.h"
#include "parser.h"
#include <climits>
#include <cstdio>
#include "common.h"

#define error(vm, val, msg)                                 \
    do {                                                    \
        LineInfo lineInfo = getLineInfo(val);               \
        l_fprintf(stderr, "ERROR at %lu:%lu: " msg,         \
                  lineInfo.line, lineInfo.column);          \
        assert(false);                                      \
    } while (false)

#define lerror(vm, val, msg, lrsp, r)                       \
    do {                                                    \
        LineInfo lineInfo = getLineInfo(val);               \
        pushStringV(vm, "ERROR at %lu:%lu: " msg,           \
                    lineInfo.line, lineInfo.column);        \
        *lrsp = LRS_COMPILETIME_ERROR;                      \
        return r;                                           \
    } while (false)

#define errorStr(vm, lrsp, r, str, ...)                   \
    do {                                                \
        pushStringV(vm, str, ##__VA_ARGS__);            \
        *lrsp = LRS_COMPILETIME_ERROR;                  \
        return r;                                       \
    } while(false)

#define STRINGIFY(X) #X
#define TOSTRING(X) STRINGIFY(X)
#define lassertStr(vm, lrsp, b, r, str, ...)                            \
    do {                                                                \
        if (!(b)) {                                                     \
            errorStr(vm, lrsp, r, __FILE__ ":" TOSTRING(__LINE__)       \
                     ": Assertion `" #b                                 \
                     "' failed.\n"                                      \
                     str, ##__VA_ARGS__);                               \
        }                                                               \
    } while(false)


#define firstInListIsType(tree, p_type)                                \
    ((get(vm, (tree)).pair &&                                          \
      get(vm, (tree)).pair->car.type ==                                \
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

void popInstr(FunctionPrototype *func) {
    pop(&func->code);
    pop(&func->lines);
}

void addRRR(FunctionPrototype *func, OpCode op, size_t line,
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
    add(&func->lines, line);
}

void addRI(FunctionPrototype *func, OpCode op, size_t line,
           uint8 reg, uint32 immediateID) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(reg)) << (bitsize(OpCode) -
                                                       bitsize(uint8)*2)) |
                                  immediateID);
    add(&func->code, assembledOp);
    add(&func->lines, line);
}

void addI(FunctionPrototype *func, OpCode op, size_t line,
          uint32 immediateID) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  immediateID);
    add(&func->code, assembledOp);
    add(&func->lines, line);
}

void addRR(FunctionPrototype *func, OpCode op, size_t line,
           uint8 a, uint8 b) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(a)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*2)) |
                                  (((OpCode)(b)) << (bitsize(OpCode) -
                                                     bitsize(uint8)*3)));
    add(&func->code, assembledOp);
    add(&func->lines, line);
}

void addR(FunctionPrototype *func, OpCode op, size_t line,
          uint8 reg) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))) |
                                  (((OpCode)(reg)) << (bitsize(OpCode) -
                                                       bitsize(uint8)*2)));
    add(&func->code, assembledOp);
    add(&func->lines, line);
}

void addN(FunctionPrototype *func, OpCode op, size_t line) {
    OpCode assembledOp = (OpCode)((op << (bitsize(OpCode) -
                                          bitsize(uint8))));
    add(&func->code, assembledOp);
    add(&func->lines, line);
}

void patchRS(FunctionPrototype *func, size_t pos, OpCode op,
             uint8 reg, int32 immediate) {

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

ASTSymbol symbolToAST(Value tree, size_t line, size_t column) {
    assert(tree.type == V_SYMBOL);
    ASTSymbol ret;
    ret.line = line;
    ret.column = column;
    ret.sym = tree;
    return ret;
}

ASTVariable variableToAST(Value tree, size_t line, size_t column) {
    assert(tree.type == V_SYMBOL);
    ASTVariable ret;
    ret.line = line;
    ret.column = column;
    ret.symbol = tree;
    return ret;
}


ASTDouble doubleToAST(Value tree, size_t line, size_t column) {
    assert(tree.type == V_DOUBLE);
    ASTDouble ret;
    ret.line = line;
    ret.column = column;
    ret.value = tree.doub;
    return ret;
}

ASTBoolean booleanToAST(Value tree, size_t line, size_t column) {
    assert(tree.type == V_BOOLEAN);
    ASTBoolean ret;
    ret.line = line;
    ret.column = column;
    ret.value = tree.boolean;
    return ret;
}

ASTString stringToAST(Value tree, size_t line, size_t column) {
    assert(tree.type == V_STRING);
    ASTString ret;
    ret.line = line;
    ret.column = column;
    ret.value = tree;
    return ret;
}

ASTNode *exprToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                   const char *file, LispisReturnStatus *lrs);

#define setDebugInfo(vm, value, node)                   \
    do {                                                \
        LineInfo lineInfo = getLineInfo(value);         \
        (node).line = lineInfo.line;                    \
        (node).column = lineInfo.column;                \
    } while(false)

ASTCall callToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                  const char *file, LispisReturnStatus *lrs) {
    assert(type(vm, tree) == V_CONS_PAIR);
    ASTCall ret;
    if (length(get(vm, tree)) < 1) {
        free(vm, tree);
        lerror(vm, get(vm, tree), "calling to nothing does not work!\n",
               lrs, ret);
    }
    setDebugInfo(vm, get(vm, tree), ret);
    ret.callee = exprToAST(vm, arena, reserve(vm,
                                              get(vm, tree).pair->car),
                           file, lrs);
    if (*lrs != LRS_OK) {
        free(vm, tree);
        return ret;
    }
    // FIXME: Change to lassertStr
    assert(get(vm, tree).pair->cdr.type == V_CONS_PAIR);
    Handle args = reserve(vm, get(vm, tree).pair->cdr);
    while (get(vm, args).pair) {
        add(&ret.args, exprToAST(vm, arena,
                                 reserve(vm, get(vm, args).pair->car),
                                 file, lrs));
        if (*lrs != LRS_OK) {
            free(vm, tree);
            free(vm, args);
            return ret;
        }
        Handle tmp = reserve(vm, get(vm, args).pair->cdr);
        free(vm, args);
        args = tmp;
    }
    free(vm, args);
    free(vm, tree);
    *lrs = LRS_OK;
    return ret;
}

// If the arglist is a symbol we cant get the lineinfo from the value
ASTArgList argListToAST(VM *vm, Handle tree, size_t line, size_t column) {
    ASTArgList ret;
    // We dont use line/column of argList, since it does not generate
    // any code
    if (type(vm, tree) == V_SYMBOL) {
        ret.vararg = true;
        add(&ret.args, variableToAST(get(vm, tree), ret.line,
                                     ret.column));
        free(vm, tree);
    } else {
        ret.vararg = false;
        while (type(vm, tree) == V_CONS_PAIR && get(vm, tree).pair) {
            if (get(vm, tree).pair->car.type != V_SYMBOL) {
                error(vm, get(vm, tree), "Arguments has to be symbols\n");
            }
            add(&ret.args, variableToAST(get(vm, tree).pair->car,
                                         ret.line, ret.column));
            Handle tmp = reserve(vm, get(vm, tree).pair->cdr);
            free(vm, tree);
            tree = tmp;
        }
        if (type(vm, tree) != V_CONS_PAIR) {
            ret.vararg = true;
            add(&ret.args, variableToAST(get(vm, tree), ret.line,
                                         ret.column));
        }
        free(vm, tree);
    }
    return ret;
}

ASTLabel labelToAST(VM *vm, Handle tree, LispisReturnStatus *lrs) {
    ASTLabel ret;
    assert(type(vm, tree) == V_CONS_PAIR);
    if (length(get(vm, tree)) < 2) {
        lerror(vm, get(vm, tree), "label needs a symbol\n", lrs, ret);
    }
    if (at(get(vm, tree), 1).type != V_SYMBOL) {
        lerror(vm, get(vm, tree), "label needs a symbol\n", lrs, ret);
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(get(vm, tree).pair->car == intern(vm, "label"));
    setDebugInfo(vm, get(vm, tree), ret);
    ret.labelSymbol = at(get(vm, tree), 1);
    return ret;
}

ASTGo goToAST(VM *vm, Handle tree, LispisReturnStatus *lrs) {
    ASTGo ret;
    assert(type(vm, tree) == V_CONS_PAIR);
    if (length(get(vm, tree)) < 2) {
        lerror(vm, get(vm, tree), "go needs a symbol\n", lrs, ret);
    }
    if (at(get(vm, tree), 1).type != V_SYMBOL) {
        lerror(vm, get(vm, tree), "go needs a symbol\n", lrs, ret);
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(get(vm, tree).pair->car == intern(vm, "go"));
    setDebugInfo(vm, get(vm, tree), ret);
    ret.labelSymbol = at(get(vm, tree), 1);
    return ret;
}

ASTCrash crashToAST(VM *vm, ArenaAllocator *arena,
                    Handle tree, const char *file,
                    LispisReturnStatus *lrs) {
    ASTCrash ret;
    assert(type(vm, tree) == V_CONS_PAIR);
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(get(vm, tree).pair->car == intern(vm, "crash!"));
    if (length(get(vm, tree)) < 2) {
        lerror(vm, get(vm, tree), "crash needs an expression argument\n",
               lrs, ret);
    }
    setDebugInfo(vm, get(vm, tree), ret);
    ret.crashvalue = exprToAST(vm, arena, reserve(vm, at(get(vm, tree),
                                                         1)), file,
                               lrs);
    return ret;
}

ASTIf ifToAST(VM *vm, ArenaAllocator *arena, Handle tree,
              const char *file, LispisReturnStatus *lrs) {
    assert(type(vm, tree) == V_CONS_PAIR);
    ASTIf ret;
    if (length(get(vm, tree)) < 3) {
        lerror(vm, get(vm, tree),
               "if needs atleast predicate and a true branch\n", lrs,
               ret);
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(get(vm, tree).pair->car == intern(vm, "if"));
    setDebugInfo(vm, get(vm, tree), ret);
    ret.pred = exprToAST(vm, arena, reserve(vm, at(get(vm, tree), 1)),
                         file, lrs);
    if (*lrs != LRS_OK) {
        free(vm, tree);
        return ret;
    }
    ret.trueBranch = exprToAST(vm, arena,
                               reserve(vm, at(get(vm, tree), 2)),
                               file, lrs);
    if (*lrs != LRS_OK) {
        free(vm, tree);
        return ret;
    }
    if (length(get(vm, tree)) == 4) {
        ret.falseBranch = exprToAST(vm, arena,
                                    reserve(vm, at(get(vm, tree), 3)),
                                    file, lrs);
        if (*lrs != LRS_OK) {
            free(vm, tree);
            return ret;
        }
    } else {
        ret.falseBranch = 0;
    }
    free(vm, tree);
    *lrs = LRS_OK;
    return ret;
}

ASTBody bodyToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                  const char *file);

ASTLambda lambdaToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                      const char *file) {
    ASTLambda ret;
    ret.file = strdup(file);
    ret.nameSymbol = intern(vm, "*anonymous*");
    setDebugInfo(vm, get(vm, tree), ret);
    ret.argList = argListToAST(vm,
                               reserve(vm, get(vm, tree).pair->car),
                               ret.line, ret.column);
    ret.body = bodyToAST(vm, arena,
                         reserve(vm, get(vm, tree).pair->cdr), file);
    free(vm, tree);
    return ret;
}

ASTDefmacro defmacroToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                          const char *file) {
    assert(type(vm, tree) == V_CONS_PAIR);
    ASTDefmacro ret;
    ret.file = strdup(file);
    if (length(get(vm, tree)) < 4) {
        error(vm, get(vm, tree),
              "defmacro needs a variable, "
              "an argument list and a body!\n");
    }
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(get(vm, tree).pair->car == intern(vm, "defmacro"));
    setDebugInfo(vm, get(vm, tree), ret);
    Value afterDefmacroSym = get(vm, tree).pair->cdr;
    if (afterDefmacroSym.pair->car.type != V_SYMBOL) {
        l_fprintf(stderr, "defmacro variable not a symbol!");
        assert(false);
    }
    ret.variable = afterDefmacroSym.pair->car;
    ret.argList =
        argListToAST(vm, reserve(vm,
                                 afterDefmacroSym.pair->cdr.pair->car),
                     ret.line, ret.column);
    ret.body = bodyToAST(vm, arena,
                         reserve(vm,
                                 afterDefmacroSym.pair->cdr.pair->cdr),
                         file);
    free(vm, tree);
    return ret;
}

ASTNode *quotedToAST(VM *vm, ArenaAllocator *arena,
                     Value tree, size_t line,
                     size_t column, LispisReturnStatus *lrs) {
    ASTNode *ret = 0;
    switch (tree.type) {
        case V_DOUBLE: {
            ASTDouble *c = alloc<ASTDouble>(arena);
            *c = doubleToAST(tree, line, column);
            ret = c;
        } break;
        case V_STRING: {
            ASTString *c = alloc<ASTString>(arena);
            *c = stringToAST(tree, line, column);
            ret = c;
        } break;
        case V_BOOLEAN: {
            ASTBoolean *c = alloc<ASTBoolean>(arena);
            *c = booleanToAST(tree, line, column);
            ret = c;
        } break;
        case V_SYMBOL: {
            ASTSymbol *c = alloc<ASTSymbol>(arena);
            *c = symbolToAST(tree, line, column);
            ret = c;
        } break;
        case V_CONS_PAIR: {
            ASTList *c = alloc<ASTList>(arena);
            if (tree.pair) {
                setDebugInfo(vm, tree, *c);
            } else {
                c->line = line;
                c->column = column;
            }
            c->dotted = false;
            while (tree.type == V_CONS_PAIR && tree.pair) {
                add(&c->elems, quotedToAST(vm, arena, tree.pair->car,
                                           line, column, lrs));
                tree = tree.pair->cdr;
            }
            if (tree.type != V_CONS_PAIR) {
                c->dotted = true;
                add(&c->elems,
                    quotedToAST(vm, arena, tree, line, column, lrs));
            }
            ret = c;
        } break;
        default: {
            lassertStr(vm, lrs, false, 0, "ERROR at %lu:%lu: %d cant be quoted yet\n");
        } break;
    }
    *lrs = LRS_OK;
    return ret;
}

MakeObjectSlot makeObjectSlot(VM *vm, ArenaAllocator *arena, Handle tree,
                              const char *file) {
    MakeObjectSlot ret;
    if (type(vm, tree) != V_CONS_PAIR ||
        length(get(vm, tree)) != 2) {
        // FIXME
        l_fprintf(stderr, "ERROR: make-object's argument has to be an associative list\n");
        assert(false);
    }
    LispisReturnStatus lrs = LRS_OK;
    ret.key = exprToAST(vm, arena, reserve(vm, get(vm, tree).pair->car),
                        file, &lrs);
    if (lrs != LRS_OK) {
        printValue(vm, pop(vm), stderr);
        assert(false);
    }
    ret.value = exprToAST(vm, arena,
                          reserve(vm, get(vm, tree).pair->cdr.pair->car),
                          file, &lrs);
    if (lrs != LRS_OK) {
        printValue(vm, pop(vm), stderr);
        assert(false);
    }
    free(vm, tree);
    return ret;
}

ASTMakeObject makeObjectToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                              const char *file) {
    ASTMakeObject ret;
    setDebugInfo(vm, get(vm, tree), ret);
    if (type(vm, tree) != V_CONS_PAIR) {
        error(vm, get(vm, tree),
              "make-object's argument has to be an associative list\n");
    }
    resize(&ret.slots, length(get(vm, tree)));
    for (size_t i = 0; get(vm, tree).pair; ++i) {
        ret.slots[i] = makeObjectSlot(vm, arena,
                                      reserve(vm, get(vm,
                                                      tree).pair->car),
                                      file);
        Handle tmp = reserve(vm, get(vm, tree).pair->cdr);
        free(vm, tree);
        tree = tmp;
    }
    free(vm, tree);
    return ret;
}

ASTDefine defineToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                      const char *file) {
    ASTDefine ret;
    assert(type(vm, tree) == V_CONS_PAIR);
    assert(get(vm, tree).pair);
    assert(get(vm, tree).pair->cdr.type == V_CONS_PAIR);
    assert(get(vm, tree).pair->cdr.pair);
    if (get(vm, tree).pair->car.type != V_SYMBOL) {
        error(vm, get(vm, tree),
              "Can only define symbols as variables\n");
    }
    // FIXME
    setDebugInfo(vm, get(vm, tree), ret);
    ret.var = get(vm, tree).pair->car;
    LispisReturnStatus lrs = LRS_OK;
    if (length(get(vm, tree)) == 2) {
        ret.expr = exprToAST(vm, arena,
                             reserve(vm,
                                     get(vm, tree).pair->cdr.pair->car),
                             file, &lrs);
        if (lrs != LRS_OK) {
            printValue(vm, pop(vm), stderr);
            assert(false);
        }
    } else if (length(get(vm, tree)) > 2) {
        ASTLambda *p = alloc<ASTLambda>(arena);
        Handle lambdaPart = reserve(vm, get(vm, tree).pair->cdr);
        *p = lambdaToAST(vm, arena, lambdaPart, file);
        setDebugInfo(vm, get(vm, tree), *p);
        p->nameSymbol = ret.var.sym;
        ret.expr = p;
    } else {
        error(vm, get(vm, tree),
              "define needs both a symbol and an expression!\n");
    }
    free(vm, tree);
    return ret;
}

ASTLet letToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                const char *file, LispisReturnStatus *lrs) {
    ASTLet ret;
    assert(type(vm, tree) == V_CONS_PAIR);
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(get(vm, tree).pair->car == intern(vm, "let"));
    setDebugInfo(vm, get(vm, tree), ret);
    ret.var = variableToAST(at(get(vm, tree), 1), ret.line, ret.column);
    if (length(get(vm, tree)) == 3) {
        ret.expr = exprToAST(vm, arena,
                             reserve(vm, at(get(vm, tree), 2)), file,
                             lrs);
    } else if (length(get(vm, tree)) > 3) {
        ASTLambda *p = alloc<ASTLambda>(arena);
        Handle lambdaPart = reserve(vm,
                    get(vm, tree).pair->cdr.pair->cdr);
        *p = lambdaToAST(vm, arena, lambdaPart, file);
        setDebugInfo(vm, get(vm, tree), *p);
        p->nameSymbol = ret.var.symbol.sym;
        ret.expr = p;
    } else {
        free(vm, tree);
        lerror(vm, get(vm, tree),
               "let needs both a symbol and an expression!\n", lrs, ret);
    }
    free(vm, tree);
    return ret;
}

ASTSet setToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                const char *file, LispisReturnStatus *lrs) {
    ASTSet ret;
    assert(type(vm, tree) == V_CONS_PAIR);
    if (length(get(vm, tree)) != 3) {
        free(vm, tree);
        lerror(vm, get(vm, tree),
               "set! needs both a symbol and an expression!\n", lrs, ret);
    }
    setDebugInfo(vm, get(vm, tree), ret);
    assert(firstInListIsType(tree, V_SYMBOL));
    assert(get(vm, tree).pair->car == intern(vm, "set!"));
    ret.var = variableToAST(at(get(vm, tree), 1), ret.line, ret.column);
    ret.expr = exprToAST(vm, arena,
                         reserve(vm, at(get(vm, tree), 2)), file, lrs);
    free(vm, tree);
    return ret;
}

void setMacroExpansionLineInfo(VM *vm, Handle tree, LineInfo info) {
    if (type(vm, tree) == V_CONS_PAIR && get(vm, tree).pair) {
        if (!hasLineInfo(get(vm, tree))) {
            setLineInfo(vm, tree, info.line, info.column);
        }
        setMacroExpansionLineInfo(vm, reserve(vm,
                                              get(vm, tree).pair->car),
                                  info);
        setMacroExpansionLineInfo(vm, reserve(vm,
                                              get(vm, tree).pair->cdr),
                                  info);
    }
    free(vm, tree);
    //info.value = get(vm, tree);
    //add(&vm->staticDebugInfo, info);
    //setMacroExpansionLineInfo(vm, reserve(vm,
    //get(vm, tree).pair->car),
    //info);
    //setMacroExpansionLineInfo(vm, reserve(vm,
    //get(vm, tree).pair->cdr),
    //info);
    //}
}

void assertLineInfo(VM *vm, Value tree) {
    if (tree.type == V_CONS_PAIR && tree.pair) {
        assert(tree.pair->lineInfo);
        assertLineInfo(vm, tree.pair->car);
        assertLineInfo(vm, tree.pair->cdr);
    }
}

Handle expandMacro(VM *vm, Handle tree, LispisReturnStatus *lrs) {
    Value macro = get(vm, &vm->macros, get(vm, tree).pair->car);
    Value argList = get(vm, tree).pair->cdr;
    assert(argList.type == V_CONS_PAIR);
    uint8 numArgs = 0;
    while (argList.pair) {
        numArgs++;
        pushValue(vm, argList.pair->car);
        argList = argList.pair->cdr;
        Handle nullHandle{12345678};
        LineInfo lf = getLineInfo(get(vm, tree));
        lassertStr(vm, lrs, argList.type == V_CONS_PAIR,
                   nullHandle, "ERROR at %lu:%lu: Dotted macro call",
                   lf.line, lf.column);
    }
    pushValue(vm, macro);
    *lrs = call(vm, numArgs);

    if (*lrs != LRS_OK) {
        free(vm, tree);
        *lrs = LRS_MACRO_ERROR;
        Handle nullHandle{12345678};
        return nullHandle;
    }

    if (peek(vm, -1).type == V_UNDEF) {
        // Hack to insert a noop
        pushSymbol(vm, "scope");
        pushNull(vm);
        cons(vm);
    }
        
    Handle ret = reserve(vm, pop(vm));
    if (type(vm, ret) == V_CONS_PAIR) {
        LineInfo info = getLineInfo(get(vm, tree));
        setMacroExpansionLineInfo(vm, reserve(vm, get(vm, ret)), info);
        setLineInfo(vm, ret, info.line, info.column);
    }
    assertLineInfo(vm, get(vm, ret));
    free(vm, tree);
    *lrs = LRS_OK;
    return ret;
}

ASTNode *exprToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                   const char *file, LispisReturnStatus *lrs) {
    // We need the previouse expressions lineinfo to give to
    // doubleToAST et. al.
    ASTNode *ret = 0;
    switch (type(vm, tree)) {
        case V_DOUBLE: {
            ASTDouble *c = alloc<ASTDouble>(arena);
            *c = doubleToAST(get(vm, tree), 1234567, 1234567);
            ret = c;
            free(vm, tree);
        } break;
        case V_BOOLEAN: {
            ASTBoolean *c = alloc<ASTBoolean>(arena);
            *c = booleanToAST(get(vm, tree), 1234567, 1234567);
            ret = c;
            free(vm, tree);
        } break;
        case V_SYMBOL: {
            ASTVariable *c = alloc<ASTVariable>(arena);
            *c = variableToAST(get(vm, tree), 1234567, 12334567);
            ret = c;
            free(vm, tree);
        } break;
        case V_STRING: {
            ASTString *c = alloc<ASTString>(arena);
            *c = stringToAST(get(vm, tree), 1234567, 12334567);
            ret = c;
            free(vm, tree);
        } break;
        case V_CONS_PAIR: {
            LineInfo lineInfo = getLineInfo(get(vm, tree));
            if (firstInListIsType(tree, V_SYMBOL) &&
                get(vm, tree).pair->car == intern(vm, "lambda")) {
                ASTLambda *c = alloc<ASTLambda>(arena);
                assert(type(vm, tree) == V_CONS_PAIR);
                if (length(get(vm, tree)) < 3) {
                    lerror(vm, get(vm, tree),
                           "lambda needs both an argument"
                           " list and a body!\n", lrs, 0);
                }
                *c = lambdaToAST(vm, arena,
                                 reserve(vm, get(vm, tree).pair->cdr),
                                 file);
                setDebugInfo(vm, get(vm, tree), *c);
                free(vm, tree);
                if (*lrs != LRS_OK) {
                    return 0;
                }
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "if")) {
                ASTIf *c = alloc<ASTIf>(arena);
                *c = ifToAST(vm, arena, tree, file, lrs);
                if (*lrs != LRS_OK) {
                    return 0;
                }
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "label")) {
                ASTLabel *c = alloc<ASTLabel>(arena);
                *c = labelToAST(vm, tree, lrs);
                free(vm, tree);
                if (*lrs != LRS_OK) {
                    return 0;
                }
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "go")) {
                ASTGo *c = alloc<ASTGo>(arena);
                *c = goToAST(vm, tree, lrs);
                free(vm, tree);
                if (*lrs != LRS_OK) {
                    return 0;
                }
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "crash!")) {
                ASTCrash *c = alloc<ASTCrash>(arena);
                *c = crashToAST(vm, arena, tree, file, lrs);
                free(vm, tree);
                if (*lrs != LRS_OK) {
                    return 0;
                }
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "defmacro")) {
                ASTDefmacro *c = alloc<ASTDefmacro>(arena);
                *c = defmacroToAST(vm, arena, tree, file);
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "let")) {
                ASTLet *c = alloc<ASTLet>(arena);
                *c = letToAST(vm, arena, tree, file, lrs);
                if (*lrs != LRS_OK) {
                    return 0;
                }
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "set!")) {
                ASTSet *c = alloc<ASTSet>(arena);
                *c = setToAST(vm, arena, tree, file, lrs);
                if (*lrs != LRS_OK) {
                    return 0;
                }
                ret = c;
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "scope")) {
                ASTBody *c = alloc<ASTBody>(arena);
                *c = bodyToAST(vm, arena,
                               reserve(vm, get(vm, tree).pair->cdr),
                               file);
                ret = c;
                free(vm, tree);
                if (*lrs != LRS_OK) {
                    return 0;
                }
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "define")) {
                if (length(get(vm, tree)) < 3) {
                    lerror(vm, get(vm, tree),
                           "define takes two arguments!\n", lrs, 0);
                }
                ASTDefine *c = alloc<ASTDefine>(arena);
                *c = defineToAST(vm, arena,
                                 reserve(vm, get(vm, tree).pair->cdr),
                                 file);
                ret = c;
                free(vm, tree);
                if (*lrs != LRS_OK) {
                    return 0;
                }
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car == intern(vm, "quote")) {
                if (length(get(vm, tree)) != 2) {
                    lerror(vm, get(vm, tree),
                           "quote takes only one argument!\n", lrs, 0);
                }
                ret = quotedToAST(vm, arena, at(get(vm, tree), 1),
                                  lineInfo.line, lineInfo.column, lrs);
                free(vm, tree);
                if (*lrs != LRS_OK) {
                    return 0;
                }
            } else if (firstInListIsType(tree, V_SYMBOL) &&
                       get(vm, tree).pair->car ==
                       intern(vm, "make-object")) {
                if (length(get(vm, tree)) != 2) {
                    lerror(vm, get(vm, tree),
                           "make-object takes only one argument!\n", lrs,
                           0);
                }
                ASTMakeObject *c = alloc<ASTMakeObject>(arena);
                *c = makeObjectToAST(vm, arena,
                                     reserve(vm, at(get(vm, tree), 1)),
                                     file);
                ret = c;
                free(vm, tree);
                if (*lrs != LRS_OK) {
                    return 0;
                }
            } else {
                if (get(vm, tree).pair &&
                    get(vm, tree).pair->car.type == V_SYMBOL &&
                    keyExists(vm, &vm->macros,
                              get(vm, tree).pair->car)) {

                    Handle newTree = expandMacro(vm, tree, lrs);
                    if (*lrs != LRS_OK) {
                        return 0;
                    }
                    ret = exprToAST(vm, arena, newTree,
                                    file, lrs);
                    if (*lrs != LRS_OK) {
                        return 0;
                    }
                } else {
                    ASTCall *c = alloc<ASTCall>(arena);
                    *c = callToAST(vm, arena, tree, file, lrs);
                    if (*lrs != LRS_OK) {
                        return 0;
                    }
                    ret = c;
                }
            }
        } break;
        default: {
            lassertStr(vm, lrs,
                       false, 0, "%d cant be converted to ast yet\n", type(vm, tree));
        } break;
    }
    assert(ret);
    *lrs = LRS_OK;
    return ret;
}

ASTBody bodyToAST(VM *vm, ArenaAllocator *arena, Handle tree,
                  const char *file) {
    assert(type(vm, tree) == V_CONS_PAIR);
    ASTBody ret;
    if (get(vm, tree).pair) {
        setDebugInfo(vm, get(vm, tree), ret);
    } else {
        ret.line = 1234;
        ret.column = 1234;
    }
    while (get(vm, tree).pair) {
        assert(type(vm, tree) == V_CONS_PAIR);
        Handle tmp = reserve(vm, get(vm, tree).pair->car);
        LispisReturnStatus lrs = LRS_OK;
        add(&ret.body, exprToAST(vm, arena, tmp, file, &lrs));
        if (lrs != LRS_OK) {
            printValue(vm, pop(vm), stderr);
            assert(false);
        }
        tmp = reserve(vm, get(vm, tree).pair->cdr);
        free(vm, tree);
        tree = tmp;
    }
    free(vm, tree);
    return ret;
}

void printReserved(VM *vm) {
    for (size_t i = 0; i < size(&vm->handles); ++i) {
        if (vm->handles[i].type != V_UNDEF) {
            l_fprintf(stdout, "%lu ", i);
        }
    }
    l_fprintf(stdout, "\n");
}

LispisReturnStatus patchGoStatements(VM *vm, Scope scope) {
    for (size_t i = 0; i < size(scope.goLabelPositions); ++i) {
        Value label = (*scope.goLabelPositions)[i].key;
        if (!keyExists(vm, scope.labelPositions, label)) {
            pushStringV(vm, "Label does not exist\n");
            return LRS_COMPILETIME_ERROR;
        }
        int64 target = get(vm, scope.labelPositions, label).codePosition;
        int64 source = (*scope.goLabelPositions)[i].value.codePosition;
        patchS(&vm->funcProtos[scope.protoID], source, OP_JMP,
               target - source);
        //source - (target + 1)); // + 1 or not?
    }
    return LRS_OK;
}

// WARNING!!! adds stuff to the current scope!!
#define allocScope(s, par, line, f, ns)                 \
    Object _newSymToReg;                                \
    DynamicArray<size_t> _newFreeRegisters;             \
    Object _newLabelPositions;                          \
    DynamicArray<ObjectSlot> _newGoLabelPositions;      \
    Object _newValueToConstantSlot;                     \
    s.protoID = size(&vm->funcProtos);                  \
    add(&vm->funcProtos, FunctionPrototype());          \
    vm->funcProtos[s.protoID].definedOnLine = line;     \
    vm->funcProtos[s.protoID].file = f;                 \
    vm->funcProtos[s.protoID].nameSymbol = ns;          \
    s.parent = par;                                     \
    s.symToReg = &_newSymToReg;                         \
    s.freeRegisters = &_newFreeRegisters;               \
    s.labelPositions = &_newLabelPositions;             \
    s.goLabelPositions = &_newGoLabelPositions;         \
    s.valueToConstantSlot = &_newValueToConstantSlot

LispisReturnStatus doString(VM *vm, const char *prog, bool verbose,
                                const char *filePath) {
    LexState lex;
    LispisReturnStatus lrs = initLexerState(vm, prog, &lex);
    if (lrs != LRS_OK) {
        return lrs;
    }
    Scope topScope;
    if (!filePath) {
        filePath = "from C++";
    }
    allocScope(topScope, 0, 0, strdup(filePath), intern(vm, filePath)); 
    ASTNode *node = 0;
    ArenaAllocator arena;
    Value func = {V_FUNCTION};
    func.func = allocFunction(vm, topScope.protoID);
    size_t topFrameID = allocFrame(vm, func.func);
    {
        Value root = allocConsPair(vm);
        Handle handle = reserve(vm, root);
        setLineInfo(vm, root, 1, 0);
        Handle currTree = handle;
        while (peekToken(&lex).type != T_EOF) {
            freeArena(&arena);
            LispisReturnStatus lrs =
                parseExpr(vm, &lex, currTree);
            if (lrs != LRS_OK) {
                freeArena(&arena);
                free(vm, currTree);
                return lrs;
            }
            if (verbose) {
                l_fprintf(stdout, "\n");
                printValue(vm, get(vm, currTree).pair->car);
                l_fprintf(stdout, "\n\n");
            }

            assertLineInfo(vm, get(vm, currTree).pair->car);
            
            node = exprToAST(vm, &arena,
                             reserve(vm, get(vm, currTree).pair->car),
                             filePath, &lrs);
            if (lrs != LRS_OK) {
                freeArena(&arena);
                free(vm, currTree);
                return lrs;
            }
            if (verbose) {
                node->traverse(vm);
            }
            node->emit(vm, topScope);
            size_t numberOfInstr = size(&vm->funcProtos[topScope.protoID].lines);
            if (numberOfInstr) {
                size_t line = 
                    vm->funcProtos[topScope.protoID].lines[numberOfInstr-1];
                addN(&vm->funcProtos[topScope.protoID], OP_NO_RET_RET,
                     line);
                // This breaks (go)ing to a label defined after the
                // go statement at top-level
                if (patchGoStatements(vm, topScope) != LRS_OK) {
                    return LRS_COMPILETIME_ERROR;
                }
                resizeFrame(vm, topFrameID);
                lrs = runFunc(vm, topFrameID);
                if (lrs != LRS_OK) {
                    return lrs; // FIXME
                }
                popInstr(&vm->funcProtos[topScope.protoID]);
            }
            if (peekToken(&lex).type != T_EOF) {
                Value p = allocConsPair(vm);
                get(vm, currTree).pair->cdr = p;
                Handle temp = reserve(vm, get(vm, currTree).pair->cdr.pair);
                free(vm, currTree);
                currTree = temp;
                node->freeRegister(topScope);
            } else {
                free(vm, currTree);
            }
        }
    }
    lassertStr(vm, &lrs, node, LRS_COMPILETIME_ERROR,
                "ERROR: Empty 'file' is not allowed\n");
    size_t returnLine = 0;
    if (size(&vm->funcProtos[topScope.protoID].lines)) {
        returnLine = last(&vm->funcProtos[topScope.protoID].lines);
    }
    if (node->hasReg) {
        Value retReg = node->getRegister(vm, topScope);
        addR(&vm->funcProtos[topScope.protoID], OP_RETURN,
             returnLine,
             retReg.regOrConstant);
    } else {
        addN(&vm->funcProtos[topScope.protoID], OP_RETURN_UNDEF,
             returnLine);
    }
    if (patchGoStatements(vm, topScope) != LRS_OK) {
        return LRS_COMPILETIME_ERROR;
    }
    runFunc(vm, topFrameID);
    freeArena(&arena);
    return LRS_OK;
}

LispisReturnStatus doFile(VM *vm, const char *path, bool verbose) {
    FILE *file = fopen(path, "r");
    long int length;
    fseek(file, 0, SEEK_END);
    length = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *prog = (char *)malloc(length+1);
    prog[length] = 0;
    fread(prog, sizeof(char), length, file);
    fclose(file);
    return doString(vm, prog, verbose, path);
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
               keyExists(vm, currScope->parent->symToReg, variable)) {
                found = true;
                uint8 reg = get(vm, currScope->parent->symToReg,
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
            for (int64 i = (int64)size(&scopes)-1;
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

void ASTBody::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    for (size_t i = 0; i < size(&body); ++i) {
        body[i]->traverse(vm);
    }
}

void ASTBody::emit(VM *vm, Scope scope) {
    if (size(&body)) {
        for (size_t i = 0; i < size(&body); ++i) {
            body[i]->emit(vm, scope);
            body[i]->freeRegister(scope);
        }
        hasReg = body[size(&body)-1]->hasReg;
    }
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

Value ASTBody::getRegister(VM *vm, Scope scope) {
    assert(hasReg);
    return body[size(&body)-1]->getRegister(vm, scope);
}

void ASTBody::freeRegister(Scope scope) {}
ASTSymbol::ASTSymbol() : ASTNode(ASTT_SYMBOL, true) {}
void ASTSymbol::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    printValue(vm, sym);
    l_fprintf(stdout, " %d\n", sym.sym.id);
}
void ASTSymbol::emit(VM *vm, Scope scope) {
    Value k;
    if (keyExists(vm, scope.valueToConstantSlot, sym)) {
        k = get(vm, scope.valueToConstantSlot, sym);
    } else {
        k = allocConstant(&vm->funcProtos[scope.protoID], sym);
        set(vm, scope.valueToConstantSlot, sym, k);
    }
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addRI(&vm->funcProtos[scope.protoID], OP_LOADK, line,
          reg.regOrConstant, k.regOrConstant);
}
Value ASTSymbol::getRegister(VM *vm, Scope scope) {
    return reg;
}
void ASTSymbol::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

// Hmmmm since we have a register machine a-normal form
// would be *very* good, or until then an enum would work.
ASTVariable::ASTVariable() : ASTNode(ASTT_VARIABLE, true),
                             symbol(Value{V_SYMBOL}) {}
void ASTVariable::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    //Value symStr = getFirstKey(&vm->symbolTable, symbol);
    printValue(vm, symbol);
    l_fprintf(stdout, " %d\n", symbol.sym.id);
};

void ASTVariable::emit(VM *vm, Scope scope) {
    if (!keyExists(vm, scope.symToReg, symbol)) {
        set(vm, scope.symToReg, symbol,
            allocReg(&vm->funcProtos[scope.protoID], scope, true));
    }
    Value reg = get(vm, scope.symToReg, symbol);
    if (reg.nonLocal) {
        uint8 upvalueIdx;
        if (getUpvalue(vm, scope, symbol, &upvalueIdx)) {
            // Upval
            addRI(&vm->funcProtos[scope.protoID], OP_GET_UPVALUE, line,
                  reg.regOrConstant, upvalueIdx);
        } else {
            // Global
            Value k;
            if (keyExists(vm, scope.valueToConstantSlot, symbol)) {
                k = get(vm, scope.valueToConstantSlot, symbol);
            } else {
                k = allocConstant(&vm->funcProtos[scope.protoID],
                                  symbol);
                set(vm, scope.valueToConstantSlot, symbol, k);
            }
            addRI(&vm->funcProtos[scope.protoID], OP_GET_GLOBAL, line,
                  reg.regOrConstant, k.regOrConstant);
        }
    } else {
        // Local
        
        // NOOP
    }
}

// Only (let var val) and arguments
void ASTVariable::setRegister(VM *vm, Scope scope) {
    //assert(!keyExists(vm, scope.symToReg, symbol));
    set(vm, scope.symToReg, symbol,
        allocReg(&vm->funcProtos[scope.protoID], scope));
}

Value ASTVariable::getRegister(VM *vm, Scope scope) {
    assert(keyExists(vm, scope.symToReg, symbol));
    return get(vm, scope.symToReg, symbol);
}

void ASTVariable::freeRegister(Scope scope) {
}

ASTArgList::ASTArgList() : ASTNode(ASTT_ARGLIST, true) {}
void ASTArgList::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "ArgList ");
    if (vararg) {
        if (size(&args) > 1) {
            for (size_t i = 0; i < size(&args) - 1; ++i) {
                args[i].traverse(vm);
            }
            l_fprintf(stdout, ".\n");
            last(&args).traverse(vm);
        } else {
            args[0].traverse(vm);
        }
    } else {
        l_fprintf(stdout, "(\n");
        for (size_t i = 0; i < size(&args); ++i) {
            args[i].traverse(vm);
        }
        l_fprintf(stdout, ")\n");
    }
};

void ASTArgList::emit(VM *vm, Scope scope) {
    vm->funcProtos[scope.protoID].vararg = vararg;
    vm->funcProtos[scope.protoID].numArgs = size(&args);
    for (size_t i = 0; i < size(&args); ++i) {
        args[i].setRegister(vm, scope);
    }
}

Value ASTArgList::getRegister(VM *vm, Scope scope) {
    l_fprintf(stderr, "ICE: argument list does not have a register\n");
    assert(false);
}

void ASTArgList::freeRegister(Scope scope) {
    l_fprintf(stderr, "ICE: argument list does not have a register\n");
    assert(false);
}

ASTLambda::ASTLambda() : ASTNode(ASTT_LAMBDA, true) {}
void ASTLambda::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "Lambda (\n");
    argList.traverse(vm);
    body.traverse(vm);
    l_fprintf(stdout, ")\n");
};

void ASTLambda::emit(VM *vm, Scope scope) {
    // create new func and stuffs, add the new func to the
    // constant table(?), load the constant.
    
    Scope newScope;
    allocScope(newScope, &scope, line, file, nameSymbol);
    size_t localProtoID =
        size(&vm->funcProtos[scope.protoID].subFuncProtoIDs);
    add(&vm->funcProtos[scope.protoID].subFuncProtoIDs,
        newScope.protoID);
    //newSymToReg.parent = symToReg;
    argList.emit(vm, newScope);
    body.emit(vm, newScope);
    if (body.hasReg) {
        Value retReg = body.getRegister(vm, newScope);
        addR(&vm->funcProtos[newScope.protoID], OP_RETURN,
             last(&body.body)->line,
             retReg.regOrConstant); // ugly
    } else {
        addN(&vm->funcProtos[newScope.protoID], OP_RETURN_UNDEF,
             last(&body.body)->line);
    }
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addRI(&vm->funcProtos[scope.protoID], OP_CREATE_FUNC, line,
          reg.regOrConstant,
          localProtoID);
    assert(patchGoStatements(vm, newScope) == LRS_OK);
}

Value ASTLambda::getRegister(VM *vm, Scope scope) {
    return reg;
}

void ASTLambda::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTCall::ASTCall() : ASTNode(ASTT_CALL, true) {}
void ASTCall::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "Call (\n");
    callee->traverse(vm);
    for (size_t i = 0; i < size(&args); ++i) {
        args[i]->traverse(vm);
    }
    l_fprintf(stdout, ")\n");
}

void ASTCall::emit(VM *vm, Scope scope) {
    callee->emit(vm, scope);
    Value calleeReg = callee->getRegister(vm, scope);
    DynamicArray<Value> argRegs;
    for (size_t i = 0; i < size(&args); ++i) {
        args[i]->emit(vm, scope);
        add(&argRegs, args[i]->getRegister(vm, scope));
    }
    addR(&vm->funcProtos[scope.protoID], OP_SETUP_CALL, line,
         calleeReg.regOrConstant);
    for (size_t i = 0; i < size(&argRegs); ++i) {
        addR(&vm->funcProtos[scope.protoID], OP_PUSH_ARG, line,
             argRegs[i].regOrConstant);
        args[i]->freeRegister(scope);
    }
    returnReg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addR(&vm->funcProtos[scope.protoID], OP_CALL, line,
         returnReg.regOrConstant);
}

Value ASTCall::getRegister(VM *vm, Scope scope) {
    return returnReg;
}

void ASTCall::freeRegister(Scope scope) {
    freeReg(scope, returnReg.regOrConstant);
}

ASTDouble::ASTDouble() : ASTNode(ASTT_DOUBLE, true) {}
void ASTDouble::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "%f\n", value);
}

void ASTDouble::emit(VM *vm, Scope scope) {
    Value v{V_DOUBLE};
    v.doub = value;
    Value k;
    if (keyExists(vm, scope.valueToConstantSlot, v)) {
        k = get(vm, scope.valueToConstantSlot, v);
    } else {
        k = allocConstant(&vm->funcProtos[scope.protoID], v);
        set(vm, scope.valueToConstantSlot, v, k);
    }
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addRI(&vm->funcProtos[scope.protoID], OP_LOADK, line,
          reg.regOrConstant, k.regOrConstant);
}

Value ASTDouble::getRegister(VM *vm, Scope scope) {
    return reg;
}

void ASTDouble::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTBoolean::ASTBoolean() : ASTNode(ASTT_BOOLEAN, true) {}

void ASTBoolean::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "%s\n", value ? "true" : "false");
}

void ASTBoolean::emit(VM *vm, Scope scope) {
    Value v{V_BOOLEAN};
    v.boolean = value;
    Value k;
    if (keyExists(vm, scope.valueToConstantSlot, v)) {
        k = get(vm, scope.valueToConstantSlot, v);
    } else {
        k = allocConstant(&vm->funcProtos[scope.protoID], v);
        set(vm, scope.valueToConstantSlot, v, k);
    }
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addRI(&vm->funcProtos[scope.protoID], OP_LOADK, line,
          reg.regOrConstant, k.regOrConstant);
}

Value ASTBoolean::getRegister(VM *vm, Scope scope) {
    return reg;
}

void ASTBoolean::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTString::ASTString() : ASTNode(ASTT_STRING, true) {}

void ASTString::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "%s\n", value.str);
}

void ASTString::emit(VM *vm, Scope scope) {
    Value k;
    if (keyExists(vm, scope.valueToConstantSlot, value)) {
        k = get(vm, scope.valueToConstantSlot, value);
    } else {
        k = allocConstant(&vm->funcProtos[scope.protoID], value);
        set(vm, scope.valueToConstantSlot, value, k);
    }
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addRI(&vm->funcProtos[scope.protoID], OP_LOADK, line,
          reg.regOrConstant, k.regOrConstant);
}

Value ASTString::getRegister(VM *vm, Scope scope) {
    return reg;
}

void ASTString::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTMakeObject::ASTMakeObject() : ASTNode(ASTT_MAKE_OBJECT, true) {}

void ASTMakeObject::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "(make-object\n(\n");
    for (size_t i = 0; i < size(&slots); ++i) {
        l_fprintf(stdout, "(\n");
        slots[i].key->traverse(vm);
        slots[i].value->traverse(vm);
        l_fprintf(stdout, ")\n");
    }
    l_fprintf(stdout, ")\n)\n");
}

void ASTMakeObject::emit(VM *vm, Scope scope) {
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    addR(&vm->funcProtos[scope.protoID], OP_MAKE_OBJECT, line,
         reg.regOrConstant);
    for (size_t i = 0; i < size(&slots); ++i) {
        slots[i].key->emit(vm, scope);
        slots[i].value->emit(vm, scope);
        Value keyReg = slots[i].key->getRegister(vm, scope);
        Value valueReg = slots[i].value->getRegister(vm, scope);
        addRRR(&vm->funcProtos[scope.protoID], OP_SET_OBJECT_SLOT, line,
               reg.regOrConstant,
               keyReg.regOrConstant,
               valueReg.regOrConstant);
    }
    for (size_t i = 0; i < size(&slots); ++i) {
        slots[i].key->freeRegister(scope);
        slots[i].value->freeRegister(scope);
    }
}

Value ASTMakeObject::getRegister(VM *vm, Scope scope) {
    return reg;
}

void ASTMakeObject::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTDefine::ASTDefine() : ASTNode(ASTT_DEFINE, false), var(Value{V_SYMBOL}) {}

void ASTDefine::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "(define\n");
    printValue(vm, var);
    l_fprintf(stdout, "\n");
    expr->traverse(vm);
    l_fprintf(stdout, ")\n");
}

void ASTDefine::emit(VM *vm, Scope scope) {
    expr->emit(vm, scope);
    Value k;
    if (keyExists(vm, scope.valueToConstantSlot, var)) {
        k = get(vm, scope.valueToConstantSlot, var);
    } else {
        k = allocConstant(&vm->funcProtos[scope.protoID], var);
        set(vm, scope.valueToConstantSlot, var, k);
    }
    addRI(&vm->funcProtos[scope.protoID], OP_DEFINE_GLOBAL, line,
          expr->getRegister(vm, scope).regOrConstant,
          k.regOrConstant);
    expr->freeRegister(scope);
}

Value ASTDefine::getRegister(VM *vm, Scope scope) {
    l_fprintf(stderr, "ERROR at %lu:%lu: define can't be used as an "
            "expression\n", line, column);
    assert(false);
}

void ASTDefine::freeRegister(Scope scope) {
}

ASTIf::ASTIf() : ASTNode(ASTT_IF, true) {}

void ASTIf::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "If (\n");
    pred->traverse(vm);
    trueBranch->traverse(vm);
    if (falseBranch) {
        falseBranch->traverse(vm);
    }
    l_fprintf(stdout, ")\n");
}

void ASTIf::emit(VM *vm, Scope scope) {
    reg = allocReg(&vm->funcProtos[scope.protoID], scope);
    pred->emit(vm, scope);
    pred->freeRegister(scope);
    size_t ifBranchPos = getPos(&vm->funcProtos[scope.protoID]);
    addN(&vm->funcProtos[scope.protoID], OP_DUMMY, line);
    int64 trueStart = getPos(&vm->funcProtos[scope.protoID]);
    trueBranch->emit(vm, scope);
    trueBranch->freeRegister(scope);
    if (trueBranch->hasReg) {
        addRR(&vm->funcProtos[scope.protoID], OP_MOVE, line,
              reg.regOrConstant,
              trueBranch->getRegister(vm, scope).regOrConstant);
    } else {
        addR(&vm->funcProtos[scope.protoID], OP_LOAD_UNDEF, line,
             reg.regOrConstant);
    }
    size_t trueBranchPos = getPos(&vm->funcProtos[scope.protoID]);
    int64 trueBranchEnd = getPos(&vm->funcProtos[scope.protoID]);
    if (falseBranch) {
        addN(&vm->funcProtos[scope.protoID], OP_DUMMY, line);
        trueBranchEnd = getPos(&vm->funcProtos[scope.protoID]);
        falseBranch->emit(vm, scope);
        falseBranch->freeRegister(scope);
        if (falseBranch->hasReg) {
            addRR(&vm->funcProtos[scope.protoID], OP_MOVE, line,
                  reg.regOrConstant,
                  falseBranch->getRegister(vm, scope).regOrConstant);
        } else {
            addR(&vm->funcProtos[scope.protoID], OP_LOAD_UNDEF, line,
                 reg.regOrConstant);
        }
        int64 endPos = getPos(&vm->funcProtos[scope.protoID]);
        patchS(&vm->funcProtos[scope.protoID], trueBranchPos,
               OP_JMP,
               endPos - trueBranchEnd);
    }
    patchRS(&vm->funcProtos[scope.protoID], ifBranchPos,
            OP_JMP_IF_FALSE,
            pred->getRegister(vm, scope).regOrConstant,
            trueBranchEnd - trueStart);
}

Value ASTIf::getRegister(VM *vm, Scope scope) {
    return reg;
}

void ASTIf::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTLet::ASTLet() : ASTNode(ASTT_LET, false) {}

void ASTLet::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "(let\n");
    var.traverse(vm);
    l_fprintf(stdout, "\n");
    expr->traverse(vm);
    l_fprintf(stdout, ")\n");
}

void ASTLet::emit(VM *vm, Scope scope) {
    var.setRegister(vm, scope);
    expr->emit(vm, scope);
    addRR(&vm->funcProtos[scope.protoID], OP_MOVE, line,
          var.getRegister(vm, scope).regOrConstant,
          expr->getRegister(vm, scope).regOrConstant);
    expr->freeRegister(scope);
}

Value ASTLet::getRegister(VM *vm, Scope scope) {
    l_fprintf(stderr, "ERROR at %lu:%lu: let can't be used as an "
            "expression\n", line, column);
    assert(false);
}

void ASTLet::freeRegister(Scope scope) {
}

ASTSet::ASTSet() : ASTNode(ASTT_SET, true) {}

void ASTSet::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "(set!\n");
    var.traverse(vm);
    printf("\n");
    expr->traverse(vm);
    l_fprintf(stdout, ")\n");
}

void ASTSet::emit(VM *vm, Scope scope) {
    expr->emit(vm, scope);
    uint8 exprReg = expr->getRegister(vm, scope).regOrConstant;

    if (!keyExists(vm, scope.symToReg, var.symbol)) {
        set(vm, scope.symToReg, var.symbol,
            allocReg(&vm->funcProtos[scope.protoID], scope, true));
    }
    Value varReg = get(vm, scope.symToReg, var.symbol);
    if (varReg.nonLocal) {
        uint8 upvalueIdx;
        if (getUpvalue(vm, scope, var.symbol, &upvalueIdx)) {
            // Upval
            addRI(&vm->funcProtos[scope.protoID], OP_SET_UPVALUE, line,
                  exprReg, upvalueIdx);
        } else {
            // Global
            Value k;
            if (keyExists(vm, scope.valueToConstantSlot, var.symbol)) {
                k = get(vm, scope.valueToConstantSlot, var.symbol);
            } else {
                k = allocConstant(&vm->funcProtos[scope.protoID],
                                  var.symbol);
                set(vm, scope.valueToConstantSlot, var.symbol, k);
            }
            addRI(&vm->funcProtos[scope.protoID], OP_SET_GLOBAL, line,
                  exprReg, k.regOrConstant);
        }
    } else {
        // Local
        addRR(&vm->funcProtos[scope.protoID], OP_MOVE, line,
              varReg.regOrConstant, exprReg);
    }
    expr->freeRegister(scope);
}

Value ASTSet::getRegister(VM *vm, Scope scope) {
    return var.getRegister(vm, scope);
}

void ASTSet::freeRegister(Scope scope) {
}

ASTList::ASTList() : ASTNode(ASTT_LIST, true) {}

void ASTList::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "(list\n");
    if (dotted) {
        for (size_t i = 0; i < size(&elems) - 1; ++i) {
            elems[i]->traverse(vm);
        }
        l_fprintf(stdout, ".\n");
        last(&elems)->traverse(vm);
    } else {
        for (size_t i = 0; i < size(&elems); ++i) {
            elems[i]->traverse(vm);
        }
    }
    l_fprintf(stdout, ")");
}

void ASTList::emit(VM *vm, Scope scope) {
    // needs to be done in reverse, so ugly as fuck.
    if (size(&elems)) {
        size_t carReg;
        size_t cdrReg = allocReg(&vm->funcProtos[scope.protoID],
                                 scope).regOrConstant;
        size_t headReg = allocReg(&vm->funcProtos[scope.protoID],
                                  scope).regOrConstant;
        size_t start = size(&elems)-1;
        if (dotted) {
            assert(size(&elems) > 2);
            elems[start]->emit(vm, scope);
            addRR(&vm->funcProtos[scope.protoID], OP_MOVE, line,
                  headReg,
                  elems[start]->getRegister(vm, scope).regOrConstant);
            elems[start]->freeRegister(scope);
            start--;
        } else {
            addR(&vm->funcProtos[scope.protoID], OP_LOAD_NULL, line,
                 headReg);
        }
        for (int i = start; i >= 0; --i) {
            elems[i]->emit(vm, scope);
            freeReg(scope, cdrReg);
            cdrReg = headReg;
            carReg =
                elems[i]->getRegister(vm, scope).regOrConstant;
            headReg = allocReg(&vm->funcProtos[scope.protoID],
                               scope).regOrConstant;
            addRRR(&vm->funcProtos[scope.protoID], OP_CONS, line,
                   headReg, carReg, cdrReg);
            elems[i]->freeRegister(scope);
        }
        reg.type = V_REG_OR_CONSTANT;
        reg.regOrConstant = headReg;
    } else {
        reg = allocReg(&vm->funcProtos[scope.protoID], scope);
        addR(&vm->funcProtos[scope.protoID], OP_LOAD_NULL, line,
             reg.regOrConstant);
    }
}

Value ASTList::getRegister(VM *vm, Scope scope) {
    return reg;
}

void ASTList::freeRegister(Scope scope) {
    freeReg(scope, reg.regOrConstant);
}

ASTDefmacro::ASTDefmacro() : ASTNode(ASTT_DEFMACRO, false) {}
void ASTDefmacro::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "Defmacro ");
    printValue(vm, variable);
    l_fprintf(stdout, "(\n");
    argList.traverse(vm);
    body.traverse(vm);
    l_fprintf(stdout, ")\n");
};

void ASTDefmacro::emit(VM *vm, Scope scope) {
    // create new func and stuffs, add the new func to the
    // constant table(?), load the constant.
    
    Scope newScope;
    allocScope(newScope, &scope, line, file, variable.sym);
    size_t localProtoID =
        size(&vm->funcProtos[scope.protoID].subFuncProtoIDs);
    add(&vm->funcProtos[scope.protoID].subFuncProtoIDs,
        newScope.protoID);
    //newSymToReg.parent = symToReg;
    argList.emit(vm, newScope);
    body.emit(vm, newScope);
    if (body.hasReg) {
        Value retReg = body.getRegister(vm, newScope);
        addR(&vm->funcProtos[newScope.protoID],
             OP_RETURN, line, retReg.regOrConstant);
    } else {
        l_fprintf(stderr, "ERROR at %lu:%lu: macro has to "
                  "return a value\n", body.line, body.column);
        assert(false);
        //addN(&vm->funcProtos[newScope.protoID], OP_RETURN_UNDEF);
    }
    Value mac = {V_FUNCTION};
    mac.func = allocFunction(vm, newScope.protoID);
    set(vm, &vm->macros, variable, mac);
    patchGoStatements(vm, newScope);
}

Value ASTDefmacro::getRegister(VM *vm, Scope scope) {
    l_fprintf(stderr, "ERROR %lu:%lu: defmacro can't be "
              "used as an expression\n", line, column);
    assert(false);
}

void ASTDefmacro::freeRegister(Scope scope) {
}

ASTLabel::ASTLabel() : ASTNode(ASTT_LABEL, false) {}
void ASTLabel::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "(Label\n");
    printValue(vm, labelSymbol);
    l_fprintf(stdout, "\n)\n");
};

void ASTLabel::emit(VM *vm, Scope scope) {
    assert(!keyExists(vm, scope.labelPositions, labelSymbol));
    Value codePos = {V_CODE_POSITION};
    codePos.codePosition = getPos(&vm->funcProtos[scope.protoID]);
    set(vm, scope.labelPositions, labelSymbol, codePos);
    //NOOP
}

Value ASTLabel::getRegister(VM *vm, Scope scope) {
    l_fprintf(stderr, "ERROR at %lu:%lu: label can't be used as "
              "an expression\n", line, column);
    assert(false);
}

void ASTLabel::freeRegister(Scope scope) {
}

ASTGo::ASTGo() : ASTNode(ASTT_GO, false) {}
void ASTGo::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "(Go\n");
    printValue(vm, labelSymbol);
    l_fprintf(stdout, "\n)\n");
};

void ASTGo::emit(VM *vm, Scope scope) {
    ObjectSlot goSlot;
    goSlot.key = labelSymbol;
    Value codePos = {V_CODE_POSITION};
    codePos.codePosition = getPos(&vm->funcProtos[scope.protoID]);
    goSlot.value = codePos;
    add(scope.goLabelPositions, goSlot);
    addN(&vm->funcProtos[scope.protoID], OP_DUMMY, line);
}

Value ASTGo::getRegister(VM *vm, Scope scope) {
    l_fprintf(stderr, "ERROR at %lu:%lu: go can't be used as "
              "an expression\n", line, column);
    assert(false);
}

void ASTGo::freeRegister(Scope scope) {
}

ASTCrash::ASTCrash() : ASTNode(ASTT_CRASH, false) {}
void ASTCrash::traverse(VM *vm) {
    l_fprintf(stdout, "%lu:%lu\n", line, column);
    l_fprintf(stdout, "(Crash\n");
    crashvalue->traverse(vm);
    l_fprintf(stdout, "\n)\n");
};

void ASTCrash::emit(VM *vm, Scope scope) {
    crashvalue->emit(vm, scope);
    addR(&vm->funcProtos[scope.protoID], OP_CRASH, line,
         crashvalue->getRegister(vm, scope).regOrConstant);
}

Value ASTCrash::getRegister(VM *vm, Scope scope) {
    l_fprintf(stderr, "ERROR at %lu:%lu: crash can't be used as "
              "an expression\n", line, column);
    assert(false);
}

void ASTCrash::freeRegister(Scope scope) {
}