#pragma once
#include "vm.h"
#include <cstddef>
#include <cstdlib>

// Needs child pointer when we make DynamicArray gc'd
// since the Scope tree has to be a root.
// (We only need one child at a time since we only compile
// one scope at a time, and the rest is garbage)
struct Scope {
    size_t protoID;
    Scope *parent;
    Object *symToReg;
    DynamicArray<size_t> *freeRegisters;
    Object *labelPositions;
    DynamicArray<ObjectSlot> *goLabelPositions;
    // key is the label, value is the code position to patch
    Object *valueToConstantSlot;
};

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
    ASTT_IF,
    ASTT_LET,
    ASTT_SET,
    ASTT_LIST,
    ASTT_DEFMACRO,
    ASTT_STRING,
    ASTT_FOR,
    ASTT_LABEL,
    ASTT_GO,
};

struct ArenaAllocator {
    void *mem = 0;
    const size_t size = 512; // TODO: Tune me!!!
    size_t top = 0;
    ArenaAllocator *next = 0;
    ArenaAllocator *last = 0;
};

struct ASTNode {
    ASTNodeType type;
    bool hasReg;
    size_t line;
    size_t column;
    ASTNode(ASTNodeType t, bool b);
    virtual void traverse(VM *vm) = 0;
    virtual void emit(VM *vm, Scope scope) = 0;
    virtual Value getRegister(VM *vm, Scope scope) = 0;
    virtual void freeRegister(Scope scope) = 0;
    //map<int, size_t> *symToReg) = 0;
};

struct ASTBody : ASTNode {
    ASTBody();
    DynamicArray<ASTNode *> body;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTSymbol : ASTNode {
    ASTSymbol();
    Value sym;
    Value reg;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

// Hmmmm since we have a register machine a-normal form
// would be *very* good, or until then an enum would work.
struct ASTVariable : ASTNode {
    ASTVariable();
    Value symbol;
    bool upvalue;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
    virtual void setRegister(VM *vm, Scope scope);
};

struct ASTArgList : ASTNode {
    ASTArgList();
    DynamicArray<ASTVariable> args;
    bool vararg;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTList : ASTNode {
    ASTList();
    Value reg;
    bool dotted;
    DynamicArray<ASTNode *> elems;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTLambda : ASTNode {
    ASTLambda();
    ASTArgList argList;
    ASTBody body;
    Value reg;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTDefmacro : ASTNode {
    ASTDefmacro();
    Value variable;
    ASTArgList argList;
    ASTBody body;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTCall : ASTNode {
    ASTCall();
    ASTNode *callee;
    DynamicArray<ASTNode *> args;
    Value returnReg;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTDouble : ASTNode {
    ASTDouble();
    double value;
    Value reg;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTBoolean : ASTNode {
    ASTBoolean();
    bool value;
    Value reg;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTString : ASTNode {
    ASTString();
    Value value;
    Value reg;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct MakeObjectSlot {
    ASTNode *key;
    ASTNode *value;
};

struct ASTMakeObject : ASTNode {
    ASTMakeObject();
    DynamicArray<MakeObjectSlot> slots;
    Value reg;
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTDefine : ASTNode {
    Value var;
    ASTNode *expr;
    ASTDefine();
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTIf : ASTNode {
    Value reg;
    ASTNode *pred;
    ASTNode *trueBranch;
    ASTNode *falseBranch;
    ASTIf();
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTLet : ASTNode {
    ASTVariable var;
    ASTNode *expr;
    ASTLet();
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

#if 0

struct ASTFor : ASTNode {
    ASTVariable var;
    ASTNode *iterExpr;
    ASTBody body;
    ASTFor();
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

#endif

struct ASTSet : ASTNode {
    ASTVariable var;
    ASTNode *expr;
    ASTSet();
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTLabel : ASTNode {
    Value labelSymbol;
    ASTLabel();
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
};

struct ASTGo : ASTNode {
    Value labelSymbol;
    ASTGo();
    virtual void traverse(VM *vm);
    virtual void emit(VM *vm, Scope scope);
    virtual Value getRegister(VM *vm, Scope scope);
    virtual void freeRegister(Scope scope);
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
void freeArena(ArenaAllocator *arena);
Value compileString(VM *vm, char *prog, bool verbose);
Value compileFile(VM *vm, const char *path, bool verbose);