#pragma once
#include "vm.h"
#include <cstddef>
#include <cstdlib>

struct Scope {
    size_t protoID;
    Scope *parent;
    Object *symToReg;
    DynamicArray<size_t> *freeRegisters;
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
    ASTNode(ASTNodeType t, bool b);
    virtual void traverse() = 0;
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot) = 0;
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot) = 0;
    virtual void freeRegister(Scope scope) = 0;
    //virtual size_t getConstantId(VM *vm, Function *func,
    //map<int, size_t> *symToReg) = 0;
};

struct ASTBody : ASTNode {
    ASTBody();
    DynamicArray<ASTNode *> body;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTSymbol : ASTNode {
    ASTSymbol();
    char *str;
    int symid;
    Value reg;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

// Hmmmm since we have a register machine a-normal form
// would be *very* good, or until then an enum would work.
struct ASTVariable : ASTNode {
    ASTVariable();
    Value symbol;
    bool upvalue;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
    void setRegister(VM *vm, Scope scope);
};

struct ASTArgList : ASTNode {
    ASTArgList();
    DynamicArray<ASTVariable> args;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};


struct ASTList : ASTNode {
    ASTList();
    Value reg;
    DynamicArray<ASTNode *> elems;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTLambda : ASTNode {
    ASTLambda();
    ASTArgList argList;
    ASTBody body;
    Value reg;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTCall : ASTNode {
    ASTCall();
    ASTNode *callee;
    DynamicArray<ASTNode *> args;
    Value returnReg;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTDouble : ASTNode {
    ASTDouble();
    double value;
    Value reg;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTBoolean : ASTNode {
    ASTBoolean();
    bool value;
    Value reg;
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
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
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTDefine : ASTNode {
    Value var;
    ASTNode *expr;
    ASTDefine();
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTIf : ASTNode {
    Value reg;
    ASTNode *pred;
    ASTNode *trueBranch;
    ASTNode *falseBranch;
    ASTIf();
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTLet : ASTNode {
    ASTVariable var;
    ASTNode *expr;
    ASTLet();
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

struct ASTSet : ASTNode {
    ASTVariable var;
    ASTNode *expr;
    ASTSet();
    virtual void traverse();
    virtual void emit(VM *vm, Scope scope,
                      Object *valueToConstantSlot);
    virtual Value getRegister(VM *vm, Scope scope,
                              Object *valueToConstantSlot);
    virtual void freeRegister(Scope scope);
};

ASTBody bodyToAST(ArenaAllocator *arena, Value tree);

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