#ifndef VM_H
#define VM_H

#include "gc.h"

#include <vector>
#include <map>
#include <cassert>

template <typename T>
struct DynamicArray {
    std::vector<T> v;
    T &operator[](size_t i) {
        assert(i < v.size());
        return v[i];
    }
};

template <typename T>
size_t size(DynamicArray<T> *a) {
    return a->v.size();
}

template <typename T>
T *array(DynamicArray<T> *a) {
    return a->v.size();
}

template <typename T>
void resize(DynamicArray<T> *a, size_t newSize) {
    a->v.resize(newSize);
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
void remove(DynamicArray<T> *a, size_t idx) {
    auto nth = a->v.begin() + idx;
    a->v.erase(nth);
}

typedef unsigned char uint8;
typedef unsigned int uint32;
typedef unsigned long long int uint64;

enum OpCode : uint64 {
#define OPCODE(op) OP_ ## op,
#include "opcodes.h"
#undef OPCODE
};

enum OpCodeType {
    OT_N,
    OT_R,
    OT_RR,
    OT_RRR,
    OT_RI
};

extern OpCodeType opCodeTypes[];

extern const char *opCodeStr[];

struct Value;

struct Function {
    DynamicArray<Value> constants;
    DynamicArray<OpCode> code;
    size_t numRegs = 0;
};

enum ValueType {
    V_FUNCTION,
    V_SYMBOL,
    V_DOUBLE,
    V_BOOLEAN,
    V_CFUNCTION,
    V_CONS_PAIR,
    V_OPAQUE_POINTER, 
    V_OBJECT,
    V_UNDEF,
};

struct ActivationFrame {
    DynamicArray<Value> registers;
    size_t stackTop = 0;
    Function *func = 0;
    size_t pc = 0;
    size_t caller = 0; // 0 if C, frameID of caller is this - 1
    //ActivationFrame *caller = 0; // null if C
    uint8 retReg = 0;
};

struct Object {
    GCObject gcObj;
    Object();
    std::map<int, Value> table;
};

struct Handle {
    size_t handle;
};

struct VM {
    DynamicArray<Function> funcs;
    DynamicArray<Value> apiStack;
    DynamicArray<ActivationFrame> frameStack;
    DynamicArray<GCObject *> handles;
    size_t frameStackTop = 0;
    Object globals;
    GC gc;
};

void free(VM *vm, Handle handle);

// WARNING: arguments are in reverse,
// ie last argument is the top of the stack
typedef size_t (*CFunction)(VM *vm, size_t numArgs);

struct ConsPair;

/*
  The evaluation order of
  getC(vm, currParent).cdr.pair = allocConsPair(vm);
  is unspecified, meaning that it may choose to
  first evaluate getC then allocConsPair. If allocConsPair
  triggers a gc cycle then the pointer that getC
  returned is invalidated and we get weird, imposible to
  debug crashes that look like gc bugs. 
  ╔═╗╦ ╦╔═╗╦╔═  ╔╦╗╦ ╦╔═╗  ╔═╗╔╦╗╔═╗╔╗╔╔╦╗╔═╗╦═╗╔╦╗
  ╠╣ ║ ║║  ╠╩╗   ║ ╠═╣║╣   ╚═╗ ║ ╠═╣║║║ ║║╠═╣╠╦╝ ║║
  ╚  ╚═╝╚═╝╩ ╩   ╩ ╩ ╩╚═╝  ╚═╝ ╩ ╩ ╩╝╚╝═╩╝╩ ╩╩╚══╩╝
*/
ConsPair &getC(VM *vm, Handle handle);
Object &getO(VM *vm, Handle handle);

Handle reserve(VM *vm, ConsPair *c);
Handle reserve(VM *vm, Object *o);

// Maybe move to NAN-tagging, its cool as fuck!
// Would slow down my strings, but who cares about strings anyway?
struct Value {
    Value();
    Value(ValueType t);
    ValueType type;
    union {
        size_t funcID;
        CFunction cfunc;
        double doub;
        bool boolean;
        struct {
            int symid; // maybe make symid 64bit?
            char *symstr;
        };
        Object *object;
        struct {
            uint64 opaqueType;
            void *opaque;
        };
        ConsPair *pair;
    };
};

struct ConsPair {
    ConsPair();
    GCObject gcObj;
    Value car;
    Value cdr;
};

Value at(Value v, size_t idx);

ConsPair *allocConsPair(VM *vm);
Object *allocObject(VM *vm);
#endif