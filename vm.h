#ifndef VM_H
#define VM_H

#include "gc.h"

#include <vector>
#include <cassert>
#include <cstring>
#include <cstdint>

struct DynamicArrayData {
    GCObject gcObject;
    bool containsValues; // ie struct Value
    char data[0];
};

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
void resize(DynamicArray<T> *a, size_t newSize, T val = T()) {
    a->v.resize(newSize, val);
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

template <typename T>
void clear(DynamicArray<T> *a) {
    a->v.clear();
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
    V_UNDEF, // Has to be first, bc zeroing a Value should result in undef
    V_FUNCTION,
    V_SYMBOL,
    V_STRING,
    V_DOUBLE,
    V_BOOLEAN,
    V_CFUNCTION,
    V_CONS_PAIR,
    V_OPAQUE_POINTER, 
    V_OBJECT,
    V_REG_OR_CONSTANT, // Used by compiler only.
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

struct ObjectSlot;

// Maybe allow strings as keys to?
// IE symbols, strings and doubles.
// This would make Object perfect for string interning.
struct Object {
    GCObject gcObj;
    Object();
    DynamicArray<ObjectSlot> slots;
};

void clear(Object *obj);

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
    Object symbolTable;
    int symbolIdTop = 0;
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

struct String {
    GCObject gcObj;
    size_t length;
    char str[0];
};

// Maybe move to NAN-tagging, its cool as fuck!
// Would slow down my strings, but who cares about strings anyway?
// WARNING!! Strings leak!
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
            uint8 regOrConstant;
            bool global;
        };
        struct {
            int symid; // maybe make symid 64bit?
            char *symstr;
        };
        char *str;
        Object *object;
        struct {
            uint64 opaqueType;
            void *opaque;
        };
        ConsPair *pair;
    };
};
// if key is V_UNDEF and value is true then this is a tombstone,
// if key is V_UNDEF and value is false then this cell is empty
// Warning! Adding anything breaks GC
struct ObjectSlot {
    Value key;
    Value value;
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

// Change to set
void add(VM *vm, Object *o, Value key, Value value);
Value &get(Object *o, Value key);
bool keyExists(Object *o, Value key);

void clearStack(VM *vm);

inline
bool operator==(Value a, Value b) {
    if (a.type != b.type) {
        return false;
    } else {
        switch (a.type) {
            case V_BOOLEAN: {
                return a.boolean == b.boolean;
            } break;
            case V_CFUNCTION: {
                return a.cfunc == b.cfunc;
            } break;
            case V_CONS_PAIR: {
                return a.pair == b.pair;
            } break;
            case V_DOUBLE: {
                return a.doub == b.doub;
            } break;
            case V_FUNCTION: {
                return a.funcID == b.funcID;
            } break;
            case V_OBJECT: {
                return a.object == b.object;
            } break;
            case V_OPAQUE_POINTER: {
                // We assume that a.opaqueType == b.opaqueType if
                // a.opaque == b.opaque
                return a.opaque == b.opaque; 
            } break;
            case V_STRING: {
                // Make strings store a hash
                return !strcmp(a.str, b.str);
            } break;
            case V_SYMBOL: {
                return a.symid == b.symid;
            } break;
            case V_UNDEF: {
                return false;
            } break;
            case V_REG_OR_CONSTANT: {
                return (a.regOrConstant == b.regOrConstant &&
                        a.global == b.global);
            } break;
        }
    }
    return false; // unreachable
}

// Illegal to use vm after calling freeVM on it.
void freeVM(VM *vm);
#endif