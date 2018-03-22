#ifndef VM_H
#define VM_H

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

OpCodeType opCodeTypes[] = {
    OT_RI, // LOADK
    OT_RR, // LOAD_LOCAL
    OT_R,  // SETUP_CALL
    OT_R,  // PUSH_ARG
    OT_R,  // CALL
    OT_R,  // RETURN
    OT_RI, // LOAD_FUNC
    OT_R,  // MAKE_OBJECT
    OT_RRR,// SET_OBJECT_SLOT
    OT_RI, // DEFINE_GLOBAL
    OT_RI, // GET_GLOBAL
    OT_N,  // RETURN_UNDEF
};

#define OPCODE(op) #op,
const char *opCodeStr[] = {
#include "opcodes.h"
};

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
    Function *func;
    size_t pc = 0;
    ActivationFrame *caller = 0; // null if C
    uint8 retReg;
};

struct Object {
    Object() : table(std::map<int, Value>()) {}
    std::map<int, Value> table;
};

struct VM {
    DynamicArray<Function> funcs;
    DynamicArray<Value> apiStack;
    Object globals;
};

// WARNING: arguments are in reverse,
// ie last argument is the top of the stack
typedef size_t (*CFunction)(VM *vm, size_t numArgs);

struct ConsPair;

struct Value {
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
    Value car;
    Value cdr;
};

Value at(Value v, size_t idx) {
    Value ret = v;
    assert(ret.type == V_CONS_PAIR);
    assert(ret.pair);
    for (size_t i = 0; i < idx; ++i) {
        ret = ret.pair->cdr;
        assert(ret.type == V_CONS_PAIR);
        assert(ret.pair);
    }
    return ret.pair->car;
}

#endif