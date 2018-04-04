#pragma once

#include "gc.h"

#include <vector>
#include <cassert>
#include <cstring>
#include <cstdint>

// How do we make shure that all the DynamicArray's are treated as roots?
struct DynamicArrayData {
    GCObject gcObject;
    bool containsValues; // ie struct Value
    size_t size;
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

// Warning! If your DynamicArray is stored in the heap this will
// crash (when we go over to managed array implementation)!
template <typename T>
void resize(DynamicArray<T> *a, size_t newSize, T val = T()) {
    a->v.resize(newSize, val);
}

// Warning! If your DynamicArray is stored in the heap this will
// crash (when we go over to managed array implementation)!
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
typedef char int8;
typedef int int32;
typedef long long int int64;

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
    OT_RI,
    OT_I,
    OT_RS,
    OT_S,
};

extern OpCodeType opCodeTypes[];

extern const char *opCodeStr[];

struct Value;

// Warning! DO NOT CREATE THIS YOUR SELF!!!!
// Use intern!!!
struct Symbol {
    int id;
    char *str;
};

struct UpvalueDesc {
    Symbol variable;
    // The variable that this refers to (debug and compiler).
    bool local;
    // Is it local?
    // That is does index refer to a register the parent closure?
    uint8 index;
    // If local == true then this is the register index of the value
    // to close over, otherwise it refers to the parent closures
    // upvalue[index]
};

struct FunctionPrototype {
    GCObject gcObj;
    DynamicArray<Value> constants;
    DynamicArray<OpCode> code;
    DynamicArray<size_t> subFuncProtoIDs;
    DynamicArray<UpvalueDesc> upvalues;
    //DynamicArray<FunctionPrototype *> subFunctions;
    size_t numRegs = 0;
    size_t numArgs;
};

struct Upvalue;

// Rename to Closure
struct Function {
    GCObject gcObj;
    FunctionPrototype *prototype;
    DynamicArray<Upvalue *> upvalues;
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
    //size_t caller = 0; // 0 if C, frameID of caller is this - 1
    bool calledFromCpp = true;
    //ActivationFrame *caller = 0; // null if C
    uint8 retReg = 0;
};

struct ObjectSlot;

// Need a way to check if key is present
// in this object and not its parent, codegen needs that.
struct Object {
    GCObject gcObj;
    Object();
    DynamicArray<ObjectSlot> slots;
    Object *parent = 0;
};

void clear(Object *obj);

struct Handle {
    size_t handle;
};


struct VM {
    DynamicArray<FunctionPrototype> funcProtos;
    DynamicArray<Value> apiStack;
    DynamicArray<ActivationFrame> frameStack;
    size_t frameStackTop = 0;
    // Needs to be Value, since the compiler works with Values
    DynamicArray<GCObject *> handles;
    Object globals;
    Object macros;
    Object symbolTable;
    GC gc;
    int symbolIdTop = 0;
    Upvalue *openUpvalueHead = 0;
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
GCObjectType type(VM *vm, Handle handle);

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
        Function *func;
        CFunction cfunc;
        double doub;
        bool boolean;
        struct { // Used in the compiler only
            uint8 regOrConstant;
            bool nonLocal;
        };
        Symbol sym;
        char *str;
        Object *object;
        struct {
            uint64 opaqueType;
            void *opaque;
        };
        ConsPair *pair;
    };
};

struct Upvalue {
    GCObject gcObj;
    Upvalue();
    Value *value;
    Value closed;
    Upvalue *next;
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
Function *allocFunction(VM *vm, FunctionPrototype *prototype);
Upvalue *allocUpvalue(VM *vm);

void set(VM *vm, Object *o, Value key, Value value);
Value &get(Object *o, Value key);
bool keyExists(Object *o, Value key);

void clearStack(VM *vm);

inline
bool operator==(Symbol a, Symbol b) {
    return a.id == b.id;
}

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
                return a.func == b.func;
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
                return a.sym == b.sym;
            } break;
            case V_UNDEF: {
                return false;
            } break;
            case V_REG_OR_CONSTANT: {
                return (a.regOrConstant == b.regOrConstant &&
                        a.nonLocal == b.nonLocal);
            } break;
        }
    }
    return false; // unreachable
}

// Illegal to use vm after calling freeVM on it.
void freeVM(VM *vm);
void getGlobal(VM *vm, Symbol variable);
void setGlobal(VM *vm, Symbol variable);

// Can't exist (atleast not as public api), instead we need specific
// different ones for different types, And Handle's for GCObject's
Value peek(VM *vm, int idx);
Value pop(VM *vm);

bool popBoolean(VM *vm);
double popDouble(VM *vm);
Symbol popSymbol(VM *vm);
void car(VM *vm);
void cdr(VM *vm);
void dup(VM *vm);

bool isEmptyList(VM *vm);

void pushDouble(VM *vm, double doub);
void pushSymbol(VM *vm, char *symstr);
void pushSymbol(VM *vm, Symbol symbol);
void pushString(VM *vm, char *str);
void pushBoolean(VM *vm, bool boolean);
void pushOpaque(VM *vm, void *opaque, uint64 type);
void pushHandle(VM *vm, Handle handle);
void pushCFunction(VM *vm, CFunction cfunc);

// Should be internal
void pushValue(VM *vm, Value v);
Symbol intern(VM *vm, const char *str);

void collect(VM *vm);

uint32_t hashValue(Value v);

void printValue(Value v);
void printObject(Object *obj);
size_t length(Value v);

#define bitsize(o) (sizeof(o)*8)

#define getOp(undecoded) ((OpCode)((undecoded) >>                       \
                                   (bitsize(OpCode) - bitsize(uint8))))

#define getRegA(undecoded) ((0x00FF000000000000 & (undecoded)) >>       \
                            (bitsize(OpCode) - bitsize(uint8)*2))

#define getRegB(undecoded) ((0x0000FF0000000000 & (undecoded)) >>       \
                            (bitsize(OpCode) - bitsize(uint8)*3))

#define getRegC(undecoded) ((0x000000FF00000000 & (undecoded)) >>       \
                            (bitsize(OpCode) - bitsize(uint8)*4))

#define getImm(undecoded) (0x00000000FFFFFFFF & (undecoded))

#define getSImm(undecoded) (0x00000000FFFFFFFF & *((int32 *)&undecoded))


void doString(VM *vm, const char *prog, size_t numArgs = 0,
              bool verbose = true);
void doFile(VM *vm, const char *path, size_t numArgs = 0,
            bool verbose = true);
void call(VM *vm, uint8 numArgs);

VM initVM(bool loadDefaults = true);