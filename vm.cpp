#include "vm.h"
#include "compiler.h"

#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cassert>
#include <cstring>


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
    OT_RI, // GET_UPVALUE
    OT_RS, // JMP_IF_FALSE
    OT_N,  // DUMMY
    OT_RR, // MOVE
    OT_S,  // JMP
    OT_R,  // LOAD_UNDEF
    OT_RI, // SET_UPVALUE
    OT_RI, // SET_GLOBAL
    OT_RRR,// CONS
    OT_R,  // LOAD_NULL
};

#define OPCODE(op) #op,
const char *opCodeStr[] = {
#include "opcodes.h"
};
#undef OPCODE

Object::Object() : gcObj(GCObject{{GC_OBJECT}}) {
    resize(&slots, 1);
    slots[0].key.type = V_UNDEF;
}

ConsPair::ConsPair() : gcObj(GCObject{{GC_CONS_PAIR}}) {}

Value::Value() : type(V_UNDEF) {
    //Null value
    opaqueType = 0;
    opaque = 0;
}

Value::Value(ValueType t) : type(t) {
    //Null value
    opaqueType = 0;
    opaque = 0;
}

Upvalue::Upvalue() : gcObj(GCObject{{GC_UPVALUE}}),
                     value(0),
                     closed(V_UNDEF),
                     next(0) {}

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

/******************************************************************
                ╔═╗╦ ╦╔═╗╦╔═  ╔═╗╔═╗╔═╗              
                ╠╣ ║ ║║  ╠╩╗  ║ ╦║  ║                
                ╚  ╚═╝╚═╝╩ ╩  ╚═╝╚═╝╚═╝              
                ╔═╗╦ ╦╔═╗╦╔═  ╔═╗╔╦╗╔╗               
                ╠╣ ║ ║║  ╠╩╗  ║ ╦ ║║╠╩╗              
                ╚  ╚═╝╚═╝╩ ╩  ╚═╝═╩╝╚═╝              
                ╔═╗╦ ╦╔═╗╦╔═  ╦  ╦╔═╗╦  ╔═╗╦═╗╦╔╗╔╔╦╗
                ╠╣ ║ ║║  ╠╩╗  ╚╗╔╝╠═╣║  ║ ╦╠╦╝║║║║ ║║
                ╚  ╚═╝╚═╝╩ ╩   ╚╝ ╩ ╩╩═╝╚═╝╩╚═╩╝╚╝═╩╝
                ╔═╗╦ ╦╔═╗╦╔═  ╔═╗╦  ╔═╗╔╗╔╔═╗        
                ╠╣ ║ ║║  ╠╩╗  ║  ║  ╠═╣║║║║ ╦        
                ╚  ╚═╝╚═╝╩ ╩  ╚═╝╩═╝╩ ╩╝╚╝╚═╝        
                ╦  ╦ ╦╔═╗╔╦╗╔═╗  ╦ ╦╔═╗╦ ╦  ╔═╗╦  ╦  
                ║  ╠═╣╠═╣ ║ ║╣   ╚╦╝║ ║║ ║  ╠═╣║  ║  
                ╩  ╩ ╩╩ ╩ ╩ ╚═╝   ╩ ╚═╝╚═╝  ╩ ╩╩═╝╩═╝
******************************************************************/
  


GCObject *copyFromTo(VM *vm, GCObject *obj, size_t size) {
    assert(obj->type <= GC_BROKEN_HEART);
    if ((size_t)obj > (size_t)vm->gc.toSpace &&
        (size_t)obj < ((size_t)vm->gc.toSpace) + vm->gc.toSpaceSize) {
        return obj;
    }
    GCObject *ret = (GCObject *)(vm->gc.toSpace + vm->gc.nextFree);
    assert(vm->gc.nextFree + size > vm->gc.nextFree);
    assert(vm->gc.nextFree + size <= vm->gc.toSpaceSize);
    assert(memcpy(vm->gc.toSpace + vm->gc.nextFree, obj, size));
    memset(obj, 0, size);
    assert(ret->type <= GC_BROKEN_HEART);
    obj->type = GC_BROKEN_HEART;
    //printf("move 0x%p to 0x%p\n", obj, ret);
    obj++;
    GCObject **bh = (GCObject **)obj;
    *bh = ret;
    vm->gc.nextFree += size;
    return ret;
}


void visitValue(VM *vm, Value *v) {
    switch (v->type) {
        case V_OBJECT: {
            assert(v->object->gcObj.type <= GC_BROKEN_HEART);
            if (v->object->gcObj.type == GC_BROKEN_HEART) {
                v->object = *(Object **)((&v->object->gcObj)+1);
            } else {
                v->object = (Object *)copyFromTo(vm, &v->object->gcObj,
                                                 sizeof(Object));
            }
            assert(v->object->gcObj.type <= GC_BROKEN_HEART);
        } break;
        case V_CONS_PAIR: {
            if (v->pair) {
                bool b = v->pair->gcObj.type > GC_BROKEN_HEART;
                if (b) {
                    assert(false);
                }
                assert(v->pair->gcObj.type <= GC_BROKEN_HEART);
                if (v->pair->gcObj.type == GC_BROKEN_HEART) {
                    v->pair = *(ConsPair **)((&v->object->gcObj)+1);
                } else {
                    v->pair = (ConsPair *)copyFromTo(vm, &v->pair->gcObj,
                                                     sizeof(ConsPair));
                }
                assert(v->pair->gcObj.type <= GC_BROKEN_HEART);
            }
        } break;
        case V_FUNCTION: {
            if (v->func->gcObj.type == GC_BROKEN_HEART) {
                v->func = *(Function **)((&v->func->gcObj)+1);
            } else {
                v->func = (Function *)copyFromTo(vm, &v->func->gcObj,
                                                 sizeof(Function));
            }
        } break;
            /*
        case V_STRING: {
            if (v->str->gcObj.type == GC_BROKEN_HEART) {
                v->str = *(String **)((&v->object->gcObj)+1);
            } else {
                v->str = (String *)copyFromTo(vm, &v->pair->gcObj,
                                              sizeof(String) +
                                              v->str->length);
            }
        } break;
            */
        default:break;
    }
}

Upvalue *visitUpvalue(VM *vm, Upvalue *upvalue) {
    if (upvalue->gcObj.type == GC_BROKEN_HEART) {
        Upvalue *ret = *(Upvalue **)((&upvalue->gcObj)+1);
        return ret;
    } else {
        bool shouldMoveValuePtr = (((size_t)upvalue->value) ==
                                   ((size_t)&upvalue->closed));
        Upvalue *ret = (Upvalue *)copyFromTo(vm, &upvalue->gcObj,
                                             sizeof(Upvalue));
        if (shouldMoveValuePtr) {
            ret->value = &ret->closed;
        }
        return ret;
    }
}

// TODO: remove iterator stuff
void visitObject(VM *vm, GCObject *o) {
    if (o->type >= GC_BROKEN_HEART) {
        assert(false);
    }
    switch (o->type) {
        case GC_UPVALUE: {
            Upvalue *upvalue = (Upvalue *)o;
            visitValue(vm, &upvalue->closed);
            vm->gc.nextToExamine += sizeof(Upvalue);
        } break;
        case GC_OBJECT: {
            Object *obj = (Object *)o;
            for (size_t i = 0; i < size(&obj->slots); ++i) {
                visitValue(vm, &obj->slots[i].key); // Might not be needed
                visitValue(vm, &obj->slots[i].value);
            }
            vm->gc.nextToExamine += sizeof(Object);
        } break;
        case GC_CONS_PAIR: {
            ConsPair *c = (ConsPair *)o;
            visitValue(vm, &c->car);
            visitValue(vm, &c->cdr);
            vm->gc.nextToExamine += sizeof(ConsPair);
        } break;
        case GC_FUNCTION: {
            Function *func = (Function *)o;
            for (size_t i = 0; i < size(&func->upvalues); ++i) {
                if (func->upvalues[i]) {
                    func->upvalues[i] = visitUpvalue(vm,
                                                     func->upvalues[i]);
                }
            }
            vm->gc.nextToExamine += sizeof(Function);
            //Nothing to do yet?
        } break;
        case GC_STRING:
        case GC_BROKEN_HEART: assert(false);
    }
}


void collect(VM *vm) {
    vm->gc.nextToExamine = 0;
    vm->gc.fromSpace = vm->gc.toSpace;

    vm->gc.fromSpaceSize = vm->gc.toSpaceSize;
    vm->gc.toSpaceSize = vm->gc.heapSize;
    int fd = open("/dev/zero", O_RDWR);
    vm->gc.toSpace = (char *)mmap(0, vm->gc.toSpaceSize,
                                  PROT_READ|PROT_WRITE,
                                  MAP_PRIVATE, fd, 0);
    close(fd);
    
    size_t numberOfAllocatedBytes = vm->gc.nextFree;
    vm->gc.nextFree = 0;
    for (size_t i = 0; i < size(&vm->apiStack); ++i) {
        visitValue(vm, &vm->apiStack[i]);
    }
    for (size_t i = 0; i < size(&vm->handles); ++i) {
        if (vm->handles[i]) {
            if (vm->handles[i]->type == GC_BROKEN_HEART) {
                vm->handles[i] = *((GCObject **)(vm->handles[i]+1));
            } else {
                switch (vm->handles[i]->type) {
                    case GC_OBJECT: {
                        vm->handles[i] = copyFromTo(vm, vm->handles[i],
                                                    sizeof(Object));
                    } break;
                    case GC_CONS_PAIR: {
                        vm->handles[i] = copyFromTo(vm, vm->handles[i],
                                                    sizeof(ConsPair));
                    } break;
                    case GC_FUNCTION: {
                        vm->handles[i] = copyFromTo(vm, vm->handles[i],
                                                    sizeof(Function));
                    } break;
                    case GC_UPVALUE:
                    case GC_STRING:
                    case GC_BROKEN_HEART:
                        assert(false);
                }
            }
            assert(vm->handles[i]->type < GC_BROKEN_HEART);
        }
    }
    for (size_t i = 0; i < vm->frameStackTop; ++i) {
        if (vm->frameStack[i].func->gcObj.type == GC_BROKEN_HEART) {
            vm->frameStack[i].func =
                *(Function **)((&vm->frameStack[i].func->gcObj)+1);
        } else {
            vm->frameStack[i].func =
                (Function *)copyFromTo(vm, &vm->frameStack[i].func->gcObj,
                                       sizeof(Function));
        }
        for (size_t j = 0; j < size(&vm->frameStack[i].registers);
             ++j) {
            visitValue(vm, &vm->frameStack[i].registers[j]);
        }
    }
    for (size_t i = 0; i < size(&vm->globals.slots); ++i) {
        visitValue(vm, &vm->globals.slots[i].key); // Might not be needed
        visitValue(vm, &vm->globals.slots[i].value);
    }
    if (vm->globals.parent) {
        fprintf(stderr, "The globals table can not yet have a parent :(\n");
        assert(false);
    }
    while(vm->gc.nextToExamine != vm->gc.nextFree) {
        GCObject *val = (GCObject *)(vm->gc.toSpace + vm->gc.nextToExamine);
        assert(val->type < GC_BROKEN_HEART);
        visitObject(vm, val);
    }

    if (vm->openUpvalueHead) {
        assert(vm->openUpvalueHead->gcObj.type == GC_BROKEN_HEART);
        vm->openUpvalueHead = visitUpvalue(vm, vm->openUpvalueHead);
        Upvalue *u = vm->openUpvalueHead;
        while (u) {
            u->next = visitUpvalue(vm, u->next);
            u = u->next;
        }
    }
    
    munmap(vm->gc.fromSpace, vm->gc.fromSpaceSize);
    size_t numberOfFreedBytes = (numberOfAllocatedBytes -
                                 vm->gc.nextFree);
    printf("Freed: %lu bytes\n", numberOfFreedBytes);
    vm->gc.fromSpace = 0;
    vm->gc.fromSpaceSize = 0;
}

void *alloc(VM *vm, size_t size) {
    if (!vm->gc.toSpace) {
        vm->gc.toSpaceSize = HEAP_START_SIZE;
        int fd = open("/dev/zero", O_RDWR);
        vm->gc.toSpace = (char *)mmap(0, vm->gc.toSpaceSize,
                                      PROT_READ|PROT_WRITE,
                                      MAP_PRIVATE, fd, 0);
        close(fd);
    }
    assert(vm->gc.nextFree + size > vm->gc.nextFree);
    if (vm->gc.nextFree + size > vm->gc.toSpaceSize) {
        fprintf(stderr, "Collection starting!\n");
        collect(vm);
        // Implement heap growing
        assert(vm->gc.nextFree + size <= vm->gc.toSpaceSize);
    }
    void *ret = vm->gc.toSpace + vm->gc.nextFree;
    vm->gc.nextFree += size;
    return ret;
}

Handle reserve(VM *vm, GCObject *obj) {
    assert(obj);
    for (size_t i = 0; i < size(&vm->handles); ++i) {
        if (!vm->handles[i]) {
            vm->handles[i] = obj;
            //fprintf(stderr, "reserved %lu\n", i);
            return Handle{i};
        }
    }
    add(&vm->handles, obj);
    //fprintf(stderr, "reserved %lu\n", size(&vm->handles)-1);
    return Handle{size(&vm->handles)-1};
}

Handle reserve(VM *vm, ConsPair *c) {
    assert(c);
    return reserve(vm, &c->gcObj);
}

Handle reserve(VM *vm, Object *o) {
    assert(o);
    return reserve(vm, &o->gcObj);
}

void free(VM *vm, Handle handle) {
    //fprintf(stderr, "free %lu\n", handle.handle);
    assert(vm->handles[handle.handle]);
    vm->handles[handle.handle] = 0;
}

ConsPair &getC(VM *vm, Handle handle) {
    assert(vm->handles[handle.handle]);
    assert(vm->handles[handle.handle]->type == GC_CONS_PAIR);
    return *((ConsPair *)vm->handles[handle.handle]);
}

Object &getO(VM *vm, Handle handle) {
    assert(vm->handles[handle.handle]);
    assert(vm->handles[handle.handle]->type == GC_OBJECT);
    return *((Object *)vm->handles[handle.handle]);
}

GCObjectType type(VM *vm, Handle handle) {
    assert(vm->handles[handle.handle]);
    assert(vm->handles[handle.handle]->type != GC_BROKEN_HEART);
    return vm->handles[handle.handle]->type;
}

ConsPair *allocConsPair(VM *vm) {
    ConsPair *ret = (ConsPair *)alloc(vm, sizeof(ConsPair));
    *ret = ConsPair();
    return ret;
}

Object *allocObject(VM *vm) {
    Object *ret = (Object *)alloc(vm, sizeof(Object));
    *ret = Object();
    return ret;
}

Function *allocFunction(VM *vm, FunctionPrototype *prototype) {
    Function *ret = (Function *)alloc(vm, sizeof(Function));
    *ret = Function();
    ret->gcObj.type = GC_FUNCTION;
    ret->prototype = prototype;
    resize(&ret->upvalues, size(&prototype->upvalues), (Upvalue *)0);
    return ret;
}

Upvalue *allocUpvalue(VM *vm) {
    Upvalue *ret = (Upvalue *)alloc(vm, sizeof(Upvalue));
    *ret = Upvalue();
    ret->next = vm->openUpvalueHead;
    vm->openUpvalueHead = ret;
    return ret;
}

// Stolen from wren
union DoubleBits {
  uint64_t bits64;
  uint32_t bits32[2];
  double doub;
};

// Stolen from wren
static inline uint32_t hashBits(DoubleBits bits)
{
  uint32_t result = bits.bits32[0] ^ bits.bits32[1];
  result ^= (result >> 20) ^ (result >> 12);
  result ^= (result >> 7) ^ (result >> 4);
  return result;
}

static inline uint32_t hashDouble(double doub) {
    DoubleBits bits;
    bits.doub = doub;
    return hashBits(bits);
}

// Change to doing this on stringAllocation instead
static inline uint32_t hashString(char *string)
{
  // FNV-1a hash. See: http://www.isthe.com/chongo/tech/comp/fnv/
  uint32_t hash = 2166136261u;
  for (; *string; string++)
  {
    hash ^= *string;
    hash *= 16777619;
  }
  return hash;
}

uint32_t hashValue(Value v) {
    switch (v.type) {
        case V_SYMBOL: {
            return v.sym.id;
        } break;
        case V_STRING: {
            return hashString(v.str);
        } break;
        case V_DOUBLE: {
            return hashDouble(v.doub);
        } break;
        case V_BOOLEAN: {
            return v.boolean;
        } break;
        default: {
            fprintf(stderr, "Can only hash strings, symbols, bools and doubles\n");
            assert(false);
        } break;
    }
}

static void resizeObject(VM *vm, Object *o, size_t newSize);

#define nextIdx(i) ((i + 1) % size(&o->slots))
#define prevIdx(i) ((i - 1) > size(&o->slots) ? size(&o->slots) - 1 : i - 1)
#define scaleHash(h, s) ((h) % (s))

// true if succeded, false if failed
static bool moveSlots(Object *o, size_t start) {
    if (scaleHash(hashValue(o->slots[start].key),
                  size(&o->slots)) != start) {
        return false;
    }
    size_t curr = nextIdx(start);
    while (curr != start && o->slots[curr].key.type != V_UNDEF) {
        uint32_t collidingMainIdx =
            scaleHash(hashValue(o->slots[curr].key),
                      size(&o->slots));
        if (collidingMainIdx != curr) {
            return false;
        }
        curr = nextIdx(curr);
    }
    if (curr == start) {
        return false;
    }
    curr = prevIdx(curr);
    while (curr != start) {
        o->slots[nextIdx(curr)] = o->slots[curr];
        curr = prevIdx(curr);
    }
    o->slots[nextIdx(curr)] = o->slots[curr];
    return true;
}

static void insertNewSlot(VM *vm, Object *o,
                          Value key, Value value) {
    // We use Brent's variation. like lua (YAY!).
    // Brent's variation mean that we:
    // find main position.
    // if collision:
    //   if colliding key is in its main position:
    //     move colliding key to its secondary position
    //   else:
    //     resize
    //     insert into the resized object
    // else:
    //   insert key in this slot
    //
    // This means that everything is either in the main slot
    // or the secondary slot, which means fast lookup
    assert(size(&o->slots) > 0);
    size_t s = size(&o->slots);
    uint32_t mainIdx = scaleHash(hashValue(key),size(&o->slots));
    if (o->slots[mainIdx].key.type != V_UNDEF) {
        // Collision
        uint32_t collidingMainIdx =
            scaleHash(hashValue(o->slots[mainIdx].key),
                      size(&o->slots));
        if (mainIdx == collidingMainIdx) {
            // colliding slot is in its main position
            if (moveSlots(o, mainIdx)) {
                // moved colliding slot to its secondary position
                o->slots[mainIdx].key = key;
                o->slots[mainIdx].value = value;
            } else {
                // cant move colliding slot
                resizeObject(vm, o, size(&o->slots)*2);
                assert(size(&o->slots) > 0);
                insertNewSlot(vm, o, key, value);
                return;
            }
        } else {
            if (o->slots[collidingMainIdx].key.type == V_UNDEF) {
                // can move colliding slot to it's main position
                o->slots[collidingMainIdx] = o->slots[mainIdx];
                o->slots[mainIdx].key = key;
                o->slots[mainIdx].value = value;
            } else {
                // can't put in secondary position
                resizeObject(vm, o, size(&o->slots)*2);
                assert(size(&o->slots) > 0);
                insertNewSlot(vm, o, key, value);
                return;
            }
        }
    } else {
        // main slot free
        o->slots[mainIdx].key = key;
        o->slots[mainIdx].value = value;
    }
}

// WARNING!!! When we implement our own DynamicArray
// we have to be carefull about this function returning a pointer.
Value *getSlot(Object *o, Value key) {
    uint32_t mainIdx = scaleHash(hashValue(key), size(&o->slots));
    if (o->slots[mainIdx].key == key) {
        // In main position
        return &o->slots[mainIdx].value;
    } else if (o->slots[nextIdx(mainIdx)].key == key) {
        // In secondary position
        return &o->slots[nextIdx(mainIdx)].value;
    } else {
        if (o->parent) {
            // Maybe in parent?
            return getSlot(o->parent, key);
        } else {
            // Not present
            return 0;
        }
    }
}

void insertSlot(VM *vm, Object *o, Value key, Value value) {
    assert(key.type == V_STRING || key.type == V_SYMBOL ||
           key.type == V_DOUBLE || key.type == V_BOOLEAN);
    Value *p = getSlot(o, key);
    if (p) {
        *p = value;
    } else {
        insertNewSlot(vm, o, key, value);
    }
}

static void resizeObject(VM *vm, Object *o, size_t newSize) {
    DynamicArray<ObjectSlot> oldSlots = o->slots;
    o->slots = DynamicArray<ObjectSlot>();
    ObjectSlot emptySlot{Value{V_UNDEF}, Value{V_BOOLEAN}};
    emptySlot.value.boolean = false;
    resize(&o->slots, newSize, emptySlot);
    assert(size(&o->slots) > 0);
    for (size_t i = 0; i < size(&oldSlots); ++i) {
        if (oldSlots[i].key.type != V_UNDEF) {
            insertSlot(vm, o, oldSlots[i].key,
                       oldSlots[i].value);
        }
    }
    assert(size(&o->slots) > 0);
}

void set(VM *vm, Object *o, Value key, Value value) {
    assert(size(&o->slots) > 0);
    insertSlot(vm, o, key, value);
}

Value &get(Object *o, Value key) {
    Value *v = getSlot(o, key);
    assert(v);
    return *v;
}

bool keyExists(Object *o, Value key) {
    return getSlot(o, key);
}

void clearStack(VM *vm) {
    clear(&vm->apiStack);
}

void clear(Object *obj) {
    clear(&obj->slots);
}

void freeVM(VM *vm) {
    munmap(vm->gc.toSpace, vm->gc.toSpaceSize);
    munmap(vm->gc.fromSpace, vm->gc.fromSpaceSize);
}

void getGlobal(VM *vm, Symbol variable) {
    Value v = {V_SYMBOL};
    v.sym = variable;
    add(&vm->apiStack, get(&vm->globals, v));
}

void setGlobal(VM *vm, Symbol variable) {
    Value value = pop(vm);
    Value v = {V_SYMBOL};
    v.sym = variable;
    set(vm, &vm->globals, v, value);
}

Value peek(VM *vm, int idx) {
    if (idx < 1) {
        idx = size(&vm->apiStack) + idx;
        return vm->apiStack[idx];
    } else {
        return vm->apiStack[idx];
    }
}

Value pop(VM *vm) {
    return pop(&vm->apiStack);
}

bool popBoolean(VM *vm) {
    Value ret = pop(vm);
    assert(ret.type == V_BOOLEAN);
    return ret.boolean;
}

double popDouble(VM *vm) {
    Value ret = pop(vm);
    assert(ret.type == V_DOUBLE);
    return ret.doub;
}

Symbol popSymbol(VM *vm) {
    Value ret = pop(vm);
    assert(ret.type == V_SYMBOL);
    return ret.sym;
}

void car(VM *vm) {
    Value pair = pop(vm);
    assert(pair.type == V_CONS_PAIR);
    assert(pair.pair);
    pushValue(vm, pair.pair->car);
}

void cdr(VM *vm) {
    Value pair = pop(vm);
    assert(pair.type == V_CONS_PAIR);
    assert(pair.pair);
    pushValue(vm, pair.pair->cdr);
}

void dup(VM *vm) {
    pushValue(vm, peek(vm, -1));
}

bool isEmptyList(VM *vm) {
    Value top = peek(vm, -1);
    return top.type == V_CONS_PAIR && top.pair == 0;
}

void pushValue(VM *vm, Value v) {
    add(&vm->apiStack, v);
}

void pushCFunction(VM *vm, CFunction cfunc) {
    Value v{V_CFUNCTION};
    v.cfunc = cfunc;
    pushValue(vm, v);
}

void pushDouble(VM *vm, double doub) {
    Value v{V_DOUBLE};
    v.doub = doub;
    pushValue(vm, v);
}

void pushSymbol(VM *vm, char *symstr) {
    Value v = {V_SYMBOL};
    v.sym = intern(vm, symstr);
    pushValue(vm, v);
}

void pushSymbol(VM *vm, Symbol symbol) {
    Value v = {V_SYMBOL};
    v.sym = symbol;
    pushValue(vm, v);
}

void pushString(VM *vm, char *str) {
    Value v{V_STRING};
    v.str = strdup(str); // LEAK!!!
    pushValue(vm, v);
}

void pushBoolean(VM *vm, bool boolean) {
    Value v{V_BOOLEAN};
    v.boolean = boolean;
    pushValue(vm, v);
}

void pushOpaque(VM *vm, void *opaque, uint64 type) {
    Value v{V_OPAQUE_POINTER};
    v.opaque = opaque;
    v.opaqueType = type;
    pushValue(vm, v);
}

void pushHandle(VM *vm, Handle handle) {
    Value v = {V_UNDEF};
    switch (type(vm, handle)) {
        case GC_OBJECT: {
            v.type = V_OBJECT;
            v.object = &getO(vm, handle);
        } break;
        case GC_CONS_PAIR: {
            v.type = V_CONS_PAIR;
            v.pair = &getC(vm, handle);
        } break;
        default:assert(false);
    }
    pushValue(vm, v);
}

Symbol intern(VM *vm, const char *str) {
    Value vStr{V_STRING};
    // unsafe, so take care
    vStr.str = (char *)str;
    Symbol symbol;
    if (keyExists(&vm->symbolTable, vStr)) {
        symbol = get(&vm->symbolTable, vStr).sym;
    } else {
        // WARNING!! Leaking!!
        Value s = vStr;
        s.str = strdup(str);
        symbol.str = s.str;
        symbol.id = vm->symbolIdTop;
        vm->symbolIdTop++;
        Value v = {V_SYMBOL};
        v.sym = symbol;
        set(vm, &vm->symbolTable, s, v);
    }
    return symbol;
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
            printf("<cfunc: %p>", v.cfunc);
        } break;
        case V_BOOLEAN: {
            printf("%s", v.boolean ? "true" : "false");
        } break;
        case V_DOUBLE: {
            printf("%f", v.doub);
        } break;
        case V_SYMBOL: {
            printf("<%s %d>", v.sym.str, v.sym.id);
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

void printOpCode(OpCode undecoded) {
    OpCode op = getOp(undecoded);
    printf("%s ", opCodeStr[op]);
    switch (opCodeTypes[op]) {
        case OT_RS: {
            uint8 reg = getRegA(undecoded);
            int32 immId = getSImm(undecoded);
            printf("r%x i%d\n", (uint32)reg, immId);
        } break;
        case OT_S: {
            int32 immId = getSImm(undecoded);
            printf("i%d\n", immId);
        } break;
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
        case OP_LOAD_NULL: {
            uint8 reg = getRegA(undecoded);
            Value v = {V_CONS_PAIR};
            v.pair = 0;
            frame->registers[reg] = v;
            frame->pc++;
        } break;
        case OP_CONS: {
            uint8 dstReg = getRegA(undecoded);
            uint8 carReg = getRegB(undecoded);
            uint8 cdrReg = getRegC(undecoded);
            Value v = {V_CONS_PAIR};
            v.pair = allocConsPair(vm);
            v.pair->car = frame->registers[carReg];
            v.pair->cdr = frame->registers[cdrReg];
            frame->registers[dstReg] = v;
            frame->pc++;
        } break;
        case OP_LOADK: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            frame->registers[reg] = frame->func->prototype->constants[k];
            frame->pc++;
        } break;
        case OP_SETUP_CALL: {
            uint8 calleeReg = getRegA(undecoded);
            Value callee = frame->registers[calleeReg];
            if (callee.type == V_FUNCTION) {
                goto call;
            } else if (callee.type == V_CFUNCTION) {
                goto ccall;
            }
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
            if (!keyExists(&vm->globals,
                           frame->func->prototype->constants[k])) {
                fprintf(stderr, "ERROR: Global '%s does not exist\n",
                        frame->func->prototype->constants[k].sym.str);
                assert(false);
                        
            }

            frame->registers[reg] = get(&vm->globals,
                                        frame->func->prototype->constants[k]);
            frame->pc++;
        } break;
        case OP_SET_GLOBAL: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            if (frame->func->prototype->constants[k].type != V_SYMBOL) {
                fprintf(stderr, "ICE: GET_GLOBAL imm not a symbol\n");
                assert(false);
            }
            if (!keyExists(&vm->globals,
                           frame->func->prototype->constants[k])) {
                fprintf(stderr, "ERROR: Global '%s does not exist\n",
                        frame->func->prototype->constants[k].sym.str);
                assert(false);
                        
            }

            set(vm, &vm->globals, frame->func->prototype->constants[k],
                frame->registers[reg]);
            frame->pc++;
        } break;
        case OP_GET_UPVALUE: {
            uint8 reg = getRegA(undecoded);
            uint32 upvalueIdx = getImm(undecoded);
            frame->registers[reg] =
                *frame->func->upvalues[upvalueIdx]->value;
            frame->pc++;
        } break;
        case OP_SET_UPVALUE: {
            uint8 reg = getRegA(undecoded);
            uint32 upvalueIdx = getImm(undecoded);
            *frame->func->upvalues[upvalueIdx]->value =
                frame->registers[reg];
            frame->pc++;
        } break;
        case OP_JMP_IF_FALSE: {
            uint8 reg = getRegA(undecoded);
            int32 relAddr = getSImm(undecoded);
            assert(frame->registers[reg].type == V_BOOLEAN);
            bool pred = frame->registers[reg].boolean;
            if (!pred) {
                frame->pc += relAddr;
            }
            frame->pc++;
        } break;
        case OP_JMP: {
            int32 relAddr = getSImm(undecoded);
            frame->pc += relAddr;
            frame->pc++;
        } break;
        case OP_MOVE: {
            uint8 a = getRegA(undecoded);
            uint8 b = getRegB(undecoded);
            frame->registers[a] = frame->registers[b];
            frame->pc++;
        } break;
        case OP_LOAD_UNDEF: {
            uint8 reg = getRegA(undecoded);
            frame->registers[reg] = Value{V_UNDEF};
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
    ccall:
        uint8 calleeReg = getRegA(undecoded);
        Value callee = frame->registers[calleeReg];
        assert(callee.type == V_CFUNCTION);
        frame->pc++;
        uint8 numArgs = 0;
    cpushArgLoop:
        undecoded = frame->func->prototype->code[frame->pc];
        instr = getOp(undecoded);
        switch(instr) {
            case OP_PUSH_ARG: {
                uint8 srcReg = getRegA(undecoded);
                pushValue(vm, frame->registers[srcReg]);
                numArgs++;
                frame->pc++;
            } break;
            case OP_CALL: {
                frame->pc++;
                uint8 retReg = getRegA(undecoded);
                size_t numRet = callee.cfunc(vm, numArgs);
                if (numRet) {
                    frame->registers[retReg] = pop(vm);
                } else {
                    frame->registers[retReg].type = V_UNDEF;
                }
                //frame = calleeFrame;
                //frameID = calleeFrameID;
                //if (dstReg != frame->func->prototype->numArgs) {
                //fprintf(stderr, "Not correct amount of arguments!\n");
                //assert(false);
                //}
                goto loop;
            } break;
            default: {
                fprintf(stderr,
                        "ICE: Illegal %s between SETUP_CALL and CALL\n",
                        opCodeStr[instr]);
                assert(false);
            } break;
        }
        goto cpushArgLoop;
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

void doString(VM *vm, const char *prog, size_t numArgs, bool verbose) {
    pushValue(vm, compileString(vm, (char *)prog, verbose));
    call(vm, numArgs);
}

void doFile(VM *vm, const char *path, size_t numArgs, bool verbose) {
    pushValue(vm, compileFile(vm, path, verbose));
    for (size_t i = 0; i < size(&vm->funcProtos); ++i) {
        printFuncProtoCode(&vm->funcProtos[i]);
    }
    call(vm, numArgs);
}

size_t lispisAnd(VM *vm, size_t numArgs) {
    bool ret = true;
    while (numArgs) {
        ret = ret && popBoolean(vm);
        numArgs--;
    }
    pushBoolean(vm, ret);
    return 1;
}

size_t lispisOr(VM *vm, size_t numArgs) {
    bool ret = false;
    while (numArgs) {
        ret = ret || popBoolean(vm);
        numArgs--;
    }
    pushBoolean(vm, ret);
    return 1;
}

size_t lispisLT(VM *vm, size_t numArgs) {
    if (numArgs < 1) {
        pushBoolean(vm, false);
        return 1;
    } else {
        double prev = popDouble(vm);
        numArgs--;
        while (numArgs) {
            double curr = popDouble(vm);
            if (prev < curr) {
                pushBoolean(vm, false);
                return 1;
            }
            prev = curr;
            numArgs--;
        }
        pushBoolean(vm, true);
        return 1;
    }
}

size_t lispisGT(VM *vm, size_t numArgs) {
    if (numArgs < 1) {
        pushBoolean(vm, false);
        return 1;
    } else {
        double prev = popDouble(vm);
        numArgs--;
        while (numArgs) {
            double curr = popDouble(vm);
            if (prev > curr) {
                pushBoolean(vm, false);
                return 1;
            }
            prev = curr;
            numArgs--;
        }
        pushBoolean(vm, true);
        return 1;
    }
}

size_t lispisEQ(VM *vm, size_t numArgs) {
    if (numArgs < 1) {
        pushBoolean(vm, true);
        return 1;
    } else {
        bool ret = true;
        Value last = pop(vm);
        numArgs--;
        while (numArgs) {
            ret = ret && last == pop(vm);
            numArgs--;
        }
        pushBoolean(vm, ret);
        return 1;
    }
}

size_t lispisList(VM *vm, size_t numArgs) {
    Value nil = {V_CONS_PAIR};
    nil.pair = 0;
    if (numArgs > 0) {
        Handle prev = reserve(vm, allocConsPair(vm));
        getC(vm, prev).car = pop(vm);
        getC(vm, prev).cdr = nil;
        numArgs--;
        while (numArgs) {
            Handle newHead = reserve(vm, allocConsPair(vm));
            getC(vm, newHead).car = pop(vm);
            getC(vm, newHead).cdr.type = V_CONS_PAIR;
            getC(vm, newHead).cdr.pair = &getC(vm, prev);
            free(vm, prev);
            prev = newHead;
            numArgs--;
        }
        Value ret = {V_CONS_PAIR};
        ret.pair = &getC(vm, prev);
        free(vm, prev);
        pushValue(vm, ret);
    } else {
        pushValue(vm, nil);
    }
    return 1;
}

VM initVM(bool loadDefaults) {
    VM vm;
    if (loadDefaults) {
        pushCFunction(&vm, lispisAnd);
        setGlobal(&vm, intern(&vm, "and"));
        pushCFunction(&vm, lispisOr);
        setGlobal(&vm, intern(&vm, "or"));
        pushCFunction(&vm, lispisLT);
        setGlobal(&vm, intern(&vm, "<"));
        pushCFunction(&vm, lispisGT);
        setGlobal(&vm, intern(&vm, ">"));
        pushCFunction(&vm, lispisEQ);
        setGlobal(&vm, intern(&vm, "="));
        pushCFunction(&vm, lispisList);
        setGlobal(&vm, intern(&vm, "list"));
    }
    return vm;
}