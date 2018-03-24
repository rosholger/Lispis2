#include "vm.h"

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
};

#define OPCODE(op) #op,
const char *opCodeStr[] = {
#include "opcodes.h"
};
#undef OPCODE

Object::Object() : gcObj(GCObject{GC_OBJECT}) {
    resize(&slots, 1);
    slots[0].key.type = V_UNDEF;
}

ConsPair::ConsPair() : gcObj(GCObject{GC_CONS_PAIR}) {}

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

// TODO: remove iterator stuff
void visitObject(VM *vm, GCObject *o) {
    if (o->type >= GC_BROKEN_HEART) {
        assert(false);
    }
    switch (o->type) {
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
        case GC_STRING: break;
        default:assert(false);
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
                    default:assert(false);
                }
            }
            assert(vm->handles[i]->type < GC_BROKEN_HEART);
        }
    }
    for (size_t i = 0; i < vm->frameStackTop; ++i) {
        for (size_t j = 0; j < size(&vm->frameStack[i].registers);
             ++j) {
            visitValue(vm, &vm->frameStack[i].registers[j]);
        }
    }
    while(vm->gc.nextToExamine != vm->gc.nextFree) {
        GCObject *val = (GCObject *)(vm->gc.toSpace + vm->gc.nextToExamine);
        assert(val->type <= GC_BROKEN_HEART);
        visitObject(vm, val);
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

// Stolen from wren
typedef union
{
  uint64_t bits64;
  uint32_t bits32[2];
  double doub;
} DoubleBits;

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

static inline uint32_t hashValue(Value v) {
    switch (v.type) {
        case V_SYMBOL: {
            return v.symid;
        } break;
        case V_STRING: {
            return hashString(v.str);
        } break;
        case V_DOUBLE: {
            return hashDouble(v.doub);
        } break;
        default: {
            fprintf(stderr, "Can only hash strings, symbols and doubles\n");
            assert(false);
        } break;
    }
}

static void resizeObject(VM *vm, Object *o, size_t newSize);

#define nextIdx(i) (i + 1) % size(&o->slots)

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
    uint32_t mainIdx = hashValue(key) % size(&o->slots);
    if (o->slots[mainIdx].key.type != V_UNDEF) {
        // Collision
        uint32_t collidingMainIdx = (hashValue(o->slots[mainIdx].key) %
                                     size(&o->slots));
        if (mainIdx == collidingMainIdx) {
            // colliding slot is in this ones main position
            if (o->slots[nextIdx(mainIdx)].key.type != V_UNDEF) {
                // can't move colliding slot
                resizeObject(vm, o, size(&o->slots)*2);
                insertNewSlot(vm, o, key, value);
                return;
            } else {
                // move colliding slot and insert this one in
                // its main position
                o->slots[nextIdx(mainIdx)] = o->slots[mainIdx];
                o->slots[mainIdx].key = key;
                o->slots[mainIdx].value = value;
            }
        } else {
            if (o->slots[collidingMainIdx].key.type == V_UNDEF) {
                // can move colliding slot to it's main position
                o->slots[collidingMainIdx] = o->slots[mainIdx];
                o->slots[mainIdx].key = key;
                o->slots[mainIdx].value = value;
            } else if (o->slots[nextIdx(mainIdx)].key.type != V_UNDEF) {
                // can't put in secondary position
                resizeObject(vm, o, size(&o->slots)*2);
                insertNewSlot(vm, o, key, value);
                return;
            } else {
                // put in secondary slot
                o->slots[nextIdx(mainIdx)].key = key;
                o->slots[nextIdx(mainIdx)].value = value;
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
    uint32_t mainIdx = hashValue(key) % size(&o->slots);
    if (o->slots[mainIdx].key == key) {
        // In main position
        return &o->slots[mainIdx].value;
    } else if (o->slots[nextIdx(mainIdx)].key == key) {
        // In secondary position
        return &o->slots[nextIdx(mainIdx)].value;
    } else {
        // Not present
        return 0;
    }
}

void insertSlot(VM *vm, Object *o, Value key, Value value) {
    assert(key.type == V_STRING || key.type == V_SYMBOL ||
           key.type == V_DOUBLE);
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
    for (size_t i = 0; i < size(&oldSlots); ++i) {
        if (oldSlots[i].key.type != V_UNDEF) {
            insertSlot(vm, o, oldSlots[i].key,
                       oldSlots[i].value);
        }
    }
}

void add(VM *vm, Object *o, Value key, Value value) {
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