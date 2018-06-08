#include "vm.h"
#include "compiler.h"

#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstdio>
#include <cassert>
#include <cstring>
#include "common.h"
#include <cstdarg>


OpCodeType opCodeTypes[] = {
#define OPCODE(op, type) OT_ ## type,
#include "opcodes.h"
#undef OPCODE
};

#define OPCODE(op, type) #op,
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
    opaque = 0;
}

Value::Value(ValueType t) : type(t) {
    //Null value
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
                    //fprintf(stderr, "upd %p to ", v->pair);
                    v->pair = *(ConsPair **)((&v->object->gcObj)+1);
                    //fprintf(stderr, "%p\n", v->pair);
                } else {
                    v->pair = (ConsPair *)copyFromTo(vm, &v->pair->gcObj,
                                                     sizeof(ConsPair));
                }
                if (v->pair->lineInfo) {
                    if (v->pair->lineInfo->gcObj.type ==
                        GC_BROKEN_HEART) {
                        v->pair->lineInfo =
                            *(LineInfo **)((&v->object->gcObj)+1);
                        //fprintf(stderr, "%p\n", v->pair);
                    } else {
                        v->pair->lineInfo =
                            (LineInfo *)copyFromTo(vm,
                                                   &v->pair->lineInfo->gcObj,
                                                   sizeof(LineInfo));
                    }
                }
                assert(v->pair->gcObj.type <= GC_BROKEN_HEART);
            }
        } break;
        case V_FUNCTION: {
            if (v->func->gcObj.type == GC_BROKEN_HEART) {
                v->func = *(Closure **)((&v->func->gcObj)+1);
            } else {
                v->func = (Closure *)copyFromTo(vm, &v->func->gcObj,
                                                sizeof(Closure));
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
    if (upvalue) {
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
    } else {
        return upvalue;
    }
}

// TODO: remove iterator stuff
void visitObject(VM *vm, GCObject *o) {
    if (o->type >= GC_BROKEN_HEART) {
        assert(false);
    }
    switch (o->type) {
        case GC_LINE_INFO: {
            vm->gc.nextToExamine += sizeof(LineInfo);
        } break;
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
            Closure *func = (Closure *)o;
            for (size_t i = 0; i < size(&func->upvalues); ++i) {
                if (func->upvalues[i]) {
                    func->upvalues[i] = visitUpvalue(vm,
                                                     func->upvalues[i]);
                }
            }
            vm->gc.nextToExamine += sizeof(Closure);
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
    for (size_t i = 0; i < vm->apiStackTop; ++i) {
        visitValue(vm, &vm->apiStack[i]);
    }
    for (size_t i = 0; i < size(&vm->handles); ++i) {
        switch (vm->handles[i].type) {
            case V_OBJECT: {
                if (vm->handles[i].object->gcObj.type ==
                    GC_BROKEN_HEART) {
                    vm->handles[i].object =
                        *((Object **)((&vm->handles[i].object->gcObj) +
                                      1));
                } else {
                    vm->handles[i].object =
                        (Object *)copyFromTo(vm,
                                             &vm->handles[i].object->gcObj,
                                             sizeof(Object));
                }
            } break;
            case V_CONS_PAIR: {
                if (vm->handles[i].pair) {
                    if (vm->handles[i].pair->gcObj.type ==
                        GC_BROKEN_HEART) {
                        vm->handles[i].pair =
                            *((ConsPair **)((&vm->handles[i].pair->gcObj) +
                                            1));
                    } else {
                        vm->handles[i].pair =
                            (ConsPair *)copyFromTo(vm,
                                                   &vm->handles[i].pair->gcObj,
                                                   sizeof(ConsPair));
                    }
                }
            } break;
            case V_FUNCTION: {
                if (vm->handles[i].func->gcObj.type ==
                    GC_BROKEN_HEART) {
                    vm->handles[i].func =
                        *((Closure **)((&vm->handles[i].func->gcObj) +
                                        1));
                } else {
                    vm->handles[i].func =
                        (Closure *)copyFromTo(vm,
                                              &vm->handles[i].func->gcObj,
                                              sizeof(Closure));
                }
            } break;
            default:break;
        }
    }
    for (size_t i = 0; i < vm->frameStackTop; ++i) {
        if (vm->frameStack[i].func->gcObj.type == GC_BROKEN_HEART) {
            vm->frameStack[i].func =
                *(Closure **)((&vm->frameStack[i].func->gcObj)+1);
        } else {
            vm->frameStack[i].func =
                (Closure *)copyFromTo(vm, &vm->frameStack[i].func->gcObj,
                                      sizeof(Closure));
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
    for (size_t i = 0; i < size(&vm->symbolTable.slots); ++i) {
        visitValue(vm, &vm->symbolTable.slots[i].key);
        visitValue(vm, &vm->symbolTable.slots[i].value);
    }
    for (size_t i = 0; i < size(&vm->macros.slots); ++i) {
        visitValue(vm, &vm->macros.slots[i].key); // Might not be needed
        visitValue(vm, &vm->macros.slots[i].value);
    }
    if (vm->globals.parent.type == V_OBJECT &&
        vm->globals.parent.object) {
        visitValue(vm, &vm->globals.parent);
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

    assert(!munmap(vm->gc.fromSpace, vm->gc.fromSpaceSize));
    size_t numberOfFreedBytes = (numberOfAllocatedBytes -
                                 vm->gc.nextFree);
    l_fprintf(stdout, "Freed: %lu bytes\n", numberOfFreedBytes);
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
        l_fprintf(stderr, "Collection starting!\n");
        collect(vm);
        // Implement heap growing
        assert(vm->gc.nextFree + size <= vm->gc.toSpaceSize);
    }
    void *ret = vm->gc.toSpace + vm->gc.nextFree;
    vm->gc.nextFree += size;
    return ret;
}

Handle reserve(VM *vm, Value v) {
    assert(v.type != V_UNDEF);
    for (size_t i = 0; i < size(&vm->handles); ++i) {
        if (vm->handles[i].type == V_UNDEF) {
            vm->handles[i] = v;
            return Handle{i};
        }
    }
    add(&vm->handles, v);
    return Handle{size(&vm->handles)-1};
}

Handle reserve(VM *vm, ConsPair *p) {
    assert(p);
    Value v = {V_CONS_PAIR};
    v.pair = p;
    return reserve(vm, v);
}

void free(VM *vm, Handle handle) {
    assert(vm->handles[handle.handle].type != V_UNDEF);
    vm->handles[handle.handle].type = V_UNDEF;
}

Value &get(VM *vm, Handle handle) {
    assert(vm->handles[handle.handle].type != V_UNDEF);
    return vm->handles[handle.handle];
}

ValueType type(VM *vm, Handle handle) {
    assert(vm->handles[handle.handle].type != V_UNDEF);
    return vm->handles[handle.handle].type;
}

Value allocConsPair(VM *vm) {
    Value ret = {V_CONS_PAIR};
    ret.pair = (ConsPair *)alloc(vm, sizeof(ConsPair));
    *ret.pair = ConsPair();
    ret.pair->lineInfo = 0;
    return ret;
}

Object *allocObject(VM *vm) {
    Object *ret = (Object *)alloc(vm, sizeof(Object));
    *ret = Object();
    return ret;
}

FunctionPrototype *getProto(VM *vm, Closure *func) {
    return &vm->funcProtos[func->protoID];
}

FunctionPrototype *getProto(VM *vm, ActivationFrame *frame) {
    return &vm->funcProtos[frame->func->protoID];
}

Closure *allocFunction(VM *vm, size_t protoID) {
    Closure *ret = (Closure *)alloc(vm, sizeof(Closure));
    *ret = Closure();
    ret->gcObj.type = GC_FUNCTION;
    ret->protoID = protoID;
    resize(&ret->upvalues, size(&getProto(vm, ret)->upvalues),
           (Upvalue *)0);
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
            l_fprintf(stderr, "Can only hash strings, symbols, bools and doubles\n");
            assert(false);
        } break;
    }
}

static void resizeObject(VM *vm, Object *o, size_t newSize);

#define nextIdx(i) ((i + 1) % size(&o->slots))
#define prevIdx(i) ((i - 1) > size(&o->slots) ? size(&o->slots) - 1 : i - 1)
#define scaleHash(h, s) ((h) % (s))

//// true if succeded, false if failed
//static bool moveSlots(Object *o, size_t start) {
//if (scaleHash(hashValue(o->slots[start].key),
//size(&o->slots)) != start) {
//return false;
//}
//size_t curr = nextIdx(start);
//while (curr != start && o->slots[curr].key.type != V_UNDEF) {
//uint32_t collidingMainIdx =
//scaleHash(hashValue(o->slots[curr].key),
//size(&o->slots));
//if (collidingMainIdx != curr) {
//return false;
//}
//curr = nextIdx(curr);
//}
//if (curr == start) {
//return false;
//}
//curr = prevIdx(curr);
//while (curr != start) {
//o->slots[nextIdx(curr)] = o->slots[curr];
//curr = prevIdx(curr);
//}
//o->slots[nextIdx(curr)] = o->slots[curr];
//return true;
//}

static ObjectSlot *findEmptySlot(Object *o, size_t start) {
    size_t curr = start;
    while (nextIdx(curr) != start &&
           o->slots[curr].key.type != V_UNDEF) {
        curr = nextIdx(curr);
    }
    if (o->slots[curr].key.type != V_UNDEF) {
        return 0;
    }
    return &o->slots[curr];
}

static ObjectSlot *insertNewSlot(VM *vm, Object *o,
                                 Value key, Value value) {
    // WRONG!!! If we have insert three keys with same hash we get
    // resize spiral of death.
    // Should just be that one of them is in its main position
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
        // Wrong! Should be:
        // Find a free pos. if we cant, grow and try again.
        // if colliding slot is not in its main position
        // move it to the free position. otherwise
        // insert into the free position.
        // Sadly this means we need tompstones.
        // Collision
        ObjectSlot *freePos = findEmptySlot(o, mainIdx);
        if (!freePos) {
            resizeObject(vm, o, size(&o->slots)*2);
            return insertNewSlot(vm, o, key, value);
        }
        uint32_t collidingMainIdx =
            scaleHash(hashValue(o->slots[mainIdx].key),
                      size(&o->slots));
        if (mainIdx == collidingMainIdx) {
            freePos->key = key;
            freePos->value = value;
            return freePos;
        } else {
            if (o->slots[collidingMainIdx].key.type == V_UNDEF) {
                // Correct
                // can move colliding slot to it's main position
                o->slots[collidingMainIdx] = o->slots[mainIdx];
                o->slots[mainIdx].key = key;
                o->slots[mainIdx].value = value;
                return &o->slots[mainIdx];
            } else {
                *freePos = o->slots[mainIdx];
                o->slots[mainIdx].key = key;
                o->slots[mainIdx].value = value;
                return &o->slots[mainIdx];
            }
        }
    } else {
        // main slot free
        o->slots[mainIdx].key = key;
        o->slots[mainIdx].value = value;
        return &o->slots[mainIdx];
    }
}

// WARNING!!! When we implement our own DynamicArray
// we have to be carefull about this function returning a pointer.
ObjectSlot *getSlot(VM *vm, Object *o, Value key,
                    bool searchParents = true) {
    uint32 mainIdx = scaleHash(hashValue(key), size(&o->slots));
    uint32 currIdx = mainIdx;
    while (o->slots[currIdx].key != key) {
        if (nextIdx(currIdx) == mainIdx) {
            if (searchParents && o->parent.type == V_OBJECT &&
                o->parent.object) {
                return getSlot(vm, o->parent.object, key);
            } else {
                return 0;
            }
        }
        if (o->slots[currIdx].key.type == V_UNDEF &&
            !o->slots[currIdx].key.boolean) {
            // Not tombstone
            if (searchParents && o->parent.type == V_OBJECT &&
                o->parent.object) {
                return getSlot(vm, o->parent.object, key);
            } else {
                return 0;
            }
        }
        currIdx = nextIdx(currIdx);
    }
    return &o->slots[currIdx];
}

ObjectSlot *insertSlot(VM *vm, Object *o, Value key, Value value) {
    assert(key.type == V_STRING || key.type == V_SYMBOL ||
           key.type == V_DOUBLE || key.type == V_BOOLEAN);
    ObjectSlot *p = getSlot(vm, o, key, false);
    if (p) {
        p->value = value;
        return p;
    } else {
        return insertNewSlot(vm, o, key, value);
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
    //if (key.type == V_SYMBOL && key.sym == vm->parentSym) {
    //assert(value.type == V_OBJECT);
    //o->parent = value;
    //}
    assert(value.type != V_UNDEF);
    assert(size(&o->slots) > 0);
    insertSlot(vm, o, key, value);
}

Value &get(VM *vm, Object *o, Value key) {
    //if (key.type == V_SYMBOL && key.sym == vm->parentSym) {
    //assert(o->parent.type == V_OBJECT && o->parent.object);
    //return o->parent;
    //}
    ObjectSlot *v = getSlot(vm, o, key);
    assert(v && v->value.type != V_UNDEF);
    return v->value;
}

bool keyExists(VM *vm, Object *o, Value key) {
    //if (key.type == V_SYMBOL && key.sym == vm->parentSym) {
    //return (o->parent.type == V_OBJECT && o->parent.object);
    //}
    assert(o);
    ObjectSlot *slot = getSlot(vm, o, key);
    return slot && slot->value.type != V_UNDEF;
}

void remove(VM *vm, Object *o, Value key) {
    assert(keyExists(vm, o, key));
    //if (key.type == V_SYMBOL && key.sym == vm->parentSym) {
    //o->parent.type = V_UNDEF;
    //}
    ObjectSlot *slot = getSlot(vm, o, key, false);
    if (!slot) {
        Value dummy = {V_UNDEF};
        slot = insertSlot(vm, o, key, dummy);
    } else {
        if (o->parent.type == V_OBJECT) {
            ObjectSlot *slotInParent = getSlot(vm, o->parent.object,
                                               key);
            if (slotInParent) {
                slot->value.type = V_UNDEF;
            } else {
                slot->key.type = V_UNDEF;
                slot->key.boolean = false;
            }
        }
    }
}

void clearStack(VM *vm) {
    vm->apiStackTop = 0;
}

void clear(Object *obj) {
    clear(&obj->slots);
}

void freeVM(VM *vm) {
    for (size_t i = 0; i < size(&vm->handles); ++i) {
        if (vm->handles[i].type != V_UNDEF) {
            l_fprintf(stderr, "LEAKING %lu\n", i);
        }
    }
    munmap(vm->gc.toSpace, vm->gc.toSpaceSize);
    munmap(vm->gc.fromSpace, vm->gc.fromSpaceSize);
}

void getGlobal(VM *vm, Symbol variable) {
    Value v = {V_SYMBOL};
    v.sym = variable;
    pushValue(vm, get(vm, &vm->globals, v));
}

void setGlobal(VM *vm, Symbol variable) {
    Value value = pop(vm);
    Value v = {V_SYMBOL};
    v.sym = variable;
    set(vm, &vm->globals, v, value);
}

void setMacro(VM *vm, Symbol variable) {
    Value value = pop(vm);
    Value v = {V_SYMBOL};
    v.sym = variable;
    set(vm, &vm->macros, v, value);
}

Value peek(VM *vm, int idx) {
    if (idx < 0) {
        idx = vm->apiStackTop + idx;
        return vm->apiStack[idx];
    } else {
        return vm->apiStack[idx];
    }
}

Value pop(VM *vm) {
    assert(vm->apiStackTop);
    vm->apiStackTop--;
    assert(vm->apiStackTop >= vm->apiStackBottom);
    return vm->apiStack[vm->apiStackTop];
    //return pop(&vm->apiStack);
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

double peekDouble(VM *vm, int idx) {
    Value ret = peek(vm, idx);
    assert(ret.type == V_DOUBLE);
    return ret.doub;
}

char *peekString(VM *vm, int idx) {
    Value ret = peek(vm, idx);
    assert(ret.type == V_STRING);
    return ret.str;
}

char *popString(VM *vm) {
    Value ret = pop(vm);
    assert(ret.type == V_STRING);
    return ret.str;
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

void cons(VM *vm) {
    Handle cdr = reserve(vm, pop(vm));
    Handle car = reserve(vm, pop(vm));
    Value ret = allocConsPair(vm);
    ret.pair->car = get(vm, car);
    ret.pair->cdr = get(vm, cdr);
    pushValue(vm, ret);
    free(vm, cdr);
    free(vm, car);
}

void dup(VM *vm) {
    pushValue(vm, peek(vm, -1));
}

void swap(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    pushValue(vm, a);
    pushValue(vm, b);
}

void setObjectSlot(VM *vm) {
    Value value = pop(vm);
    Value key = pop(vm);
    Value obj = peek(vm, -1);
    if (key.sym == vm->parentSym) {
        assert(value.type == V_OBJECT && value.object);
        obj.object->parent = value;
    } else {
        set(vm, obj.object, key, value);
    }
}

void getObjectSlot(VM *vm) {
    Value key = pop(vm);
    Value obj = pop(vm);
    assert(obj.object);
    if (key.sym == vm->parentSym) {
        assert(obj.object->parent.type == V_OBJECT &&
               obj.object->parent.object);
        pushValue(vm, obj.object->parent);
        return;
    }
    pushValue(vm, get(vm, obj.object, key));
}

bool isEmptyList(VM *vm) {
    Value top = peek(vm, -1);
    return top.type == V_CONS_PAIR && top.pair == 0;
}

bool isList(VM *vm) {
    Value top = peek(vm, -1);
    return top.type == V_CONS_PAIR;
}

bool isDouble(VM *vm) {
    Value top = peek(vm, -1);
    return top.type == V_DOUBLE;
}

void pushValue(VM *vm, Value v) {
    if (size(&vm->apiStack) <= vm->apiStackTop) {
        add(&vm->apiStack, v);
    } else {
        vm->apiStack[vm->apiStackTop] = v;
    }
    vm->apiStackTop++;
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

void pushNull(VM *vm) {
    Value v{V_CONS_PAIR};
    pushValue(vm, v);
}

void pushUndef(VM *vm) {
    Value v{V_UNDEF};
    pushValue(vm, v);
}

void pushSymbol(VM *vm, const char *symstr) {
    Value v = {V_SYMBOL};
    v.sym = intern(vm, symstr);
    pushValue(vm, v);
}

void pushSymbol(VM *vm, Symbol symbol) {
    Value v = {V_SYMBOL};
    v.sym = symbol;
    pushValue(vm, v);
}

void pushString(VM *vm, const char *str) {
    Value v{V_STRING};
    v.str = strdup(str); // LEAK!!!
    pushValue(vm, v);
}

void pushStringV(VM *vm, const char *format, ...) {
    va_list args;
    va_start(args, format);
    char buffer[512];
    vsprintf(buffer, format, args);
    pushString(vm, buffer);
    va_end(args);
}

void pushBoolean(VM *vm, bool boolean) {
    Value v{V_BOOLEAN};
    v.boolean = boolean;
    pushValue(vm, v);
}

void pushOpaque(VM *vm, void *opaque) {
    Value v{V_OPAQUE_POINTER};
    v.opaque = opaque;
    pushValue(vm, v);
}

void pushHandle(VM *vm, Handle handle) {
    pushValue(vm, get(vm, handle));
}

void pushObject(VM *vm) {
    Value o = {V_OBJECT};
    o.object = allocObject(vm);
    pushValue(vm, o);
}

Symbol intern(VM *vm, const char *str) {
    Value vStr{V_STRING};
    // unsafe, so take care
    vStr.str = (char *)str;
    Symbol symbol;
    if (keyExists(vm, &vm->symbolTable, vStr)) {
        symbol = get(vm, &vm->symbolTable, vStr).sym;
    } else {
        // WARNING!! Leaking!!
        Value s = vStr;
        s.str = strdup(str);
        //symbol.str = s.str;
        symbol.id = vm->symbolIdTop;
        vm->symbolIdTop++;
        Value v = {V_SYMBOL};
        v.sym = symbol;
        set(vm, &vm->symbolTable, s, v);
    }
    return symbol;
}

bool printModified(VM *vm, Value v, FILE *file) {
    if (!v.pair) {
        return false;
    }
    if (v.pair->car == intern(vm, "*print-as-an-integer*")) {
        assert(v.pair->cdr.type == V_DOUBLE);
        l_fprintf(file, "%d", (int)v.pair->cdr.doub);
        return true;
    }
    return false;
}

void asInt(VM *vm) {
    pushSymbol(vm, "*print-as-an-integer*");
    swap(vm);
    cons(vm);
}

void printStackTrace(VM *vm, FILE *file) {
    // We dont print def-line (cadddar trace)
    for (size_t i = 0; !isEmptyList(vm); ++i) {
        l_fprintf(file, "#%lu ", i);
        // stack-trace
        dup(vm);
        // stack-trace, stack-trace
        car(vm);
        // stack-trace, (car stack-trace)
        dup(vm);
        // stack-trace, (car stack-trace), (car stack-trace)
        car(vm);
        // stack-trace, (car stack-trace), (caar stack-trace)
        printValue(vm, pop(vm), file); // proc-name
        // stack-trace, (car stack-trace)
        cdr(vm);
        // stack-trace, (cdar stack-trace)
        dup(vm);
        // stack-trace, (cdar stack-trace), (cdar stack-trace)
        car(vm);
        // stack-trace, (cdar stack-trace), (cadar stack-trace)
        // stack-trace, (cdar stack-trace), (cadar stack-trace)
        l_fprintf(file, " at ");
        printValue(vm, pop(vm), file); // file-name
        // stack-trace, (cdar stack-trace)
        cdr(vm); // (cddar stack-trace)
        // stack-trace, (cddar stack-trace)
        car(vm);
        // stack-trace, (caddar stack-trace)
        asInt(vm);
        // stack-trace, (as-int (caddar stack-trace))
        l_fprintf(file, ":");
        printValue(vm, pop(vm), file); // call-line
        // stack-trace
        l_fprintf(file, "\n");
        cdr(vm);
        // (cdr stack-trace)
    }
}

void printRuntimeError(VM *vm) {
    dup(vm); // (error-message . stack-trace)
    car(vm); // (error-message . stack-trace), error-message
    while (!isEmptyList(vm)) {
        dup(vm); // error-message, error-message
        car(vm); // error-message, (car error-message)
        printValue(vm, pop(vm), stderr); // error-message
        cdr(vm); // (cdr error-message)
    }
    l_fprintf(stderr, "\n");
    pop(vm); // drop null
    cdr(vm); // (stack-trace)
    car(vm); // stack-trace
    printStackTrace(vm); // empty
}

// TODO: symid to symbol string
void printValue(VM *vm, Value v, FILE *file) {
    switch(v.type) {
        case V_CODE_POSITION: {
            l_fprintf(file, "<code pos: %lu>", v.codePosition);
        } break;
        case V_REG_OR_CONSTANT: {
            l_fprintf(file, "<reg or constant: %d global: %s>",
                      v.regOrConstant, v.nonLocal ? "true" : "false");
        } break;
        case V_STRING: {
            l_fprintf(file, "%s", v.str);
        } break;
        case V_UNDEF: {
            l_fprintf(file, "<undef>");
        } break;
        case V_OPAQUE_POINTER: {
            l_fprintf(file, "<opaque: %p>", v.opaque);
        } break;
        case V_FUNCTION: {
            l_fprintf(file, "<func: %p>", v.func);
        } break;
        case V_OBJECT: {
            printObject(vm, v.object, file);
        } break;
        case V_CFUNCTION: {
            l_fprintf(file, "<cfunc: %p>", v.cfunc);
        } break;
        case V_BOOLEAN: {
            l_fprintf(file, "%s", v.boolean ? "true" : "false");
        } break;
        case V_DOUBLE: {
            l_fprintf(file, "%f", v.doub);
        } break;
        case V_SYMBOL: {
            if (v.sym.id >= 0) {
                Value str = getFirstKey(&vm->symbolTable, v);
                l_fprintf(file, "%s", str.str);
            } else {
                l_fprintf(file, "GENSYMD_%d", -v.sym.id);
            }
        } break;
        case V_CONS_PAIR: {
            if (!printModified(vm, v, file)) {
                l_fprintf(file, "(");
                bool isFirst = true;
                while(v.pair) {
                    if (!isFirst) {
                        l_fprintf(file, " ");
                    } else {
                        isFirst = false;
                    }
                    printValue(vm, v.pair->car, file);
                    if (v.pair->cdr.type == V_CONS_PAIR) {
                        v = v.pair->cdr;
                    } else {
                        l_fprintf(file, " . ");
                        printValue(vm, v.pair->cdr, file);
                        break;
                    }
                }
                l_fprintf(file, ")");
            }
        } break;
    }
}

void printObject(VM *vm, Object *obj, FILE *file) {
    l_fprintf(file, "{");
    for (size_t i = 0; i < size(&obj->slots); ++i) {
        if (obj->slots[i].key.type != V_UNDEF &&
            obj->slots[i].value.type != V_UNDEF) {
            if (i != 0) {
                l_fprintf(file, " ");
            }
            l_fprintf(file, "(");
            printValue(vm, obj->slots[i].key, file);
            l_fprintf(file, " ");
            printValue(vm, obj->slots[i].value, file);
            l_fprintf(file, ")");
        }
    }
    l_fprintf(file, "}");
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
    l_fprintf(stdout, "%s ", opCodeStr[op]);
    switch (opCodeTypes[op]) {
        case OT_RS: {
            uint8 reg = getRegA(undecoded);
            int32 immId = getSImm(undecoded);
            l_fprintf(stdout, "r%x i%d\n", (uint32)reg, immId);
        } break;
        case OT_S: {
            int32 immId = getSImm(undecoded);
            l_fprintf(stdout, "i%d\n", immId);
        } break;
        case OT_I: {
            uint32 immId = getImm(undecoded);
            l_fprintf(stdout, "i%x\n", immId);
        } break;
        case OT_N: {
            l_fprintf(stdout, "\n");
        } break;
        case OT_R: {
            uint8 reg = getRegA(undecoded);
            l_fprintf(stdout, "r%x\n", (uint32)reg);
        } break;
        case OT_RR: {
            uint8 a = getRegA(undecoded);
            uint8 b = getRegB(undecoded);
            l_fprintf(stdout, "r%x r%x\n", (uint32)a, (uint32)b);
        } break;
        case OT_RRR: {
            uint8 a = getRegA(undecoded);
            uint8 b = getRegB(undecoded);
            uint8 c = getRegC(undecoded);
            l_fprintf(stdout, "r%x r%x r%x\n", (uint32)a, (uint32)b, (uint32)c);
        } break;
        case OT_RI: {
            uint8 reg = getRegA(undecoded);
            uint32 immId = getImm(undecoded);
            l_fprintf(stdout, "r%x i%x\n", (uint32)reg, immId);
        } break;
    }
}

void printFuncProtoCode(VM *vm, FunctionPrototype *func) {
    l_fprintf(stdout, "VARARG: %s\n", func->vararg ? "true" : "false");
    l_fprintf(stdout, "NUM ARGS: %lu\n", func->numArgs);
    l_fprintf(stdout, "CONSTANT TABLE:\n");
    for (size_t i = 0; i < size(&func->constants); ++i) {
        l_fprintf(stdout, "i%lx ", i);
        printValue(vm, func->constants[i]);
        l_fprintf(stdout, "\n");
    }
    l_fprintf(stdout, "CODE:\n");
    for (size_t i = 0; i < size(&func->code); ++i) {
        if (i && func->lines[i] == func->lines[i-1]) {
            l_fprintf(stdout, "|       ");
        } else {
            l_fprintf(stdout, "%-7lu ", func->lines[i]);
        }
        printOpCode(func->code[i]);
    }
}

void resizeFrame(VM *vm, size_t frameID) {
    resize(&vm->frameStack[frameID-1].registers,
           getProto(vm, &vm->frameStack[frameID-1])->numRegs);
}

size_t allocFrame(VM *vm, Closure *func) {
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
    resizeFrame(vm, vm->frameStackTop);
    return vm->frameStackTop;
}

void popFrame(VM *vm) {
    vm->frameStackTop--;
    resize(&vm->frameStack[vm->frameStackTop].registers, 0);
}

void closeAllUpvalues(VM *vm, ActivationFrame *frame) {
    if (vm->openUpvalueHead && size(&frame->registers)) {
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

// This is awfull
LispisReturnStatus pushArgument(VM *vm, ActivationFrame *frame,
                                uint8 argNum, Handle value) {
    uint8 lastVariableRegister = getProto(vm, frame)->numArgs-1;
    // weird but safe from integer underflow
    if ((argNum < getProto(vm, frame)->numArgs &&
         !getProto(vm, frame)->vararg) ||
        argNum + (size_t)1 < getProto(vm, frame)->numArgs) {
        frame->registers[argNum] = get(vm, value);
    } else {
        if (getProto(vm, frame)->vararg) {
            if (argNum + (size_t)1 ==
                getProto(vm, frame)->numArgs) {
                Value tail = allocConsPair(vm);
                tail.pair->car = get(vm, value);
                tail.pair->cdr.type = V_CONS_PAIR;
                tail.pair->cdr.pair = 0;
                frame->registers[lastVariableRegister] =
                    tail;
            } else {
                Value tail = allocConsPair(vm);
                tail.pair->car = get(vm, value);
                tail.pair->cdr.type = V_CONS_PAIR;
                tail.pair->cdr.pair = 0;
                Value *lst =
                    &frame->registers[lastVariableRegister];
                while (lst->pair->cdr.pair) {
                    lst = &lst->pair->cdr;
                }
                lst->pair->cdr = tail;
            }
        } else {
            return LRS_RUNTIME_ERROR;
        }
    }
    return LRS_OK;
}

bool correctNumberOfArgs(VM *vm, ActivationFrame *frame, uint8 numArgs) {
    if (numArgs == getProto(vm, frame)->numArgs) {
        return true;
    }
    if (!getProto(vm, frame)->vararg) {
        return false;
    }
    if (numArgs + (size_t)1 < getProto(vm, frame)->numArgs) {
        return false;
    }
    return true;
}

LispisReturnStatus lispisSetSlot(VM *vm, size_t numArgs);
LispisReturnStatus runFunc(VM *vm, size_t frameID) {

    OpCode undecoded;
    ActivationFrame *frame = &vm->frameStack[frameID-1];
 loop:
    undecoded = getProto(vm, frame)->code[frame->pc];
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
            Value v = allocConsPair(vm);
            v.pair->car = frame->registers[carReg];
            v.pair->cdr = frame->registers[cdrReg];
            frame->registers[dstReg] = v;
            frame->pc++;
        } break;
        case OP_LOADK: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            frame->registers[reg] = getProto(vm, frame)->constants[k];
            frame->pc++;
        } break;
        case OP_SETUP_CALL: {
            uint8 calleeReg = getRegA(undecoded);
            Value callee = frame->registers[calleeReg];
            if (callee.type == V_FUNCTION) {
                goto call;
            } else if (callee.type == V_CFUNCTION) {
                goto ccall;
            } else {
                pushString(vm, "Cant call ");
                pushValue(vm, callee);
                pushString(vm, " since its not a procedure");
                lispisList(vm, 3);
                goto fail;
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
                frame->pc++;
            } else {
                Value ret = frame->registers[reg];
                popFrame(vm);
                pushValue(vm, ret);
                return LRS_OK;
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
                pushUndef(vm);
                return LRS_OK;
            }
            popFrame(vm);
        } break;
        case OP_CREATE_FUNC: {
            uint8 reg = getRegA(undecoded);
            uint32 localProtoID = getImm(undecoded);
            Value func = {V_FUNCTION};
            size_t protoID =
                getProto(vm, frame)->subFuncProtoIDs[localProtoID];
            func.func = allocFunction(vm, protoID);
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
            l_fprintf(stderr, "ICE: stray PUSH_ARG at %lu\n", frame->pc);
            assert(false);
        } break;
        case OP_CALL: {
            l_fprintf(stderr, "ICE: stray CALL at %lu\n", frame->pc);
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
                l_fprintf(stderr, "ICE: Can only set slot on object\n");
                assert(false);
            }
            Value key = frame->registers[keyReg];
            if (key.type != V_SYMBOL) {
                pushString(vm, "Object key can only be a symbol (so far)"
                           ", ");
                pushValue(vm, key);
                pushString(vm, " is not a symbol");
                lispisList(vm, 3);
                goto fail;
            }
            Value val = frame->registers[valReg];
            pushValue(vm, obj);
            pushValue(vm, key);
            pushValue(vm, val);
            lispisSetSlot(vm, 3);
            pop(vm);
            //set(vm, obj.object, key, val);
            frame->pc++;
        } break;
        case OP_DEFINE_GLOBAL: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            if (getProto(vm, frame)->constants[k].type != V_SYMBOL) {
                l_fprintf(stderr, "ICE: DEFINE_GLOBAL imm not a symbol\n");
                assert(false);
            }

            set(vm, &vm->globals, getProto(vm, frame)->constants[k], 
                frame->registers[reg]); 
            frame->pc++;
        } break;
        case OP_GET_GLOBAL: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            if (getProto(vm, frame)->constants[k].type != V_SYMBOL) {
                l_fprintf(stderr, "ICE: GET_GLOBAL imm not a symbol\n");
                assert(false);
            }
            if (!keyExists(vm, &vm->globals,
                           getProto(vm, frame)->constants[k])) {
                pushString(vm, "Global ");
                pushValue(vm, getProto(vm, frame)->constants[k]);
                pushString(vm, " does not exist");
                lispisList(vm, 3);
                goto fail;
            }

            frame->registers[reg] = get(vm, &vm->globals,
                                        getProto(vm, frame)->constants[k]);
            frame->pc++;
        } break;
        case OP_SET_GLOBAL: {
            uint8 reg = getRegA(undecoded);
            uint32 k = getImm(undecoded);
            if (getProto(vm, frame)->constants[k].type != V_SYMBOL) {
                l_fprintf(stderr, "ICE: GET_GLOBAL imm not a symbol\n");
                assert(false);
            }
            if (!keyExists(vm, &vm->globals,
                           getProto(vm, frame)->constants[k])) {
                pushString(vm, "Global ");
                pushValue(vm, getProto(vm, frame)->constants[k]);
                pushString(vm, " does not exist");
                lispisList(vm, 3);
                goto fail;
            }

            set(vm, &vm->globals, getProto(vm, frame)->constants[k],
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
        case OP_CRASH: {
            uint8 reg = getRegA(undecoded);
            pushValue(vm, frame->registers[reg]);
            goto fail;
        } break;
        case OP_NO_RET_RET: {
            return LRS_OK;
        } break;
        default: {
            l_fprintf(stdout, "OP: ");
            printOpCode(undecoded);
            l_fprintf(stdout, "is not yet implemented\n");
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
        if (getProto(vm, calleeFrame)->numArgs > 0) {
            calleeFrame->registers[getProto(vm, calleeFrame)->numArgs-1].type = V_CONS_PAIR;
            calleeFrame->registers[getProto(vm, calleeFrame)->numArgs-1].pair = 0;
        }
    pushArgLoop:
        undecoded = getProto(vm, frame)->code[frame->pc];
        instr = getOp(undecoded);
        // Underflow is ok bc we only use it in cases where undeflow
        // is not possible
        switch(instr) {
            case OP_PUSH_ARG: {
                uint8 srcReg = getRegA(undecoded);
                Handle value = reserve(vm, frame->registers[srcReg]);
                if (pushArgument(vm, calleeFrame, dstReg, value) !=
                    LRS_OK) {
                    pushString(vm, "Not correct amount of arguments! ");
                    pushSymbol(vm, getProto(vm, calleeFrame)->nameSymbol);
                    pushString(vm, " requires ");
                    pushDouble(vm, getProto(vm, calleeFrame)->numArgs);
                    asInt(vm);
                    pushString(vm, " given more");
                    lispisList(vm, 5);
                    closeAllUpvalues(vm, calleeFrame);
                    popFrame(vm);
                    goto fail;
                }
                free(vm, value);
                dstReg++;
                frame->pc++;
            } break;
            case OP_CALL: {
                uint8 retReg = getRegA(undecoded);
                calleeFrame->retReg = retReg;
                frame = calleeFrame;
                frameID = calleeFrameID;
                if (!correctNumberOfArgs(vm, frame, dstReg)) {
                    pushString(vm, "Not correct amount of arguments! ");
                    pushSymbol(vm, getProto(vm, frame)->nameSymbol);
                    pushString(vm, " requires ");
                    pushDouble(vm, getProto(vm, frame)->numArgs);
                    asInt(vm);
                    pushString(vm, " given ");
                    pushDouble(vm, dstReg);
                    asInt(vm);
                    lispisList(vm, 6);
                    closeAllUpvalues(vm, calleeFrame);
                    popFrame(vm);
                    goto fail;
                }
                goto loop;
            } break;
            default: {
                l_fprintf(stderr,
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
        size_t apiStackBottom = vm->apiStackBottom;
        vm->apiStackBottom = vm->apiStackTop;
        size_t apiStackTop = vm->apiStackTop;
    cpushArgLoop:
        undecoded = getProto(vm, frame)->code[frame->pc];
        instr = getOp(undecoded);
        switch(instr) {
            case OP_PUSH_ARG: {
                uint8 srcReg = getRegA(undecoded);
                pushValue(vm, frame->registers[srcReg]);
                numArgs++;
                frame->pc++;
            } break;
            case OP_CALL: {
                uint8 retReg = getRegA(undecoded);
                LispisReturnStatus lrs = callee.cfunc(vm, numArgs);
                switch(lrs) {
                    case LRS_RETURN_ONE: {
                        frame->registers[retReg] = pop(vm);
                    } break;
                    case LRS_RETURN_NONE: {
                        frame->registers[retReg].type = V_UNDEF;
                    } break;
                    case LRS_WRONG_NUMBER_OF_ARGUMENTS: {
                        pushString(vm, "Not correct amount of arguments!"
                                   " Given ");
                        pushDouble(vm, numArgs);
                        asInt(vm);
                        lispisList(vm, 2);
                        goto fail;
                    } break;
                    case LRS_WRONG_ARGUMENT_TYPE: {
                        Value typeStr = pop(vm);
                        pushString(vm,
                                   "Wrong argument type, expected ");
                        pushValue(vm, typeStr);
                        lispisList(vm, 2);
                        goto fail;
                    } break;
                    case LRS_C_FUNC_ERROR: {
                        lispisList(vm, 1);
                        goto fail;
                    }
                    default:return lrs;
                }
                vm->apiStackBottom = apiStackBottom;
                vm->apiStackTop = apiStackTop;
                frame->pc++;
                //frame = calleeFrame;
                //frameID = calleeFrameID;
                //if (dstReg != frame->func->prototype->numArgs) {
                //fprintf(stderr, "Not correct amount of arguments!\n");
                //assert(false);
                //}
                goto loop;
            } break;
            default: {
                l_fprintf(stderr,
                          "ICE: Illegal %s between SETUP_CALL and CALL\n",
                        opCodeStr[instr]);
                assert(false);
            } break;
        }
        goto cpushArgLoop;
    }
 fail:
    pushStackTrace(vm);
    pushNull(vm);
    cons(vm);
    cons(vm);
    while (!frame->calledFromCpp) {
        frameID -= 1;
        ActivationFrame *caller = &vm->frameStack[frameID-1];
        frame = caller;
        popFrame(vm);
    }
    return LRS_RUNTIME_ERROR;
}

LispisReturnStatus call(VM *vm, uint8 numArgs) {
    Value callee = pop(vm);
    if (callee.type == V_CFUNCTION) {
        size_t apiStackBottom = vm->apiStackBottom;
        vm->apiStackBottom = vm->apiStackTop - numArgs;
        size_t apiStackTop = vm->apiStackTop - numArgs;
        if (callee.cfunc(vm, numArgs)) {
            Value ret = pop(vm);
            vm->apiStackBottom = apiStackBottom;
            vm->apiStackTop = apiStackTop;
            pushValue(vm, ret);
        } else {
            vm->apiStackBottom = apiStackBottom;
            vm->apiStackTop = apiStackTop;
            pushUndef(vm);
        }
        return LRS_OK;
    }
    assert(callee.type == V_FUNCTION);
    size_t frameID = allocFrame(vm, callee.func);
    ActivationFrame *frame = &vm->frameStack[frameID-1];
    // TODO ((lambda args body))
    if (!correctNumberOfArgs(vm, frame, numArgs)) {
        // Probably good that this is an assert, since
        // it's only called from C++
        l_fprintf(stderr, "Not correct amount of argument!\n");
        assert(false);
    }
    if (getProto(vm, frame)->numArgs > 0) {
        frame->registers[getProto(vm, frame)->numArgs-1].type = V_CONS_PAIR;
        frame->registers[getProto(vm, frame)->numArgs-1].pair = 0;
    }
    uint8 reg = 0;
    for (size_t i = vm->apiStackTop-numArgs;
         i < vm->apiStackTop; ++i) {
        Handle value = reserve(vm, vm->apiStack[i]);
        if (pushArgument(vm, frame, reg, value) != LRS_OK) {
            // This to, assert is good!
            assert(false);
        }
        free(vm, value);
        reg++;
    }
    vm->apiStackTop -= numArgs;
    return runFunc(vm, frameID);
}

LispisReturnStatus lispisLT(VM *vm, size_t numArgs) {
    if (numArgs < 1) {
        pushBoolean(vm, false);
        return LRS_RETURN_ONE;
    } else {
        ASSERT_ARG_TYPE(vm, -1, V_DOUBLE, "double");
        double prev = popDouble(vm);
        numArgs--;
        while (numArgs) {
            ASSERT_ARG_TYPE(vm, -1, V_DOUBLE, "double");
            double curr = popDouble(vm);
            if (prev < curr) {
                pushBoolean(vm, false);
                return LRS_RETURN_ONE;
            }
            prev = curr;
            numArgs--;
        }
        pushBoolean(vm, true);
        return LRS_RETURN_ONE;
    }
}

LispisReturnStatus lispisGT(VM *vm, size_t numArgs) {
    if (numArgs < 1) {
        pushBoolean(vm, false);
        return LRS_RETURN_ONE;
    } else {
        ASSERT_ARG_TYPE(vm, -1, V_DOUBLE, "double");
        double prev = popDouble(vm);
        numArgs--;
        while (numArgs) {
            ASSERT_ARG_TYPE(vm, -1, V_DOUBLE, "double");
            double curr = popDouble(vm);
            if (prev > curr) {
                pushBoolean(vm, false);
                return LRS_RETURN_ONE;
            }
            prev = curr;
            numArgs--;
        }
        pushBoolean(vm, true);
        return LRS_RETURN_ONE;
    }
}

LispisReturnStatus lispisEQ(VM *vm, size_t numArgs) {
    if (numArgs < 1) {
        pushBoolean(vm, true);
        return LRS_RETURN_ONE;
    } else {
        bool ret = true;
        Value last = pop(vm);
        numArgs--;
        while (numArgs) {
            ret = ret && last == pop(vm);
            numArgs--;
        }
        pushBoolean(vm, ret);
        return LRS_RETURN_ONE;
    }
}

LispisReturnStatus lispisList(VM *vm, size_t numArgs) {
    Value nil = {V_CONS_PAIR};
    nil.pair = 0;
    if (numArgs > 0) {
        Handle prev = reserve(vm, allocConsPair(vm));
        get(vm, prev).pair->car = pop(vm);
        get(vm, prev).pair->cdr = nil;
        numArgs--;
        while (numArgs) {
            Handle newHead = reserve(vm, allocConsPair(vm));
            get(vm, newHead).pair->car = pop(vm);
            get(vm, newHead).pair->cdr.type = V_CONS_PAIR;
            get(vm, newHead).pair->cdr = get(vm, prev);
            free(vm, prev);
            prev = newHead;
            numArgs--;
        }
        Value ret = get(vm, prev);
        free(vm, prev);
        pushValue(vm, ret);
    } else {
        pushValue(vm, nil);
    }
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisCons(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 2);
    cons(vm);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisCar(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    ASSERT_ARG_TYPE(vm, -1, V_CONS_PAIR, "cons-pair");
    Value p = pop(vm);
    ASSERT_EXPR(vm, p.pair, "car argument '()");
    pushValue(vm, p.pair->car);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisCdr(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    ASSERT_ARG_TYPE(vm, -1, V_CONS_PAIR, "cons-pair");
    Value p = pop(vm);
    ASSERT_EXPR(vm, p.pair, "cdr argument '()");
    pushValue(vm, p.pair->cdr);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisNullP(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    Value arg = pop(vm);
    pushBoolean(vm, arg.type == V_CONS_PAIR && !arg.pair);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisListP(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    Value arg = pop(vm);
    pushBoolean(vm, arg.type == V_CONS_PAIR);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisNot(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    ASSERT_ARG_TYPE(vm, -1, V_BOOLEAN, "boolean");
    pushBoolean(vm, !popBoolean(vm));
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisAdd(VM *vm, size_t numArgs) {
    double ret = 0;
    while (numArgs) {
        ASSERT_ARG_TYPE(vm, -1, V_DOUBLE, "double");
        ret += popDouble(vm);
        numArgs--;
    }
    pushDouble(vm, ret);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisSub(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs > 0);
    ASSERT_ARG_TYPE(vm, -1, V_DOUBLE, "double");
    double ret = peekDouble(vm, -numArgs);
    numArgs--;
    if (numArgs) {
        while (numArgs) {
            ASSERT_ARG_TYPE(vm, -1, V_DOUBLE, "double");
            ret -= peekDouble(vm, -numArgs);
            numArgs--;
        }
    } else {
        ret = -ret;
    }
    pushDouble(vm, ret);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisGetSlot(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 2);
    switch (peek(vm, -2).type) {
        case V_OBJECT: {
            Value key = peek(vm, -1);
            Value obj = peek(vm, -2);
            if (key.sym == vm->parentSym) {
                ASSERT_EXPR(vm, (obj.object->parent.type == V_OBJECT &&
                                 obj.object->parent.object),
                            "object does not have a parent");
            } else {
                ASSERT_EXPR(vm, keyExists(vm, obj.object, key),
                            "key does not exist");
            }
            getObjectSlot(vm);
        } break;
        case V_CONS_PAIR: {
            Value key = peek(vm, -1);
            Value obj = peek(vm, -2);
            ASSERT_EXPR(vm, key.type == V_DOUBLE,
                        "get-slot key of cons-pair not double (bad msg)");
            pushValue(vm, at(obj, key.doub));
        } break;
        default: {
            ASSERT_EXPR(vm, false,
                        "expected obj of type object or cons-pair");
        }
    }
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisSetSlot(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 3);
    ASSERT_ARG_TYPE(vm, -2, V_SYMBOL, "symbol");
    switch (peek(vm, -3).type) {
        case V_OBJECT: {
            Value value = peek(vm, -1);
            Value key = peek(vm, -2);
            if (key.sym == vm->parentSym) {
                ASSERT_EXPR(vm, (value.type == V_OBJECT && value.object),
                            "can only set objects as parent");
            }
            setObjectSlot(vm);
            pop(vm);
            pushValue(vm, value);
        } break;
        default: {
            ASSERT_EXPR(vm, false,
                        "expected obj of type object");
        }
    }
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisRemoveSlot(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 2);
    ASSERT_ARG_TYPE(vm, -1, V_SYMBOL, "symbol");
    Value key = pop(vm);
    ASSERT_ARG_TYPE(vm, -1, V_OBJECT, "object");
    Value obj = pop(vm);
    if (key.sym == vm->parentSym) {
        ASSERT_EXPR(vm, (obj.object->parent.type == V_OBJECT &&
                         obj.object->parent.object),
                    "object does not have a parent");
        pushValue(vm, obj.object->parent);
        obj.object->parent.type = V_UNDEF;
    } else {
        ASSERT_EXPR(vm, keyExists(vm, obj.object, key),
                    "key does not exist");
        pushValue(vm, get(vm, obj.object, key));
        remove(vm, obj.object, key);
    }
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisKeyExists(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 2);
    ASSERT_ARG_TYPE(vm, -1, V_SYMBOL, "symbol");
    Value key = pop(vm);
    ASSERT_ARG_TYPE(vm, -1, V_OBJECT, "object");
    Value obj = pop(vm);
    if (key.sym == vm->parentSym) {
        pushBoolean(vm, obj.object->parent.type == V_OBJECT);
    } else {
        pushBoolean(vm, keyExists(vm, obj.object, key));
    }
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisPrint(VM *vm, size_t numArgs) {
    while (numArgs) {
        printValue(vm, peek(vm, -numArgs));
        numArgs--;
    }
    return LRS_RETURN_NONE;
}

LispisReturnStatus lispisPrintln(VM *vm, size_t numArgs) {
    lispisPrint(vm, numArgs);
    l_fprintf(stdout, "\n");
    return LRS_RETURN_NONE;
}

LispisReturnStatus lispisNewline(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 0);
    pushString(vm, "\n");
    return LRS_RETURN_ONE;
}

Symbol gensym(VM *vm) {
    Symbol ret = {vm->gensymIdTop};
    vm->gensymIdTop--;
    return ret;
}

LispisReturnStatus lispisGensym(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 0);
    pushSymbol(vm, gensym(vm));
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisSetCdr(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 2);
    Handle newCdr = reserve(vm, pop(vm));
    ASSERT_ARG_TYPE(vm, -1, V_CONS_PAIR, "cons-pair");
    Handle pair = reserve(vm, pop(vm));
    ASSERT_EXPR(vm, get(vm, pair).pair, "Cant set cdr on '()");
    get(vm, pair).pair->cdr = get(vm, newCdr);
    free(vm, newCdr);
    free(vm, pair);
    return LRS_RETURN_NONE;
}

LispisReturnStatus lispisStackInfo(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    ASSERT_ARG_TYPE(vm, -1, V_DOUBLE, "double");
    size_t frameNumber = popDouble(vm);
    ASSERT_EXPR(vm, vm->frameStackTop > frameNumber,
                "argument not in range of valid stack-frames");
    ActivationFrame *frame = &vm->frameStack[vm->frameStackTop -
                                             frameNumber - 1];
    size_t defLine =
        vm->funcProtos[frame->func->protoID].definedOnLine;
    size_t callLine =
        vm->funcProtos[frame->func->protoID].lines[frame->pc];
    Symbol name =
        vm->funcProtos[frame->func->protoID].nameSymbol;
    char *file = 
        vm->funcProtos[frame->func->protoID].file;
    pushSymbol(vm, name);
    pushString(vm, file);
    pushDouble(vm, callLine);
    pushDouble(vm, defLine);
    pushNull(vm);
    cons(vm);
    cons(vm);
    cons(vm);
    cons(vm);
    return LRS_RETURN_ONE;
}

// Garbage if called from C
LispisReturnStatus lispisStackDepth(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 0);
    pushDouble(vm, vm->frameStackTop);
    return LRS_RETURN_ONE;
}

// How to deal with C functions in the stack?
void pushStackTrace(VM *vm) {
    for (int i = ((int)vm->frameStackTop - 1); i >= 0; --i) {
        if (i == ((int)vm->frameStackTop - 1)) {
            pushDouble(vm, i);
            lispisStackInfo(vm, 1);
            pushNull(vm);
        } else {
            Value tmp = pop(vm);
            pushDouble(vm, i);
            lispisStackInfo(vm, 1);
            pushValue(vm, tmp);
        }
        cons(vm);
    }
}

LispisReturnStatus lispisStackTrace(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 0);
    pushStackTrace(vm);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisAsInt(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    asInt(vm);
    return LRS_RETURN_ONE;
}

LispisReturnStatus lispisPrintStackTrace(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    printStackTrace(vm);
    return LRS_RETURN_NONE;
}

LispisReturnStatus lispisDoFile(VM *vm, size_t numArgs) {
    ASSERT_NUM_ARGS(numArgs == 1);
    ASSERT_ARG_TYPE(vm, -1, V_STRING, "string");
    char *file = popString(vm);
    LispisReturnStatus lrs = doFile(vm, file, false);
    if (lrs != LRS_OK) {
        return lrs;
    }
    return LRS_RETURN_ONE;
}

VM initVM(bool loadDefaults) {
    VM vm;
    vm.funcProtos = DynamicArray<FunctionPrototype>();
    vm.parentSym = intern(&vm, "*parent*");
    if (loadDefaults) {
        //pushCFunction(&vm, lispisAnd);
        //setGlobal(&vm, intern(&vm, "and"));
        //pushCFunction(&vm, lispisOr);
        //setGlobal(&vm, intern(&vm, "or"));
        pushCFunction(&vm, lispisLT);
        setGlobal(&vm, intern(&vm, "<"));
        pushCFunction(&vm, lispisGT);
        setGlobal(&vm, intern(&vm, ">"));
        pushCFunction(&vm, lispisEQ);
        setGlobal(&vm, intern(&vm, "="));
        pushCFunction(&vm, lispisList);
        setGlobal(&vm, intern(&vm, "list"));
        pushCFunction(&vm, lispisCons);
        setGlobal(&vm, intern(&vm, "cons"));
        pushCFunction(&vm, lispisCar);
        setGlobal(&vm, intern(&vm, "car"));
        pushCFunction(&vm, lispisCdr);
        setGlobal(&vm, intern(&vm, "cdr"));
        pushCFunction(&vm, lispisSetCdr);
        setGlobal(&vm, intern(&vm, "set-cdr!"));
        pushCFunction(&vm, lispisNullP);
        setGlobal(&vm, intern(&vm, "null?"));
        pushCFunction(&vm, lispisListP);
        setGlobal(&vm, intern(&vm, "list?"));
        pushCFunction(&vm, lispisNot);
        setGlobal(&vm, intern(&vm, "not"));
        pushCFunction(&vm, lispisAdd);
        setGlobal(&vm, intern(&vm, "+"));
        pushCFunction(&vm, lispisSub);
        setGlobal(&vm, intern(&vm, "-"));
        pushCFunction(&vm, lispisGetSlot);
        setGlobal(&vm, intern(&vm, "get-slot"));
        pushCFunction(&vm, lispisSetSlot);
        setGlobal(&vm, intern(&vm, "set-slot!"));
        pushCFunction(&vm, lispisRemoveSlot);
        setGlobal(&vm, intern(&vm, "remove-slot!"));
        pushCFunction(&vm, lispisKeyExists);
        setGlobal(&vm, intern(&vm, "key-exists?"));
        pushCFunction(&vm, lispisPrint);
        setGlobal(&vm, intern(&vm, "print!"));
        pushCFunction(&vm, lispisPrintln);
        setGlobal(&vm, intern(&vm, "println!"));
        pushCFunction(&vm, lispisNewline);
        setGlobal(&vm, intern(&vm, "newline"));
        pushCFunction(&vm, lispisGensym);
        setGlobal(&vm, intern(&vm, "gensym"));
        pushCFunction(&vm, lispisStackInfo);
        setGlobal(&vm, intern(&vm, "stack-info"));
        pushCFunction(&vm, lispisStackDepth);
        setGlobal(&vm, intern(&vm, "stack-depth"));
        pushCFunction(&vm, lispisStackTrace);
        setGlobal(&vm, intern(&vm, "stack-trace"));
        pushCFunction(&vm, lispisAsInt);
        setGlobal(&vm, intern(&vm, "as-int"));
        pushCFunction(&vm, lispisPrintStackTrace);
        setGlobal(&vm, intern(&vm, "print-stack-trace"));
        pushCFunction(&vm, lispisDoFile);
        setGlobal(&vm, intern(&vm, "do-file"));
        #include "stdlib.h"
    }
    return vm;
}

Value &getFirstKey(Object *o, Value value) {
    for (size_t i = 0; i < size(&o->slots); ++i) {
        if (o->slots[i].value == value) {
            return o->slots[i].key;
        }
    }
    if (o->parent.type == V_OBJECT && o->parent.object) {
        return getFirstKey(o->parent.object, value);
    } else {
        assert(false);
    }
}

char *getSymbolString(VM *vm, Symbol symbol) {
    assert(symbol.id >= 0);
    Value v = {V_SYMBOL};
    v.sym = symbol;
    return getFirstKey(&vm->symbolTable, v).str;
}

void setLineInfo(VM *vm, Handle handle, size_t line, size_t column) {
    assert(type(vm, handle) == V_CONS_PAIR);
    LineInfo *info = (LineInfo *)alloc(vm, sizeof(LineInfo));
    info->gcObj.type = GC_LINE_INFO;
    info->line = line;
    info->column = column;
    get(vm, handle).pair->lineInfo = info;
}

void setLineInfo(VM *vm, Value value, size_t line, size_t column) {
    Handle tmp = reserve(vm, value);
    setLineInfo(vm, tmp, line, column);
    free(vm, tmp);
}

LineInfo getLineInfo(Value value) {
    assert(value.type == V_CONS_PAIR);
    assert(value.pair);
    return *value.pair->lineInfo;
}

bool hasLineInfo(Value value) {
    return value.type == V_CONS_PAIR && value.pair &&
        value.pair->lineInfo;
}
