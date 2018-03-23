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

Object::Object() : gcObj(GCObject{GC_OBJECT}),
                   table(std::map<int, Value>()) {}

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
            for (auto it = obj->table.begin();
                 it != obj->table.end();
                 ++it) {
                visitValue(vm, &it->second);
            }
            vm->gc.nextToExamine += sizeof(Object);
        } break;
        case GC_CONS_PAIR: {
            ConsPair *c = (ConsPair *)o;
            visitValue(vm, &c->car);
            visitValue(vm, &c->cdr);
            vm->gc.nextToExamine += sizeof(ConsPair);
        } break;
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
    Object temp;
    memcpy(ret, &temp, sizeof(Object));
    *ret = temp;
    return ret;
}