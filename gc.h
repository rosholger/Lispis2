#ifndef GC_H
#define GC_H
#include <stddef.h>

#define kilobytes(b) b*1024
#define megabytes(kb) kilobytes(kb)*1024

#define HEAP_START_SIZE kilobytes(1)
#define HEAP_START_MAX_SIZE megabytes(1)

struct GC {
    char *fromSpace = 0;
    char *toSpace = 0;
    size_t nextFree = 0;
    size_t nextToExamine = 0;
    size_t toSpaceSize = 0;
    size_t fromSpaceSize = 0;
    size_t heapSize = HEAP_START_SIZE;
    // fromSpaceSize is the same as toSpaceSize until we start growing
    // the heap
};

enum GCObjectType {
    GC_CONS_PAIR,
    GC_OBJECT,
    GC_BROKEN_HEART,
};

struct GCObject {
    union {
        GCObjectType type;
        void *paddingAlignment;
    };
};
#endif