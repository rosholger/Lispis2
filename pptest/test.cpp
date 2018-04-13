#define DISPATCH_START()                        \
    switch(instr) {

#define DISPATCH_END()                          \
    }

#define XMA(n) case OP_##n: goto label_##n;

DISPATCH_START()
#include "testInc.h"
DISPATCH_END()
#undef XMA
#include "dispatch.h"
switch(instr) {
case OP_a: goto label_a;
case OP_b: goto label_b;
case OP_c: goto label_c;
}

#undef DISPATCH_START
#undef DISPATCH_END
#undef XMA

#define DISPATCH_START() goto labels[instr];
#define XMA(n)
#define DISPATCH_END()

DISPATCH_START()
#include "testInc.h"
DISPATCH_END()
goto labels[instr];