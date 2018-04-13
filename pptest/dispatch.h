switch(instr) {
#define XMA(x) case x##_test: goto labe_##x;
#include "testInc.h"
#undef XMA
 }