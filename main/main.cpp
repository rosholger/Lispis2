#include "vm.h"
#include "parser.h"
#include "compiler.h"
#include <cstdio>
#include "common.h"

int main(int argc, char *argv[]) {
    assert(sizeof(Value) == sizeof(int *) * 2);
    VM vm = initVM();
    switch (doFile(&vm, "./slot-access-sugar.lsp", 0, false)) {
        case LRS_RUNTIME_ERROR: {
            l_fprintf(stderr, "\nRUNTIME ERROR: in "
                      "slot-access-sugar.lsp:\n");
            printRuntimeError(&vm);
            exit(1);
        } break;
        case LRS_MACRO_ERROR:
        case LRS_COMPILETIME_ERROR: {
            l_fprintf(stderr, "\nCOMPILETIME ERROR: in "
                      "slot-access-sugar.lsp:\n");
            printValue(&vm, pop(&vm), stderr);
            exit(1);
        } break;
        default:break;
    }
    switch (doFile(&vm, "./basic.lsp", 0, false)) {
        case LRS_RUNTIME_ERROR: {
            l_fprintf(stderr, "\nRUNTIME ERROR: in basic.lsp:\n");
            printRuntimeError(&vm);
            exit(1);
        } break;
        case LRS_MACRO_ERROR:
        case LRS_COMPILETIME_ERROR: {
            l_fprintf(stderr, "\nCOMPILETIME ERROR: in basic.lsp:\n");
            printValue(&vm, pop(&vm), stderr);
            exit(1);
        } break;
        default:break;
    }
    l_fprintf(stdout, "\n");
    printValue(&vm, peek(&vm, -1));
    l_fprintf(stdout, "\n");
    collect(&vm);
    //pushValue(&vm, Value(V_UNDEF));
    //setGlobal(&vm, intern(&vm, "testFunc2"));
    //getGlobal(&vm, intern(&vm, "test2"));
    //printValue(pop(&vm));
    //printf("\n");
    //for (int i = 0; i < 10; ++i) {
    //getGlobal(&vm, intern(&vm, "testFunc"));
    //call(&vm, 0);
        ////getGlobal(&vm, 1);
        //printValue(peek(&vm, -1));
        //printf("\n");
        //clearStack(&vm);
        //collect(&vm);
    //}
    freeVM(&vm);
    
}