#include "vm.h"
#include "parser.h"
#include "compiler.h"
#include <cstring>
#include <cstdio>
#include <cctype>
#include <cstdlib>
#include <cassert>
#include <string>
#include <climits>

int main(int argc, char *argv[]) {
    VM vm = initVM();
    doFile(&vm, "./basic.lsp", 0, false);
    printf("\n");
    printValue(&vm, peek(&vm, -1));
    printf("\n");
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