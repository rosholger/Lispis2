#include "vm.h"
#include "parser.h"
#include "compiler.h"
#include <cstdio>
#include <cstdlib>
#include <readline/readline.h>
#include <readline/history.h>
#include <stropts.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "common.h"

char *concatLines(const char *a, const char *b) {
    char *ret = 0;
    if (*a != 0) {
        ret = (char *)malloc(strlen(a) + strlen(b)+2);
        strcpy(ret, a);
        ret[strlen(a)] = '\n';
        ret[strlen(a)+1] = 0;
    } else {
        ret = (char *)malloc(strlen(b)+1);
        ret[0] = 0;
    }
    strcat(ret, b);
    return ret;
}

char *readExpr() {
    char *ret = (char *)malloc(1);
    ret[0] = 0;
    int numParens = 0;
    char buffer[128];
    for(char *line = fgets(buffer, 128, stdin); line;
        line = fgets(buffer, 128, stdin)) {
        for (char *c = line; *c; ++c) {
            if (*c == ';') {
                break;
            }
            if (*c == '"') {
                ++c;
                while (*c && *c != '"') {
                    ++c;
                }
                ++c;
            }
            if (*c == '(' || *c == '{') {
                numParens++;
            }
            if (*c == ')' || *c == '}') {
                numParens--;
            }
        }
        char *newRet = concatLines(ret, line);
        free(ret);
        ret = newRet;
        if (numParens <= 0) {
            return ret;
        }
        usleep(10);
    }
    free(ret);
    return 0;
}

char *wrapInScope(char *str) {
    const char *prefix = "(scope";
    const char *postfix = ")";
    char *ret = (char *)malloc(strlen(prefix) +
                               strlen(str) +
                               strlen(postfix) +
                               1);
    ret[0] = 0;
    ret = strcat(ret, prefix);
    ret = strcat(ret, str);
    ret = strcat(ret, postfix);
    return ret;
}

void redirectStdout() {
    char buffer[128];
    assert(fgets(buffer, 128, stdin));
    buffer[strlen(buffer)-1] = 0;
    l_fprintf(stdout, "%s;\n", buffer);
    freopen(buffer, "w", stdout);
    freopen(buffer, "w", stderr);
    fflush(stdout);
}

void repl() {
    rl_bind_key('\t', rl_insert);
    VM vm = initVM();
    using_history();
    mkfifo("/tmp/lispis-repl", 0666);
    freopen("/tmp/lispis-repl", "r+", stdin);
    redirectStdout();
    fcntl(fileno(stdin), F_SETFL, O_NONBLOCK);
    while (true) {
        int c;
        // Change to ioctl? didnt work on stdin but maybe on named pipes?
        while ((c = fgetc(stdin)) == EOF) {
            //l_fprintf(stdout, "Nothing left\n");
            usleep(100);
        }
        ungetc(c, stdin);
        for (char *expr = readExpr(); expr; expr = readExpr()) {
            if (!strcmp(expr, " ;quit\n")) {
                l_fprintf(stdout, ";bye!\n");
                freopen("/dev/tty", "r", stdin);
                freopen("/dev/tty", "w", stdout);
                freopen("/dev/tty", "w", stderr);
                unlink("/tmp/lispis-repl");
                return;
            }
#if 0
            else {
                l_fprintf(stdout, "%s\n", expr);
            }
#endif
            if (strlen(expr) != 2 || expr[0] != ' ' || expr[1] != '\n') {
                char *wrapped = wrapInScope(expr);
                free(expr);
                switch (doString(&vm, wrapped, false, "repl")) {
                    case LRS_OK: {
                        printValue(&vm, pop(&vm));
                        l_fprintf(stdout, "\n");
                    } break;
                    case LRS_RUNTIME_ERROR: {
                        l_fprintf(stderr, "\nRUNTIME ERROR:\n");
                        printRuntimeError(&vm);
                    } break;
                    case LRS_MACRO_ERROR:
                    case LRS_COMPILETIME_ERROR: {
                        l_fprintf(stderr, "\nCOMPILETIME ERROR:\n");
                        printValue(&vm, pop(&vm), stderr);
                    } break;
                    default:assert(false);
                }
                free(wrapped);
            }
        }
    }
    freeVM(&vm);
}


int main(int argc, char *argv[]) {
    repl();
}