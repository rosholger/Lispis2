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
    char *ret = (char *)malloc(strlen(a) + strlen(b)+2);
    strcpy(ret, a);
    ret[strlen(a)] = ' ';
    ret[strlen(a)+1] = 0;
    strcat(ret, b);
    return ret;
}

char *readExpr(FILE *file) {
    char *ret = (char *)malloc(1);
    ret[0] = 0;
    int numParens = 0;
    for(char *line = readline("> "); line;
        line = readline("> ")) {
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
    }
    free(ret);
    return 0;
}

int main(int argc, char *argv[]) {
    mkfifo("/tmp/lispis-repl-out", 0666);
    FILE *out = fopen("/tmp/lispis-repl", "w");

    fprintf(out, "/tmp/lispis-repl-out\n");
    fflush(out);
    printf("Connected to lispis\n Type ;quit to quit\n");
    FILE *inp = fopen("/tmp/lispis-repl-out", "r");
    // possibly dont do this and instead send ;end back done executing
    // or maybe something like ("output") and parse the output.
    // This is prob. the best idea, but not until we have
    // error handling.
    fcntl(fileno(inp), F_SETFL, O_NONBLOCK);
    char buffer[256];
    for (char *expr = readExpr(stdin); expr; expr = readExpr(stdin)) {
        fprintf(out, "%s\n", expr);
        fflush(out);
        usleep(400);
        for (char *ret = fgets(buffer, 256, inp); ret;
             ret = fgets(buffer, 256, inp)) {
            if (!strcmp(ret, ";bye!\n")) {
                fclose(inp);
                fclose(out);
                unlink("/tmp/lispis-repl-out");
                return 0;
            }
            printf("%s", ret);
            usleep(400);
        }
        free(expr);
    }
    fclose(inp);
    fclose(out);
    unlink("/tmp/lispis-repl-out");
}