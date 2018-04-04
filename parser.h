#pragma once
#include "vm.h"
#include <cstddef>

enum TokenType {
    T_DOUBLE,
    T_SYMBOL,
    T_LEFT_BRACKET,
    T_RIGHT_BRACKET,
    T_LEFT_CURLY,
    T_RIGHT_CURLY,
    T_LEFT_PAREN,
    T_RIGHT_PAREN,
    T_QUOTE,
    T_QUASI_QUOTE,
    T_UNQUOTE,
    T_UNQUOTE_SPLICE,
    T_BOOLEAN,
    T_STRING,
    T_EOF,
    T_ERROR
};

struct Token {
    size_t start;
    size_t length;
    TokenType type;
    union {
        double d;
        bool b;
        char *str;
        struct {
            char *symstr;
            int symid;
        };
    };
};

struct LexState {
    char *prog;
    size_t pos;
    Token nextToken;
};

void eatWhiteSpace(LexState *state);
bool isFloatStartChar(int c);
Token peekToken(LexState *state);
Token nextToken(VM *vm, LexState *state);
LexState initLexerState(VM *vm, char *prog);

void parseList(VM *vm, LexState *lex, Handle parent);
void parseExpr(VM *vm, LexState *lex, Handle parent);