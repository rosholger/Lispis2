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
    T_QUASIQUOTE,
    T_UNQUOTE,
    T_UNQUOTE_SPLICING,
    T_BOOLEAN,
    T_STRING,
    T_EOF,
    T_DOT,
    T_ERROR
};

struct Token {
    size_t start;
    size_t length;
    size_t line;
    size_t column;
    TokenType type;
    union {
        double d;
        bool b;
        char *str;
        int symid;
    };
};

struct LexState {
    const char *prog;
    size_t pos = 0;
    size_t line = 1;
    size_t column = 0;
    Token nextToken;
};

void eatWhiteSpace(LexState *state);
bool isFloatStartChar(int c);
Token peekToken(LexState *state);
Token nextToken(VM *vm, LexState *state);
LispisReturnStatus initLexerState(VM *vm, const char *prog, LexState *ret);

LispisReturnStatus parseExpr(VM *vm, LexState *lex, Handle parent);
