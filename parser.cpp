#include "parser.h"
#include "vm.h"
#include <cctype>
#include <cstring>
#include <cstdlib>
#include <cstdio>

void eatWhiteSpace(LexState *state) {
    while ((isspace(state->prog[state->pos]) ||
            state->prog[state->pos] == ';') &&
           state->prog[state->pos]) {
        if (state->prog[state->pos] == ';') {
            state->pos += strcspn(state->prog+state->pos, "\n");
        } else {
            state->pos++;
        }
    }
}

bool isFloatStartChar(int c) {
    return isdigit(c) || c == '.';
}

Token peekToken(LexState *state) {
    return state->nextToken;
}

Token nextToken(VM *vm, LexState *state) {
    eatWhiteSpace(state);
    size_t tokStart = state->pos;
    const char *tokenDelimiters = ")( \n'`,";
    size_t tokLength = strcspn(state->prog+state->pos,
                               tokenDelimiters);
    if (strchr(tokenDelimiters, state->prog[tokStart])) {
        tokLength += 1;
    }
    state->pos += tokLength;

    Token tok{tokStart, tokLength, T_ERROR};

    const char *t = "true";
    const char *f = "false";

    if (state->prog[tokStart] == 0) {
        tok.type = T_EOF;
    } else if (isFloatStartChar(state->prog[tokStart])) {
        // positive floats
        char *end = 0;
        double res = strtod(state->prog+tokStart, &end);
        if (end == state->prog+state->pos) {
            tok.type = T_DOUBLE;
            tok.d = res;
        }
    } else if (state->prog[tokStart] == '-' &&
               tokLength > 1 &&
               isFloatStartChar(state->prog[tokStart+1])) {
        // negative floats
        char *end = 0;
        double res = strtod(state->prog+tokStart, &end);
        if (end == state->prog+state->pos) {
            tok.type = T_DOUBLE;
            tok.d = res;
        }
    } else if (state->prog[tokStart] == ',') {
        if (state->prog[tokStart+1] == '@') {
            tokLength++;
            state->pos++;
            tok.length++;
            tok.type = T_UNQUOTE_SPLICE;
        } else {
            tok.type = T_UNQUOTE;
        }
    } else if (state->prog[tokStart] == '`') {
        tok.type = T_QUASI_QUOTE;
    } else if (state->prog[tokStart] == '\'') {
        tok.type = T_QUOTE;
    } else if (state->prog[tokStart] == '(') {
        tok.type = T_LEFT_PAREN;
    } else if (state->prog[tokStart] == ')') {
        tok.type = T_RIGHT_PAREN;
    } else if (state->prog[tokStart] == '[') {
        tok.type = T_LEFT_BRACKET;
    } else if (state->prog[tokStart] == ']') {
        tok.type = T_RIGHT_BRACKET;
    } else if (state->prog[tokStart] == '{') {
        tok.type = T_LEFT_CURLY;
    } else if (state->prog[tokStart] == '}') {
        tok.type = T_RIGHT_CURLY;
    } else if (tokLength == strlen(t) &&
               !memcmp(state->prog+tokStart, t, tokLength)) {
        // True
        tok.type = T_BOOLEAN;
        tok.b = true;
    } else if (tokLength == strlen(f) &&
               !memcmp(state->prog+tokStart, f, tokLength)) {
        // False
        tok.type = T_BOOLEAN;
        tok.b = false;
    } else {
        // symbols
        tok.type = T_SYMBOL;
        char temp_str[100];
        assert(tokLength < 99);
        memcpy(temp_str, state->prog+tokStart, tokLength);
        temp_str[tokLength] = 0;
        Symbol symbol = intern(vm, temp_str);
        tok.symstr = symbol.str;
        tok.symid = symbol.id;
    }
    Token ret = state->nextToken;
    state->nextToken = tok;
    return ret;
}

LexState initLexerState(VM *vm, char *prog) {
    LexState ret{};
    ret.prog = prog;
    nextToken(vm, &ret);
    if (peekToken(&ret).type == T_EOF) {
        fprintf(stderr, "ERROR?: Empty 'files' are not legal\n");
        assert(false);
    }
    return ret;
}

#define allocPair(vm, parent)                                           \
    do {                                                                \
        getC(vm, parent).cdr.type = V_CONS_PAIR;                        \
        getC(vm, parent).cdr.pair = 0;                                  \
        getC(vm, parent).car.type = V_CONS_PAIR;                        \
        getC(vm, parent).car.pair = 0;                                  \
    } while(false)


void parseExpr(VM *vm, LexState *lex, Handle parent) {
    //assert(getC(vm, parent).type == V_CONS_PAIR);
    //assert(getC(vm, parent).pair == 0);
    Token tok = nextToken(vm, lex);
    switch (tok.type) {
        case T_ERROR: {
            fprintf(stderr, "ERROR: '%.*s'\n", (int)tok.length, tok.str);
            assert(false);
        } break;
        case T_DOUBLE: {
            allocPair(vm, parent);
            getC(vm, parent).car.type = V_DOUBLE;
            getC(vm, parent).car.doub = tok.d;
        } break;
        case T_BOOLEAN: {
            allocPair(vm, parent);
            getC(vm, parent).car.type = V_BOOLEAN;
            getC(vm, parent).car.boolean = tok.b;
        } break;
        case T_SYMBOL: {
            allocPair(vm, parent);
            getC(vm, parent).car.type = V_SYMBOL;
            getC(vm, parent).car.sym.str = tok.symstr;
            getC(vm, parent).car.sym.id = tok.symid;
        } break;
        case T_LEFT_PAREN: {
            allocPair(vm, parent);
            if (peekToken(lex).type != T_RIGHT_PAREN) {
                getC(vm, parent).car.type = V_CONS_PAIR;
                ConsPair *p = allocConsPair(vm);
                getC(vm, parent).car.pair = p;
                Handle car = reserve(vm, getC(vm, parent).car.pair);
                parseList(vm, lex, car);
                free(vm, car);
            }
            nextToken(vm, lex); // eat right parent
        } break;
        case T_RIGHT_PAREN: {
            fprintf(stderr, "ERROR: Mismatching parenthesis, more ) than (\n");
            assert(false);
        } break;
        case T_EOF: {
            fprintf(stderr, "ERROR: Mismatching parenthesis, more ( than )\n");
            assert(false);
        } break;
        default: {
            fprintf(stderr, "ERROR: %d not recognized yet\n", tok.type);
            assert(false);
        } break;
    }
}

void parseList(VM *vm, LexState *lex, Handle parent) {

    Token tok = peekToken(lex);
    Handle currParent = reserve(vm, &getC(vm, parent));
    while (tok.type != T_RIGHT_PAREN) {
        parseExpr(vm, lex, currParent);
        tok = peekToken(lex);
        if (tok.type != T_RIGHT_PAREN) {
            ConsPair *p = allocConsPair(vm);
            getC(vm, currParent).cdr.pair = p;
            Handle newCurrParent = reserve(vm,
                                           getC(vm,
                                                currParent).cdr.pair);
            free(vm, currParent);
            currParent = newCurrParent;
        }
    }
    free(vm, currParent);
}