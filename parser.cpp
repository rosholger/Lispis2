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
            state->line++;
            state->column = 0;
        } else {
            if (state->prog[state->pos] == '\n') {
                state->line++;
                state->column = 0;
            } else {
                state->column++;
            }
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
    // Breaks on multiline strings
    state->pos += tokLength;

    Token tok = {tokStart, tokLength,
                 state->line, state->column,
                 T_ERROR};
    const char *t = "true";
    const char *f = "false";

    if (state->prog[tokStart] == 0) {
        tok.type = T_EOF;
    } else if (state->prog[tokStart] == '"') {
        size_t stringLength = strcspn(state->prog+tokStart+1,
                                      "\"");
        if (state->prog[tokStart+1+stringLength] == '"') {
            tok.type = T_STRING;
            tok.length = stringLength+2;
            tokLength = tok.length;
            state->pos = tokStart + tok.length;
            tok.str = (char *)malloc(stringLength+1);
            tok.str[stringLength] = 0;
#if 1
            memcpy(tok.str, state->prog+tokStart+1, stringLength);
#else
            if (stringLength) {
                size_t dst = 0;
                char *src = state->prog+tokStart+1;
                for (size_t i = 0; i < stringLength; ++i) {
                    if (src[i] == '\\') {
                        ++i;
                        switch (src[i]) {
                            case '\'': {
                                tok.str[dst] = '\'';
                            } break;
                            case '\"': {
                                tok.str[dst] = '\"';
                            } break;
                            case '\\': {
                                tok.str[dst] = '\\';
                            } break;
                            case '?': {
                                tok.str[dst] = '\?';
                            } break;
                            case 'a': {
                                tok.str[dst] = '\a';
                            } break;
                            case 'b': {
                                tok.str[dst] = '\b';
                            } break;
                            case 'f': {
                                tok.str[dst] = '\f';
                            } break;
                            case 'n': {
                                tok.str[dst] = '\n';
                            } break;
                            case 'r': {
                                tok.str[dst] = '\r';
                            } break;
                            case 't': {
                                tok.str[dst] = '\t';
                            } break;
                            case 'v': {
                                tok.str[dst] = '\v';
                            } break;
                            default:assert(false);
                        }
                    }
                    ++dst;
                }
                tok.str[dst] = 0;
            }
#endif
            printf("TOK: %.*s\n", (int)tok.length, state->prog+tokStart);
        }
    } else if (state->prog[tokStart] == '.' && tokLength == 1) {
        tok.type = T_DOT;
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
            tok.type = T_UNQUOTE_SPLICING;
        } else {
            tok.type = T_UNQUOTE;
        }
    } else if (state->prog[tokStart] == '`') {
        tok.type = T_QUASIQUOTE;
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
        char temp_str[100]; // Unsafe
        assert(tokLength < 99);
        memcpy(temp_str, state->prog+tokStart, tokLength);
        temp_str[tokLength] = 0;
        Symbol symbol = intern(vm, temp_str);
        //tok.symstr = symbol.str;
        tok.symid = symbol.id;
    }
    Token ret = state->nextToken;
    state->nextToken = tok;
    state->column += tokLength;
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
        get(vm, parent).pair->cdr.type = V_CONS_PAIR;                   \
        get(vm, parent).pair->cdr.pair = 0;                             \
        get(vm, parent).pair->car.type = V_CONS_PAIR;                   \
        get(vm, parent).pair->car.pair = 0;                             \
    } while(false)

void parseObjectLiteralElems(VM *vm, LexState *lex, Handle parent) {
    Value p = allocConsPair(vm);
    get(vm, parent).pair->car.type = V_CONS_PAIR;
    get(vm, parent).pair->cdr.type = V_CONS_PAIR;
    get(vm, parent).pair->car = p;
    Handle lst = reserve(vm, get(vm, parent).pair->car);
    while (peekToken(lex).type != T_RIGHT_CURLY) {
        LineInfo info = {peekToken(lex).line, peekToken(lex).column,
                         get(vm, lst)};
        add(&vm->staticDebugInfo, info);
        Value p = allocConsPair(vm);
        get(vm, lst).pair->car.type = V_CONS_PAIR;
        get(vm, lst).pair->cdr.type = V_CONS_PAIR;
        get(vm, lst).pair->car = p;
        Handle pair = reserve(vm, get(vm, lst).pair->car);
        assert(nextToken(vm, lex).type == T_LEFT_PAREN);
        { // key
            Value p = allocConsPair(vm);
            get(vm, pair).pair->car.type = V_CONS_PAIR;
            get(vm, pair).pair->cdr.type = V_CONS_PAIR;
            get(vm, pair).pair->car = p;
            LineInfo info = {peekToken(lex).line, peekToken(lex).column,
                             get(vm, lst).pair->car};
            add(&vm->staticDebugInfo, info);
            Symbol quasiquote = intern(vm, "quasiquote");
            get(vm, pair).pair->car.pair->car.type = V_SYMBOL;
            get(vm, pair).pair->car.pair->car.sym = quasiquote;
            p = allocConsPair(vm);
            get(vm, pair).pair->car.pair->cdr.type = V_CONS_PAIR;
            get(vm, pair).pair->car.pair->cdr = p;
            info = {peekToken(lex).line, peekToken(lex).column,
                    get(vm, lst).pair->car.pair->cdr};
            add(&vm->staticDebugInfo, info);
            Handle rst = reserve(vm,
                                 get(vm, pair).pair->car.pair->cdr);
            parseExpr(vm, lex, rst);
            free(vm, rst);
        }
        p = allocConsPair(vm);
        get(vm, pair).pair->cdr.type = V_CONS_PAIR;
        get(vm, pair).pair->cdr = p;
        info = {peekToken(lex).line, peekToken(lex).column,
                get(vm, pair).pair->cdr};
        add(&vm->staticDebugInfo, info);
        parseExpr(vm, lex, reserve(vm, get(vm, pair).pair->cdr)); // value
        assert(nextToken(vm, lex).type == T_RIGHT_PAREN);
        free(vm, pair);
        if (peekToken(lex).type != T_RIGHT_CURLY) {
            Value p = allocConsPair(vm);
            get(vm, lst).pair->cdr.type = V_CONS_PAIR;
            get(vm, lst).pair->cdr = p;
            Handle tmp = reserve(vm, get(vm, lst).pair->cdr);
            free(vm, lst);
            lst = tmp;
        }
    }
    free(vm, lst);
    nextToken(vm, lex); // }
}

void parseExpr(VM *vm, LexState *lex, Handle parent) {
    //assert(getC(vm, parent).type == V_CONS_PAIR);
    //assert(getC(vm, parent).pair == 0);
    Token tok = nextToken(vm, lex);
    switch (tok.type) {
        case T_ERROR: {
            fprintf(stderr, "ERROR at %lu:%lu: '%.*s'\n",
                    tok.line, tok.column, (int)tok.length, tok.str);
            assert(false);
        } break;
        case T_DOUBLE: {
            allocPair(vm, parent);
            get(vm, parent).pair->car.type = V_DOUBLE;
            get(vm, parent).pair->car.doub = tok.d;
        } break;
        case T_BOOLEAN: {
            allocPair(vm, parent);
            get(vm, parent).pair->car.type = V_BOOLEAN;
            get(vm, parent).pair->car.boolean = tok.b;
        } break;
        case T_SYMBOL: {
            allocPair(vm, parent);
            get(vm, parent).pair->car.type = V_SYMBOL;
            get(vm, parent).pair->car.sym.id = tok.symid;
        } break;
        case T_STRING: {
            allocPair(vm, parent);
            get(vm, parent).pair->car.type = V_STRING;
            get(vm, parent).pair->car.str = tok.str;
        } break;
        case T_LEFT_CURLY: {
            allocPair(vm, parent);
            Value p = allocConsPair(vm);
            get(vm, parent).pair->car = p;
            Symbol makeObject = intern(vm, "make-object");
            get(vm, parent).pair->car.pair->car.type = V_SYMBOL;
            get(vm, parent).pair->car.pair->car.sym = makeObject;
            p = allocConsPair(vm);
            get(vm, parent).pair->car.pair->cdr.type = V_CONS_PAIR;
            get(vm, parent).pair->car.pair->cdr = p;
            Handle rst = reserve(vm,
                                 get(vm, parent).pair->car.pair->cdr);
            parseObjectLiteralElems(vm, lex, rst);
            LineInfo info = {tok.line, tok.column,
                             get(vm, parent).pair->car};
            add(&vm->staticDebugInfo, info);
            free(vm, rst);
        } break;
        case T_LEFT_PAREN: {
            allocPair(vm, parent);
            if (peekToken(lex).type != T_RIGHT_PAREN) {
                get(vm, parent).pair->car.type = V_CONS_PAIR;
                Value p = allocConsPair(vm);
                get(vm, parent).pair->car = p;
                Handle car = reserve(vm, get(vm, parent).pair->car);
                parseList(vm, lex, car);
                free(vm, car);
            }
            assert(nextToken(vm, lex).type == T_RIGHT_PAREN); // eat right parent
        } break;
        case T_QUOTE: {
            allocPair(vm, parent);
            Value p = allocConsPair(vm);
            get(vm, parent).pair->car = p;
            Symbol quote = intern(vm, "quote");
            get(vm, parent).pair->car.pair->car.type = V_SYMBOL;
            get(vm, parent).pair->car.pair->car.sym = quote;
            p = allocConsPair(vm);
            get(vm, parent).pair->car.pair->cdr.type = V_CONS_PAIR;
            get(vm, parent).pair->car.pair->cdr = p;
            Handle rst = reserve(vm,
                                 get(vm, parent).pair->car.pair->cdr);
            parseExpr(vm, lex, rst);
            LineInfo info = {tok.line, tok.column,
                             get(vm, parent).pair->car};
            add(&vm->staticDebugInfo, info);
            free(vm, rst);
        } break;
        case T_QUASIQUOTE: {
            allocPair(vm, parent);
            Value p = allocConsPair(vm);
            get(vm, parent).pair->car = p;
            Symbol quasiquote = intern(vm, "quasiquote");
            get(vm, parent).pair->car.pair->car.type = V_SYMBOL;
            get(vm, parent).pair->car.pair->car.sym = quasiquote;
            p = allocConsPair(vm);
            get(vm, parent).pair->car.pair->cdr.type = V_CONS_PAIR;
            get(vm, parent).pair->car.pair->cdr = p;
            Handle rst = reserve(vm,
                                 get(vm, parent).pair->car.pair->cdr);
            parseExpr(vm, lex, rst);
            LineInfo info = {tok.line, tok.column,
                             get(vm, parent).pair->car};
            add(&vm->staticDebugInfo, info);
            free(vm, rst);
        } break;
        case T_UNQUOTE: {
            allocPair(vm, parent);
            Value p = allocConsPair(vm);
            get(vm, parent).pair->car = p;
            Symbol unquote = intern(vm, "unquote");
            get(vm, parent).pair->car.pair->car.type = V_SYMBOL;
            get(vm, parent).pair->car.pair->car.sym = unquote;
            p = allocConsPair(vm);
            get(vm, parent).pair->car.pair->cdr.type = V_CONS_PAIR;
            get(vm, parent).pair->car.pair->cdr = p;
            Handle rst = reserve(vm,
                                 get(vm, parent).pair->car.pair->cdr);
            parseExpr(vm, lex, rst);
            LineInfo info = {tok.line, tok.column,
                             get(vm, parent).pair->car};
            add(&vm->staticDebugInfo, info);
            free(vm, rst);
        } break;
        case T_UNQUOTE_SPLICING: {
            allocPair(vm, parent);
            Value p = allocConsPair(vm);
            get(vm, parent).pair->car = p;
            Symbol unquoteSplicing = intern(vm, "unquote-splicing");
            get(vm, parent).pair->car.pair->car.type = V_SYMBOL;
            get(vm, parent).pair->car.pair->car.sym = unquoteSplicing;
            p = allocConsPair(vm);
            get(vm, parent).pair->car.pair->cdr.type = V_CONS_PAIR;
            get(vm, parent).pair->car.pair->cdr = p;
            Handle rst = reserve(vm,
                                 get(vm, parent).pair->car.pair->cdr);
            parseExpr(vm, lex, rst);
            LineInfo info = {tok.line, tok.column,
                             get(vm, parent).pair->car};
            add(&vm->staticDebugInfo, info);
            free(vm, rst);
        } break;
        case T_RIGHT_PAREN: {
            fprintf(stderr, "ERROR at %lu:%lu: Mismatching parenthesis, "
                    ") not matching any (\n",
                    tok.line, tok.column);
            assert(false);
        } break;
        case T_EOF: {
            fprintf(stderr, "ERROR: Mismatching parenthesis, more ( than )\n");
            assert(false);
        } break;
        default: {
            fprintf(stderr, "ERROR at %lu:%lu: %d not recognized yet\n",
                    tok.line, tok.column, tok.type);
            assert(false);
        } break;
    }
}

void parseList(VM *vm, LexState *lex, Handle parent) {

    Token tok = peekToken(lex);
    Handle currParent = reserve(vm, get(vm, parent));
    while (tok.type != T_RIGHT_PAREN) {
        LineInfo info = {tok.line, tok.column,
                         get(vm, currParent)};
        add(&vm->staticDebugInfo, info);
        parseExpr(vm, lex, currParent);
        tok = peekToken(lex);
        if (tok.type == T_DOT) {
            nextToken(vm, lex); // eat dot;
            // UGLY HACK!!!
            Handle cdrElemHackLst = reserve(vm, allocConsPair(vm));
            parseExpr(vm, lex, cdrElemHackLst);
            get(vm, currParent).pair->cdr =
                get(vm, cdrElemHackLst).pair->car;
            free(vm, cdrElemHackLst);
            break;
        } else if (tok.type != T_RIGHT_PAREN) {
            Value p = allocConsPair(vm);
            get(vm, currParent).pair->cdr = p;
            Handle newCurrParent = reserve(vm,
                                           get(vm,
                                               currParent).pair->cdr);
            free(vm, currParent);
            currParent = newCurrParent;
        }
    }
    free(vm, currParent);
}