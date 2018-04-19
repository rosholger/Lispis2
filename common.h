#pragma once

#define l_fprintf(f, ...)                       \
    do {                                        \
        fprintf(f, __VA_ARGS__);                \
        fflush(f);                              \
    } while (false)

;