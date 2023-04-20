#include <iostream>
#include <limits>
#include <cstdint>

#define prt(V) std::cout << #V " = " << V << "\n";
#define prt2(M, V) std::cout << M " = " << V << "\n";

int main()
{
    prt(sizeof(int8_t));
    prt(sizeof(int16_t));
    prt(sizeof(int32_t));
    prt(sizeof(int64_t));
    prt(sizeof(uint8_t));
    prt(sizeof(uint16_t));
    prt(sizeof(uint32_t));
    prt(sizeof(uint64_t));
    std::cout << "\n";

    prt(sizeof(int_fast8_t));
    prt(sizeof(int_fast16_t));
    prt(sizeof(int_fast32_t));
    prt(sizeof(int_fast64_t));
    prt(sizeof(uint_fast8_t));
    prt(sizeof(uint_fast16_t));
    prt(sizeof(uint_fast32_t));
    prt(sizeof(uint_fast64_t));
    std::cout << "\n";

    prt(sizeof(int_least8_t));
    prt(sizeof(int_least16_t));
    prt(sizeof(int_least32_t));
    prt(sizeof(int_least64_t));
    prt(sizeof(uint_least8_t));
    prt(sizeof(uint_least16_t));
    prt(sizeof(uint_least32_t));
    prt(sizeof(uint_least64_t));
    std::cout << "\n";

    prt2("max of integer memory bytes", sizeof(intmax_t));
    prt2("max of integer pointer memory bytes", sizeof(intptr_t));
    prt2("max of unsigned integer memory bytes", sizeof(uintmax_t));
    prt2("max of unsigned integer pointer memory bytes", sizeof(uintptr_t));
    std::cout << "\n";

    prt(INT8_MIN);
    prt(INT16_MIN);
    prt(INT32_MIN);
    prt(INT64_MIN);
    prt(INT_FAST8_MIN);
    prt(INT_FAST16_MIN);
    prt(INT_FAST32_MIN);
    prt(INT_FAST64_MIN);
    prt(INT_LEAST8_MIN);
    prt(INT_LEAST16_MIN);
    prt(INT_LEAST32_MIN);
    prt(INT_LEAST64_MIN);
    prt(INTPTR_MIN);
    std::cout << "\n";

    prt(INT8_MAX);
    prt(INT16_MAX);
    prt(INT32_MAX);
    prt(INT64_MAX);
    prt(INT_FAST8_MAX);
    prt(INT_FAST16_MAX);
    prt(INT_FAST32_MAX);
    prt(INT_FAST64_MAX);
    prt(INT_LEAST8_MAX);
    prt(INT_LEAST16_MAX);
    prt(INT_LEAST32_MAX);
    prt(INT_LEAST64_MAX);
    prt(INTPTR_MAX);
    std::cout << "\n";

    prt(UINT8_MAX);
    prt(UINT16_MAX);
    prt(UINT32_MAX);
    prt(UINT64_MAX);
    prt(UINT_FAST8_MAX);
    prt(UINT_FAST16_MAX);
    prt(UINT_FAST32_MAX);
    prt(UINT_FAST64_MAX);
    prt(UINT_LEAST8_MAX);
    prt(UINT_LEAST16_MAX);
    prt(UINT_LEAST32_MAX);
    prt(UINT_LEAST64_MAX);
    prt(UINTPTR_MAX);
}