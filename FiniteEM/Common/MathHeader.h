// MATH_API
#pragma once
#ifdef _LINUX
#define MATH_API
#else
#ifdef Math_EXPORTS
#define MATH_API __declspec(dllexport)
#else
#define MATH_API __declspec(dllimport)
#endif
#endif // _LINUX
