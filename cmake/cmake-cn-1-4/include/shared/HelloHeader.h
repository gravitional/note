#pragma once
#ifdef _LINUX
#define Hello_API
#else
#ifdef Hello_EXPORTS
#define Hello_API __declspec(dllexport)
#else
#define Hello_API __declspec(dllimport)
#endif
#endif // _LINUX
