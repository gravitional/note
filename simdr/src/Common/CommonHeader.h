#pragma once
#ifdef _LINUX
#   define COMMON_API
#else
#   ifdef Common_EXPORTS
#      define COMMON_API __declspec(dllexport)
#   else
#       define COMMON_API __declspec(dllimport)
#   endif
#endif //_LINUX

//#include "Global.h"