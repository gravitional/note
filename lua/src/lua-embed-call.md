# lua c 互调

## [Embedding Lua in C++][def], `lua_open` 函数被移除

事实上, lua 5.2 参考手册中并未提及 `lua_open` 函数.

一个 `lua_State` 是由 `lua_newstate` 构造的,
您可以使用 `lauxlib.h` 中的 `luaL_newstate` 函数.

要获得此类问题的答案, 更快捷的方法是查阅 Lua 5.2 源代码.

## Lua 与 C/C++ 交互

[Lua 与 C/C++ 交互](https://yjhenan.gitbooks.io/lua-primer/content/08.html)

## reference

[def]: https://stackoverflow.com/questions/8552560/embedding-lua-in-c