# rust const str const fn初始化

[How to format a const string](https://stackoverflow.com/questions/32279858/how-to-format-a-const-string)

可以使用 [const_format](https://docs.rs/const_format/latest/const_format/) crate
来初始化 `const str`;

```rs
use const_format::formatcp;

#[cfg(target_os = "macos")]
const OS: &'static str = "OSx";
#[cfg(target_os = "windows")]
const OS: &'static str = "Windows";

const SOME_STRING: &'static str = formatcp!("this os is {}", OS);

pub fn main() {
    println!("{}", SOME_STRING);
}
```

例子输出为 `this os is Windows`

这段代码只是一个例子,
因为你可以简单地将 `This os is` 复制到每个 `cfg` 字符串中, 还应该考虑使用 [std::env::const::os](https://doc.rust-lang.org/stable/std/env/consts/constant.OS.html)
