
# latexmk

```rust
use std::error::Error;
use std::ffi::OsStr;
use std::path::PathBuf;
use structopt::StructOpt;
use subprocess::{Exec, Redirection};

/// 给出 latex 文件的路径, 以及编译使用的引擎
#[derive(StructOpt, Debug)]
#[structopt(name = "xelamk")]
struct Opt {
    /// `v/verbose`标志出现的次数
    /// Verbose 模式 (-v, -vv, -vvv等).
    #[structopt(short, long, parse(from_occurrences))]
    verbose: u8,
    /// 使用的 latex 引擎
    #[structopt(short, long, default_value = "-xelatex")]
    engine: String,
    /// latex 文件的路径
    #[structopt(name = "FILE", parse(from_os_str), default_value = "main.tex")]
    filepath: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let config_str: Vec<&str> = vec![
        "-silent",
        "-pv",
        "-view=pdf",
        "-bibtex",
        "-cd",
        "-recorder",
        "-file-line-error",
        "-interaction=nonstopmode",
        "-synctex=1",
    ];

    let mut config: Vec<&OsStr> = config_str.iter().map(|x| OsStr::new(x)).collect();

    // println!("config is {:#?}", &config);

    let opt = Opt::from_args();
    let mut files: Vec<&OsStr> = opt.filepath.iter().map(|x| x.as_os_str()).collect();
    println!("\n the latex files: {:#?}", &files);

    let tex_tool = OsStr::new("latexmk"); // 返回  &OsStr
    let tex_default = OsStr::new(&opt.engine);
    let mut args = vec![tex_default];
    args.append(&mut config);
    args.append(&mut files);

    // println!("the args is {:#?}\n", args);

    let out = Exec::cmd(&tex_tool)
        .args(&args)
        .stdout(Redirection::Pipe)
        .capture()?
        .stdout_str();

    Ok(())
}
```