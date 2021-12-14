use std::error::Error;
// use std::ffi::{OsStr, OsString};
use std::path::PathBuf;
use structopt::StructOpt;
use subprocess::{Exec, Redirection};

use grep::printer::Standard;
use grep::regex::RegexMatcher;
use grep::searcher::Searcher;
use std::path::Path;
use termcolor::{ColorChoice, StandardStream};

/// 给出 latex 文件的路径, 以及编译使用的引擎
#[derive(StructOpt, Debug)]
#[structopt(name = "texmk")]
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
        "-interaction=nonstopmode",
        "-pv",
        "-view=pdf",
        "-bibtex",
        "-cd",
        "-recorder",
        "-file-line-error",
        "-synctex=1",
    ];

    // 转换成 String 的 vector
    let config: Vec<String> = config_str.iter().map(|x| x.to_string()).collect();

    // println!("config is {:#?}", &config);
    // 解析命令行参数
    let opt = Opt::from_args();
    // 将 PathBuf 转换成 OsString
    let files: Vec<String> = opt
        .filepath
        .iter()
        .map(|x| x.to_str().unwrap().to_string())
        .collect();
    // println!("\n the latex files: {:#?}", &files);
    //  &OsStr, 编译工具
    let tex_tool = String::from("latexmk");
    // 传入 字符串 slice，即 &str
    let tex_engine = String::from(&opt.engine);
    // 把参数组合起来 https://doc.rust-lang.org/std/fmt/index.html
    let mut args = Vec::from([tex_engine]);
    args.extend_from_slice(&config);
    args.extend_from_slice(&files);
    println!("the args is {:#?}\n", args);
    // 执行 latex 编译命令
    Exec::cmd(&tex_tool)
        .args(&args)
        .stderr(Redirection::Merge)
        .join()?;

    // 查找错误位置
    let mut file_log = String::from(files[0].strip_suffix(".tex").unwrap());
    file_log = file_log + ".log";
    // 查找错误
    Exec::cmd("grep")
        .arg(r" -m 10 -Pi -n --color -B 0 -A 8 \[\d+\] ")
        .arg(&file_log)
        .stderr(Redirection::Merge)
        .join()?;
    Ok(())
}

// https://docs.rs/grep-printer/latest/grep_printer/
fn find_error(file: &str) -> Result<(), Box<dyn Error>> {
    let path = Path::new(file);

    let matcher = RegexMatcher::new(r"\[\d+\]")?;
    // let mut printer = Standard::new_no_color(vec![]);
    let mut printer = Standard::new(StandardStream::stdout(ColorChoice::Always));
    Searcher::new().search_path(&matcher, path, printer.sink(&matcher))?;
    // into_inner 把我们提供给 new_no_color 的底层 writer 还给我们。
    // 它被包裹在  termcolor::NoColor 中.
    // 因此，第二个 into_inner 给我们的是实际的缓冲区.
    //  let output = String::from_utf8(printer.into_inner())?;
    //  println!("{:#?}", printer.into_inner());
    Ok(())
}
