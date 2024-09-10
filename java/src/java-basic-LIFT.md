# java 环境安装

[Hello World in Java (Windows)](https://lift.cs.princeton.edu/java/windows/)

lift.cs.princeton 打包的 Java 环境, 配合 Algorithms, 4th Edition (including COS 226 students) 使用.

`LIFT.exe` 安装的 git bash, 会附带一些 java 编译器的 wrapper 脚本,
因此记得要勾选 `install git bash` 复选框.

```bash
C:/Program Files/Git/usr/local/bin # 脚本的默认位置
C:/Program Files/Git/usr/local/lift/lib # 课程使用的自定义 lib 的位置
```

## lift-java-installer.exe 安装的组件

What does the lift-java-installer.exe installer do?
In short, it installs and configures Java, IntelliJ, Git Bash, Xming, SpotBugs, PMD, Checkstyle, and our textbook libraries, along with accompanying command-line tools. Here is a more detailed list:

+ Installs [Temurin OpenJDK 11.0.20](https://adoptium.net/temurin/releases/?version=11) and adds it to the PATH.
+ Installs [IntelliJ 2024.2](https://www.jetbrains.com/idea/download/#section=windows) with customized user preferences.
+ Installs [Git Bash 2.36.1](https://git-scm.com/downloads) and adds it to the PATH.
+ Installs [Xming 6.9.0.31](https://sourceforge.net/projects/xming).

+ Installs the following command-line tools for Java:
    + The textbook libraries [stdlib.jar](https://introcs.cs.princeton.edu/code/stdlib.jar) and [algs4.jar](https://algs4.cs.princeton.edu/code/algs4.jar).
    + Java wrapper scripts, including [javac-algs4](https://lift.cs.princeton.edu/java/javac-algs4) and [java-algs4](https://lift.cs.princeton.edu/java/java-algs4).
    + [SpotBugs 4.8.4](https://spotbugs.github.io/); our SpotBugs configuration file [spotbugs.xml](https://lift.cs.princeton.edu/java/spotbugs.xml); and wrapper script [spotbugs](https://lift.cs.princeton.edu/java/spotbugs).
    + [PMD 6.34.0](https://pmd.github.io/pmd-6.34.0/); our PMD configuration file [pmd.xml](https://lift.cs.princeton.edu/java/pmd.xml); and wrapper script [pmd](https://lift.cs.princeton.edu/java/pmd).
    + [Checkstyle 10.12.1](http://checkstyle.sourceforge.net/); various configuration files ([checkstyle-cos126.xml](https://lift.cs.princeton.edu/java/checkstyle-cos126.xml), [checkstyle-cos226.xml](https://lift.cs.princeton.edu/java/checkstyle-cos226.xml), [checkstyle-coursera.xml](https://lift.cs.princeton.edu/java/checkstyle-coursera.xml), and [checkstyle-suppressions.xml](https://lift.cs.princeton.edu/java/checkstyle-suppressions.xml));
    custom checks [checkstyle-lift.jar](https://lift.cs.princeton.edu/java/checkstyle-lift.jar); and wrapper script [checkstyle](https://lift.cs.princeton.edu/java/checkstyle).

## Which are the most important IntelliJ menu options to remember?

Here are the most important ones (and their shortcuts).
LIFT → New Java Class (Ctrl + N).  Create a new Java class.
LIFT → Recompile (Ctrl + B).  Compile the current program.
LIFT → Run with Arguments (Ctrl + E).  Run the current program with command-line arguments.
File → Save All (Ctrl + S).  Save (and reformat) all open files.
View → Tool Windows → Project (Alt + 1).  Show/hide the Project View sidebar.
View → Tool Windows → Terminal (Alt + 2).  Show/hide the Terminal window.

## How do I specify EOF to signal that standard input is empty?

On Mac OS X and Linux, type Enter Ctrl-D. On Windows, type Enter Ctrl-Z Enter, even in Git Bash.

## How can I run SpotBugs, PMD, and Checkstyle from the command line?

The installer includes wrapper scripts to simplify this process.

To run SpotBugs 4.8.4, type the following command in the terminal:

```bash
spotbugs HelloWorld.class
```

Running spotbugs on `HelloWorld.class`:
The argument must be a list of `.class` files. Here is a list of [bug descriptions](https://spotbugs.github.io/#bug-descriptions).

To run PMD 6.34.0, type the following command in the terminal:

```bash
pmd HelloWorld.java
```

Running pmd on HelloWorld.java:
The argument must be either a single .java file or a directory containing one or more .java files.
Here is a list of [bug patterns](https://docs.pmd-code.org/pmd-doc-6.34.0/pmd_rules_java.html).

To run Checkstyle 10.12.1, type one of the following commands in the terminal,
depending on whether you are COS 126, COS 226, or Coursera student:

```bash
checkstyle -cos126 HelloWorld.java
```

Running checkstyle on HelloWorld.java:

```bash
checkstyle -cos226 HelloWorld.java
```

Running checkstyle on HelloWorld.java:

```bash
checkstyle -coursera HelloWorld.java
```

Running checkstyle on HelloWorld.java:

The argument must be a list of .java files. Here is a list of [available checks](https://checkstyle.sourceforge.io/checks.html).

## Can I use the Command Prompt, PowerShell, or Windows Subsystem for Linux instead of Git Bash for Windows?

We strongly recommend Git Bash. For example, the commands javac-algs4 and checkstyle-algs4 will work only in Git Bash. If you want to use another shell, you'll need to configure it yourself.

## Which Linux-style commands are available in Git Bash for Windows?

Here are the [Bash built-in commands](https://lift.cs.princeton.edu/java/windows/bash.builtin.commands.txt) and here are the [external commands](https://lift.cs.princeton.edu/java/windows/git.bash.commands.txt) in C:\Program Files\Git\usr\bin.
