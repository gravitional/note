# open blas, lapack

Install Miniconda3 for 64 bits using winget install --id Anaconda.Miniconda3 or easily download from [conda.io](https://docs.conda.io/en/latest/miniconda.html).

Install Miniconda3 for 64 bits using winget install --id Anaconda.Miniconda3 or easily download from [conda.io](https://docs.conda.io/en/latest/miniconda.html).

Open the "Anaconda Command Prompt," now available in the Start Menu, or at `%USERPROFILE%\miniconda3\shell\condabin\conda-hook.ps1`.

In that command prompt window, use cd to change to the directory where you want to build OpenBLAS

Now install all of the tools we need:

```bash
conda update -n base conda
conda config --add channels conda-forge
conda install -y cmake flang clangdev perl libflang ninja
```

Still in the Anaconda Command Prompt window, activate the MSVC environment for 64 bits
with vcvarsall x64. On Windows 11 with Visual Studio 2022, this would be done by invoking:

```bash
# in cmd
# "c:\Program Files\Microsoft Visual Studio\2022\Preview\vc\Auxiliary\Build\vcvars64.bat"
# in powershell
Invoke-CmdScript "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
```

With VS2019, the command should be the same – except for the year number, obviously.
For other/older versions of MSVC, the VS documentation or a quick search on the web should turn up the exact wording you need.

6. Now configure the project with CMake. Starting in the project directory, execute the following:

```powershell
# set "LIB=%CONDA_PREFIX%\Library\lib;%LIB%" # in cmd
# set "CPATH=%CONDA_PREFIX%\Library\include;%CPATH%" # in cmd

$env:LIB="${env:CONDA_PREFIX}\Library\lib;${env:LIB}" # in powershell
$env:CPATH="${env:CONDA_PREFIX}\Library\include;${env:CPATH}" # in powershell
mkdir build
cd build
cmake .. -G "Ninja" -DCMAKE_CXX_COMPILER=clang-cl -DCMAKE_C_COMPILER=clang-cl \
-DCMAKE_Fortran_COMPILER=flang -DCMAKE_MT=mt -DBUILD_WITHOUT_LAPACK=no \
-DNOFORTRAN=0 -DDYNAMIC_ARCH=ON -DCMAKE_BUILD_TYPE=Release
```

7. Build the project:

```powershell
cmake --build . --config Release
```

8. Install all relevant files created by the build

```powershell
cmake --install . --prefix c:\opt -v
```

### powershell 环境变量设置

```powershell
# Invokes a Cmd.exe shell script and updates the environment.
function Invoke-CmdScript {
    param(
        [String] $scriptName
    )
    $cmdLine = """$scriptName"" $args & set"
    & $Env:SystemRoot\system32\cmd.exe /c $cmdLine |
    select-string '^([^=]*)=(.*)$' | foreach-object {
        $varName = $_.Matches[0].Groups[1].Value
        $varValue = $_.Matches[0].Groups[2].Value
        set-item Env:$varName $varValue
    }
}
```
