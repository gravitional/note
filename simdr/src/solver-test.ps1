## 批量运行solver 程序, testdriver 测试命令, debug
$runFile = 'runList.txt'; $logFile = 'runLog.log'
# 初始化两个文本文件
Out-File -Force -FilePath $runFile -Encoding 'utf8' -InputObject ''
Out-File -Force -FilePath $logFile -Encoding 'utf8' -InputObject ''
$testsOri = @(ls -Recurse -Directory 'SolvingDomain' )
$tLast = $testsOri.Length - 1
$tests = $testsOri[0..$tLast] #默认选取全部运行，按字母排序, 从0开始
$myArgs = '-dtep8'
foreach ($t in $tests) {
    $tName = $t.FullName;
    $t1 = Get-Date
    Out-File -Append -FilePath $runFile -Encoding 'utf8' -InputObject ($t1.ToString() + ' :Run: ' + $tName)
    # solver 日志 重定向, 不能加括号，不然会运行完再重定向
    D:\SolverRemote\bin\Debug\solver.exe $myArgs $tName | Tee-Object -FilePath $logFile
    # 日志重定向
    $t2 = Get-Date
    Out-File -Append -FilePath $runFile -Encoding 'utf8' -InputObject ($t2.ToString() + ' :End: ' + $tName)
    Out-File -Append -FilePath $runFile -Encoding 'utf8' -InputObject ('TotalMinuts: ' + ($t2 - $t1).TotalMinutes.ToString())
    Out-File -Append -FilePath $runFile -Encoding 'utf8' -InputObject ''
}

### 如果还有 app 项目，为了具体过滤出 simdroid 项目， 使用下面的命令
$tests = @(ls -Recurse -Directory | where { $_.FullName -match 'Solving\\SolvingDomain$' } );

### 筛选工程目录
ls -Recurse -Directory | where { $_.FullName -match 'Solving\\SolvingDomain$' }  | select FullName >test3.txt

### solver 日志 重定向
D:\SolverRemote\bin\Debug\solver.exe -dtep4 $tName | Tee-Object -FilePath $logFile
D:\SolverRemote\bin\Debug\solver.exe -dtep4 $tName > $logFile

## 批量 start-process 运行，非阻塞
$num = 0
$tests = @(ls -Recurse -Directory 'SolvingDomain' )
$runFile = 'runList.txt'; Write-Output ''> $runFile
$p = @(1..$tests.Length) # 存储返回的进程对象
foreach ($t in $tests) {
    $pOptions = @{
        FilePath               = 'D:\SolverRemote\bin\Debug\solver.exe'
        ArgumentList           = "-dtep4 $($t.FullName)"
        RedirectStandardOutput = 'running.log'
        PassThru               = $true
        UseNewEnvironment      = $false
        NoNewWindow            = $true
    }
    Write-Output ((Get-Date).ToString() + ' :Run: ' + $t.FullName) >> $runFile
    echo $pOptions
    $p[$num] = (Start-Process @pOptions)
    Write-Output ((Get-Date).ToString() + ' :End: ' + ($t.FullName) ) >> $runFile
    Write-Output ''>> $runFile
    $num += 1
}



## 查看所有 result.h5
ls -Recurse -Filter 'result.h5' | select FullName

### 统计 result.h5 共有多少个
ls -Recurse -Filter 'result.h5' | select FullName | measure

## 筛选出 result 文件夹, 并进行备份, 备份到 xxx-D4.2
$bakName = '-D4.2'
ls -Directory -Recurse -Filter 'result' | foreach { copy $_.FullName ($_.FullName + $bakName) -WhatIf }

### 另一个版本
$bakName = '-D4.2'
ls -Directory -Recurse | where { $_.FullName -match 'result$' } | foreach { copy $_.FullName ($_.FullName + $bakName) -WhatIf }
