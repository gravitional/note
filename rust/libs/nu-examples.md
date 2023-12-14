# nushell 例子

文件重命名

```nushell
let fs = glob *.{cpp,h}; $fs | each {|it| let it0 = $it; mut it = $it0 | path parse; $it.extension = 'png' ; let it = $it | path join; mv $it0 $it;}
```
