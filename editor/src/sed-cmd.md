# sed command

```bash
# 如果只想打印, 不想实际更改, 就去掉 -i 选项. 
# 不要同时使用 -i -n 选项, 会造成文件丢失.
sed -i \
-e "s#[‘’]#'#g" \
-e '{
s#\x09#    #g;
s#[“”]#"#g;
s#：#: #g; 
s#，#, #g; 
s#。#. #g; 
s#、#, #g; 
s#（#(#g; 
s#）#)#g; 
s#；#\; #g; 
s#！#! #g; 
s#？#? #g; 
s#‹#<#g; 
s#›#>#g;
s#《#<#g; 
s#》#>#g;
s#【#[#g; s#】#]#g;
s#『#{#g; s#』#}#g;
s#评估#计算#g;
}' \
-zre 's#\n([ \t]*\n[ \t]*)+\n#\n\n#g' \
-zre 's#[ \t]+\n#\n#g'
```

解释:

```bash
# 替换: 评估->计算
-e 's#评估#计算#g' 
# 删除连续空行
-zre 's#\n([ \t]*\n[ \t]*)+\n#\n\n#g'
# 删除行末空白
-zre 's#[ \t]+\n#\n#g'
```

## rust-sd

windows 上的换行符为`\r\n`, 另外`quoting`规则也不同.

[Windows 换行符为什么设计成 CR+LF](https://www.zhihu.com/question/19967857/answer/21917057)
Wiki 上有说, 简单来说就是当时的电传打字机使用 `CR-LF` 作为换行, 
其中 `CR` 将打印头挪到行首, `LF` 将它移动到下一行.
因为 `CR` 移动的距离更长, 所以它在先, 保证两个字符发完打印头一定可以移动到第二行的行头.
当时 DEC 的机器都是这么干的, 后来 CP/M、DOS 就遵守了这个约定.
Multics 则利用驱动分离了打印机和程序, 所以使用单个 `LF`, 这样处理方便些.

```powershell
function conv-punct {
$tmp=(Get-ChildItem $args) ;    
sd  --string-mode  '：'  ': '  $tmp;
sd  --string-mode  '，'  ', '  $tmp;    
sd  --string-mode  '。'  '. '  $tmp;
sd  --string-mode  '、'  ', '  $tmp;
sd  --string-mode  '（'  '('  $tmp;
sd  --string-mode  '）'  ')'  $tmp;
sd  --string-mode  '；'  ';'  $tmp;
sd  --string-mode  '！'  '!'  $tmp;
sd  --string-mode  '？'  '?'  $tmp;
sd  --string-mode  '‹'   '<'    $tmp;
sd  --string-mode  '›'   '>'    $tmp;
sd  --string-mode  '《'  '<'  $tmp;
sd  --string-mode  '》'  '>'  $tmp;
sd  --string-mode  '【'  '['  $tmp;
sd  --string-mode  '】'  ']'  $tmp;
sd  --string-mode  '『'  '{'  $tmp;
sd  --string-mode  '』'  '}'  $tmp;
sd  --string-mode  "评估" "计算"  $tmp;
sd '[“”]'  '\"'     $tmp;
sd "[‘’]"  "\'"  $tmp;
sd '\r\n([ \t]*\r\n[ \t]*)+\r\n' '\r\n\r\n'  $tmp;
sd '[ \t]+\r\n' '\r\n'     $tmp; 
}
```

`linux` 上的换行符为`\n`, 所以

```powershell
sd  --string-mode  '：'  ': '  $tmp
sd  --string-mode  '，'  ', '  $tmp
sd  --string-mode  '。'  '. '  $tmp
sd  --string-mode  '、'  ', '  $tmp
sd  --string-mode  '（'  '('  $tmp
sd  --string-mode  '）'  ')'  $tmp
sd  --string-mode  '；'  ';'  $tmp
sd  --string-mode  '！'  '!'  $tmp
sd  --string-mode  '？'  '?'  $tmp
sd  --string-mode  '‹'    '<'    $tmp
sd  --string-mode  '›'    '>'    $tmp
sd  --string-mode  '《'  '<'  $tmp
sd  --string-mode  '》'  '>'  $tmp
sd  --string-mode  '【'  '['  $tmp
sd  --string-mode  '】'  ']'  $tmp
sd  --string-mode  '『'  '{'  $tmp
sd  --string-mode  '』'  '}'  $tmp
sd '[“”]'  '"'  $tmp
sd '[‘’]'  "'"  $tmp
sd '\n\s+\n' '\n\n'  $tmp
sd '[ \t]+$'  ''  $tmp
```
