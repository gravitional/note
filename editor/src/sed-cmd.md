# sed command

```bash
# 如果只想打印, 不想实际更改, 就去掉 -i 选项. 
# 不要同时使用 -i -n 选项, 会造成文件丢失.
sed -i \
-e '{
s#[“”]#"#g;
s#[‘’]#"#g;
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
}' \
-zre 's#\n([ \t]*\n[ \t]*)+\n#\n\n#g' \
-zre 's#([ \t]+\n)#\n#g'
```

解释:

```bash
# 替换: 评估->计算
-e 's#评估#运算#g' 
# 删除连续空行
-zre 's#\n([ \t]*\n[ \t]*)+\n#\n\n#g' \
# 删除行末空白
-zre 's#(\n[ \t]+)#\n#g'
```

## rust-sd

```powershell
# 如果只想打印, 不想实际更改, 就去掉 -i 选项. 
# 不要同时使用 -i -n 选项, 会造成文件丢失.
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
sd  '[“”]'  '"'  $tmp
sd  '[‘’]'  "'"  $tmp
sd  --flags m  '\n([ \t]*\n[ \t]*)+\n'  '\n\n'  $tmp
sd  '\s+$'  ''  $tmp
```
