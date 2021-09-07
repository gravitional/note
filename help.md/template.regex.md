# template.regex.md

## ` {} to \r `

删除`{}`内的内容, 并换成换行符` \r `, 贪婪模式

```reg
%s/\{.+\}/\r/g
```

