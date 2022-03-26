# leetcode

## 不重复子串

```
cs[sym_Symbol] := Module[{posList},
  sym["winL"] = 1;
  sym["winR"] = 1;
  sym["maxLen"] = 1;
  posList = <||>;
  sym["eat"][char_] := (
    (*如果已经存在*)
    If[KeyExistsQ[posList, char] && 
      posList@char > (sym@"winL" // EchoLabel["winL"]),
     sym["maxLen"] = 
      Max[sym["maxLen"], sym["winR"] - sym["winL"] - 1] // 
       EchoLabel["maxLen"];
     sym["winL"] = posList@char
     
     ];
    posList@char = sym["winR"] // EchoLabel["winR"];
    posList // Echo;
    sym["winR"]++;
    );
  ]

cs[fn]
tea = Characters["qwertqwertqweqwert"]
tea // Length

 cs[fn]
count = 1;
Do[fn["eat"][tea[[count++]]], 18]
?? fn
```
