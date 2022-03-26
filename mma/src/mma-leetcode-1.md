# leetcode

## 不重复子串

```
cst[sym_Symbol] := Module[{posList},
  sym["winL"] = 1;
  sym["winR"] = 1;
  sym["maxLen"] = 1;
  sym["posList"] = <||>;
  sym["eat"][char_] := (
    (*如果已经存在*)
    If[KeyExistsQ[sym["posList"], char] && 
      sym["posList"]@char > sym@"winL",
     sym["maxLen"] = Max[sym["maxLen"], sym["winR"] - sym["winL"] - 1];
     sym["winL"] = sym["posList"]@char
     
     ];
    AppendTo[sym["posList"], char -> sym["winR"]] ;
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
