#!/usr/bin/env bash

# 替换文档中的中文标点. 如果只想打印，不想实际更改，就去掉 -i 选项
sed -i -n -e 's#“#\"#gp'  -e 's#”#\"#gp' -e 's#：#: #gp' -e 's#，#, #gp' -e 's#。#. #gp' -e 's#、#, #gp' -e 's#（#(#gp' -e 's#）#)#gp' test.txt 