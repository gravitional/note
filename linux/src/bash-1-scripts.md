# bash-script-exmaples

## 合并子文件到上层

```bash
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
# find all files
declare -a fList=($(find . -mindepth 1 -type f -print))
# loop rename and move
for f in ${fList[@]}; do
    mv $f "$(dirname $f)-$(basename $f)"
done
# delete empty directorys
find . -mindepth 1 -type d -print0 | xargs --null rmdir
IFS=$SAVEIFS
```

## 解压压缩文件

```bash
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
declare -a archs=($(find . -mindepth 1 -maxdepth 1 -type f -iname '*.zip' -print0 | xargs --null basename -s '.zip'))
declare -p archs
for i in ${archs[@]}; do
    7z x "$i.zip" -o$i
done
IFS=$SAVEIFS
```
