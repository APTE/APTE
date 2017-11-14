#!/bin/bash
## declare an array variable
names=("process" "term" "debug")
S="_"

## now loop through the above array
echo "Renaming files"
for i in "${names[@]}"
do
    echo "Clash: $i"
    for j in $i.ml; do
	BASE=${j%.ml}
	echo "Renaming $BASE.ml":
	mv $BASE.ml $BASE$S.ml
	mv $BASE.mli $BASE$S.mli
    done
done


echo "Renaming calls to renamed modules"
for i in "${names[@]}"
do
    echo "Clash: $i"
    before="$(tr '[:lower:]' '[:upper:]' <<< ${i:0:1})${i:1}"
    after=$before$S
    echo "Sed: $before -> $after"
    echo "sed -i 's/$i/$i$S/g' Makefile"
    sed -i "s/${i}/${i}${S}/g" Makefile
    for file in *.ml*; do
	echo "sed -i 's/${before}/${after}/g' ${file}"
	sed -i "s/${before}/${after}/g" "${file}"   
    done
done
