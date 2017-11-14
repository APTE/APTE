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
	mv $BASE$S.ml $BASE.ml
	mv $BASE$S.mli $BASE.mli
    done
done


echo "Renaming calls to renamed modules"
for i in "${names[@]}"
do
    echo "Clash: $i"
    before="$(tr '[:lower:]' '[:upper:]' <<< ${i:0:1})${i:1}"
    after=$before$S
    echo "Sed: $before -> $after"
    echo "sed -i 's/$i$S/$i/g' Makefile"
    sed -i "s/${i}${S/${i}}/g" Makefile
    for file in *.ml*; do
	echo "sed -i 's/${after}/${before}/g' ${file}"
	sed -i "s/${after}/${before}/g" "${file}"   
    done
done
