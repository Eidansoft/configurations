#!/bin/bash

function uso() {
	echo "save2stash.sh <FILE>"
}

FILE=$1
# Es obligatorio que se reciba un archivo valido
[ ! -f "$FILE" ] && uso && exit 0
BASE_DIRECTORY=$(echo "$FILE" | cut -d "/" -f1)

echo "FILE: $FILE"
echo "BASEDIRECTORY: $BASE_DIRECTORY"

echo "V1=$1"
echo "V2=$2"
echo "V3=$3"
echo "V4=$4"
echo "V5=$5"
echo "V6=$6"
echo "V7=$7"
echo "V8=$8"
echo "V9=$9"
