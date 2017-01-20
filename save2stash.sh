#!/bin/bash

# Author Alejandro Lorente eidansoft at gmail dot com
# To use this script you need to have the $FILENAMES variable created at your GIT-GUI
# Use from GIT-GUI tool with the commands:
#     /path/save2stash.sh save $GIT_DIR $FILENAMES
#     /path/save2stash.sh recover $GIT_DIR $FILENAMES

# Config
STASH_FOLDER="/Users/alorente/borrable/git_stash_folder"
TO_STASH="save"
FROM_STASH="recover"

# DO NOT TOUCH BELOW THIS LINE
OP=$1
PROJECT_FOLDER=$(dirname $2)
PROJECT_NAME=$(basename $PROJECT_FOLDER)
ALL_PARAM=$@
FILES=$(echo "$ALL_PARAM" | cut -d " " -f3-)

function uso() {
	echo "ERROR: Your call was incorrect ($0 $ALL_PARAM)"
	echo "Operation=$OP"
	echo "Project name=$PROJECT_NAME"
	echo "Files=$FILES"
	echo "You must use: save2stash.sh <$TO_STASH|$FROM_STASH> <FILES...>"
}

function createFolderIfNotExist(){
	FOLDER=$1
	[ ! -d "$FOLDER" ] && mkdir -p $FOLDER
}

function copy2stash() {
	FILE=$1
	FILEFOLDER=$(dirname $FILE)
	createFolderIfNotExist "$STASH_FOLDER/$PROJECT_NAME/$FILEFOLDER"
	mv $FILE "$STASH_FOLDER/$PROJECT_NAME/$FILE"
}

function doSaveOperation() {
	for file in $FILES; do
        	copy2stash $file
	done
}

function doRecoverOperation(){
	for file in $FILES; do
		FILEFOLDER=$(dirname $FILE)
		cp -r "$STASH_FOLDER/$PROJECT_NAME/" $PROJECT_FOLDER
		rm -rf "$STASH_FOLDER/$PROJECT_NAME"
		break;
	done
}

# Parameters are mandatory
[ "$OP" != "$TO_STASH" -a "$OP" != "$FROM_STASH" ] && echo "ERROR: Wrong operation" && uso && exit 0
[ "$FILES" == "" ] && echo "ERROR: At least one file is mandatory" && uso && exit 0

BASE_DIRECTORY=$(echo "$FILES" | cut -d "/" -f1)

createFolderIfNotExist $STASH_FOLDER
if [ "$OP" == "$TO_STASH" ]; then
	doSaveOperation
else
	doRecoverOperation
fi

echo "Files copied!!!"
