#!/usr/bin/env bash
# Rename pictures to random names while preserving folder structure. Arg should be the folder path.

set -e

if ! [[ $# -eq 1 ]]; then
    echo "Please specify a folder path in first arg"
fi

folder="$1"

# Make a copy of the folder before doing any changes, just in case
cp -R "$folder" "$folder-copy"

find "$folder"  -name '*.png' -or -name '*.jpeg' -or -name '*.jpg' -or -name '*.JPG' -or -name '*.tiff' | while read fname;
do
    mv "$fname" "$(dirname "$fname")/$(uuidgen | tr -d '-').png"
done