#!/bin/bash
# Runs Babel on all js files in the "original" folder, and puts them into the "converted" folder
rm -rf converted
mkdir converted
cp -R original/. converted/
for f in $(find converted -name '*.js')
do
    ./node_modules/.bin/babel $f > temp.js
    mv temp.js $f
done
