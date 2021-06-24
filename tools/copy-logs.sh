#!/bin/sh

TARGET=$2
FILES=$(find $1 -iname '*.rout.fail' -o -iname '*.log')

mkdir -p $TARGET
for f in $FILES; do
  T="${TARGET}/$(echo $f | sed 's/\//-/g')"
  cp $f $T
done

# This collects errors on failure and CI should fail afterwards
exit 1
