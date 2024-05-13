#!/bin/bash
cd test
FILES=`(ls *.fish)`
for f in $FILES
do 
  echo "$f:"
  cat $f
  echo "-----------------------"
  cat ${f%.fish}.sexp
  echo "-----------------------"
  ../ps3 src $f 2>&1 
  ../ps3 sexp ${f%.fish}.sexp 2>&1
done
