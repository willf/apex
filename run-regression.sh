#!/bin/bash

# Usage: 
#   run-regression.sh [-clean] [test+]

# This shell script will run all of the regression tests in
# ./regression-tests directory or just the tests given on the command
# line.  The -clean option recompiles Apex from scratch.

LISP=$ACLHOME/alisp

RUNDATA=/tmp/apex-run-$$.out
SUMMARY=/tmp/apex-run-summary-$$.out
TESTIMAGE=/tmp/testimage.dxl
APEXDIR=`pwd`
TESTDIR=./regression-tests/
DO_ADD_LISP="(PROGN $ADDED_LISP )"

# Recompile all code if specified
if [[ "-clean" -eq $1 ]]
then
  make basic-clean
  shift
fi

if [[ 0 -eq $# ]] 
then
# get the tests ignoring backup files
    TESTS=`find $TESTDIR -name '*.lisp' | grep -v "#\|~"   `
else
    TESTS=$*
fi

echo $TESTS
echo $APEXDIR

## dump an image
$ACLHOME/alisp -L regression-load -e  "(format t \"Dumping image.\")" -e "(setf (logical-pathname-translations \"apex\") \`((\"**;*.*.*\" ,(make-pathname :directory (append (pathname-directory \"$APEXDIR/\") (list :wild-inferiors)) :name :wild :type :wild))))" -e "(excl:dumplisp :name \"$TESTIMAGE\")" -kill 

## get rid of summary and rundata 
rm -f $SUMMARY $RUNDATA

echo "Starting at: `date`" | tee -a $RUNDATA
for TEST in $TESTS
do
  $LISP -I $TESTIMAGE -e "(setf (logical-pathname-translations \"apex\") \`((\"**;*.*.*\" ,(make-pathname :directory (append (pathname-directory \"$APEXDIR/\") (list :wild-inferiors)) :name :wild :type :wild))))" -e "$DO_ADD_LISP" -L $TEST -kill | tee -a $RUNDATA
done
echo "End: `date`" | tee -a $RUNDATA
egrep "^Starting|^End|^Errors|^Success|^Failure" $RUNDATA | sed 's/^End //' | sed 's/^Errors/  Errors/' | sed 's/^Failures/  Failures/' |sed 's/^Success/  Success/' >> $SUMMARY


echo  =============================
echo ==  Regression Test Summary  ==
echo  =============================

cat $SUMMARY


#$LISP -L 
