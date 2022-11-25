#!/bin/bash

cd "src/$1"
emacs -batch -l ert -l cl -l $1.el -f ert-run-tests-batch-and-exit
cd -
