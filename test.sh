#!/bin/bash

NAME=${1-test-all}

emacs -batch -l ert -l cl -l $NAME.el -f ert-run-tests-batch-and-exit
