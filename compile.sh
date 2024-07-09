#!/bin/bash

set -e

file=$1

gcc -nostdlib -o program $file
./program
echo $?
