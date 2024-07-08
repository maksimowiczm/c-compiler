#!/bin/bash

file=$1

gcc -nostdlib -o program $file
./program
echo $?
