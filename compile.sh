#!/bin/bash

file=$1

echo "Running gcc on $file"
gcc -g -o program $file || { echo "Compilation failed"; exit 1; }

echo "Executing ./program"
./program

echo "Exit code: $?"
