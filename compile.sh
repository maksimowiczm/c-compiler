#!/bin/bash

file=$1

set -e

echo "Running gcc"
gcc -nostdlib -o program $file

set +e

echo "Executing ./program"
./program

echo "Exit code: $?"
