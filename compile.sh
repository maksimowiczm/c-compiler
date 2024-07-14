#!/bin/bash

file=$1

if [ -z $file ]; then
    echo "Usage: $0 <file>"
    exit 1
fi

if [ ! -f $file ]; then
    echo "File $file not found"
    exit 1
fi

cargo build || { echo "Cargo build failed"; exit 1; }

cargo run -- asm64 $file --output "output.s" || { echo "Cargo run failed"; exit 1; }

echo "Running gcc on output.s"
gcc -g -o program "output.s" || { echo "Compilation failed"; exit 1; }

echo "Executing ./program"
./program

echo "Exit code: $?"
