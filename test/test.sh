#!/bin/sh

if [ $# -lt 1 ]; then
	echo "Usage: $0 folder"
	exit
fi

cd "$(dirname $0)"

set -e

echo "\e[32m- Running compiler\e[m"
../tack $1/main.tack -o $1/main.asm

echo "\e[34m- Running nasm\e[m"
nasm -f elf $1/main.asm
ld -m elf_i386 $1/main.o -o $1/main
rm $1/main.o

set +e

echo "\e[32m- Running program\e[m"
$1/main
echo "Exit code: $?"

