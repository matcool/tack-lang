section .text
global _start

_start:
	call main
	mov ebx, eax
	mov al, 1
	int 0x80 ; exit syscall

; -- generated asm --
