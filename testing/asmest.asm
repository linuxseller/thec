format ELF64
section '.text' executable
public _start
extrn printf
_start:
  call main
.end:
  mov rdi, rax
  mov rax, 60
  syscall
main:
  push rbp
  mov rbp, rsp
  mov rdi, L1
  mov rsi, 10
  call printf
  mov rdi, L2
  call printf
  pop rbp
  mov rax, 0
  ret
L1: db "Hello Worl%d!", 10, "", 0
L2: db "Bye World!", 10, "", 0
