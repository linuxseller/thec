format ELF64
section '.text' executable
public _start
extrn printf
_start:
  call main
.end:
  mov rax, 60
  mov rdi, 69
  syscall

main:
  push rbp
  mov rbp, rsp
  mov rdi, L1
  call printf
  mov rdi, L2
  call printf
  pop rbp
  mov rax, 0
  ret
L1: db "Hello World!", 10, "", 0
L2: db "Bye World!", 10, "", 0
