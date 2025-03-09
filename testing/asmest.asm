format ELF64
section '.data' executable
hello: db "BomBom", 10

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
  mov rdi, hello
  call printf
  pop rbp
  mov rax, 0
  ret
