format ELF64 executable

_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, hello
    mov rdx, 7
    syscall

    mov rax, 60
    mov rdi, 69
    syscall

hello: db "BomBom", 10
