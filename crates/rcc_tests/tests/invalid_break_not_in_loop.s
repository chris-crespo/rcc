    .global main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $4, %rsp
    movl $1, %r11d
    cmpl $0, %r11d
    je .L0
    jmp .L0
.L0:
    movl $0, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
    .section .note.GNU-stack,"", @progbits

