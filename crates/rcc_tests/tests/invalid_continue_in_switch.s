    .global main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $8, %rsp
    movl $1, %r11d
    cmpl $1, %r11d
    movl $0, -4(%rbp)
    sete -4(%rbp)
    cmpl $0, -4(%rbp)
    jne .L1
    jmp .L0
.L1:
    jmp .L0
.L0:
    movl $0, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
    .section .note.GNU-stack,"", @progbits

