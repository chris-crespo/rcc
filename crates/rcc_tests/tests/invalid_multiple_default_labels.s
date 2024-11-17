    .global main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $4, %rsp
    jmp .L2
.L2:
.L2:
.L0:
    movl $0, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
    .section .note.GNU-stack,"", @progbits

