    .global main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $8, %rsp
    movl $0, -4(%rbp)
.L0:
    movl $1, -4(%rbp)
.L0:
    movl $2, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
    movl $0, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
    .section .note.GNU-stack,"", @progbits

