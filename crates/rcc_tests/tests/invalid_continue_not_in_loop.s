    .global main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $4, %rsp
    jmp .L0
    movl $0, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
    movl $0, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
    .section .note.GNU-stack,"", @progbits

