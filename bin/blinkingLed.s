.text
main:
	li a0, 0
	li a1, 50000

toggle:
    beqz a2, setToOne
    li a2, 0
    j loop
setToOne:
	nop
    li a2, 1
loop:
	addi a0, a0, 1
	nop
	nop
	nop
	nop
	nop
	nop
    nop
	bne a0, a1, loop
    nop
    add a0, x0, x0
    nop
    nop
    nop
    j toggle
	nop
	li a7, 10
	ecall
