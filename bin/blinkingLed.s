.text
main:
	li a0, 0
    lui x11, 0x1e8
    addi x11, x11, 1152

toggle:
    beqz a2, setToOne
    li a2, 0
    j loop
setToOne:
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
    j toggle
	li a7, 10
	ecall
