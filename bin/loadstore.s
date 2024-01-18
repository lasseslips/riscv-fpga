.text
nop
li a0, 500
li a1, 0x0ff

lui s2, 0xabcdf
nop
nop
addi s2, s2, -85
li s3, 0x0ea

lui s4, 0xabcdf
nop
nop
addi s4, s4, -85
li s5, 0xfae


lui s6, 0xabcdf
nop
nop
addi s6, s6, -85
li s7, 0x342
nop
nop
nop
sw a0, 0(a1)
sh s2, 0(s3)
sb s4, 0(s5)
sw s6, 0(s7)

nop
nop
nop
nop
lw a4, 0(a1)
lw t0, 0(s3)
lw t1, 0(s5)
lw t2, 0(s7)

lb s8, 0(a1)
lh s9, 0(s3)
lbu s10, 0(s5)
lhu s11, 0(s7)

li a7, 10
nop
nop
ecall
