main:
	addi x1, zero, 4
	addi x2, zero, 4
	addi x3, zero, 4 
	addi x4, zero, 4 
	lw x3, 0(x3)
	nop
	nop
	lw x3, 0(x3)
	nop
	nop
	lw x3, 0(x3)
	nop
	nop
	lw x3, 0(x3)
	nop
	nop
	lw x2, 0(x2)
	nop
	lw x2, 0(x2)
	nop
	lw x2, 0(x2)
	nop
	lw x2, 0(x2)
	nop
	lw x1, 0(x1)
	lw x1, 0(x1)
	lw x1, 0(x1)
	lw x1, 0(x1)
	done
#memset 0x0,  4
#memset 0x4,  8
#memset 0x8,  12
#memset 0xc,  16
#memset 0x10, 20
