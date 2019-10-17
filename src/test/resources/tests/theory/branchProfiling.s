main:
	addi	sp,sp,-16
	sw	ra,12(sp)
	call	run
	lw	ra,12(sp)
	addi	sp,sp,16
	jr	ra
rem:
	bge	a0,a1,.L7
	ret
.L7:
	addi	sp,sp,-16
	sw	ra,12(sp)
	sub	a0,a0,a1
	call	rem
	lw	ra,12(sp)
	addi	sp,sp,16
	jr	ra
f1:
	addi	sp,sp,-16
	sw	ra,12(sp)
	sw	s0,8(sp)
	sw	s1,4(sp)
	sw	s2,0(sp)
	li	s1,0
	li	s2,241
	j	.L9
.L11:
	mv	a0,s0
.L9:
	addi	s0,a0,-1
	blez	a0,.L8
	beq	s0,s2,.L8
	li	a1,10
	mv	a0,s0
	call	rem
	bnez	a0,.L11
	add	s1,s1,s0
	j	.L11
.L8:
	mv	a0,s1
	lw	ra,12(sp)
	lw	s0,8(sp)
	lw	s1,4(sp)
	lw	s2,0(sp)
	addi	sp,sp,16
	jr	ra
f2:
	addi	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	sw	s1,20(sp)
	sw	s2,16(sp)
	sw	s3,12(sp)
	sw	s4,8(sp)
	mv	s3,a0
	li	s2,0
	li	s0,0
	li	s4,3
.L15:
	sub	a0,s3,s0
	call	f1
	mv	s1,a0
	add	a0,s0,s3
	call	f1
	add	a0,s1,a0
	add	s2,s2,a0
	addi	s0,s0,1
	bne	s0,s4,.L15
	mv	a0,s2
	lw	ra,28(sp)
	lw	s0,24(sp)
	lw	s1,20(sp)
	lw	s2,16(sp)
	lw	s3,12(sp)
	lw	s4,8(sp)
	addi	sp,sp,32
	jr	ra
f3:
	addi	sp,sp,-16
	sw	ra,12(sp)
	sw	s0,8(sp)
	sw	s1,4(sp)
	mv	s0,a0
	li	a1,10
	call	rem
	beqz	a0,.L23
	li	a1,20
	mv	a0,s0
	call	rem
	beqz	a0,.L24
	mv	a0,s0
	call	f1
	mv	s1,a0
	mv	a0,s0
	call	f2
	add	a0,s1,a0
.L18:
	lw	ra,12(sp)
	lw	s0,8(sp)
	lw	s1,4(sp)
	addi	sp,sp,16
	jr	ra
.L23:
	mv	a0,s0
	call	f2
	j	.L18
.L24:
	mv	a0,s0
	call	f1
	j	.L18
getCall:
	addi	sp,sp,-16
	sw	ra,12(sp)
	beqz	a0,.L30
	li	a5,1
	beq	a0,a5,.L31
	mv	a0,a1
	call	f3
.L25:
	lw	ra,12(sp)
	addi	sp,sp,16
	jr	ra
.L30:
	mv	a0,a1
	call	f1
	j	.L25
.L31:
	mv	a0,a1
	call	f2
	j	.L25
run:
	addi	sp,sp,-48
	sw	ra,44(sp)
	sw	s0,40(sp)
	sw	s1,36(sp)
	sw	s2,32(sp)
	sw	s3,28(sp)
	sw	s4,24(sp)
	sw	s5,20(sp)
	sw	s6,16(sp)
	sw	s7,12(sp)
	sw	s8,8(sp)
	li	s1,0
	li	s0,0
	li	s3,0
	li	s7,56
	li	s6,2
	li	s5,3
	li	s4,24
.L35:
	sub	a5,s7,s1
	lw	s8,0(a5)
	sgt	a5,s0,s6
	xori	a5,a5,1
	add	s0,s0,a5
	sub	a5,s0,s5
	snez	a5,a5
	sub	a5,zero,a5
	and	s0,s0,a5
	lw	a1,0(s1)
	mv	a0,s0
	call	getCall
	mv	s2,a0
	mv	a1,s8
	mv	a0,s0
	call	getCall
	sub	a0,s2,a0
	add	s3,s3,a0
	addi	s1,s1,4
	bne	s1,s4,.L35
	mv	a0,s3
	lw	ra,44(sp)
	lw	s0,40(sp)
	lw	s1,36(sp)
	lw	s2,32(sp)
	lw	s3,28(sp)
	lw	s4,24(sp)
	lw	s5,20(sp)
	lw	s6,16(sp)
	lw	s7,12(sp)
	lw	s8,8(sp)
	addi	sp,sp,48
	jr	ra
#memset 0x0,  0x4
#memset 0x4,  0x7
#memset 0x8,  0x3
#memset 0xc,  0x8
#memset 0x10, 0x4
#memset 0x14, 0x22
#memset 0x18, 0x19
#memset 0x1c, 0x8
#memset 0x20, 0x11
#memset 0x24, 0x10
#memset 0x28, 0x9
#memset 0x2c, 0x8
#memset 0x30, 0x7
#memset 0x34, 0x6
#memset 0x38, 0x5
#memset 0x3c, 0x10
