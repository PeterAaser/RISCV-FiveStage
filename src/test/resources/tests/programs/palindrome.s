main:
	addi	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	addi	s0,sp,32
	sw	zero,-20(s0)
	li	a5,32
	sw	a5,-24(s0)
	li	a2,7
	li	a1,0
	lw	a0,-20(s0)
	call	isPalindrome
.DEBUG_call1_return:
	mv	a5,a0
	beqz	a5,.L2
	li	a2,15
	li	a1,0
	lw	a0,-24(s0)
	call	isPalindrome
.DEBUG_call2_return:
	mv	a5,a0
	beqz	a5,.L2
	li	a5,1
	j	.L4
.L2:
	li	a5,0
.L4:
	mv	a0,a5
	lw	ra,28(sp)
	lw	s0,24(sp)
	addi	sp,sp,32
	jr	ra
isPalindrome:
	addi	sp,sp,-48
	sw	ra,44(sp)
	sw	s0,40(sp)
	addi	s0,sp,48
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	sw	a2,-44(s0)
	lw	a4,-40(s0)
	lw	a5,-44(s0)
	blt	a4,a5,.L6
	li	a5,1
	j	.L7
.L6:
	lw	a5,-40(s0)
	slli	a5,a5,2
	lw	a4,-36(s0)
	add	a5,a4,a5
	lw	a4,0(a5)
	lw	a5,-44(s0)
	slli	a5,a5,2
	lw	a3,-36(s0)
	add	a5,a3,a5
	lw	a5,0(a5)
	sub	a5,a4,a5
	seqz	a5,a5
	andi	a5,a5,0xff
	sw	a5,-20(s0)
	lw	a5,-20(s0)
	beqz	a5,.CompareFailed
	lw	a5,-40(s0)
	addi	a4,a5,1
	lw	a5,-44(s0)
	addi	a5,a5,-1
	mv	a2,a5
	mv	a1,a4
	lw	a0,-36(s0)
	call	isPalindrome
	mv	a5,a0
	beqz	a5,.CompareFailed
	li	a5,1
	j	.L7
.CompareFailed:
	li	a5,0
.L7:
	mv	a0,a5
	lw	ra,44(sp)
	lw	s0,40(sp)
	addi	sp,sp,48
	jr	ra

#memset 0,  10
#memset 4,  -3
#memset 8,  8
#memset 12, 0
#memset 16, 0
#memset 20, 8
#memset 24, -3
#memset 28, 10


#memset 32, 10
#memset 36, -3
#memset 40, 8
#memset 44, 0
#memset 48, 0
#memset 52, 10

#memset 56, -3
#memset 60, 8

#memset 64, -3
#memset 68, 8

#memset 72, 10
#memset 76, 0
#memset 80, 0
#memset 84, 8
#memset 88, -3
#memset 92, 10
