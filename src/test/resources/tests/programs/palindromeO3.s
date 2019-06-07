main:
	addi	sp,sp,-16
	li	a2,7
	li	a1,0
	li	a0,0
	sw	ra,12(sp)
	call	isPalindrome.part.0
.DEBUG_call1_return:
	bnez	a0,.L17
isPalindrome.part.0:
	slli	a4,a1,2
	slli	a3,a2,2
	add	a4,a0,a4
	add	a5,a0,a3
	lw	a4,0(a4)
	lw	a5,0(a5)
	beq	a4,a5,.L10
	li	a5,0
.L6:
	mv	a0,a5
	ret
.L10:
	addi	a1,a1,1
	addi	a2,a2,-1
	li	a5,1
	bge	a1,a2,.L6
	addi	sp,sp,-16
	sw	ra,12(sp)
	call	isPalindrome.part.0
.DEBUG_call2_return:
	lw	ra,12(sp)
	snez	a5,a0
	mv	a0,a5
	addi	sp,sp,16
	jr	ra
.L11:
	lw	ra,12(sp)
	addi	sp,sp,16
	jr	ra
.L17:
	li	a2,15
	li	a1,0
	li	a0,32
	call	isPalindrome.part.0
	snez	a0,a0
	j	.L11
isPalindrome:
	bge	a1,a2,.L20
	slli	a3,a2,2
	slli	a4,a1,2
	add	a5,a0,a3
	add	a4,a0,a4
	lw	a7,0(a4)
	lw	a6,0(a5)
	li	a3,0
	bne	a7,a6,.L28
	addi	a7,a1,1
	addi	a6,a2,-1
	li	a3,1
	bge	a7,a6,.L28
	lw	a7,4(a4)
	lw	a6,-4(a5)
	li	a3,0
	bne	a7,a6,.L28
	addi	a7,a1,2
	addi	a6,a2,-2
	li	a3,1
	bge	a7,a6,.L28
	lw	a4,8(a4)
	lw	a5,-8(a5)
	li	a3,0
	bne	a4,a5,.L28
	addi	a1,a1,3
	addi	a2,a2,-3
	li	a3,1
	bge	a1,a2,.L28
	addi	sp,sp,-16
	sw	ra,12(sp)
	call	isPalindrome.part.0
	lw	ra,12(sp)
	snez	a3,a0
	mv	a0,a3
	addi	sp,sp,16
	jr	ra
.L20:
	li	a3,1
.L28:
	mv	a0,a3
	ret


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
