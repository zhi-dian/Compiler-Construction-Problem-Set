	.text
	.align	2
	.globl printInt
	.globl fish_main
	.globl main

main:
	move $s8, $31
	jal fish_main
	move $31, $s8
	move $a0, $2
	j printInt

printInt:
	add $t0, $v0, $zero
	li $v0, 1
	syscall
	add $v0, $t0, $zero
	jr $ra

fish_main:
	li	$2, 0x0
	la	$3, varx
	sw	$2, 0($3)
	li	$2, 0x1
	la	$3, vara
	sw	$2, 0($3)
	j L1
L2:
	li	$2, 0x1
	la	$3, varb
	sw	$2, 0($3)
	j L5
L6:
	li	$2, 0x1
	la	$3, varc
	sw	$2, 0($3)
	j L9
L10:
	la	$2, varx
	lw	$2, 0($2)
	la	$3, T13
	sw	$2, 0($3)
	la	$2, varc
	lw	$2, 0($2)
	la	$3, T13
	lw	$3, 0($3)
	add	$2, $2, $3
	la	$3, varx
	sw	$2, 0($3)
	la	$2, varc
	lw	$2, 0($2)
	la	$3, T12
	sw	$2, 0($3)
	li	$2, 0x1
	la	$3, T12
	lw	$3, 0($3)
	add	$2, $2, $3
	la	$3, varc
	sw	$2, 0($3)
L9:
	la	$2, varc
	lw	$2, 0($2)
	la	$3, T11
	sw	$2, 0($3)
	la	$2, varb
	lw	$2, 0($2)
	la	$3, T11
	lw	$3, 0($3)
	sle	$2, $3, $2
	bne	$2, $0, L10
	la	$2, varb
	lw	$2, 0($2)
	la	$3, T8
	sw	$2, 0($3)
	li	$2, 0x1
	la	$3, T8
	lw	$3, 0($3)
	add	$2, $2, $3
	la	$3, varb
	sw	$2, 0($3)
L5:
	la	$2, varb
	lw	$2, 0($2)
	la	$3, T7
	sw	$2, 0($3)
	la	$2, vara
	lw	$2, 0($2)
	la	$3, T7
	lw	$3, 0($3)
	sle	$2, $3, $2
	bne	$2, $0, L6
	la	$2, vara
	lw	$2, 0($2)
	la	$3, T4
	sw	$2, 0($3)
	li	$2, 0x1
	la	$3, T4
	lw	$3, 0($3)
	add	$2, $2, $3
	la	$3, vara
	sw	$2, 0($3)
L1:
	la	$2, vara
	lw	$2, 0($2)
	la	$3, T3
	sw	$2, 0($3)
	li	$2, 0xA
	la	$3, T3
	lw	$3, 0($3)
	sle	$2, $3, $2
	bne	$2, $0, L2
	la	$2, varx
	lw	$2, 0($2)
	jr	$31


	.data
	.align 0
T11:	.word 0
T12:	.word 0
T13:	.word 0
T3:	.word 0
T4:	.word 0
T7:	.word 0
T8:	.word 0
vara:	.word 0
varb:	.word 0
varc:	.word 0
varx:	.word 0

