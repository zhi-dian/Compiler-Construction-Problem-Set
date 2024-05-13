	.text
	.align	2
	.globl main
f:
	li	$2, 0x1
	addi	$29, $29, 0x4
	jr	$31
main:
	addi	$29, $29, 0xFFFFFFFC
	sw	$31, 0($29)
	move $s8, $29
	addi	$30, $29, 0x3
	addi	$30, $30, 0xFFFFFFFC
	addi	$29, $30, 0xFFFFFFFD
	jal f
	addi	$30, $30, 0x4
	lw	$31, -3($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	addi	$30, $30, 0xFFFFFFFC
	addi	$29, $30, 0xFFFFFFFD
	jal f
	addi	$30, $30, 0x4
	lw	$31, -3($30)
	lw	$3, 0($29)
	add	$2, $2, $3
	addi	$29, $29, 0x4
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	addi	$30, $30, 0xFFFFFFFC
	addi	$29, $30, 0xFFFFFFFD
	jal f
	addi	$30, $30, 0x4
	lw	$31, -3($30)
	lw	$3, 0($29)
	add	$2, $2, $3
	addi	$29, $29, 0x4
	addi	$29, $29, 0x4
	addi	$4, $2, 0x0
	lw	$31, 0($29)
	j printInt


	.data
	.align 0

#
# below here is the print debugging support code
#
	
.data
_spaceString: .asciiz " "
_newlineString: .asciiz "\n"

.text
.globl printInt     # int reg -> unit
.globl printSpace   # unit    -> unit
.globl printNewline # unit    -> unit

printInt: # int reg->unit
	                  # The syscall takes its argument in $a0
   add $t0, $v0, $zero    # since this function does not return anything, it should probably preserve $v0
   li $v0, 1              # print_int syscall
   syscall
   add $v0, $t0, $zero    # restore $v0 
jr $ra


printSpace: # unit->unit
add $t0, $v0, $zero
la $a0, _spaceString      # address of string to print
li $v0, 4                 # system call code for print_str
syscall                   # print the string
add $v0, $t0, $zero
jr $ra

printNewline: # unit->unit
add $t0, $v0, $zero
la $a0, _newlineString    # address of string to print
li $v0, 4                 # system call code for print_str
syscall                   # print the string
add $v0, $t0, $zero
jr $ra
