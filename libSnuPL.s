	.data
ReadInt.str:
	.asciz	"%d"
ReadLong.str:
	.asciz	"%lld"
WriteLong.str:
	.asciz	"%lld"
WriteStr.str:
	.asciz	"%s"

	.text
	.globl ReadInt
ReadInt:
	sub	sp, sp, #16
	adrp	x0, ReadLong.str
	add	x0, x0, :lo12:ReadLong.str
	add	x1, sp, #4
	stp	x29, x30, [sp, #-16]!
	mov	x29, sp
	bl	scanf
	ldp	x29, x30, [sp], #16
	ldrsw	x0, [sp, #4]
	add	sp, sp, #16
	ret

	.globl ReadLong
ReadLong:
	sub	sp, sp, #16
	adrp	x0, ReadLong.str
	add	x0, x0, :lo12:ReadLong.str
	add	x1, sp, #8
	stp	x29, x30, [sp, #-16]!
	mov	x29, sp
	bl	scanf
	ldp	x29, x30, [sp], #16
	ldr	x0, [sp, #8]
	add	sp, sp, #16
	ret

	.globl WriteLong
WriteLong:
	mov	x1, x0
	adrp	x0, WriteLong.str
	add	x0, x0, :lo12:WriteLong.str
	b	printf

	.globl WriteInt
WriteInt:
	b	WriteLong

	.globl WriteChar
WriteChar:
	b	putchar

	.globl WriteStr
WriteStr:
	add	x1, x0, #8
	adrp	x0, WriteStr.str
	add	x0, x0, :lo12:WriteStr.str
	b	printf

	.globl	WriteLn
WriteLn:
	mov	x0, #10
	b	putchar

	.globl DIM
DIM:
	add	x1, x1, #-1
	lsl	x1, x1, #3
	add	x9, x0, x1
	add	x8, x9, #4
	ldrsw	x8, [x8]
	ldrsw	x9, [x9]
	sdiv	x0, x9, x8
	ret
