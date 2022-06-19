	.data
WriteLong.str:
	.asciz	"%ld"
WriteStr.str:
	.asciz	"%s"

	.text
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
