
/// A contains channel
XOP0_ROUTINE
	ldx #$00 ; Channel 0
	.byt $2c ; Jump 2 next bytes
XOP1_ROUTINE
	ldx #$04 ; Channel 1
	.byt $2c ; Jump 2 next bytes
XOP2_ROUTINE	
	ldx #$08 ; Channel 2
	.byt $2c ; Jump 2 next bytes
XOP3_ROUTINE	
	ldx #$0c
	pha
Lc6f1	
	pla
	cmp $02ae,x
	beq Lc704 
	ldy $02ae,x
	bpl Lc705
	inx
	pha
	txa
	and #$03
	bne Lc6f1
	pla
Lc704	
	rts
Lc705

	ldy #$0f
Lc707	
	cmp $2ae,y
	beq Lc71b
	dey

	bpl Lc707
	stx $19
	pha

	ldy #$80
	tax
	
	jsr Lc81c ; FIXME

	ldx $19
	pla
Lc71b	
	sta $02AE,x
	clc
	rts