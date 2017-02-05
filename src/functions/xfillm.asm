.(	
	pha
	sec
	tya
	sbc RES
	tay
	txa
	sbc RES+1
	tax
	sty RESB
	pla
	ldy #0
Lcf23	
	cpy RESB
	bcs Lcf2c
	sta (RES),y
	iny
	bne Lcf23
Lcf2c	
	pha
	tya
	
	ldy #0
	jsr XADRES_ROUTINE
	pla
	cpx #0
	beq Lcf44 
	ldy #0
Lcf3a	
	sta (RES),y
	iny
	bne Lcf3a
	inc RES+1
	dex
	bne Lcf3a
Lcf44	
	rts
.)
