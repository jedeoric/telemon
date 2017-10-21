XSCHAR_ROUTINE
.(
	sta HRS3
	sty HRS3+1
	stx HRS2
	lda #$40
	sta HRSFB
	ldy #$00
skip
	sty HRS2+1
	cpy HRS2
	bcs Lea92 
	lda (HRS3),y
	jsr LEAB5 
	ldy HRS2+1
	iny
	bne skip
.)
