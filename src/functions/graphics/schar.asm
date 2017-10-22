out
  rts

XSCHAR_ROUTINE
.(
	sta     HRS3
	sty     HRS3+1
	stx     HRS2
	lda     #$40
	sta     HRSFB
	ldy     #$00
skip
	sty     HRS2+1
	cpy     HRS2
	bcs     out
	lda     (HRS3),y
	jsr     XCHAR_ROUTINE_PUT
	ldy     HRS2+1
	iny
	bne     skip
.)
