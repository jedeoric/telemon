XRD0_ROUTINE
	lda #0
	.byt $2c
XRD1_ROUTINE	
	lda #4
	.byt $2c
XRD2_ROUTINE		
	lda #8
	.byt $2c	
XRD3_ROUTINE		
	lda #$0c

Lc7da
.(
	STA $19
	LDA #$04
	STA $1A
	TXA
	PHA
	TYA
	PHA
LC7E4	
	LDX $19
	LDA $02AE,X ; FIXME
	bpl LC7F9

	cmp #$88
	bcs  LC7F9

	tax
	ldy #$40
	jsr Lc81c
	sta $1d
	bcc LC7FF 
LC7F9	
	inc $19
	dec $1a
	bne LC7E4 
LC7FF	
	pla
	tay
	pla
	tax
	lda $1d

.)	
	rts
	
	