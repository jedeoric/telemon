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
	LDX $19
	LDA $02AE,X

	.byt $10,$0e ; FIXME
	cmp #$88
	.byt $b0,$0a ; bcs FIXME
	tax
	ldy #$40
	jsr Lc81c
	sta $1d
	.byt $90,$06,$e6,$19,$c6,$1a,$d0,$e5,$68
	.byt $a8,$68,$aa,$a5,$1d
.)	
	rts
	
	