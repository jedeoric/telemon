XWR0_ROUTINE
	lda #0
	.byt $2c
XWR1_ROUTINE
	lda #4
	.byt $2c
XWR2_ROUTINE
	lda #8
	.byt $2c
XWR3_ROUTINE	
	lda #$0c
	STA $1B
	LDA $1B
	JSR Lc7da
	.byt $b0,$f9 ;BCS F9 FIXME
	
	SEC
	RTS
	
	