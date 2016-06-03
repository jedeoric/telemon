PING_DATA	
	.byt $18,$00,$00,$00,$00,$00,$00,$3e,$10,$00,$00,$00,$0f,$00
SHOOT_DATA
	.byt $00,$00
	.byt $00,$00,$00,$00,$0f,$07,$10,$10,$10,$00,$08,$00
EXPLODE_DATA
	.byt $00,$00,$00,$00
	.byt $00,$00,$1f,$07,$10,$10,$10,$00,$18,$00
ZAP_DATA	
	.byt $00,$00,$00,$00,$00,$00
	.byt $00,$3e,$0f,$00,$00,$00,$00,$00,$00
PING_ROUTINE
	ldx #<PING_DATA 
	ldy #>PING_DATA 
	bne Lebe9
SHOOT_ROUTINE
	ldx #<SHOOT_DATA
	ldy #>SHOOT_DATA
	bne Lebe9	
EXPLODE_ROUTINE
	ldx #<EXPLODE_DATA
	ldy #>EXPLODE_DATA
Lebe9	
	jmp send_14_paramaters_to_psg 
	
ZAP_ROUTINE	
	ldx #<ZAP_DATA
	ldy #>ZAP_DATA
	jsr send_14_paramaters_to_psg
	lda #00
	tax
Lebf6
	txa
	pha
	lda #$0
	jsr lda1a  
	ldx #0
Lebff	
	dex
	bne Lebff 
	pla
	tax
	inx
	cpx #$70
	bne Lebf6 
	lda #08
	ldx #00
	jmp lda1a
	
	