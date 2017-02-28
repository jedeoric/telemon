XPLAY_ROUTINE
	lda HRS2
	asl
	asl
	asl

	ora $4d
	eor #$3f
	tax
	
	lda #7
	jsr XEPSG_ROUTINE 
	asl $53
	rol $54

	ldx $53
	lda #$0b
	jsr XEPSG_ROUTINE
	

	ldx $54
	lda #$0c
	jsr XEPSG_ROUTINE 
	ldy $51
	ldx enveloppes_play_0_to_7,y
	lda #$0d
	jmp XEPSG_ROUTINE 

enveloppes_play_0_to_7
Leb38
	.byt $00,$0b,$04,$08,$0a,$0b,$0c,$0d
periods_note_octave_0


	.byt $00,$00,$ee,$0e,$16,$0e,$4c,$0d,$8e,$0c,$d8,$0b,$2e,$0b,$8e,$0a
	.byt $f6,$09,$66,$09,$e0,$08,$60,$08,$e8,$07
/*End of period*/

XMUSIC_ROUTINE
	LDY $4F
	LDA $51
	ASL
	TAX
	LDA periods_note_octave_0,X 
	STA $4F 
	LDA periods_note_octave_0+1,X 
Leb68	
	LSR
	ROR $4F
	DEY
	BPL Leb68
	STA $50
	LDX $53
	.byt $2c
XSOUND_ROUTINE
	LDX $51
	TXA
	BNE Leb7a
	LDX #$10
Leb7a	
	LDY $4D
	DEY
	TYA
	CMP #$03
	BCC Leb84
	SBC #$03
Leb84	
	ORA #$08
	JSR XEPSG_ROUTINE 	
	CPY #$03
	BCS Leb99
	TYA
	ASL
	TAY
	ADC #$01
	LDX $50
	JSR XEPSG_ROUTINE
	TYA
	.byt $2c
Leb99	
	lda #$06
	
	LDX $4F
	JMP XEPSG_ROUTINE


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
XPING_ROUTINE
	ldx #<PING_DATA 
	ldy #>PING_DATA 
	bne Lebe9
XSHOOT_ROUTINE
	ldx #<SHOOT_DATA
	ldy #>SHOOT_DATA
	bne Lebe9	
XEXPLO_ROUTINE
	ldx #<EXPLODE_DATA
	ldy #>EXPLODE_DATA
Lebe9	
	jmp send_14_paramaters_to_psg 
	
XZAP_ROUTINE	
	ldx #<ZAP_DATA
	ldy #>ZAP_DATA
	jsr send_14_paramaters_to_psg
	lda #00
	tax
Lebf6
	txa
	pha
	lda #$0
	jsr XEPSG_ROUTINE  
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
	jmp XEPSG_ROUTINE
	
	