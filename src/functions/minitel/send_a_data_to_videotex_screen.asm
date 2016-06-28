XVDTDA_ROUTINE
send_a_data_to_videotex_screen
	ldy $37 
	bit $37
	php
	ldx #0
	stx $37
	plp
	bmi Ld46a 
	bvs Ld466 
	cmp #$13
	beq Ld45f
	cmp #$19
	beq Ld45c	
	cmp #$16
	bne Ld465	
Ld45c	
	lda #$80
	.byt $2c
Ld45f	
	lda #$40
	sta $37
	lda #$0
Ld465	
	rts
Ld466
	clc
	adc #$5f
	rts
Ld46a
	
	bvs Ld48a
	ldx #$14
Ld46e	
	cmp codes_table_ss2_syn,x

 	beq Ld479	
	dex
	bpl Ld46e
	lda #$5f
	rts
Ld479	
	cpx #5
	bcs Ld485 
	txa
	ora #$c0
	sta $37
	lda #00
	rts
Ld485	
	and #$1f
	ora #$80
	
	rts
Ld48a	
	pha
	tya
	and #7
	tax
	lda number_accent_per_caracter,x 
	tay
	lda position_table,x 
	tax
	pla
Ld498	
	cmp char_sorted_by_accent_category,x 
	beq Ld4a3 
	inx
	dey
	bne  Ld498
	tax
	rts
	

Ld4a3
	lda char_accent,x 
	rts
	
codes_table_ss2_syn

	.byt $41,$42,$43,$48,$4b
	.byt $20,$23,$24,$26
	.byt $2c,$2d,$2e,$2f,$30,$31,$38,$3c,$3d,$3e,$6a,$7a
position_table
	.byt $00,$05,$07,$0e
	.byt $11
number_accent_per_caracter
	.byt $05,$02,$07,$03,$02
	
char_sorted_by_accent_category	
	.byt "A","a","E","e","u","E","e","A","a",$45
	.byt $65,$75,$69,$6f,$45,$65,$69,$43,$63
Ld4d9
	.byt $41,$42,$43,$48,$4b
char_accent	
	.byt $87,$97
	.byt $89,$99,$88,$82,$92,$81,$86,$8b,$9b,$96,$80,$9f,$84,$93,$94,$85
	.byt $95
	

XVDTAH_ROUTINE
Ld4f1
vdt_to_hires

	ldx #00
	cmp #$a0
	
	bcc Ld4ff
	sbc #$5f
	ldy #$13
	rts
Ld4ff	
	tay
	bmi Ld503
	tay
	lda #0
	rts
Ld503	
	ldy #$12
Ld505	
	cmp char_accent,y
	beq  Ld51d
	dey
	bpl  Ld505
	clc
	adc #$a0
	cmp #$2a ; is it "-" ?
	beq Ld518 
	cmp #$3a ; is it '=' ?
	bne Ld51a 
Ld518	
	ora #$40
Ld51a	
	ldy #$19
	rts
Ld51d	
	tya
	ldx #4
Ld520	
	cmp position_table,x
	bcs Ld528
	dex
	bne Ld520
Ld528
	lda Ld4d9,x
	ldx char_sorted_by_accent_category,y
	bne Ld51a

	


put_data_in_videotex_table
Ld530	

	STA $36
	ASL $36
	ASL $36
	TAY
	BPL Ld55f ; 
	PHA
	TXA
	CMP #$60
	BCC Ld541
	SBC #$20
Ld541
	SEC
	SBC #$20
	STA $36
	LDA $33
	AND #$40
	ORA $36
	TAX
	LDA $32
	AND #$70
	STA $36
	PLA
	AND #$8F
	ORA $36
	LDY $38
	STY $36
	JMP Ld5af	
Ld55f	
	CPX #$20
	BNE Ld58e
	BIT $32
	BPL Ld58e
	AND #$70
	STA $35
	LDA $32
	AND #$04
	ORA #$80
	ORA $35
	TAX
	LDA $32
	AND #$74
	STA $32
	AND #$70
	STA $35
	LSR
	LSR
	LSR
	LSR
	BIT $34
	BVC Ld58a
	LDA $34
	AND #$07
Ld58a
	ORA $35
	ORA #$80
Ld58e
	BIT $36
	BVC Ld5a5
	DEC $2F
	DEC $31
	PHA
	SEC
	LDA $38
	SBC #$28
	TAY
	PLA
	JSR Ld5a7
	INC $2F
	INC $31
Ld5a5
	LDY $38
Ld5a7
	JSR Ld5af
	BIT $36
	BPL Ld5b6
	INY
Ld5af
	PHA
	TXA
	STA ($2E),Y
	PLA
	STA ($30),Y
Ld5b6
	RTS

; MINITEL
stop_videotex_emulation
	jsr $cf75 ; FIXME
	jmp $d74d  ; FIXME
	
	

	
	