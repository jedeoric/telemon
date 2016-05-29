send_A_to_video_screen

	STA $3E
	TYA
	PHA
	TXA
	PHA
	LDA $39
	BEQ Ld18a
	
	STA $0281
	LDA $38
	STA $0280

Ld18a
	BIT $3C
	BMI Ld1ed
	LDA $3E
	JSR send_a_data_to_videotex_screen 
	STA $00
	BEQ Ld1e6
	CMP #$20
	BCC manage_control_code 
	CMP #$A0
	bcs Ld1e1
	STA $0285
	AND #$7F
	TAX
	LDA $34
	BMI Ld1bb
	LDY $38
	CPY #$27
	
	BNE Ld1b1
	AND #$df
Ld1b1
	LDY $39
	CPY #$02
	BCS Ld1bb
	AND #$EF
	STA $34
Ld1bb
	PHA
	JSR Ld530 
	JSR $D5DC ; FIXME
	PLA
	PHA
	BMI Ld1d0
	AND #$20
	beq  Ld1d0
	JSR $D391 ; FIXME
	JSR $D759 ; FIXME
Ld1d0
	JSR $D391 ; FIXME
	PLA	
	bmi Ld1e1	
	LDX $38
	BNE Ld1e1
	AND #$10
	BEQ Ld1e1
	JSR $D3A0 ; FIXME
Ld1e1
	LDA $0285
	STA $00
Ld1e6	
	PLA
	TAX
	PLA
	TAY
	LDA $00
	RTS

/* 102 Bytes end */
