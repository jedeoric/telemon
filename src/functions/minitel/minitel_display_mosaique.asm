minitel_display_mosaique
XVDTG2_ROUTINE
	LDA #$00
	LDX #$03
	LDY $3B
	BEQ LD722
	DEX
	LDA #$03
	DEY
	BEQ LD722
	INX
	LDA #$05
LD722	
	JSR XMUL40_ROUTINE
	LDA $2C
	LDY $2D
	JSR XADRES_ROUTINE
	LDA #$38
	LDY $3A
	BEQ LD734
	LDA #$07
LD734
	STA RESB
	LDY $38
LD738	
	LDA (RES),Y
	ASL
	BMI LD73F
	LDA #$80
LD73F
	ROR
	EOR RESB
	STA (RES),Y
	TYA
	CLC
	ADC #$28
	TAY
	DEX
	BNE LD738
	RTS


