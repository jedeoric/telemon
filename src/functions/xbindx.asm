; AY contains the number
; X ...
.(
	STA TR1
	STY TR2
	
	LDA #$00 ; 65c02
	STA TR3
	STA TR4
loop35
	LDA #$FF
	STA TR0
loop32
	INC TR0
	SEC
	LDA TR1
	TAY
	SBC const_10_decimal_low,X 
	STA TR1
	LDA TR2
	PHA
	SBC const_10_decimal_high,X ; 
	STA TR2
	PLA
	BCS loop32
	STY TR1
	STA TR2
	LDA TR0
	BEQ loop9
	STA TR3
	BNE loop34+1
loop9
	LDY TR3
	BNE loop34+1
	LDA DEFAFF
loop34
	.byt $2c
	ora #$30	

	JSR lce32 
	DEX
	BPL loop35
	LDA TR1
	ORA #$30
lce32	
	LDY TR4

	STA (TR5),Y
	INC TR4
	RTS
.)
