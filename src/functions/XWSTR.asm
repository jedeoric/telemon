XWSTR0_ROUTINE
	PHA
	LDA #$00
	BEQ next37
XWSTR1_ROUTINE	
	PHA
	LDA #$04
	BNE next37
XWSTR2_ROUTINE
	PHA
	LDA #$08
	BNE next37
XWSTR3_ROUTINE	
	PHA
	LDA #$0C
next37
	STA $19
	PLA
XWSTR0_re_enter_from_XDECAL	
	STA $1B
	LDA #$04
	STA $1A
	TXA
	PHA
	TYA
	PHA
Lc77c	
next39
	LDX $19
	LDA IOTAB0,X
	CMP #$88
	BCC next38
	ASL
	TAX
	LDA ADIOB,X
	STA $02F8
	LDA $02BF,X
	STA $02F9 ; FIXME
	LDA $1B
	BIT $C795 ; FIXME
	JSR $02F7 ; FIXME
Lc79b	
next38
	INC $19
	DEC $1A
	BNE next39
	PLA
	TAY
	PLA
	TAX
	LDA $1B
	RTS
	