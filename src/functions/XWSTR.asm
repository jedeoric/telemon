XWSTR0_ROUTINE
	PHA
	LDA #$00
	BEQ next37
	PHA
	LDA #$04
	BNE next37
	PHA
	LDA #$08
	BNE next37
	PHA
	LDA #$0C
next37
	STA $19
	PLA
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
	LDA $02BE,X
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
	