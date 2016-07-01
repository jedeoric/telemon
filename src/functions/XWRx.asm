XWR0_ROUTINE
	PHA
	LDA #$00
	BEQ next37
XWR1_ROUTINE	
	PHA
	LDA #$04
	BNE next37
XWR2_ROUTINE
	PHA
	LDA #$08
	BNE next37
XWR3_ROUTINE	
	PHA
	LDA #$0C
next37
	STA work_channel
	PLA
XWSTR0_re_enter_from_XDECAL	
	STA i_o_save
	LDA #$04
	STA $1A
	TXA
	PHA
	TYA
	PHA
Lc77c	
next39
	LDX work_channel
	LDA IOTAB0,X
	CMP #$88
	BCC next38
	ASL
	TAX
	LDA ADIOB,X
	STA $02F8
	LDA $02BF,X
	STA $02F9 ; FIXME
	LDA i_o_save
LC795	
	BIT LC795 ; FIXME
	JSR $02F7 ; FIXME
Lc79b	
next38
	INC work_channel
	DEC i_o_counter
	BNE next39
	PLA
	TAY
	PLA
	TAX
	LDA $1B
Lc7a7	
	RTS
	
	
	
	