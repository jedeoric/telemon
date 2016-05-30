XDECAL_ROUTINE
Lc7a8
	ldx #0
	.byt $2c
	ldx #4
	.byt $2c
	ldx #$08
	.byt $2c
	ldx #$0c
	STX $1C
	STA $15
	STY $16
Lc7b9
loop500
	LDA $1C
	STA $19

	LDY #$00
	JSR $0411
	.byt $f0,$e3
	JSR XWSTR0_re_enter_from_XDECAL	; FIXME
	INC $15
	bne loop500
	INC $16

	bne loop500

	
	
	
	