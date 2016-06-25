
;XDECAL_ROUTINE
XFIXME_ROUTINE
	ldx #0
	.byt $2c
XFIXME1_ROUTINE	
	ldx #4
	.byt $2c
XFIXME2_ROUTINE		
	ldx #$08
	.byt $2c
XFIXME3_ROUTINE
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
	beq Lc7a7

	JSR XWSTR0_re_enter_from_XDECAL
	INC $15
	bne loop500
	INC $16
	bne loop500
	
