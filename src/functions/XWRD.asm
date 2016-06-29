XRDW0_ROUTINE
	lda #0
	.byt $2c
XRDW1_ROUTINE
	lda #4
	.byt $2c
XRDW2_ROUTINE
	lda #8
	.byt $2c
XRDW3_ROUTINE
.(	
	lda #$0c
	STA $1B
loop	
	LDA $1B
	JSR Lc7da

	bcs  loop	
.)	
LC81A ; Used to table_routine E/S 
ROUTINE_I_O_NOTHING
	SEC
	RTS
	
	