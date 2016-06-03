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
	