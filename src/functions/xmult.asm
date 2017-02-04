.(
	sta $10
	sty $11
	ldx #00
	stx TR0
	stx TR1
	stx $0e
	stx $0f
	stx RESB
	stx RESB+1
	ldx #$10
LCEAB	
	lsr $11
	ror $10
	bcc LCECA
	clc
	
	lda RES
	adc TR0
	sta TR0
	
	lda RES+1
	adc TR1
	sta TR1
	
	lda RESB
	adc $0e
	sta $0e
	
	lda RESB+1
	adc $0f
	sta $0f
LCECA
	asl RES
	rol RES+1
	rol RESB
	rol RESB+1
	
	lda TR4
	ora $11
	beq Lcedb
	dex
	bne LCEAB 
Lcedb	
	rts
.)
