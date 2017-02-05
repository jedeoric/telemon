.(	
	ldx #$00
	ldy #$ff
	sty HRSPAT ; pattern
	iny
	jsr Le7f3 
	lda FLGTEL ; we are already in Hires ?
	bmi XEFFHI_ROUTINE 
	ora #$80
	sta FLGTEL ; Set to Hires flag
	php 
	sei
	lda #$1f
	sta $bf67 
	jsr wait_0_3_seconds 
	jsr move_chars_text_to_hires 
	lda #$5c
	ldy #$02
	ldx #0
	jsr ldefd 
	jsr XEFFHI_ROUTINE 
	plp
	rts
.)

