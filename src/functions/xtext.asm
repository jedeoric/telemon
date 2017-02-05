.(
	lda FLGTEL
	bpl skip ; already in text mode
	php 
	
	sei
	and #$7f
	sta FLGTEL
	jsr move_chars_hires_to_text 
	lda #$56
	ldy #$02
	ldx #0
	jsr ldefd
	
	lda #$1a
	sta $bfdf
	jsr wait_0_3_seconds
	ldx #$28
	lda #$20

loop	
	sta $bb7f,x
	dex
	bne loop
	jsr XCSSCR_ROUTINE
	plp
skip	
	rts
.)
