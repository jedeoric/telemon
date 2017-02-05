.(
	pha 
	and #$0f
	jsr Lce60 
	tay
	pla
	lsr
	lsr
	lsr
	lsr
Lce60
	ora #$30
	cmp #$3a
	bcc skip
	adc #$6
skip
	rts
.)
