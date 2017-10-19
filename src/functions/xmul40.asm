.(
	ldy #$00
	sta RES
	sty RES+1
	asl
	rol RES+1
	asl
	rol RES+1
	adc RES
	bcc skip
	inc RES+1
skip
	asl
	rol RES+1
	asl
	rol RES+1
	asl
	rol RES+1
	sta RES
	ldy RES+1
	rts
.)

