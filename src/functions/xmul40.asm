.(
Lce69
mult_by_40
	ldy #0
	sta RES
	sty RES+1
	asl
	rol RES+1
	asl
	rol RES+1
	adc RES
	bcc Lce7b
	inc RES+1
Lce7b
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
