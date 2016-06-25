XADRES_ROUTINE
; RES +AY = RES and AY
	clc
	adc RES
	sta RES
	pha
	tya
	adc RES+1
	sta RES+1
	tay
	pla
	rts
	