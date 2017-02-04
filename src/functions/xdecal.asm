
; This routine could be replace with a simple routine in 65C816 mode

.(
	pha
	txa
	pha 
	tya
	pha
	sec
	lda DECFIN
	sbc DECDEB
	tay
	lda DECFIN+1
	sbc DECDEB+1
	tax
	bcc Lcdb9 
	stx $0b

	lda DECCIB
	cmp DECDEB
	lda DECCIB+1
	sbc DECDEB+1
	bcs Lcdbf 
	tya
	eor #$ff
	adc #1
	tay 
	sta $0a
	bcc Lcd97
	dex
	inc DECFIN+1
Lcd97	
	sec
	lda DECCIB
	sbc $0a
	sta DECCIB
	bcs Lcda2

	dec DECCIB+1
Lcda2	
	clc
	lda DECFIN+1
	sbc $0b
	sta DECFIN+1
	inx
Lcdaa	
	lda (DECFIN),y
	sta (DECCIB),y
	iny 
	bne Lcdaa
	inc DECFIN+1
	inc DECCIB+1
	dex
	bne	Lcdaa
Lcdb8	
	sec
Lcdb9	
	pla
	tay
	pla
	tax
	pla
	rts
Lcdbf
	txa
	clc
	adc DECDEB+1
	sta DECDEB+1
	txa
	clc
	adc DECCIB+1
	sta DECCIB+1
	inx
Lcdcc	
	dey
	lda (DECDEB),y
	sta (DECCIB),y
	tya
	bne Lcdcc
	dec DECDEB+1
	dec DECCIB+1
	dex
	bne Lcdcc
	beq Lcdb8
.)
