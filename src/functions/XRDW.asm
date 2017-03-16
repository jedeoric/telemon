XRD0_ROUTINE
	lda #0
	.byt $2c
XRD1_ROUTINE	
	lda #4
	.byt $2c
XRD2_ROUTINE		
	lda #8
	.byt $2c	
XRD3_ROUTINE		
	lda #$0c

; read keyboard	
Lc7da
.(
	STA work_channel
	LDA #$04
	STA i_o_counter
	TXA
	PHA
	TYA
	PHA
loop	
	LDX work_channel
	LDA IOTAB0,X ; FIXME
	bpl skip

	cmp #$88
	bcs skip

	tax
	ldy #$40
	jsr send_command_A
	sta $1d
	bcc skip2
skip	
	inc work_channel
	dec i_o_counter
	bne loop
skip2
	pla
	tay
	pla
	tax
	lda $1d

.)	
	rts
	
	