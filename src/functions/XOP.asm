
/// A contains channel
XOP0_ROUTINE
	ldx #$00 ; Channel 0
	.byt $2c ; Jump 2 next bytes
XOP1_ROUTINE
	ldx #$04 ; Channel 1
	.byt $2c ; Jump 2 next bytes
XOP2_ROUTINE	
	ldx #$08 ; Channel 2
	.byt $2c ; Jump 2 next bytes
XOP3_ROUTINE	
	ldx #$0c
	pha
.(
  .(	  
  loop
    pla
    cmp     IOTAB0,x
    beq     skip2
    ldy     IOTAB0,x
    bpl     skip
    inx
    pha
    txa
    and     #$03
    bne     loop
    pla
  skip2	
    rts
  .)  
skip
  .(
    ldy     #$0f
  loop	
    cmp     IOTAB0,y
    beq     skip2
    dey

    bpl     loop
    stx     work_channel
    pha

    ldy     #$80
    tax
    
    jsr     Lc81c 

    ldx     work_channel
    pla
  skip2	
    sta     IOTAB0,x
    clc
    rts
  .)  
.)
