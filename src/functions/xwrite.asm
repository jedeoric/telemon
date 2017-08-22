; [IN] AY contains the length to write
; [IN] PTR_READ_DEST must be set because it's the ptr_dest
; [MODIFIED] TR0,PTR_READ_DEST, YA
.(	
  jsr _ch376_set_bytes_write
  cmp #CH376_USB_INT_SUCCESS ; finished
  beq start_write
  rts
start_write
  lda     #CH376_CMD_WR_REQ_DATA
  sta     CH376_COMMAND

  ldy     CH376_DATA ; contains length of write
  sta     TR0; Number of bytes write

	ldy #0
loop:
  lda     (PTR_READ_DEST),y
  sta     CH376_DATA ; store the data
  iny
  dec     TR0
  bne     loop

	; compute PTR_READ_DEST
	sty     TR0
	lda     PTR_READ_DEST
	clc
	adc     TR0
	bcc     don_t_inc
	inc     PTR_READ_DEST+1
don_t_inc	
  sta     PTR_READ_DEST
  lda     #CH376_BYTE_WR_GO
  sta     CH376_COMMAND
  jsr     _ch376_wait_response
	cmp     #CH376_USB_INT_SUCCESS ; finished
  bne     start_write
	rts	
.)
