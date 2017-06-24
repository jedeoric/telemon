	
; [IN] AY contains the length to read
; [IN] PTR_READ_DEST must be set because it's the ptr_dest
; [IN] TR0 contains the fd id 

.(
	jsr _ch376_set_bytes_read
continue	
	cmp #CH376_USB_INT_DISK_READ; something to read
	beq readme
	cmp #CH376_USB_INT_SUCCESS ; finished
	beq finished 
	; TODO  in A : $ff X: $ff
	lda #0
	tax
	rts
readme
	jsr we_read

	;jmp end_cat
	lda #CH376_BYTE_RD_GO
	sta CH376_COMMAND
	jsr _ch376_wait_response
	jmp continue
finished
	jsr we_read
	; TODO  return bytes read
	;lda ptr1
	;lda #<8000
	;ldx ptr1+1
	;ldx #>8000
	rts	

we_read
	lda #CH376_RD_USB_DATA0
	sta CH376_COMMAND

	lda CH376_DATA ; contains length read
	sta TR0; Number of bytes to read

	ldy #0
loop9
	lda CH376_DATA ; read the data
	sta (PTR_READ_DEST),y

	iny
	cpy TR0
	bne loop9
	tya
	clc
	adc PTR_READ_DEST
	bcc next13
	inc PTR_READ_DEST+1
next13
	sta PTR_READ_DEST
	rts
.)

