




//@set filename, input : A and Y adress of the string, terminated by 0
_ch376_set_file_name
.(
#ifdef DEBUG_CH376
	lda #<str_setting_filename
	ldy #>str_setting_filename
	BRK_TELEMON(XWSTR0)
	
	lda #<str_ok_message
	ldy #>str_ok_message
	BRK_TELEMON(XWSTR0)	
#endif

	; VARAPL
	lda #CH376_SET_FILE_NAME ;$2f
	sta CH376_COMMAND
	ldx #0
loop	
	lda BUFNOM,x ; replace by bufnom
	beq end ; we reached 0 value
//	sta $bb80,x
	BRK_TELEMON(XMINMA)
	//sta $bb80+40,x
	sta CH376_DATA
	inx
	cpx #11 ; because we don't manage longfilename shortname =11
	bne loop
	
end	
	sta CH376_DATA
.)	
	rts

_ch376_file_open
	.(
#ifdef DEBUG_CH376
	lda #<str_fileopen
	ldy #>str_fileopen
	BRK_TELEMON(XWSTR0)
	
	lda #<str_ok_message
	ldy #>str_ok_message
	BRK_TELEMON(XWSTR0)	
#endif
	
	lda #CH376_FILE_OPEN ; $32
	sta CH376_COMMAND
	jsr _ch376_wait_response
	.)
	rts

	;CMD_GET_FILE_SIZE
	
_ch376_get_file_size
.(
	lda #CH376_GET_FILE_SIZE
	sta CH376_COMMAND
	lda #$68
	sta CH376_DATA
	; store file leng
	lda CH376_DATA
	sta TR0
	lda CH376_DATA
	sta TR1
	lda CH376_DATA
	sta TR2
	lda CH376_DATA
	sta TR3
	
.)
	rts
	
_ch376_reset_all:
.(
	lda #CH376_RESET_ALL ; 5 
	sta CH376_COMMAND
	; waiting
	ldy #0
	ldx #0
loop
	nop
	inx
	bne loop
	iny
	bne loop
.)
	rts
	
_ch376_check_exist
	lda #CH376_CHECK_EXIST ; 
	sta CH376_COMMAND
	lda #CH376_DISK_RD_GO
	sta CH376_COMMAND
	lda CH376_DATA
	rts

_ch376_ic_get_ver:
	lda #CH376_GET_IC_VER
	sta CH376_COMMAND
	lda CH376_DATA
	and #%00111111 ; A contains revision
	clc
	adc #$30 ; return ascii version
	rts
	
_ch376_set_usb_mode
	lda #CH376_SET_USB_MODE ; $15
	sta CH376_COMMAND
	lda #CH376_SET_USB_MODE_CODE_USB_HOST_SOF_PACKAGE_AUTOMATICALLY
	sta CH376_DATA
	rts

_ch376_set_bytes_read
	; A and Y contains number of bytes to read
	ldx #CH376_BYTE_READ
	stx CH376_COMMAND
	sta CH376_DATA
	sty CH376_DATA
	jsr _ch376_wait_response
	rts
	
_ch376_disk_mount
	lda #CH376_DISK_MOUNT ; $31
	sta CH376_COMMAND
	jsr _ch376_wait_response
	; if we read data value, we have then length of the volume name
	rts	

_ch376_wait_response
.(
; 1 return 1 if usb controller does not respond
; else A contains answer of the controller

#ifdef DEBUG_CH376
	lda #<str_waiting
	ldy #>str_waiting
	BRK_TELEMON(XWSTR0)
#endif

	ldy #$ff
loop3
	ldx #$ff ; merci de laisser une valeur importante car parfois en mode non debug, le controleur ne répond pas tout de suite
loop
;	lda #"W"
;	 sta $bb80+40,x
#ifdef DEBUG_LS
	tya
	pha
	txa
	pha
	lda #"."
	BRK_TELEMON(XWR0)
	pla
	tax
	pla
	tay
#endif
	lda CH376_COMMAND
	and #%10000000
	cmp #128
	bne no_error
	dex
	bne loop
	dey
	bne loop3
	; error is here

	lda #1 
	rts
.)
no_error
.(
	lda #CH376_GET_STATUS
	sta CH376_COMMAND
	lda CH376_DATA

	cmp #$1d
	beq good_message
	cmp #CH376_USB_INT_SUCCESS
	beq good_message
#ifdef DEBUG_LS
	lda #<str_failed_message
	ldy #>str_failed_message
	BRK_TELEMON(XWSTR0)	
#endif
	rts
good_message
#ifdef DEBUG_LS
	pha
	lda #<str_ok_message
	ldy #>str_ok_message
	BRK_TELEMON(XWSTR0)
	pla
#endif
.)
	rts
str_usbdrive_controller_not_found
	.asc "Usb drive controller not found !",0

