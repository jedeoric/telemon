_ch376_file_create
.(
	lda #CH376_CMD_FILE_CREATE
	sta CH376_COMMAND
	jsr _ch376_wait_response
	rts
.)

_ch376_dir_create
.(
  lda     #CH376_DIR_CREATE
  sta     CH376_COMMAND
  jsr     _ch376_wait_response
  rts
.)

_ch376_file_erase
.(
    lda     #CH376_FILE_ERASE
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    rts
.)

; A contains 0 if it needs to update length
_ch376_file_close
	ldx #CH376_FILE_CLOSE
	stx CH376_COMMAND
  lda #$00
  sta CH376_DATA
	jsr _ch376_wait_response
	rts
	
// [IN] AY : ptr
_ch376_seek_file
.(
	ldx #CH376_BYTE_LOCATE
	stx CH376_COMMAND
	sta CH376_DATA
	sty CH376_DATA
#ifdef CPU_65C02
	stz CH376_DATA
	stz CH376_DATA
#else	
	lda #$00
	sta CH376_DATA
	sta CH376_DATA
#endif	
	jsr _ch376_wait_response
	rts
.)



//@set filename, input : A and Y adress of the string, terminated by 0
// If the set is successful, then A contains 0
// [IN]
// [MODIFY] A, X
_ch376_set_file_name
.(
	lda #CH376_SET_FILE_NAME ;$2f
	sta CH376_COMMAND
	ldx #$0
loop	
	lda BUFNOM,x ; replace by bufnom
	beq end ; we reached 0 value
	cmp #"a" ; 'a'
	bcc skip
	cmp #$7b ; 'z'
	bcs skip
	sbc #$1f
skip
	sta CH376_DATA
	inx
	cpx #13 ; because we don't manage longfilename shortname =13 8+3 and dot and \0
	bne loop
  ;lda #$00
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
	rts	
.)
	
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
	lda #$55
	sta CH376_DATA
	;sta CH376_COMMAND
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
	.byt $2c                ; jump 2 bytes with the hack bit $xxxx
_ch376_set_bytes_write	
	ldx #CH376_BYTE_WRITE
	stx CH376_COMMAND
	sta CH376_DATA
	sty CH376_DATA
	; FIXME 65C02
#ifdef CPU_65C02	
	stz CH376_DATA
	stz CH376_DATA
#else
	lda #0
	sta CH376_DATA
	sta CH376_DATA
#endif	
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

	rts
good_message

.)
	rts
str_usbdrive_controller_not_found
	.asc "Usb drive controller not found !",0

