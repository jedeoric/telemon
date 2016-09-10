#include "include/ch376.h"
#include "include/telemon.h"





//@set filename, input : A and Y adress of the string, terminated by 0
_ch376_set_file_name
.(
	; VARAPL
	lda #CH376_SET_FILE_NAME ;$2f
	sta CH376_COMMAND
	ldx #0
loop	
	lda BUFNOM,x ; replace by bufnom
	beq end ; we reached 0 value
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
	lda #CH376_FILE_OPEN ; $32
	sta CH376_COMMAND
	jsr _ch376_wait_response
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

_ch376_disk_mount
	lda #CH376_DISK_MOUNT ; $31
	sta CH376_COMMAND
	jsr _ch376_wait_response
	; if we read data value, we have then length of the volume name
	rts	

_ch376_wait_response
.(
	ldy #255
loop2
	ldx #255
loop
;	lda #"W"
;	 sta $bb80+40,x
#ifdef DEBUG2
	txa
	pha
	BRK_TELEMON(XCRLF)
	lda #<str_waiting
	ldy #>str_waiting
	BRK_TELEMON(XWSTR0)
	BRK_TELEMON(XCRLF)
	pla
	tax
#endif
	lda CH376_COMMAND
	and #%10000000
	cmp #128
	bne no_error
	dex
	bne loop
	dey
	bne loop2
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
	cmp #$14
	beq good_message
#ifdef DEBUG
	lda #<str_failed_message
	ldy #>str_failed_message
	BRK_TELEMON(XWSTR0)	
#endif
	rts
good_message
#ifdef DEBUG2
	pha
	lda #<str_ok_message
	ldy #>str_ok_message
	BRK_TELEMON(XWSTR0)
	pla
#endif
.)
	rts


