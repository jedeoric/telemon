_ch376_verify_SetUsbPort_Mount
.(
#ifdef DEBUG_CH376
	lda #<str_detecting
	ldy #>str_detecting
	BRK_TELEMON(XWSTR0)
#endif	
	jsr _ch376_check_exist
	cmp #CH376_DETECTED
	beq detected
#ifdef SEDORIC_SD
  lda #$01
#else
	lda #<str_usbdrive_controller_not_found
	ldy #>str_usbdrive_controller_not_found
	BRK_TELEMON(XWSTR0)
#endif	

#ifdef DEBUG_CH376
	lda #<str_failed_message
	ldy #>str_failed_message
	BRK_TELEMON(XWSTR0)
#endif
	; let's start reset
	jsr _ch376_reset_all
	lda #1 ; error
	rts	
detected	

#ifdef DEBUG_CH376
	lda #<str_ok_message
	ldy #>str_ok_message
	BRK_TELEMON(XWSTR0)
	/***************************** configuring USB PORT*/	
	lda #<str_configure_usb_port
	ldy #>str_configure_usb_port
	BRK_TELEMON(XWSTR0)
#endif
	jsr _ch376_set_usb_mode
#ifdef DEBUG_LS
	lda #<str_ok_message
	ldy #>str_ok_message
	BRK_TELEMON(XWSTR0)
	
	/***************************** mouting drive*/	

	lda #<str_mounting
	ldy #>str_mounting
	BRK_TELEMON(XWSTR0)
	BRK_TELEMON(XCRLF)
#endif
	jsr _ch376_disk_mount
	cmp #CH376_USB_INT_SUCCESS
	beq ok
	clc
	lda #1
	
#ifdef CH376_ERROR_VERBOSE	
	PRINT(str_drive_error)
#endif
ok
	sec ; Carry = 1
	lda #0
	rts
#ifdef CH376_ERROR_VERBOSE
str_drive_error
	.asc "Impossible to mount key",0
#endif
.)

