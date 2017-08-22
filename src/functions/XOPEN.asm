; Use RES, A X Y TR4 cd 	
; [IN] A and X contains ptr of the string
; [IN] Y contains flag
XOPEN_ROUTINE
.(
	// A and X contains char * pointer ex /usr/bin/toto.txt but it does not manage the full path yet
	sta     RES
	stx     RES+1
	
	sty     TR4 ; save flags
	; check if usbkey is available
	jsr     _ch376_verify_SetUsbPort_Mount
	cmp     #$01
	bne     next
	; impossible to mount
	ldx     #$00
	txa
	rts

next
	
	ldy     #$00
	lda     (RES),y
	;
	cmp     #"/"
	beq     it_is_absolute
	
	; here it's relative
	jsr     XOPEN_ABSOLUTE_PATH_CURRENT_ROUTINE ; Read current path (and open)
	ldy     #$00
	ldx     #$00
	jmp     read_file

	
it_is_absolute	
init_and_go

	lda     #"/"
	sta     BUFNOM

#ifdef CPU_65C02
	ldx     #$00
	stz     BUFNOM+1 ; INIT	
#else	
	ldx     #$0 ; used to write in BUFNOM
	stx     BUFNOM+1 ; INIT	
#endif	

	jsr     open_and_read_go

read_file

loop
	lda     (RES),y
	beq     end
	cmp     #"/"
	bne     next_char
#ifdef     CPU_65C02
	stz     BUFNOM,x
#else
	lda     #$00
	sta     BUFNOM,x
#endif	
	
	jsr     open_and_read_go
	
	cmp     #CH376_ERR_MISS_FILE
	beq     file_not_found 	
	jmp     loop
next_char		
	sta     BUFNOM,x
	iny
	inx
#ifdef CPU_65C02	
	bra     loop
#else
	jmp     loop
#endif
	
not_slash_first_param
	; Call here setfilename
	ldx     #$00 ; Flush param in order to send parameter
	iny
	bne     loop
end
	cpy     #$00
	beq     skip
	sta     BUFNOM,x

	; Optimize, it's crap
	lda     TR4 ; Get flags
	AND     #O_RDONLY
	cmp     #O_RDONLY
	beq     read_only
	lda     TR4
	AND     #O_WRONLY
	cmp     #O_WRONLY
	beq     write_only
	jmp     skip
write_only
	jsr     _ch376_set_file_name
	jsr     _ch376_file_create
	inc     NUMBER_OPENED_FILES
	lda     NUMBER_OPENED_FILES
	rts

read_only	
	jsr     open_and_read_go
	cmp     #CH376_ERR_MISS_FILE
	beq     file_not_found 	
	; cc65 needs everything 
	lda     #$00
	ldx     #$00
	rts


skip
	ldx #$ff
	txa
	rts
open_and_read_go
/******DEBUG ********/
	;lda RES
	;pha
;	lda RES+1
	;pha
/****** END DEBUG ********/
#ifdef CPU_65C02
	phy
#else
	sty TR7
#endif	
	jsr _ch376_set_file_name
	jsr _ch376_file_open

	sta TR6 ; store return 
	;PRINT_INTO_TELEMON(BUFNOM)

	;lda #"-"
	;jsr XWR0_ROUTINE	

	
	ldx #0
#ifdef CPU_65C02
	ply
#else
	ldy TR7 ; because it's "/" in the first char, it means that we are here _/_usr/bin/toto.txt
#endif		
	
	iny
/******DEBUG ********/	
	;pla
	;sta RES+1
	;pla 
	;sta RES
/****** END DEBUG ********/	
	lda TR6 ; GET error of _ch376_file_open return
	rts
file_not_found 
	; return NULL
	lda #"#"
	jsr XWR0_ROUTINE
	ldx #$ff
	lda #$ff
	rts
	
.)

