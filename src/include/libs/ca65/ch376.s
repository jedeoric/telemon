; jede jede@oric.org 2017-01-22

    .export   _ch376_set_file_name
    .export   _ch376_file_open
    .export   _ch376_ic_get_version
    .export   _ch376_reset
    .export   _ch376_check_exist
    .export   _ch376_disk_mount
    .export   _ch376_set_usb_mode
    .export   _ch376_file_close
    .export   _ch376_seek_file
    .export   _ch376_rd_usb_data0
    .export   _ch376_get_entry
    .export   _ch376_file_create	
    .export   _ch376_fwrite
    .export   _ch376_write
    .export   _ch376_dir_create
    .export   _ch376_file_erase
    .export   _ch376_process_next_entry_catalog
    
    .export   _ch376_set_bytes_write

    .import   popax
    .include  "zeropage.inc"
    .include  "telestrat.inc"

CH376_SET_USB_MODE_CODE_USB_HOST_SOF_PACKAGE_AUTOMATICALLY = $06

CH376_USB_INT_SUCCESS	 	    = $14
CH376_USB_INT_CONNECT	 	    = $15
CH376_USB_INT_DISCONNECT	    = $16
CH376_USB_INT_BUF_OVER	 	    = $17
CH376_USB_INT_USB_READY         = $18
CH376_USB_INT_DISK_READ         = $1D
CH376_USB_INT_DISK_WRITE	    = $1E
CH376_USB_INT_DISK_ERR		    = $1F


CH376_ERR_OPEN_DIR          = $41
CH376_ERR_MISS_FILE         = $42
CH376_ERR_FOUND_NAME		    = $43
CH376_ERR_DISK_DISCON		    = $82
CH376_ERR_LARGE_SECTOR		  = $84
CH376_ERR_TYPE_ERROR		    = $92
CH376_ERR_BPB_ERROR         = $A1
CH376_ERR_DISK_FULL         = $B1
CH376_ERR_FDT_OVER          = $B2
CH376_ERR_FILE_CLOSE		    = $B4

CH376_GET_IC_VER            = $01
CH376_SET_BAUDRATE          = $02
CH376_GET_ENTER_SLEEP		    = $03
CH376_RESET_ALL             = $05
CH376_CHECK_EXIST           = $06
CH376_GET_FILE_SIZE         = $0C

CH376_SET_USB_MODE          = $15
CH376_GET_STATUS            = $22
CH376_RD_USB_DATA0          = $27
CH376_CMD_WR_REQ_DATA       = $2D
CH376_SET_FILE_NAME         = $2F

CH376_DISK_CONNECT          = $30 ; check the disk connection status
CH376_DISK_MOUNT            = $31
CH376_FILE_OPEN             = $32
CH376_FILE_ENUM_GO          = $33
CH376_FILE_CREATE           = $34
CH376_FILE_ERASE            = $35
CH376_FILE_CLOSE	 	        = $36
CH376_BYTE_LOCATE           = $39
CH376_BYTE_READ             = $3A
CH376_BYTE_RD_GO            = $3B
CH376_BYTE_WRITE            = $3C
CH376_BYTE_WR_GO            = $3D
CH376_DISK_CAPACITY         = $3E
CH376_DIR_CREATE            = $40
CH376_DISK_RD_GO            = $55

.proc _ch376_file_close
    ldx     #CH376_FILE_CLOSE
    stx     CH376_COMMAND
    sta     CH376_DATA
    jsr     _ch376_wait_response
    rts	
.endproc

.proc _ch376_seek_file
    ldy     #CH376_BYTE_LOCATE
    sty     CH376_COMMAND    
    
    sta     CH376_DATA
    stx     CH376_DATA
    
    lda     #$00                   ; Don't manage 32 bits length
    sta     CH376_DATA
    sta     CH376_DATA

    jsr     _ch376_wait_response
    lda     #CH376_RD_USB_DATA0
    sta     CH376_COMMAND
    lda     CH376_DATA
    sta     TR0
    lda     CH376_DATA
    sta     TR1
    ;lda     CH376_DATA
    ;sta     TR2
    ;lda     CH376_DATA
    ;sta     TR3
    lda     TR0
    ldx     TR1
    
    
    rts
.endproc

; void ch376_set_file_name(char *filename)
.proc _ch376_set_file_name
    sta     ptr1
    stx     ptr1+1
    lda     #CH376_SET_FILE_NAME 
    sta     CH376_COMMAND
    ldy     #0
loop:
    lda     (ptr1),y 
    beq     end                    ; we reached 0 value
    BRK_TELEMON     XMINMA
    sta     CH376_DATA
    iny
    cpy     #13                    ; because we don't manage longfilename shortname =11
    bne     loop
    rts
end:	
    sta     CH376_DATA
    rts
.endproc 
	
; char _ch376_file_open();
.proc _ch376_file_open
    lda     #CH376_FILE_OPEN ; $32
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    ldx     #$00
    rts
.endproc 
	
.proc _ch376_get_file_size
    lda     #CH376_GET_FILE_SIZE
    sta     CH376_COMMAND
    lda     #$68
    sta     CH376_DATA
    ; store file length 32 bits
    lda     CH376_DATA
    sta     tmp1
    lda     CH376_DATA
    sta     tmp1+1
    lda     CH376_DATA
    sta     tmp2
    lda     CH376_DATA
    sta     tmp2+1
    rts
.endproc 

; void ch376_reset();
.proc _ch376_reset
    lda     #CH376_RESET_ALL ; 5 
    sta     CH376_COMMAND
    ; waiting
    ldy     #$00
    ldx     #$00
loop:
    nop
    inx
    bne     loop
    iny
    bne     loop
    rts
.endproc 

; char  ch376_check_exist(char value);
	
.proc _ch376_check_exist
    sta     tmp1
    lda     #CH376_CHECK_EXIST ; 6
    sta     CH376_COMMAND
    lda     tmp1
    sta     CH376_DATA
    lda     CH376_DATA
    rts
.endproc 
	
; char 	ch376_ic_get_version(void)
.proc _ch376_ic_get_version
    lda     #CH376_GET_IC_VER ; 1
    sta     CH376_COMMAND
    ldx     #$00
    lda     CH376_DATA
    rts
.endproc 

; void ch376_set_usb_mode(char mode)
.proc _ch376_set_usb_mode
    ldx     #CH376_SET_USB_MODE ; $15
    stx     CH376_COMMAND
    sta     CH376_DATA
    rts
.endproc 
		
; 	void ch376_set_bytes_write(int value);
.proc _ch376_set_bytes_write
    ldy     #CH376_BYTE_WRITE
    sty     CH376_COMMAND
    sta     CH376_DATA
    stx     CH376_DATA
    lda     #$00
    sta     CH376_DATA
    sta     CH376_DATA
    jsr     _ch376_wait_response
    rts
.endproc 	
	
.proc _ch376_set_bytes_read
    ldy     #CH376_BYTE_READ
    sty     CH376_COMMAND
    ; Storing 32 bits value
    sta     CH376_DATA
    stx     CH376_DATA
    lda     #$00
    sta     CH376_DATA
    sta     CH376_DATA
    jsr     _ch376_wait_response
    rts
.endproc 		

; char 	ch376_disk_mount();
.proc _ch376_disk_mount
    lda     #CH376_DISK_MOUNT ; $31
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    ; if we read data value, we have then length of the volume name
    ldx     #0
    rts	
.endproc 

; char 	ch376_wait_response();
.proc _ch376_wait_response
;  return 1 if usb controller does not respond
; else A contains answer of the controller
    ldy     #$FF ; We have to wait 35 ms, but well, this loop is broken when controler is OK
loop3:
    ldx     #$FF                 ; don't decrease this counter. Because ch376 won't respond if there is a lower value
loop:
    lda     CH376_COMMAND
    and     #%10000000
    cmp     #128
    bne     no_error
    dex
    bne     loop
    dey
    bne     loop3
    ; error is here
    rts
no_error:
    lda     #CH376_GET_STATUS
    sta     CH376_COMMAND
    lda     CH376_DATA
    rts
.endproc 

.proc _ch376_fread
    ; use ptr1 to count bytes
    jsr     _ch376_set_bytes_read
continue:	
    cmp     #CH376_USB_INT_DISK_READ  ; something to read
    beq     we_read
    cmp     #CH376_USB_INT_SUCCESS ; finished
    beq     finished 
    ; TODO  in A : $ff X: $ff
    lda     #0
    tax
    rts
we_read:
    lda     #CH376_RD_USB_DATA0
    sta     CH376_COMMAND

    lda     CH376_DATA ; contains length read
    sta     tmp2; Number of bytes to read

    ldy     #0
loop:
    lda     CH376_DATA ; read the data
    sta     (PTR_READ_DEST),y
    iny
    cpy     tmp2
    bne     loop
    tya
    clc
    adc     PTR_READ_DEST
    bcc     next
    inc     PTR_READ_DEST+1
next:
    sta     PTR_READ_DEST
	
    lda     #CH376_BYTE_RD_GO
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    jmp     continue
finished:
    ; TODO  return bytes read
    lda     tmp1
    ldx     tmp1+1
    rts	
.endproc	

; void _ch376_fwrite(void *ptr,int number)
.proc _ch376_fwrite
    ; use ptr1 to count bytes
    sta     ptr2                  ;  number param
    stx     ptr2+1                ;  number param
	
    jsr     popax
    sta     PTR_READ_DEST
    stx     PTR_READ_DEST+1
	
    lda     ptr2
    ldx     ptr2+1
    jsr     _ch376_set_bytes_write
    cmp     #CH376_USB_INT_SUCCESS ; finished
    beq     start_write
	; error 
    rts
start_write:

    lda     #CH376_CMD_WR_REQ_DATA
    sta     CH376_COMMAND
    ldy     CH376_DATA ; contains length read
    sty     tmp2; Number of bytes to read
	
    ldy     #$00
loop:
    lda     (PTR_READ_DEST),y
    sta     CH376_DATA ; read the data
    iny
    dec     tmp2
    bne     loop
    ; compute PTR_READ_DEST
    sty     tmp2  
    lda     PTR_READ_DEST
    clc
    adc     tmp2
    bcc     don_t_inc
    inc     PTR_READ_DEST+1
don_t_inc:	
    sta     PTR_READ_DEST
    lda     #CH376_BYTE_WR_GO
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    cmp     #CH376_USB_INT_SUCCESS ; finished
    bne     start_write
    rts
    ;jmp continue
finished:
    lda     tmp1
    ldx     tmp1+1
    rts	
.endproc	


; void _ch376_write(void *ptr)
.proc _ch376_write
    sta     PTR_READ_DEST
    stx     PTR_READ_DEST+1

start_write:

    lda     #CH376_CMD_WR_REQ_DATA
    sta     CH376_COMMAND
    lda     CH376_DATA ; contains length read
    sta     tmp2; Number of bytes to read
	
    ldy     #$00
loop:
    lda     (PTR_READ_DEST),y
    sta     CH376_DATA ; read the data
    iny
    dec     tmp2
    bne     loop
    ; compute PTR_READ_DEST
    sty     tmp2  
    lda     PTR_READ_DEST
    clc
    adc     tmp2
    bcc     don_t_inc
    inc     PTR_READ_DEST+1
don_t_inc:	
    sta     PTR_READ_DEST
    lda     #CH376_BYTE_WR_GO
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    cmp     #CH376_USB_INT_SUCCESS ; finished
    bne     start_write
    rts
finished:
    lda     tmp1
    ldx     tmp1+1
    rts	
.endproc	

.proc _ch376_file_create
    lda     #CH376_FILE_CREATE
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    ldx     #$00
    rts
.endproc

.proc _ch376_dir_create
    lda     #CH376_DIR_CREATE
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    rts
.endproc

.proc _ch376_get_entry
    sta     ptr1
    stx     ptr1+1
    ; optimize fixme
    ldy     #$00
loop: 
    lda     CH376_DATA ; Fetch char
    sta     (ptr1),y ; we store it
    iny
    cpy     #8+3
    bne     loop
    iny
    lda     CH376_DATA ; read attribute (folder or file)
    sta     (ptr1),y ; we store it
    ; read all information
loop2:
    lda     CH376_DATA ; read attributes : 20 bytes
    sta      (ptr1),y ; we store it
    iny
    cpy     #33 ; 8+3 file name + \0 + attribute (One byte) + others attributes (20 bytes)
    bne     loop2
    
    lda     #$00
    ldy     #8+3
    sta     (ptr1),y
    rts
.endproc

.proc _ch376_process_next_entry_catalog
    lda     #CH376_FILE_ENUM_GO ; 33
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    rts
.endproc   

.proc _ch376_file_erase
    lda     #CH376_FILE_ERASE
    sta     CH376_COMMAND
    jsr     _ch376_wait_response
    ldx     #$00
    rts
.endproc

.proc _ch376_rd_usb_data0
    LDA     #CH376_RD_USB_DATA0 ;$27
    STA     CH376_COMMAND
    lda     CH376_DATA ;fetch the length 
    rts
.endproc

.proc _ch376_readdir
  ;jsr popax ; contains wild card
	;lda #"*"
	;sta BUFNOM
	;lda #0
	;sta BUFNOM+1

	;jsr _ch376_set_file_name
	;jsr _ch376_file_open
	
	;lda #0 
;next_entry
	;cmp #CH376_USB_INT_SUCCESS ;int_success
	;beq fin_catalogue
	;cmp #$1d
	;bne fin_catalogue

	;LDA #CH376_RD_USB_DATA0 ;$27
	;STA CH376_COMMAND
	;lda CH376_DATA ;fetch the length 
	;cmp #32
	;beq catalogue_ok

	rts
;catalogue_ok
;	jsr display_one_file_catalog
;	jmp next_entry
;	;jsr _ch376_init
	;beq print_failed ; print failed because not detected
;fin_catalogue
;	rts

;display_one_file_catalog
;
;	ldy #1
;	ldx #1
;loop12
	
;	lda CH376_DATA ; Fetch char
;	sta BUFNOM,y ; we store it
;	iny
;
	
;	inx
;	cpx #9 ; Do we display dot ?
;	bne no_dot_to_display

;	lda #"."
	;sta BUFNOM,y
	
	;sty TR5 ; store the position of '.'
	
	;iny

;no_dot_to_display
	; Suppress dot if there is not an extension
;	jmp don_t_test_if_extension_contains_chars
	; if X=10 then we test if there is an extension
;	cpx #10 
	;bne don_t_test_if_extension_contains_chars
	;cmp #" " ; Is it a space ? if yes, we delete . of the filename
	;bne don_t_test_if_extension_contains_chars
	; deleting .
	;lda TR5
	;sty TR5
	;tay
	;lda #" "
	;sta BUFNOM,y ; at this step, filename is "man" instead of "man."
	;ldy TR5
	;iny 
	; dey
	; End of suppress dot 
	
;don_t_test_if_extension_contains_chars
;	cpx #12
;	bne loop12
;loop19
;	LDA #0
;	sta BUFNOM,y
;	sty TEMP_ORIX_1 ; Store the length

	

	; reading attributes
	;lda CH376_DATA ; fetching attributes
	;cmp #$10
	;bne it_is_a_file
	; Its a directorty
	;lda #COLOR_FOR_DIRECTORY
	;sta BUFNOM
	;dey ; we remove "."
	;lda #0
	;sta BUFNOM,y
	;sty TEMP_ORIX_1
;it_is_a_file
	;ldx #20 ; don't care but we read it, it's the attribu tes
;loop3
;	lda CH376_DATA ; First byte is a
;	dex
;	bpl loop3

	;lda BUFNOM ; read first char of the file
	;cmp #"." ; is it a dot ?
	;beq no_other_space	 ; Yes we don't want to be displayed
	
	;lda BUFNOM+1 ; read first char of the file
	;cmp #"." ; is it a dot ?
	;beq no_other_space	 ; Yes we don't want to be displayed

	;dec NUMBER_OF_COLUMNS
;	bne no_need_to_CRLF

	;BRK_TELEMON(XCRLF)
	;lda #NUMBER_OF_COLUMNS_LS
	;sta NUMBER_OF_COLUMNS

;no_need_to_CRLF
	;PRINT(BUFNOM)

	; Add space in order to have a columns

;loop10
	;ldy TEMP_ORIX_1
	;cpy #13
	;beq no_other_space
	;iny
	;sty TEMP_ORIX_1
	;lda #" "
	;BRK_TELEMON(XWR0)
	;jmp loop10

;no_other_space	
;	lda #CH376_FILE_ENUM_GO ; 33
;	sta CH376_COMMAND
	;jsr _ch376_wait_response
	
	rts

.endproc
