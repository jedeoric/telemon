; [IN] AY contains the length to write
; [IN] PTR_READ_DEST must be set because it's the ptr_dest
; [MODIFIED] TR0,PTR_READ_DEST, YA

; [UNCHANGED] X

.(
  jsr     _ch376_set_bytes_write

  cmp     #CH376_USB_INT_SUCCESS    ; finished
  beq     writeme
  rts     
continue	
  cmp     #CH376_USB_INT_DISK_WRITE  ; something to read
  beq     writeme
  cmp     #CH376_USB_INT_SUCCESS    ; finished
  beq     finished 
  ; TODO  in A : $ff X: $ff
  lda     #$00
  tax
  rts
writeme
   
  jsr     we_write

  lda     #CH376_BYTE_WR_GO 
  sta     CH376_COMMAND
  jsr     _ch376_wait_response
  
#ifdef     CPU_65C02
  bra     continue
#else 
  jmp     continue
#endif    

finished
  ; at this step PTR_READ_DEST is updated
  rts	

we_write


  lda     #CH376_CMD_WR_REQ_DATA
  sta     CH376_COMMAND

  lda     CH376_DATA                ; contains length write
  beq     finished                  ; we don't have any bytes to write then stops (Assinie report)
  sta     TR0                       ; Number of bytes to read, storing this value in order to loop
  
  ldy     #$00
loop
  lda     (PTR_READ_DEST),y                ; read the data
  sta     CH376_DATA         ; send data in the ptr address
  iny                               ; inc next ptr addrss
  cpy     TR0                       ; do we read enough bytes
  bne     loop                      ; no we read
  
  tya                               ; We could do "lda TR0" but TYA is quicker. Add X bytes to A in order to update ptr (Y contains the size of the bytes reads)
  clc                               ; 
  adc     PTR_READ_DEST
  bcc     next
  inc     PTR_READ_DEST+1
next
  sta     PTR_READ_DEST
  
  rts
.)

