XWR0_ROUTINE
.(
	PHA                     ; Push byte to write
	LDA     #$00
	BEQ     skip2
+XWR1_ROUTINE	
	PHA
	LDA     #$04
	BNE     skip2
+XWR2_ROUTINE
	PHA
	LDA     #$08
	BNE     skip2
+XWR3_ROUTINE	
	PHA
	LDA     #$0C
skip2
	STA     work_channel
	PLA                      ; Get byte to write
+XWSTR0_re_enter_from_XDECAL	
	STA     i_o_save         ; save the byte to write in I_O_save
	LDA     #$04
	STA     i_o_counter
	TXA
	PHA
	TYA
	PHA

loop2
	LDX     work_channel    ; It contains the value of the I/O 
	LDA     IOTAB0,X        ; X contains 0, 4, 8 $0c
	CMP     #$88
	BCC     skip            ; If it's higher than $88, it means that it's not an input 
	ASL                     ; It's an input set it *2
	TAX                     ; 
	LDA     ADIOB,X         ; GET vectors  
	STA     ADIODB_VECTOR+1
	LDA     ADIOB+1,X
	STA     ADIODB_VECTOR+2 ; 
	LDA     i_o_save        ; Get Byte to write

loop	
	BIT     loop             
	JSR     ADIODB_VECTOR   ;  and write this byte

skip
	INC     work_channel
	DEC     i_o_counter
	BNE     loop2
	PLA
	TAY
	PLA
	TAX
	LDA     i_o_save
+Lc7a7	
	RTS
.)	
	
	
	