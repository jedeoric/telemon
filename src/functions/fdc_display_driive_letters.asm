#ifdef WITH_FDC	
	lda #<str_drive
	ldy #>str_drive
	BRK_TELEMON(XWSTR0)  ; display DRIVE:
	; let's go to display all Drive availables
	PLA
	PHA
	TAX
	lda $c2dc,x
	sta CDRIVE
	STX DRVDEF ; set drive by default
	PLA
	TAX
loop50	
	TXA
	CLC
loop51	
	ADC #"A" ; display A, because it adds to c2dc (drives ) 41 (equal to A in ascii table)
	BRK_TELEMON(XWR0)
.(	
loop	
	inx 
	CPX #$04
	beq next61
	LDA TABDRV,X ;3 
	beq loop
.)	
	lda #"-" ; display - to separate drive 2
	BRK_TELEMON(XWR0) ; 2

next52	
	jmp loop50 ;3
#endif	
