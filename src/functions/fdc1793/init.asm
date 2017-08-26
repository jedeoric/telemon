FDC1783_init
.(
	NOP
	NOP
	LDX #$03

loop8
.(
	LDA definition_for_CDRIVE_init,X 
	STA CDRIVE

	LDA #%00001000 ; launch seek
	STA FDCCR
	TAY
loop
	INY
	bne loop
	NOP
.)	
	
	LDY #$40
	STX RES
c03C
.(
loop
	LDA FDCCR
	LSR
	bcc next10
	INC RES
	bne loop
	DEY
	bne loop
	TYA
.)
	beq next9
next10	
	LSR FLGTEL
	LDA #$AA
next9	

	STA TABDRV,X
	DEX
c055	
	bpl loop8
	nop
	INX
.)

