; test if printer is connected	
XTSTLP_ROUTINE	
routine_to_define_18
.(
	LDX #$00
	STX V1DRA
	LDA V1DRB
	AND #$EF
	STA V1DRB
	ORA #$10
	STA V1DRB
loop48
	LDA V1IFR
	AND #$02
	BNE next40
	DEX
	BNE loop48
	RTS

next40
	lda FLGTEL
	ora #$02
	sta FLGTEL
	rts
.)
