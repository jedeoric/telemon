; test if printer is connected	
XTSTLP_ROUTINE	
.(
	LDX     #$00
	STX     V1DRA
	LDA     V1DRB
	AND     #$EF
	STA     V1DRB
	ORA     #$10
	STA     V1DRB
loop
	LDA     V1IFR
	AND     #$02
	BNE     next
	DEX
	BNE     loop
	RTS

next
	lda     FLGTEL
	ora     #$02
	sta     FLGTEL
	rts
.)
