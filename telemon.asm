#define CDRIVE $314
#define FDCCR $0310

#define BRK_TELEMON(value)\
	.byt 00,value;\

/*
020d FLGTEL
02be IOVECTORS
02ee FLGRST
c000 tmEntryPoint
c05b tmDiskReset
c05d tmDiskResetLoop
c2a6 tmHWReset
c4d1 tmDiskCmd
c8bf tmSerialIO
d903 tmSetKeyColumn7f
da1a tmAYWrite
da4f tmAYSilent
*/

/*************************** VIA 1 vectors*/

#define V1DRB $0300
#define V1DDRB $0302
#define V1IER $030e
#define V1DDRA $0303

#define V1PCR $030C

/***************************  ACIA vectors*/


#define ACIASR $031D
#define ACIACR $031E

/*************************** VIA 1 vectors*/


#define V2DRB $0320
#define V2DRA $0321
#define V2DDRB $0322
#define V2DDRA $0323
#define V2T1 $0324
#define V2PCR $032C
#define V2IER $032E

/*************************** TELEMON VECTORS*/

/* PAGE 2 TELEMON */

#define RES $00 ; address general usage

#define FLGTEL $020D
#define TABDRV $0208 ; Activating drive 0 if not connected, b7 equal double side
#define IOTAB0 $02ae ; activating channel 1

#define ADIOB $02be ; 48 bytes ? I/O management address

#define CSRND $02EF ; current value of random generator

#define LPRFX $0288 ; print width

#define LPRX $0286 ; word cursor in the line
#define FLGLPR $028a ;; word b7 ready

#define KORAM $020f ; total Max ram Bytes	
#define BNKST $0200 ; RESB 8 value of bytes $fffb of each bank
#define KOROM $020E	; Ko ROM total

#define FLGKBD $0275	;Keyboard flag : b7 majn b6 if sound

*=$c000
telemon
	SEI
	CLD
	LDX #$FF
	TXS ; init stack
	inx
	stx $0418
	jsr init_via
	jsr init_printer
	jsr init_disk
	; init channels loading 15

	LDX #$0F
loop1
	LSR IOTAB0,X ; init channels (0 to 3)
	DEX
	bpl loop1
	
	LDA #$D0
	JSR routine_to_define_2
	LDA $02FA ; testing if VIRQ low byte is $4C ?
	CMP #$4C
	BNE end_rout ; non equal to $4C
	LDA $026D
	AND #$20
	BNE end_rout
	LDA FLGTEL
	AND #$01
	CLC
	bcc next2
end_rout
	lda #01
	sec
next2
	sta FLGTEL
	ror $02EE
	bmi next1
	jmp routine_to_define_3 
next1
	LDX #$2F
next8		
	LDA data_to_define_1,X
	STA ADIOB,X 
	DEX
	bpl next8
	LDX #$04
loop4	
	LDA data_to_define_6,X ; FIXME
	STA CSRND,X
	DEX
	bpl loop4	
before2
	
	NOP
	NOP

	LDX #$03
C028
loop8
	LDA definition_for_CDRIVE_init,X 
	STA CDRIVE

	LDA #$08
	STA FDCCR
	
	TAY
c034	
loop6		
	INY
	bne loop6
	NOP

	LDY #$40
	
	STX RES
c03C
loop7	
	LDA FDCCR
	LSR
	bcc next10
	INC RES
	bne loop7
	DEY
	bne loop7
	TYA
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
loop3	
	LDA loading_vectors_page_4,X ; X should be equal to 0
	STA $0400,X
	LDA loading_vectors_b800,X 
	STA $B800,X
	LDA loading_code_to_page_6,X
	STA $0600,X
	LDA data_to_define_4,X 
	STA $0700,X
	INX ; loop until 256 bytes are filled
c072	
	bne loop3
	JSR $0603

routine_to_define_3 	
c0ac
	LDA #$00
loop9	
	PHA
	TAX
	JSR routine_to_define_10 ;FIXMEROUT
	PLA
	CLC
	ADC #$0C
	CMP #$30
	bne loop9
	LDA #$00 ; INIT VALUE of the rom to 0 bytes
	STA KOROM
	
	LDA #$40 ; INIT VALUE of the RAM to 64 Kbytes
	STA KORAM

	LDX #$07
	LDY BNKST,X
	TYA
	AND #$10
	BNE next3
	TYA
	PHA
	AND #$0F
	TAY
	INY
	PLA
	AND #$20
	CLC
	BEQ next4
	TYA

	ADC KOROM
	STA KOROM
c0ad	
	.byt $90,$07 ;C0AD   90 07      BCC $C0B6
next4
	TYA
	ADC KORAM
	STA KORAM
next3
	DEX
	.byt $d0,$d9 ;C0B7   D0 D9      BNE $C092
	BIT $02EE ; FIXME
	BPL next5
	LDX #$0B ; copy to $2F4 12 bytes
before1
	LDA data_to_define_5,X ; SETUP VNMI, VIRQ, VAPLIC
	STA $02F4,X ; FIXME
	DEX
	BPL before1
	JSR routine_to_define_4 
next5
	LDA $026C ; FIXME
	AND #$90
	BEQ next6
	LDA FLGTEL
	ORA #$40
	STA FLGTEL
next6
	JSR routine_to_define_9 ; $DF5B 
	JSR routine_to_define_8	 ; routine_to_define_8
	JSR routine_to_define_7 ; 
	JSR routine_to_define_5 ; $DFAB
	JSR init_rs232 ; $DB54 
	LDA FLGKBD
	LSR
	AND #$03
;	#define BRK text


	
	BRK_TELEMON($52) 	;.byt $00,$52

	



	
	.byt $a9,$80,$00,$00,$a9,$88,$00,$00,$00
	.byt $3c,$a9,$8f,$00,$01,$a9,$13,$a0,$c4,$2c,$0d,$02,$50,$08,$a9,$8f
	.byt $00,$00,$a9,$0d,$a0,$c4,$00,$15,$2c,$ee,$02,$10,$26,$a9,$9b,$a0
	.byt $c3,$00,$14,$a9,$e1,$a0,$c3,$00,$14,$00,$25,$ad,$0f,$02,$20,$9b
	.byt $c2,$a9,$b9,$a0,$c3,$00,$14,$ad,$0e,$02,$20,$9b,$c2,$a9,$c2,$a0
	.byt $c3,$00,$14,$20,$bf,$c6,$d0,$05,$00,$25,$4c,$88,$c1,$2c,$ee,$02
	.byt $10,$06,$a9,$01,$a0,$c4,$00,$14,$2c,$ee,$02,$30,$0f,$20,$68,$c2
	.byt $ae,$f4,$02,$ad,$f5,$02,$ac,$f6,$02,$4c,$5c,$c2,$a2,$00,$bd,$08
	.byt $02,$d0,$07,$e8,$e0,$04,$d0,$f6,$f0,$36,$8a,$48,$ad,$20,$02,$f0
	.byt $04,$a9,$2c,$00,$10,$a9,$cc,$a0,$c3,$00,$14,$68,$48,$aa,$bd,$dc
	.byt $c2,$8d,$14,$03,$8e,$0c,$02,$68,$aa,$8a,$18,$69,$41,$00,$10,$e8
	.byt $e0,$04,$f0,$11,$bd,$08,$02,$f0,$f6,$a9,$2d,$00,$10,$4c,$c9,$c1
	.byt $ad,$20,$02,$f0,$02,$00,$25,$a9,$d3,$a0,$c3,$00,$14,$a9,$00,$8d
	.byt $00,$02,$ad,$0d,$02,$4a,$b0,$0f,$a9,$19,$a0,$c4,$00,$14,$20,$00
	.byt $b8,$a9,$30,$a0,$c4,$00,$14,$a2,$02,$bd,$5c,$c4,$9d,$5d,$05,$ca
	.byt $10,$f7,$20,$00,$06,$20,$68,$c2,$ad,$0d,$02,$4a,$b0,$2f,$a9,$55
	.byt $a0,$c4,$a2,$07,$00,$24,$a2,$00,$a9,$7d,$a0,$ff,$20,$5c,$c2,$f0
	.byt $1c,$ad,$0d,$02,$09,$04,$8d,$0d,$02,$a2,$06,$bd,$00,$02,$c9,$ef
	.byt $f0,$05,$ca,$10,$f6,$30,$06,$a9,$00,$a0,$c0,$d0,$0f,$a2,$00,$00
	.byt $35,$ae,$fd,$02,$30,$30,$ad,$fe,$02,$ac,$ff,$02,$8d,$15,$04,$8c
	.byt $16,$04,$8e,$17,$04,$4c,$0c,$04,$58,$a9,$02,$85,$44,$a5,$44,$d0
	.byt $fc,$a2,$0c,$00,$57,$ad,$1e,$03,$29,$f3,$09,$08,$8d,$1e,$03,$a9
	.byt $8f,$00,$05,$60,$00,$10,$00,$0c,$c9,$03,$d0,$03,$20,$00,$90,$c9
	.byt $01,$d0,$f1,$a9,$00,$a0,$e0,$a2,$00,$f0,$c1,$a0,$00,$a2,$20,$86
	.byt $14,$a2,$01,$4c,$39,$ce
init_via
	lda #$7f 
	sta V1IER ; Initialize via1
	sta V2IER ; Initialize via2
	sta ACIASR ; Init ACIA

	lda #$00
	sta CDRIVE
	lda #$ff
	sta V1DDRA
	
	LDA #$F7
	STA V1DRB
	STA V1DDRB

	LDA #$17
	STA V2DRA
	STA V2DDRA

	LDA #$E0
	STA V2DRB
	STA V2DDRB

	LDA #$CC
	STA V1PCR
	STA V2PCR

	RTS

definition_for_CDRIVE_init
	.byt $84,$a4,$c4,$e4


loading_code_to_page_6
	
	.byt $4c,$50,$06 ;jmp $0605 ? 3 bytes
	
	lda #$00 ; 2 bytes
	sta $0321 ; 3 bytes
	/*	
	TAX
loop11	
	LDA $0700,X
	STA $C500,X
	INX
	BNE loop11
	

	LDX #$07
	STX $0321
	LDY #$00
loop14
	LDA $FF00,Y
	PHA
loop12
	ADC #$04
	BCC loop12
	PLA
	CMP $FF00,Y
	BNE $063B
	INY
	BNE $0619
	STY $FFFB
	LDA $FFFB
	CPY $FFFB
	BNE next13
	INY
	BNE next12
	LDA #$0F
next12
	BIT $10A9
next13
	STA $0200,X
	CMP #$02
	BNE $0647
	JMP ($FFFC)
	DEX
	BNE $0614
	LDA #$07
	STA $0321
	RTS
	LDX #$00
	JSR $065E
	LDX #$06
	JSR $065E
	DEX
	BNE $0657
	RTS
	LDA $0200,X
	BPL $0685
	STX $0321
	LDA $FFF8
	STA $02
	LDA $FFF9
	STA $03
	LDY #$00
	STX $0321
	LDA ($02),Y
	PHA
	LDA #$07
	STA $0321
	PLA
	BEQ $0685
	BRK
	BPL $064B
	BNE $0672
	LDA $0200,X
	ASL A
	BPL $06A2
	STX $0321
	LDA $FFFC
	LDY $FFFD
	STA $02FE
	STY $02FF
	STX $02FD
	LDA #$07
	STA $0321
	rts
	*/
	.byt $aa,$bd,$00,$07,$9d,$00,$c5,$e8
	.byt $d0,$f7,$a2,$07
	.byt $8e,$21,$03,$a0,$00,$b9,$00,$ff,$48,$69,$04,$90
	.byt $fc,$68,$d9,$00,$ff,$d0,$14,$c8,$d0,$ef,$8c,$fb,$ff,$ad,$fb,$ff
	.byt $cc,$fb,$ff,$d0,$08,$c8,$d0,$f2,$a9,$0f,$2c,$a9,$10,$9d,$00,$02
	.byt $c9,$02,$d0,$03,$6c,$fc,$ff,$ca,$d0,$ca
	lda #$07
	sta $0321
	rts
	
	.byt $a2,$00,$20,$5e,$06,$a2,$06,$20,$5e,$06,$ca,$d0,$fa,$60,$bd,$00
	.byt $02,$10,$22,$8e,$21,$03,$ad,$f8,$ff,$85,$02,$ad,$f9,$ff,$85,$03
	.byt $a0,$00,$8e,$21,$03,$b1,$02,$48,$a9,$07,$8d,$21,$03,$68,$f0,$05
	.byt $00,$10,$c8,$d0,$ed,$bd,$00,$02,$0a,$10,$17,$8e,$21,$03,$ad,$fc
	.byt $ff,$ac,$fd,$ff,$8d,$fe,$02,$8c,$ff,$02,$8e,$fd,$02,$a9,$07,$8d
	.byt $21,$03,$60,$4c,$00,$00
data_to_define_5
	; 12 bytes
	.byt $07,$92,$c3,$4c,$00,$00,$4c,$06,$04,$80,$00,$00
	
	
	.byt $a9,$33,$a0,$c4,$00,$14,$4c,$98,$c3,$0c,$97,$96,$95,$94
	.byt $93,$92,$91,$90
	.byt $20
	.asc "TELESTRAT"
	
	.byt $20
	.byt $90
	.byt $91,$92,$93,$94,$95,$96,$97,$90,$00,$20
	.asc "Ko RAM"
	
	.byt $2c
	.byt $00,$20
	
	.asc "Ko ROM"
	.byt $0d,$0a,$00
	

	.asc "Drive:",0
	.byt $0d,$0a
	
	.asc "TELEMON V2.4"
	
	.byt $0d,$0a
	.asc "(c) 1986 ORIC International"
	.byt $0d,$0a

	.byt $00,$0a
	.asc "Imprimante",0
	.byt $1b,$3a,$69
	.byt $43,$11,$00,$1b,$3a,$6a,$43,$14,$00,$8c
	.asc "Inserez une disquette",0
	.byt $0d,$18,$00
	.asc "Logiciel ecrit par Fabrice BROCHE",0
	.asc "BONJOURCOM"
data_to_define_6	
	; FIVE Bbytes to load in CSRND
	.byt $80,$4f,$c7,$52,$58
loading_vectors_b800
	.byt $a9,$e8,$8d,$21,$03,$a9,$00,$a0,$c1,$85,$00,$84
	.byt $01,$a2,$01,$8e,$12,$03,$20,$4f,$b8,$ad,$00,$c1,$d0,$2f,$ad,$03
	.byt $c1,$ac,$04,$c1,$85,$00,$84,$01,$ec,$01,$c1,$d0,$09,$a9,$58,$20
	.byt $6d,$b8,$a2,$00,$ea,$ea,$e8,$8e,$12,$03,$20,$4f,$b8,$e6,$01,$ce
	.byt $02,$c1,$d0,$e4,$20,$05,$c1,$ad,$fb,$ff,$8d,$00,$02,$a9,$ef,$8d
	.byt $21,$03,$60,$a9,$88

	.byt $8d,$10,$03,$a0,$04,$88,$d0,$fd,$ad,$10,$03
	.byt $4a,$90,$1e,$ad,$18,$03,$30,$f5,$ad,$13,$03,$91,$00,$c8,$4c,$5f
	.byt $b8

routine_to_define_2	
c4d1
	STA FDCCR
	LDY #$03
loop10
	DEY
	BNE loop10
next11
	LDA FDCCR
	LSR
	BCS next11
	rts
	
	
	.byt $ea,$ad,$10,$03,$29,$1c,$f0,$f7,$d0,$c9
routine_to_define_10
	STX $02
	TXA
	LDX #$FF
loop18
	SEC
	SBC #$03
	INX
	BCS loop18
	LDA data_to_define_7,X 
	STA $00
	LDA $C6B0,X
	STA $01
	LDA $C6B1,X
	LDY $C6B2,X
	LDX $02
	BIT $C518
	BVC next19
	LDA #$00
	BIT $01A9
	BIT $C524
next19
	SEC
	JMP $0409
	BIT $C518
	BVC next20
	BIT $C524
next20
	CLC
	JMP $0409
loading_vectors_page_4

	JMP $0493
	JMP $04A1
	JMP $047E
	JMP $0419
	JMP $0436
/*
	BRK
	BRK
	JMP $04AF
	JMP $0000
	BRK
	BRK
	PHP
	SEI
	PHA
	LDA $0321
	AND #$F8
	STA $0321
	PLA
	JSR $C500
	TAY
	LDA $0321
	ORA #$07
	STA $0321
	ROR
	PLP
	ASL
	TYA
	RTS
	PHP
	SEI
	PHA
	TXA
	PHA
	LDA $0321
	LDX $0418
	STA $04C8,X
	INC $0418
	PLA
	TAX
	LDA $0417
	JSR $046A
	PLA
	PLP
	JSR $0414
	PHP
	SEI
	PHA
	TXA
	PHA
	DEC $0418
	LDX $0418
	LDA $04C8,X
	JSR $046A
	PLA
	TAX
	PLA
	PLP
	RTS

	PHP
	SEI
	AND #$07
	STA $04C7
	LDA $0321
	AND #$F8
	ORA $04C7
	STA $0321
	PLP
	RTS
	
	STA $21
	LDA $0321
	AND #$07
	STA $040F
	LDA $0321
	ORA #$07
	STA $0321
	JMP $C868
	LDA $0321
	AND #$F8
	ORA $040F
	STA $0321
	LDA $21
	RTI
	PHA
	LDA $0321
	AND #$F8
	ORA $0410
	STA $0321
	PLA
	RTS
	
	LDA $0321
	AND #$F8
	ORA $0410
	STA $0321
	LDA ($15),Y
	PHA
	LDA $0321
	ORA #$07
	STA $0321
	PLA
	RTS
	
	*/
	/*
C5B1   90 4C      BCC $C5FF
C5B3   50 0F      BVC $C5C4
C5B5   A8         TAY
C5B6   F0 2C      BEQ $C5E4
C5B8   BD 88 C0   LDA $C088,X
C5BB   1D 89 C0   ORA $C089,X
C5BE   F0 02      BEQ $C5C2
C5C0   18         CLC
C5C1   60         RTS
C5C2   38         SEC
C5C3   60         RTS
C5C4   85 02      STA $02
C5C6   84 03      STY $03
C5C8   38         SEC
C5C9   E5 00      SBC $00
C5CB   9D 8A C0   STA $C08A,X
C5CE   98         TYA
C5CF   E5 01      SBC $01
C5D1   9D 8B C0   STA $C08B,X
C5D4   8A         TXA
C5D5   69 03      ADC #$03
C5D7   AA         TAX
C5D8   A0 03      LDY #$03
C5DA   B9 00 00   LDA $0000,Y
C5DD   9D 7F C0   STA $C07F,X
C5E0   CA         DEX
C5E1   88         DEY
C5E2   10 F6      BPL $C5DA
C5E4   A9 00      LDA #$00
C5E6   9D 88 C0   STA $C088,X
C5E9              .END
*/

	.byt $00,$00,$4c,$af,$04,$4c,$00,$00,$00,$00,$08,$78,$48
	.byt $ad,$21,$03,$29,$f8,$8d,$21,$03,$68,$20,$00,$c5,$a8,$ad,$21,$03
	.byt $09,$07,$8d,$21,$03,$6a,$28,$0a,$98,$60
	
	.byt $08,$78,$48,$8a,$48,$ad
	.byt $21,$03,$ae,$18,$04,$9d,$c8,$04,$ee,$18,$04,$68,$aa,$ad,$17,$04
	.byt $20,$6a,$04,$68,$28,$20,$14,$04,$08,$78,$48,$8a,$48,$ce,$18,$04
	.byt $ae,$18,$04,$bd,$c8,$04,$20,$6a,$04,$68,$aa,$68,$28,$60,$08,$78
	.byt $29,$07,$8d,$c7,$04,$ad,$21,$03,$29,$f8,$0d,$c7,$04,$8d,$21,$03
	.byt $28,$60,$85,$21,$ad,$21,$03,$29,$07,$8d,$0f,$04,$ad,$21,$03,$09
	.byt $07,$8d,$21,$03,$4c,$68,$c8,$ad,$21,$03,$29,$f8,$0d,$0f,$04,$8d
	.byt $21,$03,$a5,$21,$40,$48,$ad,$21,$03,$29,$f8,$0d,$10,$04,$8d,$21
	.byt $03,$68,$60
	
	
	.byt $ad,$21,$03,$29,$f8,$0d,$10,$04,$8d,$21,$03,$b1,$15
	.byt $48,$ad,$21,$03,$09,$07,$8d,$21,$03,$68,$60
	
data_to_define_4	
	; should be length 256 bytes ?
	.byt $90,$4c,$50,$0f,$a8,$f0,$2c,$bd,$88,$c0,$1d,$89,$c0,$f0,$02,$18
	.byt $60,$38,$60,$85,$02,$84,$03,$38,$e5,$00,$9d,$8a,$c0,$98,$e5,$01
	.byt $9d,$8b,$c0,$8a,$69,$03,$aa,$a0,$03,$b9,$00,$00,$9d,$7f,$c0,$ca
	.byt $88,$10,$f6,$a9,$00
	.byt $9d,$88,$c0,$9d,$89,$c0,$bd,$82,$c0,$9d,$84,$c0,$9d,$86,$c0,$bd
	.byt $83,$c0,$9d,$85,$c0,$9d,$87,$c0,$60,$70,$26,$20,$07,$c5,$b0,$20
	.byt $bd,$86,$c0,$bc,$87,$c0,$20,$a6,$c5,$9d,$86,$c0,$98,$9d,$87,$c0
	.byt $bd,$88,$c0,$d0,$03,$de,$89,$c0,$de,$88,$c0,$a0,$00,$b1,$24,$18
	.byt $60,$48,$bd,$88,$c0,$dd,$8a,$c0,$bd,$89,$c0,$fd,$8b,$c0,$b0,$1f
	.byt $bd,$84,$c0,$bc,$85,$c0,$20,$a6,$c5,$9d,$84,$c0,$98,$9d,$85,$c0
	.byt $fe,$88,$c0,$d0,$03,$fe,$89,$c0,$a0,$00,$68,$91,$24,$18,$60,$68
	.byt $60,$18,$69,$01,$90,$01,$c8,$dd,$82,$c0,$85,$24,$98,$fd,$83,$c0
	.byt $90,$08,$bd,$80,$c0,$bc,$81,$c0,$85,$24,$84,$25,$a5,$24,$60
data_to_define_7
	.byt $c4
	.byt $c5,$80,$c6,$80,$c6,$00,$c8,$00,$c8,$00,$ca,$00,$ca,$00,$d2,$a2
	.byt $00,$8e,$01,$03,$ad,$00,$03,$29,$ef,$8d,$00,$03,$09,$10,$8d,$00
	.byt $03,$ad,$0d,$03,$29,$02,$d0,$04,$ca,$d0,$f6,$60,$ad,$0d,$02,$09
	.byt $02,$8d,$0d,$02,$60,$a2,$00,$2c,$a2,$04,$2c,$a2,$08,$2c,$a2,$0c
	.byt $48,$68,$dd,$ae,$02,$f0,$0d,$bc,$ae,$02,$10,$09,$e8,$48,$8a,$29
	.byt $03,$d0,$ee,$68,$60,$a0,$0f,$d9,$ae,$02,$f0,$0f,$88,$10,$f8,$86
	.byt $19,$48,$a0,$80,$aa,$20,$1c,$c8,$a6,$19,$68,$9d,$ae,$02,$18,$60
	.byt $a2,$00,$2c,$a2,$04,$2c,$a2,$08,$2c,$a2,$0c,$a0,$03,$c9,$00,$f0
	.byt $1d,$dd,$ae,$02,$f0,$05,$e8,$88,$10,$f7,$60,$5e,$ae,$02,$a2,$0f
	.byt $dd,$ae,$02,$f0,$f5,$ca,$10,$f8,$aa,$a0,$81,$4c,$1c,$c8,$5e,$ae
	.byt $02,$e8,$88,$10,$f9,$60,$a9,$0a,$20,$5d,$c7,$a9,$0d,$48,$a9,$00
	.byt $f0,$0d,$48,$a9,$04,$d0,$08,$48,$a9,$08,$d0,$03,$48,$a9,$0c,$85
	.byt $19,$68,$85,$1b,$a9,$04,$85,$1a,$8a,$48,$98,$48,$a6,$19,$bd,$ae
	.byt $02,$c9,$88,$90,$16,$0a,$aa,$bd,$be,$02,$8d,$f8,$02,$bd,$bf,$02
	.byt $8d,$f9,$02,$a5,$1b,$2c,$95,$c7,$20,$f7,$02,$e6,$19,$c6,$1a,$d0
	.byt $db,$68,$a8,$68,$aa,$a5,$1b,$60,$a2,$00,$2c,$a2,$04,$2c,$a2,$08
	.byt $2c,$a2,$0c,$86,$1c,$85,$15,$84,$16,$a5,$1c,$85,$19,$a0,$00,$20
	.byt $11,$04,$f0,$e3,$20,$72,$c7,$e6,$15,$d0,$ee,$e6,$16,$d0,$ea,$a9
	.byt $00,$2c,$a9,$04,$2c,$a9,$08,$2c,$a9,$0c,$85,$19,$a9,$04,$85,$1a
	.byt $8a,$48,$98,$48,$a6,$19,$bd,$ae,$02,$10,$0e,$c9,$88,$b0,$0a,$aa
	.byt $a0,$40,$20,$1c,$c8,$85,$1d,$90,$06,$e6,$19,$c6,$1a,$d0,$e5,$68
	.byt $a8,$68,$aa,$a5,$1d,$60,$a9,$00,$2c,$a9,$04,$2c,$a9,$08,$2c,$a9
	.byt $0c,$85,$1b,$a5,$1b,$20,$da,$c7,$b0,$f9,$38,$60,$84,$17,$84,$18
	.byt $48,$8a,$0a,$aa,$bd,$be,$02,$8d,$f8,$02,$bd,$bf,$02,$8d,$f9,$02
	.byt $68,$46,$17,$24,$18,$4c,$f7,$02
	
data_to_define_1
	; length must be $30
	; used to set I/O vectors
	.byt $5c,$d9 ; 0
	.byt $1a,$c8 
	.byt $f7,$da
	.byt $5d,$db
	.byt $1a,$c8 ; 
	.byt $1a,$c8
	.byt $1a,$c8
	.byt $1a,$c8
	.byt $86,$db
	.byt $8c,$db ;
	.byt $92,$db
	.byt $98,$db
	.byt $1a,$c8
	.byt $1a,$c8
	.byt $70,$da ;30
	.byt $12,$db
	.byt $79,$db
	.byt $c6,$d5
	.byt $1a,$c8
	.byt $1a,$c8
	.byt $1a,$c8,$1a,$c8,$1a,$c8,$1a,$c8,$86,$22,$84,$23,$68,$85,$24,$29
	.byt $10,$f0,$40,$ba,$68,$d0,$03,$de,$02,$01,$38,$e9,$01,$48,$85,$15
	.byt $bd,$02,$01,$85,$16,$ad,$0f,$04,$8d,$10,$04,$a0,$00,$20,$11,$04
	.byt $0a,$aa,$a9,$04,$48,$a9,$02,$48,$bd,$a5,$ca,$bc,$a4,$ca,$90,$06
	.byt $bd,$a5,$cb,$bc,$a4,$cb,$48,$98,$48,$a5,$24,$48,$a5,$21,$a4,$23
	.byt $a6,$22,$40,$a5,$24,$48,$38,$66,$1e,$20,$bf,$c8,$4c,$b9,$c9
routine_to_define_12
	TYA
	PHA
	LDA $031D
	BPL next23
	LSR $1E
	PHA
	AND #$08
	BEQ next24
	LDX $031C
	PLA
	PHA
	AND #$07
	BEQ next25+1 ; ? Don't know yet
	ORA #$B0
next25
	BIT $8A
	LDX #$0C
	JSR $C51D

next24

	PLA
	AND #$10
	BEQ next23
	LDX #$18
	JSR $C50F
	BCS next26
	LDA $031D
	AND #$20
	BNE next23
	JSR $C518
	STA $031C
	LDA $031F
	AND #$07
	STA $3F
	BCC next23
next26
	INC $20
	BNE next23
	DEC $3F
	BNE next23
	LDA $028A
	LSR
	LSR 
	LSR
	LDA $031E
	AND #$F3
	BCC next29
	AND #$FE
next29
	STA $031E
next23
	PLA
	TAY
	RTS
	/*
C91E   CE 15 02   DEC $0215
C921   D0 50      BNE next27
C923   A9 04      LDA #$04
C925   8D 15 02   STA $0215
C928   2C 8A 02   BIT $028A
C92B   10 03      BPL next28
C92D   20 2F CA   JSR $CA2F
next28
C930   A5 44      LDA $44
C932   D0 02      BNE next29
C934   C6 45      DEC $45
next29
C936   C6 44      DEC $44
C938   38         SEC
C939   EE 10 02   INC $0210
C93C   AD 10 02   LDA $0210
C93F   E9 0A      SBC #$0A
C941   90 30      BCC next27
C943   8D 10 02   STA $0210
C946   2C 14 02   BIT $0214
C949   10 03      BPL next30
C94B   20 75 CA   JSR $CA75
next30
C94E   EE 11 02   INC $0211
C951   A5 42      LDA $42
C953   D0 02      BNE next31
C955   C6 43      DEC $43
next31
C957   C6 42      DEC $42
C959   AD 11 02   LDA $0211
C95C   E9 3C      SBC #$3C
C95E   90 13      BCC next27
C960   8D 11 02   STA $0211
C963   EE 12 02   INC $0212
C966   AD 12 02   LDA $0212
C969   E9 3C      SBC #$3C
C96B   90 06      BCC next27
C96D   8D 12 02   STA $0212
C970   EE 13 02   INC $0213
next27
C973   CE 16 02   DEC $0216
C976   D0 19      BNE end
C978   A9 0A      LDA #$0A
C97A   8D 16 02   STA $0216
C97D   AD 17 02   LDA $0217
C980   49 80      EOR #$80
C982   8D 17 02   STA $0217
C985   2C 48 02   BIT $0248
C988   10 07      BPL end
C98A   70 05      BVS end
C98C   A6 28      LDX $28
C98E   4C 2D DE   JMP $DE2D
end
C991   60         RTS
*/



	
	.byt $ce,$15
	.byt $02,$d0,$50,$a9,$04,$8d,$15,$02,$2c,$8a,$02,$10,$03,$20,$2f,$ca
	.byt $a5,$44,$d0,$02,$c6,$45,$c6,$44,$38,$ee,$10,$02,$ad,$10,$02,$e9
	.byt $0a,$90,$30,$8d,$10,$02,$2c,$14,$02,$10,$03,$20,$75,$ca,$ee,$11
	.byt $02,$a5,$42,$d0,$02,$c6,$43,$c6,$42,$ad,$11,$02,$e9,$3c,$90,$13
	.byt $8d,$11,$02,$ee,$12,$02,$ad,$12,$02,$e9,$3c,$90,$06,$8d,$12,$02
	.byt $ee,$13,$02,$ce,$16,$02,$d0,$19,$a9,$0a,$8d,$16,$02,$ad,$17,$02
	.byt $49,$80,$8d,$17,$02,$2c,$48,$02,$10,$07,$70,$05,$a6,$28,$4c,$2d
	.byt $de,$60,$ad,$0d,$03,$29,$20,$f0,$20,$ad,$8f,$02,$ac,$90,$02,$8d
	.byt $08,$03,$8c,$09,$03,$ad,$8c,$02,$4a,$90,$06,$20,$85,$e0,$4c,$b9
	.byt $c8
routine_todefine_1:
C9b1
	LDA #$FF
	STA $0309
	JMP $C8B9
	BIT $030D
	BMI $C9CC
	BIT $1E
	BPL $C9C9
	LDX $22
	LDY $23
	JMP $0400
	JMP $C8B6
	LSR $1E
	BIT $030D
	BVC $CA1F
	BIT $0304
	JSR $C91E
	DEC $02A6
	BNE $CA00
	JSR $D7DF
	JSR $C8BF
	BIT $0270
	BPL $C9F0
	LDA #$14
	STA $02A7
	BNE $C9FB
	LDA $02A8
	BIT $02A7
	BMI $C9FD
	DEC $02A7
	LDA #$01
	STA $02A6
	BIT $028C
	BPL $CA0B
	JSR $DFFA
	BIT $028C
	BVC $CA10
	JSR $DFFB
	LDA $028C
	LSR
	BCC $CA19
	JSR $E0E1
	JMP $C8B9
	JMP $C992
	LDA $030D
	AND #$02
	BEQ $CA1C
	BIT $0301
	JSR $CA2F
	JMP $C8B9
	LDX #$24
	JSR $C518
	BCC $CA3E
	ASL $028A
	SEC
	ROR $028A
	RTS
	/*
DA3E   8D 01 03   STA $0301
DA41   AD 00 03   LDA $0300
DA44   29 EF      AND #$EF
DA46   8D 00 03   STA $0300
DA49   09 10      ORA #$10
DA4B   8D 00 03   STA $0300
DA4E   0E 8A 02   ASL $028A
DA51   4E 8A 02   LSR $028A
	*/
	

	.byt $8d,$01
	.byt $03,$ad,$00,$03,$29,$ef,$8d,$00,$03,$09,$10,$8d,$00,$03,$0e,$8a
	.byt $02,$4e,$8a,$02,$60,$a9,$00,$a2,$04,$9d,$10,$02,$ca,$10,$fa,$a9
	.byt $01,$8d,$15,$02,$60,$4e,$14,$02,$60,$08,$78,$85,$40,$84,$41,$38
	.byt $6e,$14,$02,$28,$60,$a0,$00,$ad,$13,$02,$20,$90,$ca,$a9,$3a,$91
	.byt $40,$c8,$ad,$12,$02,$20,$90,$ca,$a9,$3a,$91,$40,$c8,$ad,$11,$02
	.byt $a2,$2f,$38,$e9,$0a,$e8,$b0,$fb,$48,$8a,$91,$40,$68,$c8,$69,$3a
	.byt $91,$40,$c8,$60,$e5,$c6,$e8,$c6,$eb,$c6,$ee,$c6,$20,$c7,$23,$c7
	.byt $26,$c7,$29,$c7,$cf,$c7,$d2,$c7,$d5,$c7,$d8,$c7,$06,$c8,$09,$c8
	.byt $0c,$c8,$0f,$c8,$5d,$c7,$62,$c7,$67,$c7,$6c,$c7,$a8,$c7,$ab,$c7
	.byt $ae,$c7,$b1,$c7,$6c,$cd,$75,$cf,$45,$cf,$06,$cf,$14,$cf,$31,$ff
	.byt $bf,$c6,$f0,$d0,$69,$ce,$97,$ce,$89,$ce,$dc,$ce,$f0,$cf,$56,$c7
	.byt $49,$e7,$00,$00,$ef,$cd,$39,$ce,$54,$ce,$9b,$f4,$e0,$cb,$35,$e4
	.byt $b0,$e6,$80,$e6,$40,$d1,$11,$d7,$37,$e5,$6c,$e6,$1e,$de,$20,$de
	.byt $fb,$de,$54,$de,$5c,$de,$f7,$fe,$42,$d4,$f1,$d4,$55,$ca,$65,$ca
	.byt $69,$ca,$00,$00,$e9,$d9,$1a,$da,$d8,$dd,$0d,$eb,$73,$eb,$5a,$eb
	.byt $ec,$eb,$df,$eb,$72,$da,$e4,$da,$b9,$e1,$09,$e2,$50,$e2,$00,$00
	.byt $00,$00,$00,$00,$03,$d9,$1f,$d8,$4c,$ff,$00,$00,$1d,$c5,$18,$c5
	.byt $0f,$c5,$0c,$c5,$07,$c5,$ea,$c4,$b1,$cf,$00,$00,$9a,$ed,$77,$ed
	.byt $e5,$ed,$ca,$ed,$fc,$ed,$d7,$ed,$a5,$ee,$4a,$ef,$20,$ef,$3f,$ef
	.byt $7a,$ef,$85,$ef,$a5,$f4,$1e,$f9,$b2,$ef,$9b,$ef,$8b,$f1,$8a,$f2
	.byt $1a,$f6,$53,$f6,$8e,$f7,$81,$f7,$0a,$f8,$35,$f8,$8c,$f6,$46,$f1
	.byt $6f,$f2,$35,$f7,$10,$f6,$b6,$f8,$aa,$f8,$6a,$f4,$14,$f3,$71,$f7
	.byt $87,$f3,$77,$f3,$ed,$f3,$23,$f3,$a6,$f3,$52,$f3,$96,$f3,$cd,$f8
	.byt $12,$fa,$00,$00,$e7,$e7,$d9,$e7,$c1,$e7,$cd,$e7,$92,$e7,$66,$e8
	.byt $85,$e8,$cb,$e9,$2f,$e9,$3c,$e9,$5d,$e9,$5f,$e9,$19,$e8,$2c,$e8
	.byt $73,$ea,$af,$ea,$93,$ea,$00,$00,$00,$00,$00,$00,$e5,$eb,$d9,$eb
	.byt $84,$60,$85,$66,$86,$63,$a2,$00,$20,$1e,$de,$a4,$62,$a6,$66,$2c
	.byt $e8,$c8,$20,$f9,$cc,$30,$04,$c4,$63,$d0,$f5,$86,$67,$a6,$60,$38
	.byt $8a,$e5,$66,$18,$65,$62,$a8,$20,$d3,$cc,$20,$06,$c8,$48,$2c,$0d
	.byt $02,$50,$0d,$a9,$08,$20,$5d,$c7,$a9,$20,$20,$5d,$c7,$4c,$2e,$cc
	.byt $a4,$61,$a6,$65,$b1,$26,$29,$7f,$91,$26,$c8,$ca,$d0,$f6,$68,$c9
	.byt $20,$f0,$08,$c9,$1b,$f0,$04,$c9,$0d,$d0,$03,$a6,$60,$60,$c9,$0a
	.byt $d0,$2b,$a5,$60,$c5,$67,$f0,$05,$e6,$60,$4c,$fd,$cb,$24,$68,$30
	.byt $ac,$e6,$60,$e6,$67,$e6,$66,$2c,$0d,$02,$70,$8f,$a6,$62,$a4,$63
	.byt $20,$54,$de,$a4,$63,$a6,$60,$20,$f9,$cc,$4c,$fd,$cb,$c9,$0b,$d0
	.byt $29,$a5,$60,$c5,$66,$d0,$1b,$a5,$60,$f0,$19,$c6,$66,$c6,$67,$c6
	.byt $60,$2c,$0d,$02,$70,$11,$a6,$62,$a4,$63,$20,$5c,$de,$a4,$62,$4c
	.byt $65,$cc,$c6,$60,$4c,$fd,$cb,$4c,$eb,$cb,$c9,$30,$90,$f6,$c9,$3a
	.byt $b0,$f2,$a6,$60,$e0,$19,$90,$06,$a6,$66,$86,$60,$b0,$e6,$48,$06
	.byt $60,$a5,$60,$06,$60,$06,$60,$65,$60,$85,$60,$68,$29,$0f,$65,$60
	.byt $e9,$00,$85,$60,$90,$e2,$c5,$66,$90,$de,$c5,$67,$f0,$02,$b0,$d8
	.byt $4c,$fd,$cb,$20,$5a,$cd,$2c,$0d,$02,$50,$0f,$a2,$02,$a9,$09,$20
	.byt $5d,$c7,$ca,$10,$f8,$a9,$2d,$4c,$5d,$c7,$a4,$61,$a6,$65,$b1,$26
	.byt $09,$80,$91,$26,$c8,$ca,$d0,$f6,$60,$98,$48,$8a,$48,$48,$20,$5a
	.byt $cd,$e8,$a5,$69,$a4,$6a,$85,$15,$84,$16,$a0,$00,$ca,$f0,$11,$c8
	.byt $d0,$02,$e6,$16,$20,$11,$04,$d0,$f6,$c8,$d0,$f0,$e6,$16,$d0,$ec
	.byt $a6,$16,$18,$98,$65,$15,$90,$01,$e8,$85,$02,$86,$03,$a9,$20,$85
	.byt $14,$68,$18,$69,$01,$a0,$00,$a2,$01,$20,$39,$ce,$a9,$20,$20,$5d
	.byt $c7,$a5,$02,$a4,$03,$20,$a8,$c7,$a0,$01,$20,$11,$04,$38,$f0,$01
	.byt $18,$66,$68,$68,$aa,$68,$a8,$24,$68,$60,$a9,$1f,$20,$5d,$c7,$98
	.byt $09,$40,$20,$5d,$c7,$a5,$61,$09,$40,$4c,$5d,$c7,$48,$8a,$48,$98
	.byt $48,$38,$a5,$06,$e5,$04,$a8,$a5,$07,$e5,$05,$aa,$90,$3b,$86,$0b
	.byt $a5,$08,$c5,$04,$a5,$09,$e5,$05,$b0,$35,$98,$49,$ff,$69,$01,$a8
	.byt $85,$0a,$90,$03,$ca,$e6,$07,$38,$a5,$08,$e5,$0a,$85,$08,$b0,$02
	.byt $c6,$09,$18,$a5,$07,$e5,$0b,$85,$07,$e8,$b1,$06,$91,$08,$c8,$d0
	.byt $f9,$e6,$07,$e6,$09,$ca,$d0,$f2,$38,$68,$a8,$68,$aa,$68,$60,$8a
	.byt $18,$65,$05,$85,$05,$8a,$18,$65,$09,$85,$09,$e8,$88,$b1,$04,$91
	.byt $08,$98,$d0,$f8,$c6,$05,$c6,$09,$ca,$d0,$f1,$f0,$db,$0a,$64,$e8
	.byt $10,$00,$00,$03,$27,$a2,$00,$a0,$00,$2c,$a2,$03,$2c,$a2,$02,$85
	.byt $0d,$84,$0e,$a9,$00,$85,$0f,$85,$10,$a9,$ff,$85,$0c,$e6,$0c,$38
	.byt $a5,$0d,$a8,$fd,$dd,$cd,$85,$0d,$a5,$0e,$48,$fd,$e1,$cd,$85,$0e
	.byt $68,$b0,$ea,$84,$0d,$85,$0e,$a5,$0c,$f0,$04,$85,$0f,$d0,$07,$a4
	.byt $0f,$d0,$03,$a5,$14,$2c,$09,$30,$20,$32,$ce,$ca,$10,$cb,$a5,$0d
	.byt $09,$30,$a4,$10,$91,$11,$e6,$10,$60,$48,$a9,$00,$85,$11,$a9,$01
	.byt $85,$12,$68,$20,$ef,$cd,$a0,$00,$b9,$00,$01,$20,$5d,$c7,$c8,$c4
	.byt $10,$d0,$f5,$60,$48,$29,$0f,$20,$60,$ce,$a8,$68,$4a,$4a,$4a,$4a
	.byt $09,$30,$c9,$3a,$90,$02,$69,$06,$60,$a0,$00,$85,$00,$84,$01,$0a
	.byt $26,$01,$0a,$26,$01,$65,$00,$90,$02,$e6,$01,$0a,$26,$01,$0a,$26
	.byt $01,$0a,$26,$01,$85,$00,$a4,$01,$60,$18,$65,$00,$85,$00,$48,$98
	.byt $65,$01,$85,$01,$a8,$68,$60,$85,$10,$84,$11,$a2,$00,$86,$0c,$86
	.byt $0d,$86,$0e,$86,$0f,$86,$02,$86,$03,$a2,$10,$46,$11,$66,$10,$90
	.byt $19,$18,$a5,$00,$65,$0c,$85,$0c,$a5,$01,$65,$0d,$85,$0d,$a5,$02
	.byt $65,$0e,$85,$0e,$a5,$03,$65,$0f,$85,$0f,$06,$00,$26,$01,$26,$02
	.byt $26,$03,$a5,$10,$05,$11,$f0,$03,$ca,$d0,$d0,$60,$85,$0c,$84,$0d
	.byt $a2,$00,$86,$02,$86,$03,$a2,$10,$06,$00,$26,$01,$26,$02,$26,$03
	.byt $38,$a5,$02,$e5,$0c,$a8,$a5,$03,$e5,$0d,$90,$06,$84,$02,$85,$03
	.byt $e6,$00,$ca,$d0,$e3,$60,$a9,$00,$a0,$a0,$85,$00,$84,$01,$a0,$68
	.byt $a2,$bf,$a9,$40,$48,$38,$98,$e5,$00,$a8,$8a,$e5,$01,$aa,$84,$02
	.byt $68,$a0,$00,$c4,$02,$b0,$05,$91,$00,$c8,$d0,$f7,$48,$98,$a0,$00
	.byt $20,$89,$ce,$68,$e0,$00,$f0,$0c,$a0,$00,$91,$00,$c8,$d0,$fb,$e6
	.byt $01,$ca,$d0,$f6,$60,$a2,$00,$a0,$ff,$8c,$aa,$02,$c8,$20,$f3,$e7
	.byt $ad,$0d,$02,$30,$b1,$09,$80,$8d,$0d,$02,$08,$78,$a9,$1f,$8d,$67
	.byt $bf,$20,$a4,$cf,$20,$d8,$fe,$a9,$5c,$a0,$02,$a2,$00,$20,$fd,$de
	.byt $20,$06,$cf,$28,$60,$ad,$0d,$02,$10,$29,$08,$78,$29,$7f,$8d,$0d
	.byt $02,$20,$db,$fe,$a9,$56,$a0,$02,$a2,$00,$20,$fd,$de
	
	
	.byt $a9,$1a,$8d
	.byt $df,$bf,$20,$a4,$cf,$a2,$28,$a9,$20,$9d,$7f,$bb,$ca,$d0,$fa,$20
	.byt $20,$de,$28,$60,$a0,$1f,$a2,$00,$ca,$d0,$fd,$88,$d0,$fa,$60,$38
	.byt $24,$18,$66,$15,$a2,$00,$20,$0f,$c5,$90,$08,$8a,$69,$0b,$aa,$e0
	.byt $30,$d0,$f3,$08,$a9,$dc,$a0,$cf,$b0,$04,$a9,$e6,$a0,$cf,$24,$15
	.byt $10,$05,$20,$f9,$fe,$28,$60,$20,$f9,$fe,$28,$60,$7f,$00,$00,$08
	.byt $3c,$3e,$3c,$08,$00,$00,$7f,$00,$00,$08,$34,$32,$34,$08,$00,$00
	.byt $85,$15,$84,$16,$86,$00,$e6,$00,$ac,$0c,$02,$8c,$17,$05,$8c,$00
	.byt $05,$a0,$0c,$a9,$3f,$99,$17,$05,$88,$d0,$fa,$8a,$f0,$3b,$e0,$01
	.byt $d0,$22,$20,$df,$d0,$38,$e9,$41,$c9,$04,$b0,$40,$8d,$17,$05,$8d
	.byt $00,$05,$a2,$01,$a0,$0c,$a9,$3f,$d9,$17,$05,$f0,$05,$88,$d0,$f8
	.byt $18,$60,$38,$60,$a0,$01,$20,$df,$d0,$c9,$2d,$d0,$1f,$a0,$00,$20
	.byt $df,$d0,$38,$e9,$41,$b0,$03,$a2,$81,$60,$c9,$04,$b0,$f9,$e0,$02
	.byt $d0,$04,$8d,$0c,$02,$60,$8d,$17,$05,$a0,$02,$2c,$a0,$00,$a2,$00
	.byt $20,$df,$d0,$b0,$1d,$c9,$2e,$f0,$19,$c9,$2a,$f0,$22,$20,$fb,$d0
	.byt $90,$06,$a2,$80,$60,$a2,$82,$60,$e0,$09,$f0,$f9,$9d,$18,$05,$e8
	.byt $d0,$de,$a9,$20,$e0,$09,$f0,$06,$9d,$18,$05,$e8,$d0,$f6,$88,$a2
	.byt $00,$20,$df,$d0,$90,$0e,$a0,$02,$b9,$5d,$05,$99,$21,$05,$88,$10
	.byt $f7,$4c,$22,$d0,$c9,$2e,$d0,$ca,$20,$df,$d0,$b0,$e1,$88,$20,$df
	.byt $d0,$90,$0e,$a9,$20,$e0,$03,$f0,$e8,$9d,$21,$05,$e8,$d0,$f6,$f0
	.byt $e0,$c9,$2a,$d0,$08,$20,$df,$d0,$b0,$d7,$a2,$83,$60,$20,$fb,$d0
	.byt $b0,$a0,$e0,$03,$f0,$06,$9d,$21,$05,$e8,$d0,$d2,$a2,$84,$60,$20
	.byt $11,$04,$20,$f0,$d0,$c8,$c4,$00,$b0,$05,$c9,$20,$f0,$f1,$18,$60
	.byt $c9,$61,$90,$06,$c9,$7b,$b0,$02,$e9,$1f,$60,$c9,$3f,$f0,$0e,$c9
	.byt $30,$90,$0c,$c9,$3a,$90,$06,$c9,$41,$90,$04,$c9,$5b,$18,$60,$38
	.byt $60,$a0,$00,$a9,$90,$84,$00,$85,$01,$a2,$94,$a9,$00,$20,$14,$cf
	.byt $a0,$00,$a9,$94,$84,$00,$85,$01,$a2,$98,$a9,$87,$20,$14,$cf,$a0
	.byt $00,$a9,$a0,$84,$00,$85,$01,$a0,$3f,$a2,$bf,$a9,$10,$4c,$14,$cf
	.byt $a5,$39,$20,$69,$ce,$48,$98,$48,$a9,$00,$a0,$90,$20,$89,$ce,$85
	.byt $2e,$84,$2f,$85,$30,$c8,$c8,$c8,$c8,$84,$31,$68,$85,$01,$68,$0a
	.byt $26,$01,$0a,$26,$01,$0a,$26,$01,$85,$00,$a9,$00,$a0,$a0,$20,$89
	.byt $ce,$85,$2c,$84,$2d,$4c,$56,$d7,$85,$3e,$98,$48,$8a,$48,$a5,$39
	.byt $f0,$08,$8d,$81,$02,$a5,$38,$8d,$80,$02,$24,$3c,$30,$5f,$a5,$3e
	.byt $20,$42,$d4,$85,$00,$f0,$4f,$c9,$20,$90,$58,$c9,$a0,$b0,$42,$8d
	.byt $85,$02,$29,$7f,$aa,$a5,$34,$30,$12,$a4,$38,$c0,$27,$d0,$02,$29
	.byt $df,$a4,$39,$c0,$02,$b0,$04,$29,$ef,$85,$34,$48,$20,$30,$d5,$20
	.byt $dc,$d5,$68,$48,$30,$0a,$29,$20,$f0,$06,$20,$91,$d3,$20,$59,$d7
	.byt $20,$91,$d3,$68,$30,$0b,$a6,$38,$d0,$07,$29,$10,$f0,$03,$20,$a0
	.byt $d3,$ad,$85,$02,$85,$00,$68,$aa,$68,$a8,$a5,$00,$60,$20,$2e,$d2
	.byt $4c,$e6,$d1,$20,$f9,$d1,$4c,$e6,$d1,$aa,$18,$bd,$0b,$d2,$69,$7e
	.byt $85,$00,$a9,$d3,$69,$00,$85,$01,$6c,$00,$00,$00,$00,$00,$00,$00
	.byt $00,$00,$01,$04,$10,$22,$44,$53,$59,$63,$76,$00,$7d,$b3,$b6,$80
	.byt $00,$b9,$00,$83,$b9,$00,$bc,$00,$00,$a7,$bf,$4c,$b7,$d2,$a5,$3c
	.byt $29,$03,$85,$36,$a5,$3c,$0a,$30,$f2,$0a,$0a,$30,$11,$a5,$3e,$29
	.byt $3f,$aa,$46,$3c,$ad,$85,$02,$20,$78,$d1,$ca,$d0,$f7,$60,$a5,$36
	.byt $f0,$1d,$a5,$3e,$c9,$30,$90,$12,$c9,$59,$b0,$0e,$8d,$82,$02,$c6
	.byt $3c,$a9,$07,$85,$34,$a9,$00,$85,$32,$60,$46,$3c,$4c,$78,$d1,$46
	.byt $3c,$a5,$3e,$c9,$30,$90,$f3,$c9,$69,$b0,$ef,$29,$3f,$8d,$83,$02
	.byt $a5,$3e,$c9,$40,$b0,$1d,$ad,$82,$02,$29,$03,$0a,$0a,$6d,$82,$02
	.byt $e9,$2f,$0a,$6d,$83,$02,$e9,$2f,$85,$39,$20,$d7,$d3,$20,$59,$d7
	.byt $4c,$40,$d1,$20,$59,$d7,$ad,$83,$02,$85,$38,$c6,$38,$ad,$82,$02
	.byt $29,$3f,$85,$39,$4c,$40,$d1,$a6,$36,$a5,$3e,$e0,$03,$d0,$22,$8d
	.byt $82,$02,$a2,$00,$86,$35,$c9,$36,$f0,$0d,$c9,$39,$90,$22,$c9,$3c
	.byt $b0,$1e,$29,$03,$e9,$00,$2c,$a9,$00,$09,$c0,$85,$3c,$60,$46,$3c
	.byt $60,$e6,$35,$a6,$35,$9d,$81,$02,$c6,$3c,$c6,$36,$10,$ef,$30,$ee
	.byt $c9,$40,$90,$e9,$46,$3c,$c9,$48,$b0,$0d,$29,$07,$85,$36,$a5,$34
	.byt $29,$f8,$05,$36,$85,$34,$60,$c9,$4a,$b0,$0c,$4a,$a5,$34,$29,$f7
	.byt $b0,$02,$09,$08,$85,$34,$60,$c9,$4c,$90,$2f,$c9,$50,$b0,$11,$29
	.byt $03,$0a,$0a,$0a,$0a,$85,$36,$a5,$34,$29,$cf,$05,$36,$85,$34,$60
	.byt $c9,$58,$b0,$17,$29,$07,$0a,$0a,$0a,$0a,$85,$36,$a5,$32,$29,$84
	.byt $05,$36,$24,$34,$30,$02,$09,$80,$85,$32,$60,$d0,$00,$c9,$5b,$b0
	.byt $1b,$4a,$24,$34,$10,$09,$a9,$00,$90,$02,$a9,$40,$85,$33,$60,$a5
	.byt $32,$29,$70,$b0,$02,$09,$04,$09,$80,$85,$32,$60,$f0,$fd,$c9,$5e
	.byt $b0,$0c,$4a,$a5,$34,$29,$bf,$90,$02,$09,$40,$85,$34,$60,$60,$4c
	.byt $d8,$dd,$20,$59,$d7,$a5,$38,$f0,$35,$c6,$38,$4c,$56,$d7,$20,$59
	.byt $d7,$e6,$38,$a5,$38,$c9,$28,$90,$f2,$a5,$39,$f0,$ec,$20,$d7,$d3
	.byt $20,$59,$d7,$a6,$39,$f0,$0c,$e0,$18,$d0,$02,$a2,$00,$e8,$86,$39
	.byt $4c,$40,$d1,$ad,$80,$02,$ae,$81,$02,$85,$38,$4c,$ae,$d3,$a9,$27
	.byt $85,$38,$20,$59,$d7,$a6,$39,$ca,$d0,$02,$a2,$18,$86,$39,$4c,$40
	.byt $d1,$20,$11,$d1,$4c,$25,$d4,$20,$59,$d7,$a9,$00,$85,$38,$4c,$56
	.byt $d7,$a9,$40,$85,$33,$a5,$32,$29,$74,$85,$32,$a5,$34,$29,$0f,$09
	.byt $80,$85,$34,$60,$a5,$34,$29,$0f,$85,$34,$60,$4c,$4f,$d7,$4c,$4d
	.byt $d7,$a5,$38,$48,$a5,$39,$48,$a9,$20,$20,$78,$d1,$a5,$38,$f0,$09
	.byt $c9,$27,$d0,$f3,$a9,$20,$20,$78,$d1,$20,$59,$d7,$68,$85,$39,$68
	.byt $85,$38,$4c,$40,$d1,$20,$d7,$d3,$20,$61,$d2,$20,$59,$d7,$4c,$ab
	.byt $d3,$a9,$89,$2c,$a9,$84,$2c,$a9,$a1,$2c,$a9,$c3,$2c,$a9,$91,$85
	.byt $3c,$60,$a4,$37,$24,$37,$08,$a2,$00,$86,$37,$28,$30,$1c,$70,$16
	.byt $c9,$13,$f0,$0b,$c9,$19,$f0,$04,$c9,$16,$d0,$09,$a9,$80,$2c,$a9
	.byt $40,$85,$37,$a9,$00,$60,$18,$69,$5f,$60,$70,$1e,$a2,$14,$dd,$a7
	.byt $d4,$f0,$06,$ca,$10,$f8,$a9,$5f,$60,$e0,$05,$b0,$08,$8a,$09,$c0
	.byt $85,$37,$a9,$00,$60,$29,$1f,$09,$80,$60,$48,$98,$29,$07,$aa,$bd
	.byt $c1,$d4,$a8,$bd,$bc,$d4,$aa,$68,$dd,$c6,$d4,$f0,$06,$e8,$88,$d0
	.byt $f7,$aa,$60,$bd,$de,$d4,$60,$41,$42,$43,$48,$4b,$20,$23,$24,$26
	.byt $2c,$2d,$2e,$2f,$30,$31,$38,$3c,$3d,$3e,$6a,$7a,$00,$05,$07,$0e
	.byt $11,$05,$02,$07,$03,$02,$41,$61,$45,$65,$75,$45,$65,$41,$61,$45
	.byt $65,$75,$69,$6f,$45,$65,$69,$43,$63,$41,$42,$43,$48,$4b,$87,$97
	.byt $89,$99,$88,$82,$92,$81,$86,$8b,$9b,$96,$80,$9f,$84,$93,$94,$85
	.byt $95,$a2,$00,$c9,$a0,$90,$05,$e9,$5f,$a0,$13,$60,$a8,$30,$04,$a8
	.byt $a9,$00,$60,$a0,$12,$d9,$de,$d4,$f0,$13,$88,$10,$f8,$18,$69,$a0
	.byt $c9,$2a,$f0,$04,$c9,$3a,$d0,$02,$09,$40,$a0,$19,$60,$98,$a2,$04
	.byt $dd,$bc,$d4,$b0,$03,$ca,$d0,$f8,$bd,$d9,$d4,$be,$c6,$d4,$d0,$ea
	.byt $85,$36,$06,$36,$06,$36,$a8,$10,$26,$48,$8a,$c9,$60,$90,$02,$e9
	.byt $20,$38,$e9,$20,$85,$36,$a5,$33,$29,$40,$05,$36,$aa,$a5,$32,$29
	.byt $70,$85,$36,$68,$29,$8f,$05,$36,$a4,$38,$84,$36,$4c,$af,$d5,$e0
	.byt $20,$d0,$2b,$24,$32,$10,$27,$29,$70,$85,$35,$a5,$32,$29,$04,$09
	.byt $80,$05,$35,$aa,$a5,$32,$29,$74,$85,$32,$29,$70,$85,$35,$4a,$4a
	.byt $4a,$4a,$24,$34,$50,$04,$a5,$34,$29,$07,$05,$35,$09,$80,$24,$36
	.byt $50,$13,$c6,$2f,$c6,$31,$48,$38,$a5,$38,$e9,$28,$a8,$68,$20,$a7
	.byt $d5,$e6,$2f,$e6,$31,$a4,$38,$20,$af,$d5,$24,$36,$10,$08,$c8,$48
	.byt $8a,$91,$2e,$68,$91,$30,$60,$20,$75,$cf,$4c,$4d,$d7

routine_to_define_7
	LDA #$00
	STA $3C
	STA $3D
	STA $37
	rts
	;.byt $a9,$00,$85
	;.byt $3c,$85,$3d,$85,$37,$60
	.byt $10,$11,$b0,$ed,$20,$45,$cf,$20,$bd,$d5
	.byt $a9,$96,$a0,$d7,$20,$f9,$fe,$a9,$0c,$4c,$78,$d1,$46,$3d,$08,$26
	.byt $3d,$28,$b0,$2b,$a4,$38,$f0,$27,$48,$8a,$48,$88,$b1,$2e,$30,$1b
	.byt $aa,$b1,$30,$30,$06,$e0,$20,$d0,$12,$f0,$05,$8a,$29,$3f,$d0,$0b
	.byt $a5,$34,$29,$07,$c6,$38,$20,$48,$d6,$e6,$38,$68,$aa,$68,$18,$a8
	.byt $10,$41,$8a,$30,$25,$a0,$00,$a9,$9c,$84,$00,$85,$01,$8a,$85,$03
	.byt $20,$b2,$fe,$a2,$07,$bd,$00,$9c,$24,$03,$70,$03,$3d,$09,$d7,$09
	.byt $40,$9d,$00,$9c,$ca,$10,$ee,$4c,$b0,$d6,$a9,$00,$b0,$02,$a5,$32
	.byt $4a,$4a,$4a,$4a,$29,$07,$09,$10,$a2,$0f,$9d,$00,$9c,$ca,$10,$fa
	.byt $4c,$9f,$d6,$8a,$a2,$13,$86,$03,$0a,$26,$03,$0a,$26,$03,$0a,$26
	.byt $03,$85,$02,$a0,$07,$b1,$02,$09,$40,$99,$00,$9c,$88,$10,$f6,$a4
	.byt $38,$b1,$2e,$10,$06,$29,$04,$d0,$07,$f0,$0a,$88,$10,$f3,$30,$05
	.byt $a9,$3f,$8d,$07,$9c,$24,$36,$10,$16,$a2,$07,$bd,$00,$9c,$85,$02
	.byt $20,$f5,$d6,$9d,$08,$9c,$20,$f5,$d6,$9d,$00,$9c,$ca,$10,$ec,$24
	.byt $34,$50,$0d,$a0,$0f,$b9,$00,$9c,$09,$80,$99,$00,$9c,$88,$10,$f5
	.byt $a5,$2c,$a4,$2d,$24,$36,$50,$07,$38,$e9,$40,$88,$b0,$01,$88,$85
	.byt $00,$84,$01,$a2,$00,$46,$03,$a4,$38,$bd,$00,$9c,$91,$00,$24,$36
	.byt $10,$06,$bd,$08,$9c,$c8,$91,$00,$18,$a5,$00,$69,$28,$85,$00,$90
	.byt $02,$e6,$01,$24,$36,$50,$08,$a5,$03,$49,$80,$85,$03,$30,$d8,$e8
	.byt $e0,$08,$d0,$d3,$60,$a9,$00,$a0,$03,$46,$02,$08,$6a,$28,$6a,$88
	.byt $d0,$f7,$4a,$4a,$29,$3f,$09,$40,$60,$1b,$1b,$00,$1b,$00,$1b,$1b
	.byt $00,$a9,$00,$a2,$03,$a4,$3b,$f0,$09,$ca,$a9,$03,$88,$f0,$03,$e8
	.byt $a9,$05,$20,$69,$ce,$a5,$2c,$a4,$2d,$20,$89,$ce,$a9,$38,$a4,$3a
	.byt $f0,$02,$a9,$07,$85,$02,$a4,$38,$b1,$00,$0a,$30,$02,$a9,$80,$6a
	.byt $45,$02,$91,$00,$98,$18,$69,$28,$a8,$ca,$d0,$ec,$60,$18,$24,$38
	.byt $08,$06,$3d,$28,$66,$3d,$a9,$80,$2c,$a9,$00,$25,$3d,$24,$3d,$50
	.byt $02,$a9,$00,$85,$02,$a5,$2c,$a4,$2d,$85,$00,$84,$01,$a4,$38,$b1
	.byt $30,$30,$0a,$29,$40,$f0,$06,$a5,$02,$49,$80,$85,$02,$a2,$08,$a4
	.byt $38,$b1,$00,$29,$7f,$05,$02,$91,$00,$18,$98,$69,$28,$a8,$90,$02
	.byt $e6,$01,$ca,$d0,$ec,$60,$2f,$01,$02,$04,$04,$08,$08,$10,$20,$5c
	.byt $20,$10,$08,$08,$04,$04,$02,$01,$5f,$00,$00,$00,$00,$00,$00,$00
	.byt $3f,$60,$00,$00,$00,$3f,$00,$00,$00,$00,$7b,$20,$20,$20,$20,$20
	.byt $20,$20,$20,$7c,$08,$08,$08,$08,$08,$08,$08,$08,$7d,$01,$01,$01
	.byt $01,$01,$01,$01,$01,$7e,$3f,$00,$00,$00,$00,$00,$00,$00,$00,$20
	.byt $03,$d9,$f0,$2e,$ae,$70,$02,$10,$08,$ad,$71,$02,$3d,$e8,$01,$d0
	.byt $16,$88,$b9,$68,$02,$8d,$71,$02,$98,$09,$80,$8d,$70,$02,$20,$1f
	.byt $d8,$ad,$72,$02,$4c,$18,$d8,$ce,$74,$02,$d0,$0f,$20,$1f,$d8,$4c
	.byt $15,$d8,$8d,$70,$02,$ad,$73,$02,$8d,$74,$02,$60,$4c,$dd,$d8,$20
	.byt $bf,$c8,$a9,$00,$48,$ad,$70,$02,$0a,$0a,$0a,$a8,$ad,$71,$02,$4a
	.byt $b0,$03,$c8,$90,$fa,$ad,$6c,$02,$aa,$29,$90,$f0,$08,$68,$09,$01
	.byt $48,$98,$69,$3f,$a8,$98,$c9,$20,$90,$09,$e9,$08,$c9,$58,$90,$02
	.byt $e9,$08,$a8,$8a,$29,$20,$d0,$c4,$b1,$2a,$2c,$75,$02,$10,$0a,$c9
	.byt $61,$90,$06,$c9,$7b,$b0,$02,$e9,$1f,$a8,$8a,$29,$04,$f0,$12,$2d
	.byt $6f,$02,$f0,$05,$a9,$80,$8d,$7e,$02,$68,$09,$80,$48,$98,$29,$1f
	.byt $a8,$98,$a2,$00,$48,$c9,$06,$d0,$07,$ad,$75,$02,$49,$40,$b0,$23
	.byt $c9,$14,$f0,$1a,$c9,$17,$d0,$07,$ad,$75,$02,$49,$20,$b0,$14,$c9
	.byt $1b,$d0,$13,$ad,$75,$02,$29,$20,$f0,$0c,$68,$a9,$00,$48,$ad,$75
	.byt $02,$49,$80,$8d,$75,$02,$68,$a2,$00,$20,$1d,$c5,$68,$a2,$00,$20
	.byt $1d,$c5,$2c,$75,$02,$50,$07,$a2,$cf,$a0,$d8,$4c,$e7,$d9,$60,$1f
	.byt $00,$00,$00,$00,$00,$00,$3e,$10,$00,$00,$1f,$00,$00,$b1,$2a,$c9
	.byt $2d,$f0,$15,$c9,$3d,$f0,$14,$68,$09,$40,$48,$ad,$75,$02,$4a,$b0
	.byt $0f,$b1,$2a,$29,$1f,$09,$80,$2c,$a9,$60,$2c,$a9,$7e,$4c,$82,$d8
	.byt $6c,$76,$02

init_disk	
d903	
	LDY #$07
	LDA #$7F
loop21
	PHA
	TAX
	LDA #$0E
	JSR routine_to_define_11
	LDA #$00
	STA $0268,Y
	JSR routine_to_define_12 
	LDA $0300
	AND #$B8
	TAX
	CLC
	ADC #$08
	STA $1F
loop20	; d921
	STX $0300

	INX
	LDA #$08

	AND $0300
	BNE next7
loop23	
	CPX $1F
	bne loop20
	
d930
	beq next22

next7
	DEX
	TXA
	PHA
	AND #$07
	TAX
	LDA $D9A9,X
	ORA $0268,Y
	STA $0268,Y
	PLA
	TAX
	INX
	bne loop23
next22  ;$D946
	PLA
	SEC
	ROR
	DEY
	bpl loop21 ; D94A

	LDY #$08
loop22	
	LDA $0267,Y
	BNE out1
	CPY #$06
	BNE out2
	DEY
out2
	DEY
	bne loop22 ; d959

out1
	RTS
	/*
D95C   30 27      BMI $D985
D95E   A9 01      LDA #$01
D960   8D A8 02   STA $02A8
D963   8D A6 02   STA $02A6
D966   08         PHP
D967   78         SEI
D968   A2 00      LDX #$00
D96A   20 18 C5   JSR $C518
D96D   B0 13      BCS $D982
D96F   8D 79 02   STA $0279
D972   A2 00      LDX #$00
D974   20 18 C5   JSR $C518
D977   B0 09      BCS $D982
D979   8D 78 02   STA $0278
D97C   AD 79 02   LDA $0279
D97F   28         PLP
D980   18         CLC
D981   60         RTS
D982   28         PLP
D983   38         SEC
D984   60         RTS
	*/

	
	

	.byt $30,$27,$a9,$01
	.byt $8d,$a8,$02,$8d,$a6,$02,$08,$78,$a2,$00,$20,$18,$c5,$b0,$13,$8d
	.byt $79,$02,$a2,$00,$20,$18,$c5,$b0,$09,$8d,$78,$02,$ad,$79,$02,$28
	.byt $18,$60,$28,$38,$60
	
	
	.byt $90,$06,$a9,$40,$8d,$0e,$03,$60,$ad,$0b,$03
	.byt $09,$40,$8d,$0b,$03,$a9,$a8,$a0,$61,$8d,$04,$03,$8c,$05,$03,$a9
	.byt $c0,$8d,$0e,$03,$a2,$00,$4c,$0c,$c5,$01,$02,$04,$08,$10,$20,$40
	.byt $80
routine_to_define_4	

	LDA #$FF
	STA $0303
	STA $02A7
	LDA #$F7
	STA $0302
	LDA #$01
	STA $0273
	STA $0274
	STA $02A8
	STA $02A6
	LDA #$0E
	STA $0272
	LDA #$3F
	LDY #$FA
	STA $2A
	STY $2B
	LSR $0270
	LDA #$C0
	STA $0275
	LDA #$00
	STA $027E
	RTS

	.byt $18,$24,$38,$08,$78,$a5,$16,$48,$a5
	.byt $15,$48,$86,$15,$84,$16,$08,$a0,$00,$28,$08,$b0,$04,$b1,$15,$90
	.byt $03,$20,$11,$04,$aa,$98,$48,$20,$1a,$da,$68,$a8,$c8,$c0,$0e,$d0
	.byt $e8,$28,$68,$85,$15,$68,$85,$16,$28,$60
	
routine_to_define_11
	PHA
	STA $030F
	CMP #$07
	BNE next21
	TXA
	ORA #$40
	TAX
next21
	TYA
	PHA
	PHP
	SEI
	LDA $030C
	AND #$11
	TAY
	ORA #$EE
	STA $030C
	TYA
	ORA #$CC
	STA $030C
	STX $030F
	TYA
	ORA #$EC
	STA $030C
	TYA
	ORA #$CC
	STA $030C
	PLP
	PLA
	TAY
	PLA
	rts

init_printer	
da4f
	LDA #$07
	LDX #$7F
	JMP routine_to_define_11 
routine_to_define_8	
	LDA #$50
	STA LPRFX
	LDA #$00


	STA LPRX
	LDA #$80
	STA FLGLPR
	LDA #$53
	LDY #$E2
	STA $0250 ; LPRVEC ?
	STY $0251 ; LPRVEC ?
	rts
	
	
	
	
	.byt $30,$60,$48,$8a,$48,$a9,$82,$8d,$0e,$03,$ba,$bd,$02,$01,$20,$a5
	.byt $da,$2c,$8a,$02,$70,$1b,$c9,$20,$b0,$06,$c9,$0d,$d0,$13,$f0,$0c
	.byt $ae,$86,$02,$e8,$ec,$88,$02,$90,$05,$20,$e4,$da,$a2,$00,$8e,$86
	.byt $02,$68,$aa,$68,$60,$aa,$ad,$8a,$02,$29,$04,$f0,$08,$20,$2f,$db
	.byt $8a,$a2,$18,$d0,$0a,$ad,$0d,$02,$29,$02,$f0,$15,$8a,$a2,$24,$2c
	.byt $8a,$02,$70,$06,$c9,$7f,$d0,$02,$a9,$20,$48,$20,$1d,$c5,$68,$b0
	.byt $ee,$60,$b0,$0c,$ad,$8a,$02,$29,$04,$d0,$06,$a9,$82,$8d,$0e,$03
	.byt $60,$4c,$7d,$db,$48,$a9,$0d,$20,$72,$da,$ad,$8a,$02,$4a,$b0,$05
	.byt $a9,$0a,$20,$72,$da,$68,$60,$30,$05,$a2,$0c,$4c,$18,$c5,$b0,$09
	.byt $ad,$1e,$03,$29,$0d,$09,$60,$d0,$3a,$ad,$1e,$03,$09,$02,$8d,$1e
	.byt $03,$60,$30,$26,$aa,$10,$0f,$c9,$c0,$b0,$0b,$09,$40,$48,$a9,$1b
	.byt $a2,$18,$20,$1d,$c5,$68,$48,$a2,$18,$20,$1d,$c5,$68,$b0,$f7,$ad
	.byt $1e,$03,$29,$f3,$09,$04,$8d,$1e,$03,$60,$b0,$17,$ad,$1e,$03,$29
	.byt $02,$09,$65,$8d,$1e,$03,$ad,$21,$03,$29,$ef,$8d,$21,$03,$a9,$38
	.byt $8d,$1f,$03,$60
init_rs232
	; RS232T: 
	;	b0 to b3 : speed
	;	b4 : external clock for 0, 1 for internal clock
	;	b6 - b5 : 00=8 bits, 01=7 bits, 10=6 bits, 11=5 bits
	;	b7 : 0=stop, 1= 2 or 1.5 stops
#define RS232T $59
#define RS232C $5A
	LDA #$1E 
	STA RS232T
	LDA #$00
	STA RS232C
	rts


	.byt $10,$98,$b0
	.byt $a8,$ad,$1e,$03,$29,$0d,$05,$5a,$8d,$1e,$03,$ad,$21,$03,$09,$10
	.byt $8d,$21,$03,$a5,$59,$8d,$1f,$03,$60,$10,$ab,$b0,$d6,$ad,$1e,$03
	.byt $29,$02,$09,$05,$d0,$e0,$48,$08,$a9,$00,$f0,$10,$48,$08,$a9,$01
	.byt $d0,$0a,$48,$08,$a9,$02,$d0,$04,$48,$08,$a9,$03,$85,$28,$28,$10
	.byt $03,$4c,$ce,$de,$68,$85,$29,$ad,$8a,$02,$29,$02,$f0,$05,$a5,$29
	.byt $20,$72,$da,$a5,$29,$85,$29,$48,$8a,$48,$98,$48,$a6,$28,$bd,$18
	.byt $02,$85,$26,$bd,$1c,$02,$85,$27,$a5,$29,$c9,$20,$b0,$7e,$bd,$48
	.byt $02,$48,$20,$1e,$de,$a9,$dc,$48,$a9,$2a,$48,$a5,$29,$0a,$a8,$b9
	.byt $ec,$db,$48,$b9,$eb,$db,$48,$a9,$00,$38,$60,$e9,$dc,$ea,$dc,$e9
	.byt $dc,$e9,$dc,$0c,$dd,$e9,$dc,$e9,$dc,$d7,$dd,$46,$dd,$91,$dd,$9c
	.byt $dd,$54,$dd,$b7,$dd,$66,$dd,$73,$dd,$e9,$dc,$11,$dd,$12,$dd,$ce
	.byt $dd,$cb,$dd,$e9,$dc,$e9,$dc,$10,$dd,$e9,$dc,$79,$dd,$e9,$dc,$e9
	.byt $dc,$0e,$dd,$0e,$dd,$0f,$dd,$fa,$dd,$0d,$dd,$a6,$28,$bc,$20,$02
	.byt $b1,$26,$9d,$4c,$02,$a5,$26,$9d,$18,$02,$a5,$27,$9d,$1c,$02,$68
	.byt $9d,$48,$02,$20,$2d,$de,$68,$a8,$68,$aa,$68,$60,$bd,$48,$02,$29
	.byt $0c,$d0,$47,$a5,$29,$10,$06,$c9,$a0,$b0,$02,$29,$7f,$85,$29,$20
	.byt $6b,$dc,$a9,$09,$85,$29,$4c,$ce,$db,$85,$29,$a0,$80,$bd,$48,$02
	.byt $29,$20,$d0,$02,$a0,$00,$98,$05,$29,$9d,$4c,$02,$bc,$20,$02,$91
	.byt $26,$bd,$48,$02,$29,$02,$f0,$11,$bd,$24,$02,$dd,$34,$02,$f0,$09
	.byt $98,$69,$28,$a8,$bd,$4c,$02,$91,$26,$60,$29,$08,$f0,$1a,$a5,$29
	.byt $30,$a4,$c9,$40,$90,$a0,$29,$1f,$20,$69,$dc,$a9,$09,$20,$b5,$db
	.byt $a9,$1b,$20,$b5,$db,$4c,$46,$dc,$bd,$48,$02,$48,$20,$1e,$de,$68
	.byt $48,$4a,$b0,$18,$a5,$29,$29,$3f,$9d,$24,$02,$20,$07,$de,$9d,$18
	.byt $02,$98,$9d,$1c,$02,$68,$09,$01,$48,$4c,$2b,$dc,$a5,$29,$29,$3f
	.byt $9d,$20,$02,$68,$29,$fa,$48,$4c,$2b,$dc,$60,$bd,$20,$02,$29,$f8
	.byt $69,$07,$dd,$2c,$02,$f0,$12,$90,$10,$20,$67,$dd,$20,$9d,$dd,$a6
	.byt $28,$bd,$20,$02,$29,$07,$d0,$e3,$60,$9d,$20,$02,$60,$6a,$6a,$6a
	.byt $6a,$6a,$6a,$6a,$a8,$ba,$5d,$03,$01,$9d,$03,$01,$85,$00,$98,$29
	.byt $10,$d0,$01,$60,$a6,$28,$25,$00,$f0,$12,$fe,$28,$02,$fe,$28,$02
	.byt $bd,$20,$02,$dd,$28,$02,$b0,$03,$4c,$67,$dd,$60,$de,$28,$02,$de
	.byt $28,$02,$60,$de,$20,$02,$60,$bd,$20,$02,$dd,$28,$02,$d0,$f4,$bd
	.byt $2c,$02,$9d,$20,$02,$bd,$24,$02,$dd,$30,$02,$d0,$11,$bd,$30,$02
	.byt $bc,$34,$02,$aa,$20,$5c,$de,$bd,$28,$02,$9d,$20,$02,$60,$de,$24
	.byt $02,$4c,$07,$de,$bc,$28,$02,$4c,$7d,$dd,$bc,$20,$02,$bd,$2c,$02
	.byt $85,$29,$a9,$20,$91,$26,$c8,$c4,$29,$90,$f9,$91,$26,$60,$fe,$20
	.byt $02,$60,$bd,$20,$02,$dd,$2c,$02,$d0,$f4,$20,$67,$dd,$bd,$24,$02
	.byt $dd,$34,$02,$d0,$0d,$bd,$30,$02,$bc,$34,$02,$aa,$20,$54,$de,$4c
	.byt $67,$dd,$fe,$24,$02,$4c,$07,$de,$20,$fb,$dd,$20,$74,$dd,$bd,$24
	.byt $02,$dd,$34,$02,$f0,$35,$20,$9d,$dd,$4c,$bb,$dd,$4c,$b9,$e1,$a9
	.byt $02,$4d,$8a,$02,$8d,$8a,$02,$60,$a2,$f0,$a0,$dd,$20,$e7,$d9,$a0
	.byt $60,$a2,$00,$ca,$d0,$fd,$88,$d0,$fa,$a9,$07,$a2,$3f,$4c,$1a,$da
	.byt $46,$00,$00,$00,$00,$00,$00,$3e,$0f,$00,$00,$bd,$28,$02,$9d,$20
	.byt $02,$bd,$30,$02,$9d,$24,$02,$bd,$24,$02,$20,$12,$de,$85,$26,$84
	.byt $27,$60,$20,$69,$ce,$bd,$38,$02,$bc,$3c,$02,$4c,$89,$ce,$18,$24
	.byt $38,$08,$1e,$48,$02,$28,$7e,$48,$02,$30,$28,$a9,$80,$3d,$48,$02
	.byt $29,$80,$5d,$4c,$02,$bc,$20,$02,$91,$26,$48,$bd,$48,$02,$29,$02
	.byt $f0,$10,$bd,$24,$02,$dd,$34,$02,$f0,$08,$98,$69,$28,$a8,$68,$91
	.byt $26,$60,$68,$60,$a9,$00,$85,$07,$a9,$28,$d0,$06,$a9,$ff,$85,$07
	.byt $a9,$d8,$85,$06,$86,$00,$98,$38,$e5,$00,$48,$8a,$24,$06,$10,$01
	.byt $98,$a6,$28,$20,$12,$de,$18,$7d,$28,$02,$90,$01,$c8,$85,$08,$84
	.byt $09,$18,$65,$06,$85,$04,$98,$65,$07,$85,$05,$68,$85,$00,$f0,$34
	.byt $30,$3b,$38,$a6,$28,$bd,$2c,$02,$fd,$28,$02,$85,$01,$a4,$01,$b1
	.byt $04,$91,$08,$88,$10,$f9,$18,$a5,$04,$65,$06,$85,$04,$a5,$05,$65
	.byt $07,$85,$05,$18,$a5,$08,$65,$06,$85,$08,$a5,$09,$65,$07,$85,$09
	.byt $c6,$00,$d0,$d9,$a4,$01,$a9,$20,$91,$08,$88,$10,$fb,$60,$90,$07
	.byt $a6,$28,$20,$1e,$de,$68,$60,$a9,$01,$8d,$16,$02,$a9,$80,$8d,$17
	.byt $02,$68,$60
data_to_define_2
	; text mode  Text mode bytes it will  fill SCRTXT
	.byt $00,$27,$01,$1b
	.byt $80,$bb ; adress of text mode (first byte)
	; hires mode it will  fill SCRHIR
	.byt $00,$27,$00,$02
	.byt $68,$bf ; last bytes for text mode
	
	
	
	.byt $00
	.byt $27,$1a,$1b,$80,$bb,$00,$27,$01,$18,$80,$bb,$38,$24
ROUTINE_TO_DEFINE_7
	CLC
	PHP
	STA $15
	STY $16
	TXA
	CLC
	ADC #$18
	TAX
	LDY #$05
next18
	PLP
	PHP
	BCS next16
	LDA ($15),Y
	BCC next17
next16
	JSR $0411
next17
	STA $0224,X
	TXA
	SEC
	SBC #$04
	TAX
	DEY
	BPL next18
	LDA #$07
	STA $0240,X
	LDA #$00
	STA $0244,X
	LDA #$00
	STA $0248,X
	LDA $0228,X
	STA $0220,X
	LDA $0230,X
	STA $0224,X
	LDA $0238,X
	STA $0218,X
	LDA $023C,X
	STA $021C,X
	LDA #$20
	STA $024C,X
	LDA $28
	PHA
	STX $28
	LDA #$0C
	JSR $DBB5
	PLA
	STA $28
	PLP
	rts

init_screens
routine_to_define_9
	LDA #$1A
	STA $BFDF ; Switch to text mode
	JSR $FE49
	; fill the first line with space characters
	LDX #$27 ; loop on #$28 caracters
	LDA #$20
loop16
	STA $BB80,X  ; display space on first line in mode text
	DEX
	BPL loop16
	LDY #$11 ; loop with $12 to fill text definitions and Hires
loop17
#define SCRTXT $0256 ; desc scrtxt 6 bytes
#define SCRHIR $025C ; desc 6 bytes for HIres
#define SCRTRA $0262 ; desc 6 bytes for trace
	LDA data_to_define_2,Y ; data_to_define_2
	STA SCRTXT,Y ; thise fill also  SCRHIR
	DEY
	BPL loop17
	ASL $020D
	LSR $020D
	LDA #$F5
	LDY #$DE
	BIT $020D
	BVS next14
	LDA #$56
	LDY #$02
next14
	LDX #$00
	JMP ROUTINE_TO_DEFINE_7 ; $DEFD
	LDA $0320
	AND #$3F
	ORA #$40
	BNE next15
	LDA $0320
	AND #$3F
	ORA #$80
next15
	STA $0320
	LDA $0320
	AND #$1F
	rts

	.byt $38,$60
routine_to_define_5
	LDA #$41
	STA $028C
	LDX #$06
loop15
	LDA data_to_define_3,X ; data_to_define_3
	STA $029D,X
	DEX
	BPL loop15
	LDA #$01
	STA $0297
	STA $029C
	LDA #$06
	STA $0298
	STA $029B
	LDA #$01
	STA $0299
	LDA #$0A
	STA $029A
	LDA #$03
	STA $02A4
	STA $02A5
	LDA #$10
	LDY #$27
	STA $028F
	STY $0290
	STA $0308
	STY $0309
	LDA #$A0
	STA $030E
	RTS


data_to_define_3
	.byt $0b,$0a,$20,$08,$09,$03,$03
	
	.byt $60,$ad,$8d,$02,$29,$04
	.byt $d0,$12,$20,$90,$df,$29,$04,$d0,$15,$ce,$93,$02,$d0,$29,$ae,$97
	.byt $02,$4c,$1e,$e0,$20,$90,$df,$29,$04,$d0,$1c,$ae,$98,$02,$8e,$93
	.byt $02,$85,$58,$ad,$8d,$02,$29,$1b,$05,$58,$8d,$8d,$02,$a5,$58,$d0
	.byt $06,$ad,$9f,$02,$20,$9f,$e1,$ad,$8d,$02,$29,$1b,$49,$1b,$f0,$1b
	.byt $20,$90,$df,$29,$1b,$85,$58,$ad,$8d,$02,$29,$1b,$45,$58,$d0,$12
	.byt $ce,$91,$02,$d0,$2f,$ae,$97,$02,$4c,$65,$e0,$20,$90,$df,$29,$1b
	.byt $85,$58,$ae,$98,$02,$8e,$91,$02,$ad,$8d,$02,$29,$04,$05,$58,$8d
	.byt $8d,$02,$a2,$04,$09,$04,$4a,$48,$b0,$06,$bd,$9d,$02,$20,$9f,$e1
	.byt $68,$ca,$10,$f2,$60,$20,$99,$df,$29,$1b,$85,$58,$c9,$1b,$d0,$05
	.byt $ce,$a4,$02,$d0,$ef,$ad,$a5,$02,$8d,$a4,$02,$a5,$58,$c9,$1b,$f0
	.byt $14,$29,$1b,$4d,$8e,$02,$29,$1b,$d0,$0b,$ce,$92,$02,$d0,$31,$ae
	.byt $99,$02,$4c,$bb,$e0,$20,$99,$df,$ae,$9a,$02,$8e,$92,$02,$29,$1b
	.byt $85,$58,$ad,$8e,$02,$29,$64,$05,$58,$8d,$8e,$02,$a5,$58,$09,$04
	.byt $a2,$04,$4a,$48,$b0,$06,$bd,$9d,$02,$20,$9d,$e1,$68,$ca,$10,$f2
	.byt $60,$ad,$8e,$02,$29,$04,$d0,$12,$20,$99,$df,$29,$04,$d0,$13,$ce
	.byt $94,$02,$d0,$27,$ae,$97,$02,$4c,$02,$e1,$20,$99,$df,$29,$04,$ae
	.byt $98,$02,$85,$58,$8e,$94,$02,$ad,$8e,$02,$29,$7b,$05,$58,$8d,$8e
	.byt $02,$a5,$58,$d0,$06,$ad,$9f,$02,$20,$9d,$e1,$ad,$8e,$02,$29,$20
	.byt $d0,$15,$20,$99,$df,$ad,$2f,$03,$29,$20,$d0,$14,$ce,$95,$02,$d0
	.byt $2a,$ae,$9c,$02,$4c,$40,$e1,$20,$99,$df,$ad,$2f,$03,$ae,$9b,$02
	.byt $8e,$95,$02,$29,$20,$85,$58,$ad,$8e,$02,$29,$5f,$05,$58,$8d,$8e
	.byt $02,$29,$20,$d0,$06,$ad,$a2,$02,$20,$9d,$e1,$ad,$8e,$02,$29,$40
	.byt $d0,$15,$20,$99,$df,$ad,$2f,$03,$29,$80,$d0,$14,$ce,$96,$02,$d0
	.byt $2b,$ae,$9c,$02,$4c,$80,$e1,$20,$99,$df,$ad,$2f,$03,$ae,$9b,$02
	.byt $8e,$96,$02,$4a,$29,$40,$85,$58,$ad,$8e,$02,$29,$3f,$05,$58,$8d
	.byt $8e,$02,$29,$40,$d0,$06,$ad,$a3,$02,$4c,$9d,$e1,$60,$38,$24,$18
	.byt $08,$86,$58,$a2,$00,$20,$1d,$c5,$a9,$08,$28,$b0,$02,$a9,$20,$a2
	.byt $00,$20,$1d,$c5,$a6,$58,$60,$38,$60,$a6,$28,$bd,$20,$02,$48,$bd
	.byt $24,$02,$48,$a9,$1e,$20,$b5,$db,$20,$e4,$da,$a6,$28,$bc,$20,$02
	.byt $b1,$26,$c9,$20,$b0,$02,$a9,$20,$20,$72,$da,$bd,$20,$02,$dd,$2c
	.byt $02,$f0,$08,$a9,$09,$20,$b5,$db,$4c,$cb,$e1,$20,$e4,$da,$a6,$28
	.byt $bd,$24,$02,$dd,$34,$02,$d0,$eb,$a9,$1f,$20,$b5,$db,$68,$09,$40
	.byt $20,$b5,$db,$68,$09,$40,$4c,$b5,$db,$20,$e4,$da,$ad,$88,$02,$48
	.byt $a9,$28,$8d,$88,$02,$a9,$1e,$20,$78,$d1,$a4,$38,$b1,$30,$30,$06
	.byt $b1,$2e,$c9,$20,$b0,$02,$a9,$20,$20,$72,$da,$a9,$09,$20,$78,$d1
	.byt $a5,$38,$d0,$e6,$a4,$39,$88,$d0,$e1,$20,$e4,$da,$68,$8d,$86,$02
	.byt $60,$18,$33,$1b,$0a,$0d,$00,$f0,$4b,$1b,$0d,$0a,$40,$1b,$0a,$0a
	.byt $6c,$50,$02,$a2,$05,$ad,$8a,$02,$48,$09,$40,$8d,$8a,$02,$bd,$40
	.byt $e2,$20,$72,$da,$ca,$d0,$f7,$86,$0c,$a2,$06,$bd,$45,$e2,$20,$72
	.byt $da,$ca,$d0,$f7,$86,$0d,$a9,$05,$85,$0e,$a5,$0c,$0a,$0a,$0a,$20
	.byt $69,$ce,$85,$11,$98,$18,$69,$a0,$85,$12,$a9,$08,$85,$10,$a4,$0d
	.byt $b1,$11,$aa,$29,$40,$d0,$04,$8a,$29,$80,$aa,$8a,$10,$02,$49,$3f
	.byt $a6,$0e,$4a,$ca,$10,$fc,$26,$0f,$98,$18,$69,$28,$a8,$90,$02,$e6
	.byt $12,$c6,$10,$d0,$db,$a5,$0f,$20,$72,$da,$c6,$0e,$10,$bc,$e6,$0d
	.byt $a5,$0d,$c9,$28,$d0,$b0,$e6,$0c,$a5,$0c,$c9,$19,$d0,$9b,$a2,$04
	.byt $bd,$4b,$e2,$20,$72,$da,$ca,$d0,$f7,$68,$8d,$8a,$02,$60,$bc,$2c
	.byt $02,$24,$88,$b1,$00,$c9,$20,$d0,$07,$98,$dd,$28,$02,$d0,$f3,$60
	.byt $c9,$7f,$d0,$04,$98,$dd,$28,$02,$60,$bc,$28,$02,$b1,$00,$c9,$7f
	.byt $60,$a6,$28,$bd,$24,$02,$85,$61,$a5,$61,$20,$12,$de,$20,$f9,$e2
	.byt $f0,$0b,$a5,$61,$dd,$30,$02,$f0,$08,$c6,$61,$b0,$eb,$18,$c8,$84
	.byt $60,$60,$a6,$28,$bd,$24,$02,$85,$63,$20,$12,$de,$20,$de,$e2,$84
	.byt $62,$f0,$1b,$a5,$63,$dd,$34,$02,$f0,$13,$e6,$63,$a5,$63,$20,$12
	.byt $de,$20,$f9,$e2,$f0,$05,$20,$de,$e2,$d0,$e4,$c6,$63,$60,$60,$20
	.byt $01,$e3,$4c,$61,$e3,$a6,$28,$bd,$20,$02,$85,$60,$bd,$24,$02,$85
	.byt $61,$20,$22,$e3,$a5,$61,$85,$65,$c5,$63,$d0,$0c,$a5,$62,$c5,$60
	.byt $b0,$06,$a9,$00,$8d,$90,$05,$60,$a9,$00,$85,$64,$46,$66,$a5,$65
	.byt $20,$12,$de,$a4,$60,$a5,$65,$c5,$61,$f0,$05,$a6,$28,$bc,$28,$02
	.byt $b1,$00,$c9,$20,$b0,$02,$09,$80,$a6,$64,$24,$66,$10,$06,$a9,$20
	.byt $91,$00,$d0,$0d,$9d,$90,$05,$e6,$64,$e4,$67,$90,$04,$c6,$64,$66
	.byt $66,$98,$c8,$a6,$65,$e4,$63,$d0,$0c,$c5,$62,$d0,$d3,$a6,$64,$a9
	.byt $00,$9d,$90,$05,$60,$a6,$28,$dd,$2c,$02,$d0,$c4,$e6,$65,$d0,$ae
	.byt $66,$66,$a9,$00,$85,$64,$a5,$26,$a4,$27,$85,$00,$84,$01,$a6,$28
	.byt $bc,$20,$02,$a6,$64,$bd,$90,$05,$f0,$32,$a9,$20,$24,$66,$30,$0b
	.byt $bd,$90,$05,$10,$06,$c9,$a0,$b0,$02,$29,$1f,$91,$00,$2c,$0d,$02
	.byt $50,$03,$20,$56,$e6,$98,$c8,$a6,$28,$dd,$2c,$02,$d0,$0a,$a9,$28
	.byt $a0,$00,$20,$89,$ce,$bc,$28,$02,$e6,$64,$d0,$c7,$2c,$0d,$02,$50
	.byt $09,$ae,$20,$02,$ac,$24,$02,$20,$2a,$e6,$ac,$20,$02,$b1,$26,$a6
	.byt $28,$9d,$4c,$02,$60,$85,$67,$8a,$48,$98,$10,$0a,$20,$01,$e3,$a6
	.byt $60,$a4,$61,$20,$2a,$e6,$a9,$0d,$20,$48,$e6,$20,$6c,$e6,$68,$aa
	.byt $f0,$08,$a9,$09,$20,$48,$e6,$ca,$d0,$f8,$a6,$28,$bd,$48,$02,$30
	.byt $05,$a9,$11,$20,$48,$e6,$20,$af,$cf,$20,$cf,$c7,$b0,$f8,$48,$a9
	.byt $11,$20,$48,$e6,$68,$c9,$0d,$d0,$43,$48,$20,$4f,$e3,$68,$48,$c9
	.byt $0b,$f0,$11,$a6,$62,$a4,$63,$20,$2a,$e6,$a9,$0d,$20,$48,$e6,$a9
	.byt $0a,$20,$48,$e6,$a2,$ff,$e8,$bd,$90,$05,$c9,$20,$f0,$f8,$8a,$48
	.byt $a0,$00,$2c,$e8,$c8,$bd,$90,$05,$99,$90,$05,$d0,$f6,$a9,$90,$a0
	.byt $05,$20,$49,$e7,$85,$00,$84,$01,$68,$a8,$68,$60,$c9,$03,$f0,$b9
	.byt $c9,$0e,$d0,$0d,$20,$01,$e3,$a6,$60,$a4,$61,$20,$2a,$e6,$4c,$d5
	.byt $e4,$c9,$18,$d0,$0a,$20,$55,$e3,$38,$20,$d0,$e3,$4c,$5a,$e4,$c9
	.byt $7f,$d0,$47,$ad,$78,$02,$4a,$b0,$0a,$90,$00,$a9,$08,$2c,$a9,$09
	.byt $20,$48,$e6,$a6,$28,$bd,$4c,$02,$c9,$7f,$f0,$f2,$20,$55,$e3,$ad
	.byt $90,$05,$d0,$0d,$a9,$20,$20,$48,$e6,$a9,$08,$20,$48,$e6,$4c,$5a
	.byt $e4,$a2,$01,$bd,$90,$05,$f0,$06,$9d,$8f,$05,$e8,$d0,$f5,$a9,$20
	.byt $9d,$8f,$05,$18,$20,$d0,$e3,$4c,$5a,$e4,$c9,$20,$90,$06,$20,$37
	.byt $e5,$4c,$5a,$e4,$4c,$b9,$e5,$a8,$8a,$48,$98,$48,$20,$55,$e3,$a5
	.byt $62,$ac,$90,$05,$d0,$02,$a5,$60,$a6,$28,$dd,$2c,$02,$d0,$5f,$a5
	.byt $63,$dd,$34,$02,$f0,$58,$69,$01,$20,$12,$de,$20,$f9,$e2,$d0,$4e
	.byt $bc,$34,$02,$a6,$63,$e8,$20,$5c,$de,$2c,$0d,$02,$50,$40,$a2,$00
	.byt $a4,$63,$c8,$20,$2a,$e6,$a9,$18,$20,$56,$e6,$a9,$0a,$20,$48,$e6
	.byt $a6,$28,$bd,$4c,$02,$c9,$7f,$d0,$06,$20,$6c,$e6,$4c,$97,$e5,$20
	.byt $56,$e6,$a9,$09,$20,$b5,$db,$ad,$24,$02,$dd,$34,$02,$d0,$e1,$ad
	.byt $20,$02,$dd,$2c,$02,$d0,$d9,$a4,$61,$a6,$60,$20,$2a,$e6,$68,$20
	.byt $48,$e6,$18,$20,$d0,$e3,$68,$aa,$60,$c9,$08,$d0,$18,$48,$ad,$78
	.byt $02,$4a,$b0,$07,$68,$20,$48,$e6,$4c,$5a,$e4,$20,$01,$e3,$a6,$60
	.byt $a4,$61,$4c,$e7,$e5,$c9,$09,$d0,$15,$48,$ad,$78,$02,$4a,$90,$e4
	.byt $20,$22,$e3,$a6,$62,$a4,$63,$68,$20,$2a,$e6,$4c,$5a,$e4,$c9,$0a
	.byt $d0,$12,$a6,$28,$bd,$24,$02,$dd,$34,$02,$d0,$19,$a9,$0a,$2c,$a9
	.byt $0b,$4c,$79,$e4,$c9,$0b,$d0,$0f,$a6,$28,$bd,$24,$02,$dd,$30,$02
	.byt $f0,$ed,$a9,$0b,$2c,$a9,$0a,$c9,$0c,$d0,$09,$20,$48,$e6,$20,$6c
	.byt $e6,$4c,$5a,$e4,$20,$48,$e6,$4c,$5a,$e4,$a9,$1f,$20,$48,$e6,$98
	.byt $09,$40,$20,$48,$e6,$8a,$09,$40,$20,$b5,$db,$2c,$0d,$02,$50,$2b
	.byt $e8,$8a,$ca,$09,$40,$4c,$56,$e6,$2c,$0d,$02,$50,$03,$20,$56,$e6
	.byt $2c,$50,$e6,$4c,$86,$db,$85,$0c,$98,$48,$8a,$48,$a2,$18,$a5,$0c
	.byt $20,$1d,$c5,$68,$aa,$68,$a8,$a5,$0c,$b0,$eb,$60,$2c,$0d,$02,$50
	.byt $0a,$a9,$19,$20,$56,$e6,$a9,$2e,$20,$56,$e6,$a9,$7f,$4c,$b5,$db
	.byt $a5,$5c,$a6,$5d,$86,$03,$85,$02,$a0,$00,$b1,$02,$f0,$20,$aa,$a0
	.byt $02,$a5,$01,$d1,$02,$90,$17,$f0,$02,$b0,$09,$88,$a5,$00,$d1,$02
	.byt $90,$0c,$f0,$0b,$18,$8a,$65,$02,$90,$dc,$e6,$03,$b0,$d8,$18,$60
	.byt $85,$0e,$a9,$00,$85,$0f,$85,$10,$20,$80,$e6,$90,$2a,$86,$0f,$a5
	.byt $5e,$a4,$5f,$85,$06,$84,$07,$a5,$02,$a4,$03,$85,$08,$84,$09,$18
	.byt $8a,$65,$02,$90,$01,$c8,$85,$04,$84,$05,$20,$6c,$cd,$a9,$ff,$85
	.byt $10,$45,$0f,$85,$0f,$e6,$0f,$a5,$0e,$f0,$4d,$a5,$5e,$a4,$5f,$85
	.byt $06,$84,$07,$a5,$02,$a4,$03,$85,$04,$84,$05,$18,$a5,$0e,$69,$03
	.byt $48,$65,$02,$90,$01,$c8,$85,$08,$84,$09,$20,$6c,$cd,$18,$68,$48
	.byt $65,$0f,$85,$0f,$90,$02,$e6,$10,$a0,$00,$68,$91,$02,$c8,$a5,$00
	.byt $91,$02,$c8,$a5,$01,$91,$02,$a2,$00,$c8,$a1,$0c,$91,$02,$e6,$0c
	.byt $d0,$02,$e6,$0d,$c6,$0e,$d0,$f1,$18,$a5,$0f,$65,$5e,$85,$5e,$a4
	.byt $10,$98,$65,$5f,$85,$5f,$a5,$0f,$60,$85,$00,$84,$01,$a0,$00,$84
	.byt $02,$84,$03,$b1,$00,$c9,$30,$90,$2c,$c9,$3a,$b0,$28,$29,$0f,$48
	.byt $06,$02,$26,$03,$a5,$02,$a6,$03,$06,$02,$26,$03,$06,$02,$26,$03
	.byt $65,$02,$85,$02,$8a,$65,$03,$85,$03,$68,$65,$02,$85,$02,$90,$02
	.byt $e6,$03,$c8,$d0,$ce,$98,$aa,$a5,$02,$a4,$03,$60,$20,$10,$08,$04
	.byt $02,$01,$18,$24,$56,$10,$01,$38,$26,$56,$90,$24,$a4,$49,$b1,$4b
	.byt $0a,$10,$1d,$a6,$4a,$bd,$8c,$e7,$24,$57,$30,$0e,$50,$05,$11,$4b
	.byt $91,$4b,$60,$49,$7f,$31,$4b,$91,$4b,$60,$70,$04,$51,$4b,$91,$4b
	.byt $60,$18,$a5,$4b,$69,$28,$85,$4b,$90,$f6,$e6,$4c,$60,$38,$a5,$4b
	.byt $e9,$28,$85,$4b,$b0,$ea,$c6,$4c,$60,$a6,$4a,$e8,$e0,$06,$d0,$04
	.byt $a2,$00,$e6,$49,$86,$4a,$60,$a6,$4a,$ca,$10,$04,$a2,$05,$c6,$49
	.byt $86,$4a,$60,$84,$47,$86,$46,$98,$a0,$00,$20,$69,$ce,$85,$4b,$18
	.byt $98,$69,$a0,$85,$4c,$86,$00,$a9,$06,$a0,$00,$84,$01,$20,$dc,$ce
	.byt $a5,$00,$85,$49,$a5,$02,$85,$4a,$60,$18,$a5,$46,$85,$06,$65,$4d
	.byt $85,$08,$a5,$47,$85,$07,$65,$4f,$85,$09,$90,$0e,$a0,$06,$a2,$03
	.byt $b9,$4d,$00,$95,$06,$88,$88,$ca,$10,$f6,$a2,$03,$86,$05,$bd,$62
	.byt $e8,$85,$04,$a2,$06,$a9,$00,$95,$4e,$46,$04,$2a,$46,$04,$2a,$a8
	.byt $b9,$06,$00,$95,$4d,$ca,$ca,$10,$ec,$20,$66,$e8,$a6,$05,$ca,$10
	.byt $db,$60,$26,$67,$73,$32,$a6,$4d,$a4,$4f,$20,$f3,$e7,$a2,$ff,$38
	.byt $a5,$51,$e5,$4d,$85,$4d,$b0,$03,$86,$4e,$38,$a5,$53,$e5,$4f,$85
	.byt $4f,$b0,$02,$86,$50,$ad,$aa,$02,$85,$56,$20,$42,$e9,$86,$46,$84
	.byt $47,$24,$4e,$10,$08,$a5,$4d,$49,$ff,$85,$4d,$e6,$4d,$24,$50,$10
	.byt $08,$a5,$4f,$49,$ff,$85,$4f,$e6,$4f,$a5,$4d,$c5,$4f,$90,$3e,$08
	.byt $a5,$4d,$f0,$37,$a6,$4f,$20,$21,$e9,$28,$d0,$04,$a9,$ff,$85,$00
	.byt $24,$4e,$10,$06,$20,$e7,$e7,$4c,$cd,$e8,$20,$d9,$e7,$18,$a5,$00
	.byt $65,$02,$85,$02,$90,$0d,$24,$50,$30,$06,$20,$c1,$e7,$4c,$e3,$e8
	.byt $20,$cd,$e7,$20,$92,$e7,$c6,$4d,$d0,$d6,$60,$28,$60,$a5,$4f,$f0
	.byt $f9,$a6,$4d,$20,$21,$e9,$24,$50,$10,$06,$20,$cd,$e7,$4c,$03,$e9
	.byt $20,$c1,$e7,$18,$a5,$00,$65,$02,$85,$02,$90,$0d,$24,$4e,$10,$06
	.byt $20,$e7,$e7,$4c,$19,$e9,$20,$d9,$e7,$20,$92,$e7,$c6,$4f,$d0,$d6
	.byt $60,$86,$01,$a0,$00,$84,$00,$20,$dc,$ce,$a9,$ff,$85,$02,$60,$a6
	.byt $4d,$a4,$4f,$20,$4e,$e9,$20,$f3,$e7,$4c,$9c,$e7,$20,$42,$e9,$4c
	.byt $36,$e9,$18,$a5,$46,$65,$4d,$aa,$18,$a5,$47,$65,$4f,$a8,$e0,$f0
	.byt $b0,$05,$c0,$c8,$b0,$01,$60,$68,$8d,$ab,$02,$68,$60,$18,$24,$38
	.byt $48,$08,$86,$00,$24,$00,$30,$3f,$86,$28,$90,$05,$9d,$40,$02,$b0
	.byt $03,$9d,$44,$02,$bd,$48,$02,$29,$10,$d0,$0c,$a9,$0c,$20,$b5,$db
	.byt $a9,$1d,$20,$b5,$db,$a6,$28,$bd,$30,$02,$20,$69,$ce,$bd,$38,$02
	.byt $bc,$3c,$02,$20,$89,$ce,$bc,$28,$02,$88,$88,$38,$bd,$34,$02,$fd
	.byt $30,$02,$aa,$e8,$98,$b0,$0c,$a9,$00,$a2,$a0,$85,$00,$86,$01,$a2
	.byt $c8,$a9,$00,$28,$69,$00,$a8,$68,$91,$00,$48,$18,$a5,$00,$69,$28
	.byt $85,$00,$90,$02,$e6,$01,$68,$ca,$d0,$ee,$60,$a5,$46,$48,$a5,$47
	.byt $48,$ad,$aa,$02,$85,$56,$a5,$47,$38,$e5,$4d,$a8,$a6,$46,$20,$f3
	.byt $e7,$a2,$08,$a5,$4d,$ca,$0a,$10,$fc,$86,$0c,$a9,$80,$85,$0e,$85
	.byt $10,$0a,$85,$0f,$a5,$4d,$85,$11,$38,$66,$0d,$a5,$10,$a6,$11,$20
	.byt $62,$ea,$18,$a5,$0e,$65,$12,$85,$0e,$a5,$0f,$85,$12,$65,$13,$85
	.byt $0f,$c5,$12,$f0,$0d,$b0,$06,$20,$d9,$e7,$4c,$20,$ea,$20,$e7,$e7
	.byt $46,$0d,$a5,$0e,$a6,$0f,$20,$62,$ea,$38,$a5,$10,$e5,$12,$85,$10
	.byt $a5,$11,$85,$12,$e5,$13,$85,$11,$c5,$12,$f0,$0e,$b0,$06,$20,$c1
	.byt $e7,$4c,$4e,$ea,$20,$cd,$e7,$4c,$4e,$ea,$24,$0d,$30,$03,$20,$92
	.byt $e7,$a5,$0f,$d0,$a3,$a5,$11,$c5,$4d,$d0,$9d,$68,$a8,$68,$aa,$4c
	.byt $f3,$e7,$85,$12,$86,$13,$a6,$0c,$a5,$13,$2a,$66,$13,$66,$12,$ca
	.byt $d0,$f6,$60,$a5,$4b,$a4,$4c,$85,$00,$84,$01,$a6,$4f,$a4,$49,$a5
	.byt $51,$91,$00,$c8,$ca,$d0,$fa,$a9,$28,$a0,$00,$20,$89,$ce,$c6,$4d
	.byt $d0,$e9,$60,$85,$51,$84,$52,$86,$4f,$a9,$40,$85,$57,$a0,$00,$84
	.byt $50,$c4,$4f,$b0,$ed,$b1,$51,$20,$b5,$ea,$a4,$50,$c8,$d0,$f0,$a5
	.byt $4d,$0a,$46,$4f,$6a,$48,$a5,$46,$c9,$ea,$90,$17,$a6,$4a,$a5,$47
	.byt $69,$07,$a8,$e9,$bf,$90,$09,$f0,$07,$c9,$08,$d0,$02,$a9,$00,$a8
	.byt $20,$f3,$e7,$68,$20,$31,$ff,$a0,$00,$84,$00,$a5,$49,$48,$a5,$4a
	.byt $48,$b1,$02,$0a,$0a,$f0,$0c,$48,$10,$03,$20,$9c,$e7,$20,$d9,$e7
	.byt $68,$d0,$f1,$20,$c1,$e7,$68,$85,$4a,$68,$85,$49,$a4,$00,$c8,$c0
	.byt $08,$d0,$d6,$a5,$46,$69,$05,$aa,$a4,$47,$4c,$f3,$e7,$a5,$4f,$0a
	.byt $0a,$0a,$05,$4d,$49,$3f,$aa,$a9,$07,$20,$1a,$da,$06,$53,$26,$54
	.byt $a6,$53,$a9,$0b,$20,$1a,$da,$a6,$54,$a9,$0c,$20,$1a,$da,$a4,$51
	.byt $be,$38,$eb,$a9,$0d,$4c,$1a,$da,$00,$0b,$04,$08,$0a,$0b,$0c,$0d
	.byt $00,$00,$ee,$0e,$16,$0e,$4c,$0d,$8e,$0c,$d8,$0b,$2e,$0b,$8e,$0a
	.byt $f6,$09,$66,$09,$e0,$08,$60,$08,$e8,$07,$a4,$4f,$a5,$51,$0a,$aa
	.byt $bd,$40,$eb,$85,$4f,$bd,$41,$eb,$4a,$66,$4f,$88,$10,$fa,$85,$50
	.byt $a6,$53,$2c,$a6,$51,$8a,$d0,$02,$a2,$10,$a4,$4d,$88,$98,$c9,$03
	.byt $90,$02,$e9,$03,$09,$08,$20,$1a,$da,$c0,$03,$b0,$0c,$98,$0a,$a8
	.byt $69,$01,$a6,$50,$20,$1a,$da,$98,$2c,$a9,$06,$a6,$4f,$4c,$1a,$da
	.byt $18,$00,$00,$00,$00,$00,$00,$3e,$10,$00,$00,$00,$0f,$00,$00,$00
	.byt $00,$00,$00,$00,$0f,$07,$10,$10,$10,$00,$08,$00,$00,$00,$00,$00
	.byt $00,$00,$1f,$07,$10,$10,$10,$00,$18,$00,$00,$00,$00,$00,$00,$00
	.byt $00,$3e,$0f,$00,$00,$00,$00,$00,$00,$a2,$a0,$a0,$eb,$d0,$0a,$a2
	.byt $ae,$a0,$eb,$d0,$04,$a2,$bc,$a0,$eb,$4c,$e7,$d9,$a2,$ca,$a0,$eb
	.byt $20,$e7,$d9,$a9,$00,$aa,$8a,$48,$a9,$00,$20,$1a,$da,$a2,$00,$ca
	.byt $d0,$fd,$68,$aa,$e8,$e0,$70,$d0,$ed,$a9,$08,$a2,$00,$4c,$1a,$da
	.byt $a2,$0c,$4c,$5d,$db,$20,$10,$ec,$b0,$fb,$60,$2c,$1b,$ec,$4c,$79
	.byt $db,$24,$5b,$70,$24,$aa,$30,$10,$c9,$20,$b0,$1d,$69,$20,$48,$a9
	.byt $02,$20,$49,$ec,$68,$4c,$49,$ec,$c9,$a0,$b0,$04,$69,$c0,$b0,$ee
	.byt $29,$7f,$48,$a9,$01,$20,$49,$ec,$68,$2c,$49,$ec,$4c,$12,$db,$86
	.byt $0c,$84,$0d,$48,$24,$5b,$10,$06,$20,$21,$ec,$4c,$61,$ec,$20,$1b
	.byt $ec,$68,$45,$0e,$85,$0e,$a6,$0c,$a4,$0d,$60,$86,$0c,$84,$0d,$0e
	.byt $7e,$02,$90,$03,$68,$68,$60,$24,$5b,$30,$10,$20,$10,$ec,$b0,$ef
	.byt $48,$45,$0e,$85,$0e,$68,$a6,$0c,$a4,$0d,$60,$20,$b4,$ec,$b0,$df
	.byt $24,$5b,$70,$ec,$c9,$20,$b0,$e8,$48,$20,$b9,$ec,$aa,$68,$a8,$8a
	.byt $c0,$01,$d0,$04,$09,$80,$30,$d8,$c9,$40,$b0,$04,$e9,$1f,$b0,$d0
	.byt $69,$3f,$90,$cc,$a2,$0c,$4c,$18,$c5,$20,$b4,$ec,$b0,$fb,$60,$38
	.byt $24,$18,$a9,$80,$4c,$5d,$db,$38,$24,$18,$a9,$80,$4c,$79,$db,$38
	.byt $24,$18,$a9,$80,$4c,$f7,$da,$38,$24,$18,$a9,$80,$4c,$12,$db,$38
	.byt $ad,$2f,$05,$ed,$2d,$05,$8d,$2a,$05,$ad,$30,$05,$ed,$2e,$05,$8d
	.byt $2b,$05,$ad,$2d,$05,$ac,$2e,$05,$85,$00,$84,$01,$60,$a2,$32,$a9
	.byt $16,$20,$4f,$ec,$ca,$d0,$f8,$a9,$24,$20,$4f,$ec,$a9,$00,$85,$0e
	.byt $a2,$00,$bd,$18,$05,$20,$4f,$ec,$e8,$e0,$0c,$d0,$f5,$a9,$00,$20
	.byt $4f,$ec,$a2,$00,$bd,$2c,$05,$20,$4f,$ec,$e8,$e0,$07,$d0,$f5,$a5
	.byt $0e,$4c,$4f,$ec,$20,$6b,$ec,$c9,$16,$d0,$f9,$a2,$0a,$20,$6b,$ec
	.byt $c9,$16,$d0,$f0,$ca,$d0,$f6,$20,$6b,$ec,$c9,$16,$f0,$f9,$c9,$24
	.byt $d0,$e2,$a9,$00,$85,$0e,$20,$6b,$ec,$aa,$f0,$06,$20,$b5,$db,$4c
	.byt $56,$ed,$a2,$00,$20,$6b,$ec,$9d,$2c,$05,$e8,$e0,$07,$d0,$f5,$20
	.byt $6b,$ec,$09,$30,$4c,$b5,$db,$20,$c1,$ec,$20,$c9,$ec,$20,$10,$ec
	.byt $b0,$03,$20,$b5,$db,$20,$cf,$c7,$b0,$f3,$c9,$03,$f0,$06,$20,$1b
	.byt $ec,$4c,$7d,$ed,$20,$bf,$ec,$4c,$c7,$ec,$20,$c1,$ec,$0e,$7e,$02
	.byt $b0,$25,$20,$10,$ec,$b0,$f6,$aa,$30,$04,$c9,$20,$b0,$13,$48,$a9
	.byt $81,$20,$b5,$db,$68,$20,$54,$ce,$20,$b5,$db,$98,$20,$b5,$db,$a9
	.byt $87,$20,$b5,$db,$4c,$9d,$ed,$4c,$bf,$ec,$66,$5b,$46,$5b,$20,$c9
	.byt $ec,$20,$0a,$ee,$4c,$c7,$ec,$66,$5b,$38,$66,$5b,$20,$d9,$ec,$20
	.byt $0a,$ee,$4c,$d7,$ec,$66,$5b,$46,$5b,$a9,$40,$8d,$0e,$03,$20,$c1
	.byt $ec,$20,$56,$ee,$a9,$c0,$8d,$0e,$03,$4c,$bf,$ec,$66,$5b,$38,$66
	.byt $5b,$20,$d1,$ec,$20,$56,$ee,$4c,$cf,$ec,$24,$5b,$70,$03,$20,$fd
	.byt $ec,$20,$df,$ec,$a9,$00,$85,$0e,$ad,$2a,$05,$f0,$12,$a0,$00,$b1
	.byt $00,$20,$4f,$ec,$ce,$2a,$05,$e6,$00,$d0,$ed,$e6,$01,$d0,$e9,$ad
	.byt $2b,$05,$f0,$1d,$a0,$00,$b1,$00,$20,$4f,$ec,$c8,$d0,$f8,$ce,$2b
	.byt $05,$e6,$01,$24,$5b,$10,$e8,$a9,$30,$85,$44,$a5,$44,$d0,$fc,$f0
	.byt $de,$a5,$0e,$4c,$4f,$ec,$24,$5b,$70,$03,$20,$34,$ed,$20,$df,$ec
	.byt $24,$5b,$50,$08,$a9,$ff,$8d,$2a,$05,$8d,$2b,$05,$a0,$00,$84,$0e
	.byt $ad,$2a,$05,$f0,$11,$20,$6b,$ec,$91,$00,$ce,$2a,$05,$e6,$00,$d0
	.byt $ef,$e6,$01,$4c,$70,$ee,$ad,$2b,$05,$f0,$12,$a0,$00,$20,$6b,$ec
	.byt $91,$00,$c8,$d0,$f8,$e6,$01,$ce,$2b,$05,$4c,$86,$ee,$20,$6b,$ec
	.byt $09,$30,$4c,$b5,$db,$a9,$00,$8d,$8c,$02,$a9,$10,$2c,$2d,$03,$d0
	.byt $32,$38,$60,$a9,$ff,$8d,$28,$03,$8d,$29,$03,$ad,$29,$03,$c9,$c5
	.byt $b0,$f9,$2c,$20,$03,$a9,$20,$2d,$2d,$03,$d0,$13,$a9,$10,$2d,$2d
	.byt $03,$f0,$f2,$ad,$29,$03,$c9,$ad,$90,$07,$c9,$b5,$a9,$01,$60,$a9
	.byt $00,$38,$60,$78,$a2,$04,$20,$b3,$ee,$ca,$d0,$fa,$20,$b3,$ee,$f0
	.byt $0a,$b0,$f9,$e8,$4c,$ec,$ee,$58,$4c,$aa,$ee,$e0,$06,$90,$f8,$20
	.byt $b3,$ee,$b0,$fb,$a0,$1e,$a2,$00,$20,$b3,$ee,$90,$01,$e8,$88,$d0
	.byt $f7,$e0,$0f,$b0,$e2,$58,$a9,$0a,$85,$44,$a5,$44,$d0,$fc,$18,$60
	.byt $20,$d9,$ec,$a9,$6f,$20,$30,$ef,$a9,$68,$20,$30,$ef,$4c,$d7,$ec
	.byt $48,$a9,$1b,$20,$49,$ec,$a9,$39,$20,$49,$ec,$68,$4c,$49,$ec,$20
	.byt $d9,$ec,$a9,$67,$20,$30,$ef,$4c,$d7,$ec,$20,$d1,$ec,$a9,$fa,$85
	.byt $44,$a5,$44,$c9,$f0,$d0,$fa,$a2,$0c,$20,$0c,$c5,$a5,$44,$d0,$05
	.byt $20,$cf,$ec,$38,$60,$20,$b4,$ec,$b0,$f2,$c9,$13,$d0,$ee,$20,$b9
	.byt $ec,$c9,$53,$d0,$e7,$20,$cf,$ec,$18,$60,$48,$20,$d9,$ec,$68,$20
	.byt $49,$ec,$4c,$d7,$ec,$48,$20,$c9,$ec,$68,$20,$1b,$ec,$4c,$c7,$ec
	.byt $a9,$e4,$a0,$f5,$4c,$af,$ef,$60,$20,$ec,$f1,$a5,$65,$49,$ff,$85
	.byt $65,$45,$6d,$85,$6e,$a5,$60,$4c,$b2,$ef,$20,$e5,$f0,$90,$3f,$20
	.byt $ec,$f1,$d0,$03,$4c,$77,$f3,$ba,$86,$89,$a6,$66,$86,$7f,$a2,$68
	.byt $a5,$68,$a8,$f0,$d2,$38,$e5,$60,$f0,$24,$90,$12,$84,$60,$a4,$6d
	.byt $84,$65,$49,$ff,$69,$00,$a0,$00,$84,$7f,$a2,$60,$d0,$04,$a0,$00
	.byt $84,$66,$c9,$f9,$30,$c4,$a8,$a5,$66,$56,$01,$20,$fc,$f0,$24,$6e
	.byt $10,$57,$a0,$60,$e0,$68,$f0,$02,$a0,$68,$38,$49,$ff,$65,$7f,$85
	.byt $66,$b9,$04,$00,$f5,$04,$85,$64,$b9,$03,$00,$f5,$03,$85,$63,$b9
	.byt $02,$00,$f5,$02,$85,$62,$b9,$01,$00,$f5,$01,$85,$61,$b0,$03,$20
	.byt $90,$f0,$a0,$00,$98,$18,$a6,$61,$d0,$4a,$a6,$62,$86,$61,$a6,$63
	.byt $86,$62,$a6,$64,$86,$63,$a6,$66,$86,$64,$84,$66,$69,$08,$c9,$28
	.byt $d0,$e4,$a9,$00,$85,$60,$85,$65,$60,$65,$7f,$85,$66,$a5,$64,$65
	.byt $6c,$85,$64,$a5,$63,$65,$6b,$85,$63,$a5,$62,$65,$6a,$85,$62,$a5
	.byt $61,$65,$69,$85,$61,$4c,$81,$f0,$69,$01,$06,$66,$26,$64,$26,$63
	.byt $26,$62,$26,$61,$10,$f2,$38,$e5,$60,$b0,$c7,$49,$ff,$69,$01,$85
	.byt $60,$90,$0c,$e6,$60,$f0,$40,$66,$61,$66,$62,$66,$63,$66,$64,$60
	.byt $a5,$65,$49,$ff,$85,$65,$a5,$61,$49,$ff,$85,$61,$a5,$62,$49,$ff
	.byt $85,$62,$a5,$63,$49,$ff,$85,$63,$a5,$64,$49,$ff,$85,$64,$a5,$66
	.byt $49,$ff,$85,$66,$e6,$66,$d0,$0e,$e6,$64,$d0,$0a,$e6,$63,$d0,$06
	.byt $e6,$62,$d0,$02,$e6,$61,$60,$a9,$01,$85,$8b,$a6,$89,$9a,$60,$a2
	.byt $6e,$b4,$04,$84,$66,$b4,$03,$94,$04,$b4,$02,$94,$03,$b4,$01,$94
	.byt $02,$a4,$67,$94,$01,$69,$08,$30,$e8,$f0,$e6,$e9,$08,$a8,$a5,$66
	.byt $b0,$14,$16,$01,$90,$02,$f6,$01,$76,$01,$76,$01,$76,$02,$76,$03
	.byt $76,$04,$6a,$c8,$d0,$ec,$18,$60,$82,$13,$5d,$8d,$de,$82,$49,$0f
	.byt $da,$9e,$88,$34,$00,$00,$00,$03,$7f,$5e,$56,$cb,$79,$80,$13,$9b
	.byt $0b,$64,$80,$76,$38,$93,$16,$82,$38,$aa,$3b,$20,$80,$35,$04,$f3
	.byt $34,$81,$35,$04,$f3,$34,$80,$80,$00,$00,$00,$80,$31,$72,$17,$f8
	.byt $60,$a9,$02,$4c,$c9,$f0,$ba,$86,$89,$20,$bd,$f3,$f0,$f3,$30,$f1
	.byt $a5,$60,$e9,$7f,$48,$a9,$80,$85,$60,$a9,$2c,$a0,$f1,$20,$af,$ef
	.byt $a9,$31,$a0,$f1,$20,$87,$f2,$a9,$a5,$a0,$f8,$20,$98,$ef,$a9,$17
	.byt $a0,$f1,$20,$e1,$f6,$a9,$36,$a0,$f1,$20,$af,$ef,$68,$20,$e9,$f9
	.byt $a9,$3b,$a0,$f1,$20,$ec,$f1,$f0,$b7,$d0,$05,$f0,$b3,$ba,$86,$89
	.byt $20,$17,$f2,$a9,$00,$85,$6f,$85,$70,$85,$71,$85,$72,$a5,$66,$20
	.byt $b9,$f1,$a5,$64,$20,$b9,$f1,$a5,$63,$20,$b9,$f1,$a5,$62,$20,$b9
	.byt $f1,$a5,$61,$20,$be,$f1,$4c,$01,$f3,$d0,$03,$4c,$cf,$f0,$4a,$09
	.byt $80,$a8,$90,$19,$18,$a5,$72,$65,$6c,$85,$72,$a5,$71,$65,$6b,$85
	.byt $71,$a5,$70,$65,$6a,$85,$70,$a5,$6f,$65,$69,$85,$6f,$66,$6f,$66
	.byt $70,$66,$71,$66,$72,$66,$66,$98,$4a,$d0,$d6,$60,$85,$7d,$84,$7e
	.byt $a0,$04,$b1,$7d,$85,$6c,$88,$b1,$7d,$85,$6b,$88,$b1,$7d,$85,$6a
	.byt $88,$b1,$7d,$85,$6d,$45,$65,$85,$6e,$a5,$6d,$09,$80,$85,$69,$88
	.byt $b1,$7d,$85,$68,$a5,$60,$60,$a5,$68,$f0,$1c,$18,$65,$60,$90,$04
	.byt $30,$1a,$18,$2c,$10,$11,$69,$80,$85,$60,$f0,$13,$a5,$6e,$85,$65
	.byt $60,$a5,$65,$49,$ff,$30,$05,$68,$68,$4c,$42,$f0,$4c,$c7,$f0,$4c
	.byt $46,$f0,$20,$87,$f3,$aa,$f0,$10,$18,$69,$02,$b0,$ef,$a2,$00,$86
	.byt $6e,$20,$c2,$ef,$e6,$60,$f0,$e4,$60,$84,$20,$00,$00,$00,$20,$87
	.byt $f3,$a2,$00,$a9,$59,$a0,$f2,$86,$6e,$20,$23,$f3,$4c,$8a,$f2,$ba
	.byt $86,$89,$20,$49,$f1,$20,$87,$f3,$a9,$08,$a0,$f1,$20,$23,$f3,$4c
	.byt $8a,$f2,$a9,$03,$85,$8b,$60,$20,$ec,$f1,$f0,$f6,$ba,$86,$89,$20
	.byt $96,$f3,$a9,$00,$38,$e5,$60,$85,$60,$20,$17,$f2,$e6,$60,$f0,$9c
	.byt $a2,$fc,$a9,$01,$a4,$69,$c4,$61,$d0,$10,$a4,$6a,$c4,$62,$d0,$0a
	.byt $a4,$6b,$c4,$63,$d0,$04,$a4,$6c,$c4,$64,$08,$2a,$90,$0c,$e8,$95
	.byt $72,$f0,$05,$10,$33,$a9,$01,$2c,$a9,$40,$28,$b0,$0e,$06,$6c,$26
	.byt $6b,$26,$6a,$26,$69,$b0,$e3,$30,$cb,$10,$df,$a8,$a5,$6c,$e5,$64
	.byt $85,$6c,$a5,$6b,$e5,$63,$85,$6b,$a5,$6a,$e5,$62,$85,$6a,$a5,$69
	.byt $e5,$61,$85,$69,$98,$4c,$cd,$f2,$0a,$0a,$0a,$0a,$0a,$0a,$85,$66
	.byt $28,$a5,$6f,$85,$61,$a5,$70,$85,$62,$a5,$71,$85,$63,$a5,$72,$85
	.byt $64,$4c,$22,$f0,$20,$c7,$f8,$f0,$06,$a9,$12,$a0,$f1,$d0,$04,$a9
	.byt $0d,$a0,$f1,$85,$7d,$84,$7e,$a0,$04,$b1,$7d,$85,$64,$88,$b1,$7d
	.byt $85,$63,$88,$b1,$7d,$85,$62,$88,$b1,$7d,$85,$65,$09,$80,$85,$61
	.byt $88,$b1,$7d,$85,$60,$84,$66,$60,$a2,$73,$2c,$a2,$78,$a0,$00,$20
	.byt $96,$f3,$86,$7d,$84,$7e,$a0,$04,$a5,$64,$91,$7d,$88,$a5,$63,$91
	.byt $7d,$88,$a5,$62,$91,$7d,$88,$a5,$65,$09,$7f,$25,$61,$91,$7d,$88
	.byt $a5,$60,$91,$7d,$84,$66,$60,$a5,$6d,$85,$65,$a2,$05,$b5,$67,$95
	.byt $5f,$ca,$d0,$f9,$86,$66,$60,$20,$96,$f3,$a2,$06,$b5,$5f,$95,$67
	.byt $ca,$d0,$f9,$86,$66,$60,$a5,$60,$f0,$fb,$06,$66,$90,$f7,$20,$b8
	.byt $f0,$d0,$f2,$4c,$83,$f0,$a5,$65,$30,$0e,$a5,$60,$c9,$91,$b0,$08
	.byt $20,$39,$f4,$a5,$64,$a4,$63,$60,$a9,$0a,$4c,$c9,$f0,$a5,$60,$f0
	.byt $09,$a5,$65,$2a,$a9,$ff,$b0,$02,$a9,$01,$60,$20,$bd,$f3,$2c,$a9
	.byt $ff,$85,$61,$a9,$00,$85,$62,$a2,$88,$a5,$61,$49,$ff,$2a,$a9,$00
	.byt $85,$63,$85,$64,$86,$60,$85,$66,$85,$65,$4c,$1d,$f0,$85,$61,$84
	.byt $62,$a2,$90,$38,$b0,$e8,$46,$65,$60,$85,$7d,$84,$7e,$a0,$00,$b1
	.byt $7d,$c8,$aa,$f0,$b8,$b1,$7d,$45,$65,$30,$b6,$e4,$60,$d0,$21,$b1
	.byt $7d,$09,$80,$c5,$61,$d0,$19,$c8,$b1,$7d,$c5,$62,$d0,$12,$c8,$b1
	.byt $7d,$c5,$63,$d0,$0b,$c8,$a9,$7f,$c5,$66,$b1,$7d,$e5,$64,$f0,$c8
	.byt $a5,$65,$90,$02,$49,$ff,$4c,$c3,$f3,$a5,$60,$f0,$4a,$38,$e9,$a0
	.byt $24,$65,$10,$09,$aa,$a9,$ff,$85,$67,$20,$96,$f0,$8a,$a2,$60,$c9
	.byt $f9,$10,$06,$20,$e5,$f0,$84,$67,$60,$a8,$a5,$65,$29,$80,$46,$61
	.byt $05,$61,$85,$61,$20,$fc,$f0,$84,$67,$60,$a5,$60,$c9,$a0,$b0,$f9
	.byt $20,$39,$f4,$84,$66,$a5,$65,$84,$65,$49,$80,$2a,$a9,$a0,$85,$60
	.byt $a5,$64,$85,$88,$4c,$1d,$f0,$85,$61,$85,$62,$85,$63,$85,$64,$a8
	.byt $60,$85,$61,$86,$62,$a2,$90,$38,$4c,$de,$f3,$20,$a5,$f4,$a9,$00
	.byt $a0,$01,$4c,$a8,$c7,$a0,$00,$a9,$20,$24,$65,$10,$02,$a9,$2d,$99
	.byt $00,$01,$85,$65,$84,$77,$c8,$a9,$30,$a6,$60,$d0,$03,$4c,$c8,$f5
	.byt $a9,$00,$e0,$80,$f0,$02,$b0,$09,$a9,$d5,$a0,$f5,$20,$84,$f1,$a9
	.byt $f7,$85,$74,$a9,$da,$a0,$f5,$20,$f9,$f3,$f0,$1e,$10,$12,$a9,$df
	.byt $a0,$f5,$20,$f9,$f3,$f0,$02,$10,$0e,$20,$42,$f2,$c6,$74,$d0,$ee
	.byt $20,$5e,$f2,$e6,$74,$d0,$dc,$20,$90,$ef,$20,$39,$f4,$a2,$01,$a5
	.byt $74,$18,$69,$0a,$30,$09,$c9,$0b,$b0,$06,$69,$ff,$aa,$a9,$02,$38
	.byt $e9,$02,$85,$75,$86,$74,$8a,$f0,$02,$10,$13,$a4,$77,$a9,$2e,$c8
	.byt $99,$00,$01,$8a,$f0,$06,$a9,$30,$c8,$99,$00,$01,$84,$77,$a0,$00
	.byt $a2,$80,$18,$a5,$64,$79,$ec,$f5,$85,$64,$a5,$63,$79,$eb,$f5,$85
	.byt $63,$a5,$62,$79,$ea,$f5,$85,$62,$a5,$61,$79,$e9,$f5,$85,$61,$e8
	.byt $b0,$04,$10,$df,$30,$02,$30,$da,$8a,$90,$04,$49,$ff,$69,$0a,$69
	.byt $2f,$c8,$c8,$c8,$c8,$84,$76,$a4,$77,$c8,$aa,$29,$7f,$99,$00,$01
	.byt $c6,$74,$d0,$06,$a9,$2e,$c8,$99,$00,$01,$84,$77,$a4,$76,$8a,$49
	.byt $ff,$29,$80,$aa,$c0,$24,$d0,$ab,$a4,$77,$b9,$00,$01,$88,$c9,$30
	.byt $f0,$f8,$c9,$2e,$f0,$01,$c8,$a9,$2b,$a6,$75,$f0,$2e,$10,$08,$a9
	.byt $00,$38,$e5,$75,$aa,$a9,$2d,$99,$02,$01,$a9,$45,$99,$01,$01,$8a
	.byt $a2,$2f,$38,$e8,$e9,$0a,$b0,$fb,$69,$3a,$99,$04,$01,$8a,$99,$03
	.byt $01,$a9,$00,$99,$05,$01,$f0,$08,$99,$00,$01,$a9,$00,$99,$01,$01
	.byt $a9,$00,$a0,$01,$60,$9e,$6e,$6b,$28,$00,$9e,$6e,$6b,$27,$fd,$9b
	.byt $3e,$bc,$1f,$fd,$80,$00,$00,$00,$00,$fa,$0a,$1f,$00,$00,$98,$96
	.byt $80,$ff,$f0,$bd,$c0,$00,$01,$86,$a0,$ff,$ff,$d8,$f0,$00,$00,$03
	.byt $e8,$ff,$ff,$ff,$9c,$00,$00,$00,$0a,$ff,$ff,$ff,$ff,$4c,$42,$f0
	.byt $20,$87,$f3,$a9,$e4,$a0,$f5,$20,$23,$f3,$f0,$70,$ba,$86,$89,$a5
	.byt $68,$f0,$ea,$a2,$80,$a0,$00,$20,$52,$f3,$a5,$6d,$10,$0f,$20,$6a
	.byt $f4,$a9,$80,$a0,$00,$20,$f9,$f3,$d0,$03,$98,$a4,$88,$20,$79,$f3
	.byt $98,$48,$20,$49,$f1,$a9,$80,$a0,$00,$20,$84,$f1,$20,$8f,$f6,$68
	.byt $4a,$90,$0a,$a5,$60,$f0,$06,$a5,$65,$49,$ff,$85,$65,$60,$81,$38
	.byt $aa,$3b,$29,$07,$71,$34,$58,$3e,$56,$74,$16,$7e,$b3,$1b,$77,$2f
	.byt $ee,$e3,$85,$7a,$1d,$84,$1c,$2a,$7c,$63,$59,$58,$0a,$7e,$75,$fd
	.byt $e7,$c6,$80,$31,$72,$18,$10,$81,$00,$00,$00,$00,$ba,$86,$89,$a9
	.byt $5e,$a0,$f6,$20,$84,$f1,$a5,$66,$69,$50,$90,$03,$20,$96,$f3,$85
	.byt $7f,$20,$8a,$f3,$a5,$60,$c9,$88,$90,$03,$20,$31,$f2,$20,$6a,$f4
	.byt $a5,$88,$18,$69,$81,$f0,$f3,$38,$e9,$01,$48,$a2,$05,$b5,$68,$b4
	.byt $60,$95,$60,$94,$68,$ca,$10,$f5,$a5,$7f,$85,$66,$20,$9b,$ef,$20
	.byt $53,$f6,$a9,$63,$a0,$f6,$20,$f7,$f6,$a9,$00,$85,$6e,$68,$4c,$19
	.byt $f2,$85,$85,$84,$86,$20,$48,$f3,$a9,$73,$20,$84,$f1,$20,$fb,$f6
	.byt $a9,$73,$a0,$00,$4c,$84,$f1,$85,$85,$84,$86,$20,$4b,$f3,$b1,$85
	.byt $85,$87,$a4,$85,$c8,$98,$d0,$02,$e6,$86,$85,$85,$a4,$86,$20,$84
	.byt $f1,$a5,$85,$a4,$86,$18,$69,$05,$90,$01,$c8,$85,$85,$84,$86,$20
	.byt $af,$ef,$a9,$78,$a0,$00,$c6,$87,$d0,$e4,$60,$98,$35,$44,$7a,$6b
	.byt $68,$28,$b1,$46,$20,$20,$bd,$f3,$aa,$30,$18,$a9,$ef,$a0,$02,$20
	.byt $23,$f3,$8a,$f0,$e5,$a9,$2b,$a0,$f7,$20,$84,$f1,$a9,$30,$a0,$f7
	.byt $20,$af,$ef,$a6,$64,$a5,$61,$85,$64,$86,$61,$a9,$00,$85,$65,$a5
	.byt $60,$85,$66,$a9,$80,$85,$60,$20,$22,$f0,$a2,$ef,$a0,$02,$4c,$52
	.byt $f3,$20,$48,$f3,$20,$35,$f7,$a9,$73,$a0,$00,$20,$84,$f1,$4c,$6a
	.byt $f4,$20,$b1,$f8,$a9,$dc,$a0,$f7,$20,$af,$ef,$4c,$91,$f7,$20,$b1
	.byt $f8,$20,$87,$f3,$a9,$e1,$a0,$f7,$a6,$6d,$20,$67,$f2,$20,$87,$f3
	.byt $20,$6a,$f4,$a9,$00,$85,$6e,$20,$9b,$ef,$a9,$e6,$a0,$f7,$20,$98
	.byt $ef,$a5,$65,$48,$10,$0f,$20,$90,$ef,$a5,$65,$30,$0b,$a5,$8a,$49
	.byt $ff,$85,$8a,$24,$48,$20,$53,$f6,$a9,$e6,$a0,$f7,$20,$af,$ef,$68
	.byt $10,$03,$20,$53,$f6,$a9,$eb,$a0,$f7,$4c,$e1,$f6,$81,$49,$0f,$da
	.byt $a2,$83,$49,$0f,$da,$a2,$7f,$00,$00,$00,$00,$05,$84,$e6,$1a,$2d
	.byt $1b,$86,$28,$07,$fb,$f8,$87,$99,$68,$89,$01,$87,$23,$35,$df,$e1
	.byt $86,$a5,$5d,$e7,$28,$83,$49,$0f,$da,$a2,$20,$b1,$f8,$20,$48,$f3
	.byt $a9,$00,$85,$8a,$20,$91,$f7,$a2,$80,$a0,$00,$20,$52,$f3,$a9,$73
	.byt $a0,$00,$20,$23,$f3,$a9,$00,$85,$65,$a5,$8a,$20,$c4,$f7,$a9,$80
	.byt $a0,$00,$4c,$87,$f2,$a5,$65,$48,$10,$03,$20,$53,$f6,$a5,$60,$48
	.byt $c9,$81,$90,$07,$a9,$a5,$a0,$f8,$20,$87,$f2,$a9,$6d,$a0,$f8,$20
	.byt $e1,$f6,$68,$c9,$81,$90,$07,$a9,$dc,$a0,$f7,$20,$98,$ef,$68,$10
	.byt $03,$20,$53,$f6,$20,$c7,$f8,$f0,$03,$4c,$aa,$f8,$60,$0b,$76,$b3
	.byt $83,$bd,$d3,$79,$1e,$f4,$a6,$f5,$7b,$83,$fc,$b0,$10,$7c,$0c,$1f
	.byt $67,$ca,$7c,$de,$53,$cb,$c1,$7d,$14,$64,$70,$4c,$7d,$b7,$ea,$51
	.byt $7a,$7d,$63,$30,$88,$7e,$7e,$92,$44,$99,$3a,$7e,$4c,$cc,$91,$c7
	.byt $7f,$aa,$aa,$aa,$13,$81,$00,$00,$00,$00,$a9,$bd,$a0,$f8,$4c,$84
	.byt $f1,$20,$c7,$f8,$f0,$16,$a9,$c2,$a0,$f8,$4c,$84,$f1,$86,$65,$2e
	.byt $e0,$d8,$7b,$0e,$fa,$35,$19,$ad,$0d,$02,$29,$20,$60,$85,$00,$84
	.byt $01,$20,$af,$ef,$a6,$00,$a4,$01,$4c,$52,$f3,$20,$fc,$f9,$90,$07
	.byt $c9,$41,$90,$31,$e9,$37,$2c,$e9,$2f,$c9,$10,$b0,$28,$0a,$0a,$0a
	.byt $0a,$a2,$04,$0a,$26,$62,$26,$61,$b0,$18,$ca,$d0,$f6,$f0,$dc,$20
	.byt $fc,$f9,$b0,$11,$c9,$32,$b0,$0d,$c9,$31,$26,$62,$26,$61,$b0,$02
	.byt $90,$ed,$4c,$c7,$f0,$a2,$90,$38,$20,$de,$f3,$a2,$00,$60,$85,$00
	.byt $84,$01,$ba,$86,$89,$a9,$00,$85,$02,$85,$03,$85,$66,$a2,$05,$95
	.byt $60,$95,$73,$ca,$10,$f9,$20,$fe,$f9,$90,$16,$c9,$23,$f0,$9c,$c9
	.byt $25,$f0,$bc,$c9,$2d,$f0,$05,$c9,$2b,$d0,$08,$2c,$86,$03,$20,$fc
	.byt $f9,$90,$7a,$c9,$2e,$f0,$4f,$c9,$45,$f0,$04,$c9,$65,$d0,$4d,$a6
	.byt $02,$20,$fc,$f9,$90,$10,$c9,$2d,$f0,$05,$c9,$2b,$d0,$2a,$2c,$66
	.byt $77,$20,$fc,$f9,$b0,$24,$a5,$75,$c9,$0a,$90,$09,$a9,$64,$24,$77
	.byt $30,$11,$4c,$c7,$f0,$0a,$0a,$18,$65,$75,$0a,$18,$a4,$02,$71,$00
	.byt $38,$e9,$30,$85,$75,$4c,$71,$f9,$86,$02,$24,$77,$10,$0e,$a9,$00
	.byt $38,$e5,$75,$4c,$ae,$f9,$66,$76,$24,$76,$50,$a2,$a5,$75,$38,$e5
	.byt $74,$85,$75,$f0,$12,$10,$09,$20,$5e,$f2,$e6,$75,$d0,$f9,$f0,$07
	.byt $20,$42,$f2,$c6,$75,$d0,$f9,$a5,$03,$30,$16,$10,$17,$48,$24,$76
	.byt $10,$02,$e6,$74,$20,$42,$f2,$68,$38,$e9,$30,$20,$e9,$f9,$4c,$4e
	.byt $f9,$20,$53,$f6,$a2,$00,$4c,$96,$f3,$48,$20,$87,$f3,$68,$20,$d1
	.byt $f3,$a5,$6d,$45,$65,$85,$6e,$a6,$60,$4c,$b2,$ef,$e6,$02,$a4,$02
	.byt $b1,$00,$20,$f0,$d0,$c9,$20,$f0,$f3,$c9,$30,$90,$03,$c9,$3a,$60
	.byt $38,$60,$20,$87,$f3,$a5,$68,$f0,$19,$10,$21,$38,$a9,$a1,$e5,$68
	.byt $90,$1a,$aa,$ca,$f0,$13,$46,$69,$66,$6a,$66,$6b,$66,$6c,$90,$f3
	.byt $b0,$0a,$a2,$03,$95,$69,$ca,$10,$ea,$a9,$01,$60,$a9,$00,$60,$37
	.byt $6a,$6d,$6b,$20,$75,$79,$38,$6e,$74,$36,$39,$2c,$69,$68,$6c,$35
	.byt $72,$62,$3b,$2e,$6f,$67,$30,$76,$66,$34,$2d,$0b,$70,$65,$2f,$31
	.byt $1b,$7a,$00,$08,$7f,$61,$0d,$78,$71,$32,$5c,$0a,$5d,$73,$00,$33
	.byt $64,$63,$27,$09,$5b,$77,$3d,$26,$4a,$4d,$4b,$20,$55,$59,$2a,$4e
	.byt $54,$5e,$28,$3c,$49,$48,$4c,$25,$52,$42,$3a,$3e,$4f,$47,$29,$56
	.byt $46,$24,$5f,$0b,$50,$45,$3f,$21,$1b,$5a,$00,$08,$7f,$41,$0d,$58
	.byt $51,$40,$7c,$0a,$7d,$53,$00,$23,$44,$43,$22,$09,$7b,$57,$2b,$37
	.byt $6a,$3b,$6b,$20,$75,$79,$38,$6e,$74,$36,$39,$2c,$69,$68,$6c,$35
	.byt $72,$62,$6d,$2e,$6f,$67,$30,$76,$66,$34,$2d,$0b,$70,$65,$2f,$31
	.byt $1b,$77,$00,$08,$7f,$71,$0d,$78,$61,$32,$5c,$0a,$5d,$73,$00,$33
	.byt $64,$63,$27,$09,$5b,$7a,$3d,$26,$4a,$3a,$4b,$20,$55,$59,$2a,$4e
	.byt $54,$5e,$28,$3c,$49,$48,$4c,$25,$52,$42,$4d,$3e,$4f,$47,$29,$56
	.byt $46,$24,$5f,$0b,$50,$45,$3f,$21,$1b,$57,$00,$08,$7f,$51,$0d,$58
	.byt $41,$40,$7c,$0a,$7d,$53,$00,$23,$44,$43,$22,$09,$7b,$5a,$2b,$7d
	.byt $6a,$2c,$6b,$20,$75,$79,$21,$6e,$74,$7e,$5c,$3b,$69,$68,$6c,$28
	.byt $72,$62,$6d,$3a,$6f,$67,$40,$76,$66,$2f,$29,$0b,$70,$65,$3d,$26
	.byt $1b,$77,$00,$08,$7f,$71,$0d,$78,$61,$7b,$3c,$0a,$24,$73,$00,$22
	.byt $64,$63,$7c,$09,$60,$7a,$2d,$37,$4a,$3f,$4b,$20,$55,$59,$38,$4e
	.byt $54,$36,$39,$2e,$49,$48,$4c,$35,$52,$42,$4d,$5d,$4f,$47,$30,$56
	.byt $46,$34,$27,$0b,$50,$45,$2b,$31,$23,$57,$00,$08,$7f,$51,$0d,$58
	.byt $41,$32,$3e,$0a,$2a,$53,$00,$33,$44,$43,$25,$09,$5b,$5a,$5e,$1c
	.byt $22,$10,$48,$00,$c8,$1c,$22,$1c,$22,$3e,$22,$e2,$08,$10,$3e,$20
	.byt $3c,$20,$fe,$0e,$90,$3c,$10,$fe,$d4,$3e,$20,$3c,$20,$fe,$1c,$22
	.byt $a0,$22,$1c,$08,$1c,$22,$1c,$02,$1e,$22,$de,$dc,$1c,$22,$3e,$22
	.byt $e2,$10,$08,$a2,$22,$dc,$10,$08,$3e,$20,$3c,$20,$fe,$1e,$68,$2c
	.byt $68,$de,$08,$14,$3e,$20,$3c,$20,$fe,$00,$08,$10,$3f,$10,$08,$c0
	.byt $08,$1c,$2a,$88,$48,$00,$04,$02,$3f,$02,$04,$c0,$88,$48,$2a,$1c
	.byt $08,$18,$24,$18,$80,$c0,$48,$3e,$48,$00,$fe,$08,$10,$1c,$22,$3e
	.byt $20,$de,$d4,$1c,$22,$3e,$20,$de,$d4,$18,$88,$dc,$c0,$1e,$a0,$1e
	.byt $08,$08,$d4,$a2,$dc,$10,$08,$1c,$02,$1e,$22,$de,$00,$c8,$fe,$08
	.byt $c0,$10,$08,$1c,$22,$3e,$20,$de,$c0,$1e,$28,$2c,$28,$de,$08,$14
	.byt $1c,$22,$3e,$20,$de,$10,$30,$10,$12,$04,$08,$1e,$04,$10,$30,$14
	.byt $12,$14,$08,$de,$30,$10,$30,$22,$24,$08,$1e,$04,$1c,$22,$1c,$a2
	.byt $dc,$80,$80,$40,$88,$48,$00,$c8,$94,$80,$40,$54,$3e,$14,$3e,$54
	.byt $00,$08,$1e,$28,$1c,$0a,$3c,$c8,$30,$32,$04,$08,$10,$26,$c6,$10
	.byt $68,$10,$2a,$24,$da,$88,$80,$40,$08,$10,$a0,$10,$c8,$08,$04,$82
	.byt $04,$c8,$08,$2a,$1c,$08,$1c,$2a,$c8,$00,$48,$3e,$48,$40,$80,$40
	.byt $48,$10,$80,$3e,$80,$00,$80,$40,$04,$40,$00,$02,$04,$08,$10,$20
	.byt $40,$1c,$22,$26,$2a,$32,$22,$dc,$08,$18,$88,$08,$dc,$1c,$22,$02
	.byt $04,$08,$10,$fe,$3e,$02,$04,$0c,$02,$22,$dc,$04,$0c,$14,$24,$3e
	.byt $44,$00,$3e,$20,$3c,$42,$22,$dc,$0c,$10,$20,$3c,$22,$22,$dc,$3e
	.byt $02,$04,$08,$90,$00,$1c,$62,$1c,$62,$dc,$1c,$62,$1e,$02,$04,$d8
	.byt $40,$08,$40,$08,$40,$40,$08,$40,$48,$10,$04,$08,$10,$20,$10,$08
	.byt $c4,$40,$3e,$00,$3e,$80,$10,$08,$04,$02,$04,$08,$d0,$1c,$22,$04
	.byt $48,$00,$c8,$1c,$22,$2a,$2e,$2c,$20,$de,$08,$14,$62,$3e,$62,$00
	.byt $3c,$62,$3c,$62,$fc,$1c,$22,$a0,$22,$dc,$3c,$a2,$62,$fc,$3e,$60
	.byt $3c,$60,$fe,$3e,$60,$3c,$a0,$00,$1e,$a0,$26,$22,$de,$a2,$3e,$a2
	.byt $00,$1c,$88,$48,$dc,$82,$02,$02,$22,$dc,$22,$24,$28,$30,$28,$24
	.byt $e2,$a0,$a0,$fe,$22,$36,$6a,$a2,$00,$62,$32,$2a,$26,$62,$00,$1c
	.byt $62,$a2,$dc,$3c,$62,$3c,$a0,$00,$1c,$a2,$2a,$24,$da,$3c,$62,$3c
	.byt $28,$24,$e2,$1c,$22,$20,$1c,$02,$22,$dc,$3e,$88,$88,$00,$a2,$a2
	.byt $dc,$a2,$62,$14,$c8,$a2,$6a,$36,$e2,$62,$14,$08,$14,$62,$00,$62
	.byt $14,$88,$c8,$3e,$02,$04,$08,$10,$20,$fe,$1e,$90,$50,$de,$00,$20
	.byt $10,$08,$04,$02,$40,$3c,$84,$44,$fc,$08,$14,$2a,$88,$c8,$0e,$90
	.byt $3c,$10,$fe,$0c,$12,$2d,$69,$2d,$12,$0c,$40,$1c,$02,$1e,$22,$1e
	.byt $00,$60,$3c,$a2,$fc,$40,$1e,$a0,$de,$42,$1e,$a2,$de,$40,$1c,$22
	.byt $3e,$20,$de,$0c,$12,$10,$3c,$90,$00,$40,$1c,$62,$1e,$02,$1c,$60
	.byt $3c,$a2,$e2,$c8,$18,$88,$dc,$04,$00,$0c,$84,$24,$18,$60,$22,$24
	.byt $38,$24,$e2,$18,$88,$48,$dc,$40,$36,$aa,$e2,$40,$3c,$a2,$e2,$40
	.byt $1c,$a2,$dc,$40,$3c,$62,$3c,$60,$40,$1e,$62,$1e,$42,$40,$2e,$30
	.byt $a0,$00,$40,$1e,$20,$1c,$02,$fc,$50,$3c,$50,$12,$cc,$40,$a2,$26
	.byt $da,$40,$a2,$14,$c8,$40,$62,$6a,$f6,$40,$22,$14,$08,$14,$e2,$40
	.byt $a2,$1e,$02,$1c,$40,$3e,$04,$08,$10,$fe,$0e,$58,$30,$58,$ce,$88
	.byt $88,$48,$38,$4c,$06,$4c,$f8,$2a,$15,$2a,$15,$2a,$15,$2a,$15,$40
	.byt $08,$3c,$3e,$3c,$c8,$00,$38,$07,$3f,$a9,$b9,$2c,$0d,$02,$10,$02
	.byt $a9,$9d,$a0,$00,$84,$00,$85,$01,$98,$48,$20,$b2,$fe,$68,$18,$69
	.byt $01,$c9,$40,$d0,$f4,$a5,$01,$e9,$03,$85,$0c,$e9,$04,$85,$01,$a9
	.byt $8f,$a0,$fb,$85,$02,$84,$03,$a0,$00,$a2,$00,$a1,$02,$aa,$e6,$02
	.byt $d0,$02,$e6,$03,$20,$9f,$fe,$8a,$29,$c0,$f0,$ed,$c9,$c0,$f0,$08
	.byt $c9,$40,$f0,$06,$20,$9f,$fe,$2c,$a2,$00,$20,$9f,$fe,$d0,$da,$8a
	.byt $29,$3f,$91,$00,$c8,$d0,$0a,$e6,$01,$a5,$01,$c5,$0c,$d0,$02,$68
	.byt $68,$60,$a2,$03,$86,$02,$48,$29,$03,$aa,$bd,$45,$fe,$91,$00,$c8
	.byt $91,$00,$c8,$a6,$02,$e0,$02,$f0,$07,$91,$00,$c8,$d0,$02,$e6,$01
	.byt $68,$4a,$4a,$c6,$02,$d0,$df,$60,$a0,$05,$2c,$a0,$0b,$a2,$05,$b9
	.byt $eb,$fe,$95,$04,$88,$ca,$10,$f7,$4c,$6c,$cd,$00,$b4,$80,$bb,$00
	.byt $98,$00,$98,$80,$9f,$00,$b4,$18,$24,$38,$66,$00,$85,$15,$84,$16
	.byt $a0,$00,$20,$27,$ff,$f0,$1f,$20,$31,$ff,$e6,$15,$d0,$02,$e6,$16
	.byt $20,$27,$ff,$91,$02,$c8,$c0,$08,$d0,$f6,$98,$18,$65,$15,$85,$15
	.byt $90,$de,$e6,$16,$b0,$da,$60,$24,$00,$10,$03,$b1,$15,$60,$4c,$11
	.byt $04,$a2,$13,$86,$03,$0a,$26,$03,$0a,$26,$03,$0a,$26,$03,$85,$02
	.byt $2c,$0d,$02,$30,$06,$a5,$03,$69,$1c,$85,$03,$60,$c9,$04,$f0,$35
	.byt $c9,$05,$f0,$38,$0a,$29,$06,$85,$00,$aa,$ad,$75,$02,$29,$f9,$05
	.byt $00,$8d,$75,$02,$bd,$90,$ff,$bc,$91,$ff,$85,$2a,$84,$2b,$20,$49
	.byt $fe,$ad,$75,$02,$29,$06,$c9,$04,$d0,$07,$a9,$98,$a0,$ff,$4c,$f9
	.byt $fe,$c9,$02,$d0,$0a,$a9,$aa,$a0,$ff,$4c,$f9,$fe,$4c,$49,$fe,$60
	.byt $3f,$fa,$af,$fa,$1f,$fb,$1f,$fb,$5b,$1c,$22,$1c,$02,$1e,$22,$1e
	.byt $00,$60,$1c,$22,$00,$22,$22,$26,$1a,$00,$7b,$04,$08,$1c,$22,$3e
	.byt $20,$1e,$00,$7d,$10,$08,$1c,$22,$3e,$20,$1e,$00,$40,$10,$08,$1c
	.byt $02,$1e,$22,$1e,$00,$5c,$00,$00,$1e,$20,$20,$20,$1e,$04,$7c,$10
	.byt $08,$22,$22,$22,$26,$1a,$00,$7e,$1c,$22,$1c,$22,$3e,$20,$1c,$00
	.byt $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byt $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
NMI:
	.byt $00,$2f
RESET:
	.byt $00,$c0
BRK_IRQ:	
	.byt $fa,$02

