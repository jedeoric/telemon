#include "include/telemon.h"
#include "include/via6522_1.h"
#include "include/via6522_2.h"
#include "include/acia6551.h"
#include "include/fdc1793.h"



#define CDRIVE $314

#define bank_signature $ff00


#define BRK_TELEMON(value)\
	.byt 00,value;\

	


; interrupt primitives

; Switch to keyboard, A containts value :
; 00 Qwerty
; 02 french
; 04 accent
; 01 azerty
; 03 bwana
; 05 accent off

*=$c000
telemon
	SEI
	CLD
	LDX #$FF
	TXS ; init stack
	inx
	stx $0418 ; Store in BNKCIB ?? ok but already init with label data_adress_418, when loading_vectors_telemon is executed
	jsr init_via ; OK
	jsr init_printer ; OK
	jsr init_disk
	; init channels loading 15

	LDX #$0F
loop1
	LSR IOTAB0,X ; init channels (0 to 3)
	DEX
	bpl loop1
	
	LDA #$D0
	JSR routine_to_define_2
	LDA VIRQ ; testing if VIRQ low byte is $4C ?
	CMP #$4C
	BNE end_rout ; non equal to $4C
	LDA $026D
	AND #$20
	BNE end_rout
	LDA FLGTEL
	AND #%00000001 ; Stratsed is here b0=1 strased not here 
	CLC
	bcc next2
end_rout
	lda #01 ; store that stratsed is missing
	sec
next2
	sta FLGTEL ; store that stratsed is missing
	ror FLGRST
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
	LDA data_to_define_6,X 
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


loading_vectors_telemon
.(
loop
	LDA loading_vectors_page_4,X ; X should be equal to 0
	STA $0400,X
	LDA loading_vectors_b800,X 
	STA $B800,X
	LDA loading_code_to_page_6,X 
	STA $0600,X
	LDA data_to_define_4,X 
	STA $0700,X ; used to copy in Overlay RAM ... see  loop40 label
	INX ; loop until 256 bytes are filled
c072	
	bne loop
.)
	JSR $0603

routine_to_define_3 	
c0ac
	LDA #$00
.(
loop
	PHA
	TAX
	JSR routine_to_define_10 
	PLA
	CLC
	ADC #$0C
	CMP #$30
	bne loop
	LDA #$00 ; INIT VALUE of the rom to 0 bytes
	STA KOROM
.)	
	LDA #$40 ; INIT VALUE of the RAM to 64 Kbytes
	STA KORAM

	LDX #$07
loop38		; $c092
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
	; loop
	bcc next3;adress C0AD  jump to $C0B6
next4
	TYA
	ADC KORAM
	STA KORAM
next3
	DEX
	bne loop38 ; jump to $c092, adress c0b7
	BIT FLGRST; 
	BPL next5
	LDX #$0B ; copy to $2F4 12 bytes
before1
	LDA data_vectors_VNMI_VIRQ_VAPLIC,X ; SETUP VNMI, VIRQ, VAPLIC
	STA VNMI,X ; 
	DEX
	BPL before1
	JSR routine_to_define_4 
next5
	LDA $026C ; 
	AND #$90
	BEQ next6
	LDA FLGTEL
	ORA #$40
	STA FLGTEL
next6
	JSR routine_to_define_9 ; $DF5B 
	JSR routine_to_define_8	 ; routine_to_define_8
	JSR init_minitel ; 
	JSR init_joystick ; $DFAB
	JSR init_rs232 ; $DB54 
	; Here we go :  set up keyboard !
	LDA FLGKBD
	LSR
	AND #%00000011
	BRK_TELEMON(XGOKBD) 	
	lda #XKBD ; Setup keyboard on channel 0
	BRK_TELEMON(XOP0)
	lda #XSCR ; Setup screen !  on channel 0
	BRK_TELEMON(XOP0) 
	BRK_TELEMON($3C)  ; Don't know this vector
	lda #XMDS
	BRK_TELEMON(XOP1)

	lda #<store_str2 ; Write attributes on first line (status line)
	ldy #>store_str2	
	bit FLGTEL ;
	bvc next32
	lda #XMDS
	BRK_TELEMON(XOP0)
	lda #<store_str1
	ldy #>store_str1
next32
	BRK_TELEMON(XWSTR1)
	BIT FLGRST ; COLD RESET ?
	
	bpl telemon_hot_reset	; no

	; display telestrat at the fiest line
	LDA #<str_telestrat
	LDY #>str_telestrat
	BRK_TELEMON(XWSTR0)
	; display Oric international 1986
	lda #<str_oric_international
	ldy #>str_oric_international
	BRK_TELEMON(XWSTR0) ;display on channel 0

; it's similar to lda #10 brk xwr0 lda #13 brk XWR0
	BRK_TELEMON(XCRLF)
	; fetch KORAM and display
	lda KORAM
	jsr telemon_convert_to_decimal ; convert in decimal accumulator A
	; display KORAM
	lda #<str_KORAM
	ldy #>str_KORAM
	BRK_TELEMON(XWSTR0)

	LDA KOROM
	JSR telemon_convert_to_decimal
	LDA #<str_KOROM
	LDY #>str_KOROM
	BRK_TELEMON(XWSTR0)
; adress : $c146
telemon_hot_reset	
	jsr XTSTLP_ROUTINE ; printer connected ?
	bne next34 
	BRK_TELEMON(XCRLF)
	JMP next35
next34	
	BIT FLGRST; does it test printer ? not a printer
	BPL next35 ; jumps if Negative flag is ok
	; display printer
	LDA #<str_printer
	LDY #>str_printer
	BRK_TELEMON(XWSTR0)

next35	

	BIT FLGRST
	BMI next49
	JSR routine_to_define_19 
	; load VNMI Number of bank and address
	LDX VNMI ; BANK
	LDA VNMI+1 ; ADRESS low
	LDY VNMI+2 ; adress hih
	JMP next57 
next49
	LDX #$00
loop49
	LDA TABDRV,X
	BNE next50
	INX
	CPX #$04
	BNE loop49

	BEQ next58 ; 
; c11a
next50
	TXA
	PHA
	LDA SCRX ; load X cursor position 
	BEQ next51 ; If 0, then no need to display "," because it's A drive
	LDA #","
	BRK_TELEMON(XWR0) ; display ','
c1b5
next51	
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
	ADC #$41 ; display A, because it adds to c2dc (drives ) 41 (equal to A in ascii table)
	BRK_TELEMON(XWR0)
loop12	
	inx 
	CPX #$04
	beq next61
	LDA TABDRV,X ;3 
	beq loop12	
	lda #"-" ; display - to separate drive 2
	BRK_TELEMON(XWR0) ; 2
next52	
	jmp loop50 ;3
next58	
	lda $220
	beq loop58

next61	
	BRK_TELEMON($25)
	
loop58	
	; display TELEMON
	lda #<str_telemon

	ldy #>str_telemon
	
	BRK_TELEMON(XWSTR0)
	lda #$00
	sta BNKST ; Switch to ram overlay ?
	LDA FLGTEL ; does stratsed is load ?
	LSR
	BCS next54
	LDA #<str_insert_disk
	LDY #>str_insert_disk
	BRK_TELEMON(XWSTR0)
	jsr $b800 ; FIXME  
	lda #<str_tofix
	ldy #>str_tofix
	BRK_TELEMON(XWSTR0)

next54
	LDX #$02
loop55	
	LDA $C45C,X ; CORRECTME
	STA $055D,X ; CORRECTME
	DEX
	BPL loop55
	JSR $0600 ; CORRECTME
	JSR routine_to_define_19
	LDA FLGTEL ; test if strased is here
	LSR ; shift FLGTEL value to the right, b0 is in the carry
	bcs display_cursor ; Strased is loaded, we display the cursor
	; trying to load bonjour.com 
	LDA #<str_loading_bonjour ; strased is not here, load bonjour.com !
	LDY #>str_loading_bonjour
	LDX #$07
	BRK_TELEMON(XNOMFI)
	ldx #$00
	lda #$7d
	LDY #$FF
	JSR next57
	beq display_cursor ; Display cursor
	LDA FLGTEL ; something is wrong
	ORA #$04
	STA FLGTEL
	LDX #$06
loop56	
	LDA $0200,X
	CMP #$EF
	BEQ next56
	DEX
	bpl loop56
	BMI display_cursor
next56
	LDA #$00
	LDY #$C0
	bne next57
display_cursor	

	LDX #$00
	BRK_TELEMON(XCSSCR) ; display cursors
	ldx VAPLIC
	bmi Lc286 

	lda $02FE
	ldy $02ff
	
next57
	STA $0415
	STY $0416
	STX BNKCIB
	JMP $040C

routine_to_define_19
	CLI
	LDA #$02
	STA TIMEUD
loop57
	LDA TIMEUD
	BNE loop57
	LDX #$0C
	BRK_TELEMON($57) 
	LDA ACIACR
	AND #$F3
	ORA #$08
	STA ACIACR
	LDA #$8F
	BRK_TELEMON(XCL1) 
	rts
Lc284
	BRK_TELEMON(XWR0)
	
Lc286
.(
	BRK_TELEMON(XRDW0)
	CMP #$03
	BNE next
	JSR $9000
next	
	CMP #$01
	BNE Lc284
	LDA #$00
	LDY #$E0
	LDX #$00
	BEQ next57
.)	

telemon_convert_to_decimal
	LDY #$00 ; 00
	LDX #$20 ;
	STX $14
	LDX #$01
	JMP XDECIM_ROUTINE 
	

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
	sta V2DRA ; 3 bytes ; switch to overlay ram ?
	TAX
loop40

	LDA $0700,X
	STA BUFROU,X ; store data in c500 
	INX
	BNE loop40 ; copy 256 bytes to BUFROU in OVERLAY RAM
; reading all connected bank (cardridges)!
	LDX #$07
loop47
	STX V2DRA ; Switch to the Bank ;
; reading cardridge bank
	LDY #$00
loop43 
	LDA bank_signature,Y ; first iteration is equal to $a0
	PHA ; push A (first iteration $a0)
loop41
	ADC #$04 ; add 4
	BCC loop41 ; Loop until it reached $00
	PLA ; 
	CMP bank_signature,Y ;  did signature changed ? ?
	BNE loop42+1
	INY
	BNE loop43
loop45
	STY $FFFB ; Store  trying to fix someting on RAM ?
	LDA $FFFB
	CPY $FFFB
	BNE loop44
	INY
	BNE loop45
	LDA #$0F
loop42
	.byt $2c
	lda #$10

loop44
	STA BNKST,X ; Fill $fffb of each bank 
	CMP #$02
	BNE loop46
	JMP (RESET) ; something is wrong : reset 
Lc327
loop46
	DEX
	BNE loop47
	lda #$07
	sta V2DRA ; return to telemon bank
	rts
Lc330
	ldx #0
	jsr $065e ; FIXME
	ldx #$06 ; bank 6
Lcc337	
	jsr $065e ; FIXME
	dex 
	bne Lcc337
	rts
	lda $0200,x
	bpl Lc365 

	stx $0321
	lda $fff8 
	sta $02
	lda $fff9
	sta $03
	ldy #0
Lc352		
	stx $0321 

	lda ($02),y
	pha

	lda #7
	sta $321
		
	pla
	beq Lc365 

	BRK_TELEMON(XWR0) ; FIXME
	iny
	
	bne Lc352
Lc365	
	lda $0200,x
	asl
	bpl Lc382	
	stx $0321
	lda $fffc ; read execution address
	ldy $fffd 
	sta $02fe
	sty $02ff
	stx $02fd
	lda #7
	sta $0321
Lc382	
	rts

; FIXME these bytes ?
	.byt $4c,$00,$00
data_vectors_VNMI_VIRQ_VAPLIC
	; 12 bytes
	.byt $07,<display_developper,>display_developper ; VAPLIC vectors : bank + address ?
	.byt $4c
	.byt $00,$00
VIRQ_CODE
	jmp $0406 ; stored in $2FA (VIRQ) 
	.byt $80 ; will be stored in $2fd
	.byt $00 ; will be stored in $2fE
	.byt $00 ; will be stored in $2FF
display_developper	
; VAPLIC execution
; VAPLIC Routines
/**************************** BEGIN LOOP ON DEVELOPPER NAME !*/
	LDA #<str_license
	LDY #>str_license
	BRK_TELEMON($14)
loop_str_licence	
	jmp loop_str_licence 
/**************************** END LOOP ON DEVELOPPER NAME !*/
	
str_telestrat	
	.asc $0c,$97,$96,$95,$94,$93,$92,$91,$90," TELESTRAT ",$90,$91,$92,$93,$94,$95,$96,$97,$90,$00
str_KORAM	
	.asc " Ko RAM,",0
str_KOROM
	.asc " Ko ROM",$0d,$0a,$00
str_drive
	.asc "Drive:",0
str_telemon
	.asc $0d,$0a,"TELEMON V2."
#ifdef HAVE_MINITEL	
	.byt "4"
#endif	
#ifdef HAVE_USBDRIVE	
	.byt "5"
#endif	

str_oric_international
	.asc $0d,$0a,"(c) 1986 ORIC International",$0d,$0a,$00
str_printer
	.asc $0a,"Imprimante",0
store_str1	
	.byt $1b,$3a,$69
	.byt $43,$11,$00
store_str2	
	.byt $1b,$3a,$6a,$43,$14,$00
str_insert_disk	
	.asc $8c,"Inserez une disquette",0 ; 8c for blink
str_tofix
	.byt	$0d,$18,$00
str_license	
	.asc "Logiciel ecrit par Fabrice BROCHE",0
str_loading_bonjour	
	.asc "BONJOURCOM"
data_to_define_6
Lc45f
	; FIVE Bbytes to load in CSRND
	.byt $80,$4f,$c7,$52,$58
loading_vectors_b800
	LDA #%11101000
	STA V2DRA
	LDA #$00
	LDY #$C1
	STA RES
	STY $01
	LDX #$01
	STX FDCSR 
	JSR $B84F ; FIXME
	LDA $C100 ; FIXME
	bne next100
	
	LDA $C103 ; FIXME
	LDY $C104 ; FIXME
	STA RES
loop101	
	STY $01
loop102	
	CPX $C101

	BNE next101
	
	LDA #$58
	JSR $B86D ; FIXME
	LDX #$00
	NOP
	NOP
next101
	INX
	STX $0312
	JSR $B84F ; FIXME      
	INC $01  
	DEC $C102 ; FIXME      
	BNE loop102
	JSR $C105 ; FIXME
	LDA $FFFB ; FIXME
	STA $0200
next100
	LDA #$EF
	STA V2DRA
	RTS
loop105	
address_b84F
	LDA #$88
	STA $0310
	LDY #$04
loop100
	DEY
	BNE loop100
loop103
	LDA $0310
	LSR
	bcc end6
	LDA $0318
	bmi loop103
	LDA FDCDR
	STA ($00),Y
	INY
address_b86a	
	JMP $B85F ; address_b85f
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
end7	
	rts
	nop
end6
	LDA $0310
	AND #$1C
	BEQ end7
	BNE loop105
routine_to_define_10
	STX RESB
	TXA
	LDX #$FF
.(	
loop
	SEC
	SBC #$03
	INX
	BCS loop
.)
ROUTINE_TO_DEFINE_40	
	LDA data_to_define_7,X 
	STA RES
	LDA $C6B0,X ; FIXME
	STA $01
	
	LDA $C6B1,X ; CORRECTME
	LDY $C6B2,X ; CORRECTME
	
	LDX RESB
	BIT $C518 ; CORRECTME
	BVC next19

	LDA #$00
lc50e
	.byt $2c
	lda #1
	;BIT $01A9 ; CORRECTME
	.byt $2c
	.byt $24
	.byt $c5
	;BIT $C524 ; CORRECTME
next19
	SEC
	JMP $0409
Lc518
	.byt $2c
	.byt $18 
	.byt $c5
	;BIT $C518 ; CORRECTME
	
	BVC next20
LC51D
	.byt $2c
	.byt $24
	.byt $c5
	;BIT $C524 ; CORRECTME
next20
	CLC
	JMP $0409
	
;*********************************************************************************	
; CODE INSERTED IN PAGE 4	
;*********************************************************************************	
loading_vectors_page_4
code_adress_400
	JMP $0493 ;code_adress_493
code_adress_403	
	JMP $04A1 ;code_adress_4A1
code_adress_406	
	JMP $047E ; see code_adress_47E
code_adress_409	
	JMP $0419 ; see code_adress_419	
code_adress_40c
	JMP $0436 ; 436 see  code_adress_436
code_adress_40f
	.byt $00 
	.byt $00 ; used for 
	jmp $04af
	.byt $4c,$00,$00
data_adress_417 	
.byt $00 ; Init BNKCIB with 0
data_adress_418
.byt $00 ; init also 418 but it already initialized !
code_adress_419	
	PHP
	SEI
	PHA
	LDA V2DRA
	AND #%11111000
	STA V2DRA
	PLA
	JSR $C500 ; FIXME
	TAY
	LDA V2DRA
	ORA #$07
	STA V2DRA
	ROR
	PLP
	ASL 
	TYA
	RTS
code_adress_436

	PHP
	SEI
	PHA
	TXA
	PHA
	LDA V2DRA
	LDX $0418
	STA $04C8,X
	INC $0418
	PLA
	TAX
	LDA $0417
	JSR $046A ; see code_adress_46A	
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
	JSR $046A ; FIXME
	PLA
	TAX
	PLA
	PLP
	RTS
code_adress_46A	
	PHP
	SEI
	AND #$07
	STA $04C7
	LDA V2DRA
	AND #$F8
	ORA $04C7
	STA V2DRA
	PLP
	RTS
code_adress_47E ; brk gestion ?
	STA $21
	LDA V2DRA
	AND #$07
	STA $040F ; store old bank before interrupt ?
	LDA V2DRA ; Switch to telemon bank and jump
	ORA #$07
	STA V2DRA
	JMP LC868	 
code_adress_493
	LDA V2DRA
	AND #$F8
	ORA $040F
	STA V2DRA
	LDA $21
	RTI
code_adress_4A1
	PHA
	LDA V2DRA
	AND #$F8
	ORA $0410
	STA V2DRA
	PLA
	RTS

code_adress_4AF	
	LDA V2DRA
	AND #%11111000
	ORA $0410 
	STA V2DRA
	LDA ($15),Y
	PHA
	LDA V2DRA
	ORA #%00000111
	STA V2DRA
	PLA
	RTS	

data_adress_04c7	
to_put_in_address_700
data_to_define_4	
	; should be length 256 bytes ?
	
	.byt $90,$4c,$50,$0f,$a8,$f0,$2c,$bd,$88,$c0,$1d,$89,$c0,$f0,$02,$18
	rts
	sec
	rts
	
	sta RESB
	sty $03
	
	sec
	sbc RES
	sta $c08a,x
	tya
	sbc $01
	sta $c08b,x
	
	txa
	adc #3
	tax
	ldy #3
Lc614	
	lda $0000,y
	sta $c07f,x
	dex
	dey
	bpl  Lc614
	
	lda #0
	sta $c088,x
	sta $c089,x
	lda $c082,x
	sta $c084,x
	sta $c086,x
	lda $c083,x
	sta $c085,x
	sta $c087,x
	rts
	
	bvs $c661 ; FIXME
	jsr $c507 ; FIXME

	
	.byt $b0,$20
	.byt $bd,$86,$c0,$bc,$87,$c0,$20,$a6,$c5,$9d,$86,$c0,$98,$9d,$87,$c0
	.byt $bd,$88,$c0,$d0,$03,$de,$89,$c0,$de,$88,$c0,$a0,$00,$b1,$24,$18
	.byt $60,$48,$bd,$88,$c0,$dd,$8a,$c0,$bd,$89,$c0,$fd,$8b,$c0,$b0,$1f
	.byt $bd,$84,$c0,$bc,$85,$c0,$20,$a6,$c5,$9d,$84,$c0,$98,$9d,$85,$c0
	.byt $fe,$88,$c0,$d0,$03,$fe,$89,$c0,$a0,$00,$68,$91,$24,$18,$60,$68
	.byt $60,$18,$69,$01,$90,$01,$c8,$dd,$82,$c0,$85
routine_to_define_16
	BIT $98
	SBC $C083,X ; FIXME
	BCC next36 
	LDA $C080,X ; FIXME
	LDY $C081,X ; FIXME
	STA IRQSVP
next36
	STY $25
	LDA IRQSVP
	RTS

address_buffers
data_to_define_7
	.byt $c4,$c5 ; Keyboard buffer
	.byt $80,$c6,$80,$c6,$00,$c8,$00,$c8,$00,$ca,$00,$ca,$00,$d2

; test if printer is connected	
XTSTLP_ROUTINE	
routine_to_define_18
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
	
	
#include "functions/XOP.asm"
#include "functions/XCL.asm"




XCRLF_ROUTINE
	lda #$0a
	jsr XWR0_ROUTINE ; FALSE
	lda #$0d



Lc75d	
#include "functions/XWRx.asm"
#include "functions/XWSTRx.asm"
#include "functions/XRDW.asm"
#include "functions/XWRD.asm"	
	

	
Lc81c
	STY $17
	STY $18
	PHA
	TXA
	ASL
	TAX
	LDA $02BE,X
	STA $02F8
	LDA $02BF,X
	STA $02F9
	PLA
	LSR $17
	BIT $18
	JMP $02F7



Lc838	
data_to_define_1
	; length must be $30
	; used to set I/O vectors
	.byt <manage_I_O_keyboard,>manage_I_O_keyboard ; 0
	.byt $1a,$c8 ; not used 
	.byt $f7,$da ; MINITEL (mde) FIXME
	.byt $5d,$db ; RSE FIXME
	.byt $1a,$c8 ;  not used  
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8 ; not used 
	.byt $86,$db ; FIXME
	.byt $8c,$db ; FIXME
	.byt $92,$db  ; FIXME
	.byt $98,$db
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8  ; FIXME
	.byt <Lda70,>Lda70 ;30
	.byt <Ldb12,>Ldb12
	.byt $79,$db ; FIXME
	.byt $c6,$d5 ; FIXME
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8 ; not used 

	.byt $1a,$c8 ; not used 
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8 ; not used 
LC868
	; management of BRK $XX
	; on the stack we have 
	; SP = P register
	; SP-1 = PC+2 adress of brk sent
	; SP-2 = PC+1
	STX IRQSVX ; save register X
	STY IRQSVY ; save register X
	PLA ; pull P (flag register)
	STA IRQSVP ; save P (flag register)
	AND #%00010000 ; test B flag B flag means an that we reach a brk commands
	BEQ LC8B3 ; is it a break ?
	TSX ; yes we get Stack pointer
	PLA ; we pull pointer program +2 
	BNE LC87A
	DEC $0102,X ; CORRECTME
LC87A
	SEC
	SBC #$01
	PHA
	STA $15
	LDA $0102,X
	STA $16
	LDA $040F
	STA $0410
	LDY #$00
	JSR $0411
	ASL
	TAX
	LDA #$04
	PHA
	LDA #$02
	PHA
	LDA vectors_telemon+1,X ; fetch vector of brk
	LDY vectors_telemon,X 
	BCC LC8A6
	LDA $CBA5,X ; CORRECTME
	LDY $CBA4,X ; CORRECTME
LC8A6
	PHA
	TYA
	PHA 
	LDA IRQSVP  ; fetch P flag
	PHA ; push P flag to return in correct state
	LDA IRQSVA
	LDY IRQSVY
	LDX IRQSVX
	RTI
LC8B3
next200
	LDA IRQSVP ; fetch P flag
	PHA ; push P flag to return in correct state
LC8B6		
	SEC
	ROR $1E
LC8B9
	JSR LC8BF
	JMP LC9b9  




; routine BRK ?
LC8BF
routine_to_define_12
	TYA
	PHA
	LDA ACIASR 
	BPL next23
	LSR $1E
	PHA
	AND #$08
	BEQ next24
	LDX ACIADR
	PLA
	PHA
	AND #$07
	BEQ next25+1 ; ? Don't know yet
	ORA #$B0
next25
	BIT $8A
	LDX #$0C
	JSR LC51D

next24

	PLA
	AND #$10
	BEQ next23
	LDX #$18 
	JSR lc50e+1; CORRECTME
	BCS next26
	LDA ACIASR
	AND #$20
	BNE next23
	JSR Lc518 
	STA ACIADR
	LDA ACIACT
	AND #$07
	STA $3F
	BCC next23
next26
	INC $20
	BNE next23
	DEC $3F
	BNE next23
	LDA FLGLPR
	LSR
	LSR 
	LSR
	LDA ACIACR
	AND #$F3
	BCC next29
	AND #$FE
next29
	STA ACIACR
Lc91b
next23

	PLA
Lc91c
	TAY
Lc91d	
	RTS
	
Lc91e
	DEC $0215
	BNE Lc973
	LDA #$04
	STA $0215
	BIT FLGLPR
	BPL Lc930
	JSR Lca2f 
Lc930
	LDA TIMEUD
	BNE lc936
	DEC TIMEUD+1
lc936
	DEC TIMEUD
	SEC
	INC TIMED
	LDA TIMED
	SBC #$0A
	BCC Lc973
	STA TIMED
	BIT FLGCLK
	BPL Lc94e
	JSR Lca75 
Lc94e
	INC TIMES
	LDA TIMEUS
	BNE Lc957
	DEC TIMEUS+1
Lc957
	DEC TIMEUS
	LDA TIMES
	SBC #$3C
	BCC Lc973
	STA TIMES
	INC TIMEM
	LDA TIMEM
	SBC #$3C
	BCC Lc973
	STA TIMEM
	INC TIMEH
Lc973
	DEC $0216
	BNE Lc991
	LDA #$0A
	STA $0216
	LDA $0217
	EOR #$80
	STA $0217
	BIT FLGSCR
	BPL Lc991
	BVS Lc991
	LDX SCRNB
	JMP $DE2D ; FIXME
Lc991
	RTS

manage_irq_T1_and_T2
Lc992
	lda $030d
	and #$20
	beq LC9b9
	lda $028f
	ldy $0290
	sta $0308
	sty $0309
	lda $028c
	lsr
	bcc C9b1
	jsr le085 
	jmp LC8B9 

routine_todefine_1:
C9b1
	LDA #$FF
	STA $0309
	JMP LC8B9 
LC9b9	
	BIT V1IFR

	BMI next110
	BIT $1E
	BPL next111
	LDX IRQSVX
	LDY IRQSVY
	JMP $0400
next111	
	JMP LC8B6 
next110
	LSR $1E
	BIT V1IFR
	BVC next112
	BIT V1T1
	JSR Lc91e 
	DEC $02A6
	BNE next113 
	JSR manage_keyboard 
	JSR LC8BF 
	BIT $0270 ; CORRECTME
	BPL next114 
	LDA #$14 ; CORRECTME
	STA $02A7
	BNE LC9FB 
next114	
	LDA $02A8 ; CORRECTME
	BIT $02A7
	BMI lc9fd 
	DEC $02A7 ; CORRECTME
LC9FB
next115	
	LDA #$01
lc9fd	
	STA $02A6 ; CORRECTME
Lca00
next113	
	BIT FLGJCK
	BPL Lca0b 
	JSR Ldffa 
	BIT FLGJCK
Lca0b	
	BVC Lca10
		
	JSR Ldffb
Lca10
	LDA FLGJCK
	LSR
	BCC LCA19 
	JSR Le0e1 
LCA19	
	JMP LC8B9 
Lca1c
	JMP Lc992
Lca1f	
next112	
	LDA V1IFR
	AND #$02
	BEQ Lca1c
	BIT $0301
	JSR Lca2f 
	JMP LC8B9
manage_printer	
Lca2f	
	LDX #$24
	JSR Lc518 
	BCC Lca3e
	ASL FLGLPR ; printer
	SEC
	ROR FLGLPR ; printer
	RTS
Lca3e
	sta $0301
	lda $300
	and #$ef
	sta $0300
	ora #$10
	sta $0300
	asl $028a
	lsr $028a
	rts
reset_clock
	lda #0
	ldx #4
Lda59	
	sta $0210,x
	dex
	bpl Lda59
	lda #1
	sta $0215
	rts
switch_off_clock
	lsr $0214
	rts
display_clock_regulary	
	php
	sei
	sta $40
	sty $41
	sec
	ror $214
	plp
	rts
	
	
Lca75
	LDY #$00
	LDA TIMEH
	JSR telemon_display_clock_chars
	LDA #$3A
	STA (ADCLK),Y
	INY
	LDA TIMEM
	JSR telemon_display_clock_chars
	LDA #$3A
	STA (ADCLK),Y
	INY
	LDA $0211
telemon_display_clock_chars
; display clock at the adress specified	
Lca90
	LDX #$2F
	SEC
Lca93
	SBC #$0A
	INX
	BCS Lca93
	PHA
	TXA
	STA (ADCLK),Y
	PLA
	INY
	ADC #$3A
	STA (ADCLK),Y
	INY
	RTS

	; table des vecteurs du brk
vectors_telemon
;0
	.byt <XOP0_ROUTINE,>XOP0_ROUTINE ; 0
	.byt <XOP1_ROUTINE,>XOP1_ROUTINE ; 1
	.byt <XOP2_ROUTINE,>XOP2_ROUTINE ; 2
	.byt <XOP3_ROUTINE,>XOP3_ROUTINE

	.byt <XCL0_ROUTINE,>XCL0_ROUTINE ; 4	
	.byt <XCL1_ROUTINE,>XCL1_ROUTINE ; 5
	.byt <XCL2_ROUTINE,>XCL2_ROUTINE ; 6
	.byt <XCL3_ROUTINE,>XCL3_ROUTINE ; 7

	
	.byt <XRD0_ROUTINE,>XRD0_ROUTINE ; 8
	.byt <XRD1_ROUTINE,>XRD1_ROUTINE ; 9
	.byt <XRD2_ROUTINE,>XRD2_ROUTINE ; 0a
	.byt <XRD3_ROUTINE,>XRD3_ROUTINE ; 0b
	
	.byt <XRDW0_ROUTINE,>XRDW0_ROUTINE ; 0c XRDW0
	.byt <XRDW1_ROUTINE,>XRDW1_ROUTINE ; 0d
	.byt <XRDW2_ROUTINE,>XRDW2_ROUTINE ; 0e
	.byt <XRDW3_ROUTINE,>XRDW3_ROUTINE ; 0f
	

	
	.byt <XWR0_ROUTINE,>XWR0_ROUTINE ; ;10  
	.byt <XWR1_ROUTINE,>XWR1_ROUTINE  ; 
	.byt <XWR2_ROUTINE,>XWR2_ROUTINE ; 
	.byt <XWR3_ROUTINE,>XWR3_ROUTINE ; 
; 18	
	.byt <XWSTR0_ROUTINE,>XWSTR0_ROUTINE ; 14 
	.byt <XWSTR1_ROUTINE,>XWSTR1_ROUTINE ; FIXME
	.byt <XWSTR2_ROUTINE,>XWSTR2_ROUTINE	; FIXME 
	.byt <XWSTR3_ROUTINE,>XWSTR3_ROUTINE 
; 

	.byt <XDECAL_ROUTINE,>XDECAL_ROUTINE ; $18
	.byt <XTEXT_ROUTINE,>XTEXT_ROUTINE ; XTEXT ; 19
	.byt <XHIRES_ROUTINE,>XHIRES_ROUTINE ; XHIRES
	.byt <XEFFHI_ROUTINE,>XEFFHI_ROUTINE ; XEFFHI ; 1b
	
	
	.byt <XFILLM_ROUTINE,>XFILLM_ROUTINE ; XFILLM
	.byt <ZADCHA_ROUTINE,>ZADCHA_ROUTINE ; ZADCHA
	.byt <XTSTLP_ROUTINE,>XTSTLP_ROUTINE ; XTSTLP
	.byt <XMINMA_ROUTINE,>XMINMA_ROUTINE
	.byt <XMUL40_ROUTINE,>XMUL40_ROUTINE
	.byt <XMULT_ROUTINE,>XMULT_ROUTINE
	.byt <XADRES_ROUTINE,>XADRES_ROUTINE ; XADRES
	.byt <XDIVIS_ROUTINE,>XDIVIS_ROUTINE ; 
	.byt <XNOMFI_ROUTINE,>XNOMFI_ROUTINE ; XNOMFI 
	.byt <XCRLF_ROUTINE,>XCRLF_ROUTINE
	.byt $49,$e7 ; XDECAY FIXME
	.byt $00,$00 ; nothing 
	.byt <XBINDX_ROUTINE,>XBINDX_ROUTINE ; XBINDX
	.byt <XDECIM_ROUTINE,>XDECIM_ROUTINE
	.byt <XHEXA_ROUTINE,>XHEXA_ROUTINE
	.byt $9b,$f4 ; XA1AFF FIXME
	.byt <XMENU_ROUTINE,>XMENU_ROUTINE ; XMENU
	.byt $35,$e4 ; XEDT FIXME
	.byt $b0,$e6 ; XINSER FIXME
	.byt $80,$e6 ; XSCELG FIXME
	.byt $40,$d1 ; control_videotex : no name FIXME
	.byt $11,$d7
	.byt $37,$e5,$6c,$e6,$1e,$de,$20,$de
	.byt $fb,$de,$54,$de,$5c,$de,$f7,$fe,$42,$d4,$f1,$d4,$55,$ca,$65,$ca
	.byt $69,$ca,$00,$00,$e9,$d9,$1a,$da,$d8,$dd,$0d,$eb,$73,$eb,$5a,$eb
	.byt $ec,$eb
	.byt $df,$eb
	.byt $72,$da,$e4,$da,$b9,$e1,$09,$e2,$50,$e2,$00,$00
	.byt $00,$00,$00,$00,$03,$d9,$1f,$d8,$4c,$ff,$00,$00,$1d,$c5,$18,$c5
	.byt $0f,$c5,$0c,$c5,$07,$c5,$ea,$c4,$b1,$cf,$00,$00,$9a,$ed,$77,$ed
	.byt $e5,$ed,$ca,$ed,$fc,$ed,$d7,$ed,$a5,$ee,$4a,$ef,$20,$ef,$3f,$ef
	.byt $7a,$ef,$85,$ef,$a5,$f4,$1e,$f9,$b2,$ef,$9b,$ef,$8b,$f1,$8a,$f2
	.byt $1a,$f6,$53,$f6,$8e,$f7,$81,$f7,$0a,$f8,$35,$f8,$8c,$f6,$46,$f1
	.byt $6f,$f2,$35,$f7,$10,$f6,$b6,$f8,$aa,$f8,$6a,$f4,$14,$f3,$71,$f7
	.byt $87,$f3,$77,$f3,$ed,$f3,$23,$f3,$a6,$f3,$52,$f3,$96,$f3,$cd,$f8
	.byt $12,$fa,$00,$00,$e7,$e7,$d9,$e7,$c1,$e7,$cd,$e7,$92,$e7,$66,$e8
	.byt $85,$e8,$cb,$e9,$2f,$e9,$3c,$e9,$5d,$e9,$5f,$e9,$19,$e8,$2c,$e8
	.byt $73,$ea,$af,$ea,$93,$ea
	.byt $00,$00 ; nothing
	.byt $00,$00 ; nothing
	.byt $00,$00 ; nothing
	.byt <XEXPLO_ROUTINE,>XEXPLO_ROUTINE ; $9c
	.byt <XPING_ROUTINE,>XPING_ROUTINE ; $9d


XMENU_ROUTINE
menu_deroulant
.(
	sty VARMNB
	sta $66
	stx $63
	ldx #0
	jsr lde1e ;  switch off cursor
Lcbeb	
	ldy $62
	ldx $66
	.byt $2c
Lcbf0
	inx
	iny 
	jsr $ccf9  ; FIXME
	
	bmi next
	cpy $63
	bne Lcbf0 
next	
	stx $67

Lcbfd
	ldx VARMNB
	sec
	txa
	sbc $66
	clc
	adc $62
	tay 
	jsr Lccd3 
	jsr XRDW0_ROUTINE 
	pha
	bit FLGTEL ; Is it on minitel mode ?
	
	bvc Lcc20 
	lda #$08
	jsr Lc75d 
	lda #$20
	jsr Lc75d 
	jmp Lcc2e 
Lcc20	
	ldy $61
	ldx $65
Lcc24	
	lda (ADSCR),y
	and #$7f
	sta (ADSCR),y
	iny 
	dex
	bne Lcc24 
Lcc2e	
	pla
	cmp #$20
	beq Lcc3b
	cmp #$1b
	beq Lcc3b
	cmp #$0d
	bne Lcc3e
Lcc3b	
	ldx VARMNB
	rts
Lcc3e
	cmp #$0a
	bne Lcc6d 
	lda VARMNB
	cmp $67 
	beq Lcc4d
	inc VARMNB
	jmp Lcbfd
Lcc4d	
	bit $68
	bmi Lcbfd 
	inc VARMNB
	inc $67
	inc $66
	bit FLGTEL  ; Minitel mode ?
	bvs Lcbeb 
	ldx $62
	ldy $63
	jsr $de54 ; FIXME
Lcc63	
	ldy $63
Lcc65	
	ldx VARMNB
	
	jsr display_x_choice 
	jmp Lcbfd
Lcc6d	
	cmp #$0b
	bne Lcc9a 
	lda VARMNB
	cmp $66
	bne Lcc92
	
	lda VARMNB
	beq Lcc94 
	dec $66
	dec $67 
	dec VARMNB 
	bit $020d ; Minitel ?
	bvs Lcc97 
	ldx $62
	ldy $63
	jsr $de5c  ; FIXME
	ldy $62
	jmp Lcc65
Lcc92	
	dec VARMNB
Lcc94
	jmp Lcbfd 
Lcc97	
	jmp Lcbeb
Lcc9a	
	cmp #$30
	bcc Lcc94
	cmp #$3a
	bcs Lcc94 
	ldx VARMNB
	cpx #$19
	bcc Lccae
Lcca8	
	ldx $66
	stx VARMNB 
	bcs Lcc94
Lccae	
	pha
	asl VARMNB
	lda VARMNB 
	asl VARMNB 
	asl VARMNB
	adc VARMNB
	sta VARMNB
	pla
	and #$0f
	adc VARMNB
	sbc #0
	
	sta VARMNB
	bcc Lcca8 
	cmp $66
	bcc Lcca8 
	cmp $67
	beq Lccd0  
	bcs Lcca8 
Lccd0	
	jmp Lcbfd
	
	
.)	
	

display_bar_in_inverted_video_mode
Lccd3
	jsr $cd5a ; FIXME
	bit $020d ; Minitel ?
	bvc Lccea 
	ldx #2
Lccdd	
	lda #9
	jsr XWR0_ROUTINE
	dex
	bpl Lccdd 
	lda #$2d
	jmp XWR0_ROUTINE 
Lccea	
	ldy $61 
	ldx $65
Lccee	
	lda ($26),y
	ora #$80
	sta ($26),y
	iny
	dex
	bne Lccee
	rts
Lccf9
display_x_choice
	tya
	pha
	txa
	pha
	pha
	jsr put_cursor_in_61_x	
	inx
	lda $69
	ldy $6a
	sta $15
	sty $16
	ldy #0
Lcd0c	
	dex
	beq Lcd20
Lcd0f
	iny 
	bne Lcd14
	inc $16
Lcd14	
	jsr $0411
	bne Lcd0f
	
	iny
	bne Lcd0c
	inc $16
	bne Lcd0c
Lcd20	
	ldx $16
	clc
	tya
	adc $15
	bcc Lcd29 
	inx
Lcd29	
	sta $02
	stx $03
	lda #$20
	sta $14
	pla
	clc
	adc #1
	ldy #0
	ldx #1 
	jsr $ce39 ; FIXME
	
	lda #$20
	jsr XWR0_ROUTINE
	lda $02
	ldy $03
	jsr XWSTR0_ROUTINE
	ldy #1
	jsr $0411
	sec
	beq Lcd51 
	clc
Lcd51	
	ror $68
	pla
	tax
	pla
	tay
	bit $68
	rts
	
	

put_cursor_in_61_x	
	lda #$1f
	jsr XWR0_ROUTINE
	tya
	ora #$40
	jsr XWR0_ROUTINE 
	lda $61
	ora #$40
	jmp XWR0_ROUTINE 

	
	

XDECAL_ROUTINE
	pha
	txa
	pha 
	tya
	pha
	sec
	lda $06
	sbc $04
	tay
	lda $07
	sbc $05
	tax
	bcc Lcdb9 
	stx $0b

	lda $08
	cmp $04
	lda $09
	sbc $05
	bcs Lcdbf 
	tya
	eor #$ff
	adc #1
	tay 
	sta $0a
	bcc Lcd97
	dex
	inc $07
Lcd97	
	sec
	lda $08
	sbc $0a
	sta $08
	bcs Lcda2

	dec $09
Lcda2	
	clc
	lda $07
	sbc $0b
	sta $07
	inx
Lcdaa	
	lda ($06),y
	sta ($08),y
	iny 
	bne Lcdaa
	inc $07
	inc $09
	dex
	bne	Lcdaa
Lcdb8	
	sec
Lcdb9	
	pla
	tay
	pla
	tax
	pla
	rts
Lcdbf
	txa
	clc
	adc $05
	sta $05
	txa
	clc
	adc $09
	sta $09
	inx
Lcdcc	
	dey
	lda ($04),y
	sta ($08),y
	tya
	bne Lcdcc
	dec $05
	dec $09
	dex
	bne Lcdcc
	beq Lcdb8
	

data_for_decimal_conversion
	
	.byt $0a,$64,$e8
	.byt $10,$00,$00,$03,$27

Lcde5	
	ldx #0
	ldy #0
	.byt $2c

Lcdea
convert_into_decimal_0_to_65535
	
	ldx #$03
	.byt $2c
Lcded
convert_into_decimal_0_to_9999
	ldx #2

	
XBINDX_ROUTINE
; AY contains the number
; X ...
.(
	STA TR1
	STY TR2
	LDA #$00
	STA TR3
	STA TR4
loop35
	LDA #$FF
	STA TR0
loop32
	INC TR0
	SEC
	LDA TR1
	TAY
	SBC $CDDD,X ; CORRECTME
	STA TR1
	LDA TR2
	PHA
	SBC $CDE1,X ; CORRECTME
	STA TR2
	PLA
	BCS loop32
	STY TR1
	STA TR2
	LDA TR0
	BEQ loop33
	STA TR3
	BNE loop34+1
loop33
	LDY TR3
	BNE loop34+1
	LDA DEFAFF
loop34
	BIT $3009 ; CORRECTME
	JSR lce32 
	DEX
	BPL loop35
	LDA TR1
	ORA #$30
lce32	
	LDY TR4

	STA (TR5),Y
	INC TR4
	RTS
.)

XDECIM_ROUTINE
	PHA
	LDA #$00
	STA TR5
	LDA #$01
	STA TR6
	PLA
	JSR XBINDX_ROUTINE
	LDY #$00
loop31

	LDA FUFTRV,Y
	jSR XWR0_ROUTINE
	INY
	CPY TR4
	BNE loop31
	RTS
	
	
XHEXA_ROUTINE	
	pha 
	and #$0f
	jsr Lce60 
	tay
	pla
	lsr
	lsr
	lsr
	lsr
Lce60
	ora #$30
	cmp #$3a
	bcc Lce68
	adc #$6
Lce68
	rts

XMUL40_ROUTINE
Lce69
mult_by_40
	ldy #0
	sta RES
	sty $01
	asl
	rol $01
	asl
	rol $01
	adc RES
	bcc Lce7b
	inc $01
Lce7b
	asl
	rol $01
	asl
	rol $01
	asl
	rol $01
	sta RES
	ldy $01
	rts

#include "functions/XADRES.asm"
	
XMULT_ROUTINE

	sta $10
	sty $11
	ldx #00
	stx $0c
	stx $0d
	stx $0e
	stx $0f
	stx $02
	stx $03
	ldx #$10
	lsr $11
	ror $10
	bcc $ceca ; FIXME
	clc
	
	lda RES
	adc $0c
	sta $0c
	
	lda $01
	adc $0d
	sta $0d
	
	lda $02
	adc $0e
	sta $0e
	
	lda $03
	adc $0f
	sta $0f

	asl RES
	rol $01
	rol $02
	rol $03
	
	lda TR4
	ora $11
	beq Lcedb
	dex
	bne $ceab ; FIXME
Lcedb	
	rts

XDIVIS_ROUTINE	
	sta $0c
	sty $0d
	ldx #0
	stx $02
	stx $03
	ldx #$10
Lcee8	
	asl RES
	rol $01
	rol $02
	rol $03
	sec
	lda $02
	sbc $0c
	tay 
	lda $03
	sbc $0d
	bcc $cf02 ; FIXME
	sty $02
	sta $03
	inc RES
	dex
	bne Lcee8
	rts

XEFFHI_ROUTINE
	lda #00
	ldy #$a0
	sta RES
	sty $01
	ldy #$68
	ldx #$bf
	lda #$40

XFILLM_ROUTINE	
	pha
	sec
	tya
	sbc RES
	tay
	txa
	sbc $01
	tax
	sty $02
	pla
	ldy #0
Lcf23	
	cpy $02
	bcs Lcf2c
	sta ($00),y
	iny
	bne Lcf23
Lcf2c	
	pha
	tya
	
	ldy #0
	jsr $ce89 ; FIXME
	pla
	cpx #0
	beq Lcf44 
	ldy #0
Lcf3a	
	sta ($00),y
	iny
	bne Lcf3a
	inc $01
	dex
	bne Lcf3a
Lcf44	
	rts
	
XHIRES_ROUTINE	
	ldx #$00
	ldy #$ff
	sty $02aa ; pattern
	iny
	jsr Le7f3 
	lda $020d ; we are already in Hires ?
	bmi $cf06 ; FIXME 
	ora #$80
	sta $020d ; Set to Hires flag
	
	php 
	sei
	lda #$1f
	sta $bf67 
	jsr $cfa4 ; FIXME
	jsr $fed8 ; FIXME
	lda #$5c
	ldy #$02
	ldx #0
	jsr $defd ; FIXME
	jsr $cf06 ; FIXME
	plp
	rts
	
XTEXT_ROUTINE	
switch_text
	lda $020d
	bpl $cfa3 ; FIXME
	php 
	
	sei
	and #$7f
	sta $020d
	jsr $fedb ; FIXME
	lda #$56
	ldy #$02
	ldx #0
	jsr $defd ; FIXME
	
	lda #$1a
	sta $bfdf
	jsr $cfa4 ; FIXME
	ldx #$28
	lda #$20

Lcf99	
	sta $bb7f,x
	dex
	bne Lcf99	
	jsr $de20
	plp
	rts
	
wait_0_3_seconds ; Wait 0,3333 seconds 	
	ldy #$1f
	ldx #$00
Lcfa8
	dex
	bne Lcfa8
	dey
	bne Lcfa8
	rts
	

test_if_all_buffers_are_empty
	sec
	.byt $24 ; jump
	clc
	ror $15
	ldx #0
Lcfb6	
	jsr $c50f ; FIXME
	bcc Lcfc3 ; FIXME
	txa
	adc #$0b
	tax
	cpx #$30
	bne Lcfb6 
Lcfc3	
	php
	lda #$dc ; FIXME
	ldy #$cf ; FIXME
	bcs Lcfce 
	lda #$e6
	ldy #$cf
Lcfce	
	bit $15
	bpl Lcfd7 
	jsr $fef9 ; FIXME Define prompt
	plp
	rts
Lcfd7	
	jsr $fef9 ; FIXME
	plp
	rts
	
	
	
	

table_to_define_prompt_charset
	.byt $7f ; char 127
	.byt $00,$00,$08,$3c,$3e,$3c,$08,$00,$00
table_to_define_prompt_charset_empty	
	.byt $7f,$00,$00,$08,$34,$32,$34,$08,$00,$00
	

XNOMFI_ROUTINE
	sta $15
	sty $16
	stx RES
	inc RES
	ldy $020c
	sty $517
	sty $500
	ldy #$0c
	lda #$3f
Ld005	
	sta BUFNOM,y
	dey
	bne Ld005
	txa 
	beq Ld049 
	
	cpx #1
	bne Ld034 
	jsr Ld0df 
	sec
	sbc #$41
	cmp #4
	bcs Ld05c
	sta BUFNOM
	sta $500
Ld022	
	ldx #1
	ldy #$0c
	lda #$3f
Ld028	
	cmp BUFNOM,y
	beq Ld032
	dey
	bne Ld028
	clc
	rts
Ld032	
	sec
	rts
Ld034
	ldy #1
	jsr Ld0df
	cmp #$2d
	bne Ld05c
	ldy #0
	jsr Ld0df
	sec
	sbc #$41
	bcs Ld04a 
Ld047	
	ldx #$81
Ld049	
	rts
Ld04a
	cmp #4
	bcs Ld047
	cpx #2
	bne Ld056
	sta $020c
	rts
Ld056

	sta BUFNOM
	ldy #2
	.byt $2c
Ld05c	
	ldy #0
	ldx #0
Ld060	
	jsr Ld0df 
	bcs Ld082
	cmp #$2e
	beq Ld082
	cmp #$2a ; is it '*' ?
	beq Ld08f 
Ld06d	
	jsr test_if_a_is_valid_in_a_filename 
	bcc Ld078
Ld072	
	ldx #$80
Ld074
	rts
Ld075
	ldx #$82
	rts
Ld078
	cpx #9
	beq Ld075
	
	sta BUFNOM+1,x
	inx 
	bne Ld060
Ld082	
	lda #$20
Ld084	
	cpx #9
	beq Ld08e 
	sta BUFNOM+1,x
	inx
	bne Ld084
Ld08e	
	dey
Ld08f	
	ldx #0
	jsr Ld0df 
	bcc Ld0a4 
	ldy #2
Ld098	
	lda $055d,y
	
	sta $0521,y
	dey
	bpl Ld098
Ld0a1	
	jmp Ld022 
Ld0a4	
	cmp #$2e ; is it '.' ?
	bne Ld072
	jsr Ld0df 
	bcs Ld08e
	dey
Ld0ae	
	jsr Ld0df 
	bcc Ld0c1 
	lda #$20
	cpx #3
	beq Ld0a1 
	sta $0521,x
	inx
	bne $d0b5
	beq Ld0a1
Ld0c1
	cmp #$2a
	bne Ld0cd
	jsr Ld0df 
	bcs Ld0a1
	ldx #$83
	rts
Ld0cd	
	jsr test_if_a_is_valid_in_a_filename
	bcs Ld072 
	cpx #3
	beq Ld0dc
	sta $0521,x
	inx
	bne Ld0ae 
Ld0dc	
	ldx #$84
	rts
	
Ld0df
	jsr $0411
	jsr uppercase_char
	iny 
	cpy RES
	bcs Ld0ef
	cmp #$20
	beq Ld0df
	clc
Ld0ef	
	rts
	
XMINMA_ROUTINE
uppercase_char
	cmp #"a" ; 'a'
	bcc Ld0fa
	cmp #$7b ; 'z'
	bcs Ld0fa 
	sbc #$1f
Ld0fa
	rts
test_if_a_is_valid_in_a_filename
Ld0fb
	cmp #$3f ; is it '?'
	beq Ld10d 
	cmp #$30
	bcc Ld10f	
	cmp #$3a ; 9 ?
	bcc Ld10d 
	cmp #"A"
	bcc Ld10f	
	cmp #$5b
Ld10d	
	clc	
	rts
Ld10f	
	sec
	rts
; MINITEL 
init_vdt_table
	ldy #0
	lda #$90
	sty RES
	sta $01
	ldx #$94
	lda #$0
	jsr $cf14 ; FIXME
	ldy #0
	lda #$94
	sty RES
	sta $01 
	ldx #$98
	lda #$87
	jsr $cf14 ; FIXME
	
	ldy #0
	lda #$a0
	
	sty RES
	sta $01
	ldy #$3f
	ldx #$bf
	lda #$10
	jmp $cf14 ; FIXME
	
	
; MINITEL
manage_VDT_cursor
Ld140
	lda $39
	jsr $ce69 ; FIXME
	pha
	tya
	pha
	lda #0
	ldy #$90
	jsr $ce89 ; FIXME
	sta $2e
	sty $2f
	sta $30
	iny
	iny
	iny
	iny
	sty $31
	pla
	sta $01
	pla
	asl
	rol $01
	asl
	rol $01
	asl 
	rol $01
	sta RES
	lda #0
	ldy #$a0
	jsr $ce89
	sta $2c
	sty $2d
	jmp $d756 ; FIXME
	

/**** MINITEL **/
/* 102 Bytes begin */
Ld178
;#ifdef HAVE_MINITEL
#include "functions/minitel/send_A_to_video_screen.asm"
;#endif
; follow_a_sequence
follow_a_sequence

Ld1ed	; 
	jsr manage_a_sequence 
	jmp Ld1e6

; manage control code	
Ld1f3
manage_control_code
	jsr $d1f9 ; FIXME
	jmp Ld1e6

manage_code_control_videotex
Ld1f9
	tax
	clc
	lda table_code_control	,x ; FIXME
	adc #$7e ; CORRECTME adress Aaddind to d37e
	sta RES
	lda #$d3
	adc #$00
	sta $01 ; CORRECTME
	jmp ($0000)
Ld20b

/**** BEGIN OF TABLE CONTROL **/
table_code_control	
	.byt $00,$00,$00,$00,$00,$00,$00  ; COde 0 to 6 not managed
	.byt $01 ; BEL (bip) $d37f
	.byt $04,$10,$22,$44,$53,$59,$63,$76,$00,$7d,$b3,$b6,$80
	.byt $00,$b9,$00,$83,$b9,$00,$bc,$00,$00,$a7,$bf
/**** END OF TABLE CONTROL **/
Ld22b
	jmp management_sequence_esc
Ld22e
manage_a_sequence
	lda $3c
	and #$03
	sta $36
	lda $3c
	asl
	bmi Ld22b
	asl
	asl
Ld23b	
	BMI Ld24e
	LDA $3E
	AND #$3F
	TAX
	LSR $3C
Ld244
	LDA $0285 ; FIXME
	JSR send_A_to_video_screen 
	DEX
	BNE Ld244
	RTS
Ld24e	
	LDA $36
	BEQ Ld26f
	LDA $3E
	CMP #$30
	BCC Ld26a
	CMP #$59
	BCS Ld26a
	STA $0282
	DEC $3C
	LDA #$07
	STA $34
	LDA #$00
	STA $32
	RTS
Ld26a
	lsr $3c
	jmp send_A_to_video_screen 
Ld26f	
	lsr $3c
	lda $3e
	cmp #$30
	bcc Ld26a
	cmp #$69
	bcs Ld26a
	and #$3f
	sta $0283
	lda $3e
	cmp #$40
	BCS Ld2a3 
	lda $0282
	and  #$03
	asl
	asl
	adc $0282
	sbc #$2f
	asl
	adc $283
	sbc #$2f
	sta $39
	jsr CTRL_M_KEYBOARD 

	jsr $d759 ; FIXME
	jmp $d140 ; FIXME
Ld2a3		
	jsr $d759 ; FIXME
	lda $0283
	sta $38
	dec $38
	lda $0282 ; CORRECTME
	and #$3f
	sta $39
	jmp $d140 ; FIXME

/*END routine */

/*Gestion de la sequence ESC*/
Ld2b7

management_sequence_esc
	LDX $36
	LDA $3E
	CPX #$03
	BNE Ld2e1 
	STA $0282
	LDX #$00
	STX $35
	CMP #$36
	BEQ Ld2d7 
	CMP #$39
	BCC Ld2f0 
	CMP #$3C
	BCS Ld2f0
	AND #$03
	SBC #$00
	.byt $2c ;jump 2 instructions
Ld2d7	
	lda #00
	ora #$c0

	sta $3c
Ld2dd	
	RTS
Ld2de
	lsr $3c
	rts
Ld2e1
	inc $35
	ldx $35
	sta $0281,x
	dec $3c
	dec $36
	bpl Ld2dd
	bmi Ld2de
Ld2f0
	CMP #$40
	BCC Ld2dd	
	LSR $3C
	CMP #$48
	BCS Ld307
	AND #$07
	STA $36
	LDA $34
	AND #$F8
	ORA $36
	STA $34
	RTS
Ld307
	CMP #$4A
	BCS Ld317
	LSR
	LDA $34
	AND #$F7
	BCS Ld314
	ORA #$08
Ld314
	STA $34
	RTS
Ld317
	CMP #$4C
	BCC Ld34a 
	CMP #$50
	BCS Ld330 
	AND #$03
	ASL
	ASL
	ASL
	ASL
	STA $36
	LDA $34
	AND #$CF
	ORA $36
	STA $34
	RTS

Ld330
	CMP #$58
	BCS Ld34b
	AND #$07
	ASL
	ASL
	ASL
	ASL
	STA $36
	LDA $32
	AND #$84
	ORA $36
	BIT $34
	BMI Ld348
	ORA #$80
Ld348
	STA $32
Ld34a
	RTS
Ld34b	
	BNE Ld34d
Ld34d
	CMP #$5B
	BCS Ld36c
	LSR
	BIT $34
	BPL Ld34f
	LDA #$00
	BCC Ld35c
	LDA #$40
Ld35c
	STA $33
	RTS
Ld34f
	LDA $32
	AND #$70
	BCS Ld367
	ORA #$04
Ld367
	ORA #$80
	STA $32
Ld36b
	RTS
Ld36c
	BEQ Ld36b
	CMP #$5E
	BCS Ld37e
	LSR
	LDA $34
	AND #$BF
	BCC Ld37b
	ORA #$40
Ld37b
	STA $34
	RTS
Ld37e
	RTS

CTRL_G_KEYBOARD ; Send oups
	jmp $ddd8 ; FIXME
CTRL_H_KEYBOARD 
	jsr $d759 ; switch off cursor  Why not BRK ?
	lda $38
	beq $d3be ; FIXME
Ld389	
	dec $38
Ld38b
	jmp $d756 ; FIXME
CTRL_I_KEYBOARD 	
	jsr $d759 ; FIXME
	inc $38
	lda $38
	cmp #$28
	bcc Ld38b
	lda $39
	beq Ld389	
	jsr $d3d7
CTRL_J_KEYBOARD ; 10 code
	jsr $d759 ; FIXME
	ldx $39
	beq $d3b3 ; FIXME
	cpx #$18
	bne Ld3ad
	ldx #$0
Ld3ad	
	inx
Ld3ae
	stx $39
	jmp $d140 ; FIXME
	
	lda $280
	ldx $281
	sta $38
	jmp  Ld3ae
	lda #$27 
	sta $38
	
CTRL_K_KEYBOARD 
	jsr $d759 ; FIXME
	ldx $39
	dex
	bne Ld3cc
	ldx #$18
Ld3cc	
	stx $39
	jmp $d140 ; FIXME
	
CTRL_L_KEYBOARD 
	jsr $d111 ; FIXME Initialize VIDEOTEX MINITEL
	jmp $d425 ; FIXME 

CTRL_M_KEYBOARD
	jsr $d759 ; FIXME
	lda #0
	sta $38
	jmp $d756 ; FIXME
	
CTRL_N_KEYBOARD ; clear current line in prompt
	lda #$40
	sta $33
	lda $32
	and #$74
	sta $32
	lda $34
	and #$0f
	ora #$80
	sta $34
	rts
CTRL_O_KEYBOARD
	lda $34
	and #$0f
	sta $34
	rts
CTRL_Q_KEYBOARD
	jmp $d74f ; FIXME
CTRL_T_KEYBOARD	
	jmp $d74d ; FIXME
	
CTRL_X_KEYBOARD
	lda $38
	pha
	lda $39
	pha
Ld407	
	lda #$20
	jsr send_A_to_video_screen 
	lda $38
	beq Ld419
	cmp #$27
	bne Ld407
	lda #$20
	jsr send_A_to_video_screen  
Ld419	
	jsr $d759 ; FIXME
	pla
	sta $39
	pla
	sta $38
	jmp $d140 ; FIXME
CTRL_DOT_KEYBOARD	
	jsr CTRL_M_KEYBOARD 
	jsr $d261 ; FIXME
	jsr $d759 ; FIXME
	jmp $d3ab ; FIXME

	lda #$89 ; REP
	.byt $2c
	
	lda #$84 ; SEP
	.byt $2c
	
	lda #$a1 ; SYN
	.byt $2c
	
	lda #$c3 ; ESC
	.byt $2c
	
	lda #$91
	sta $3c
	rts
; MINITEL
;#ifdefine HAVE_MINITEL
#include "functions/minitel/send_a_data_to_videotex_screen.asm"
;#endif







Ld5db
init_minitel
routine_to_define_7
	LDA #$00
	STA FLGVD0
	STA $3D ; CORRECTME
	STA $37 ; CORRECTME
	rts
; MINITEL
	bpl Ld5d9
	bcs stop_videotex_emulation  
	jsr $cf45   ; FIXME
	jsr $d5bd    ; FIXME
	lda #$96 ; FIXME : $D796
	ldy #$d7
	jsr $fef9 ; FIXME
	lda #$0c
Ld5d9	
	jmp $d178 ; Erase screen
	

#ifdef HAVE_MINITEL
; 281 bytes
#include "functions/minitel/display_in_videotex_mode.asm"

compute_A_and_02_for_double_width
	lda #0
	ldy #3
	lsr $02
	php
	ror
	plp
	ror
	dey
	bne $d6f9 ; FIXME
	lsr
	lsr
	and #$3f
	ora #$40
	rts
table_conversion_G1	
	.byt $1b,$1b,$00,$1b,$00,$1b,$1b
	.byt $00
#include "functions/minitel/minitel_display_mosaique.asm"
switch_off_cursor_videotex
	clc
	.byt $24
switch_on_cursor_videotex	
	sec
	php
	asl $3d
	plp
	ror $3d

Ld756	
display_cursor_videotex ; minitel
	lda #$80
	.byt $2c
	lda #00
	and $3d
	bit $3d
	bvc Ld763 
	lda #0
	
Ld763	
	sta $02
	lda $2c
	ldy $2d
	sta RES
	sty $01
	ldy $38
	lda ($30),y
	bmi Ld77d
	
	and #$40
	beq Ld77d 
	lda $02
	eor #$80
	sta $02
Ld77d	
	ldx #$8
	ldy $38
Ld781	
	lda ($00),y
	and #$7f
	ora $02 
	sta ($00),y
	clc
	tya
	adc #$28
	tay
	bcc Ld792 
	inc $01
Ld792	
	dex
	bne Ld781	 
	rts


table_of_char_videotex_special
	.byt $2f
	.byt $01,$02,$04,$04,$08,$08,$10,$20 ; "/"
	; ç is 
	.byt $5c
	.byt $20,$10,$08,$08,$04,$04,$02,$01  
	
	; £ is _
	.byt $5f
	.byt $00,$00,$00,$00,$00,$00,$00,$3f
	
	; copyright is -
	.byt $60
	.byt $00,$00,$00,$3f,$00,$00,$00,$00

	.byt $7b
	.byt $20,$20,$20,$20,$20,$20,$20,$20
	
	.byt $7c
	.byt $08,$08,$08,$08,$08,$08,$08,$08
	
	.byt $7d
	.byt $01,$01,$01,$01,$01,$01,$01,$01
	
	.byt $7e
	.byt $3f,$00,$00,$00,$00,$00,$00,$00,$00
#endif	

#ifdef HAVE_USBDRIVE
; 281 bytes reserved
.dsb 382,0
.asc "kernel.x02",0
#include "functions/usbdrive/ch376_primitives.asm"
#endif	

	

	


	
manage_keyboard
	jsr d903 
	beq Ld812
	ldx $0270
	bpl Ld7f1 
	lda $0271 
	and $01e8,x
	bne Ld807
Ld7f1	
	dey
	lda $0268,y
	sta $0271
	tya
	ora #$80
	sta $0270
	jsr Ld81f 
Ld800	
routine_to_define_20
;	CLD
	LDA $0272 ; CORRECTME
	JMP next60
Ld807	
	DEC $0274 ; CORRECTME
	BNE end2 
	JSR routine_to_define_21 ; FIXME
	JMP Ld815
Ld812
	STA $0270 ; CORRECTME
Ld815	
	LDA $0273 ; CORRECTME
next60		
	STA $0274 ; CORRECTME
end2
	RTS
next75
	jmp Ld8dd 
Ld81f
routine_to_define_21
	JSR LC8BF 
	LDA #$00
	PHA
	LDA $0270 ; CORRECTME
	ASL
	ASL
	ASL
	TAY
	LDA $0271 ; CORRECTME
next63
	LSR 
	BCS next62
	INY
	BCC next63
next62
	LDA $026C
	TAX
	AND #$90
	BEQ next64
	PLA
	ORA #$01
	PHA
	TYA
	ADC #$3F
	TAY
next64
	TYA
	CMP #$20
	BCC next65
	SBC #$08
	CMP #$58
	BCC next66
	SBC #$08
next66
	TAY
next65
	TXA
	AND #$20
	BNE next75 
	LDA ($2A),Y ; CORRECTME
	BIT $0275 ; CORRECTME
	BPL next67
	CMP #$61
	BCC next67
	CMP #$7B
	BCS next67
	SBC #$1F
next67
	TAY
	TXA
	AND #$04
	BEQ next68
	AND $026F ; CORRECTME
	BEQ next69
	LDA #$80
	STA $027E ; CORRECTME
next69
	PLA
	ORA #$80
	PHA
	TYA
	AND #$1F
	TAY
next68
	TYA
Ld882	
	LDX #$00
	PHA
	CMP #$06
	BNE next70
	LDA $0275 ; CORRECTME
	EOR #$40
	BCS next71
next70
	CMP #$14
	BEQ next72
	CMP #$17
	BNE next73
	LDA $0275
	EOR #$20
	BCS next71
next73
	CMP #$1B
	BNE next74
	LDA $0275
	AND #$20
	BEQ next74
	PLA
	LDA #$00
	PHA
next72
	LDA $0275 ; CORRECTME
	EOR #$80
next71
	STA $0275 ; CORRECTME
next74
	PLA
	LDX #$00
	JSR LC51D
	PLA
	LDX #$00
	JSR LC51D
	BIT $0275
	BVC end3
	LDX #<sound_bip_keyboard
	LDY #>sound_bip_keyboard
	JMP ld9e7
end3
	RTS
Ld8cf
sound_bip_keyboard
	.byt $1f
	.byt $00,$00,$00,$00,$00,$00
	.byt $3e,$10,$00,$00
	.byt $1f,$00,$00
/* END bip keyboard **/	

manage_function_touch
Ld8dd
	lda ($2a),y
	cmp #$2D
	beq Ld8f8 
	cmp #$3d
	beq Ld8fb 
	pla
	ora #$40
	pha
	lda $0275
	lsr
	bcs Ld900 
	lda ($2a),y
	and #$1f
	ora #$80
	.byt $2c
Ld8f8	
	lda #$60
	.byt $2c
Ld8fb	
	lda #$7e
	jmp Ld882 
Ld900
	jmp ($0276)


init_disk	
d903	
	LDY #$07
	LDA #$7F
loop21
	PHA
	TAX
	LDA #$0E
	JSR routine_to_define_11 ; OK
	LDA #$00
	STA KBDCOL,Y
	JSR routine_to_define_12 
	LDA V1DRB
	AND #$B8
	TAX
	CLC
	ADC #$08
	STA $1F
loop20	; d921
	STX V1DRB

	INX
	LDA #$08

	AND V1DRB
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
	LDA Ld9a9,X ; FIXME POUET
	ORA KBDCOL,Y
	STA KBDCOL,Y
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


manage_I_O_keyboard
Ld95c
	bmi Ld985 ; FIXME
	lda #1
	sta $2a8
	sta $2a6
	php
	sei
	ldx #0
	jsr $c518 ; FIXME
	bcs Ld982 
	sta $279
	ldx #00
	jsr $c518 ; FIXME
	bcs Ld982 
	sta $0278
	lda $0279
	plp
	clc
	rts
Ld982	
	plp
	sec
	rts
Ld985	
	bcc Ld98d
	lda #$40
	sta $030e
	rts
Ld98d	
	lda $030b
	ora #$40
	sta $030b
	
	lda #$a8
	ldy #$61
	sta $304
	sty $305
	lda #$c0
	sta $30e
	

flush_keyboard_buffer
	ldx #$00
	jmp $c50c ; FIXME

	
data_to_define_KBDCOL	
Ld9a9	
	.byt $01,$02,$04,$08,$10,$20,$40
	.byt $80
Ld9b1	
routine_to_define_4	
init_keyboard
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
send_14_paramaters_to_psg	
ld9e7
	CLC
	BIT $38
	PHP
	SEI
	LDA $16
	PHA
	LDA $15
	PHA
	STX $15
	STY $16
	PHP
	LDY #$00
ld9f9
	PLP
	PHP
	BCS lda01
	LDA ($15),Y
	BCC lda04
lda01
	JSR $0411
lda04
	TAX
	TYA
	PHA
	JSR lda1a 
	PLA
	TAY
	INY
	CPY #$0E
	BNE ld9f9
	PLP
	PLA
	STA $15
	PLA
	STA $16
	PLP
	RTS
	
	
	
lda1a
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
	LDA V1PCR
	AND #$11
	TAY
	ORA #$EE
	STA V1PCR
	TYA
	ORA #$CC
	STA V1PCR
	STX $030F
	TYA
	ORA #$EC
	STA V1PCR
	TYA
	ORA #$CC
	STA V1PCR
	PLP
	PLA
	TAY
	PLA
	rts

init_printer	
da4f
	LDA #$07
	LDX #$7F
	JMP routine_to_define_11 ; OK
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
Lda70	
	.byt $30
	rts
Lda72
.(
	PHA
	TXA
	PHA
	LDA #$82
	STA $030E
	TSX
	LDA $0102,X
	JSR $DAA5 ; FIXME
	BIT $028A
	BVS next901
	CMP #$20
	BCS next900
	CMP #$0D
	BNE next901
	BEQ next902
next900
	LDX $0286
	INX
	CPX $0288
	BCC next903
	JSR Ldae4 ; FIXME
next902
	LDX #$00
next903
	STX $0286
next901
	PLA
	TAX
	PLA

	RTS

.)	

	.byt $aa,$ad,$8a,$02

	.byt $29,$04,$f0,$08,$20,$2f,$db
	.byt $8a,$a2,$18,$d0,$0a,$ad,$0d,$02,$29,$02,$f0,$15,$8a,$a2,$24

next906
	BIT $028A	
	BVS next909

	CMP #$7F
	BNE next909

	
	LDA #$20
next909
	PHA	
	JSR $C51D ; FIXME
	pla 
	bcs next906

	rts

	
	BCS next908
	LDA $028A
	AND #$04
	BNE Ldae1	
	
	LDA #$82
	STA $030E
next908
	RTS

	
Ldae1
	jmp $db7d ; FIXME
	
Ldae4
	PHA
	LDA #$0D
	JSR Lda72 
	LDA $028A
	LSR
	BCS Ldaf5
	LDA #$0A
	JSR Lda72  
Ldaf5
	PLA
	RTS
	BMI Ldafe
	LDX #$0C
	JMP Lc518 
Ldafe
	BCS Ldb09
	LDA ACIACR
	AND #$0D
	ORA #$60
	BNE Ldb43 
Ldb09
	LDA ACIACR
	ORA #$02
	STA ACIACR
RTS
Ldb12

.byt 	$30,$26,$aa,$10,$0f,$c9,$c0,$b0,$0b,$09,$40,$48,$a9,$1b
	.byt $a2,$18,$20,$1d,$c5,$68,$48,$a2,$18,$20,$1d,$c5,$68,$b0,$f7,$ad
	.byt $1e,$03,$29,$f3,$09,$04,$8d,$1e,$03,$60,$b0,$17,$ad,$1e,$03,$29
	.byt $02,$09,$65
Ldb43	
	.byt $8d,$1e,$03,$ad,$21,$03,$29,$ef,$8d,$21,$03,$a9,$38

	.byt $8d,$1f,$03,$60
Ldb54	
init_rs232
	; RS232T: 
	;	b0 to b3 : speed
	;	b4 : external clock for 0, 1 for internal clock
	;	b6 - b5 : 00=8 bits, 01=7 bits, 10=6 bits, 11=5 bits
	;	b7 : 0=stop, 1= 2 or 1.5 stops

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
	.byt $20,$72,$da,$a5,$29
Ldbb5

	STA SCRNB+1
	PHA
	TXA
	PHA
	TYA
	PHA
	
	LDX SCRNB
	LDA ADSCRL,X
	STA ADSCR
	LDA ADSCRH,X
	STA ADSCR+1
	
	LDA SCRNB+1
	CMP #$20
	BCS Ldc4c
Ldbce	
	LDA FLGSCR,X
	PHA
	
	JSR lde1e 
	LDA #$DC
	PHA
	LDA #$2A
	PHA
	LDA SCRNB+1
	ASL
	TAY
	LDA Ldbec,Y 
	PHA
	LDA Ldbeb,Y 
	PHA
	LDA #$00
	SEC
	RTS

Ldbeb
	.byt $e9
Ldbec
	.byt $dc
	
	.byt $ea,$dc,$e9
	.byt $dc,$e9,$dc,$0c,$dd,$e9,$dc,$e9,$dc,$d7,$dd,$46,$dd,$91,$dd,$9c

	.byt $dd,$54,$dd,$b7,$dd,$66,$dd,$73,$dd,$e9,$dc,$11,$dd,$12,$dd,$ce
	.byt $dd,$cb,$dd,$e9,$dc,$e9,$dc,$10,$dd,$e9,$dc,$79,$dd,$e9,$dc,$e9
	.byt $dc,$0e,$dd,$0e,$dd,$0f,$dd,$fa,$dd,$0d,$dd,$a6,$28,$bc,$20,$02
	.byt $b1,$26,$9d,$4c,$02,$a5,$26,$9d,$18,$02,$a5,$27,$9d,$1c,$02,$68

	.byt $9d,$48,$02,$20,$2d,$de,$68,$a8,$68,$aa,$68,$60
Ldc4c

	LDA FLGSCR,X
	AND #$0C
	BNE Ldc9a
	LDA SCRNB+1
	BPL Ldc5d
	CMP #$A0
	BCS Ldc5d
	AND #$7F
Ldc5d
	STA SCRNB+1
	JSR Ldc6b 
	LDA #$09
	STA SCRNB+1
	JMP Ldbce 
	STA SCRNB+1
Ldc6b
	LDY #$80
	LDA FLGSCR,X
	AND #$20
	BNE Ldc76
	LDY #$00
Ldc76
	TYA
	ORA SCRNB+1
	STA CURSCR,X
	LDY SCRX,X

	STA (ADSCR),Y
	LDA FLGSCR,X
	AND #$02
	BEQ Ldc99
	LDA SCRY,X
	CMP SCRFY,X
	BEQ Ldc99
	TYA
	ADC #$28
	TAY
	LDA CURSCR,X
	STA (ADSCR),Y
Ldc99
	RTS
Ldc9a
	and #8
	beq $dcb8 ; FIXME
	lda $29
	bmi $dc46 ; FIXME
	cmp #$40
	bcc $dc46 ; FIXME
	and #$1f
	;.byt $29,$08,$f0,$1a,$a5,$29
	;.byt $30,$a4,$c9,$40,$90,$a0,$29,$1f
	.byt $20,$69,$dc,$a9,$09,$20,$b5,$db

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
	.byt $27,$60,$20,$69,$ce,$bd,$38,$02,$bc,$3c,$02,$4c,$89,$ce
lde1e
	CLC
	BIT $38
	PHP
	ASL $0248,X
	PLP
	ROR $0248,X
	bmi lde54
	LDA #$80
	AND $0248,X
	AND #$80
	EOR $024C,X
	LDY $0220,X
	STA (ADSCR),Y
	PHA
	LDA $0248,X
	AND #$02
	beq lde53
	LDA $0224,X
	CMP $0234,X
	BEQ lde53
	TYA
	ADC #$28
	TAY
	PLA
	STA (ADSCR),Y
	RTS

lde53
	pla
lde54	
	rts
	.byt $a9,$00,$85,$07,$a9,$28,$d0,$06,$a9,$ff,$85,$07

	.byt $a9,$d8,$85,$06,$86,$00,$98,$38,$e5,$00,$48,$8a,$24,$06,$10,$01

	.byt $98,$a6,$28,$20,$12,$de,$18,$7d,$28,$02,$90,$01,$c8,$85,$08,$84
	.byt $09,$18,$65,$06,$85,$04,$98,$65,$07,$85,$05,$68,$85,$00,$f0,$34
	.byt $30,$3b,$38,$a6,$28,$bd,$2c,$02,$fd,$28,$02,$85,$01,$a4,$01,$b1
	.byt $04,$91,$08,$88,$10,$f9,$18,$a5,$04,$65,$06,$85,$04,$a5,$05,$65

	.byt $07,$85,$05,$18,$a5,$08,$65,$06,$85,$08,$a5,$09,$65,$07,$85,$09
	.byt $c6,$00,$d0,$d9,$a4,$01,$a9,$20,$91,$08,$88,$10,$fb,$60,$90,$07
	.byt $a6,$28

	.byt $20,$1e,$de,$68,$60,$a9,$01,$8d,$16,$02,$a9,$80,$8d,$17
	.byt $02,$68,$60
ldee3	
data_to_define_2
	; text mode  Text mode bytes it will  fill SCRTXT
	.byt $00,$27,$01,$1b
	.byt $80,$bb ; adress of text mode (first byte)
	; hires mode it will  fill SCRHIR
	.byt $00,$27,$00,$02
	.byt $68,$bf ; last bytes for text mode
	
	
	
	.byt $00
	.byt $27,$1a,$1b,$80,$bb,$00,$27,$01,$18,$80,$bb,$38,$24
ldefd	
ROUTINE_TO_DEFINE_7
	CLC
	PHP
	STA $15 ; CORRECTME
	STY $16 ; CORRECTME
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
	STA SCRY,X
	TXA
	SEC
	SBC #$04
	TAX
	DEY
	BPL next18
; loop 4 times to set color ink/paper and flags on the 4 possibles screens	
	LDA #$07
	STA SCRCT,X ; Set ink to white
	LDA #$00
	STA SCRCF,X ; set paper to black
	LDA #$00
	STA FLGSCR,X
	LDA SCRDX,X
	STA SCRX,X ; init cursor to 0 (beginning of the line)
	LDA SCRDY,X
	STA SCRY,X
	LDA SCRBAL,X
	STA ADSCRL,X
	LDA SCRBAH,X
	STA ADSCRH,X
	LDA #$20
	STA CURSCR,X
	LDA SCRNB 
	PHA
	STX SCRNB 
	LDA #$0C
	JSR Ldbb5 
	PLA 
	STA SCRNB
	PLP
	rts

init_screens
LDF5B
routine_to_define_9
	LDA #$1A
	STA $BFDF ; Switch to text mode
	JSR routine_to_define_22 
	; fill the first line with space characters
	LDX #$27 ; loop on #$28 caracters
	LDA #$20
loop16
	STA $BB80,X  ; display space on first line in mode text
	DEX
	BPL loop16
	LDY #$11 ; loop with $12 to fill text definitions and Hires
loop17

	LDA data_to_define_2,Y ; data_to_define_2
	STA SCRTXT,Y ; thise fill also  SCRHIR
	DEY
	BPL loop17
	ASL FLGTEL
	LSR FLGTEL
	LDA #$F5
	LDY #$DE
	BIT FLGTEL
	BVS next14
	LDA #$56
	LDY #$02
Ldf8b
next14
	LDX #$00
	JMP ROUTINE_TO_DEFINE_7 ; $DEFD
Ldf90
	LDA V2DRB
	AND #$3F
	ORA #$40
	BNE next15
Ldf99
	LDA V2DRB
	AND #$3F
	ORA #$80
	
next15
	STA V2DRB
	LDA V2DRB
	AND #$1F
	rts

	.byt $38,$60
init_joystick
routine_to_define_5
	LDA #%01000001 ; SET mouse and joystick flag
	STA FLGJCK
; init JCKTAB values
.(
	LDX #$06 ; 7 bytes 
loop
	LDA telemon_values_for_JCKTAB,X ; data_to_define_3
	STA JCKTAB,X
	DEX
	BPL loop
.)

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
	STA V1IER
	RTS

telemon_values_for_JCKTAB
	.byt $0b,$0a,$20,$08,$09,$03,$03
Ldffa
	rts
Ldffb
	LDA JCGVAL
	AND #$04
	BNE Le014
	JSR Ldf90 
	AND #$04
	BNE Le01e
	DEC $0293 ; CORRECTME
	BNE Le037
	LDX $0297 ; CORRECTME
	JMP Le01e 
Le014
	JSR Ldf90 
	AND #$04
	BNE Le037
	LDX $0298 ; CORRECTME
Le01e
	STX $0293 ; CORRECTME
	STA $58 ; CORRECTME
	LDA JCGVAL
	AND #$1B
	ORA $58 ; CORRECTME
	STA JCGVAL
	LDA $58 ; CORRECTME
	BNE Le037
	LDA $029F ; CORRECTME
	JSR Le19f 
Le037
	LDA JCGVAL
	AND #$1B
	EOR #$1B
	BEQ Le05b
	JSR Ldf90 
	AND #$1B
	STA $58 ; CORRECTME
	LDA JCGVAL
	AND #$1B
	EOR $58
	BNE Le062
	DEC $0291
	BNE Le084
	LDX $0297 ; CORRECTME
	JMP Le065
Le05b
	JSR Ldf90 
	AND #$1B
	STA $58 ; CORRECTME
Le062
	LDX $0298 ; CORRECTME
Le065
	STX $0291 ; CORRECTME
	LDA JCGVAL
	AND #$04
	ORA $58 ; CORRECTME
	STA JCGVAL
	LDX #$04
	ORA #$04
Le076
	LSR
	PHA
	BCS Le080
	LDA JCKTAB,X
	JSR Le19f 
Le080
	PLA
	DEX
	BPL Le076
Le084
	RTS

le085

 /*                                                                             
                            GESTION DE LA SOURIS                            
                                                                                
                                                                                
Action:G?re la souris comme pr?c?demment le joystick gauche, ? ceci pr?s qu'il  
       ne s'agit plus avec la souris de g?rer un d?lai de r?p?tition (sauf pour 
       les boutons), mais plutot une vitesse de r?p?tition. Dans le buffer      
       clavier, l'octet KBDSHT ajout? au codes ASCII souris est 8, soit b3 ? 1. 
  */                                                                              
                                                                                
	JSR $DF99    ;  on lit la valeur souris     FIXME                       
	AND #$1B    ;   on isole les directions                           
	STA $58     ;   dans $58                                          
	CMP #$1B    ;   la souris bouge ?                                 
	BNE $E095   ;   non ---------------------------------------------- FIXME
	DEC $02A4   ;   on d?place ?                                     I
	BNE $E084   ;   non, on sort.                                    I FIXME
	LDA $02A5    ;  on place vitesse d?placement dans  <--------------
	STA $02A4    ;  $2A4                                              
	LDA $58     ;   on lit le code                                    
	CMP #$1B    ;   souris fixe ?                                     
	BEQ $E0B5    ;  oui ---------------------------------------------- FIXME
	AND #$1B     ;  non, on isole les valeurs direction              I
	EOR $028E   ;   et on retourne les bits de JCDVAL                I
	AND #$1B   ;    en isolant les bits direction                    I
	BNE $E0B5  ;    ce ne sont pas les m?mes exactement -------------O FIXME
	DEC $0292  ;    on r?p?te ?                                      I
	BNE $E0E0  ;    non                                              I FIXME
	LDX $0299 ;     oui, on met le diviseur r?p?tition               I
	JMP $E0BB ;  ---dans le compteur                                 I FIXME
	JSR $DF99  ; I  on lit la souris <-------------------------------- FIXME
	LDX $029A ;  I  on place le compteur avant r?p?tition             
	STX $0292 ;  -->dans le d?compteur                                
	AND #$1B  ;     on isole les bits de direction                    
	STA $58  ;      dans $58                                          
	LDA $028E ;     on prend JDCVAL                                   
	AND #$64 ;      %01100100, on isole les bits de Feu               
	ORA $58   ;     on ajoute les bits de direction                   
	STA $028E  ;    dans JDCVAL                                       
	LDA $58   ;                                                       
	ORA #$04  ;     on ?teint le feu principal                        
	LDX #$04  ;  
Le0d2
	LSR                                                              
	PHA                                                              
	BCS $E0DC     ;                           FIXME                        
	LDA $029D,X   ; et on envoie les valeurs ASCII dans le buffer     
	JSR $E19D      ;                                                    FIXME
	PLA                                                              
	DEX                                                              
	BPL Le0d2                                                       
	RTS                                                              

Le0e1

	LDA JCDVAL
	AND #$04
	BNE Le0fa
	JSR Ldf99 
	AND #$04
	BNE Le102
	DEC $0294 ; CORRECTME
	BNE Le11b
	LDX $0297 ; CORRECTME
	JMP Le102
Le0fa
	JSR Ldf99
	AND #$04
	LDX $0298 ; CORRECTME
Le102
	STA $58 ; CORRECTME
	STX $0294 ; CORRECTME
	LDA JCDVAL
	AND #$7B
	ORA $58
	STA JCDVAL
	LDA $58
	BNE Le11b
	LDA $029F ; CORRECTME
	JSR Le19d 
Le11b
	LDA JCDVAL
	AND #$20
	BNE Le137
	JSR Ldf99 
	LDA V2DRAB
	AND #$20
	BNE Le140
	DEC $0295
	BNE Le15b
	LDX $029C
	JMP Le140
Le137
	JSR Ldf99
	LDA V2DRAB
	LDX $029B
Le140
	STX $0295
	AND #$20
	STA $58 ; CORRECTME
	LDA JCDVAL
	AND #$5F
	ORA $58 ; CORRECTME
	STA JCDVAL
	AND #$20
	BNE Le15b
	LDA $02A2
	JSR Le19d
Le15b
	LDA JCDVAL
	AND #$40
	BNE Le177
	JSR Ldf99
	LDA V2DRAB
	AND #$80
	BNE Le180
	DEC $0296 ; CORRECTME
	BNE Le19c
	LDX $029C ; CORRECTME
	JMP Le180
Le177
	JSR Ldf99
	LDA V2DRAB
	LDX $029B ; CORRECTME
Le180
	STX $0296 ; CORRECTME
	LSR
	AND #$40
	STA $58 ; CORRECTME
	LDA JCDVAL
	AND #$3F ; CORRECTME
	ORA $58 ; CORRECTME
	STA JCDVAL
	AND #$40
	BNE Le19c 
	LDA $02A3 ; CORRECTME
	JMP Le19d 
Le19c
	RTS
Le19d
	
	.byt $38,$24
Le19f

	CLC
	PHP
	STX $58 ; CORRECTME
	LDX #$00
	JSR LC51D 
	LDA #$08
	PLP
	BCS Le1af
	LDA #$20
Le1af
	LDX #$00
	JSR LC51D 
	LDX $58   ; CORRECTME
	RTS
Le1b7	
	sec
	rts


	LDX $28
	LDA $0220,X
	PHA
	LDA $0224,X
	PHA

	LDA #$1E
	JSR Ldbb5 
	JSR Ldae4
Le1cb
	LDX $28
	LDY $0220,X
	LDA ($26),Y
	CMP #$20
	BCS Le1d8
	LDA #$20
Le1d8
	JSR Lda72  
	LDA $0220,X
	CMP $022C,X
	BEQ Le1eb 
Le1e3
	LDA #$09
	JSR Ldbb5 
	JMP Le1cb 
	
; minitel hard_copy videotex
; send on the printer hires screen in videotex mode
Le1eb
.(
	JSR Ldae4  
	LDX $28
	LDA $0224,X
	CMP $0234,X
	BNE Le1e3  
	LDA #$1F
	JSR Ldbb5 
	PLA
	ORA #$40
	JSR Ldbb5 
	PLA
	ORA #$40
	JMP Ldbb5 
	JSR Ldae4 
	LDA $0288
	PHA
	LDA #$28
	STA $0288
	LDA #$1E
	JSR Ld178  
Le21a
	LDY $38
	LDA ($30),Y
	BMI Le226  
	LDA ($2E),Y
	CMP #$20
	BCS Le228 
Le226
	LDA #$20
Le228
	JSR Lda72  
	LDA #$09
	JSR Ld178  
	LDA $38
	BNE Le21a 
	LDY $39
	DEY
	BNE Le21a 
	JSR Ldae4  
	PLA
	STA $0286
	rts
.)
	
Le241
data_for_hard_copy
	.byt $18,$33,$1b,$0a,$0d,$00,$f0,$4b,$1b,$0d,$0a,$40,$1b,$0a,$0a
	
execute_hard_copy_hires
	jmp ($0250)
	
hard_copy_hires

	LDX #$05
	LDA $028A ; fixme
	PHA
	ORA #$40
	STA $028A
	LDA $E240,X ; fixme
	JSR $DA72 ; fixme
	DEX
	BNE $E25E ; fixme
	STX $0C
	LDX #$06
	LDA $E245,X ; fixme
	JSR $DA72 ; fixme
	DEX
	BNE $E26B ; fixme
	STX $0D
	LDA #$05
	STA $0E
	LDA $0C
	ASL
	ASL
	ASL
	JSR $CE69 ; fixme
	STA $11
	TYA
	CLC
	ADC #$A0
	STA $12
	LDA #$08
	STA $10
	LDY $0D
	LDA ($11),Y
	TAX
	AND #$40
	BNE $E29B ; fixme
	TXA
	AND #$80
	TAX
	TXA
	BPL $E2A0 ; fixme
	EOR #$3F
	LDX $0E
	LSR
	DEX
	BPL $E2A2 ; fixme
	ROL $0F
	TYA
	CLC
	ADC #$28
	TAY
	BCC $E2B1 ; fixme
	INC $12
	DEC $10
	BNE $E290 ; fixme
	LDA $0F
	JSR $DA72 ; fixme
	DEC $0E
	BPL $E27A ; fixme
	INC $0D
	LDA $0D
	CMP #$28
	BNE $E276 ; fixme
	INC $0C
	LDA $0C
	CMP #$19
	BNE $E269 ; fixme
	LDX #$04
	LDA $E24B,X ; fixme
	JSR $DA72 ; fixme
	DEX
	BNE $E2D0 ; fixme
	PLA
	STA $028A
	RTS
Le2de
put_cursor_on_last_char_of_the_line
	LDY $022C,X
	.byt $24
	dey
	LDA ($00),Y
	CMP #$20
	BNE $E2F0 ; FIXME
	TYA
	CMP $0228,X
	BNE $E2E2 ; FIXME
	RTS

test_if_prompt_is_on_beginning_of_the_line

	cmp #$7f
	bne Le2f8
	tya
	cmp $0228,x
Le2f8	
	rts
Le2f9
	LDY $0228,X
	LDA ($00),Y
	CMP #$7F
	RTS
Le2e6
	LDX $28
	LDA $0224,X
	STA $61
Le2ed	
	LDA $61
	JSR $DE12 ; FIXME
	JSR $E2F9 ; FIXME

	beq Le302
	lda $61
	CMP $0230,X
	
	BEQ Le306
	DEC $61
	bcs Le2ed
Le302	
	CLC
	INY
	STY $60
Le306	
	RTS
put_cursor_on_beginning_of_the_line	
Le322	
	LDX $28
	LDA $0224,X
	STA $63
	JSR $DE12 ; FIXME
	JSR $E2DE ; FIXME
Le32f	
	STY $62
	BEQ Le34e 
	LDA $63
	CMP $0234,X
	BEQ Le34d 
	INC $63
	LDA $63
	JSR $DE12 ; FIXME
	JSR $E2F9 ; FIXME
	BEQ Le34b 
	JSR $E2DE ; FIXME
	BNE Le32f 
Le34b	
	DEC $63
Le34d	
	RTS
Le34e	
	rts



	JSR $E301 ; FIXME
	JMP $E361 ; FIXME
send_the_end_of_line_in_bufedt	
	
	LDX $28
	LDA $0220,X
	STA $60
	LDA $0224,X
	STA $61
	JSR $E322 ; FIXME
	LDA $61
	STA $65
	CMP $63
	BNE Le378
	LDA $62
	CMP $60
	BCS Le378
	LDA #$00
	STA $0590
	RTS
Le378	

	LDA #$00
	STA $64
	LSR $66
Le37e	
	LDA $65
	JSR $DE12 ; FIXME
	LDY $60
	LDA $65
	CMP $61
	BEQ Le390
	LDX $28
	LDY $0228,X
Le390	
	LDA ($00),Y
	CMP #$20
	BCS Le398
	ORA #$80
Le398	
	LDX $64
	BIT $66
	BPL Le3a4
	LDA #$20
	STA ($00),Y
	BNE Le3b1
Le3a4
	STA $0590,X
	INC $64
	CPX $67
	BCC Le3b1
	DEC $64
	ROR $66
Le3b1	
	TYA
	INY
	LDX $65
	CPX $63
	BNE Le3c5
	CMP $62
	BNE Le390
	LDX $64
	LDA #$00
	STA $0590,X
	RTS
Le3c5	


	LDX $28
	CMP $022C,X
	BNE Le390
	INC $65
	BNE Le37e
	
display_bufedt_content	

	ROR $66
	LDA #$00
	STA $64
	LDA $26
	LDY $27
	STA RES
	STY $01
	LDX $28
	LDY $0220,X

Le3e3
	LDX $64
	LDA $0590,X
	BEQ Le41c 
	LDA #$20
	BIT $66
	BMI Le3fb
	LDA $0590,X
	BPL Le3fb
	CMP #$A0
	BCS Le3fb
	AND #$1F
Le3fb	
	STA ($00),Y

	BIT $020D ; Minitel ?
	BVC Le405
	JSR $E656 ; FIXME
Le405	
	TYA
	INY
	LDX $28
	CMP $022C,X
	BNE Le418
	LDA #$28
	LDY #$00
	JSR $CE89 ; FIXME
	LDY $0228,X
Le418	
	INC $64
	BNE Le3e3
Le41c	
	BIT $020D ; Minitel ?
	BVC Le42a 
	LDX $0220
	LDY $0224

	JSR $E62A ; FIXME
Le42a		
	LDY $0220
	LDA ($26),Y
	LDX $28
	STA $024C,X
	RTS
edit_line_in_bufedt

	STA $67
	TXA
	PHA
	TYA
	BPL Le446
	JSR $E301 ; FIXME
	LDX $60
	LDY $61
	JSR $E62A  ; FIXME
Le446	
	LDA #$0D
	JSR $E648  ; FIXME
	JSR $E66C  ; FIXME
	PLA
	TAX
	BEQ Le45a
Le452	
	LDA #$09
	JSR $E648  ; FIXME
	DEX
	BNE Le452  
Le45a	
	LDX $28
	LDA $0248,X
	BMI Le466 
	LDA #$11
	JSR $E648  ; FIXME
Le466
	JSR $CFAF  ; FIXME
	JSR $C7CF  ; FIXME
	BCS Le466 
	PHA
	LDA #$11
	JSR $E648  ; FIXME
	PLA
	CMP #$0D
	BNE Le4bc
Le479	
	PHA
	JSR $E34F  ; FIXME
	PLA
	PHA
	CMP #$0B
	BEQ Le494  
	LDX $62
	LDY $63
	JSR $E62A  ; FIXME
	LDA #$0D
	JSR $E648  ; FIXME
	LDA #$0A
	JSR $E648  ; FIXME
Le494	
	LDX #$FF
Le496	
	INX
	LDA $0590,X
	CMP #$20
	BEQ Le496 
	TXA
	PHA
	LDY #$00
	.byt $2c
Le4a3	
	inx
	iny

	LDA $0590,X
	STA $0590,Y
	BNE Le4a3 
	LDA #$90
	LDY #$05
	JSR $E749  ; FIXME
	STA RES
	STY $01
	PLA
	TAY
	PLA
	RTS


Le4bc



	CMP #$03
	BEQ Le479
	CMP #$0E
	BNE Le4d1
	JSR $E301 ; FIXME
	LDX $60
	LDY $61
	JSR $E62A ; FIXME
	JMP $E4D5 ; FIXME
Le4d1	
	CMP #$18
	BNE Le4df
	JSR $E355 ; FIXME
	SEC
	JSR $E3D0 ; FIXME
	JMP $E45A ; FIXME
Le4df	
	CMP #$7F
	BNE Le52a
	LDA $0278
	LSR
	BCS Le4f3
	BCC Le4eb ; awful
Le4eb	
	LDA #$08
	.byt $2c
Le4ee	
	lda #9
	
	JSR $E648 ; FIXME
Le4f3	
	LDX $28
	LDA $024C,X
	CMP #$7F
	BEQ Le4ee 
	JSR $E355 ; FIXME
	LDA $0590
	BNE Le511
	LDA #$20
	JSR $E648 ; FIXME
	LDA #$08
	JSR $E648 ; FIXME
	JMP $E45A ; FIXME
Le511	
	LDX #$01
Le513	
	LDA $0590,X
	BEQ Le51e
	STA $058F,X
	INX
	BNE Le513
Le51e	
	LDA #$20
	STA $058F,X
	CLC
	JSR $E3D0 ; FIXME
	JMP $E45A ; FIXME
Le52a	
	CMP #$20
	BCC Le534
	JSR $E537 ; FIXME
	JMP $E45A ; FIXME
Le534	
	JMP $E5B9 ; FIXME
manage_normal_char

	TAY
	TXA
	PHA
	TYA
	PHA
	JSR $E355 ; FIXME
	LDA $62
	LDY $0590
	BNE  Le548
	LDA $60
Le548	
	LDX $28
	CMP $022C,X
	BNE Le5ae
	LDA $63
	CMP $0234,X
	BEQ Le5ae
	ADC #$01
	JSR $DE12 ; FIXME
	JSR $E2F9 ; FIXME
	BNE Le5ae
	LDY $0234,X
	LDX $63
	INX
	JSR $DE5C ; FIXME
	BIT $020D ; Minitel ?
	BVC Le5ae
	LDX #$00
	LDY $63
	INY
	JSR $E62A ; FIXME
	LDA #$18
	JSR $E656 ; FIXME
	LDA #$0A
	JSR $E648 ; FIXME
Le580	
	LDX $28
	LDA $024C,X
	CMP #$7F
	BNE Le58f
	JSR $E66C ; FIXME
	JMP $E597 ; FIXME
Le58f	
	JSR $E656 ; FIXME
	LDA #$09
	JSR $DBB5 ; FIXME
	LDA $0224
	CMP $0234,X
	BNE Le580
	LDA $0220
	CMP $022C,X
	BNE Le580
	LDY $61
	LDX $60
	JSR $E62A ; FIXME
Le5ae	
	PLA
	JSR $E648 ; FIXME
	CLC 
	JSR $E3D0 ; FIXME
	PLA
	TAX
	RTS
manage_code_control
Le5b9
	

	CMP #$08
	BNE Le5d5
	PHA
	LDA $0278
	LSR
	BCS Le5cb
Le5c4	
	PLA
	JSR $E648 ; FIXME
	JMP $E45A ; FIXME
Le5cb	
	JSR $E301 ; FIXME
	LDX $60
Le5d2	
	LDY $61
	JMP $E5E7 ; FIXME
Le5d5
	CMP #$09
	BNE Le5ee
	PHA
	LDA $0278
	LSR
	BCC Le5c4
	JSR $E322 ; FIXME
	LDX $62
	LDY $63
	PLA
	JSR $E62A  ; FIXME
	JMP $E45A  ; FIXME
Le5ee	
	CMP #$0A
	BNE Le604
	LDX $28
	LDA $0224,X
	CMP $0234,X
	BNE Le615
	LDA #$0A
	.byt $2c
Le5ff	
	lda #$0b
	
	JMP $E479 ; FIXME
Le604	
	CMP #$0B
	BNE Le617
	LDX $28
	LDA $0224,X
	CMP $0230,X
	BEQ Le5ff
	LDA #$0B
	
	.byt $2c
Le615	
	lda #$0a
Le617	
	CMP #$0C
	BNE Le624
	
	JSR Le648 
	JSR Le66c 
	JMP $E45A ; FIXME
Le624	
	JSR Le648 
	JMP $E45A ; FIXME




/*
POSITIONNE LE CURSEUR EN X,Y                        
                                                                                
Action:positionne le curseur ? l'?cran et sur le minitel s'il est actif en tant 
       que sortie vid?o.                           
*/
Le62a
	LDA #$1F      ; on envoie un US                                   
	JSR Le648                                                     
	TYA          ;  on envoie Y+64                                    
	ORA #$40                                                         
	JSR Le648                                                    
	TXA         ;   et X+64                                           
	ORA #$40                                                         
	JSR $DBB5        ; FIXME                                                
	BIT $020D     ; mode minitel ?                                    
	BVC $E66B     ; non      ; FIXME                                          
	INX           ; on ajoute une colonne                             
	TXA           ; dans A                                            
	DEX           ; et on revient en arri?re                          
	ORA #$40     ;  on ajoute 40                                      
	JMP $E656    ;  et on envoie au minitel ; FIXME   

	/*
                   ENVOIE UN CODE SUR LE TERMINAL VIDEO                    
                                                                                
Action:envoie un code sur l'?cran et ?ventuellement sur le minitel s'il est     
       actif comme sortie vid?o. Seule la fen?tre 0 est g?r?e, ce qui ote       
       d?finitivement tout espoir de gestion d'entr?e de commande sur une autre 
       fen?tre.                                                                 
      */                                                                          
Le648                                                                        
	BIT $020D    ;  mode minitel ?                                    
	BVC $E650    ;  non ---------------------------------------------- FIXME
	JSR $E656    ;  oui, on envoie le code au minitel                I FIXME
	BIT $E650    ;  V=0 et N=0 pour ?criture <------------------------ FIXME
	JMP $DB86    ;  dans la fen?tre 0                                 
                                                                                
/*
                 ENVOIE UN CODE AU BUFFER SERIE SORTIE                    
  */                                                                              
	STA $0C    ;    on sauve le code <--------------------------------
	TYA        ;    on sauve Y                                       I
	PHA        ;                                                     I
	TXA         ;   et X                                             I
	PHA         ;                                                    I
	LDX #$18   ;    on indexe buffer ACIA sortie (minitel sortie)    I
	LDA $0C     ;   on envoie le code                                I
	JSR $C51D   ;                                                    I FIXME
	PLA         ;   on restaure les registres                        I
	TAX        ;                                                     I
	PLA        ;                                                     I
	TAY         ;                                                    I
	LDA $0C     ;                                                    I
	BCS $E656   ;   si l'envoi s'est mal pass?, on recommence -------- FIXME
	RTS                     
	/*																			
                             AFFICHE LE PROMPT                              
      */  
Le66c	  
	BIT $020D  ;    mode minitel ?                                    
	BVC $E67B   ;   non ---------------------------------------------- FIXME
	LDA #$19   ;    on envoie SS2 2/E au minitel                     I
	JSR $E656  ;                            FIXME                         I
	LDA #$2E   ;    donc on affiche la fl?che ->                     I
	JSR $E656  ;                       FIXME                              I
	LDA #$7F   ;    on affiche un prompt <----------------------------
	JMP $DBB5   ;   ? l'?cran          FIXME     
																				
/*

                   CHERCHE UNE LIGNE D'APRES SON NUMERO                    
                                                                                
                                                                                
Action:Recherche la ligne num?ro RES ? partir de l'adresse SCEDEB.              
       Une ligne de programme est compos?e de l'ent?te de 3 octets suivants:    
       1er octet    :longeur de la ligne, ou 0 si derni?re ligne                
       2 et 3e octet:num?ro de la ligne.                                        
       En sortie, C=1 si la ligne a ?t? trouv?e (adresse dans RESB), 0 sinon.   
                                                                                
*/																				

Le680
	LDA $5C   ;     AX=adresse de base de recherche                   
	LDX $5D   ;                                                       
	STX $03   ;     dans RESB                                         
	STA $02   ;  -->                                                  
	LDY #$00   ; I                                                    
	LDA ($02),Y ;I  on lit la longeur de la ligne                     
	BEQ $E6AE   ;I  0, on sort --------------------------------------- FIXME
	TAX         ;I  on sauve la longueur dans X                      I
	LDY #$02    ;I  on lit le num?ro de la ligne                     I
	LDA $01     ;I                                                   I
	CMP ($02),Y ;I  poids fort lu ?gal au demand? ?                  I
	BCC $E6AE   ;I  sup?rieur, on sort ------------------------------O FIXME 
	BEQ $E69B   ;I  ?gal, on continue le test                        I FIXME
	BCS $E6A4   ;I  inf?rieur, on passe --------------------------   I FIXME
	DEY         ;I  on lit le poids faible                       I   I
	LDA RES     ;I                                               I   I
	CMP ($02),Y ;I  poids faible lu ?gal au demand? ?            I   I
	BCC $E6AE   ;I  sup?rieur, on sort --------------------------+---O FIXME
	BEQ $E6AF   ;I  ?gal, on sort avec C=1                       I   I FIXME
	CLC         ;I  <---------------------------------------------   I
	TXA         ;I                                                   I
	ADC $02     ;I  on passe la ligne                                I
	BCC $E686   ;I                                                   I FIXME
	INC $03     ;I                                                   I
	BCS $E686   ;---et on continue                                   I FIXME
	CLC         ;   C=0, ligne non trouv?e <--------------------------
	RTS      

/*																			                                                                               
                      INSERE UNE LIGNE DANS UN LISTING                      
                                                                                
                                                                                
Action:ins?re la ligne num?ro RES contenue ? l'adresse TR0-1, de longueur A     
       dans le listing commen?ant ? l'adresse SCEDEB et finissant ? l'adresse   
       SCEFIN. En sortie, SCEDEB contient l'adresse de la ligne dans le listing,
       SCEFIN la nouve fin du listing et TR3-4 la diff?rence de taille du       
       listing, en compl?ment ? 2. Tr?s pratique tout ?a !                      
                                                                                
*/                                                                       
Le6b0
	STA $0E     ;   sauve la longueur de la ligne                     
	LDA #$00    ;   met 0 dans TR3-4                                  
	STA $0F                                                          
	STA $10                                                          
	JSR $E680   ;   cherche le num?ro de la ligne ? ins?rer        FIXME   
	BCC $E6E7   ;   la ligne n'existe pas ---------------------------- FIXME
	STX $0F     ;   on sauve la longueur de la ligne trouv?e         I
	LDA $5E     ;   on met SCEFIN                                    I
	LDY $5F     ;                                                    I
	STA $06     ;   dans DECFIN                                      I
	STY $07     ;                                                    I
	LDA $02     ;   adresse de la ligne trouv?e                      I
	LDY $03     ;                                                    I
	STA $08     ;   dans DECCIB                                      I
	STY $09     ;                                                    I
	CLC         ;                                                    I
	TXA        ;                                                     I
	ADC $02     ;                                                    I
	BCC $E6D6  ;    et l'adresse de la fin de la ligne               I FIXME
	INY         ;                                                    I
	STA $04     ;   dans DECDEB                                      I
	STY $05    ;                                                     I
	JSR $CD6C   ;   et on ram?ne la fin du listing (efface la ligne) I FIXME
	LDA #$FF   ;    on met -1                                        I
	STA $10     ;   dans TR4                                         I
	EOR $0F    ;    on compl?mente TR3 ? 2                           I
	STA $0F     ;   donc on remet dans TR3-4                         I
	INC $0F     ;   l'oppos? de TR3-4                                I
	LDA $0E      ;  on prend la longueur ? ins?rer <------------------
	BEQ $E738  ;    c'est 0, on devait effacer la ligne -------------- FIXME
	LDA $5E     ;   on prend la fin du listing                       I
	LDY $5F    ;                                                     I
	STA $06    ;    dans DECFIN                                      I
	STY $07    ;                                                     I
	LDA $02    ;    on prend l'adresse de la ligne                   I
	LDY $03    ;                                                     I
	STA $04    ;    dans DECDEB                                      I
	STY $05    ;                                                     I
	CLC        ;            #A4FF                                    I
	LDA $0E    ;    on ajoute 3 ? la longueur (ent?te de ligne)      I
	ADC #$03   ;                                                     I
	PHA         ;   dans la pile                                     I
	ADC $02     ;   on ajoute la longueur                            I
	BCC $E706   ;   ? DECDEB                                         I FIXME
	INY         ;                                                    I
	STA $08     ;   dans DECCIB                                      I
	STY $09      ;                                                   I
	JSR $CD6C    ;  et on lib?re la place pour la ligne              I FIXME
	CLC         ;                                                    I
	PLA        ;                                                     I
	PHA         ;   on prend la longueur                             I
	ADC $0F    ;    on calcule longueur nouvelle ligne               I
	STA $0F   ;     - longueur ligne pr?c?dente                      I
	BCC $E718 ;                                                      I FIXME
	INC $10   ;     dans TR3-4 (compl?ment ? 2)                      I
	LDY #$00  ;     on ?crit la longueur de la ligne                 I
	PLA        ;                                                     I
	STA ($02),Y ;                                                    I
	INY         ;                                                    I
	LDA RES    ;                                                     I
	STA ($02),Y ;   le poids faible du num?ro de ligne               I
	INY         ;                                                    I
	LDA $01      ;                                                   I
	STA ($02),Y  ;  le poids fort du num?ro de ligne                 I
	LDX #$00     ;                                                   I
	INY          ;                                                   I
	LDA ($0C,X) ;   et le contenu de la ligne                        I
	STA ($02),Y  ;  ? la suite                                       I
	INC $0C     ;                                                    I
	BNE $E734  ;                                                     I FIXME
	INC $0D     ;                                                    I
	DEC $0E    ;    jusqu'? la fin                                   I
	BNE $E729  ;                                                     IFIXME
	CLC        ;    <-------------------------------------------------
	LDA $0F    ;    on calcule dans SCEFIN                            
	ADC $5E    ;                                                      
	STA $5E    ;                                                      
	LDY $10   ;     la nouvelle adresse de fin du listing             
	TYA                                                              
	ADC $5F                                                          
	STA $5F                                                          
	LDA $0F    ;    et AY=diff?rence de longuer des lignes            
	RTS     


/*
                                                                               
                        CONVERSION ASCII -> BINAIRE                         
                                                                                
                                                                                
Principe:On lit un ? un les chiffres de la chaine stock?e en AY jusqu'? ce      
         qu'on ait plus de chiffres. On multiplie au fur et ? mesure le resultat
         par 10 avant d'ajouter le chiffre trouv?. Le principe est ais? ?       
         assimiler et la routine compacte. Un bon exemple d'optimisation.       
         En sortie, AY et RESB contient le nombre, AY l'adresse de la chaine,   
         et X le nombre de caract?res d?cod?s.                                  
 */                                                                               
Le749                                                                                
	STA RES     ;   on sauve l'adresse du nombre                      
	STY $01    ;    dans RES                                          
	LDY #$00   ;    et on met RESB ? 0                                
	STY $02                                                          
	STY $03                                                          
	LDA ($00),Y ;   on lit le code <------------------------------    
	CMP #$30    ;   inf?rieur ? 0 ?                              I    
	BCC $E785  ;    oui -----------------------------------------+---- FIXME
	CMP #$3A    ;   sup?rieur ? 9 ?                              I   I
	BCS $E785   ;   oui -----------------------------------------+---O FIXME
	AND #$0F    ;   on isole le chiffre                          I   I
	PHA        ;    dans la pille                                I   I
	ASL $02    ;    RESB*2                                       I   I
	ROL $03     ;                                                I   I
	LDA $02    ;    AX=RESB*2                                    I   I
	LDX $03    ;                                                 I   I
	ASL $02    ;    *4                                           I   I
	ROL $03    ;                                                 I   I
	ASL $02    ;    *8                                           I   I
	ROL $03   ;                                                  I   I
	ADC $02    ;    +RESB*2                                      I   I
	STA $02    ;                                                 I   I
	TXA        ;                                                 I   I
	ADC $03    ;                                                 I   I
	STA $03     ;   = RESB*10                                    I   I
	PLA         ;   plus chiffre lu                              I   I
	ADC $02     ;                                                I   I
	STA $02     ;                                                I   I
	BCC $E782  ;                                                 I   IFIXME
	INC $03    ;                                                 I   I
	INY       ;     on ajoute un chiffre lu                      I   I
	BNE $E753 ;     et on recommence  ----------------------------   IFIXME
	TYA       ;     nombre de chiffres lus <--------------------------
	TAX       ;     dans X                                            
	LDA $02   ;     nombre dans AY et RESB                            
	LDY $03    ;                                                      
	RTS
	
data_for_hires_display
Le78c
	.byt $20,$10,$08,$04
	.byt $02,$01

display_cursor_in_hires
	CLC       ;     C=0                                               
	BIT $56      ;  on fait tourner HRS5+1 sur lui-m?me               
	BPL $E798   ;   afin de conserver le pattern                      FIXME
	SEC                                                              
	ROL $56                                                          
	BCC $E7C0  ;    si b7 de $56   ? 0, on saute <-------------------- FIXME
	LDY $49     ;   sinon on prend X/6                               I
	LDA ($4B),Y ;   on lit le code actuel                            I
	ASL         ;   on sort b7                                       I
	BPL $E7C0   ;   pas pixel, on sort ------------------------------O FIXME
	LDX $4A     ;   on prend le reste de X/6                         I
	LDA data_for_hires_display,X  ;  on lit le bit correspondant                      I 
	BIT $57     ;   b7 de HRSFB ? 1 ?                                I
	BMI $E7BA   ;   b7 ? 1, donc 3 ou 2                              I FIXME
	BVC $E7B3   ;   FB=0 ----------------------------------------    I FIXME
	ORA ($4B),Y  ;  FB=1, on ajoute le code                     I    I
	STA ($4B),Y ;   et on le place                              I    I
	RTS         ;                                               I    I
	EOR #$7F    ;   on inverse le bit  <-------------------------    I
	AND ($4B),Y ;   et on l'?teint                                   I
	STA ($4B),Y  ;  avant de le placer                               I
	RTS         ;                                                    I
	BVS $E7C0   ;   FB=3, on sort -----------------------------------O FIXME
	EOR ($4B),Y ;   FB=2, on inverse le bit                          I
	STA ($4B),Y  ;  et on sort     	a                                I
Le7c0
	RTS  
/*
                                                                               
                    DEPLACEMENT RELATIF DU CURSEUR HIRES                    
                                                                                
                                                                                
Action:Ces quatres routines permettent un d?placement extr?mement rapide du     
       curseur HIRES d'apr?s l'adresse de la ligne ou il se trouve (ADHRS),     
       la colonne dans laquelle il se trouve (HRSX40) et sa position dans       
       l'octet point? (HRSX6).                                                  
       Attention:Les coordonn?es HRSX et HRSY ne sont pas modifi?es ni v?rifi?es
                 avant le d?placement, ? vous de g?rer cela.                    
                                                                                
                    DEPLACE LE CURSEUR HIRES VERS LE BAS                    	
*/
Le7c1	
	CLC       ;     on ajoute 40                                      
	LDA $4B    ;    ? ADHRS                                           
	ADC #$28                                                         
	STA $4B                                                          
	BCC Le7c0                                                     
	INC $4C                                                          
	RTS     
/*	
                   DEPLACE LE CURSEUR HIRES VERS LE HAUT                    
  */

 Le7cd
	SEC      ;      on soustrait 40                                   
	LDA $4B   ;     ? ADHRS                                           
	SBC #$28                                                         
	STA $4B                                                          
	BCS     Le7c0                                                     
	DEC $4C                                                          
	RTS      	
/*	

                     DEPLACE LE CURSEUR VERS LA DROITE                      
  */
  Le7d9
	LDX $4A  ;      on d?place d'un pixel                             
	INX                                                              
	CPX #$06  ;     si on est ? la fin                                
	BNE Le7e4
	LDX #$00   ;    on revient au d?but                               
	INC $49     ;   et ajoute une colonne 
Le7e4
	STX $4A                                                          
	RTS    
/*	
                     DEPLACE LE CURSEUR VERS LA GAUCHE                      
  */

Le7e7  
	LDX $4A                                                          
	DEX         ;   on d?place ? gauche                               
	BPL Le7f0   ;   si on sort                                        
	LDX #$05    ;   on se place ? droite                              
	DEC $49     ;   et on enl?ve une colonne 
Le7f0                       
	STX $4A                                                          
	RTS      	


/*
                         PLACE LE CURSEUR EN X,Y                           
                                                                                
                                                                                
Action:calcule l'adresse du curseur en calculant la position de la ligne par    
       $A000+40*Y, la colonne dans X/6 et la position dans l'octet par X mod 6. 
       Suite ? une erreur dans la table des vecteur TELEMON, cette routine n'est
       pas appel?e (alors qu'elle devrait l'?tre) par BRK XHRSSE...             
       En sortie, HSRX,Y,X40,X6 et ADHRS sont ajust?s en fonction de X et Y.    
*/


Le7f3                                                                                
	STY $47   ;     Y dans HRSY                                       
	STX $46   ;     X dans HRSX                                       
	TYA       ;     et Y dans A                                       
	LDY #$00  ;     AY=A, ligne du curseur                            
	JSR $CE69  ;    on calcule 40*ligne               FIXME                 
	STA $4B    ;                                           
	CLC                                                              
	TYA                                                              
	ADC #$A0    ;   et on ajoute $A000, ?cran HIRES                   
	STA $4C    ;    dans ADHRS                                        
	STX RES    ;    on met la colonne dans RES                        
	LDA #$06   ;    A=6                                               
	LDY #$00   ;    et Y=0  (dans RES+1)                              
	STY $01   ;     AY=6 et RES=colonne                               
	JSR $CEDC ;     on divise la colonne par 6       FIXME                  
	LDA RES   ;     on sauve colonne/6 dans HSRX40                    
	STA $49  ;                                                        
	LDA $02  ;      et le reste dans HRSX6                            
	STA $4A  ;                                                        
	RTS      ;      I
 /*                                                                               
                                                                               
                       TRACE UN RECTANGLE EN RELATIF                        
                                                                                
                                                                                
Principe:On calcule les coordonn?es absolues des 4 coins et on trace en absolu. 
         Pas tr?s optimis? en temps tout cela, il aurait ?t? plus simple de     
         de tracer directement en relatif !!!                                   
         Le rectangle est trac? comme ABOX avec les param?tres dans HRSx.       
*/
Le819                                                                                
	CLC         ;   C=0                                               
	LDA $46     ;   on place les coordon?es actuelles                 
	STA $06     ;   du curseur dans $06-07                            
	ADC $4D     ;   et les coordonn?es (X+dX,Y+dY)                    
	STA $08                                                          
	LDA $47                                                          
	STA $07                                                          
	ADC $4F                                                          
	STA $09     ;   dans $08-09                                       
	BCC $E83A   ;   inconditionnel    FIXME                                 
                                                                                
                                                	
 /*                                                                              
                         TRACE UN RECTANGLE ABSOLU                          
                                                                                
                                                                                
Principe:Par un proc?d? tr?s astucieux, on va tracer les 4 traits (en absolu)   
         joignant les 4 points. Voila bien la seule astuce inutile ! Il aurait  
         ?t? 100 (pourquoi pas 1000 !?) fois plus simple, puisque le rectangle  
         n'est fait que de verticales et d'horizontales, de tracer le rectangle 
         imm?diatement en relatif plutot que de passer par des calculs de       
         tangentes lourds et donnant un r?sultat connu (0 et infini) !!!        
         Cette pi?tre routine n?cessite les param?tres comme ABOX dans HRSx.    
         Notez ?galement l'utilisation de l'absolu,X plutot que du page 0,X en  
         $E850... tss tss !                                                     
 */
Le82c 
	LDY #$06    ;   on place les 4 param?tres (poids faible seulement)
	LDX #$03                                                         
	LDA $004D,Y  ;  de HRSx                                           
	STA $06,X    ;  dans $06-7-8-9                                    
	DEY                                                              
	DEY                                                              
	DEX                                                              
	BPL $E830               ; FIXME                                          
	LDX #$03     ;  on va tracer 4 traits                             
	STX $05      ;  dans $05 <----------------------------------------
	LDA table_for_rect,X   ; on lit le code coordonn?es                       I 
	STA $04      ;  dans $04                                         I
	LDX #$06     ;  on va extraire 8 bits                            I
	LDA #$00     ;  A=0 <----------------------------------------    I
	STA $4E,X    ;  poids fort HRSx ? 0 et positif              I    I
	LSR $04     ;   on sort 2 bits                              I    I
	ROL         ;   dans A                                      I    I
	LSR $04     ;                                               I    I
	ROL         ;                                               I    I
	TAY         ;   et Y                                        I    I
	LDA $0006,Y  ;  on lit la coordonn?e correspondante         I    I
	STA $4D,X    ;  et on stocke dans HRSx                      I    I
	DEX         ;                                               I    I
	DEX          ;                                              I    I
	BPL $E845   ;   on fait les 4 coordonn?es ADRAW -------------    I FIXME
	JSR $E866   ;   on trace le trait en absolu                      I FIXME
	LDX $05    ;                                                     I
	DEX        ;                                                     I
	BPL $E83C   ;   et on fait 4 traits ------------------------------ FIXME
	RTS                                                              
table_for_rect
Le862
	.byt $26,$67,$73,$32
/*
                         TRACE DE TRAIT EN ABSOLU                          
                                                                                
                                                                                
Action:on calcule dX et dY les d?placements dans HRS1 et HRS2 et on trace en    
       relatif. En entr?e, comme ADRAW dans HRSx.                               
*/
Le866

	LDX $4D     ;   X=colonne                                         
	LDY $4F     ;   Y=ligne du curseur                                
	JSR Le7f3   ;   on place le curseur en X,Y    FIXME                     
	LDX #$FF    ;   on met -1 dans X pour un changement de signe      
	SEC         ;   ?ventuel dans les param?tres                      
	LDA $51     ;   on prend X2                                       
	SBC $4D     ;   -X1                                               
	STA $4D     ;   dans HRS1 (DX)                                    
	BCS $E87B   ;   si DX<0, on inverse le signe de HRS1               FIXME
	STX $4E     ;   DEC $4E aurait ?t? mieux...                       
	SEC                                                              
	LDA $53      ;  on prend Y2                                       
	SBC $4F      ;  -Y1                                               
	STA $4F     ;   dans HRS2 (DY)                                    
	BCS $E885   ;   et si DY n?gatif, on met signe -1                 FIXME
	STX $50     ;   ou DEC $50                                        
                                                  


 /*                                                                              
                         TRACE DE TRAIT EN RELATIF                          
                                                                                
                                                                                
Principe:Le principe du trac? des droites est en fait assez complexe. On aurait 
         aim? que F. BROCHE nous ponde une routine hyper-optimis?e dont il a le 
         secret. Ce n'est malheureusement pas le cas puisque cette routine      
         l'algorithme des ROM V1.0 et 1.1. Sans doute parce qu'il est tr?s      
         efficace...                                                            
         Pour tracer un trait le plus rapidement possible, on cherche lequel des
         deux axes est le plus grand et on trace selon cet axe. Pour tracer,    
         on avance sur l'axe de t points (t est la valeur de la tangente) et on 
         avance d'un point sur l'autre axe, et ainsi de suite jusqu'? ce qu'on  
         ait parcouru tout l'axe.                                               
         Ainsi DRAW 10,2,1 donnera en fait 2 paliers de 5 pixels de large.      
         Le cas dX=dY (d?placements ?gaux) est trait? avec t=-1, de plus les    
         poids fort des d?placements gardent le signe car on prend la valeur    
         absolue de dX et dY pour les calculs.                                  
   */                                                                             
Le885                                                                             
	LDA $02AA  ;    sauve le pattern                                  
	STA $56    ;    dans HRS1+1                                       
	JSR $E942  ;    v?rifie la validit? de dX et dY        FIXME            
	STX $46    ;    X et Y contiennent HRSX+dX et HRSY+dY             
	STY $47     ;   dans HRSX et HRSY                                 
	BIT $4E    ;    dX n?gatif ?                                      
	BPL $E89D  ;    non ---------------------------------------------- FIXME
	LDA $4D    ;    oui, on compl?mente                              I
	EOR #$FF    ;   dX                                               I
	STA $4D    ;                                                     I
	INC $4D    ;    ? 2                                              I
	BIT $50    ;    dY n?gatif ? <------------------------------------
	BPL $E8A9  ;    non ---------------------------------------------- FIXME
	LDA $4F    ;    oui on compl?mente                               I
	EOR #$FF   ;    dY                                               I
	STA $4F    ;                                                     I
	INC $4F    ;    ? 2                                              I
	LDA $4D    ;    on teste dX et dY <-------------------------------
	CMP $4F                                                          
	BCC $E8ED   ;   dX<dY -------------------------------------------- FIXME
	PHP         ;   dX>=dY , on trace selon dX                       I
	LDA $4D     ;   on prends dX                                     I
	BEQ $E8EB  ;    dX=0, on sort -------------------------------    I FIXME
	LDX $4F    ;    X=dY                                        I    I
	JSR $E921  ;    on calcule dY/dX                            I    I FIXME
	PLP        ;                                                I    I
	BNE $E8C0  ;    dX<>dY -----------------------------------  I    I FIXME
	LDA #$FF   ;    dX=dY, la tangente est 1                 I  I    I
	STA RES   ;     en fait, -1, mais c'est la m?me chose    I  I    I
	.byt $24,$4e  ; --->on teste dX <-----------------------------  I    I
	BPL $E8CA ; I   dX>0 -------------------------------------  I    I FIXME
	JSR $E7E7 ; I   dX<0, on d?place le curseur ? gauche     I  I    I FIXME
	JMP $E8CD ; I---                                         I  I    I FIXME 
	JSR $E7D9 ; II  on on d?place le curseur ? droite <-------  I    I FIXME
	CLC       ; I-->a-t-on parcouru une valeur de la tangente   I    I
	LDA RES   ; I                                               I    I
	ADC $02   ; I   on stocke le r?sultat dans $02              I    I
	STA $02   ; I                                               I    I
	BCC $E8E3  ;I   non, on continue -------------------------  I    I FIXME
	BIT $50   ; I   oui, dY<0 ?                              I  I    I
	BMI $E8E0 ; I   oui -------------------------------      I  I    I FIXME
	JSR $E7C1 ; I   non, on d?place le curseur        I      I  I    I FIXME
	JMP $E8E3  ;I---vers le bas                       I      I  I    I FIXME
	JSR $E7CD ; II  on d?place vers le haut <----------      I  I    I FIXME
	JSR $E792  ;I-->on affiche le point <---------------------  I    I FIXME
	DEC $4D   ; I   on d?cremente dX,                           I    I
	BNE $E8C0 ; ----on n'a pas parcouru tout l'axe              I    I FIXME
	RTS       ;  -->sinon, on sort                              I    I
	PLP      ;   I  <--------------------------------------------    I
	RTS       ;  I                                                   I
	LDA $4F   ;  I  on trace la droite selon dY <---------------------
	BEQ $E8EA  ; ---dY=0, on sort                                      FIXME
	LDX $4D   ;     X=dX                                              
	JSR $E921 ;     on calcule dX/dY dans RES                          FIXME
	BIT $50                                                          
	BPL $E900  ;    dY>0 --------------------------------------------- FIXME
	JSR $E7CD  ;    dY<0, on d?place vers le haut                    I FIXME
	JMP $E903  ; ---et on saute                                      I FIXME
	JSR $E7C1  ; I  on d?place vers le bas <-------------------------- FIXME
	CLC       ;  -->a-t-on parcouru la tangente ?                     
	LDA RES                                                          
	ADC $02                                                          
	STA $02     ;   (dans $02)                                        
	BCC $E919   ;   non ---------------------------------------------- FIXME
	BIT $4E     ;                                                    I
	BPL $E916   ;   dX>0 ------------------------------------        I FIXME
	JSR $E7E7   ;   dX<0, on d?place vers                   I        I FIXME
	JMP $E919  ; ---la gauche                               I        I FIXME
	JSR $E7D9  ; I  on d?place vers la droite <--------------        I FIXME
	JSR $E792  ; -->on affiche le point <----------------------------- FIXME
	DEC $4F    ;    et on d?crit dY                                   
	BNE $E8F6                                                       ;  FIXME
	RTS         ;   avant de sortir de longueur des lignes            

	;   CALCUL LA TANGENTE (*256) D'UN TRAIT                    
Le921

	STX $01     ;   dX (ou dY)*256 dans RES+1                         
	LDY #$00    ;   dY (ou dX) dans AY                                
	STY RES                                                          
	JSR $CEDC    ;  calcul dX*256/dY (ou dY/dX)   FIXME                    
	LDA #$FF     ;  reste =-1                                         
	STA $02    ;    resultat dans RES                                 
	RTS   

           ;                    ROUTINE CURSET                               
Le92f                                                                               
	LDX $4D      ;  X=HRSX                                            
	LDY $4F     ;   Y=HRSY                                            
	JSR $E94E    ;  on v?rifie les coordonn?es      FIXME                   
	JSR Le7f3    ;  on place le curseur en X,Y       FIXME                  
	JMP $E79C    ;  et on affiche sans g?rer pattern   	 FIXME
	
     ;                          ROUTINE CURMOV                               
Le93c
	JSR $E942    ;  on v?rifie les param?tres    FIXME                      
	JMP $E936   ;   et on d?place    	 FIXME

/*
                VERIFIE LA VALIDITE DES PARAMETRES RELATIFS                 
                                                                                
Action:V?rifie si l'adressage relatif du curseur est dans les limites de l'?cran
       HIRES, soit si 0<=X+dX<240 et 0<=Y+dY<200.                               
  */
Le942  
	CLC                                                              
	LDA $46     ;   on prend HRSX                                     
	ADC $4D     ;   plus le d?placement horizontal                    
	TAX          ;  dans X                                            
	CLC                                                              
	LDA $47     ;   HRSY                                              
	ADC $4F     ;   plus le d?placement vertical                      
	TAY         ;   dans Y           

/*
                        TESTE SI X ET Y SONT VALIDES                        
                                                                                
                                                                                
Principe:Si X>239 ou Y>199 alors on ne retourne pas au programme appelant, mais 
         ? son appelant, en indiquant l'erreur dans HRSERR.                     
  */                                                                              
le94e
	CPX #$F0     ;  X>=240 ?                                          
	BCS $E957   ;   oui ---------------------------------------------- FIXME
	CPY #$C8    ;   Y>=200 ?                                         I
	BCS $E957    ;  oui ---------------------------------------------O FIXME
	RTS         ;   coordonn?es ok, on sort.                         I
	PLA          ;  on d?pile poids fort (>0) <-----------------------
	STA $02AB    ;  dans HRSERR                                       
	PLA          ;  et poids faible de l'adresse de retour            
	RTS          ;  et on retourne ? l'appelant de l'appelant    



paper_routine
Le95d
	clc
	.byt $24

INK_ROUTINE
Le95f
	sec
/*	
                    FIXE LA COULEUR DE FOND OU DU TEXTE                     
                                                                                
                                                                                
Principe:A contient la couleur, X la fen?tre ou 128 si mode HIRES et C=1 si la  
         couleur est pour l'encre, 0 pour le fond.                              
         Changer la couleur consiste ? remplit la colonne couleur correspondante
         avec le code de couleur. Auncun test de validit? n'?tant fait, on peut 
         utiliser ce moyen pour remplir les colonnes 0 et 1 de n'importe quel   
         attribut.                                                              
*/
Le960  
	PHA           ; on sauve la couleur                               
	PHP           ; et C                                              
	STX RES       ; fen?tre dans RES                                  
	BIT RES       ; HIRES ?                                           
	BMI $E9A7     ; oui ---------------------------------------------- FIXME
	STX $28       ; TEXT, on met le num?ro de fen?tre dans $28       I
	BCC $E971     ; si C=0, c'est PAPER                              I FIXME
	STA $0240,X  ;  on stocke la couleur d'encre                     I
	BCS $E974    ;  si C=1 c'est INK                                 I FIXME
	STA $0244,X  ;  ou la couleur de fond                            I
	LDA $0248,X  ;  est on en 38 colonnes ?                          I
	AND #$10     ;                                                   I
	BNE $E987     ; mode 38 colonnes ------------------------------  I
	LDA #$0C     ;  mode 40 colonnes, on efface l'?cran           I  I
	JSR $DBB5    ;  (on envoie CHR$(12))                          I  I FIXME
	LDA #$1D     ;  et on passe en 38 colonnes                    I  I
	JSR $DBB5    ;  (on envoie CHR$(29))                          I  I FIXME
	LDX $28      ;  on prend X=num?ro de fen?tre                  I  I
	LDA $0230,X  ;  on prend la ligne 0 de la fen?tre <------------  I
	JSR $CE69    ;  *40 dans RES                                     I FIXME
	LDA $0238,X  ;  AY=adresse de base de la fen?tre                 I
	LDY $023C,X  ;                                                   I
	JSR $CE89   ;   on ajoute l'adresse ? RES (ligne 0 *40) dans RES I FIXME
	LDY $0228,X  ;  on prend la premi?re colonne de la fen?tre       I
	DEY         ;   on enl?ve deux colonnes                          I
	DEY         ;                                                    I
	SEC         ;                                                    I
	LDA $0234,X ;   on calcule le nombre de lignes                   I
	SBC $0230,X ;   de la fen?tre                                    I
	TAX         ;   dans X                                           I
	INX         ;                                                    I
	TYA         ;   colonne 0 dans Y                                 I
	BCS $E9B3   ;   inconditionnel --------------------------------- I FIXME
	LDA #$00     ;  <----------------------------------------------+--
	LDX #$A0     ;                                                 I  
	STA RES      ;  RES=$A000 , adresse HIRES                      I  
	STX $01     ;                                                  I  
	LDX #$C8    ;   X=200 pour 200 lignes                          I  
	LDA #$00    ;   A=0 pour colonne de d?but = colonne 0          I  
	PLP         ;   on sort C <-------------------------------------  
	ADC #$00    ;   A=A+C                                             
	TAY        ;    dans Y                                            
	PLA        ;    on sort le code                                   
	STA ($00),Y; -->on le place dans la colonne correspondante        
	PHA        ; I  on le sauve                                       
	CLC        ; I                                                    
	LDA RES    ; I  on passe 28 colonnes                              
	ADC #$28    ;I  (donc une ligne)                                  
	STA RES     ;I                                                    
	BCC $E9C6  ; I         FIXME                                            
	INC $01    ; I                                                    
	PLA        ; I  on sort le code                                   
	DEX        ; I  on compte X lignes                                
	BNE $E9B8   ;---                          FIXME                        
	RTS         ;   et on sort----------------------------------------


   /*                                                                            
                              TRACE UN CERCLE                               
                                                                                
                                                                                
Principe:Pour tracer une ellipsoide en g?n?ral, on utilise la formule :         
                                                                                
         (X*X)/(A*A)+(Y*Y)/(B*B)=1, A et B ?tant respectivement la largeur      
         et la hauteur de l'ellipse. Pour un cercle, A=B donc on ?crit :        
                                                                                
         X*X+Y*Y=R*R soit encore X=SQR(R*R-Y*Y).                                
                                                                                
         Pour tracer le cercle, il suffit de faire varier Y de 0 ? R. On        
         obtient des valeurs positives de X et de Y donc la quart inf?rieur     
         droit du cercle. On trace les 3 autres quarts par sym?tries. Le        
         probl?me d'un tel algorithme c'est qu'il n?cessite le calcul d'une     
         exponentiation (SQR(A)=A^0.5) et une soustraction d?cimale.            
         Son atout est de n'avoir ? calculer qu'un quart des valeurs.           
                                                                                
         Les concepteurs de l'ATMOS (et ? fortiori F. BROCHE) ayant jug? que cet
         algorithme ?tait par trop complexe et laborieux, on pr?f?r? le calcul  
         par suites crois?es dont la formule est :                              
                                                                                
         X0=0 et Xn=X(n-1)+Yn/R   (n et n-1 sont les indices des termes X et Y) 
         Y0=R et Yn=Y(n-1)-Xn/R                                                 
                                                                                
         Etant donn?e la priorit? de calcul, on calcule en fait les termes :    
                                                                                
         Xn = Xn-1 + Yn-1 / R                                                   
         Yn = Yn-1 - Xn   / R ce qui fait d?ja une petite erreur de calcul.     
                                                                                
         De plus, diviser ? chaque fois par R serait long. Les programmeurs,    
         et c'est l? leur g?nie, ont donc pens? ? deux choses fort astucieuses: 
                                                                                
         a) on divisera non pas par R, mais par la puissance de deux            
            imm?diatement sup?rieure ? R afin de se ramener ? des d?calages.    
            on devient ainsi trop pr?cis, ce qui rattrape l'erreur pass?e.      
                                                                                
         b) on va coder Xn et Yn sur deux octets qui seront se et sf,           
            respectivement les parties enti?res et d?cimale de Xn et Yn.        
            on calcule Xn=AB par Xn=A+B/256. Ce qui revient en fait ? consid?rer
            les 8 bits de B (b7b6b5b4b3b2b1b0) comme des bits de puissance      
            n?gatives d?croissantes (b-1b-2b-3b-4b-5b-6b-7b-8). La pr?cision    
            est donc inf?rieure a 2^-9, soit ? 0,002. Ce qui est tr?s suffisant.
                                                                                
         Une fois ces deux conventions pos?es, on peut tracer le cercle tr?s    
         facilement. Son aspect sera de sym?trie diagonales et non verticale/   
         horizontale du fait de la quadrature exerc?e sur les valeurs mais bon. 
         Pour tracer, on calcule un par un les termes des suites et si la valeur
         enti?re d'un des termes au moins change, on affiche le point. Et on    
         continue jusqu'? ce que Xn et Yn soit revenus ? leur position initiale.
                                                                                
Remarque:La routine est bugg?e, en effet si le rayon est 0, la boucle de calcul 
         de la puissance de 2 > au rayon est infinie, idem si le rayon est 128. 
         Il aurait suffit d'incr?menter le rayon avant le calcul...             
 */
 
Le9cb
                                                                               
	LDA $46       ; on sauve HRSX                                     
	PHA                                                              
	LDA $47      ;  et HRSY                                           
	PHA                                                              
	LDA $02AA   ;   et on met le pattern dans $56                     
	STA $56      ;  car le trac? du cercle en tient compte            
	LDA $47      ;  on prend HRSY                                     
	SEC                                                              
	SBC $4D     ;   -rayon                                            
	TAY          ;  dans Y                                            
	LDX $46     ;   on prend HRSX                                     
	JSR Le7f3    ;  et on place le premier point du cercle (X,Y-R)      FIXME  
	LDX #$08    ;   X=7+1 pour calculer N tel que Rayon<2^N.          
	LDA $4D     ;   on prend le rayon                                 
	DEX         ;   on enl?ve une puissance                           
	ASL         ;   on d?cale le rayon ? gauche                       
	BPL $E9E5   ;   jusqu'? ce qu'un bit se pr?sente dans b7         FIXME     
	STX $0C    ;    exposant du rayon dans $0C                        
	LDA #$80    ;   A=$80 soit 0,5 en d?cimal                         
	STA $0E     ;   dans sfX                                          
	STA $10     ;   et sfY                                            
	ASL         ;   A=0                                               
	STA $0F     ;   dans seX                                          
	LDA $4D     ;   A=Rayon                                           
	STA $11     ;   dans seY                                          
	SEC                                                              
	ROR $0D     ;   on met b7 de $0D ? 1 (ne pas afficherle point)    
	LDA $10    ;    AX=sY                                             
	LDX $11                                                          
	JSR Lea62   ;   on calcule sY/R (en fait sY/2^N)                
	CLC                                                              
	LDA $0E     ;   on calcule sX=sX+sY/R                             
	ADC $12                                                          
	STA $0E                                                          
	LDA $0F                                                          
	STA $12                                                          
	ADC $13                                                          
	STA $0F      ;  la partie enti?re seX a boug? ?                   
	CMP $12                                                          
	BEQ Lea22   ;  non ----------------------------------------------    
	BCS Lea1d    ;  elle a augment? ----------------------------     I    
	JSR $E7D9    ;;  elle ? baiss?, on d?place le curseur       I     I  
	JMP Lea20  ; ---? droite                                   I     I  
Lea1d
	JSR $E7E7 ;  I  on d?place le curseur ? gauche <------------     I  FIXME 
Lea20	
	LSR $0D    ; -->on indique qu'il faut afficher le point          I
Lea22
	LDA $0E    ;    AX=sX <-------------------------------------------
	LDX $0F                                                          
	JSR Lea62 ;    on calcule sX/R (en fait sX/2^N)                    
	SEC                                                              
	LDA $10    ;    et sY=sY-sX/R                                     
	SBC $12                                                          
	STA $10                                                          
	LDA $11                                                          
	STA $12                                                          
	SBC $13                                                          
	STA $11    ;    seY ? chang? (faut-il se d?placer verticalement)? 
	CMP $12                                                          
	BEQ Lea4a  ;    non ----------------------------------------------    
	BCS Lea44 ;     on est mont? --------------------------------    I  
	JSR $E7C1 ;     on est descendu, on d?place le curseur      I    I  FIXME  
	JMP $EA4E ;  ---vers le bas et on affiche                   I    I  FIXME  
Lea44
	JSR $E7CD ;  I  on d?place le curseur vers le haut <---------    I  FIXME  
Lea41	
	JMP $EA4E ;  O--et on affiche                                    I  FIXME  
Lea4a	
	BIT $0D   ;  I  faut-il afficher le point ? <---------------------
	BMI Lea51 ;  I  non, on passe  -----------------------------------    
	JSR $E792 ;  -->on affiche le point nouvellement calcul?         I  FIXME  
Lea51
	LDA $0F   ;     seX=0 ? <-----------------------------------------
	BNE $E9F8  ;    non, on boucle                                      FIXME  
	LDA $11    ;    oui, seY=rayon?                                   
	CMP $4D                                                          
	BNE $E9F8  ;    non, on boucle                                FIXME       
	PLA        ;    oui, on a fait le tour                            
	TAY        ;    on reprend les coordonn?es du curseur sauv?es     
	PLA        ;    dans X et Y                                       
	TAX                                                              
	JMP Le7f3    ;  et on replace le curseur            FIXME                  
  
/*
                       CALCUL LE DEPLACEMENT sX ou sY                       
                                                                                
Action:calcule dans $13,$12 la valeur de (X,A)/R, en fait (X,A)/2^N.            
*/

Lea62																				
	STA $12       ; on place la partie fractionnaire dans $12         
	STX $13       ; et la partie enti?re dans $13                     
	LDX $0C       ; X=N tel que Rayon<2^N
Lea68	
	LDA $13       ; on garde le signe du r?sultat                     
	ROL                                                              
	ROR $13       ; et on divise par 2^X                              
	ROR $12       ; dans $13,$12                                      
	DEX                                                              
	BNE Lea68                                                        
	RTS         

FILL_ROUTINE
	lda $4b
	ldy $4c
	sta RES 
	sty $01
Lea7b
	ldx $4f
	ldy $49
	lda $51
Lea81	
	sta (RES),y
	iny
	dex
	bne Lea81 
	lda #$28
	ldy #0
	jsr XADRES_ROUTINE 
	dec $4d
	bne Lea7b 
Lea92	
	rts
	
SCHAR_ROUTINE
	sta $51
	sty $52
	stx $4f
	lda #$40
	sta $57
	ldy #$00
Lea9f
	sty $50
	cpy $4f
	bcs Lea92 
	lda ($51),y
	jsr $eab5 ; FIXME
	ldy $50
	iny
	bne Lea9f 
CHAR_ROUTINE	
	LDA $4D
	ASL
	LSR $4F
	ROR
	PHA
	LDA $46
	CMP #$EA
	BCC Lead3
	LDX $4A
	LDA $47
	ADC #$07
	TAY
	SBC #$BF
	BCC Lead0 	
	BEQ Lead0 	
	CMP #$08
	BNE Leacf 
	LDA #$00
Leacf	
	TAY
Lead0	
	JSR Le7f3
Lead3	
	PLA
	JSR Lff31 
	LDY #$00
Lead9	
	STY RES
	LDA $49
	PHA
	LDA $4A
	PHA
	LDA ($02),Y
	ASL
Leae4	
	ASL
	BEQ Leaf3 
	PHA
	BPL Leaed 
	JSR $E79C ; FIXME
Leaed	
	JSR $E7D9 ; FIXME
	PLA
	BNE Leae4 
Leaf3	
	JSR Le7c1 
	PLA
	STA $4A
	PLA
	STA $49
	LDY RES
	INY
	CPY #$08
	BNE Lead9
	LDA $46
	ADC #$05
	TAX
	LDY $47
	JMP Le7f3 




	

	
#include "functions/sound/sounds.asm"
	
READ_A_SERIAL_BUFFER_CODE_INPUT
Lec10
	ldx #$0c
	jmp $db5d ; FIXME
wait_code_on_SERIAL_BUFFER_INPUT
.(
loop
	jsr READ_A_SERIAL_BUFFER_CODE_INPUT 
	bcs loop
	rts
.)	
write_caracter_in_output_serial_buffer
	bit write_caracter_in_output_serial_buffer
	jmp $db79

Minitel	
send_a_to_minitel output
.(
Lec21
	bit $5b
	bvs Lec49 
	tax
	bmi next910
	cmp #$20
	bcs Lec49 
	adc #$20
next912	
	pha
	lda #$02
	jsr Lec49 
	pla 
	jmp Lec49 
next910
	cmp #$A0
	
	bcs next911 
	adc #$c0
	
	bcs next912 
next911	
	and #$7f
	pha 
	lda #1
	jsr Lec49
	pla 
Lec49	
	bit Lec49
	jmp Ldb12 
.)	
send_A_to_serial_output_with_check
Lec4f
; MINITEL
	stx $0C
	sty $0d
	pha
	bit $5b
	bpl Lec5e ; FIXME
	jsr send_a_to_minitel
	jmp $ec61
Lec5e	
	jsr $ec1b
	pla
	eor $0e
	sta $0e
	ldx $0c
	ldy $0d
	rts
Lec6b
	STX $0C
	STY $0D
Lec6f	
	ASL $027E
	BCC Lec77
	PLA
	PLA
	RTS
Lec77	
	BIT $5B
	BMI Lec8b
	JSR $EC10 ; FIXME
	BCS Lec6f
	PHA
	EOR $0E
	STA $0E
	PLA
	LDX $0C
	LDY $0D
	RTS
	
Lec8b
	JSR $ECB4 ; FIXME
	BCS $EC6F ; FIXME
	BIT $5B
	BVS $EC80 ; FIXME
	CMP #$20
	BCS $EC80 ; FIXME
	PHA
	JSR $ECB9 ; FIXME
	TAX
	PLA
	TAY
	TXA
	CPY #$01
	BNE $ECA8 ; FIXME
	ORA #$80
	BMI $EC80 ; FIXME
	CMP #$40
	BCS $ECB0 ; FIXME
	SBC #$1F
	BCS $EC80 ; FIXME
	ADC #$3F
	BCC $EC80 ; FIXME
	LDX #$0C
	JMP $C518 ; FIXME
	JSR $ECB4 ; FIXME
	BCS $ECB9 ; FIXME
	RTS
Lecbf
	sec
	.byt $24
	clc
	lda #$80
	jmp $db5d ; FIXME
	
	sec 
	.byt $24
	clc
	lda #$80
	jmp $db79  ; FIXME
	
	sec
	.byt $24
	clc
	lda #$80
	jmp $daf7   ; FIXME
	
	
	sec
	.byt $24
	clc 
	lda #$80
	jmp $db12 ; FIXME

compute_file_size
Lecdf
	SEC
	LDA $052F
	SBC $052D
	STA $052A
	LDA $0530
	SBC $052E
	STA $052B
	LDA $052D
	LDY $052E
	STA RES
	STY $01
	rts

send_serial_header_file
Lecfd
	LDX #$32
	LDA #$16
	JSR $EC4F
	DEX
	BNE $ECFF
	LDA #$24
	JSR $EC4F
	LDA #$00
	STA $0E
	LDX #$00
	LDA $0518,X
	JSR $EC4F
	INX
	CPX #$0C
	BNE $ED12
	LDA #$00
	JSR $EC4F
	
	LDX #$00
	LDA $052C,X
	JSR $EC4F
	
	
	
	INX
	CPX #$07
	BNE $ED24
	LDA $0E
	JMP $EC4F

read_header_file
Led34	
	
	JSR $EC6B
	CMP #$16
	BNE $ED34
	LDX #$0A
	JSR $EC6B
	CMP #$16
	BNE $ED34
	DEX
	BNE $ED3D
	JSR $EC6B
	CMP #$16
	BEQ $ED47
	CMP #$24
	BNE $ED34
	LDA #$00
	STA $0E
	JSR $EC6B
	TAX
	BEQ $ED62
	JSR $DBB5
	JMP $ED56
	LDX #$00
	
	
	
	JSR $EC6B
	STA $052C,X
	INX
	CPX #$07
	BNE $ED64
	JSR $EC6B
	ORA #$30
	JMP $DBB5
	
Led77	
	JSR $ECC1
	JSR $ECC9
	JSR $EC10
	BCS $ED85
	JSR $DBB5
	JSR $C7CF
	BCS $ED7D
	CMP #$03
	BEQ $ED94
	JSR $EC1B
	JMP $ED7D
	JSR $ECBF
	JMP $ECC7
	


Led9a
sdump_routine
	JSR $ECC1 ; FIXME
	ASL $027E
	BCS $EDC7 ; FIXME
	JSR $EC10 ; FIXME
	BCS $ED9D ; FIXME
	TAX
	BMI $EDAE ; FIXME
	CMP #$20
	BCS $EDC1 ; FIXME
	PHA
	LDA #$81
	JSR $DBB5 ; FIXME
	PLA
	JSR $CE54 ; FIXME
	JSR $DBB5 ; FIXME
	TYA 
	JSR $DBB5 ; FIXME
	LDA #$87 
	JSR $DBB5 ; FIXME
	JMP $ED9D ; FIXME
	JMP $ECBF ; FIXME

SSAVE_ROUTINE
Ledca
	ror $5b
	lsr $5b
	jsr $ecc9 ; FIXME
	jsr Lee0a ; FIXME
	jmp $ecc7 ; FIXME

MSAVE_ROUTINE
Ledd7
	ror $5b
	sec
	ror $5b
	jsr $ecd9 ; FIXME
	jsr Lee0a ; FIXME
	jmp $ecd7 ; FIXME

SLOAD_ROUTINE 
Lede5
	ROR $5B
	LSR $5B
	LDA #$40
	STA $030E
	JSR $ECC1 ; FIXME
	JSR $EE56 ; FIXME
	LDA #$C0
	STA $030E
	JMP $ECBF ; FIXME
	
	
MLOAD_ROUTINE	
	ROR $5B
	SEC
	ROR $5B
	jsr $ecd1 ; FIXME
	jsr $ee56 ; FIXME
	jmp $eccf ; FIXME

;;;;;;;;;;;;;;;;;;	
save_file_rs232_minitel
Lee0a
	BIT $5B
	BVS $EE11 ; FIXME
	JSR $ECFD ; FIXME
	JSR $ECDF ; FIXME
	LDA #$00
	STA $0E
	LDA $052A
	BEQ $EE2F ; FIXME
	LDY #$00
	LDA ($00),Y
	JSR $EC4F ; FIXME
	DEC $052A
	INC RES
	BNE $EE18 ; FIXME
	INC $01
	BNE $EE18 ; FIXME
	LDA $052B
	BEQ $EE51 ; FIXME
	LDY #$00
	LDA ($00),Y
	JSR $EC4F ; FIXME
	INY
	BNE $EE36 ; FIXME
	DEC $052B
	INC $01
	BIT $5B
	BPL $EE2F ; FIXME
	LDA #$30 
	STA $44
	LDA $44
	BNE $EE4B ; FIXME
	BEQ $EE2F ; FIXME
	LDA $0E
	JMP $EC4F ; FIXME
read_a_file_rs232_minitel
Lee56
	BIT $5B
	BVS $EE5D  ; FIXME
	JSR $ED34  ; FIXME
	JSR $ECDF  ; FIXME
	BIT $5B
	BVC $EE6C  ; FIXME
	LDA #$FF
	STA $052A
	STA $052B
	LDY #$00
	STY $0E
	LDA $052A
	BEQ $EE86  ; FIXME
	JSR $EC6B  ; FIXME
	STA ($00),Y
	DEC $052A
	INC RES
	BNE $EE70  ; FIXME
	INC $01
	JMP $EE70  ; FIXME
	LDA $052B
	BEQ $EE9D  ; FIXME
	LDY #$00
	JSR $EC6B  ; FIXME
	STA ($00),Y
	INY
	BNE $EE8D  ; FIXME
	INC $01
	DEC $052B
	JMP $EE86  ; FIXME
	JSR $EC6B  ; FIXME
	ORA #$30
	JMP $DBB5  ; FIXME

RING_ROUTINE
Leea5
; minitel (wait a ring on phone line)	
	lda #0
	sta $028c
	lda #$10
	bit $032d
	bne $eee3
	sec
	rts
; minitel detect ring on line
Leeb3
	LDA #$FF
	STA $0328
	STA $0329
	LDA $0329
	CMP #$C5
	BCS $EEBB ; FIXME
	BIT $0320
	LDA #$20
	AND $032D 
	BNE $EEDF ; FIXME
	LDA #$10
	AND $032D
	BEQ $EEC5 ; FIXME
	LDA $0329
	CMP #$AD
	BCC $EEE1 ; FIXME
	CMP #$B5
	LDA #$01
	RTS
	lda #0
	sec
	rts
Leee3
; minitel
; detect_ring (next routine)
	sei
	ldx #4
	jsr $eeb3 ; FIXME
	dex

	BNE $EEE6 ; FIXME
	JSR $EEB3 ; FIXME
	BEQ $EEFB ; FIXME
	BCS $EEEC ; FIXME
	INX
	JMP $EEEC ; FIXME
	CLI
	JMP $EEAA ; FIXME
	CPX #$06 ; FIXME
	BCC $EEF7 ; FIXME
	JSR $EEB3 ; FIXME
	BCS $EEFF ; FIXME
	LDY #$1E
	LDX #$00
	JSR $EEB3 ; FIXME
	BCC $EF0E ; FIXME
	INX
	DEY
	BNE $EF08 ; FIXME
	CPX #$0F
	BCS $EEF7 ; FIXME
	CLI
	LDA #$0A
	STA $44
	LDA $44
	BNE $EF1A ; FIXME
	CLC
	rts

Lef20
; minitel ; get the line
	jsr $ecd9 ; FIXME
	lda #$6f
	jsr send_pro1_sequence_to_minitel
	lda #$68
	jsr send_pro1_sequence_to_minitel
	jmp $ecd7 ; FIXME
	
; minitel
; send pro1 sequence to minitel
send_pro1_sequence_to_minitel
Lef30
	pha
	lda #$1b
	jsr $ec49 ; FIXME
	lda #$39
	jsr $ec49 ; FIXME
	pla 
	jmp $ec49 ; FIXME

Lef3f
free_the_minitel_line
	jsr $ecd9 ; FIXME
	lda #$67
	jsr send_pro1_sequence_to_minitel
	jmp $ecd7 ; FIXME
	

Lef4a
#ifdef HAVE_MINITEL	
WCXFIN

; Wait CONNEXION/FIN in 25 seconds (minitel function)
	; MINITEL 
	jsr $ecd1 ; FIXME
	lda #$fa
	sta $44
loop600	
	lda $44
	cmp #$f0
	bne loop600
	ldx #$0c
	jsr $c50c ; FIXME
loop601	
	lda $44
	bne  next600
	jsr $eccf ; FIXME
	
	sec
	rts
next600	
	jsr $ecb4 ; FIXME
	bcs  loop601
	cmp #$13
	bne loop601
	jsr $ecb9 ; FIXME 
	cmp #$53
	bne loop601
	jsr $eccf ; FIXME
	clc
	rts

MOUT
; minitel 
	pha
	jsr $ecd9 ; FIXME
	pla
	jsr $ec49 ; FIXME
	jmp $ecd7 ;FIXME
#endif


#ifdef HAVE_USBDRIVE
.dsb 59,0
#endif 

SOUT
; RS232 
	pha 
	jsr $ecc9 ; FIXME
	pla
	jsr $ec1b ; FIXME
	jmp $ecc7 ; FIXME
add_0_5_A_ACC1
	lda #$e4 ; FIXME
	ldy #$f5 ; FIXME
	jmp $efaf ; AY+acc1 ; FIXME
Lef97 	
	rts ; Don't know why there is this RTS !

ACC2_ACC1	
	.byt $20,$ec,$f1 ; ??? FIXME
	lda $65
	eor #$ff
	sta $65
	eor $6d
	sta $6e
	lda $60
	jmp $efb2
Lefaa
mantisse_A
	jsr $f0e5 ; FIXME
	bcc $efee ; FIXME

AY_add_acc1
		jsr $f1ec ; FIXME
Lefb2	
ACC2_ADD_ACC1	
	bne  next700
	jmp $f377 ; FIXME
next700	
	tsx
	stx $89
	ldx $66
	stx $7f
	ldx #$68
	lda $68
	tay 
	beq Lef97 
	sec
	sbc $60
	beq next802	
	bcc next801 
	sty $60
	ldy $6d
	sty $65
	eor #$ff
	adc #0
	
	ldy #00
	sty $7f
	ldx #$60
	bne next800
next801	
	ldy #0
	sty $66
next800	
	cmp #$f9
	bmi mantisse_A
	tay
	lda $66
	lsr $01,x
	
	jsr $f0fc ; FIXME
next802	
	bit $6e
	bpl Lf049 
	ldy #$60
	cpx #$68
	beq $effa ; FIXME
	ldy #$68
	sec
	eor #$ff
	adc $7f
	sta $66
	lda $0004,y
	sbc $04,x
	sta $64
	lda $0003,y
	sbc $03,x
	sta $63
	lda $0002,y
	sbc $02,x
	sta $62
	lda $0001,y
	sbc $01,x
	sta $61

	bcs Lf022 ; FIXME
	jsr $f090 ; FIXME
Lf022
	ldy #00
	tya
	clc
	ldx $61
	
	bne $f074 ; FIXME
	ldx $62
	stx $61 
	
	ldx $63
	stx $62

	ldx $64
	
	stx $63

	ldx $66
	stx $64
	sty $66

	adc #$08
	cmp #$28
	bne $f026 ; FIXME
Lf042
	lda #0
	sta $60

	sta $65	
	rts
Lf049

	ADC $7F
	STA $66
	LDA $64
	ADC $6C
	STA $64
	LDA $63
	ADC $6B
	STA $63
	LDA $62
	ADC $6A
	STA $62
	LDA $61
	ADC $69
	STA $61
	JMP Lf081
Ld068
	ADC #$01
	ASL $66
	ROL $64
	ROL $63
	ROL $62
	ROL $61
	BPL Ld068
	
	SEC
	SBC $60
	BCS Lf042 
	EOR #$FF
	ADC #$01
	STA $60
Lf081
	BCC Lf08f
	INC $60
	BEQ $F0C7 ; FIXME
	ROR $61
	ROR $62
	ROR $63
	ROR $64
Lf08f
	RTS
Lf090	

       lda     $65
        eor     #$FF
        sta     $65
LF096:  lda     $61
        eor     #$FF
        sta     $61
        lda     $62
        eor     #$FF
        sta     $62
        lda     $63
        eor     #$FF
        sta     $63
        lda     $64
        eor     #$FF
        sta     $64
        lda     $66
        eor     #$FF
        sta     $66
        inc     $66
        bne     LF0C6
LF0B8:  inc     $64
        bne     LF0C6
        inc     $63
        bne     LF0C6
        inc     $62
        bne     LF0C6
        inc     $61
LF0C6:  rts



LF0C7:  lda     #$01
LF0C9:  sta     $8B
        ldx     $89
        txs
        rts

	
justify__to_the_right_with_A_and_X
LF0CF:  ldx     #$6E
LF0D1:  ldy     $04,x
        sty     $66
        ldy     $03,x
        sty     $04,x
        ldy     $02,x
        sty     $03,x
        ldy     $01,x
        sty     $02,x
        ldy     $67
        sty     $01,x
LF0E5:  adc     #$08
        bmi     LF0D1
        beq     LF0D1
        sbc     #$08
        tay
        lda     $66
        bcs     LF106
LF0F2:  asl     $01,x
        bcc     LF0F8
        inc     $01,x
LF0F8:  ror     $01,x
        ror     $01,x
LF0FC:  ror     $02,x
        ror     $03,x
        ror     $04,x
        ror   
        iny
        bne     LF0F2
LF106:  clc
        rts	
const_ln_10
	.byt $82,$13,$5d,$8d,$de ; 2.302585093 = ln(10)
const_pi_radians
	.byt $82,$49,$0f,$da,$9e ; PI in radians (3.14159265)
const_pi_degree	
	.byt $88,$34,$00,$00,$00

polynome_ln_coef	
	
	.byt $03,$7f,$5e,$56,$cb,$79,$80,$13,$9b
	.byt $0b,$64,$80,$76,$38,$93,$16
	
	.byt $82,$38,$aa,$3b,$20 ; 2.885390073 = 2/ln(2)

	
const_for_ln	
	.byt $80,$35,$04,$f3
	.byt $34,$81,$35,$04,$f3,$34,$80,$80,$00,$00,$00,$80,$31,$72,$17,$f8
LF140
	rts
LF141	
	lda #2
	jmp $f0c9

LN_ROUTINE
Lf146

        tsx
        stx     $89
LF149:  jsr     $F3BD  ; FIXME
        beq     LF141
        bmi     LF141
        lda     $60
        sbc     #$7F
        pha
        lda     #$80
        sta     $60
        lda     #$2C
        ldy     #$F1
        jsr     $EFAF  ; FIXME
        lda     #$31
        ldy     #$F1
        jsr     $F287  ; FIXME
        lda     #$A5
        ldy     #$F8
        jsr     $EF98  ; FIXME
        lda     #$17
        ldy     #$F1
        jsr     $F6E1  ; FIXME
        lda     #$36
        ldy     #$F1
        jsr     $EFAF  ; FIXME
        pla
        jsr     $F9E9  ; FIXME
        lda     #$3B
        ldy     #$F1
LF184:  jsr     $F1EC
        beq     LF140
        bne     LF190
        beq     LF140
        tsx
        stx     $89
LF190:  jsr     $F217
        lda     #$00
        sta     $6F
        sta     $70
        sta     $71
        sta     $72
        lda     $66
        jsr     LF1B9  
        lda     $64
        jsr     LF1B9  
        lda     $63
        jsr     LF1B9 
        lda     $62
        jsr     LF1B9 
        lda     $61
        jsr     $F1BE  ; FIXME
        jmp     $F301  ; FIXME

LF1B9:  bne     LF1BE
        jmp     $F0CF  ; FIXME

LF1BE:  lsr
        ora     #$80
LF1C1:  tay
        bcc     LF1DD
        clc
        lda     $72
        adc     $6C
        sta     $72
        lda     $71
        adc     $6B
        sta     $71
        lda     $70
        adc     $6A
        sta     $70
        lda     $6F
        adc     $69
        sta     $6F
LF1DD:  ror     $6F
        ror     $70
        ror     $71
        ror     $72
        ror     $66
        tya
        lsr
        bne     LF1C1
        rts
	
;ay -> acc2

LF1EC:
		sta     $7D
        sty     $7E
        ldy     #$04
        lda     ($7D),y
        sta     $6C
        dey
        lda     ($7D),y
        sta     $6B
        dey
        lda     ($7D),y
        sta     $6A
        dey
        lda     ($7D),y
        sta     $6D
        eor     $65
        sta     $6E
        lda     $6D
        ora     #$80
        sta     $69
        dey
        lda     ($7D),y
        sta     $68
        lda     $60
        rts
	

LF217:  lda     $68
LF219:  beq     LF237
        clc
        adc     $60
        bcc     LF224 
        bmi     Lf23c
        clc
        .byte   $2C
LF224:  bpl     LF237
        adc     #$80
        sta     $60
        beq     Lf23f
        lda     $6E
        sta     $65
        rts

LF231:  lda     $65
        eor     #$FF
        bmi    Lf23c
LF237:  pla
        pla
        jmp     $F042 ; FIXME
Lf23c		
		jmp $f0c7 ; FIXME
Lf23f		
		jmp $f046 ; FIXME
; 10*acc1->acc1	
Lf242
	JSR $F387 ; FIXME
	TAX
	BEQ Lf258
	CLC
	ADC #$02
	BCS Lf23c
	LDX #$00
	STX $6E
	JSR $EFC2 ; FIXME
	INC $60
	beq Lf23c
Lf258
	rts
	
Lf259
ten_in_floating_point
	.byt $84,$20,$00,$00,$00 ; Ten in floating point
Lf25e
acc1_1_divide_10_in_acc1
	jsr $f387 ; FIXME
	ldx #0
	lda #<ten_in_floating_point
	ldy #>ten_in_floating_point
	stx $6e
	jsr $f323 ; FIXME
	jmp $f28a ; FIXME
	
	
LOG_ROUTINE
Lf26f
	tsx
	stx $89
	jsr $f149  ; FIXME
	jsr $f387  ; FIXME
	lda #<const_ln_10
	ldy #>const_ln_10
	jsr $f323  ; FIXME
	jmp $f28a  ; FIXME

display_divide_per_0
Lf282
	lda #3
	sta $8b ; FLERR
	rts
Lf287
	JSR LF1EC  
	BEQ $F282 ; FIXME
	TSX
	STX $89
	JSR $F396 ; FIXME 
	LDA #$00
	SEC
	SBC $60
	STA $60
	JSR $F217 ; FIXME
	INC $60
	BEQ $F23C ; FIXME
	LDX #$FC
	LDA #$01
	LDY $69
	CPY $61
	BNE $F2BA ; FIXME
	LDY $6A
	CPY $62
	BNE $F2BA ; FIXME
	LDY $6B
	CPY $63
	BNE $F2BA ; FIXME
	LDY $6C
	CPY $64
	PHP
	ROL
	BCC $F2CA ; FIXME
	INX
	STA $72,X
	BEQ $F2C8 ; FIXME
	BPL $F2F8 ; FIXME
	LDA #$01
Lf2c7	
	.byt $2c
	lda #$40
	;
	PLP
	BCS $F2DB ; FIXME
	ASL $6C
	ROL $6B
	ROL $6A
	ROL $69
	BCS $F2BA ; FIXME
	BMI $F2A4 ; FIXME
	BPL $F2BA ; FIXME
	TAY
	LDA $6C
	SBC $64
	STA $6C
	LDA $6B
	SBC $63
	STA $6B
	LDA $6A
	SBC $62
	STA $6A
	LDA $69
	SBC $61
	STA $69
	TYA 
	JMP $F2CD ; FIXME
	
	asl
	asl
	asl
	asl
	asl
	asl
	sta $66
	plp

; acc3->acc1	
Lf301
	
	lda $6f
	sta $61
	lda $70
	sta $62
	lda $71
	sta $63
	lda $72
	sta $64
	jmp $f022 ; FIXME
	
Lf314
; pi->acc1
	jsr $f8c7 ; FIXME
	beq Lf31f ; is it in radian mode ?
	lda #<const_pi_degree
	ldy #>const_pi_degree
	bne Lf323
Lf31f	
	lda #<const_pi_radians ; radian
	ldy #>const_pi_radians
Lf323

	STA $7D
	STY $7E
	LDY #$04
	LDA ($7D),Y
	STA $64
	DEY
	LDA ($7D),Y
	STA $63
	DEY
	LDA ($7D),Y
	STA $62
	DEY
	LDA ($7D),Y
	STA $65
	ORA #$80
	STA $61
	DEY
	LDA ($7D),Y
	STA $60
	STY $66
	RTS
LF348
	ldx #$73
	.byt $2c
LF34B
	ldx #$78
	ldy #$00

	JSR $F396 ; FIXME
	STX $7D
	STY $7E
	LDY #$04
	LDA $64
	STA ($7D),Y
	DEY
	LDA $63
	STA ($7D),Y
	DEY
	LDA $62
	STA ($7D),Y
	DEY
	LDA $65
	ORA #$7F
	AND $61
	STA ($7D),Y
	DEY
	LDA $60
	STA ($7D),Y
	STY $66
	RTS
LF377
	lda $6d
	sta $65
	ldx #$05
LF37D	
	lda $67,x
	sta $5f,x
	dex
	bne LF37D
	stx $66
	rts

LF387
	; arrondi ACC1 in ACC2_ACC1
	jsr $f396
	
LF38A:  ldx     #$06
LF38C:  lda     $5F,x
        sta     $67,x
        dex
        bne     LF38C
        stx     $66
LF395:  rts

LF396:
	lda     $60
    beq     LF395
    asl     $66
    bcc     LF395
    jsr     $F0B8 
    bne     LF395
    jmp     $F083 ; FIXME

    lda     $65
    bmi     $F3B8 ; FIXME
    lda     $60
    cmp     #$91
    bcs     $F3B8 ; FIXME
    jsr     $F439 ; FIXME
    lda     $64
    ldy     $63
    rts	

LF3B8	
	lda #$0a
	jmp $f0c9	

LF3BD:  lda     $60
        beq     LF3CA
LF3C1:  lda     $65
LF3C3:  rol   
        lda     #$FF
        bcs     LF3CA
        lda     #$01
LF3CA:  rts

	jsr $f3bd
	.byt $2c

LF3CD
;	ACC=-
	lda #$ff
LF3D1	
	STA $61
	LDA #$00
	STA $62
	LDX #$88
	LDA $61
	EOR #$FF
	ROL
LF3DE	
	LDA #$00
	STA $63
	STA $64
	STX $60
	STA $66
	STA $65
	JMP $F01D ; FIXME
	
	sta $61
	sty $62
	ldx #$90
	sec
	bcs LF3DE
ABS_ROUTINE	
LF3F6	
	lsr $65
	rts

LF3F9
	STA $7D
	STY $7E
	LDY #$00
	LDA ($7D),Y
	INY
	TAX 
	BEQ $F3BD  ; FIXME
	LDA ($7D),Y
	EOR $65
	BMI $F3C1  ; FIXME
	CPX $60
	BNE $F430  ; FIXME
	LDA ($7D),Y
	ORA #$80
	CMP $61
	BNE $F430  ; FIXME
	INY
	LDA ($7D),Y
	CMP $62
	BNE $F430  ; FIXME
	INY
	LDA ($7D),Y
	CMP $63
	BNE $F430  ; FIXME
	INY
	LDA #$7F
	CMP $66
	LDA ($7D),Y
	SBC $64
	BEQ $F3F8  ; FIXME
	LDA $65
	BCC $F436  ; FIXME
	EOR #$FF
	JMP $F3C3  ; FIXME


LF439
	LDA $60
	BEQ $F487 ; FIXME
	SEC
	SBC #$A0
	BIT $65
	BPL $F44D ; FIXME
	TAX
	LDA #$FF
	STA $67
	JSR $F096 ; FIXME
	TXA
	LDX #$60
	CMP #$F9
	BPL LF459 
	JSR $F0E5 ; FIXME
	STY $67
	RTS

LF459	
	TAY
	LDA $65
	AND #$80
	LSR $61
	ORA $61
	STA $61
	JSR $F0FC ; FIXME
	STY $67
	RTS
	
INT_ROUTINE	

LF46A
	LDA $60
	CMP #$A0
	BCS $F469
	JSR $F439
	STY $66
	LDA $65
	STY $65
	EOR #$80
	ROL
	LDA #$A0
	STA $60
	LDA $64
	STA $88
	JMP $F01D ; FIXME
	STA $61
	STA $62
	STA $63
	STA $64
	TAY
	RTS
LF491
	sta $61
	stx $62
	ldx #$90
	sec
	jmp $f3de ; FIXME
LF49B
	jsr $f4a5 ; FIXME
	lda #0
	ldy #1
	jmp $c7a8 ; FIXME
	
LF4A5
	LDY #$00
	LDA #$20
	BIT $65
	BPL $F4AF ; FIXME
	LDA #$2D
	STA $0100,Y
	STA $65
	STY $77
	INY
	LDA #$30
	LDX $60
	BNE $F4C0 ; FIXME
	JMP $F5C8 ; FIXME
	LDA #$00
	CPX #$80
	BEQ $F4C8 ; FIXME
	BCS $F4D1 ; FIXME
	LDA #$D5 ; FIXME
	LDY #$F5 ; FIXME 
	JSR $F184 ; FIXME
	LDA #$F7
	STA $74
	LDA #$DA ; FIXME
	LDY #$F5 ; FIXME
	JSR $F3F9 ; FIXME
	BEQ $F4FA ; FIXME
	BPL $F4F0 ; FIXME
	LDA #$DF ; FIXME
	LDY #$F5 ; FIXME
	JSR $F3F9 ; FIXME
	BEQ $F4E9 ; FIXME
	BPL $F4F7 ; FIXME
	JSR $F242 ; FIXME
	DEC $74 
	BNE $F4DE ; FIXME
	JSR $F25E ; FIXME
	INC $74
	BNE $F4D3 ; FIXME
	JSR $EF90 ; FIXME
	JSR $F439 ; FIXME
	LDX #$01
	LDA $74
	CLC
	ADC #$0A
	BMI $F50F ; FIXME
	CMP #$0B
	BCS $F510 ; FIXME
	ADC #$FF
	TAX
	LDA #$02
	SEC
	SBC #$02
	STA $75
	STX $74
	TXA
	BEQ $F51B ; FIXME
	BPL LF52E 
	LDY $77
	LDA #$2E
	INY
	STA $0100,Y
	TXA
	BEQ $F52C ; FIXME
	LDA #$30
	INY
	STA $0100,Y
	STY $77
LF52E

	LDY #$00
	LDX #$80
	CLC
	LDA $64
	ADC $F5EC,Y ; FIXME
	STA $64
	LDA $63
	ADC $F5EB,Y ; FIXME
	STA $63
	LDA $62
	ADC $F5EA,Y ; FIXME
	STA $62
	LDA $61
	ADC LF5E9,Y 
	STA $61
	INX
	BCS $F556 ; FIXME
	BPL $F533 ; FIXME
	BMI $F558 ; FIXME
	BMI $F532 ; FIXME
	TXA 
	BCC $F55F ; FIXME
	EOR #$FF
	ADC #$0A
	ADC #$2F
	INY
	INY
	INY
	INY
	STY $76
	LDY $77
	INY
	TAX
	AND #$7F
	STA $0100,Y
	DEC $74
	BNE $F57A ; FIXME
	LDA #$2E
	INY
	STA $0100,Y
	STY $77
	LDY $76
	TXA
	EOR #$FF
	AND #$80
	TAX
	CPY #$24
	BNE $F533 ; FIXME
	LDY $77
	LDA $0100,Y
	DEY
	CMP #$30
	BEQ $F58A ; FIXME
	CMP #$2E
	BEQ $F597 ; FIXME
	INY
	LDA #$2B
	LDX $75
	BEQ $F5CB ; FIXME
	BPL $F5A7 ; FIXME
	LDA #$00
	SEC
	SBC $75
	TAX
	LDA #$2D
	STA $0102,Y
	LDA #$45
	STA $0101,Y
	TXA
	LDX #$2F
	SEC
	INX
	SBC #$0A
	BCS $F5B3 ; FIXME
	ADC #$3A
	STA $0104,Y
	TXA
	STA $0103,Y
	LDA #$00
	STA $0105,Y
	BEQ $F5D0 ; FIXME
	STA $0100,Y
	LDA #$00
	STA $0101,Y
	LDA #$00
	LDY #$01
	RTS


LF5d5	
const_for_decimal_convert	
	.byt $9e,$6e,$6b,$28,$00 ; 1 000 000 000  float
LF5DA	
	.byt $9e,$6e,$6b,$27,$fd ; 999 999 999

	.byt $9b,$3e,$bc,$1f,$fd ; 999 999.9
const_zero_dot_half	
LF5E4	
	.byt $80,$00,$00,$00,$00 ; 0.5 
LF5E9	
	.byt $fa,$0a,$1f,$00
const_ten_million
LF5ED
	.byt $00,$98,$96,$80 ; 10 000 000 
	.byt $ff,$f0,$bd,$c0 ; -1 000 000


	.byt $00,$01,$86,$a0,$ff,$ff,$d8,$f0,$00,$00,$03
	.byt $e8,$ff,$ff,$ff,$9c,$00,$00,$00,$0a
LF609	
	.byt $ff,$ff,$ff,$ff
LF60D
	jmp $f042 ; FIXME
SQR_ROUTINE
LF610
	jsr $f387 ; FIXME
	lda #<const_zero_dot_half	
	ldy #>const_zero_dot_half	
	jsr $f323  ; FIXME

LF61A
	BEQ $F68C  ; FIXME
	TSX
	STX $89
	LDA $68
	BEQ $F60D  ; FIXME
	LDX #$80
	LDY #$00
	JSR $F352  ; FIXME
	LDA $6D
	BPL $F63D  ; FIXME
	JSR $F46A  ; FIXME
	LDA #$80
	LDY #$00
	JSR $F3F9  ; FIXME
	BNE $F63D  ; FIXME
	TYA
	LDY $88
	JSR $F379  ; FIXME
	TYA
	PHA
	JSR $F149  ; FIXME
	LDA #$80
	LDY #$00
	JSR $F184  ; FIXME
	JSR $F68F  ; FIXME
	PLA
	LSR
	BCC $F65D  ; FIXME

LF653
	; negative nimber
	lda $60
	beq LF65D 
	lda $65
	eor #$ff
	sta $65
LF65D	
	rts
const_1_divide_ln_2 ; 1/ln(2)	
	.byt $81,$38,$aa,$3b,$29
coef_polynome
LF663
	.byt $07 ; for 8 coef
	.byt $71,$34,$58,$3e,$56
	.byt $74,$16,$7e,$b3,$1b
	.byt $77,$2f,$ee,$e3,$85
	.byt $7a,$1d,$84,$1c,$2a
	.byt $7c,$63,$59,$58,$0a
	.byt $7e,$75,$fd,$e7,$c6
	.byt $80,$31,$72,$18,$10
	.byt $81,$00,$00,$00,$00 ; 1
EXP_ROUTINE
LF68C
	TSX
	STX $89
	LDA #$5E ; FIXME
	LDY #$F6 ; FIXME
	JSR $F184 ; FIXME
	LDA $66
	ADC #$50
	BCC $F69F ; FIXME
	JSR $F396 ; FIXME
	STA $7F
	JSR $F38A ; FIXME 
	LDA $60
	CMP #$88
	BCC $F6AD ; FIXME
	JSR $F231 ; FIXME
	JSR $F46A ; FIXME
	LDA $88
	CLC
	ADC #$81
	BEQ $F6AA ; FIXME
	SEC
	SBC #$01
	PHA
	LDX #$05
	LDA $68,X
	LDY $60,X
	STA $60,X
	STY $68,X
	DEX
	BPL $F6BD ; FIXME
	LDA $7F
	STA $66
	JSR $EF9B ; FIXME
	JSR $F653 ; FIXME
	LDA #$63 ; FIXME
	LDY #$F6 ; FIXME
	JSR $F6F7 ; FIXME
	LDA #$00
	STA $6E
	PLA
	JMP $F219 ; FIXME


LF6E1	
	STA $85
	STY $86
	JSR $F348 ; FIXME
	LDA #$73
	JSR $F184 ; FIXME
	JSR $F6FB ; FIXME
	LDA #$73
	LDY #$00
	JMP $F184	 ; FIXME

	
L6F7
	STA $85
	STY $86
	JSR $F34B  ; FIXME
	LDA ($85),Y
	STA $87
	LDY $85
	INY
	TYA
	BNE $F70A  ; FIXME
	INC $86
	STA $85
	LDY $86
	JSR $F184  ; FIXME
	LDA $85
	LDY $86
	CLC
	ADC #$05
	BCC $F71B  ; FIXME
	INY
	STA $85
	STY $86
	JSR $EFAF  ; FIXME
	LDA #$78
	LDY #$00
	DEC $87
	BNE $F70E  ; FIXME

	rts
	

values_rnd
LF72B	
	.byt $98,$35,$44,$7a,$6b ; 11879546,42
LF730
	.byt $68,$28,$b1,$46,$20 ;3.927678 E-08
	
RND_ROUTINE
LF735	
	.byt $20,$bd,$f3,$aa,$30,$18,$a9,$ef,$a0,$02,$20
	.byt $23,$f3,$8a,$f0,$e5,$a9,$2b,$a0,$f7,$20,$84,$f1,$a9,$30,$a0,$f7
	.byt $20,$af,$ef,$a6,$64,$a5,$61,$85,$64,$86,$61,$a9,$00,$85,$65,$a5
	.byt $60,$85,$66,$a9,$80,$85,$60,$20,$22,$f0,$a2,$ef,$a0,$02,$4c,$52
	.byt $f3
	
	.byt $20,$48,$f3,$20,$35,$f7,$a9,$73,$a0,$00,$20,$84,$f1,$4c,$6a
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
	.byt $41,$32,$3e,$0a,$2a,$53,$00,$33,$44,$43,$25,$09,$5b,$5a,$5e

	.byt $1c
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
	.byt $08,$3c,$3e,$3c,$c8
Lfe45	
	.byt $00,$38,$07,$3f
Lfe49
routine_to_define_22

	LDA #%10111001 ; 
	BIT FLGTEL
.(
	BPL next
	LDA #$9D ; FILL CHARSET ?
next

	LDY #$00
	STY RES
	STA RES+1
	TYA
.)	




.(
loop
	PHA
	JSR routine_to_define_24 
	PLA
	CLC
	ADC #$01
	CMP #$40
	BNE loop
.)

	LDA RES+1
	SBC #$03
	STA TR0
	SBC #$04
	STA RES+1
	LDA #$8F

	LDY #$FB
	STA RESB
	STY RESB+1
	LDY #$00
loop70
	LDX #$00
	LDA (RESB,X)
	TAX
	INC RESB
.(
	BNE next
	INC RESB+1
next
.)
	JSR routine_to_define_23 
	TXA
	AND #$C0
	BEQ loop70
	CMP #$C0
	BEQ loop71+1
	CMP #$40
	beq next76
	JSR routine_to_define_23 
	
loop71		
	.byt $2c ; Is BIT $00A2 when $FE9F return
	ldx #$00
next76	
	JSR routine_to_define_23
	BNE loop70
routine_to_define_23	
	TXA
	AND #$3F
	STA (RES),Y
	INY
	BNE end4
	INC RES+1
	LDA RES+1
	CMP TR0
	BNE end4
	PLA
	PLA
end4
	RTS

Lfeb2		
routine_to_define_24
put_an_alternate_char_in_memory
	LDX #$03
	STX RESB
next81
	PHA
	AND #$03
	TAX
	LDA Lfe45,X
	STA (RES),Y
	INY
	STA (RES),Y
	INY
	LDX RESB
	CPX #$02
	BEQ next80
	STA (RES),Y
	INY
	BNE next80
	INC RES+1
Lfed0
next80
	PLA
	LSR
	LSR
	DEC RESB
	BNE next81
	RTS
	
Led8	
move_chars_text_to_hires
	ldy #5
	.byt $2c
move_chars_hires_to_text
	ldy #$0b
	ldx #5
Lfedf	
	lda code_in_order_to_move_chars_tables,y ; FIXME
	sta $04,x
	dey
	dex
	bpl Lfedf 
	jmp $cd6c ; FIXME

code_in_order_to_move_chars_tables
	; Text to hires 6 bytes
	.byt $00,$b4,$80,$bb,$00,$98
	; hires to text 6 bytes
	.byt $00,$98,$80,$9f,$00,$b4

redefine_table_chars
; define a char in the adress AY 
; it take the first byte : it's the ascii char
; Next 8 bytes is the definition of the char
	clc
	.byt  $24 ; jump a byte
Lfef9	
	sec
	ror $00
	sta $15
	sty $16

	

Lff00
	ldy #00
	jsr Lff27 
	beq Lff26
	jsr Lff31
	inc $15
	bne Lff10
	inc $16
Lff10
	jsr Lff27 
	sta ($02),y
	iny
	cpy #8
	bne Lff10
	tya
	clc
	adc $15
	sta $15
	bcc Lff00
	inc $16
	bcs Lff00
Lff26	
	rts
read_a_code_in_15_and_y	
Lff27
	bit $00
	bpl Lff2e
	lda ($15),y
	rts
Lff2e	
	jmp $411
ZADCHA_ROUTINE	
Lff31	
compute_address_of_a_char
	ldx #$13
	stx $03
	asl
	rol $03
	asl
	rol $03
	asl
	rol $03
	sta $02
	bit FLGTEL
	bmi Lff4b
	lda $03
	adc #$1c
	sta $03
Lff4b
	rts
	
Lff4c	
select_keyboard_mode
	cmp #$04
	beq Lff85
	cmp #$05
	beq Lff8c
	asl
	and #$06
	sta $00
	tax
	lda $0275
	and #$f9
	ora $00
	sta $0275
	lda Lff90,x
	ldy Lff90+1,x
	sta $2a
	sty $2b
	jsr routine_to_define_22 
	lda $0275
	and #$06
	cmp #$04
	bne Lff81
	lda #$98
	ldy #$ff
	jmp Lfef9 
Lff81	
	cmp #2
	bne Lff8f
Lff85	
	lda #$aa ; FIXME
	ldy #$ff ; FIXME
	jmp Lfef9 
Lff8c	
	jmp routine_to_define_22 
Lff8f	
	rts

Lff90
	.byt $3f,$fa ; QWERTY TABLE FIXME
	.byt $af,$fa ; AZETY TABLE FIXME
	.byt $1f,$fb ; FRENCH TABLE FIXME
	.byt $1f,$fb ; French bwana FIXME
Lff98	
tab_accent_charset
a_accent_circonflexe
	.byt $5b ; Accent circonflexe
	.byt $1c,$22,$1c,$02,$1e,$22,$1e,$00
u_accent_circonflexe	
	.byt $60
	.byt $1c,$22,$00,$22,$22,$26,$1a,$00
	
	.byt $7b,$04,$08,$1c,$22,$3e
	.byt $20,$1e,$00,$7d,$10,$08,$1c,$22,$3e,$20,$1e,$00,$40,$10,$08,$1c
	.byt $02,$1e,$22,$1e,$00,$5c,$00,$00,$1e,$20,$20,$20,$1e,$04,$7c,$10
	.byt $08,$22,$22,$22,$26,$1a,$00,$7e,$1c,$22,$1c,$22,$3e,$20,$1c,$00
	.byt $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byt $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; fffa
NMI:
	.byt $00,$2f
; fffc
RESET:
	.byt $00,$c0
; fffe
BRK_IRQ:	
	.byt <VIRQ,>VIRQ

