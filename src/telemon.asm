#include "include/telemon.h"
#include "include/via6522_1.h"
#include "include/via6522_2.h"
#include "include/acia6551.h"
#include "include/fdc1793.h"



#define CDRIVE $314

#define bank_signature $ff00


#define BRK_TELEMON(value)\
	.byt 00,value;\

	
;#define HAVE_MINITEL
;#define HAVE_MINITEL
;#define HAVE_USBDRIVE
;#echo PREPROCESSOR_CMD
;#printdef HAVE_MINITEL

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
	jsr routine_to_define_18 
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
	JMP routine_to_define_14 
	

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

	BRK_TELEMON($10) ; FIXEME
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
	STA $00
	STY $01
	LDX #$01
	STX FDCSR 
	JSR $B84F ; FIXME
	LDA $C100 ; FIXME
	bne next100
	
	LDA $C103 ; FIXME
	LDY $C104 ; FIXME
	STA $00
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
	LDA $0313
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
	STX $02
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
	STA $00
	LDA $C6B0,X ; FIXME
	STA $01
	
	LDA $C6B1,X ; CORRECTME
	LDY $C6B2,X ; CORRECTME
	
	LDX $02
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
	
	sta $02
	sty $03
	
	sec
	sbc $00
	sta $c08a,x
	tya
	sbc $01
	sta $c08b,x
	
	
	.byt $8a,$69,$03,$aa,$a0,$03,$b9,$00,$00,$9d,$7f,$c0,$ca
	.byt $88,$10,$f6,$a9,$00
	.byt $9d,$88,$c0,$9d,$89,$c0,$bd,$82,$c0,$9d,$84,$c0,$9d,$86,$c0,$bd
	.byt $83,$c0,$9d,$85,$c0,$9d,$87,$c0,$60,$70,$26,$20,$07,$c5,$b0,$20
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
test_if_printer_is_connected	
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
Lc720
#include "functions/XCL.asm"


Lc756

XCRLF_ROUTINE
	lda #$0a
	jsr XWSTR0_ROUTINE 
	lda #$0d

;#include "functions/XCRLF.asm"

Lc75d	
#include "functions/XWSTR.asm"
Lc7a8
#include "functions/XDECA.asm"
Lc7cf
#include "functions/XRD.asm"
#include "functions/XWR.asm"	
	

	
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
	.byt <XOP0_ROUTINE,>XOP0_ROUTINE
	.byt <XOP1_ROUTINE,>XOP1_ROUTINE
	.byt <XOP2_ROUTINE,>XOP2_ROUTINE
	.byt <XOP3_ROUTINE,>XOP3_ROUTINE
	
	.byt <XCL0_ROUTINE,>XCL0_ROUTINE
	.byt <XCL1_ROUTINE,>XCL1_ROUTINE
	.byt <XCL2_ROUTINE,>XCL2_ROUTINE
	.byt <XCL3_ROUTINE,>XCL3_ROUTINE

	
	.byt <XRD0_ROUTINE,>XRD0_ROUTINE
	.byt <XRD1_ROUTINE,>XRD1_ROUTINE
	.byt <XRD2_ROUTINE,>XRD2_ROUTINE
	.byt <XRD3_ROUTINE,>XRD3_ROUTINE
	
	.byt <XWR0_ROUTINE,>XWR0_ROUTINE
	.byt <XWR1_ROUTINE,>XWR1_ROUTINE
	.byt <XWR2_ROUTINE,>XWR2_ROUTINE
	.byt <XWR3_ROUTINE,>XWR3_ROUTINE
	
	.byt <XWSTR0_ROUTINE,>XWSTR0_ROUTINE
	.byt <XWSTR1_ROUTINE,>XWSTR1_ROUTINE
	.byt <XWSTR2_ROUTINE,>XWSTR2_ROUTINE
	.byt <XWSTR3_ROUTINE,>XWSTR3_ROUTINE
	
	.byt <XDECAL_ROUTINE,>XDECAL_ROUTINE
	.byt <XDECAL1_ROUTINE,>XDECAL1_ROUTINE
	.byt <XDECAL2_ROUTINE,>XDECAL2_ROUTINE	
	.byt <XDECAL3_ROUTINE,>XDECAL3_ROUTINE
; POUET	



	.byt $6c,$cd,$75,$cf,$45,$cf,$06,$cf,$14,$cf,$31,$ff
	.byt $bf,$c6,$f0,$d0,$69,$ce,$97,$ce,$89,$ce,$dc,$ce,$f0,$cf,$56,$c7
	.byt $49,$e7,$00,$00,$ef,$cd,$39,$ce,$54,$ce,$9b,$f4,$e0,$cb,$35,$e4
	.byt $b0,$e6,$80,$e6,$40,$d1,$11,$d7,$37,$e5,$6c,$e6,$1e,$de,$20,$de
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

Lcbe0	
menu_deroulant
.(
	sty $60
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
	ldx $60
	sec
	txa
	sbc $66
	clc
	adc $62
	tay 
	jsr Lccd3 ; FIXME
	jsr XWR0_ROUTINE 
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
	ldx $60
	
	jsr display_x_choice 
	jmp Lcbfd
Lcc6d	
	cmp #$0b
	bne Lcc9a 
	lda $60
	cmp $66
	bne Lcc92
	
	lda $60
	beq Lcc94 
	dec $66
	dec $67 
	dec $60 
	bit $020d ; Minitel ?
	bvs Lcc97 
	ldx $62
	ldy $63
	jsr $de5c  ; FIXME
	ldy $62
	jmp Lcc65
Lcc92	
	dec $60
Lcc94
	jmp Lcbfd 
Lcc97	
	jmp Lcbeb
Lcc9a	
	cmp #$30
	bcc Lcc94
	cmp #$3a
	bcs Lcc94 
	ldx $60
	cpx #$19
	bcc Lccae
Lcca8	
	ldx $66
	stx $60 
	bcs Lcc94
Lccae	
	pha
	asl $60
	lda $60 
	asl $60 
	asl $60
	adc $60
	sta $60
	pla
	and #$0f
	adc $60
	sbc #0
	
	sta $60
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
	jsr XWSTR0_ROUTINE
	dex
	bpl Lccdd 
	lda #$2d
	jmp XWSTR0_ROUTINE 
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
	jsr XWSTR0_ROUTINE
	lda $02
	ldy $03
	jsr $c7a8 ; FIXME
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
	jsr XWSTR0_ROUTINE
	tya
	ora #$40
	jsr XWSTR0_ROUTINE ;FIXME
	lda $61
	ora #$40
	jmp XWSTR0_ROUTINE ;FIXME

	
	

	
shift_memory_block
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
	bcc $cdb9 ; FIXME
	stx $0b

	lda $08
	cmp $04
	lda $09
	sbc $05
	bcs $cdbf ; FIXME
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
	bcs $cda2 ; FIXME

	dec $09
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
	sec
	pla
	tay
	pla
	tax
	pla
	rts
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
	beq $cdb8 ; FIXME
	

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
Lcdef	
routine_to_define_15
convert_into_decimal
; AY contains the number
; X ...

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

Lce39
routine_to_define_14
convert_in_decimal_and_display_on_channel_0
	PHA
	LDA #$00
	STA TR5
	LDA #$01
	STA TR6
	PLA
	JSR convert_into_decimal
	LDY #$00
loop31

	LDA FUFTRV,Y
	jSR XWSTR0_ROUTINE
	INY
	CPY TR4
	BNE loop31
	RTS
	
	
Lce54	
convert_into_hex
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


Lce69
mult_by_40
	ldy #0
	sta $00
	sty $01
	asl
	rol $01
	asl
	rol $01
	adc $00
	bcc Lce7b
	inc $01
Lce7b
	asl
	rol $01
	asl
	rol $01
	asl
	rol $01
	sta $00
	ldy $01
	rts

Lce89	
add_integers
; RES +AY = RES and AY
	clc
	adc $00
	sta $00 
	pha
	tya
	adc $01
	sta $01
	tay
	pla
	rts
	
mult_integers
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
	
	lda $00
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

	asl $00
	rol $01
	rol $02
	rol $03
	
	lda $10
	ora $11
	beq Lcedb
	dex
	bne $ceab ; FIXME
Lcedb	
	rts
	
divide_interger
Lcedc
	sta $0c
	sty $0d
	ldx #0
	stx $02
	stx $03
	ldx #$10
Lcee8	
	asl $00
	rol $01
	rol $02
	rol $03
	sec
	lda $02
	sbc $0c
	tay 
	lda $03
	sbc $0d
	bcc $cf02
	sty $02
	sta $03
	inc $00
	dex
	bne Lcee8
	rts

clear_hires
	lda #00
	ldy #$a0
	sta $00
	sty $01
	ldy #$68
	ldx #$bf
	lda #$40

Fill_a_memory_zone	
	pha
	sec
	tya
	sbc $00
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
switch_hires
	ldx #$00
	ldy #$ff
	sty $02aa ; pattern
	iny
	jsr $e7f3 ; FIXME
	lda $020d ; we are already in Hires ?
	bmi $cf06 ; FIXME 
	ora #$80
	sta $020d ; Set to Hires flag
	php 
	sei
	lda #$1f
	sta $bf67

	
	.byt $20,$a4,$cf,$20,$d8,$fe,$a9,$5c,$a0,$02,$a2,$00,$20,$fd,$de
	.byt $20,$06,$cf,$28,$60,$ad,$0d,$02,$10,$29,$08,$78,$29,$7f,$8d,$0d
	.byt $02,$20,$db,$fe,$a9,$56,$a0,$02,$a2,$00,$20,$fd,$de
	
	
	.byt $a9,$1a,$8d
	.byt $df,$bf,$20,$a4,$cf,$a2,$28,$a9,$20,$9d,$7f,$bb,$ca,$d0,$fa,$20
	.byt $20,$de,$28,$60,$a0,$1f,$a2,$00,$ca,$d0,$fd,$88,$d0,$fa,$60,$38
	.byt $24,$18,$66,$15,$a2,$00,$20,$0f,$c5,$90,$08,$8a,$69,$0b,$aa,$e0
	.byt $30,$d0,$f3,$08,$a9,$dc,$a0,$cf,$b0,$04,$a9,$e6,$a0,$cf,$24,$15
	.byt $10,$05,$20,$f9,$fe,$28,$60,$20,$f9,$fe,$28,$60
table_to_define_prompt_charset
	.byt $7f ; char 127
	.byt $00,$00,$08,$3c,$3e,$3c,$08,$00,$00
	
	.byt $7f,$00,$00,$08,$34,$32,$34,$08,$00,$00
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
	.byt $ce,$85,$2c,$84,$2d,$4c,$56,$d7
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
	sta $00
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




Ld46a
	
	
	.byt $70,$1e,$a2,$14,$dd,$a7
	.byt $d4,$f0,$06,$ca,$10,$f8,$a9,$5f,$60,$e0,$05,$b0,$08,$8a,$09,$c0
	.byt $85,$37,$a9,$00,$60,$29,$1f,$09,$80,$60

	.byt $48,$98,$29,$07,$aa,$bd
	.byt $c1,$d4,$a8,$bd,$bc,$d4,$aa,$68,$dd,$c6,$d4,$f0,$06,$e8,$88,$d0
	.byt $f7,$aa,$60

	.byt $bd,$de,$d4,$60
	.byt $41,$42,$43,$48,$4b,$20,$23,$24,$26
	.byt $2c,$2d,$2e,$2f,$30,$31,$38,$3c,$3d,$3e,$6a,$7a,$00,$05,$07,$0e
	.byt $11,$05,$02,$07,$03,$02,$41,$61,$45,$65,$75,$45,$65,$41,$61,$45
	.byt $65,$75,$69,$6f,$45,$65,$69,$43,$63,$41,$42,$43,$48,$4b,$87,$97
	.byt $89,$99,$88,$82,$92,$81,$86,$8b,$9b,$96,$80,$9f,$84,$93,$94,$85
	.byt $95,$a2,$00,$c9,$a0,$90,$05,$e9,$5f,$a0,$13,$60
	
	.byt $a8,$30,$04,$a8
	.byt $a9,$00,$60,$a0,$12,$d9,$de,$d4,$f0,$13,$88,$10,$f8,$18,$69,$a0
	.byt $c9,$2a,$f0,$04,$c9,$3a,$d0,$02,$09,$40,$a0,$19,$60
	
	.byt $98,$a2,$04
	.byt $dd,$bc,$d4,$b0,$03,$ca,$d0,$f8,$bd,$d9,$d4
	

	.byt $be,$c6,$d4,$d0,$ea
Ld530	

	STA $36
	ASL $36
	ASL $36
	TAY
	BPL Ld55f ; 
	PHA
	TXA
	CMP #$60
	BCC Ld541
	SBC #$20
Ld541
	SEC
	SBC #$20
	STA $36
	LDA $33
	AND #$40
	ORA $36
	TAX
	LDA $32
	AND #$70
	STA $36
	PLA
	AND #$8F
	ORA $36
	LDY $38
	STY $36
	JMP Ld5af	
Ld55f	
	CPX #$20
	BNE Ld58e
	BIT $32
	BPL Ld58e
	AND #$70
	STA $35
	LDA $32
	AND #$04
	ORA #$80
	ORA $35
	TAX
	LDA $32
	AND #$74
	STA $32
	AND #$70
	STA $35
	LSR
	LSR
	LSR
	LSR
	BIT $34
	BVC Ld58a
	LDA $34
	AND #$07
Ld58a
	ORA $35
	ORA #$80
Ld58e
	BIT $36
	BVC Ld5a5
	DEC $2F
	DEC $31
	PHA
	SEC
	LDA $38
	SBC #$28
	TAY
	PLA
	JSR Ld5a7
	INC $2F
	INC $31
Ld5a5
	LDY $38
Ld5a7
	JSR Ld5af
	BIT $36
	BPL Ld5b6
	INY
Ld5af
	PHA
	TXA
	STA ($2E),Y
	PLA
	STA ($30),Y
Ld5b6
RTS



	.byt $20,$75,$cf,$4c,$4d,$d7
Ld5db
init_minitel
routine_to_define_7
	LDA #$00
	STA FLGVD0
	STA $3D ; CORRECTME
	STA $37 ; CORRECTME
	rts

	.byt $10,$11,$b0,$ed,$20,$45,$cf,$20,$bd,$d5
	.byt $a9,$96,$a0,$d7,$20,$f9,$fe,$a9,$0c,$4c,$78,$d1

#ifdef HAVE_MINITEL
; 281 bytes
#include "functions/minitel/display_in_videotex_mode.asm"
	.byt $a9,$00,$a0,$03,$46,$02,$08,$6a,$28,$6a,$88
	.byt $d0,$f7,$4a,$4a,$29,$3f,$09,$40,$60,$1b,$1b,$00,$1b,$00,$1b,$1b
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
	sta $00
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
/*
	TAX
	LDA $028A
	AND #$04
	BEQ next905
	JSR $DB2F ; FIXME
	TXA
	LDX #$18
	BNE next906
next905
	LDA FLGTEL
	AND #$02
	BEQ next907
	TXA
	LDX #$24



*/
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



	
	
	
	
	.byt $29,$08,$f0,$1a,$a5,$29
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
	.byt $20,$99,$df,$29,$1b,$85,$58,$c9,$1b,$d0,$05
	.byt $ce,$a4,$02,$d0,$ef,$ad,$a5,$02,$8d,$a4,$02,$a5,$58,$c9,$1b,$f0
	.byt $14,$29,$1b,$4d,$8e,$02,$29,$1b,$d0,$0b,$ce,$92,$02,$d0,$31,$ae
	.byt $99,$02,$4c,$bb,$e0,$20,$99,$df,$ae,$9a,$02,$8e,$92,$02,$29,$1b
	.byt $85,$58,$ad,$8e,$02,$29,$64,$05,$58,$8d,$8e,$02,$a5,$58,$09,$04
	.byt $a2,$04,$4a,$48,$b0,$06,$bd,$9d,$02,$20,$9d,$e1,$68,$ca,$10,$f2
	.byt $60
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
Le1eb
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
POUET2	
	

	.byt $18,$33,$1b,$0a,$0d,$00,$f0,$4b,$1b,$0d,$0a,$40,$1b,$0a,$0a
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
	.byt $d0,$f6,$60
FILL_ROUTINE
	lda $4b
	ldy $4c
	sta $00 
	sty $01
Lea7b
	ldx $4f
	ldy $49
	lda $51
Lea81	
	sta (RES),y
	iny
	dex
	bne Lea81 ; FIXME
	lda #$28
	ldy #0
	jsr $ce89 ; FIXME
	dec $4d
	bne Lea7b ; FIXME
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
	
	.byt $a5
	.byt $4d,$0a,$46,$4f,$6a,$48,$a5,$46,$c9,$ea,$90,$17,$a6,$4a,$a5,$47
	.byt $69,$07,$a8,$e9,$bf,$90,$09,$f0,$07,$c9,$08,$d0,$02,$a9,$00,$a8
	.byt $20,$f3,$e7,$68,$20,$31,$ff,$a0,$00,$84,$00,$a5,$49,$48,$a5,$4a
	.byt $48,$b1,$02,$0a,$0a,$f0,$0c,$48,$10,$03,$20,$9c,$e7,$20,$d9,$e7
	.byt $68,$d0,$f1,$20,$c1,$e7,$68,$85,$4a,$68,$85,$49,$a4,$00,$c8,$c0
	.byt $08,$d0,$d6,$a5,$46,$69,$05,$aa,$a4,$47,$4c,$f3,$e7
PLAY_ROUTINE
	lda $4f
	asl
	asl
	asl

	ora $4d
	eor #$3f
	tax
	
	lda #7
	jsr lda1a 
	asl $53
	rol $54

	ldx $53
	lda #$0b
	jsr lda1a ; FIXME
	

	ldx $54
	lda #$0c
	jsr lda1a 
	ldy $51
	ldx enveloppes_play_0_to_7,y ; FIXME
	lda #$0d
	jmp lda1a 

enveloppes_play_0_to_7
Leb38
	.byt $00,$0b,$04,$08,$0a,$0b,$0c,$0d
periods_note_octave_0


	.byt $00,$00,$ee,$0e,$16,$0e,$4c,$0d,$8e,$0c,$d8,$0b,$2e,$0b,$8e,$0a
	.byt $f6,$09,$66,$09,$e0,$08,$60,$08,$e8,$07
/*End of period*/

MUSIC_ROUTINE
	.byt $a4,$4f,$a5,$51,$0a,$aa
	.byt $bd,$40,$eb,$85,$4f,$bd,$41,$eb,$4a,$66,$4f,$88,$10,$fa,$85,$50
	.byt $a6,$53,$2c,$a6,$51,$8a,$d0,$02,$a2,$10,$a4,$4d,$88,$98,$c9,$03
	.byt $90,$02,$e9,$03,$09,$08,$20,$1a,$da,$c0,$03,$b0,$0c,$98,$0a,$a8
	.byt $69,$01,$a6,$50,$20,$1a,$da,$98,$2c,$a9,$06,$a6,$4f,$4c,$1a,$da
	

	
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
	jmp $db12 ; FIXME
.)	
send_A_to_serial_output_with_check
Lec4f
; MINITEL
	stx $0C
	sty $0d
	pha
	bit $5b
	bpl $ec5e ; FIXME
	jsr $ec21 ; FIXME
	
	.byt $4c,$61,$ec,$20,$1b
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
	.byt $5b
	
	
	.byt $20,$d1,$ec,$20,$56,$ee,$4c,$cf,$ec,$24,$5b,$70,$03,$20,$fd
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
	.byt $d9,$ec,$a9,$67,$20,$30,$ef,$4c,$d7,$ec
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
	jsr $ecb9 ; FIXME POUET
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
	jsr $ecc9
	pla
	jsr $ec1b ; FIXME
	jmp $ecc7 ; FIXME
add_0_5_A_ACC1
	lda #$e4
	ldy #$f5
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

	bcs $f022 ; FIXME
	jsr $f090 ; FIXME

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
Lf90	



	
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
	lda #$aa
	ldy #$ff
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

