/***********************************************************************/
/* DASM and source converted with labels : jede (jede[at]oric[dot]org) */
/* may and june 2016                                                   */
/* with the help of G. Meister telemon dasm 						   */
/* telemon 2.4                                                         */
/***********************************************************************/


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
	jsr XALLKB_ROUTINE
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
	JSR XDEFBU_ROUTINE 
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
	lda $0200,x ; FIXME
	bpl Lc365 

	stx V2DRA
	lda $fff8 
	sta $02
	lda $fff9
	sta $03
	ldy #0
Lc352		
	stx V2DRA 

	lda (RESB),y
	pha

	lda #7
	sta V2DRA
		
	pla
	beq Lc365 

	BRK_TELEMON(XWR0)
	iny
	
	bne Lc352
Lc365	
	lda $0200,x
	asl
	bpl Lc382	
	stx V2DRA
	lda $fffc ; read execution address
	ldy $fffd 
	sta $02fe
	sty $02ff
	stx $02fd
	lda #7
	sta V2DRA
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
	STY RES+1
	LDX #$01
	STX FDCSR 
	JSR $B84F ; FIXME
	LDA $C100 ; FIXME
	bne next100
	
	LDA $C103 ; FIXME
	LDY $C104 ; FIXME
	STA RES
loop101	
	STY RES+1
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
	INC RES+1  
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
	STA FDCCR
	LDY #$04
loop100
	DEY
	BNE loop100
loop103
	LDA FDCCR
	LSR
	bcc end6
	LDA $0318 ; FIXE
	bmi loop103
	LDA FDCDR
	STA (RES),Y
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
	LDA FDCCR
	AND #$1C
	BEQ end7
	BNE loop105
XDEFBU_ROUTINE	

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
	STA RES+1
	
	LDA $C6B1,X ; CORRECTME
	LDY $C6B2,X ; CORRECTME
	
	LDX RESB
	;BIT $C518 ; CORRECTME
XINIBU_ROUTINE	
	.byt $2c,$18,$c5  ; CORRECTME
	BVC next19

XVIDBU_ROUTINE	
	LDA #$00
lc50e
	.byt $2c
XTSTBU_ROUTINE	
	lda #1

	.byt $2c 
	.byt $24 ; FIXME
	.byt $c5 ; FIXME

next19
	SEC
	JMP $0409
Lc518
XLISBU_ROUTINE
	.byt $2c
	.byt $18  ; FIXME
	.byt $c5 ; FIXME
	;BIT $C518 ; CORRECTME
	
	BVC next20
XECRBU_ROUTINE	
LC51D
	.byt $2c
	.byt $24 ; FIXME
	.byt $c5 ; FIXME

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
LC5FE	
	sta RESB
	sty $03
	
	sec
	sbc RES
	sta $c08a,x
	tya
	sbc RES+1
	sta $c08b,x
	
	txa
	adc #3
	tax
	ldy #3
Lc614	
	lda $0000,y ; FIXME
	sta $c07f,x ; FIXME
	dex
	dey
	bpl  Lc614
	
	lda #0
	sta $c088,x ; FIXME
	sta $c089,x ; FIXME
	lda $c082,x ; FIXME
	sta $c084,x ; FIXME
	sta $c086,x ; FIXME
	lda $c083,x ; FIXME
	sta $c085,x ; FIXME
	sta $c087,x ; FIXME
	rts
	
	bvs LC661 
	jsr $c507 ; FIXME
	bcs $c660 ; FIXME
	lda $c086,x ; FIXME
	ldy $c087,x ; FIXME
	jsr $c5a6 ; FIXME
	sta $c086,x ; FIXME
	tya
	sta $c087,x ; FIXME
	lda $c088,x ; FIXME
	bne LC658 
	dec $c089,x; FIXME
LC658	
	dec $c088,x ; FIXME
	ldy #0
	lda ($24),y
	clc
	rts
	
LC661
	PHA
	LDA $C088,X ; FIXME
	CMP $C08A,X ; FIXME
	LDA $C089,X ; FIXME
	SBC $C08B,X ; FIXME
	BCS LC68F 
	LDA $C084,X ; FIXME
	LDY $C085,X ; FIXME
	JSR $C5A6 ; FIXME
	STA $C084,X ; FIXME
	TYA
	STA $C085,X ; FIXME
	INC $C088,X ; FIXME
	BNE LC688
	INC $C089,X ; FIXME
LC688	
	LDY #$00
	PLA
	STA (IRQSVP),Y
	CLC
	RTS
LC68F	
	PLA
	RTS
LC691
	clc
	adc #1
	bcc LC697 
	iny
LC697	
	cmp $c082,x ; FIXME
	
	sta $24 ; FIXME
	
	

routine_to_define_16
LC69C
	tya
	SBC $C083,X ; FIXME
	BCC next36 
	LDA $C080,X ; FIXME
	LDY $C081,X ; FIXME
	STA IRQSVP
next36
	STY $25 ; FIXME
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
	jsr XWR0_ROUTINE 
	lda #$0d



Lc75d	
#include "functions/XWRx.asm"
#include "functions/XWSTRx.asm"
#include "functions/XRDW.asm"
#include "functions/XWRD.asm"	
	

send_command_A	
Lc81c
	STY $17 ;FIXME
	STY $18 ;FIXME
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
	.byt <LDB5D,>LDB5D ; RSE 
	.byt $1a,$c8 ;  not used  
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8 ; not used 
	.byt <LDB86,>LDB86
	.byt $8c,$db ; FIXME
	.byt $92,$db  ; FIXME
	.byt $98,$db  ; FIXME
	.byt $1a,$c8 ; not used 
	.byt $1a,$c8  ; FIXME
	.byt <Lda70,>Lda70 ;30
	.byt <Ldb12,>Ldb12
	.byt <LDB79,>LDB79 
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
	ROR TRANSITION_RS232
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
	LSR TRANSITION_RS232
	PHA
	AND #$08
	BEQ next24
	LDX ACIADR
	PLA
	PHA
	AND #$07
	BEQ next25
	ORA #$B0
	.byt $24
next25	
	txa

	LDX #$0C
	JSR XECRBU_ROUTINE

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
	JSR XLISBU_ROUTINE 
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
	DEC FLGCLK_FLAG
	BNE Lc973
	LDA #$04
	STA FLGCLK_FLAG
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
	LDA #$14 
	STA $02A7 ; CORRECTME
	BNE LC9FB 
next114	
	LDA $02A8 ; CORRECTME
	BIT $02A7 ; CORRECTME
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
	BIT V1DRA
	JSR Lca2f 
	JMP LC8B9
manage_printer	
Lca2f	
	LDX #$24
	JSR XLISBU_ROUTINE 
	BCC Lca3e
	ASL FLGLPR ; printer
	SEC
	ROR FLGLPR ; printer
	RTS
Lca3e
	sta V1DRA
	lda V1DRB
	and #$ef
	sta V1DRB
	ora #$10
	sta V1DRB
	asl FLGLPR
	lsr FLGLPR
	rts
	
XRECLK_ROUTINE	
reset_clock
	lda #0
	ldx #4
Lda59	
	sta $0210,x
	dex
	bpl Lda59
	lda #1
	sta FLGCLK_FLAG
	rts
XCLCL_ROUTINE
	lsr $0214
	rts
XWRCLK_ROUTINE

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
	.byt <XCRLF_ROUTINE,>XCRLF_ROUTINE ; $25
	.byt <XDECAY_ROUTINE,>XDECAY_ROUTINE ; XDECAY  $26
	.byt $00,$00 ; nothing  $27 
	.byt <XBINDX_ROUTINE,>XBINDX_ROUTINE ; XBINDX $28
	.byt <XDECIM_ROUTINE,>XDECIM_ROUTINE
	.byt <XHEXA_ROUTINE,>XHEXA_ROUTINE
	.byt <XA1AFF_ROUTINE,>XA1AFF_ROUTINE ; XA1AFF 
	.byt <XMENU_ROUTINE,>XMENU_ROUTINE ; XMENU
	.byt <XEDT_ROUTINE,>XEDT_ROUTINE ; XEDT 
	.byt <XINSER_ROUTINE,>XINSER_ROUTINE ; XINSER 
	.byt <XSCELG_ROUTINE,>XSCELG_ROUTINE ; XSCELG $2f
	.byt <XVDTCT_ROUTINE,>XVDTCT_ROUTINE ; control_videotex : no name $30 MINITEL
	.byt <XVDTG2_ROUTINE,>XVDTG2_ROUTINE ; $31 
	.byt <XEDTIN_ROUTINE,>XEDTIN_ROUTINE; XEDTIN $32
	.byt <XECRPR_ROUTINE,>XECRPR_ROUTINE; XECRPR $33 $ece6
	.byt <XCOSCR_ROUTINE,>XCOSCR_ROUTINE  ;XCOSCR $34
	.byt <XCSSCR_ROUTINE,>XCSSCR_ROUTINE ; $35 XCSSCR 
	.byt <XSCRSE_ROUTINE,>XSCRSE_ROUTINE ; $36 
	.byt <XSCROH_ROUTINE,>XSCROH_ROUTINE ; $37
	.byt <XSCROB_ROUTINE,>XSCROB_ROUTINE ; $38 FIXME XSCROB
	.byt <XSCRNE_ROUTINE,>XSCRNE_ROUTINE ; $39
	.byt <XVDTDA_ROUTINE,>XVDTDA_ROUTINE ; $3a 
	.byt <XVDTAH_ROUTINE,>XVDTAH_ROUTINE ; $3b
	.byt <XRECLK_ROUTINE,>XRECLK_ROUTINE ; $3c
	.byt <XCLCL_ROUTINE,>XCLCL_ROUTINE ; $3d
	.byt <XWRCLK_ROUTINE,>XWRCLK_ROUTINE ; $3e
	.byt $00,$00 ; nothing $3f
	.byt <XSONPS_ROUTINE,>XSONPS_ROUTINE ; $40
	.byt <XEPSG_ROUTINE,>XEPSG_ROUTINE ; $41
	.byt <XOUPS_ROUTINE,>XOUPS_ROUTINE ; $42 XOUPS ddd8
	.byt <XPLAY_ROUTINE,>XPLAY_ROUTINE ;XPLAY
	.byt <XSOUND_ROUTINE,>XSOUND_ROUTINE
	.byt <XMUSIC_ROUTINE,>XMUSIC_ROUTINE
	.byt <XZAP_ROUTINE,>XZAP_ROUTINE
	.byt <XSHOOT_ROUTINE,>XSHOOT_ROUTINE ; 47
	.byt <XLPRBI_ROUTINE,>XLPRBI_ROUTINE ; $48
	.byt <XLPCRL_ROUTINE,>XLPCRL_ROUTINE ; $49
	.byt <XHCSCR_ROUTINE,>XHCSCR_ROUTINE ; $4a
	.byt <XHCVDT_ROUTINE,$e2 ; FIXME BUG
	
	.byt <XHCHRS_ROUTINE,>XHCHRS_ROUTINE ; $4c
	.byt $00,$00 ; $4d
	.byt $00,$00 ; $4e
	.byt $00,$00 ; $4f
	.byt <XALLKB_ROUTINE,>XALLKB_ROUTINE ; $50
	.byt <XKBDAS_ROUTINE,>XKBDAS_ROUTINE ; $51
	.byt <XGOKBD_ROUTINE,>XGOKBD_ROUTINE ; $52
	.byt $00,$00
	.byt <XECRBU_ROUTINE,>XECRBU_ROUTINE
	.byt <XLISBU_ROUTINE,>XLISBU_ROUTINE
	.byt <XTSTBU_ROUTINE,>XTSTBU_ROUTINE
	.byt <XVIDBU_ROUTINE,>XVIDBU_ROUTINE
	.byt <XINIBU_ROUTINE,>XINIBU_ROUTINE	
	.byt <XDEFBU_ROUTINE,>XDEFBU_ROUTINE
	.byt <XBUSY_ROUTINE,>XBUSY_ROUTINE
	.byt $00,$00
	.byt <XSDUMP_ROUTINE,>XSDUMP_ROUTINE
	.byt <XCONSO_ROUTINE,>XCONSO_ROUTINE
	.byt <XSLOAD_ROUTINE,>XSLOAD_ROUTINE
	.byt <XSSAVE_ROUTINE,>XSSAVE_ROUTINE
	.byt <XMLOAD_ROUTINE,>XMLOAD_ROUTINE
	.byt <XMSAVE_ROUTINE,>XMSAVE_ROUTINE
	.byt <XRING_ROUTINE,>XRING_ROUTINE
	.byt <XWCXFI_ROUTINE,>XWCXFI_ROUTINE
	.byt <XLIGNE_ROUTINE,>XLIGNE_ROUTINE
	.byt <XDECON_ROUTINE,>XDECON_ROUTINE
	.byt <XMOUT_ROUTINE,>XMOUT_ROUTINE
	.byt <XSOUT_ROUTINE,>XSOUT_ROUTINE
	.byt <XA1DEC_ROUTINE,>XA1DEC_ROUTINE
	.byt <XDECA1_ROUTINE,>XDECA1_ROUTINE
	.byt <XA1PA2_ROUTINE,>XA1PA2_ROUTINE
	.byt <XA2NA1_ROUTINE,>XA2NA1_ROUTINE
	.byt <XA1MA2_ROUTINE,>XA1MA2_ROUTINE
	.byt <XA2DA1_ROUTINE,>XA2DA1_ROUTINE
	.byt <XA2EA1_ROUTINE,>XA2EA1_ROUTINE
	.byt <XNA1_ROUTINE,>XNA1_ROUTINE
	.byt <XSIN_ROUTINE,>XSIN_ROUTINE
	.byt <XCOS_ROUTINE,>XCOS_ROUTINE
	.byt <XTAN_ROUTINE,>XTAN_ROUTINE
	.byt <XATN_ROUTINE,>XATN_ROUTINE
	.byt <XEXP_ROUTINE,>XEXP_ROUTINE
	.byt <XLN_ROUTINE,>XLN_ROUTINE
	.byt <XLOG_ROUTINE,>XLOG_ROUTINE
	.byt <XRND_ROUTINE,>XRND_ROUTINE
	.byt <XSQR_ROUTINE,>XSQR_ROUTINE
	.byt <XRAD_ROUTINE,>XRAD_ROUTINE
	.byt <XDEG_ROUTINE,>XDEG_ROUTINE
	.byt <XINT_ROUTINE,>XINT_ROUTINE
	.byt <XPI_ROUTINE,>XPI_ROUTINE
	.byt <XRAND_ROUTINE,>XRAND_ROUTINE
	.byt <XA1A2_ROUTINE,>XA1A2_ROUTINE
	.byt <XA2A1_ROUTINE,>XA2A1_ROUTINE
	.byt <XIYAA1_ROUTINE,>XIYAA1_ROUTINE
	.byt <XAYA1_ROUTINE,>XAYA1_ROUTINE
	.byt <XA1IAY_ROUTINE,>XA1IAY_ROUTINE
	.byt <XA1XY_ROUTINE,>XA1XY_ROUTINE
	.byt <XAA1_ROUTINE,>XAA1_ROUTINE
	.byt <XADNXT_ROUTINE,>XADNXT_ROUTINE
	.byt <XINTEG_ROUTINE,>XINTEG_ROUTINE
	.byt $00,$00
	.byt <XHRSCG_ROUTINE,>XHRSCG_ROUTINE
	.byt <XHRSCD_ROUTINE,>XHRSCD_ROUTINE
	.byt <XHRSCB_ROUTINE,>XHRSCB_ROUTINE
	.byt <XHRSCH_ROUTINE,>XHRSCH_ROUTINE
	.byt <XHRSSE_ROUTINE,>XHRSSE_ROUTINE
	.byt <XDRAWA_ROUTINE,>XDRAWA_ROUTINE
	.byt <XDRAWR_ROUTINE,>XDRAWR_ROUTINE
	.byt <XCIRCL_ROUTINE,>XCIRCL_ROUTINE
	.byt <XCURSE_ROUTINE,>XCURSE_ROUTINE
	.byt <XCURMO_ROUTINE,>XCURMO_ROUTINE
	.byt <XPAPER_ROUTINE,>XPAPER_ROUTINE  
	.byt <XINK_ROUTINE,>XINK_ROUTINE ; Xink $93
	.byt <XBOX_ROUTINE,>XBOX_ROUTINE
	.byt <XABOX_ROUTINE,>XABOX_ROUTINE; $95
	.byt <XFILL_ROUTINE,>XFILL_ROUTINE
	.byt <XCHAR_ROUTINE,>XCHAR_ROUTINE ;$97
	.byt <XSCHAR_ROUTINE,>XSCHAR_ROUTINE ; 98
	.byt $00,$00 ; nothing $99
	.byt $00,$00 ; nothing $9a
	.byt $00,$00 ; nothing $9b
	.byt <XEXPLO_ROUTINE,>XEXPLO_ROUTINE ; $9c
	.byt <XPING_ROUTINE,>XPING_ROUTINE ; $9d


XMENU_ROUTINE
menu_deroulant
.(
	sty VARMNB
	sta $66
	stx $63
	ldx #0
	jsr XCOSCR_ROUTINE ;  switch off cursor
Lcbeb	
	ldy $62
	ldx $66
	.byt $2c
Lcbf0
	inx
	iny 
	jsr Lccf9  
	
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
	jsr XSCROH_ROUTINE 
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
	bit FLGTEL ; Minitel ?
	bvs Lcc97 
	ldx $62
	ldy $63
	jsr XSCROB_ROUTINE  
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
	jsr put_cursor_in_61_x 
	bit FLGTEL ; Minitel ?
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
	lda (ADSCR),y
	ora #$80
	sta (ADSCR),y
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
	jsr XDECIM_ROUTINE 
	
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
	lda (DECFIN),y
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
	.byt $2c,$09,$30 ; FIXME
	
	;BIT $3009 ; CORRECTME
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
	sty RES+1
	asl
	rol RES+1
	asl
	rol RES+1
	adc RES
	bcc Lce7b
	inc RES+1
Lce7b
	asl
	rol RES+1
	asl
	rol RES+1
	asl
	rol RES+1
	sta RES
	ldy RES+1
	rts

#include "functions/XADRES.asm"
	
XMULT_ROUTINE

	sta $10
	sty $11
	ldx #00
	stx TR0
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
	adc TR0
	sta TR0
	
	lda RES+1
	adc $0d
	sta $0d
	
	lda $02
	adc $0e
	sta $0e
	
	lda $03
	adc $0f
	sta $0f

	asl RES
	rol RES+1
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
	sta TR0
	sty $0d
	ldx #0
	stx $02
	stx $03
	ldx #$10
Lcee8	
	asl RES
	rol RES+1
	rol $02
	rol $03
	sec
	lda $02
	sbc TR0
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
	sty RES+1
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
	sbc RES+1
	tax
	sty $02
	pla
	ldy #0
Lcf23	
	cpy $02
	bcs Lcf2c
	sta (RES),y
	iny
	bne Lcf23
Lcf2c	
	pha
	tya
	
	ldy #0
	jsr XADRES_ROUTINE ; FIXME
	pla
	cpx #0
	beq Lcf44 
	ldy #0
Lcf3a	
	sta (RES),y
	iny
	bne Lcf3a
	inc RES+1
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
	lda FLGTEL ; we are already in Hires ?
	bmi $cf06 ; FIXME 
	ora #$80
	
	sta FLGTEL ; Set to Hires flag
	
	php 
	sei
	lda #$1f
	sta $bf67 
	jsr wait_0_3_seconds 
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
	lda FLGTEL
	bpl $cfa3 ; FIXME
	php 
	
	sei
	and #$7f
	sta FLGTEL
	jsr $fedb ; FIXME
	lda #$56
	ldy #$02
	ldx #0
	jsr $defd ; FIXME
	
	lda #$1a
	sta $bfdf
	jsr wait_0_3_seconds ; FIXME
	ldx #$28
	lda #$20

Lcf99	
	sta $bb7f,x
	dex
	bne Lcf99	
	jsr XCSSCR_ROUTINE
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
XBUSY_ROUTINE	
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
	sta RES+1
	ldx #$94
	lda #$0
	jsr XFILLM_ROUTINE 
	ldy #0
	lda #$94
	sty RES
	sta RES+1 
	ldx #$98
	lda #$87
	jsr XFILLM_ROUTINE
	
	ldy #0
	lda #$a0
	
	sty RES
	sta RES+1
	ldy #$3f
	ldx #$bf
	lda #$10
	jmp XFILLM_ROUTINE
	
	
; MINITEL
manage_VDT_cursor
XVDTCT_ROUTINE
Ld140
	lda VDTY
	jsr XMUL40_ROUTINE 
	pha
	tya
	pha
	lda #0
	ldy #$90
	jsr XADRES_ROUTINE
	sta $2e
	sty $2f
	sta $30
	iny
	iny
	iny
	iny
	sty $31
	pla
	sta RES+1
	pla
	asl
	rol RES+1
	asl
	rol RES+1
	asl 
	rol RES+1
	sta RES
	lda #0
	ldy #$a0
	jsr XADRES_ROUTINE
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
	sta RES+1 ; CORRECTME
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
	sta VDTY
	jsr CTRL_M_KEYBOARD 

	jsr $d759 ; FIXME
	jmp $d140 ; FIXME
Ld2a3		
	jsr $d759 ; FIXME
	lda $0283
	sta VDTX
	dec VDTX
	lda $0282 ; CORRECTME
	and #$3f
	sta VDTY
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
	jmp XOUPS_ROUTINE 
CTRL_H_KEYBOARD 
	jsr $d759 ; switch off cursor  Why not BRK ?
	lda VDTX
	beq $d3be ; FIXME
Ld389	
	dec VDTX
Ld38b
	jmp $d756 ; FIXME
CTRL_I_KEYBOARD 	
	jsr $d759 ; FIXME
	inc VDTX
	lda VDTX
	cmp #$28
	bcc Ld38b
	lda VDTY
	beq Ld389	
	jsr $d3d7
CTRL_J_KEYBOARD ; 10 code
	jsr $d759 ; FIXME
	ldx VDTY
	beq $d3b3 ; FIXME
	cpx #$18
	bne Ld3ad
	ldx #$0
Ld3ad	
	inx
Ld3ae
	stx VDTY
	jmp $d140 ; FIXME
	
	lda $280
	ldx $281
	sta VDTX
	jmp  Ld3ae
	lda #$27 
	sta VDTX
	
CTRL_K_KEYBOARD 
	jsr $d759 ; FIXME
	ldx VDTY
	dex
	bne Ld3cc
	ldx #$18
Ld3cc	
	stx VDTY
	jmp $d140 ; FIXME
	
CTRL_L_KEYBOARD 
	jsr $d111 ; FIXME Initialize VIDEOTEX MINITEL
	jmp $d425 ; FIXME 

CTRL_M_KEYBOARD
	jsr $d759 ; FIXME
	lda #0
	sta VDTX
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
	lda VDTX
	pha
	lda VDTY
	pha
Ld407	
	lda #$20
	jsr send_A_to_video_screen 
	lda VDTX
	beq Ld419
	cmp #$27
	bne Ld407
	lda #$20
	jsr send_A_to_video_screen  
Ld419	
	jsr $d759 ; FIXME
	pla
	sta VDTY
	pla
	sta VDTX
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
	sty RES+1
	ldy VDTX
	lda ($30),y
	bmi Ld77d
	
	and #$40
	beq Ld77d 
	lda $02
	eor #$80
	sta $02
Ld77d	
	ldx #$8
	ldy VDTX
Ld781	
	lda (RES),y
	and #$7f
	ora $02 
	sta (RES),y
	clc
	tya
	adc #$28
	tay
	bcc Ld792 
	inc RES+1
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
	jsr XALLKB_ROUTINE 
	beq Ld812
	ldx $0270
	bpl Ld7f1 
	lda $0271 
	and $01e8,x
	bne Ld807
Ld7f1	
	dey
	lda KBDCOL,y
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
	JSR XKBDAS_ROUTINE ; FIXME
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

XKBDAS_ROUTINE
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
	JSR XECRBU_ROUTINE
	PLA
	LDX #$00
	JSR XECRBU_ROUTINE
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

XALLKB_ROUTINE
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
	jsr XLISBU_ROUTINE ; FIXME
	bcs Ld982 
	sta $279
	ldx #00
	jsr XLISBU_ROUTINE ; FIXME
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
	STA ADKBD
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
	.byt $24
XSONPS_ROUTINE	
	sec
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
	JSR XEPSG_ROUTINE 
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
	
	
XEPSG_ROUTINE	
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
	bmi $dad2 ; FIXME
XLPRBI_ROUTINE	
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
	BIT FLGLPR
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
LDAA5

	TAX         ;   Donn?e dans X                                    I
	LDA FLGLPR   ;   mode RS232 ?                                     I
	AND #$04    ;                                                    I
	BEQ $DAB5   ;   non, parrall?le -------------------------------- I
	JSR $DB2F   ;   oui, on pr?pare la RS232                       I I
	TXA         ;   donn?e dans A                                  I I
	LDX #$18    ;   X=buffer ACIA sortie                           I I
	BNE LDABF  ;  --inconditionnel                                 I I
	LDA FLGTEL   ; I imprimante d?tect?e ? <------------------------- I
	AND #$02   ;  I                                                  I
	BEQ $DAD1   ; I non, on sort                                     I
	TXA         ; I oui, donn?e dans A                               I
	LDX #$24    ; I X=buffer CENTRONICS sortie                       I

LDABF
next906
	BIT FLGLPR	
	BVS next909

	CMP #$7F
	BNE next909

	
	LDA #$20
next909
	PHA	
	JSR XECRBU_ROUTINE ; FIXME
	pla 
	bcs next906

	rts

	
	BCS next908
	LDA FLGLPR
	AND #$04
	BNE Ldae1	
	
	LDA #$82
	STA $030E
next908
	RTS

	
Ldae1
	jmp $db7d ; FIXME

XLPCRL_ROUTINE	
Ldae4
	PHA
	LDA #$0D
	JSR XLPRBI_ROUTINE 
	LDA FLGLPR
	LSR
	BCS Ldaf5
	LDA #$0A
	JSR XLPRBI_ROUTINE  
Ldaf5
	PLA
	RTS
	BMI Ldafe
	LDX #$0C
	JMP XLISBU_ROUTINE 
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
/*
                        GESTION DE LA SORTIE MINITEL                       I
                                                                               I
                                                                               I
Principe:N'?tant gu?re familiaris? avec les modes de fonctionnement de l'ACIA, I
         je vous donne le source mais je serais bien incapable d'expliciter    I
         les modifications d'ACIACR et d'ACIACT, registres de controle et de   I
         commande de l'ACIA 6551.                                              I
                                                                               I
*/
                                        ;                                     I
	BMI $DB3A ;     ouverture-fermeture ---------------------------- I
	TAX       ;     donn?e dans X                                  I I
	BPL $DB26 ;      si <128, on passe ----------------------       I I
	CMP #$C0  ;      sinon, c'est <128+64 ?                 I       I I
	BCS $DB26 ;                                             I       I I
	ORA #$40    ;    oui, on force b7                       I       I I
	PHA                          ;                         I       I I
	LDA #$1B   ;    et on envoie un ESC avant              I       I I
	LDX #$18 ;      la donn?e                              I       I I
	JSR XECRBU_ROUTINE ;                                            I       I I
	PLA       ;                                            I       I I
	PHA       ;     <---------------------------------------       I I
	LDX #$18  ;     on envoie la donn?e                            I I
	JSR XECRBU_ROUTINE ;     dans le BUFFER ACIA sortie                     I I
	PLA                                                        ;   I I
	BCS $DB26 ;     si la donn?e n'a pas ?t? ?crite, on boucle     I I
	LDA $031E  ;    on prend V2IER                                 I I
	AND #$F3  ;     %11110011 force b2 ? 0                         I I
	ORA #$04 ;      et b3 ? 1                                      I I
	STA $031E ;     dans ACIACR                                    I I
	RTS                     ;                                      < I
	BCS LDB53 ;     C=1 on ferme ==================================  I
	LDA $031E ;     ouverture                                      > I
	AND #$02 ;      ACIACR ? 0 sauf b1                             I I
	ORA #$65 ;      %01101001, bits forc?s ? 1                     I I
Ldb43
	STA $031E ;     dans ACIACR <----------------------------------+--
	LDA V2DRA ;     V2DRA                                          I  
	AND #$EF  ;     %11101111 force mode MINITEL                   I  

	STA V2DRA ;                                                    I  
	LDA #$38  ;     %00111000 dans ACIACT                          I  
	STA $031F ;                                                    I  
LDB53
	RTS       ;     et on sort--------------------------------------  

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

	                                                                              
               ;          GESTION DE L'ENTREE RS232                          
LDB5D
	BPL $DAF7    ;  lecture, voir MINITEL (pourquoi pas $DAF9 ?)    FIXME   
	BCS $DB09   ;   C=1, on ferme    FIXME                                 
	LDA $031E   ;   on ouvre                                          
	AND #$0D     ;  on fixe le mode de controle                       
	ORA $5A     ;   de la RS232                                       
	STA $031E                                                        
	LDA V2DRA                                                        
	ORA #$10     ;  %00010000 on force RS232                          
	STA V2DRA                                                        
	LDA $59     ;   et on fixe le mode de transmission                
	STA $031F   ;   dans ACIACR                                       
	RTS                                                              
                           
                                                                                
   ;                      GESTION DE LA SORTIE RS232                         
LDB79
	BPL $DB26     ; ?criture, comme MINITEL       FIXME                     
	BCS $DB53    ;;  pas de fermeture (RTS)     FIXME                        
	LDA $031E    ;  ouverture,on lit ACIACR                            
	AND #$02     ;  isole b1                                          
	ORA #$05     ;  %00000101 force b0 et b2 ? 1                      
	BNE $DB66    ;  inconditionnel        	 FIXME

                                                                               
     ;                 GESTION DES SORTIES EN MODE TEXT                      
                                                                                
;Principe:tellement habituel que ?a en devient monotone... mais bien pratique !  
LDB86
	PHA       ;     on sauve A et P                                   
	PHP                                                              
	LDA #$00    ;   fen?tre 0                                         
	BEQ $DB9C   ;   inconditionnel     FIXME                                
																	
	PHA                                                              
	PHP                                                              
	LDA #$01    ;   fen?tre 1                                         
	BNE $DB9C    ;                                          FIXME           
																	
	PHA                                                              
	PHP                                                              
	LDA #$02     ;  fen?tre 2                                         
	BNE $DB9C                              ;           FIXME                
																	
	PHA                                                              
	PHP                                                              
	LDA #$03      ; fen?tre 3                                         
																	
	STA $28       ; stocke la fen?tre dans SCRNB                      
	PLP          ;  on lit la commande                                
	BPL $DBA4    ;  ?criture -------     FIXME                              
	JMP $DECE    ;  ouverture      I      FIXME                             
	PLA          ;  on lit la donn?e <                                
	STA $29      ;  que l'on sauve                                    
	LDA FLGLPR    ;  ?cho sur imprimante ?                             
	AND #$02                                                         
	BEQ LDBB3     ; non --------------------------------------        
	LDA $29      ;  oui, on envoie le code sur imprimante    I        
	JSR $DA72   ;                               FIXME              I  
LDBB3
	LDA $29      ;  <-----------------------------------------        


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
	
	JSR XCOSCR_ROUTINE 
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

	
TABLE_OF_SHORTCUT_KEYBOARD	
Ldbeb
	.byt $e9
Ldbec
	.byt $dc
LDBED	
	.byt $ea,$dc ; CTRL A tabulation ; FIXME
	.byt $e9,$dc ; FIXME
	.byt $e9,$dc ; FIXME
	.byt $0c,$dd ; FIXME
	.byt $e9,$dc ; FIXME
	.byt $e9,$dc ; FIXME
	.byt $d7,$dd ; FIXME
	.byt $46,$dd
	.byt $91,$dd
	.byt $9c  ; FIXME

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

	JSR $DC69   ;  on le place ? l'?cran                            I
	LDA #$09     ;   on d?place le curseur ? droite                   I
	JSR Ldbb5  ;                                                     I
	LDA #$1B   ;     on envoie un ESC (fin de ESC)                    I
	JSR Ldbb5   ;                                                    I
	JMP $DC46   ;   et on sort                                       I
	LDA $0248,X ;   US, on lit FLGSCR <-------------------------------
	PHA         ;   que l'on sauve                                    
	JSR $DE1E   ;   on ?teint le curseur                              
	PLA         ;   on prend FLGSCR                                   
	PHA                                                              
	LSR         ;   doit-on envoyer Y ou X ?                          
	BCS $DCDC   ;   X ------------------------------------------------
	LDA $29     ;   on lit Y                                         I
	AND #$3F    ;   on vire b4 (protocole US)                        I
	STA SCRY,X ;   et on fixe Y                                     I
	JSR $DE07   ;   on ajuste l'adresse dans la fen?tre              I
	STA $0218,X ;   dans ADSCRL                                      I
	TYA         ;                                                    I
	STA $021C,X  ;  et ADSCRH                                        I
	PLA         ;   on indique prochain code pour X                  I
	ORA #$01    ;                                                    I
	PHA        ;                                                     I
	JMP $DC2B    ;  et on sort                                       I
	LDA $29      ;  on lit X <----------------------------------------
	AND #$3F    ;   on vire b4                                        
	STA SCRX,X ;   dans SCRX                                         
	PLA                                                              
	AND #$FA    ;   on indique fin de US                              
	PHA                                                              
	JMP $DC2B   ;   et on sort                                        
	RTS                                                              	

/*
                                                                                
                       GESTION DES CODES DE CONTROLE                        
                                                                                
                                                                  #A055         
Principe:G?nial... la gestion des codes de controle est surement la partie la   
         plus caract?ristique de l'esprit BROCHIEN (apr?s le BRK bien sur). La  
         gestion est supr?mement optimis?e pour tout les codes, elle est        
         surement le fruit d'une mure reflexion. Chapeau.                       
         En entr?e de chaque routine, A=0, C=1 et la pile contient en son       
         sommet -3, FLGSCR. Le RTS branche en fait en $DC2B, routine g?n?rale   
         de fin de gestion de code de controle.                                 
                                                                                
                                                                                
                              CODE 01 - CTRL A                              
                                                                                
Action:Place le curseur sur une tabulation, colonne multiple de 8.              
  */                                                                              
	LDA SCRX,X ; ->on lit ala colonne                                
	AND #$F8     ;I on la met ? 0                                     
	ADC #$07     ;I on place sur une tabulation 8 (C=1)               
	CMP $022C,X ; I est-on en fin de ligne ?                          
	BEQ $DD09   ; I non                                               
	BCC $DD09  ;  I --------------------------------------------------
	JSR $DD67  ;  I oui, on ram?ne le curseur en d?but de ligne      I
	JSR $DD9D  ;  I et on passe une ligne                            I
	LDX $28     ; I                                                  I
	LDA SCRX,X ; I on prend la colonne                              I
	AND #$07   ;  I est-on sur une tabulation                        I
	BNE $DCEB  ;  --non, on tabule...                                I
	RTS       ;                                                      I
	STA SCRX,X ;   on sauve la colonne <-----------------------------
	RTS         ;   et on sort codes A                               I
                                                                                
;                             CODE 4 - CTRL D                               
LDD0D                                                                                
	ROR          ;  on pr?pare masque %00000010                       
                                                                                
 ;                               CODE 31 - US                                
                              ;on pr?pare masque %00000100                       
LDD0E							  
	ROR                                                              
;                               CODE 27 - ESC                                
                                                                                
 ;                             on pr?pare masque %00001000                       
LDD0F
	ROR                                                              
 ;                             CODE 29 - CTRL ]                              
                                                                                
  ;                            on pr?pare masque %00010000                       
LDD10  
	ROR                                                              
                                                                                
    ;                          CODE 22 - CTRL V                              
LDD11
	ROR           ; on pr?pare masque %00100000                       
                                                                                
     ;                         CODE 16 - CTRL P                              
LDD12
	ROR  ;           on pr?pare masque %01000000                       
                                                                                
          ;                    CODE 17 - CTRL Q                              
LDD13

	ROR    ;        on pr?pare masque %10000000                       
    /*                                                                            
                                                         
                                                                                
 
                                                                                
	*/																			
LDD14
	TAY           ; dans Y                                            
	TSX          ;  on indexe FLGSCR dans la pile                     
	EOR $0103,X  ;  on inverse le bit correspondant au code (bascule) 
	STA $0103,X ;   et on replace                                     
	STA RES      ;  et dans $00                                       
	TYA                                                              
	AND #$10    ;   mode 38/40 colonne ?                              
	BNE $DD24   ;   oui ----------------------------------------------
	RTS         ;   non on sort                                      I
	LDX $28     ;   on prend le num?ro de fen?tre <-------------------
	AND RES      ;  mode monochrome (ou 40 colonnes) ?                
	BEQ $DD3C    ;  oui ----------------------------------------------
	INC SCRDX,X  ;  non, on interdit la premi?re colonne             I
	INC SCRDX,X  ;  et la deuxi?me                                   I
	LDA SCRX,X  ;  est-on dans une colonne                          I
	CMP SCRDX,X  ;  interdite ?                                      I
	BCS $DD3B  ;---non                                               I
	JMP $DD67  ;I  oui,on en sort                                    I
	RTS   ;  <---                                                    I
	DEC SCRDX,X ;   on autorise colonne 0 et 1 <----------------------
	DEC SCRDX,X                                                      
	RTS     	
LDD43	
	DEC SCRX,X  ;  on ram?ne le curseur un cran ? gauche  <----------
	RTS  ;                                                           I

 ;                             CODE 8 - CTRL H                              I
 ;                                                                              I
;Action:d?place le curseur vers la gauche                                       I
LDD47  
	LDA SCRX,X   ; est-on d?ja au d?but de la fen?tre ?             I
	CMP SCRDX,X  ;                                                   I
	BNE LDD43    ;  non, on ram?ne ? gauche --------------------------
	LDA $022C,X  ;  oui, on se place ? la fin de la fen?tre           
	STA SCRX,X                                                      


	
;                              CODE 11 - CTRL K                               
                                                                                
;Action:d?place le curseur vers le haut                                          
LDD55                                                                                
	LDA SCRY,X ;   et si on est pas                                  
	CMP SCRDY,X    ;au sommet de la fen?tre,                          
	BNE LDD6E   ;   on remonte d'une ligne ---------------------------
	LDA SCRDY,X ;   X et Y contiennent le d?but et la                I
	LDY SCRFY,X  ;  fin de la fen?tre X                              I
	TAX          ;                                                   I
	JSR XSCROB_ROUTINE    ;  on scrolle l'?cran vers le bas ligne X ? Y       I
	LDA SCRDX,X  ;  on place d?but de la fen?tre dans X              I
	STA SCRX,X  ;                                                   I
	RTS          ;                                                   I
LDD6E
	DEC SCRY,X   ; on remontre le curseur <--------------------------
	JMP $DE07    ;  et on ajuste ADSCR     	
	
	
	
;                              CODE 14 - CTRL N                              
                                                                                
;Action:efface la ligne courante                                                 
LDD74                                                                              
	LDY SCRDX,X ;    on prend la premi?re colonne de la fenetre        
	JMP $DD7D   ;   et on efface ce qui suit (BPL aurait ?t? mieux...)
                       
                                                                          
 ;                             CODE 24 - CTRL X                              
                                                                                
;Action:efface la fin de la ligne courante                                       
LDD7A                                                                                
	LDY SCRX,X  ;  on prend la colonne du curseur                    
	LDA $022C,X  ;  et la derni?re colonne de la fen?tre              
	STA $29      ;  dans $29                                          
	LDA #$20     ;  on envoie un espace                               
	STA (ADSCR),Y                                                      
	INY           ; jusqu'? la fin de la ligne                        
	CPY $29                                                          
	BCC $DD84                                                        
	STA (ADSCR),Y   ; et ? la derni?re position aussi                   
	RTS           ; (INC $29 avant la boucle aurait ?t? mieux !)      
																	
	INC SCRX,X                                                      
	RTS                                                              
	
	
	
 ;                             CODE 9 - CTRL I                               
                                                                                
;Action:d?place le curseur ? droite                                              
LDD92                                                                              
	LDA SCRX,X  ;  on lit la colonne du curseur                      
	CMP $022C,X   ; derni?re colonne ?                                
	BNE $DD8E     ; non, on d?place le curseur                        
	JSR $DD67   ;   oui, on revient ? la premi?re colonne     

	

                      ;        CODE 10 - CTRL J                              
                                                                                
;Action:d?place le curseur vers la droite                                        
LDD9D                                                                               
	LDA SCRY,X  ;  on est en bas de la fen?tre ?                     
	CMP SCRFY,X  ;                                                    
	BNE LDDB2    ;  non ----------------------------------------------
	LDA SCRDY,X  ;  oui, X et Y contiennent d?but et fin de fen?tre  I
	LDY SCRFY,X  ;                                                   I
	TAX          ;                                                   I
	JSR $DE54    ;  on scrolle la fen?tre                            I
	JMP $DD67     ; on revient en d?but de ligne                     I
LDDB2	
	INC SCRY,X  ;  on incr?mente la ligne <-------------------------I 
	JMP $DE07    ;  et on ajuste ADSCR                      
     	
	

     ;                         CODE 12 - CTRL L                              
                                                                                
;Action:Efface la fen?tre                                                        
LDDB8                                                                               
	JSR LDDFB    ;  on remet le curseur en haut de la fen?tre         
	JSR $DD74    ;  on efface la ligne courante                        FIXME
	LDA SCRY,X   ; on est ? la fin de la fen?tre ?                   
	CMP SCRFY,X  ;                                                     
	BEQ LDDFB    ;  oui, on sort en repla?ant le curseur en haut     
	JSR $DD9D    ;  non, on d?place le curseur vers le bas             FIXME
	JMP $DDBB     ; et on boucle  (Et BPL, non ?!?!)                   FIXME
		
	
	
                            ;  CODE 19 - CTRL S                              
LDDCC                                                                             
	JMP XHCSCR_ROUTINE    ;  on ex?cute un HCOPY    FIXME    	


	; CODE 18 - CTRL R   	
;Action:bascule l'?cho imprimante du clavier                                     
LDDCF                                                                                
	LDA #$02     ;  on inverse b1                                     
	EOR FLGLPR    ;  de FLGLPR                                         
	STA FLGLPR                                                        
	RTS      	
	
XOUPS_ROUTINE
LDDD8	
/*                             CODE 7 - CTRL G                               

Action:?met un OUPS                                                             
 */
	LDX #<XOUPS_DATA    ;   on indexe les 14 donn?es du OUPS                  
	LDY #>XOUPS_DATA                                                         
	JSR $D9E7   ;   et on envoie au PSG                               
	LDY #$60    ;   I                                                 
	LDX #$00    ;   I                                                
LDDE3
	DEX        ;    I D?lai d'une seconde                             
	BNE LDDE3    ;  I                                                 
	DEY          ;  I                                                 
	BNE LDDE3    ;  I                                                 
	LDA #$07     ;  un JMP $DA4F suffisait ...                        
	LDX #$3F                                                         
	JMP $DA1A
XOUPS_DATA
LDDF0                                                                               
	.byt $46,00,00,00,00,00;  p?riode 1,12 ms, fr?quence 880 Hz (LA 4) 
LDDF6
	.byt  00,$3E,$0F,00,00  ;  canal 1, volume 15 musical  
/*

                           INITIALISE UNE FENETRE                           
                                                                                
Action:on place le curseur en (0,0) et on calcule son adresse                   
  */ 
LDDFB
	LDA SCRDX,X  ;  on prend la premi?re colonne                      
	STA SCRX,X  ;  dans SCRX                                         
	LDA SCRDY,X  ;  la premi?re ligne dans                            
	STA $0224,X  ;  SCRY                                              
	LDA $0224,X  ;  et on calcule l'adresse                           
	JSR LDE12    ;  de la ligne                                       
	STA $26      ;  dans ADSCR                                        
	STY $27      ;                                                    
	RTS    	

/*
	CALCULE L'ADRESSE DE LA LIGNE A                       
                                                                                
                                                                                
Action:En entr?e, A contient le num?ro de la ligne et en sortie, RES contient   
       l'adresse ? l'?cran de cette ligne.                                      
  */                                                                              
LDE12
	JSR $CE69    ;  RES=A*40   FIXME                                        
	LDA $0238,X  ;  AY=adresse de la fen?tre                          
	LDY $023C,X                                                      
	JMP $CE89     ; on calcule dans RES l'adresse de la ligne   FIXME
	
XCOSCR_ROUTINE
lde1e
	CLC
	.byt $24
XCSSCR_ROUTINE	
LDE20	
	sec
	PHP
	ASL $0248,X
	PLP
	ROR $0248,X
	bmi lde53
	LDA #$80
	AND $0248,X
	AND #$80
	EOR $024C,X
	LDY SCRX,X
	STA (ADSCR),Y
	PHA
	LDA $0248,X
	AND #$02
	beq lde52
	LDA SCRY,X
	CMP SCRFY,X
	BEQ lde52
	TYA
	ADC #$28
	TAY
	PLA
	STA (ADSCR),Y
	RTS

lde52
	pla
lde53	
	rts
Lde54
XSCROH_ROUTINE
  /*                                                                             
                      SCROLLE UNE FENETRE VERS LE BAS                       
                                                                                
                                                                                
Action:scrolle vers le bas de la ligne X ? la ligne Y la fen?tre courante.      
                                                                                
    */                                                                            
	LDA #$00     ;  on prend $0028, soit 40                           
	STA $07                                                          
	LDA #$28                                                         
	BNE LDE62    ;  inconditionnel                                    
 /*                                                                               
                                                                              
                      SCROLLE UNE FENETRE VERS LE HAUT                      
                                                                                
                                                                                
Action:scrolle vers le haut de la ligne X ? la ligne Y la fen?tre courante.     
                                                                                
                                                                                */

																				
LDE5C																				
XSCROB_ROUTINE
	LDA #$FF   ;    on prend $FFD8, soit -40 en compl?ment ? 2        
	STA $07                                                          
	LDA #$D8                                                         
LDE62																	
	STA $06     ;   $06-07 contiennent le d?placement                 
	STX RES    ;    on met la ligne de d?part en RES                  
	TYA                                                              
	SEC                                                              
	SBC RES     ;   on calcule le nombre de lignes                    
	PHA        ;    on sauve le nombre de lignes                      
	TXA        ;    ligne de d?but dans A                             
	BIT $06                                                          
	BPL LDE71  ;    d?placement n?gatif ?                             
	TYA        ;    oui, ligne de fin dans A
LDE71	
	LDX $28                                                          
	JSR $DE12   ;   on calcule l'adresse de la ligne   FIXME                
	CLC                                                              
	ADC SCRDX,X ;   l'adresse exacte de la ligne dans la fen?tre      
	BCC LDE7D                                                       
	INY
LDE7D	
	STA $08      ;  est dans $08-09                                   
	STY $09                                                          
	CLC           ; on ajoute le d?placement                          
	ADC $06                                                          
	STA $04                                                          
	TYA                                                              
	ADC $07                                                          
	STA $05     ;   dans $04-05                                       
	PLA         ;   on sort le nombre de lignes                       
	STA RES     ;   dans RES                                          
	BEQ LDEC4   ;   si nul on fait n'importe quoi ! on devrait sortir!
	BMI LDECD    ;  si n?gatif, on sort ------------------------------
	SEC          ;  on calcule                                       I
	LDX $28     ;                                                    I
	LDA $022C,X   ; la largeur de la fen?tre                         I
	SBC SCRDX,X  ;                                                   I
	STA RES+1      ;  dans RES+1                                       I
LDE9D
	LDY RES+1 
LDE9F ;                                                  I
	LDA ($04),Y ;   on transf?re une ligne                           I
	STA ($08),Y ;                                                    I
	DEY         ;                                                    I
	BPL LDE9F    ;                                                    I
	CLC        ;                                                     I
	LDA $04     ;   on ajoute le d?placement                         I
	ADC $06     ;   ? l'adresse de base                              I
	STA $04     ;                                                    I
	LDA $05     ;                                                    I
	ADC $07     ;                                                    I
	STA $05      ;                                                   I
	CLC          ;                                                   I
	LDA $08     ;   et ? l'adresse d'arriv?e                         I
	ADC $06     ;                                                    I
	STA $08     ;                                                    I
	LDA $09     ;                                                    I
	ADC $07      ;                                                   I
	STA $09      ;                                                   I
	DEC RES      ;  on d?compte une ligne de faite                   I
	BNE LDE9D    ;  et on fait toutes les lignes                     I
LDEC4
	LDY RES+1      ;  on remplit la derni?re ligne                     I
	LDA #$20     ;                                                   I
LDEC8
	STA ($08),Y  ;  avec de espaces                                  I
	DEY          ;                                                   I
	BPL LDEC8    ;                                                   I
LDECD
	RTS          ;  <-------------------------------------------------
 
/*
                                     ???                                     
                                                                                
Action:inconnue... ne semble pas ?tre appel?e et utilise des variables          
       IRQ dont on ne sait rien.                                                
 */                                                                               
	BCC LDED7    ;  si C=0 on passe ------------                      
	LDX $28      ;                             I                      
	JSR $DE1E    ;  on ?teint le curseur       I                      
	PLA         ;   et on sort A de la pile    I                      
	RTS          ;                             I    
LDED7
	LDA #$01     ;  on met 1 en $216 <----------                      
	STA $0216                                                        
	LDA #$80      ; on force b7 ? 1 dans $217                         
	STA $0217                                                        
	PLA           ; on sort A                                         
	RTS           ; et on sort    

ldee3	
data_to_define_2
	; text mode  Text mode bytes it will  fill SCRTXT
data_text_window
LDEE3	
	.byt $00,$27 ; 0 to 39
	.byt $01,$1b ; 1 to 27
	.byt $80,$bb ; adress of text mode (first byte)
	; hires mode it will  fill SCRHIR
data_hires_window
LDEE9
	.byt $00,$27 ; 0 to 39
LDEEB	
	.byt $00,$02 ; 0 to 2
LDEED	
	.byt $68,$bf ; last bytes for text mode
	
	
data_trace_window	
LDEEF
	.byt $00,$27 ; 0 to 39
LDEF1	
	.byt $1a,$1b ; 26 to 27
LDEF3	
	.byt $80,$bb ; adress
data_videotex_window ; minitel
LDEF5	
	.byt $00,$27 ; 0 to 39
LDEF7 	
	.byt $01,$18 ; 1 to 24
adress_text_mode	
	.byt $80,$bb
XSCRSE_ROUTINE
LDEFB	
	sec
	.byt $24
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
                                                                                
	JSR Ldf99    ;  on lit la valeur souris                         
	AND #$1B    ;   on isole les directions                           
	STA $58     ;   dans $58                                          
	CMP #$1B    ;   la souris bouge ?                                 
	BNE LE095   ;   non ---------------------------------------------- 
	DEC $02A4   ;   on d?place ?                                     I
	BNE Le084   ;   non, on sort.                                    I 
LE095	
	LDA $02A5    ;  on place vitesse d?placement dans  <--------------
	STA $02A4    ;  $2A4                                              
	LDA $58     ;   on lit le code                                    
	CMP #$1B    ;   souris fixe ?                                     
	BEQ LE0B5    ;  oui ----------------------------------------------
	AND #$1B     ;  non, on isole les valeurs direction              I
	EOR $028E   ;   et on retourne les bits de JCDVAL                I
	AND #$1B   ;    en isolant les bits direction                    I
	BNE LE0B5  ;    ce ne sont pas les m?mes exactement -------------O 
	DEC $0292  ;    on r?p?te ?                                      I
	BNE LE0E0  ;    non                                              I 
	LDX $0299 ;     oui, on met le diviseur r?p?tition               I
	JMP LE0BB ;  ---dans le compteur                                 I 
LE0B5
	JSR Ldf99  ; I  on lit la souris <-------------------------------- 
	LDX $029A ;  I  on place le compteur avant r?p?tition   
LE0BB
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
	BCS LE0DC     ;                                              
	LDA JCKTAB,X   ; et on envoie les valeurs ASCII dans le buffer     
	JSR Le19d      ;                                                    
LE0DC
	PLA                                                              
	DEX                                                              
	BPL Le0d2
LE0E0
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
	JSR XECRBU_ROUTINE 
	LDA #$08
	PLP
	BCS Le1af
	LDA #$20
Le1af
	LDX #$00
	JSR XECRBU_ROUTINE 
	LDX $58   ; CORRECTME
	RTS
Le1b7	
	sec
	rts
; hard copy of text window	
XHCSCR_ROUTINE
LE1B9
	LDX $28
	LDA SCRX,X
	PHA
	LDA SCRY,X
	PHA

	LDA #$1E
	JSR Ldbb5 
	JSR XLPCRL_ROUTINE
Le1cb
	LDX $28
	LDY SCRX,X
	LDA (ADSCR),Y
	CMP #$20
	BCS Le1d8
	LDA #$20
Le1d8
	JSR XLPRBI_ROUTINE  
	LDA SCRX,X
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
	JSR XLPCRL_ROUTINE  
	LDX $28
	LDA SCRY,X
	CMP SCRFY,X
	BNE Le1e3  
	LDA #$1F
	JSR Ldbb5 
	PLA
	ORA #$40
	JSR Ldbb5 
	PLA
	ORA #$40
	JMP Ldbb5
LE209
XHCVDT_ROUTINE
	JSR XLPCRL_ROUTINE 
	LDA $0288
	PHA
	LDA #$28
	STA $0288
	LDA #$1E
	JSR Ld178  
Le21a
	LDY VDTX
	LDA ($30),Y
	BMI Le226  
	LDA ($2E),Y
	CMP #$20
	BCS Le228 
Le226
	LDA #$20
Le228
	JSR XLPRBI_ROUTINE  
	LDA #$09
	JSR Ld178  
	LDA VDTX
	BNE Le21a 
	LDY VDTY
	DEY
	BNE Le21a 
	JSR XLPCRL_ROUTINE  
	PLA
	STA $0286
	rts
.)
	
Le241
data_for_hard_copy
	.byt $18,$33,$1b,$0a,$0d,$00,$f0,$4b,$1b,$0d,$0a,$40,$1b,$0a,$0a

XHCHRS_ROUTINE
LE250
execute_hard_copy_hires
	jmp ($0250)
LE253	
hard_copy_hires
	LDX #$05
	LDA FLGLPR 
	PHA
	ORA #$40
	STA FLGLPR
	LDA $E240,X ; fixme
	JSR $DA72 ; fixme
	DEX
	BNE $E25E ; fixme
	STX TR0
	LDX #$06
	LDA $E245,X ; fixme
	JSR $DA72 ; fixme
	DEX
	BNE $E26B ; fixme
	STX $0D
	LDA #$05
	STA $0E
	LDA TR0
	ASL
	ASL
	ASL
	JSR XMUL40_ROUTINE 
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
	INC TR0
	LDA TR0
	CMP #$19
	BNE $E269 ; fixme
	LDX #$04
	LDA $E24B,X ; fixme
	JSR $DA72 ; fixme
	DEX
	BNE $E2D0 ; fixme
	PLA
	STA FLGLPR
	RTS
Le2de
put_cursor_on_last_char_of_the_line
	LDY $022C,X
	.byt $24
	dey
	LDA (RES),Y
	CMP #$20
	BNE $E2F0 ; FIXME
	TYA
	CMP SCRDX,X
	BNE $E2E2 ; FIXME
	RTS

test_if_prompt_is_on_beginning_of_the_line

	cmp #$7f
	bne Le2f8
	tya
	cmp SCRDX,x
Le2f8	
	rts
Le2f9
	LDY SCRDX,X
	LDA (RES),Y
	CMP #$7F
	RTS
LE301
Le2e6
	LDX $28
	LDA SCRY,X
	STA $61
Le2ed	
	LDA $61
	JSR $DE12 ; FIXME
	JSR $E2F9 ; FIXME

	beq Le302
	lda $61
	CMP SCRDY,X
	
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
	LDA SCRY,X
	STA $63
	JSR $DE12 ; FIXME
	JSR put_cursor_on_last_char_of_the_line ; FIXME
Le32f	
	STY $62
	BEQ Le34e 
	LDA $63
	CMP SCRFY,X
	BEQ Le34d 
	INC $63
	LDA $63
	JSR $DE12 ; FIXME
	JSR $E2F9 ; FIXME
	BEQ Le34b 
	JSR put_cursor_on_last_char_of_the_line ; FIXME
	BNE Le32f 
Le34b	
	DEC $63
Le34d	
	RTS
Le34e	
	rts



	JSR LE301 
	JMP $E361 ; FIXME
send_the_end_of_line_in_bufedt	
	
	LDX $28
	LDA SCRX,X
	STA $60
	LDA SCRY,X
	STA $61
	JSR Le322 
	LDA $61
	STA $65
	CMP $63
	BNE Le378
	LDA $62
	CMP $60
	BCS Le378
	LDA #$00
	STA BUFEDT
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
	LDY SCRDX,X
Le390	
	LDA (RES),Y
	CMP #$20
	BCS Le398
	ORA #$80
Le398	
	LDX $64
	BIT $66
	BPL Le3a4
	LDA #$20
	STA (RES),Y
	BNE Le3b1
Le3a4
	STA BUFEDT,X
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
	STA BUFEDT,X
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
	STY RES+1
	LDX $28
	LDY SCRX,X

Le3e3
	LDX $64
	LDA BUFEDT,X
	BEQ Le41c 
	LDA #$20
	BIT $66
	BMI Le3fb
	LDA BUFEDT,X
	BPL Le3fb
	CMP #$A0
	BCS Le3fb
	AND #$1F
Le3fb	
	STA (RES),Y

	BIT FLGTEL ; Minitel ?
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
	JSR XADRES_ROUTINE ; FIXME
	LDY SCRDX,X
Le418	
	INC $64
	BNE Le3e3
Le41c	
	BIT FLGTEL ; Minitel ?
	BVC Le42a 
	LDX SCRX
	LDY SCRY

	JSR $E62A ; FIXME
Le42a		
	LDY SCRX
	LDA (ADSCR),Y
	LDX $28
	STA $024C,X
	RTS

XEDT_ROUTINE	
edit_line_in_bufedt
	STA $67
	TXA
	PHA
	TYA
	BPL Le446
	JSR LE301 ; FIXME
	LDX $60
	LDY $61
	JSR $E62A  ; FIXME
Le446	
	LDA #$0D
	JSR $E648  ; FIXME
	JSR XECRPR_ROUTINE  
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
	LDA BUFEDT,X
	CMP #$20
	BEQ Le496 
	TXA
	PHA
	LDY #$00
	.byt $2c
Le4a3	
	inx
	iny

	LDA BUFEDT,X
	STA BUFEDT,Y
	BNE Le4a3 
	LDA #$90
	LDY #$05
	JSR XDECAY_ROUTINE  
	STA RES
	STY RES+1
	PLA
	TAY
	PLA
	RTS


Le4bc



	CMP #$03
	BEQ Le479
	CMP #$0E
	BNE Le4d1
	JSR LE301 ; FIXME
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
	JMP Le45a ; FIXME
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
	LDA BUFEDT
	BNE Le511
	LDA #$20
	JSR $E648 ; FIXME
	LDA #$08
	JSR $E648 ; FIXME
	JMP Le45a ; FIXME
Le511	
	LDX #$01
Le513	
	LDA BUFEDT,X
	BEQ Le51e
	STA $058F,X
	INX
	BNE Le513
Le51e	
	LDA #$20
	STA $058F,X
	CLC
	JSR $E3D0 ; FIXME
	JMP Le45a ; FIXME
Le52a	
	CMP #$20
	BCC Le534
	JSR XEDTIN_ROUTINE ; FIXME
	JMP Le45a ; FIXME
Le534	
	JMP $E5B9 ; FIXME
manage_normal_char

XEDTIN_ROUTINE
	TAY
	TXA
	PHA
	TYA
	PHA
	JSR $E355 ; FIXME
	LDA $62
	LDY BUFEDT
	BNE  Le548
	LDA $60
Le548	
	LDX $28
	CMP $022C,X
	BNE Le5ae
	LDA $63
	CMP SCRFY,X
	BEQ Le5ae
	ADC #$01
	JSR $DE12 ; FIXME
	JSR $E2F9 ; FIXME
	BNE Le5ae
	LDY SCRFY,X
	LDX $63
	INX
	JSR XSCROB_ROUTINE ; FIXME
	BIT FLGTEL ; Minitel ?
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
	JSR XECRPR_ROUTINE ; FIXME
	JMP $E597 ; FIXME
Le58f	
	JSR $E656 ; FIXME
	LDA #$09
	JSR Ldbb5 ; FIXME
	LDA SCRY
	CMP SCRFY,X
	BNE Le580
	LDA SCRX
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
	JMP Le45a ; FIXME
Le5cb	
	JSR LE301 
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
	JSR Le322 ; FIXME
	LDX $62
	LDY $63
	PLA
	JSR $E62A  ; FIXME
	JMP Le45a  
Le5ee	
	CMP #$0A
	BNE Le604
	LDX $28
	LDA SCRY,X
	CMP SCRFY,X
	BNE Le615
	LDA #$0A
	.byt $2c
Le5ff	
	lda #$0b
	
	JMP Le479 
Le604	
	CMP #$0B
	BNE Le617
	LDX $28
	LDA SCRY,X
	CMP SCRDY,X
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
	JMP Le45a
Le624	
	JSR Le648 
	JMP Le45a 




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
	JSR Ldbb5        ; FIXME                                                
	BIT FLGTEL     ; mode minitel ?                                    
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
	BIT FLGTEL    ;  mode minitel ?                                    
	BVC $E650    ;  non ---------------------------------------------- FIXME
	JSR $E656    ;  oui, on envoie le code au minitel                I FIXME
	BIT $E650    ;  V=0 et N=0 pour ?criture <------------------------ FIXME
	JMP $DB86    ;  dans la fen?tre 0                                 
                                                                                
/*
                 ENVOIE UN CODE AU BUFFER SERIE SORTIE                    
  */                                                                              
	STA TR0    ;    on sauve le code <--------------------------------
	TYA        ;    on sauve Y                                       I
	PHA        ;                                                     I
	TXA         ;   et X                                             I
	PHA         ;                                                    I
	LDX #$18   ;    on indexe buffer ACIA sortie (minitel sortie)    I
	LDA TR0     ;   on envoie le code                                I
	JSR XECRBU_ROUTINE   ;                                                    I FIXME
	PLA         ;   on restaure les registres                        I
	TAX        ;                                                     I
	PLA        ;                                                     I
	TAY         ;                                                    I
	LDA TR0     ;                                                    I
	BCS $E656   ;   si l'envoi s'est mal pass?, on recommence -------- FIXME
	RTS                     
	/*																			
                             AFFICHE LE PROMPT                              
      */  
Le66c
XECRPR_ROUTINE 
	BIT FLGTEL  ;    mode minitel ?                                    
	BVC $E67B   ;   non ---------------------------------------------- FIXME
	LDA #$19   ;    on envoie SS2 2/E au minitel                     I
	JSR $E656  ;                            FIXME                         I
	LDA #$2E   ;    donc on affiche la fl?che ->                     I
	JSR $E656  ;                       FIXME                              I
	LDA #$7F   ;    on affiche un prompt <----------------------------
	JMP Ldbb5   ;   ? l'?cran          FIXME     
																				
/*

                   CHERCHE UNE LIGNE D'APRES SON NUMERO                    
                                                                                
                                                                                
Action:Recherche la ligne num?ro RES ? partir de l'adresse SCEDEB.              
       Une ligne de programme est compos?e de l'ent?te de 3 octets suivants:    
       1er octet    :longeur de la ligne, ou 0 si derni?re ligne                
       2 et 3e octet:num?ro de la ligne.                                        
       En sortie, C=1 si la ligne a ?t? trouv?e (adresse dans RESB), 0 sinon.   
                                                                                
*/																				
XSCELG_ROUTINE
Le680
	LDA $5C   ;     AX=adresse de base de recherche                   
	LDX $5D   ;                                                       
	STX $03   ;     dans RESB                                         
	STA RESB   ;  -->                                                  
	LDY #$00   ; I                                                    
	LDA (RESB),Y ;I  on lit la longeur de la ligne                     
	BEQ $E6AE   ;I  0, on sort --------------------------------------- FIXME
	TAX         ;I  on sauve la longueur dans X                      I
	LDY #$02    ;I  on lit le num?ro de la ligne                     I
	LDA RES+1     ;I                                                   I
	CMP (RESB),Y ;I  poids fort lu ?gal au demand? ?                  I
	BCC $E6AE   ;I  sup?rieur, on sort ------------------------------O FIXME 
	BEQ $E69B   ;I  ?gal, on continue le test                        I FIXME
	BCS $E6A4   ;I  inf?rieur, on passe --------------------------   I FIXME
	DEY         ;I  on lit le poids faible                       I   I
	LDA RES     ;I                                               I   I
	CMP (RESB),Y ;I  poids faible lu ?gal au demand? ?            I   I
	BCC $E6AE   ;I  sup?rieur, on sort --------------------------+---O FIXME
	BEQ $E6AF   ;I  ?gal, on sort avec C=1                       I   I FIXME
	CLC         ;I  <---------------------------------------------   I
	TXA         ;I                                                   I
	ADC RESB     ;I  on passe la ligne                                I
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
XINSER_ROUTINE                                                                     
Le6b0
	STA $0E     ;   sauve la longueur de la ligne                     
	LDA #$00    ;   met 0 dans TR3-4                                  
	STA $0F                                                          
	STA $10                                                          
	JSR XSCELG_ROUTINE   ;   cherche le num?ro de la ligne ? ins?rer        FIXME   
	BCC LE6E7   ;   la ligne n'existe pas ---------------------------- 
	STX $0F     ;   on sauve la longueur de la ligne trouv?e         I
	LDA $5E     ;   on met SCEFIN                                    I
	LDY $5F     ;                                                    I
	STA $06     ;   dans DECFIN                                      I
	STY $07     ;                                                    I
	LDA RESB     ;   adresse de la ligne trouv?e                      I
	LDY $03     ;                                                    I
	STA $08     ;   dans DECCIB                                      I
	STY $09     ;                                                    I

	CLC         ;                                                    I
	TXA        ;                                                     I
	ADC RESB     ;                                                    I
	BCC $E6D6  ;    et l'adresse de la fin de la ligne               I FIXME
	INY         ;                                                    I
	STA $04     ;   dans DECDEB                                      I
	STY $05    ;                                                     I
	JSR XDECAL_ROUTINE   ;   et on ram?ne la fin du listing (efface la ligne) I FIXME
	LDA #$FF   ;    on met -1                                        I
	STA $10     ;   dans TR4                                         I
	EOR $0F    ;    on compl?mente TR3 ? 2                           I
	STA $0F     ;   donc on remet dans TR3-4                         I
	INC $0F     ;   l'oppos? de TR3-4                                I
LE6E7
	LDA $0E      ;  on prend la longueur ? ins?rer <------------------
	BEQ $E738  ;    c'est 0, on devait effacer la ligne -------------- FIXME
	LDA $5E     ;   on prend la fin du listing                       I
	LDY $5F    ;                                                     I
	STA $06    ;    dans DECFIN                                      I
	STY $07    ;                                                     I
	LDA RESB    ;    on prend l'adresse de la ligne                   I
	LDY $03    ;                                                     I
	STA $04    ;    dans DECDEB                                      I
	STY $05    ;                                                     I
	CLC        ;            #A4FF                                    I
	LDA $0E    ;    on ajoute 3 ? la longueur (ent?te de ligne)      I
	ADC #$03   ;                                                     I
	PHA         ;   dans la pile                                     I
	ADC RESB     ;   on ajoute la longueur                            I
	BCC $E706   ;   ? DECDEB                                         I FIXME
	INY         ;                                                    I
	STA $08     ;   dans DECCIB                                      I
	STY $09      ;                                                   I
	JSR XDECAL_ROUTINE    ;  et on lib?re la place pour la ligne              I FIXME
	CLC         ;                                                    I
	PLA        ;                                                     I
	PHA         ;   on prend la longueur                             I
	ADC $0F    ;    on calcule longueur nouvelle ligne               I
	STA $0F   ;     - longueur ligne pr?c?dente                      I
	BCC $E718 ;                                                      I FIXME
	INC $10   ;     dans TR3-4 (compl?ment ? 2)                      I
	LDY #$00  ;     on ?crit la longueur de la ligne                 I
	PLA        ;                                                     I
	STA (RESB),Y ;                                                    I
	INY         ;                                                    I
	LDA RES    ;                                                     I
	STA (RESB),Y ;   le poids faible du num?ro de ligne               I
	INY         ;                                                    I
	LDA RES+1      ;                                                   I
	STA (RESB),Y  ;  le poids fort du num?ro de ligne                 I
	LDX #$00     ;                                                   I
	INY          ;                                                   I
	LDA (TR0,X) ;   et le contenu de la ligne                        I
	STA (RESB),Y  ;  ? la suite                                       I
	INC TR0     ;                                                    I
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
Le748
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
XDECAY_ROUTINE 
Le749                                                                                
	STA RES     ;   on sauve l'adresse du nombre                      
	STY RES+1    ;    dans RES                                          
	LDY #$00   ;    et on met RESB ? 0                                
	STY RESB                                                          
	STY $03                                                          
	LDA (RES),Y ;   on lit le code <------------------------------    
	CMP #$30    ;   inf?rieur ? 0 ?                              I    
	BCC $E785  ;    oui -----------------------------------------+---- FIXME
	CMP #$3A    ;   sup?rieur ? 9 ?                              I   I
	BCS $E785   ;   oui -----------------------------------------+---O FIXME
	AND #$0F    ;   on isole le chiffre                          I   I
	PHA        ;    dans la pille                                I   I
	ASL RESB    ;    RESB*2                                       I   I
	ROL $03     ;                                                I   I
	LDA RESB    ;    AX=RESB*2                                    I   I
	LDX $03    ;                                                 I   I
	ASL RESB    ;    *4                                           I   I
	ROL $03    ;                                                 I   I
	ASL RESB    ;    *8                                           I   I
	ROL $03   ;                                                  I   I
	ADC RESB    ;    +RESB*2                                      I   I
	STA RESB    ;                                                 I   I
	TXA        ;                                                 I   I
	ADC $03    ;                                                 I   I
	STA $03     ;   = RESB*10                                    I   I
	PLA         ;   plus chiffre lu                              I   I
	ADC RESB     ;                                                I   I
	STA RESB     ;                                                I   I
	BCC $E782  ;                                                 I   IFIXME
	INC $03    ;                                                 I   I
	INY       ;     on ajoute un chiffre lu                      I   I
	BNE $E753 ;     et on recommence  ----------------------------   IFIXME
	TYA       ;     nombre de chiffres lus <--------------------------
	TAX       ;     dans X                                            
	LDA RESB   ;     nombre dans AY et RESB                            
	LDY $03    ;                                                      
	RTS
	
data_for_hires_display
Le78c
	.byt $20,$10,$08,$04
	.byt $02,$01

	
XHRSSE_ROUTINE	
LE792
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
XHRSCB_ROUTINE
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
XHRSCH_ROUTINE
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
XHRSCD_ROUTINE  
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
XHRSCG_ROUTINE
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
	JSR XMUL40_ROUTINE  ;    on calcule 40*ligne               FIXME                 
	STA $4B    ;                                           
	CLC                                                              
	TYA                                                              
	ADC #$A0    ;   et on ajoute $A000, ?cran HIRES                   
	STA $4C    ;    dans ADHRS                                        
	STX RES    ;    on met la colonne dans RES                        
	LDA #$06   ;    A=6                                               
	LDY #$00   ;    et Y=0  (dans RES+1)                              
	STY RES+1   ;     AY=6 et RES=colonne                               
	JSR $CEDC ;     on divise la colonne par 6       FIXME                  
	LDA RES   ;     on sauve colonne/6 dans HSRX40                    
	STA $49  ;                                                        
	LDA RESB  ;      et le reste dans HRSX6                            
	STA $4A  ;                                                        
	RTS      ;      I
 /*                                                                               
                                                                               
                       TRACE UN RECTANGLE EN RELATIF                        
                                                                                
                                                                                
Principe:On calcule les coordonn?es absolues des 4 coins et on trace en absolu. 
         Pas tr?s optimis? en temps tout cela, il aurait ?t? plus simple de     
         de tracer directement en relatif !!!                                   
         Le rectangle est trac? comme ABOX avec les param?tres dans HRSx.       
*/
XBOX_ROUTINE
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
XABOX_ROUTINE 
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
	JSR XDRAWA_ROUTINE  ;   on trace le trait en absolu                      I FIXME
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
XDRAWA_ROUTINE
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
	BCS XDRAWR_ROUTINE   ;   et si DY n?gatif, on met signe -1                 FIXME
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
XDRAWR_ROUTINE
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
	JSR XHRSCG_ROUTINE ; I   dX<0, on d?place le curseur ? gauche     I  I    I FIXME
	JMP $E8CD ; I---                                         I  I    I FIXME 
	JSR XHRSCD_ROUTINE ; II  on on d?place le curseur ? droite <-------  I    I FIXME
	CLC       ; I-->a-t-on parcouru une valeur de la tangente   I    I
	LDA RES   ; I                                               I    I
	ADC RESB   ; I   on stocke le r?sultat dans RESB              I    I
	STA RESB   ; I                                               I    I
	BCC $E8E3  ;I   non, on continue -------------------------  I    I FIXME
	BIT $50   ; I   oui, dY<0 ?                              I  I    I
	BMI $E8E0 ; I   oui -------------------------------      I  I    I FIXME
	JSR XHRSCB_ROUTINE ; I   non, on d?place le curseur        I      I  I    I FIXME
	JMP $E8E3  ;I---vers le bas                       I      I  I    I FIXME
	JSR XHRSCH_ROUTINE ; II  on d?place vers le haut <----------      I  I    I FIXME
	JSR XHRSSE_ROUTINE	  ;I-->on affiche le point <---------------------  I    I FIXME
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
	JSR XHRSCH_ROUTINE ;    dY<0, on d?place vers le haut                    I FIXME
	JMP $E903  ; ---et on saute                                      I FIXME
	JSR XHRSCB_ROUTINE  ; I  on d?place vers le bas <-------------------------- FIXME
	CLC       ;  -->a-t-on parcouru la tangente ?                     
	LDA RES                                                          
	ADC RESB                                                          
	STA RESB     ;   (dans RESB)                                        
	BCC $E919   ;   non ---------------------------------------------- FIXME
	BIT $4E     ;                                                    I
	BPL $E916   ;   dX>0 ------------------------------------        I FIXME
	JSR XHRSCG_ROUTINE   ;   dX<0, on d?place vers                   I        I FIXME
	JMP $E919  ; ---la gauche                               I        I FIXME
	JSR XHRSCD_ROUTINE  ; I  on d?place vers la droite <--------------        I FIXME
	JSR XHRSSE_ROUTINE	 ; -->on affiche le point <----------------------------- FIXME
	DEC $4F    ;    et on d?crit dY                                   
	BNE $E8F6                                                       ;  FIXME
	RTS         ;   avant de sortir de longueur des lignes            

	;   CALCUL LA TANGENTE (*256) D'UN TRAIT                    
Le921

	STX RES+1     ;   dX (ou dY)*256 dans RES+1                         
	LDY #$00    ;   dY (ou dX) dans AY                                
	STY RES                                                          
	JSR $CEDC    ;  calcul dX*256/dY (ou dY/dX)   FIXME                    
	LDA #$FF     ;  reste =-1                                         
	STA RESB    ;    resultat dans RES                                 
	RTS   

           ;                    ROUTINE CURSET                               
XCURSE_ROUTINE
Le92f                                                                               
	LDX $4D      ;  X=HRSX                                            
	LDY $4F     ;   Y=HRSY                                            
	JSR $E94E    ;  on v?rifie les coordonn?es      FIXME                   
	JSR Le7f3    ;  on place le curseur en X,Y       FIXME                  
	JMP $E79C    ;  et on affiche sans g?rer pattern   	 FIXME
	
     ;                          ROUTINE CURMOV
XCURMO_ROUTINE
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



XPAPER_ROUTINE
Le95d
	clc
	.byt $24

XINK_ROUTINE
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
	JSR Ldbb5    ;  (on envoie CHR$(12))                          I  I FIXME
	LDA #$1D     ;  et on passe en 38 colonnes                    I  I
	JSR Ldbb5    ;  (on envoie CHR$(29))                          I  I FIXME
	LDX $28      ;  on prend X=num?ro de fen?tre                  I  I
	LDA SCRDY,X  ;  on prend la ligne 0 de la fen?tre <------------  I
	JSR XMUL40_ROUTINE    ;  *40 dans RES                                     I FIXME
	LDA SCRBAL,X  ;  AY=adresse de base de la fen?tre                 I
	LDY $023C,X  ;                                                   I
	JSR XADRES_ROUTINE   ;   on ajoute l'adresse ? RES (ligne 0 *40) dans RES I FIXME
	LDY SCRDX,X  ;  on prend la premi?re colonne de la fen?tre       I
	DEY         ;   on enl?ve deux colonnes                          I
	DEY         ;                                                    I
	SEC         ;                                                    I
	LDA SCRFY,X ;   on calcule le nombre de lignes                   I
	SBC SCRDY,X ;   de la fen?tre                                    I
	TAX         ;   dans X                                           I
	INX         ;                                                    I
	TYA         ;   colonne 0 dans Y                                 I
	BCS $E9B3   ;   inconditionnel --------------------------------- I FIXME
	LDA #$00     ;  <----------------------------------------------+--
	LDX #$A0     ;                                                 I  
	STA RES      ;  RES=$A000 , adresse HIRES                      I  
	STX RES+1     ;                                                  I  
	LDX #$C8    ;   X=200 pour 200 lignes                          I  
	LDA #$00    ;   A=0 pour colonne de d?but = colonne 0          I  
	PLP         ;   on sort C <-------------------------------------  
	ADC #$00    ;   A=A+C                                             
	TAY        ;    dans Y                                            
	PLA        ;    on sort le code                                   
	STA (RES),Y; -->on le place dans la colonne correspondante        
	PHA        ; I  on le sauve                                       
	CLC        ; I                                                    
	LDA RES    ; I  on passe 28 colonnes                              
	ADC #$28    ;I  (donc une ligne)                                  
	STA RES     ;I                                                    
	BCC $E9C6  ; I         FIXME                                            
	INC RES+1    ; I                                                    
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
XCIRCL_ROUTINE
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
	STX TR0    ;    exposant du rayon dans $0C                        
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
	JSR XHRSCD_ROUTINE    ;;  elle ? baiss?, on d?place le curseur       I     I  
	JMP Lea20  ; ---? droite                                   I     I  
Lea1d
	JSR XHRSCG_ROUTINE ;  I  on d?place le curseur ? gauche <------------     I  FIXME 
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
	JSR XHRSCB_ROUTINE ;     on est descendu, on d?place le curseur      I    I  FIXME  
	JMP $EA4E ;  ---vers le bas et on affiche                   I    I  FIXME  
Lea44
	JSR XHRSCH_ROUTINE ;  I  on d?place le curseur vers le haut <---------    I  FIXME  
Lea41	
	JMP $EA4E ;  O--et on affiche                                    I  FIXME  
Lea4a	
	BIT $0D   ;  I  faut-il afficher le point ? <---------------------
	BMI Lea51 ;  I  non, on passe  -----------------------------------    
	JSR XHRSSE_ROUTINE	 ;  -->on affiche le point nouvellement calcul?         I  FIXME  
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
	LDX TR0       ; X=N tel que Rayon<2^N
Lea68	
	LDA $13       ; on garde le signe du r?sultat                     
	ROL                                                              
	ROR $13       ; et on divise par 2^X                              
	ROR $12       ; dans $13,$12                                      
	DEX                                                              
	BNE Lea68                                                        
	RTS         

XFILL_ROUTINE
LEA73
	lda $4b
	ldy $4c
	sta RES 
	sty RES+1
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
	
XSCHAR_ROUTINE
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

XCHAR_ROUTINE
LEAAF

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
	LDA (RESB),Y
	ASL
Leae4	
	ASL
	BEQ Leaf3 
	PHA
	BPL Leaed 
	JSR $E79C ; FIXME
Leaed	
	JSR XHRSCD_ROUTINE ; FIXME
	PLA
	BNE Leae4 
Leaf3	
	JSR XHRSCB_ROUTINE
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
	stx TR0
	sty $0d
	pha
	bit $5b
	bpl Lec5e 
	jsr send_a_to_minitel
	jmp $ec61
Lec5e	
	jsr $ec1b
	pla
	eor $0e
	sta $0e
	ldx TR0
	ldy $0d
	rts
Lec6b
	STX TR0
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
	LDX TR0
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
	JMP XLISBU_ROUTINE ; FIXME
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
	STY RES+1
	rts

send_serial_header_file
Lecfd
	LDX #$32
	LDA #$16
	JSR $EC4F
	DEX
	BNE $ECFF ; FIXME
	LDA #$24
	JSR $EC4F ; FIXME
	LDA #$00 
	STA $0E
	LDX #$00
	LDA $0518,X
	JSR $EC4F ; FIXME
	INX
	CPX #$0C
	BNE $ED12 ; FIXME
	LDA #$00
	JSR $EC4F ; FIXME
	
	LDX #$00
	LDA $052C,X
	JSR $EC4F ; FIXME
	
	
	
	INX
	CPX #$07
	BNE $ED24
	LDA $0E
	JMP $EC4F ; FIXME

read_header_file
Led34	
	
	JSR $EC6B ; FIXME
	CMP #$16
	BNE $ED34 ; FIXME
	LDX #$0A
	JSR $EC6B ; FIXME
	CMP #$16
	BNE $ED34 ; FIXME
	DEX
	BNE $ED3D ; FIXME
	JSR $EC6B ; FIXME
	CMP #$16
	BEQ $ED47
	CMP #$24
	BNE $ED34 ; FIXME
	LDA #$00
	STA $0E
	JSR $EC6B; FIXME
	TAX
	BEQ $ED62 ; FIXME
	JSR Ldbb5
	JMP $ED56 ; FIXME
	LDX #$00
	
	
	
	JSR $EC6B ; FIXME
	STA $052C,X
	INX
	CPX #$07
	BNE $ED64 ; FIXME
	JSR $EC6B ; FIXME
	ORA #$30
	JMP Ldbb5

XCONSO_ROUTINE	
Led77	
	JSR $ECC1 ; FIXME
	JSR $ECC9; FIXME
	JSR $EC10 ; FIXME
	BCS $ED85; FIXME
	JSR Ldbb5
	JSR $C7CF; FIXME
	BCS $ED7D ; FIXME
	CMP #$03
	BEQ $ED94 ; FIXME
	JSR $EC1B; FIXME
	JMP $ED7D 
	JSR $ECBF ; FIXME
	JMP $ECC7 ; FIXME
	


Led9a
XSDUMP_ROUTINE
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
	JSR Ldbb5 ; FIXME
	PLA
	JSR $CE54 ; FIXME
	JSR Ldbb5 ; FIXME
	TYA 
	JSR Ldbb5 ; FIXME
	LDA #$87 
	JSR Ldbb5 ; FIXME
	JMP $ED9D ; FIXME
	JMP $ECBF ; FIXME

XSSAVE_ROUTINE
Ledca
	ror $5b
	lsr $5b
	jsr $ecc9 ; FIXME
	jsr Lee0a ; FIXME
	jmp $ecc7 ; FIXME

XMSAVE_ROUTINE
Ledd7
	ror $5b
	sec
	ror $5b
	jsr $ecd9 ; FIXME
	jsr Lee0a ; FIXME
	jmp $ecd7 ; FIXME

XSLOAD_ROUTINE 
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
	
	
XMLOAD_ROUTINE	
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
	LDA (RES),Y
	JSR $EC4F ; FIXME
	DEC $052A
	INC RES
	BNE $EE18 ; FIXME
	INC RES+1
	BNE $EE18 ; FIXME
	LDA $052B
	BEQ $EE51 ; FIXME
	LDY #$00
	LDA (RES),Y
	JSR $EC4F ; FIXME
	INY
	BNE $EE36 ; FIXME
	DEC $052B
	INC RES+1
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
	STA (RES),Y
	DEC $052A
	INC RES
	BNE $EE70  ; FIXME
	INC RES+1
	JMP $EE70  ; FIXME
	LDA $052B
	BEQ $EE9D  ; FIXME
	LDY #$00
	JSR $EC6B  ; FIXME
	STA (RES),Y
	INY
	BNE $EE8D  ; FIXME
	INC RES+1
	DEC $052B
	JMP $EE86  ; FIXME
	JSR $EC6B  ; FIXME
	ORA #$30
	JMP Ldbb5  ; FIXME

XRING_ROUTINE
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

XLIGNE_ROUTINE
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
XDECON_ROUTINE
	jsr $ecd9 ; FIXME
	lda #$67
	jsr send_pro1_sequence_to_minitel
	jmp $ecd7 ; FIXME
	

Lef4a
#ifdef HAVE_MINITEL
XWCXFI_ROUTINE

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

XMOUT_ROUTINE
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

XSOUT_ROUTINE
; RS232 
	pha 
	jsr $ecc9 ; FIXME
	pla
	jsr $ec1b ; FIXME
	jmp $ecc7 ; FIXME
add_0_5_A_ACC1
	lda #<const_zero_dot_half 
	ldy #>const_zero_dot_half 
	jmp $efaf ; AY+acc1 ; FIXME
Lef97 	
	rts ; Don't know why there is this RTS !

	
ACC2_ACC1
	jsr $f1ec ; FIXME
XA2NA1_ROUTINE	
	lda $65
	eor #$ff
	sta $65
	eor $6d
	sta $6e
	lda $60
	jmp XA1PA2_ROUTINE
Lefaa
mantisse_A
	jsr $f0e5 ; FIXME
	bcc $efee ; FIXME

AY_add_acc1
		jsr $f1ec ; FIXME
XA1PA2_ROUTINE
Lefb2
ACC2_ADD_ACC1	
	bne  next700
	jmp XA2A1_ROUTINE 
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
	lsr RES+1,x
	
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
	sbc RESB,x
	sta $62
	lda $0001,y
	sbc RES+1,x
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
        ldy     RESB,x
        sty     $03,x
        ldy     RES+1,x
        sty     RESB,x
        ldy     $67
        sty     RES+1,x
LF0E5:  adc     #$08
        bmi     LF0D1
        beq     LF0D1
        sbc     #$08
        tay
        lda     $66
        bcs     LF106
LF0F2:  asl     RES+1,x
        bcc     LF0F8
        inc     RES+1,x
LF0F8:  ror     RES+1,x
        ror     RES+1,x
LF0FC:  ror     RESB,x
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

XLN_ROUTINE
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
        lda     #<const_for_ln 
        ldy     #>const_for_ln 
        jsr     $EFAF  ; FIXME
        lda     #$31 ; FIXME
        ldy     #$F1 ; FIXME
        jsr     $F287  ; FIXME
        lda     #<const_atn_1 
        ldy     #>const_atn_1 
        jsr     $EF98  ; FIXME
        lda     #<polynome_ln_coef
        ldy     #>polynome_ln_coef
        jsr     $F6E1  ; FIXME
        lda     #$36 ; FIXME
        ldy     #$F1 ; FIXME
        jsr     $EFAF  ; FIXME
        pla
        jsr     $F9E9  ; FIXME
        lda     #$3B ; FIXME
        ldy     #$F1 ; FIXME
		
LF184:  jsr     $F1EC ; FIXME
        beq     LF140
        bne     LF190
XA1MA2_ROUTINE		
        beq     LF140
LF18D		
        tsx
        stx     $89
LF190:  jsr     $F217 ; FIXME
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
	JSR XA1A2_ROUTINE ; FIXME
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
	jsr XA1A2_ROUTINE ; FIXME
	ldx #0
	lda #<ten_in_floating_point
	ldy #>ten_in_floating_point
	stx $6e
	jsr $f323 ; FIXME
	jmp XA2DA1_ROUTINE	
	
	
XLOG_ROUTINE
Lf26f
	tsx
	stx $89
	jsr $f149  ; FIXME
	jsr XA1A2_ROUTINE 
	lda #<const_ln_10
	ldy #>const_ln_10
	jsr $f323  ; FIXME
	jmp XA2DA1_ROUTINE	

display_divide_per_0
Lf282
	lda #3
	sta $8b ; FLERR
	rts
Lf287
	JSR LF1EC 
XA2DA1_ROUTINE	
LF28A
	BEQ display_divide_per_0
	TSX
	STX $89
	JSR XAA1_ROUTINE
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

XPI_ROUTINE	
Lf314
; pi->acc1
	jsr test_if_degree_mode ; FIXME
	beq Lf31f ; is it in radian mode ?
	lda #<const_pi_degree
	ldy #>const_pi_degree
	bne Lf323
Lf31f	
	lda #<const_pi_radians ; radian
	ldy #>const_pi_radians

XAYA1_ROUTINE	
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

	JSR XAA1_ROUTINE 

XA1XY_ROUTINE
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
XA2A1_ROUTINE	
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
XA1A2_ROUTINE
	; arrondi ACC1 in ACC2_ACC1
	jsr XAA1_ROUTINE
	
LF38A:  ldx     #$06
LF38C:  lda     $5F,x
        sta     $67,x
        dex
        bne     LF38C
        stx     $66
LF395:  rts
XAA1_ROUTINE
LF396:
	lda     $60
    beq     LF395
    asl     $66
    bcc     LF395
    jsr     $F0B8 
    bne     LF395
    jmp     $F083 ; FIXME

XA1IAY_ROUTINE	
LF3A6	
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

XIYAA1_ROUTINE	
LF3ED	
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
	
XINT_ROUTINE	

LF46A
	LDA $60
	CMP #$A0
	BCS $F469  ; FIXME
	JSR $F439 ; FIXME
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
	jmp LF3DE
XA1AFF_ROUTINE	
LF49B
	jsr XA1DEC_ROUTINE	 
	lda #0
	ldy #1
	jmp $c7a8 ; FIXME

XA1DEC_ROUTINE	
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
	JSR LF184 ; FIXME
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
XSQR_ROUTINE
LF610
	jsr XA1A2_ROUTINE 
	lda #<const_zero_dot_half	
	ldy #>const_zero_dot_half	
	jsr XAYA1_ROUTINE 

XA2EA1_ROUTINE
LF61A
	BEQ XEXP_ROUTINE  
	TSX
	STX $89
	LDA $68
	BEQ $F60D  ; FIXME
	LDX #$80
	LDY #$00
	JSR XA1XY_ROUTINE 
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
	JSR LF184  
	JSR $F68F  ; FIXME
	PLA
	LSR
	BCC $F65D  ; FIXME

XNA1_ROUTINE	
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
XEXP_ROUTINE
LF68C
	TSX
	STX $89
	LDA #$5E ; FIXME
	LDY #$F6 ; FIXME
	JSR LF184 ; FIXME
	LDA $66
	ADC #$50
	BCC $F69F ; FIXME
	JSR XAA1_ROUTINE ; FIXME
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
	JSR XNA1_ROUTINE 
	LDA #<coef_polynome 
	LDY #>coef_polynome 
	JSR $F6F7 ; FIXME
	LDA #$00
	STA $6E
	PLA
	JMP LF219 


LF6E1	
	STA $85
	STY $86
	JSR $F348 ; FIXME
	LDA #$73
	JSR LF184 ; FIXME
	JSR $F6FB ; FIXME
	LDA #$73
	LDY #$00
	JMP LF184	 ; FIXME

	
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
	JSR LF184  ; FIXME
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
	
XRND_ROUTINE
LF735
	JSR $F3BD
	TAX
	BMI $F753
	LDA #$EF
	LDY #$02
	JSR $F323
	TXA
	BEQ $F72A
	LDA #$2B
	LDY #$F7
	JSR LF184
	LDA #$30
	LDY #$F7
	JSR $EFAF ; FIXME
	LDX $64
	LDA $61
	STA $64
	STX $61
	LDA #$00
	STA $65
	LDA $60
	STA $66
	LDA #$80
	STA $60
	JSR $F022 ; FIXME
	LDX #$EF ; FIXME
	LDY #$02 ; FIXME
	JMP XA1XY_ROUTINE ; FIXME

XRAND_ROUTINE
LF771
	JSR $F348 ; FIXME
	JSR XRND_ROUTINE 
	LDA #$73
	LDY #$00
	JSR LF184 
	JMP $F46A ; FIXME

XCOS_ROUTINE
LF781
	JSR $F8B1 ; FIXME
	LDA #<CONST_PI_DIVIDED_BY_TWO ; FIXME
	LDY #>CONST_PI_DIVIDED_BY_TWO ; FIXME
	JSR $EFAF ; FIXME
	JMP $F791 ; FIXME

XSIN_ROUTINE
LF78E
	JSR $F8B1 ; FIXME
	JSR XA1A2_ROUTINE ; FIXME
	LDA #$E1 ; FIXME
	LDY #$F7 ; FIXME
	LDX $6D
	JSR $F267 ; FIXME
	JSR XA1A2_ROUTINE ; FIXME
	JSR $F46A ; FIXME
	LDA #$00
	STA $6E
	JSR $EF9B ; FIXME
	LDA #$E6
	LDY #$F7
	JSR $EF98 ; FIXME
	LDA $65
	PHA
	BPL $F7C5
	JSR $EF90 ; FIXME
	LDA $65
	BMI $F7C8 ; FIXME
	LDA $8A
	EOR #$FF
	STA $8A
	BIT $48
	JSR XNA1_ROUTINE ; FIXME
	LDA #$E6 ; FIXME
	LDY #$F7 ; FIXME
	JSR $EFAF ; FIXME
	PLA
	BPL $F7D5 ; FIXME
	JSR XNA1_ROUTINE ; FIXME
	LDA #<coef_polynome_sin
	LDY #>coef_polynome_sin
	JMP $F6E1 ; FIXME



	
CONST_SIN_AND_COS
CONST_PI_DIVIDED_BY_TWO
LF7DC
	.byt $81,$49,$0f,$da,$a2
const_pi_mult_by_two
	.byt $83,$49,$0f,$da,$a2 ; 6.283185307
const_0_dot_twenty_five ; 0.25
	.byt $7f,$00,$00,$00,$00
coef_polynome_sin
	.byt $05 ; 6 coef
	.byt $84,$e6,$1a,$2d,$1b
	.byt $86,$28,$07,$fb,$f8
	.byt $87,$99,$68,$89,$01
	.byt $87,$23,$35,$df,$e1
	.byt $86,$a5,$5d,$e7,$28
	.byt $83,$49,$0f,$da,$a2
LF80A
XTAN_ROUTINE
	JSR $F8B1 ; FIXME
	JSR $F348 ; FIXME
	LDA #$00
	STA $8A
	JSR $F791 ; FIXME
	LDX #$80
	LDY #$00
	JSR XA1XY_ROUTINE ; FIXME
	LDA #$73
	LDY #$00
	JSR $F323 ; FIXME
	LDA #$00
	STA $65
	LDA $8A
	JSR $F7C4 ; FIXME
	LDA #$80
	LDY #$00
	JMP $F287 ; FIXME


	
XATN_ROUTINE
LF835
	LDA $65
	PHA
	BPL $F83D ; FIXME
	JSR XNA1_ROUTINE 
	LDA $60
	PHA
	CMP #$81
	BCC $F84B ; FIXME
	LDA #<const_atn_1
	LDY #>const_atn_1 
	JSR $F287 ; FIXME
	LDA #<const_coef_atn 
	LDY #>const_coef_atn 
	JSR $F6E1 ; FIXME
	PLA
	CMP #$81 
	BCC $F85E ; FIXME
	LDA #<CONST_SIN_AND_COS 
	LDY #>CONST_SIN_AND_COS
	JSR $EF98 ; FIXME
	PLA
	BPL $F864 ; FIXME
	JSR XNA1_ROUTINE 
	JSR test_if_degree_mode 
	BEQ LF86C 
	JMP XDEG_ROUTINE	 
LF86C
	RTS

L86d
const_coef_atn	
	.byt $0b ; 11 coef
	.byt $76,$b3,$83,$bd,$d3
	.byt $79,$1e,$f4,$a6,$f5
	.byt $7b,$83,$fc,$b0,$10
	.byt $7c,$0c,$1f,$67,$ca
	.byt $7c,$de,$53,$cb,$c1
	.byt $7d,$14,$64,$70,$4c
	.byt $7d,$b7,$ea,$51,$7a
	.byt $7d,$63,$30,$88,$7e
	.byt $7e,$92,$44,$99,$3a
	.byt $7e,$4c,$cc,$91,$c7
	.byt $7f,$aa,$aa,$aa,$13
const_atn_1	
	.byt $81,$00,$00,$00,$00 ; 1 coef 0

XDEG_ROUTINE	
LF8AA
	; convert ACC1 in defree
	lda #<const_180_divided_by_pi
	ldy #>const_180_divided_by_pi 
	jmp LF184 

LF8B1
	jsr test_if_degree_mode  
	beq LF8CC 	 
XRAD_ROUTINE
LF8B
	lda #<const_pi_divided_by_180   
	ldy #>const_pi_divided_by_180  
	jmp LF184
	
const_180_divided_by_pi
LF8BD
	.byt $86,$65,$2e,$e0,$d8
const_pi_divided_by_180	
	.byt $7b,$0e,$fa,$35,$19

test_if_degree_mode
LF8C7
	lda FLGTEL
	and #$20
LF8CC
	rts
XADNXT_ROUTINE	
Lf8cd
	sta     RES
	sty     RES+1
	jsr     $EFAF ; FIXME
	ldx     RES
	ldy     RES+1
	jmp     XA1XY_ROUTINE

LF8DB:  jsr     $F9FC
        bcc     LF8E7
        cmp     #$41
        bcc     LF915
        sbc     #$37
        .byt   $2C
LF8E7:  sbc     #$2F
        cmp     #$10
        bcs     LF915
        asl   
        asl    
        asl    
        asl    
        ldx     #$04
LF8F3:  asl    
        rol     $62
        rol     $61
        bcs     LF912
        dex
        bne     LF8F3
        beq     LF8DB
LF8FF:  jsr     $F9FC
        bcs     LF915
        cmp     #$32
        bcs     LF915
        cmp     #$31
        rol     $62
        rol     $61
        bcs     LF912
        bcc     LF8FF
LF912:  jmp     $F0C7 ; FIXME

LF915:  ldx     #$90
        sec
        jsr     LF3DE 
        ldx     #$00
        rts	

;;;;;;;;;;;;;;; 
XDECA1_ROUTINE
LF91E
        sta     RES
        sty     RES+1
        tsx
        stx     $89
        lda     #$00
        sta     RESB
        sta     $03
        sta     $66
        ldx     #$05
LF92F:  sta     $60,x
        sta     $73,x
        dex
        bpl     LF92F
        jsr     LF9FE
        bcc     LF951
        cmp     #$23
        beq     LF8DB
        cmp     #$25
        beq     LF8FF
        cmp     #$2D
        beq     LF94C
        cmp     #$2B
        bne     LF953
        .byte   $2C
LF94C:  stx     $03
LF94E:  jsr     LF9FC
LF951:  bcc     LF9CD
LF953:  cmp     #$2E
        beq     LF9A6
        cmp     #$45
        beq     LF95F
        cmp     #$65
        bne     LF9AC
LF95F:  ldx     RESB
        jsr     LF9FC
        bcc     LF976
        cmp     #$2D
        beq     LF96F
        cmp     #$2B
        bne     LF998
        .byte   $2C
LF96F:  ror     $77
LF971:  jsr     LF9FC
        bcs     LF99A
LF976:  lda     $75
        cmp     #$0A
        bcc     LF985
        lda     #$64
        bit     $77
        bmi     LF993
        jmp     LF0C7

LF985:  asl    
        asl    
        clc
        adc     $75
        asl    
        clc
        ldy     RESB
        adc     (RES),y
        sec
        sbc     #$30
LF993:  sta     $75
        jmp     LF971

LF998:  stx     RESB
LF99A:  bit     $77
        bpl     LF9AC
        lda     #$00
        sec
        sbc     $75
        jmp     LF9AE

LF9A6:  ror     $76
        bit     $76
        bvc     LF94E
LF9AC:  lda     $75
LF9AE:  sec
        sbc     $74
        sta     $75
        beq     LF9C7
        bpl     LF9C0
LF9B7:  jsr     $F25E
        inc     $75
        bne     LF9B7
        beq     LF9C7
LF9C0:  jsr     $F242
        dec     $75
        bne     LF9C0
LF9C7:  lda     $03
        bmi     LF9E1
        bpl     LF9E4
LF9CD:  pha
        bit     $76
        bpl     LF9D4
        inc     $74
LF9D4:  jsr     $F242
        pla
        sec
        sbc     #$30
        jsr     LF9E9
        jmp     LF94E

LF9E1:  jsr     XNA1_ROUTINE
LF9E4:  ldx     #$00
        jmp     XAA1_ROUTINE

LF9E9:  pha
        jsr     XA1A2_ROUTINE
        pla
        jsr     LF3D1
        lda     $6D
        eor     $65
        sta     $6E
        ldx     $60
        jmp     XA1PA2_ROUTINE ; FIXME

LF9FC:  inc     RESB
LF9FE:  ldy     RESB
        lda     (RES),y
        jsr     $D0F0 ; FIXME
        cmp     #$20
        beq     LF9FC 
        cmp     #$30
        bcc     LFA10
        cmp     #$3A
        rts

LFA10:  sec
        rts

XINTEG_ROUTINE		
		
        jsr     XA1A2_ROUTINE
        lda     $68
        beq     LFA32
        bpl     LFA3C
        sec
        lda     #$A1
        sbc     $68
        bcc     LFA3C
        tax
LFA23:  dex
        beq     LFA39
        lsr     $69
        ror     $6A
        ror     $6B
        ror     $6C
        bcc     LFA23
        bcs     LFA3C
LFA32:  ldx     #$03
        sta     $69,x
        dex
        bpl     LFA23
LFA39:  lda     #$01
        rts

LFA3C:  lda     #$00
        rts


/****** BEGIN CHARSET ********************/
LFA3F
table_chars_qwerty
	.byt $37,$6a,$6d,$6b,$20,$75,$79,$38,$6e,$74,$36,$39,$2c,$69,$68,$6c,$35
	.byt $72,$62,$3b,$2e,$6f,$67,$30,$76,$66,$34,$2d,$0b,$70,$65,$2f,$31
	.byt $1b,$7a,$00,$08,$7f,$61,$0d,$78,$71,$32,$5c,$0a,$5d,$73,$00,$33
	.byt $64,$63,$27,$09,$5b,$77,$3d,$26,$4a,$4d,$4b,$20,$55,$59,$2a,$4e
	.byt $54,$5e,$28,$3c,$49,$48,$4c,$25,$52,$42,$3a,$3e,$4f,$47,$29,$56

	.byt $46,$24,$5f,$0b,$50,$45,$3f,$21,$1b,$5a,$00,$08,$7f,$41,$0d
	
	.byt $58

	.byt $51,$40,$7c,$0a,$7d,$53,$00,$23,$44,$43,$22,$09,$7b,$57,$2b
LFAAF
table_chars_azerty
	.byt $37

	.byt $6a,$3b,$6b,$20,$75,$79,$38,$6e,$74,$36,$39,$2c,$69,$68,$6c,$35

	.byt $72,$62,$6d,$2e,$6f,$67,$30,$76,$66,$34,$2d,$0b,$70,$65,$2f,$31
	.byt $1b,$77,$00,$08,$7f,$71,$0d,$78,$61,$32,$5c,$0a,$5d,$73,$00,$33

	.byt $64,$63,$27,$09,$5b,$7a,$3d,$26,$4a,$3a,$4b,$20,$55,$59,$2a,$4e
	.byt $54,$5e,$28,$3c,$49,$48,$4c,$25,$52,$42,$4d,$3e,$4f,$47,$29,$56

	.byt $46,$24,$5f,$0b,$50,$45,$3f,$21,$1b,$57,$00,$08,$7f,$51,$0d
	
	.byt $58
	.byt $41,$40,$7c,$0a,$7d,$53,$00,$23,$44,$43,$22,$09,$7b,$5a,$2b
LFB1F		
table_chars_french_table
table_chars_bwana_table
	.byt $7d
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
/****** END OF CHARSET ********************/	
	
codes_for_calc_alternates	
Lfe45	
	.byt $00,$38,$07,$3f
Lfe49
routine_to_define_22

	LDA #$b9 ;  index of alternate chars
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
	BEQ loop71
	CMP #$40
	beq next76
	JSR routine_to_define_23 
	
	
	.byt $2c 
loop71		
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
	lda code_in_order_to_move_chars_tables,y 
	sta $04,x
	dey
	dex
	bpl Lfedf 
	jmp XDECAL_ROUTINE ; FIXME

code_in_order_to_move_chars_tables
	; Text to hires 6 bytes
	.byt $00,$b4,$80,$bb,$00,$98
	; hires to text 6 bytes
	.byt $00,$98,$80,$9f,$00,$b4

XSCRNE_ROUTINE
; define a char in the adress AY 
; it take the first byte : it's the ascii char
; Next 8 bytes is the definition of the char
	clc
	.byt  $24 ; jump a byte
Lfef9	
	sec
	ror RES
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
	sta (RESB),y
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
	bit RES
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
	sta RESB
	bit FLGTEL
	bmi Lff4b
	lda $03
	adc #$1c
	sta $03
Lff4b
	rts
	
Lff4c	
select_keyboard_mode
XGOKBD_ROUTINE
	cmp #$04
	beq Lff85
	cmp #$05
	beq Lff8c
	asl
	and #$06
	sta RES
	tax
	lda $0275
	and #$f9
	ora RES
	sta $0275
	lda Lff90,x
	ldy Lff90+1,x
	sta ADKBD
	sty $2b
	jsr routine_to_define_22 
	lda $0275
	and #$06
	cmp #$04
	bne Lff81
	lda #<a_accent_circonflexe
	ldy #>a_accent_circonflexe
	jmp Lfef9 
Lff81	
	cmp #2
	bne Lff8f
Lff85	
	lda #<e_accent_aigu 
	ldy #>e_accent_aigu 
	jmp Lfef9 
Lff8c	
	jmp routine_to_define_22 
Lff8f	
	rts

Lff90
	.byt <table_chars_qwerty,>table_chars_qwerty 
	.byt <table_chars_azerty,>table_chars_azerty
	.byt <table_chars_french_table,>table_chars_french_table 
	.byt <table_chars_french_table,>table_chars_french_table
Lff98	
tab_accent_charset
a_accent_circonflexe
LFF98
	.byt $5b ; Accent circonflexe
	.byt $1c,$22,$1c,$02,$1e,$22,$1e,$00
u_accent_circonflexe	
LFFA1
	.byt $60
	.byt $1c,$22,$00,$22,$22,$26,$1a,$00
LFFAA	
e_accent_aigu	
	.byt $7b
	.byt $04,$08,$1c,$22,$3e,$20,$1e,$00
LFFB3	
e_accent_grave	
	.byt $7d
	.byt $10,$08,$1c,$22,$3e,$20,$1e,$00
LFFBC	
a_accent_grave	
	.byt $40
	.byt $10,$08,$1c,$02,$1e,$22,$1e,$00
c_cedille	
LFFC5
	.byt $5c
	.byt $00,$00,$1e,$20,$20,$20,$1e,$04
u_accent_grave
LFFCE
	.byt $7c
	.byt $10,$08,$22,$22,$22,$26,$1a,$00
LFFD7	
e_accent_circonflexe	
	.byt $7e
	.byt $1c,$22,$1c,$22,$3e,$20,$1c,$00

free_bytes ; 28 bytes	
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

