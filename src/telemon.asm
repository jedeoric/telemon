/***********************************************************************/
/* DASM and source converted with labels : jede (jede[at]oric[dot]org) */
/* may and june 2016                                                   */
/* with the help of G. Meister telemon dasm 						   */
/* telemon 2.4                                                         */
/***********************************************************************/

/*
issues if you modify someting :

1) BONJOUR.COM does not load, and can't be load : in BUFNOM, something is wrong : it try to loads BONJOUR.XXX X is a char of str_bonjour

*/

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

	LDA #$D0 ; send command to FDC 
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
	BRK_TELEMON(XRECLK)  ; Don't know this vector
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
	BRK_TELEMON(XCRLF)
	
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
	LDX #$02 ; store default extension
loop55	
	LDA str_default_extention,X ;
	STA BUFDEF,X ; CORRECTME
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
	LDA BNKST,X
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
	lda BNKST,x 
	bpl Lc365 

	stx V2DRA
	lda $fff8 
	sta $02
	lda $fff9
	sta RESB+1
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
	lda BNKST,x
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
	BRK_TELEMON(XWSTR0)
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
	.asc $0d,$0a,"TELEMON V"
str_telemon_version
	.asc "2.5.0"
	

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
	.asc "BONJOUR"
str_default_extention
	.asc "COM";
	
; ERROR for BONJOURCOM



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
	STX FDCSR
	JSR $B84F ; FIXME      
	INC RES+1  
	DEC $C102 ; FIXME      
	BNE loop102
	JSR $C105 ; FIXME
	LDA $FFFB ; FIXME
	STA BNKST
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
	LDA $0318 ; FIXME
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
set_buffers	
	LDA data_to_define_7,X 
	STA RES
	LDA begin_keyboard_buffer+1,X ; Get high adress of the buffer
	STA RES+1
	
	LDA end_keyboard_buffer,X
	LDY end_keyboard_buffer+1,X 
	
	LDX RESB


XINIBU_ROUTINE


	BIT XLISBU_ROUTINE

	BVC next19

XVIDBU_ROUTINE	
	LDA #$00
lc50e
	.byt $2c
XTSTBU_ROUTINE	
	lda #1
	bit code_adress_400


next19
	SEC
	JMP $0409
Lc518
XLISBU_ROUTINE
	bit XLISBU_ROUTINE
	BVC next20
XECRBU_ROUTINE	
LC51D
	bit loading_vectors_page_4
	
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
	JSR BUFROU
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
	LDA (ADDRESS_READ_BETWEEN_BANK),Y
	PHA
	LDA V2DRA
	ORA #%00000111
	STA V2DRA
	PLA
	RTS	
/*THIS ROUTINE IS COPIED IN $700*/
data_adress_04c7	
to_put_in_address_700
data_to_define_4	
	; should be length 256 bytes ?
	bcc LC639	
	bvc LC5FE	
	tay

	beq LC61E
	lda $c088,x
	ora $c089,x
	beq LC5FC	
	clc 
	rts
LC5FC	
	sec
	rts
LC5FE	
	sta RESB
	
	sty RESB+1
	
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
LC61E	
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

	
LC639	
	bvs LC661 
	jsr $c507
	bcs LC660 
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
LC660	
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
begin_keyboard_buffer	
LC6AF
	.byt $c4,$c5 ; Keyboard buffer begin $c5c4
end_keyboard_buffer	
LC6B1	
	.byt $80,$c6 ; Keyboard buffer end $c680
	.byt $80,$c6  ; buffer acia/minitel input begin
	.byt $00,$c8  ; buffer acia/minitel input end
	.byt $00,$c8   ; buffer acia/minitel output begin
	.byt $00,$ca ; buffer acia/minitel output end
	.byt $00,$ca ;  buffer printer output begin
	.byt $00,$d2 ;  buffer printer output end

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
; NOERROR

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
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <LDAF7,>LDAF7 ; MINITEL (mde) 
	.byt <LDB5D,>LDB5D ; RSE 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ;  not used  
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING; not used 
	.byt <LDB86,>LDB86
	.byt <LDB8C,>LDB8C
	.byt <LDB92,>LDB92 
	.byt <LDB98,>LDB98  
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING 
	.byt <Lda70,>Lda70 ;30
	.byt <Ldb12,>Ldb12
	.byt <LDB79,>LDB79 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 

	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	.byt <ROUTINE_I_O_NOTHING,>ROUTINE_I_O_NOTHING ; not used 
	

	
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
	DEC BUFTRV+2,X ; CORRECTME
LC87A
	SEC
	SBC #$01
	PHA
	STA ADDRESS_READ_BETWEEN_BANK
	LDA BUFTRV+2,X
	STA ADDRESS_READ_BETWEEN_BANK+1
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
	LDA vectors_telemon_second_table+1,X ; Second table because X >127 
	LDY vectors_telemon_second_table,X ;
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
	JSR XTSTBU_ROUTINE; CORRECTME
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
	JMP LDE2D 
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
	DEC KEYBOARD_COUNTER
	BNE next113 
	JSR manage_keyboard 
	JSR LC8BF
	BIT $0270 ; CORRECTME
	BPL next114 
	LDA #$14 
	STA KEYBOARD_COUNTER+1 ; CORRECTME
	BNE LC9FB 
next114	
	LDA KEYBOARD_COUNTER+2 ; CORRECTME
	BIT KEYBOARD_COUNTER+1 ; CORRECTME
	BMI lc9fd 
	DEC KEYBOARD_COUNTER+1 ; CORRECTME
LC9FB
next115	
	LDA #$01
lc9fd	
	STA KEYBOARD_COUNTER ; CORRECTME
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
	.byt <XWSTR1_ROUTINE,>XWSTR1_ROUTINE ;
	.byt <XWSTR2_ROUTINE,>XWSTR2_ROUTINE	; 
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
	.byt $00,$00 ; 

	.byt $00,$00 ; nothing  $31

	.byt <XEDTIN_ROUTINE,>XEDTIN_ROUTINE; XEDTIN $32
	.byt <XECRPR_ROUTINE,>XECRPR_ROUTINE; XECRPR $33 $ece6
	.byt <XCOSCR_ROUTINE,>XCOSCR_ROUTINE  ;XCOSCR $34
	.byt <XCSSCR_ROUTINE,>XCSSCR_ROUTINE ; $35 XCSSCR 
	.byt <XSCRSE_ROUTINE,>XSCRSE_ROUTINE ; $36 
	.byt <XSCROH_ROUTINE,>XSCROH_ROUTINE ; $37
	.byt <XSCROB_ROUTINE,>XSCROB_ROUTINE ; $38 XSCROB
	.byt <XSCRNE_ROUTINE,>XSCRNE_ROUTINE ; $39
	.byt $00,$00 ; $3a 
	.byt $00,$00 ; $3b
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
	.byt $00,$00
	
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
vectors_telemon_second_table	
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
	sty ACC1E
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
	ldx ACC1E
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
	ldx ACC1E
	rts
Lcc3e
	cmp #$0a
	bne Lcc6d 
	lda ACC1E
	cmp $67 
	beq Lcc4d
	inc ACC1E
	jmp Lcbfd
Lcc4d	
	bit $68
	bmi Lcbfd 
	inc ACC1E
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
	ldx ACC1E
	
	jsr display_x_choice 
	jmp Lcbfd
Lcc6d	
	cmp #$0b
	bne Lcc9a 
	lda ACC1E
	cmp $66
	bne Lcc92
	
	lda ACC1E
	beq Lcc94 
	dec $66
	dec $67 
	dec ACC1E 
	bit FLGTEL ; Minitel ?
	bvs Lcc97 
	ldx $62
	ldy $63
	jsr XSCROB_ROUTINE  
	ldy $62
	jmp Lcc65
Lcc92	
	dec ACC1E
Lcc94
	jmp Lcbfd 
Lcc97	
	jmp Lcbeb
Lcc9a	
	cmp #$30
	bcc Lcc94
	cmp #$3a
	bcs Lcc94 
	ldx ACC1E
	cpx #$19
	bcc Lccae
Lcca8	
	ldx $66
	stx ACC1E 
	bcs Lcc94
Lccae	
	pha
	asl ACC1E
	lda ACC1E 
	asl ACC1E 
	asl ACC1E
	adc ACC1E
	sta ACC1E
	pla
	and #$0f
	adc ACC1E
	sbc #0
	
	sta ACC1E
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
	sta ADDRESS_READ_BETWEEN_BANK
	sty ADDRESS_READ_BETWEEN_BANK+1
	ldy #0
Lcd0c	
	dex
	beq Lcd20
Lcd0f
	iny 
	bne Lcd14
	inc ADDRESS_READ_BETWEEN_BANK+1
Lcd14	
	jsr $0411
	bne Lcd0f
	
	iny
	bne Lcd0c
	inc ADDRESS_READ_BETWEEN_BANK+1
	bne Lcd0c
Lcd20	
	ldx ADDRESS_READ_BETWEEN_BANK+1
	clc
	tya
	adc ADDRESS_READ_BETWEEN_BANK
	bcc Lcd29 
	inx
Lcd29	
	sta $02
	stx RESB+1
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
	lda RESB
	ldy RESB+1
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
	lda DECFIN+1
	sbc $05
	tax
	bcc Lcdb9 
	stx $0b

	lda DECCIB
	cmp $04
	lda DECCIB+1
	sbc $05
	bcs Lcdbf 
	tya
	eor #$ff
	adc #1
	tay 
	sta $0a
	bcc Lcd97
	dex
	inc DECFIN+1
Lcd97	
	sec
	lda DECCIB
	sbc $0a
	sta DECCIB
	bcs Lcda2

	dec DECCIB+1
Lcda2	
	clc
	lda DECFIN+1
	sbc $0b
	sta DECFIN+1
	inx
Lcdaa	
	lda (DECFIN),y
	sta (DECCIB),y
	iny 
	bne Lcdaa
	inc DECFIN+1
	inc DECCIB+1
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
	adc DECCIB+1
	sta DECCIB+1
	inx
Lcdcc	
	dey
	lda ($04),y
	sta (DECCIB),y
	tya
	bne Lcdcc
	dec $05
	dec DECCIB+1
	dex
	bne Lcdcc
	beq Lcdb8
	

data_for_decimal_conversion
const_10_decimal_low	
LCDDD
	.byt $0a ; 19
const_100_decimal_low
LCDDE
	.byt $64 ; 100 
const_1000_decimal_low	; $3e8=1000
LCDDF
	.byt $e8
const_10000_decimal_low	; $3e8=1000	
LCDE0
	.byt $10
const_10_decimal_high
LCDE1
	.byt $00
	.byt $00
	.byt $03
	.byt $27

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
	SBC const_10_decimal_low,X 
	STA TR1
	LDA TR2
	PHA
	SBC const_10_decimal_high,X ; 
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
	.byt $2c
	ora #$30	

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
	stx TR1
	stx $0e
	stx $0f
	stx RESB
	stx RESB+1
	ldx #$10
LCEAB	
	lsr $11
	ror $10
	bcc LCECA
	clc
	
	lda RES
	adc TR0
	sta TR0
	
	lda RES+1
	adc TR1
	sta TR1
	
	lda RESB
	adc $0e
	sta $0e
	
	lda RESB+1
	adc $0f
	sta $0f
LCECA
	asl RES
	rol RES+1
	rol RESB
	rol RESB+1
	
	lda TR4
	ora $11
	beq Lcedb
	dex
	bne LCEAB 
Lcedb	
	rts


XDIVIS_ROUTINE	
	sta TR0
	sty TR1
	ldx #0
	stx RESB
	stx RESB+1
	ldx #$10
Lcee8	
	asl RES
	rol RES+1
	rol RESB
	rol RESB+1
	sec
	lda RESB
	sbc TR0
	tay

	lda RESB+1
	sbc TR1
	bcc LCF02
	sty RESB
	sta RESB+1
	inc RES
LCF02	
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
	sty RESB
	pla
	ldy #0
Lcf23	
	cpy RESB
	bcs Lcf2c
	sta (RES),y
	iny
	bne Lcf23
Lcf2c	
	pha
	tya
	
	ldy #0
	jsr XADRES_ROUTINE
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
	bmi XEFFHI_ROUTINE 
	ora #$80
	
	sta FLGTEL ; Set to Hires flag
	
	php 
	sei
	lda #$1f
	sta $bf67 
	jsr wait_0_3_seconds 
	jsr move_chars_text_to_hires 
	lda #$5c
	ldy #$02
	ldx #0
	jsr ldefd 
	jsr XEFFHI_ROUTINE 
	plp
	rts

XTEXT_ROUTINE	
switch_text
	lda FLGTEL
	bpl LCFA3
	php 
	
	sei
	and #$7f
	sta FLGTEL
	jsr move_chars_hires_to_text 
	lda #$56
	ldy #$02
	ldx #0
	jsr ldefd
	
	lda #$1a
	sta $bfdf
	jsr wait_0_3_seconds
	ldx #$28
	lda #$20

Lcf99	
	sta $bb7f,x
	dex
	bne Lcf99	
	jsr XCSSCR_ROUTINE
	plp
LCFA3	
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
	ror ADDRESS_READ_BETWEEN_BANK
	ldx #0
Lcfb6	
	jsr XTSTBU_ROUTINE 
	bcc Lcfc3 
	txa
	adc #$0b
	tax
	cpx #$30
	bne Lcfb6 
Lcfc3	
	php
	lda #<table_to_define_prompt_charset
	ldy #>table_to_define_prompt_charset
	bcs Lcfce 
	lda #<table_to_define_prompt_charset_empty
	ldy #>table_to_define_prompt_charset_empty
Lcfce	
	bit ADDRESS_READ_BETWEEN_BANK
	bpl Lcfd7 
	jsr Lfef9 
	plp
	rts
Lcfd7	
	jsr Lfef9 
	plp
	rts
	
	
	
	

table_to_define_prompt_charset
	.byt $7f ; char 127
	.byt $00,$00,$08,$3c,$3e,$3c,$08,$00,$00
table_to_define_prompt_charset_empty	
	.byt $7f,$00,$00,$08,$34,$32,$34,$08,$00,$00
	

XNOMFI_ROUTINE
	sta ADDRESS_READ_BETWEEN_BANK
	sty ADDRESS_READ_BETWEEN_BANK+1
	stx RES
	inc RES
	ldy $020c
	sty BUFNOM
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
	cmp #"."
	beq Ld082
	cmp #"*" ; is it '*' ?
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
	cmp #"." ; is it '.' ?
	bne Ld072
	jsr Ld0df 
	bcs Ld08e
	dey
Ld0ae	
	jsr Ld0df 
	bcc Ld0c1 
	lda #$20
LD0B5	
	cpx #3
	beq Ld0a1
	sta $0521,x
	inx
	bne LD0B5
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

CTRL_G_KEYBOARD ; Send oups
	jmp XOUPS_ROUTINE 

CTRL_O_KEYBOARD
	lda $34
	and #$0f
	sta $34
	rts

init_minitel
	LDA #$00
	STA FLGVD0
	STA $3D ; CORRECTME
	STA $37 ; CORRECTME
	rts

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
	JSR XKBDAS_ROUTINE
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
	LDA (ADKBD),Y 
	BIT FLGKBD 
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
	LDA FLGKBD ; CORRECTME
	EOR #$40
	BCS next71
next70
	CMP #$14
	BEQ next72
	CMP #$17
	BNE next73
	LDA FLGKBD
	EOR #$20
	BCS next71
next73
	CMP #$1B
	BNE next74
	LDA FLGKBD
	AND #$20
	BEQ next74
	PLA
	LDA #$00
	PHA
next72
	LDA FLGKBD 
	EOR #$80
next71
	STA FLGKBD
next74
	PLA
	LDX #$00
	JSR XECRBU_ROUTINE
	PLA
	LDX #$00
	JSR XECRBU_ROUTINE
	BIT FLGKBD
	BVC end3
	LDX #<sound_bip_keyboard
	LDY #>sound_bip_keyboard
	JMP send_14_paramaters_to_psg
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
	lda (ADKBD),y
	cmp #$2D
	beq Ld8f8 
	cmp #$3d
	beq Ld8fb 
	pla
	ora #$40
	pha
	lda FLGKBD
	lsr
	bcs Ld900 
	lda (ADKBD),y
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
	JSR routine_to_define_11 
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
	LDA Ld9a9,X
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
	bmi Ld985 
	lda #1
	sta $2a8
	sta $2a6
	php
	sei
	ldx #0
	jsr XLISBU_ROUTINE 
	bcs Ld982 
	sta $279
	ldx #00
	jsr XLISBU_ROUTINE 
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
	sta V1IER
	rts
Ld98d	
	lda V1ACR
	ora #$40
	sta V1ACR
	
	lda #$a8
	ldy #$61
	sta V1T1
	sty $305
	lda #$c0
	sta V1IER
	

flush_keyboard_buffer
	ldx #$00
	jmp XVIDBU_ROUTINE

	
data_to_define_KBDCOL	
Ld9a9	
	.byt $01,$02,$04,$08,$10,$20,$40
	.byt $80
Ld9b1	
routine_to_define_4	
init_keyboard
	LDA #$FF
	STA $0303
	STA KEYBOARD_COUNTER+1
	LDA #$F7
	STA $0302
	LDA #$01
	STA $0273
	STA $0274
	STA KEYBOARD_COUNTER+2
	STA KEYBOARD_COUNTER
	LDA #$0E
	STA $0272
	LDA #<LFA3F
	LDY #>LFA3F
	STA ADKBD
	STY $2B
	LSR $0270
	LDA #$C0
	STA FLGKBD
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
	LDA ADDRESS_READ_BETWEEN_BANK+1
	PHA
	LDA ADDRESS_READ_BETWEEN_BANK
	PHA
	STX ADDRESS_READ_BETWEEN_BANK
	STY ADDRESS_READ_BETWEEN_BANK+1
	PHP
	LDY #$00
ld9f9
	PLP
	PHP
	BCS lda01
	LDA (ADDRESS_READ_BETWEEN_BANK),Y
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
	STA ADDRESS_READ_BETWEEN_BANK
	PLA
	STA ADDRESS_READ_BETWEEN_BANK+1
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
	JMP routine_to_define_11
routine_to_define_8	
	LDA #$50
	STA LPRFX
	LDA #$00


	STA LPRX
	LDA #$80
	STA FLGLPR
	; store hard_copy_routine in $250 in order to jump in
	LDA #<hard_copy_hires 
	LDY #>hard_copy_hires 
	STA $0250 ; LPRVEC ?
	STY $0251 ; LPRVEC ?
	rts
Lda70	
	bmi LDAD2	
XLPRBI_ROUTINE	
Lda72
.(
	PHA
	TXA
	PHA
	LDA #$82
	STA V1IER
	TSX
	LDA $0102,X
	JSR LDAA5 
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
	JSR Ldae4
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
	BEQ LDAB5   ;   non, parrall?le -------------------------------- I
	JSR LDB2F   ;   oui, on pr?pare la RS232                       I I
	TXA         ;   donn?e dans A                                  I I
	LDX #$18    ;   X=buffer ACIA sortie                           I I
	BNE LDABF  ;  --inconditionnel                                 I I
LDAB5
	LDA FLGTEL   ; I imprimante d?tect?e ? <------------------------- I
	AND #$02   ;  I                                                  I
	BEQ LDAD1   ;  I non, on sort                                     I
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
	JSR XECRBU_ROUTINE 
	pla 
	bcs next906
LDAD1
	rts
LDAD2	
	
	BCS next908
	LDA FLGLPR
	AND #$04
	BNE Ldae1	

	LDA #$82
	STA V1IER
next908
	RTS


Ldae1
	jmp LDB7D 

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
LDAF7	
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
	BMI LDB3A ;     ouverture-fermeture ---------------------------- I
	TAX       ;     donn?e dans X                                  I I
	BPL LDB26 ;      si <128, on passe ----------------------       I I
	CMP #$C0  ;      sinon, c'est <128+64 ?                 I       I I
	BCS LDB26 ;                                             I       I I
	ORA #$40    ;    oui, on force b7                       I       I I
	PHA                          ;                         I       I I
	LDA #$1B   ;    et on envoie un ESC avant              I       I I
	LDX #$18 ;      la donn?e                              I       I I
	JSR XECRBU_ROUTINE ;                                            I       I I
	PLA       ;                                            I       I I
LDB26
	PHA       ;     <---------------------------------------       I I
	LDX #$18  ;     on envoie la donn?e                            I I
	JSR XECRBU_ROUTINE ;     dans le BUFFER ACIA sortie                     I I
	PLA                                                        ;   I I
	BCS LDB26 ;     si la donn?e n'a pas ?t? ?crite, on boucle     I I
LDB2F
	LDA ACIACR  ;    on prend V2IER                                 I I
	AND #$F3  ;     %11110011 force b2 ? 0                         I I
	ORA #$04 ;      et b3 ? 1                                      I I
	STA ACIACR ;     dans ACIACR                                    I I
	RTS
	;                                      < I
LDB3A
	BCS LDB53 ;     C=1 on ferme ==================================  I
	LDA ACIACR ;     ouverture                                      > I
	AND #$02 ;      ACIACR ? 0 sauf b1                             I I
	ORA #$65 ;      %01101001, bits forc?s ? 1                     I I
Ldb43
	STA ACIACR ;     dans ACIACR <----------------------------------+--
	LDA V2DRA ;     V2DRA                                          I  
	AND #$EF  ;     %11101111 force mode MINITEL                   I  

	STA V2DRA ;                                                    I  
	LDA #$38  ;     %00111000 dans ACIACT                          I  
	STA ACIACT ;                                                    I  
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
	BPL LDAF7    ;  lecture, voir MINITEL (pourquoi pas $DAF9 ?)       
	BCS Ldb09   ;   C=1, on ferme                                    
	LDA ACIACR   ;   on ouvre                                          
	AND #$0D     ;  on fixe le mode de controle
LDB66	
	ORA $5A     ;   de la RS232                                       
	STA ACIACR                                                        
	LDA V2DRA                                                        
	ORA #$10     ;  %00010000 on force RS232                          
	STA V2DRA                                                        
	LDA $59     ;   et on fixe le mode de transmission                
	STA ACIACT   ;   dans ACIACR                                       
	RTS                                                              
                           
                                                                                
   ;                      GESTION DE LA SORTIE RS232                         
LDB79
	BPL LDB26     ; ?criture, comme MINITEL                           
	BCS LDB53    ;;  pas de fermeture (RTS) 
LDB7D	
	LDA ACIACR    ;  ouverture,on lit ACIACR                            
	AND #$02     ;  isole b1                                          
	ORA #$05     ;  %00000101 force b0 et b2 ? 1                      
	BNE LDB66    ;  inconditionnel        	

                                                                               
     ;                 GESTION DES SORTIES EN MODE TEXT                      
                                                                                
;Principe:tellement habituel que ?a en devient monotone... mais bien pratique !  
LDB86
	PHA       ;     on sauve A et P                                   
	PHP                                                              
	LDA #$00    ;   fen?tre 0                                         
	BEQ LDB9C   ;   inconditionnel                                    
LDB8C
	PHA                                                              
	PHP                                                              
	LDA #$01    ;   fen?tre 1                                         
	BNE LDB9C    ;                                                 

LDB92	
	PHA                                                              
	PHP                                                              
	LDA #$02     ;  fen?tre 2                                         
	BNE LDB9C                              ;           
LDB98
	PHA                                                              
	PHP                                                              
	LDA #$03      ; fen?tre 3                                         
LDB9C
	STA $28       ; stocke la fen?tre dans SCRNB                      
	PLP          ;  on lit la commande                                
	BPL LDBA4    ;  ?criture -------    
	JMP LDECE    ;  ouverture      I      
LDBA4
	PLA          ;  on lit la donn?e <                                
	STA $29      ;  que l'on sauve                                    
	LDA FLGLPR    ;  ?cho sur imprimante ?                             
	AND #$02                                                         
	BEQ LDBB3     ; non --------------------------------------        
	LDA $29      ;  oui, on envoie le code sur imprimante    I        
	JSR Lda72   ;                                           I  
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
	LDA #>LDC2B-1 ; FIXME ?
	PHA
	LDA #<LDC2B-1 ; FIXME ?
	PHA
	LDA SCRNB+1
	ASL ; MULT2 in order to get vector 
	TAY
	LDA TABLE_OF_SHORTCUT_KEYBOARD+1,Y 
	PHA
	LDA TABLE_OF_SHORTCUT_KEYBOARD	,Y 
	PHA
	LDA #$00
	SEC
	RTS



TABLE_OF_SHORTCUT_KEYBOARD	
Ldbeb
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1  ; Nothing
LDBED	
	.byt <CTRL_A_START-1,>CTRL_A_START-1 ; CTRL A tabulation 
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1 
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1; Already managed  
	.byt <CTRL_D_START-1,>CTRL_D_START-1 
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1; E
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1 ; F Already managed 
	.byt <CTRL_G_START-1,>CTRL_G_START-1 ;G
	.byt <CTRL_H_START-1,>CTRL_H_START-1 ;  H
	.byt <CTRL_I_START-1,>CTRL_I_START-1 ; I
	.byt <CTRL_J_START-1,>CTRL_J_START-1 ;
	.byt <CTRL_K_START-1,>CTRL_K_START-1 ; 
	.byt <CTRL_L_START-1,>CTRL_L_START-1 ;
	.byt <CTRL_M_START-1,>CTRL_M_START-1 ; M
	.byt <CTRL_N_START-1,>CTRL_N_START-1 ;  N
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1;  O
	.byt <CTRL_P_START-1,>CTRL_P_START-1 ;P
	.byt <CTRL_Q_START-1,>CTRL_Q_START-1 ;
	.byt <CTRL_R_START-1,>CTRL_R_START-1 ;  R
	.byt <CTRL_S_START-1,>CTRL_S_START-1 ;S 
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1 ;  T
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1; U 
	.byt <CTRL_V_START-1   ,>CTRL_V_START-1  ; V 
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1 ; W
	.byt <CTRL_X_START-1,>CTRL_X_START-1 ; X
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1; Y
	.byt <KEYBOARD_NO_SHORTCUT-1,>KEYBOARD_NO_SHORTCUT-1;  Z 
	.byt <CTRL_ESC_START-1,>CTRL_ESC_START-1 ;  ESC
	.byt <CTRL_ESC_START-1,>CTRL_ESC_START-1 ; ??? Like ESC
	.byt <CTRL_CROCHET_START-1,>CTRL_CROCHET_START-1 ;  CTRL ]
	.byt <CTRL_HOME_START-1  ,>CTRL_HOME_START  -1 ;  HOME
	.byt <CTRL_US_START-1,>CTRL_US_START-1 ;  US 


	
	
LDC2B
	
	ldx $28
	ldy $0220,x
	lda ($26),y
	sta $024c,x
	lda $26
	sta $0218,x
	lda $27
	sta $021c,x
	pla
	sta $0248,x
	jsr LDE2D 
LDC46	
	pla
	tay
	pla
	tax
	pla
	rts
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
LDC69
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
	beq LDCB8 
	lda $29
	bmi LDC46 
	cmp #$40
	bcc LDC46
	and #$1f

	JSR LDC69   ;  on le place ? l'?cran                            I
	LDA #$09     ;   on d?place le curseur ? droite                   I
	JSR Ldbb5  ;                                                     I
	LDA #$1B   ;     on envoie un ESC (fin de ESC)                    I
	JSR Ldbb5   ;                                                    I
	JMP LDC46   ;   et on sort                                        I
LDCB8
	LDA $0248,X ;   US, on lit FLGSCR <-------------------------------
	PHA         ;   que l'on sauve                                    
	JSR lde1e   ;   on ?teint le curseur                            
	PLA         ;   on prend FLGSCR                                   
	PHA                                                              
	LSR         ;   doit-on envoyer Y ou X ?                          
	BCS LDCDC   ;   X ------------------------------------------------

	LDA $29     ;   on lit Y                                         I
	AND #$3F    ;   on vire b4 (protocole US)                        I
	STA SCRY,X ;   et on fixe Y                                     I
	JSR LDE07   ;  on ajuste l'adresse dans la fen?tre              I
	STA $0218,X ;   dans ADSCRL                                      I
	TYA         ;                                                    I
	STA $021C,X  ;  et ADSCRH                                        I
	PLA         ;   on indique prochain code pour X                  I
	ORA #$01    ;                                                    I
	PHA        ;                                                     I
	JMP LDC2B   ; et on sort                                       I
LDCDC
	LDA $29      ;  on lit X <----------------------------------------
	AND #$3F    ;   on vire b4                                        
	STA SCRX,X ;   dans SCRX                                         
	PLA                                                              
	AND #$FA    ;   on indique fin de US                              
	PHA                                                              
	JMP LDC2B   ;  et on sort
KEYBOARD_NO_SHORTCUT ; USED TO RTS keyboard shortcut not managed	
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
  
  
CTRL_A_START
LDCEB
	LDA SCRX,X ; ->on lit ala colonne                                
	AND #$F8     ;I on la met ? 0                                     
	ADC #$07     ;I on place sur une tabulation 8 (C=1)               
	CMP SCRFX,X ; I est-on en fin de ligne ?                          
	BEQ LDD09  ; I non                                               
	BCC LDD09 ;  I --------------------------------------------------

	JSR LDD67 ;;  I oui, on ram?ne le curseur en d?but de ligne      I

	JSR LDD9D  ;  I et on passe une ligne                            I
	LDX $28     ; I                                                  I

	LDA SCRX,X ; I on prend la colonne                              I
	AND #$07   ;  I est-on sur une tabulation                        I
	BNE LDCEB  ;   --non, on tabule...                                I
	RTS       ;                                                      I
LDD09
	STA SCRX,X ;   on sauve la colonne <-----------------------------
	RTS         ;   et on sort codes A                               I
                                                                                
;                             CODE 4 - CTRL D                               
CTRL_D_START
LDD0D                                                                                
	ROR          ;  on pr?pare masque %00000010                       
                                                                                
 ;                               CODE 31 - US                                
CTRL_US_START
                              ;on pr?pare masque %00000100                       
LDD0E							  
	ROR                                                              
;                               CODE 27 - ESC                                
CTRL_ESC_START
 ;                             on pr?pare masque %00001000                       
LDD0F
	ROR                                                              
 ;                             CODE 29 - CTRL ]                              

  ;                            on pr?pare masque %00010000   
CTRL_CROCHET_START  
LDD10  
	ROR                                                              
                                                                                
    ;                          CODE 22 - CTRL V                              
CTRL_V_START
LDD11
	ROR           ; on pr?pare masque %00100000                       
                                                                                
     ;                         CODE 16 - CTRL P                              
CTRL_P_START
LDD12
	ROR  ;           on pr?pare masque %01000000                       
                                                                                
          ;                    CODE 17 - CTRL Q                              
CTRL_Q_START
  
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
	BNE LDD24   ;     oui ----------------------------------------------
	RTS         ;   non on sort                                      I
LDD24
	LDX $28     ;   on prend le num?ro de fen?tre <-------------------
	AND RES      ;  mode monochrome (ou 40 colonnes) ?                
	BEQ LDD3C    ;   oui ----------------------------------------------
	INC SCRDX,X  ;  non, on interdit la premi?re colonne             I
	INC SCRDX,X  ;  et la deuxi?me                                   I
	LDA SCRX,X  ;  est-on dans une colonne                          I
	CMP SCRDX,X  ;  interdite ?                                      I
	BCS LDD3B  ; ---non                                               I
	JMP LDD67  ;  I  oui,on en sort                                    I
LDD3B
	RTS   ;  <---                                                    I
LDD3C
	DEC SCRDX,X ;   on autorise colonne 0 et 1 <----------------------
	DEC SCRDX,X                                                      
	RTS     	
LDD43	
	DEC SCRX,X  ;  on ram?ne le curseur un cran ? gauche  <----------
	RTS  ;                                                           I

 ;                             CODE 8 - CTRL H                              I
 ;                                                                              I
;Action:d?place le curseur vers la gauche                                       I
CTRL_H_START
LDD47  
	LDA SCRX,X   ; est-on d?ja au d?but de la fen?tre ?             I
	CMP SCRDX,X  ;                                                   I
	BNE LDD43    ;  non, on ram?ne ? gauche --------------------------
	LDA SCRFX,X  ;  oui, on se place ? la fin de la fen?tre           
	STA SCRX,X                                                      


	
;                              CODE 11 - CTRL K                               
                                                                                
;Action:d?place le curseur vers le haut                                          
CTRL_K_START
LDD55                                                                                
	LDA SCRY,X ;   et si on est pas                                  
	CMP SCRDY,X    ;au sommet de la fen?tre,                          
	BNE LDD6E   ;   on remonte d'une ligne ---------------------------
	LDA SCRDY,X ;   X et Y contiennent le d?but et la                I
	LDY SCRFY,X  ;  fin de la fen?tre X                              I
	TAX          ;                                                   I
	JSR XSCROB_ROUTINE    ;  on scrolle l'?cran vers le bas ligne X ? Y       I
CTRL_M_START
LDD67
	LDA SCRDX,X  ;  on place d?but de la fen?tre dans X              I
	STA SCRX,X  ;                                                   I
	RTS          ;                                                   I
LDD6E
	DEC SCRY,X   ; on remontre le curseur <--------------------------
	JMP LDE07    ;  et on ajuste ADSCR     	
	
	
	
;                              CODE 14 - CTRL N                              
                                                                                
;Action:efface la ligne courante                                                 
CTRL_N_START
LDD74                                                                              
	LDY SCRDX,X ;    on prend la premi?re colonne de la fenetre        
	JMP LDD7D   ;    et on efface ce qui suit (BPL aurait ?t? mieux...)
                       
                                                                          
 ;                             CODE 24 - CTRL X                              

 
                                                                             
;Action:efface la fin de la ligne courante                                       
CTRL_X_START
LDD7A                                                                                
	LDY SCRX,X  ;  on prend la colonne du curseur                    
LDD7D
	LDA SCRFX,X  ;  et la derni?re colonne de la fen?tre              
	STA $29      ;  dans $29 

	LDA #$20     ;  on envoie un espace                               
LDD84
	STA (ADSCR),Y                                                      
	INY           ; jusqu'? la fin de la ligne                        
	CPY $29                                                          
	BCC LDD84                                                       
	STA (ADSCR),Y   ; et ? la derni?re position aussi                   
	RTS           ; (INC $29 avant la boucle aurait ?t? mieux !)      
LDD8E
	INC SCRX,X                                                      
	RTS                                                              
	
	
	
 ;                             CODE 9 - CTRL I                               
                                                                                
;Action:d?place le curseur ? droite                                              
CTRL_I_START
LDD92                                                                              
	LDA SCRX,X  ;  on lit la colonne du curseur                      
	CMP SCRFX,X   ; derni?re colonne ?                                
	BNE LDD8E     ;   non, on d?place le curseur                        
	JSR LDD67   ;    oui, on revient ? la premi?re colonne     

	

                      ;        CODE 10 - CTRL J                              
                                                                                
;Action:d?place le curseur vers la droite                                        
CTRL_J_START
LDD9D                                                                               
	LDA SCRY,X  ;  on est en bas de la fen?tre ?                     
	CMP SCRFY,X  ;                                                    
	BNE LDDB2    ;  non ----------------------------------------------
	LDA SCRDY,X  ;  oui, X et Y contiennent d?but et fin de fen?tre  I
	LDY SCRFY,X  ;                                                   I
	TAX          ;                                                   I
	JSR Lde54    ;  on scrolle la fen?tre                            I
	JMP LDD67     ; on revient en d?but de ligne                     I
LDDB2	
	INC SCRY,X  ;  on incr?mente la ligne <-------------------------I 
	JMP LDE07    ;  et on ajuste ADSCR                      
     	
	

     ;                         CODE 12 - CTRL L                              
                                                                                
;Action:Efface la fen?tre                                                     
CTRL_L_START
LDDB8                                                                               
	JSR LDDFB    ;  on remet le curseur en haut de la fen?tre         
LDDBB
	JSR LDD74    ;  on efface la ligne courante                       
	LDA SCRY,X   ; on est ? la fin de la fen?tre ?                   
	CMP SCRFY,X  ;                                                     
	BEQ LDDFB    ;  oui, on sort en repla?ant le curseur en haut     
	JSR LDD9D    ;  non, on d?place le curseur vers le bas            
	JMP LDDBB     ; et on boucle  (Et BPL, non ?!?!)                  
		
	
	
                            ;  CODE 19 - CTRL S                              
CTRL_S_START

LDDCC                                                                             
	JMP XHCSCR_ROUTINE    ;  on ex?cute un HCOPY     	


	; CODE 18 - CTRL R   	
;Action:bascule l'?cho imprimante du clavier  
                                   
CTRL_R_START
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
CTRL_G_START 
 
	LDX #<XOUPS_DATA    ;   on indexe les 14 donn?es du OUPS                  
	LDY #>XOUPS_DATA                                                         
	JSR send_14_paramaters_to_psg   ;   et on envoie au PSG                               
	LDY #$60    ;   I                                                 
	LDX #$00    ;   I                                                
LDDE3
	DEX        ;    I D?lai d'une seconde                             
	BNE LDDE3    ;  I                                                 
	DEY          ;  I                                                 
	BNE LDDE3    ;  I                                                 
	LDA #$07     ;  un JMP $DA4F suffisait ...                        
	LDX #$3F                                                         
	JMP routine_to_define_11
XOUPS_DATA
LDDF0                                                                               
	.byt $46,00,00,00,00,00;  p?riode 1,12 ms, fr?quence 880 Hz (LA 4) 
LDDF6
	.byt  00,$3E,$0F,00,00  ;  canal 1, volume 15 musical  
/*

                           INITIALISE UNE FENETRE                           
                                                                                
Action:on place le curseur en (0,0) et on calcule son adresse                   
  */
CTRL_HOME_START  
LDDFB
	LDA SCRDX,X  ;  on prend la premi?re colonne                      
	STA SCRX,X  ;  dans SCRX                                         
	LDA SCRDY,X  ;  la premi?re ligne dans                            
	STA $0224,X  ;  SCRY                                              
LDE07
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
	JSR XMUL40_ROUTINE    ;  RES=A*40                                          
	LDA $0238,X  ;  AY=adresse de la fen?tre                          
	LDY $023C,X                                                      
	JMP XADRES_ROUTINE     ; on calcule dans RES l'adresse de la ligne   
	
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
LDE2D	
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
	STA DECFIN+1                                                          
	LDA #$28                                                         
	BNE LDE62    ;  inconditionnel                                    
 /*                                                                               
                                                                              
                      SCROLLE UNE FENETRE VERS LE HAUT                      
                                                                                
                                                                                
Action:scrolle vers le haut de la ligne X ? la ligne Y la fen?tre courante.     
                                                                                
                                                                                */



																				
LDE5C																				
XSCROB_ROUTINE
	LDA #$FF   ;    on prend $FFD8, soit -40 en compl?ment ? 2        fixme 
	STA DECFIN+1                                                          
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
	JSR LDE12   ;   on calcule l'adresse de la ligne                 
	CLC                                                              
	ADC SCRDX,X ;   l'adresse exacte de la ligne dans la fen?tre      
	BCC LDE7D                                                       
	INY
LDE7D	
	STA DECCIB      ;  est dans $08-09                                   
	STY DECCIB+1                                                          
	CLC           ; on ajoute le d?placement                          
	ADC $06                                                          
	STA $04                                                          
	TYA                                                              
	ADC DECFIN+1                                                          
	STA $05     ;   dans $04-05                                       
	PLA         ;   on sort le nombre de lignes                       
	STA RES     ;   dans RES                                          
	BEQ LDEC4   ;   si nul on fait n'importe quoi ! on devrait sortir!
	BMI LDECD    ;  si n?gatif, on sort ------------------------------
	SEC          ;  on calcule                                       I
	LDX $28     ;                                                    I
	LDA SCRFX,X   ; la largeur de la fen?tre                         I
	SBC SCRDX,X  ;                                                   I
	STA RES+1      ;  dans RES+1                                       I
LDE9D
	LDY RES+1 
LDE9F ;                                                  I
	LDA ($04),Y ;   on transf?re une ligne                           I
	STA (DECCIB),Y ;                                                    I
	DEY         ;                                                    I
	BPL LDE9F    ;                                                    I
	CLC        ;                                                     I
	LDA $04     ;   on ajoute le d?placement                         I
	ADC $06     ;   ? l'adresse de base                              I
	STA $04     ;                                                    I
	LDA $05     ;                                                    I
	ADC DECFIN+1     ;                                                    I
	STA $05      ;                                                   I
	CLC          ;                                                   I
	LDA DECCIB     ;   et ? l'adresse d'arriv?e                         I
	ADC $06     ;                                                    I
	STA DECCIB     ;                                                    I
	LDA DECCIB+1     ;                                                    I
	ADC DECFIN+1      ;                                                   I
	STA DECCIB+1      ;                                                   I
	DEC RES      ;  on d?compte une ligne de faite                   I
	BNE LDE9D    ;  et on fait toutes les lignes                     I
LDEC4
	LDY RES+1      ;  on remplit la derni?re ligne                     I
	LDA #$20     ;                                                   I
LDEC8
	STA (DECCIB),Y  ;  avec de espaces                                  I
	DEY          ;                                                   I
	BPL LDEC8    ;                                                   I
LDECD
	RTS          ;  <-------------------------------------------------
 
/*
                                     ???                                     
                                                                                
Action:inconnue... ne semble pas ?tre appel?e et utilise des variables          
       IRQ dont on ne sait rien.      

Note de Jede : si utilis?e chercher le label LDECE	   
 */
LDECE 
	BCC LDED7    ;  si C=0 on passe ------------                      
	LDX $28      ;                             I                      
	JSR lde1e    ;  on ?teint le curseur       I                      
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
	STA ADDRESS_READ_BETWEEN_BANK ; CORRECTME
	STY ADDRESS_READ_BETWEEN_BANK+1 ; CORRECTME
	TXA
	CLC
	ADC #$18
	TAX
	LDY #$05
next18
	PLP
	PHP
	BCS next16
	LDA (ADDRESS_READ_BETWEEN_BANK),Y
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
	LDA #<LDEF5
	LDY #>LDEF5
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
	sec
	rts
	
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
	STA VABKP1 ; CORRECTME
	LDA JCGVAL
	AND #$1B
	ORA VABKP1 ; CORRECTME
	STA JCGVAL
	LDA VABKP1 ; CORRECTME
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
	STA VABKP1 ; CORRECTME
	LDA JCGVAL
	AND #$1B
	EOR VABKP1
	BNE Le062
	DEC $0291
	BNE Le084
	LDX $0297 ; CORRECTME
	JMP Le065
Le05b
	JSR Ldf90 
	AND #$1B
	STA VABKP1 ; CORRECTME
Le062
	LDX $0298 ; CORRECTME
Le065
	STX $0291 ; CORRECTME
	LDA JCGVAL
	AND #$04
	ORA VABKP1 ; CORRECTME
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
	STA VABKP1     ;   dans VABKP1                                          
	CMP #$1B    ;   la souris bouge ?                                 
	BNE LE095   ;   non ---------------------------------------------- 
	DEC $02A4   ;   on d?place ?                                     I
	BNE Le084   ;   non, on sort.                                    I 
LE095	
	LDA $02A5    ;  on place vitesse d?placement dans  <--------------
	STA $02A4    ;  $2A4                                              
	LDA VABKP1     ;   on lit le code                                    
	CMP #$1B    ;   souris fixe ?                                     
	BEQ LE0B5    ;  oui ----------------------------------------------
	AND #$1B     ;  non, on isole les valeurs direction              I
	EOR JCDVAL   ;   et on retourne les bits de JCDVAL                I
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
	STA VABKP1  ;      dans VABKP1                                          
	LDA JCDVAL ;     on prend JDCVAL                                   
	AND #$64 ;      %01100100, on isole les bits de Feu               
	ORA VABKP1   ;     on ajoute les bits de direction                   
	STA JCDVAL  ;    dans JDCVAL                                       
	LDA VABKP1   ;                                                       
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
	STA VABKP1 ; CORRECTME
	STX $0294 ; CORRECTME
	LDA JCDVAL
	AND #$7B
	ORA VABKP1
	STA JCDVAL
	LDA VABKP1
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
	STA VABKP1 ; CORRECTME
	LDA JCDVAL
	AND #$5F
	ORA VABKP1 ; CORRECTME
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
	STA VABKP1
	LDA JCDVAL
	AND #$3F ; CORRECTME
	ORA VABKP1 
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
	STX VABKP1 
	LDX #$00
	JSR XECRBU_ROUTINE 
	LDA #$08
	PLP
	BCS Le1af
	LDA #$20
Le1af
	LDX #$00
	JSR XECRBU_ROUTINE 
	LDX VABKP1   
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
	CMP SCRFX,X
	BEQ Le1eb 
Le1e3
	LDA #$09
	JSR Ldbb5 
	JMP Le1cb 
	
; minitel hard_copy videotex
; send on the printer hires screen in videotex mode
Le1eb
.(
	;removeme minitel



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
LE25E	
	LDA data_for_hard_copy-1,X ; Why not data_for_hard_copy instead of data_for_hard_copy+1 ? Because this routine start with X=5 and did a dex/bne instead of bpl/ldx #4. It was $e240 which is "rts"
	JSR Lda72 
	DEX
	BNE LE25E 
	STX TR0
LE269	
	LDX #$06
LE26B	
	LDA data_for_hard_copy+4,X
	JSR Lda72 
	DEX
	BNE LE26B
	STX TR1
LE276	
	LDA #$05
	STA $0E
LE27A	
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
	LDY TR1
LE290	
	LDA ($11),Y
	TAX
	AND #$40
	BNE LE29B 
	TXA
	AND #$80
	TAX
LE29B	
	TXA
	BPL LE2A0
	EOR #$3F
LE2A0	
	LDX $0E
LE2A2	
	LSR
	DEX
	BPL LE2A2 
	ROL $0F
	TYA
	CLC
	ADC #$28
	TAY
	BCC LE2B1 
	INC $12
LE2B1	
	DEC $10
	BNE LE290 
	LDA $0F
	JSR Lda72
	DEC $0E
	BPL LE27A 
	INC TR1
	LDA TR1
	CMP #$28
	BNE LE276 
	INC TR0
	LDA TR0
	CMP #$19
	BNE LE269 
	LDX #$04
LE2D0	
	LDA data_for_hard_copy+10,X
	JSR Lda72 
	DEX
	BNE LE2D0 
	PLA
	STA FLGLPR
	RTS
Le2de
put_cursor_on_last_char_of_the_line
	LDY SCRFX,X
	.byt $24
LE2E2	
	dey
	LDA (RES),Y
	CMP #$20
	BNE test_if_prompt_is_on_beginning_of_the_line
	TYA
	CMP SCRDX,X
	BNE LE2E2 
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
	JSR LDE12 
	JSR Le2f9 ;

	beq Le302
	lda $61
	CMP SCRDY,X
	
	BEQ Le306
	DEC $61
	bcs Le2ed
Le302	
	CLC
	INY
	STY ACC1E
Le306	
	RTS
put_cursor_on_beginning_of_the_line	
Le322	
	LDX $28
	LDA SCRY,X
	STA $63
	JSR LDE12
	JSR put_cursor_on_last_char_of_the_line
Le32f	
	STY $62
	BEQ Le34e 
	LDA $63
	CMP SCRFY,X
	BEQ Le34d 
	INC $63
	LDA $63
	JSR LDE12
	JSR Le2f9 
	BEQ Le34b 
	JSR put_cursor_on_last_char_of_the_line 
	BNE Le32f 
Le34b	
	DEC $63
Le34d	
	RTS
Le34e	
	rts


LE34F
	JSR LE301 
	JMP LE361
send_the_end_of_line_in_bufedt	
LE355	
	LDX $28
	LDA SCRX,X
	STA ACC1E
	LDA SCRY,X
	STA $61
LE361	
	JSR Le322
		
	LDA $61
	STA $65
	CMP $63
	BNE Le378

	LDA $62
	CMP ACC1E
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
	JSR LDE12
	LDY ACC1E
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
	CMP SCRFX,X
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
	JSR LE656
Le405	
	TYA
	INY
	LDX $28
	CMP SCRFX,X
	BNE Le418
	LDA #$28
	LDY #$00
	JSR XADRES_ROUTINE 
	LDY SCRDX,X
Le418	
	INC $64
	BNE Le3e3
Le41c	
	BIT FLGTEL ; Minitel ?
	BVC Le42a 
	LDX SCRX
	LDY SCRY

	JSR Le62a 
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
	JSR LE301 
	LDX ACC1E
	LDY $61
	JSR Le62a 
Le446	
	LDA #$0D
	JSR Le648 
	JSR XECRPR_ROUTINE  
	PLA
	TAX
	BEQ Le45a
Le452	
	LDA #$09
	JSR Le648  
	DEX
	BNE Le452  
Le45a	
	LDX $28
	LDA $0248,X
	BMI Le466 
	LDA #$11
	JSR Le648 
Le466
	JSR test_if_all_buffers_are_empty 
	JSR XRD0_ROUTINE 
	BCS Le466 
	PHA
	LDA #$11
	JSR Le648  
	PLA
	CMP #$0D
	BNE Le4bc
Le479	
	PHA
	JSR LE34F  
	PLA
	PHA
	CMP #$0B
	BEQ Le494  
	LDX $62
	LDY $63
	JSR Le62a 
	LDA #$0D
	JSR Le648  
	LDA #$0A
	JSR Le648 
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
	JSR LE301
	LDX ACC1E
	LDY $61
	JSR Le62a 
	JMP LE4D5 
Le4d1	
	CMP #$18
	BNE Le4df
LE4D5	
	JSR send_the_end_of_line_in_bufedt
	SEC
	JSR display_bufedt_content 
	JMP Le45a
Le4df	
	CMP #$7F
	BNE Le52a
	LDA $0278
	LSR
	BCS Le4f3
	BCC Le4eb ; awful because it is the next instruction : 2 bytes lost !
Le4eb	
	LDA #$08
	.byt $2c
Le4ee	
	lda #9
	
	JSR Le648
Le4f3	
	LDX $28
	LDA $024C,X
	CMP #$7F
	BEQ Le4ee 
	JSR send_the_end_of_line_in_bufedt
	LDA BUFEDT
	BNE Le511
	LDA #$20
	JSR Le648
	LDA #$08
	JSR Le648 
	JMP Le45a

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
	JSR display_bufedt_content 
	JMP Le45a 
Le52a	
	CMP #$20
	BCC Le534
	JSR XEDTIN_ROUTINE 
	JMP Le45a 
Le534	
	JMP manage_code_control 
manage_normal_char

XEDTIN_ROUTINE
	TAY
	TXA
	PHA
	TYA
	PHA
	JSR send_the_end_of_line_in_bufedt 
	LDA $62
	LDY BUFEDT
	BNE  Le548
	LDA ACC1E
Le548	
	LDX $28
	CMP SCRFX,X
	BNE Le5ae
	LDA $63
	CMP SCRFY,X
	BEQ Le5ae
	ADC #$01
	JSR LDE12 
	JSR Le2f9 
	BNE Le5ae
	LDY SCRFY,X
	LDX $63
	INX
	JSR XSCROB_ROUTINE
	BIT FLGTEL ; Minitel ?
	BVC Le5ae
	LDX #$00
	LDY $63
	INY
	JSR Le62a 
	LDA #$18
	JSR LE656 
	LDA #$0A
	JSR Le648
Le580	
	LDX $28
	LDA $024C,X
	CMP #$7F
	BNE Le58f
	JSR XECRPR_ROUTINE
	JMP LE597
Le58f	
	JSR LE656
	LDA #$09
	JSR Ldbb5
LE597	
	LDA SCRY
	CMP SCRFY,X
	BNE Le580
	
	LDA SCRX
	CMP SCRFX,X
	BNE Le580
	
	LDY $61
	LDX ACC1E
	JSR Le62a 
Le5ae	
	PLA
	JSR Le648 
	CLC 
	JSR display_bufedt_content
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
	JSR Le648 
	JMP Le45a 
Le5cb	
	JSR LE301 
	LDX ACC1E
Le5d2	
	LDY $61
	JMP LE5E7
Le5d5
	CMP #$09
	BNE Le5ee
	PHA
	LDA $0278
	LSR
	BCC Le5c4
	JSR Le322 
	LDX $62
	LDY $63
LE5E7	
	PLA
	
	JSR Le62a 
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
	JSR Ldbb5                                                       
	BIT FLGTEL     ; mode minitel ?                                    
	BVC LE66B     ; non                                              
	INX           ; on ajoute une colonne                             
	TXA           ; dans A                                            
	DEX           ; et on revient en arri?re                          
	ORA #$40     ;  on ajoute 40                                      
	JMP LE656    ;  et on envoie au minitel

	/*
                   ENVOIE UN CODE SUR LE TERMINAL VIDEO                    
                                                                                
Action:envoie un code sur l'?cran et ?ventuellement sur le minitel s'il est     
       actif comme sortie vid?o. Seule la fen?tre 0 est g?r?e, ce qui ote       
       d?finitivement tout espoir de gestion d'entr?e de commande sur une autre 
       fen?tre.                                                                 
      */                                                                          
Le648                                                                        
	BIT FLGTEL    ;  mode minitel ?                                    
	BVC LE650    ;  non ---------------------------------------------- 
	JSR LE656    ;  oui, on envoie le code au minitel                I 
LE650
	BIT LE650    ;  V=0 et N=0 pour ?criture <------------------------ 
	JMP LDB86    ;  dans la fen?tre 0                               
                                                                                
/*
                 ENVOIE UN CODE AU BUFFER SERIE SORTIE                    
  */
LE656
	STA TR0    ;    on sauve le code <--------------------------------
	TYA        ;    on sauve Y                                       I
	PHA        ;                                                     I
	TXA         ;   et X                                             I
	PHA         ;                                                    I
	LDX #$18   ;    on indexe buffer ACIA sortie (minitel sortie)    I
	LDA TR0     ;   on envoie le code                                I
	JSR XECRBU_ROUTINE   ;                                                    I
	PLA         ;   on restaure les registres                        I
	TAX        ;                                                     I
	PLA        ;                                                     I
	TAY         ;                                                    I
	LDA TR0     ;                                                    I
	BCS LE656   ;   si l'envoi s'est mal pass?, on recommence --------
LE66B
	RTS                     
	/*																			
                             AFFICHE LE PROMPT                              
      */  
Le66c
XECRPR_ROUTINE 
	BIT FLGTEL  ;    mode minitel ?                                    
	BVC LE67B   ;   non ---------------------------------------------- 
	LDA #$19   ;    on envoie SS2 2/E au minitel                     I
	JSR LE656  ;                                                     I
	LDA #$2E   ;    donc on affiche la fl?che ->                     I
	JSR LE656  ;                       E                              I
LE67B
	LDA #$7F   ;    on affiche un prompt <----------------------------
	JMP Ldbb5   ;   ? l'?cran               
																				
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
	STX RESB+1   ;     dans RESB                                         
LE686
	STA RESB   ;  -->                                                  
	LDY #$00   ; I                                                    
	LDA (RESB),Y ;I  on lit la longeur de la ligne                     
	BEQ LE6AE   ;I  0, on sort --------------------------------------- 
	TAX         ;I  on sauve la longueur dans X                      I
	LDY #$02    ;I  on lit le num?ro de la ligne                     I
	LDA RES+1     ;I                                                   I
	CMP (RESB),Y ;I  poids fort lu ?gal au demand? ?                  I
	BCC LE6AE   ;I  sup?rieur, on sort ------------------------------O  
	BEQ LE69B   ;I  Egal, on continue le test                        I 
	BCS LE6A4   ;I  inf?rieur, on passe --------------------------   I 
LE69B
	DEY         ;I  on lit le poids faible                       I   I
	LDA RES     ;I                                               I   I
	CMP (RESB),Y ;I  poids faible lu ?gal au demand? ?            I   I
	BCC LE6AE  ;I  sup?rieur, on sort --------------------------+---O 
	BEQ LE6AF   ;I  ?gal, on sort avec C=1                       I   I 
LE6A4
	CLC         ;I  <---------------------------------------------   I
	TXA         ;I                                                   I
	ADC RESB     ;I  on passe la ligne                                I
	BCC LE686   ;I                                                   I 
	INC RESB+1     ;I                                                   I
	BCS LE686   ;---et on continue                                   I 
LE6AE
	CLC         ;   C=0, ligne non trouv?e <--------------------------
LE6AF
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
	JSR XSCELG_ROUTINE   ;   cherche le num?ro de la ligne ? ins?rer         
	BCC LE6E7   ;   la ligne n'existe pas ---------------------------- 
	STX $0F     ;   on sauve la longueur de la ligne trouv?e         I
	LDA $5E     ;   on met SCEFIN                                    I
	LDY $5F     ;                                                    I
	STA $06     ;   dans DECFIN                                      I
	STY DECFIN+1     ;                                                    I
	LDA RESB     ;   adresse de la ligne trouv?e                      I
	LDY RESB+1     ;                                                    I
	STA DECCIB     ;   dans DECCIB                                      I
	STY DECCIB+1     ;                                                    I

	CLC         ;                                                    I
	TXA        ;                                                     I
	ADC RESB     ;                                                    I
	BCC LE6D6  ;    et l'adresse de la fin de la ligne               I 
	INY
         ;                                                    I
LE6D6
	STA $04     ;   dans DECDEB                                      I
	STY $05    ;                                                     I
	JSR XDECAL_ROUTINE   ;   et on ram?ne la fin du listing (efface la ligne) I 
	LDA #$FF   ;    on met -1                                        I
	STA $10     ;   dans TR4                                         I
	EOR $0F    ;    on compl?mente TR3 ? 2                           I
	STA $0F     ;   donc on remet dans TR3-4                         I
	INC $0F     ;   l'oppos? de TR3-4                                I
LE6E7
	LDA $0E      ;  on prend la longueur ? ins?rer <------------------
	BEQ LE738  ;    c'est 0, on devait effacer la ligne -------------- 
	LDA $5E     ;   on prend la fin du listing                       I
	LDY $5F    ;                                                     I
	STA $06    ;    dans DECFIN                                      I
	STY DECFIN+1    ;                                                     I
	LDA RESB    ;    on prend l'adresse de la ligne                   I
	LDY RESB+1    ;                                                     I
	STA $04    ;    dans DECDEB                                      I
	STY $05    ;                                                     I
	CLC        ;            #A4FF                                    I
	LDA $0E    ;    on ajoute 3 ? la longueur (ent?te de ligne)      I
	ADC #$03   ;                                                     I
	PHA         ;   dans la pile                                     I
	ADC RESB     ;   on ajoute la longueur                            I
	BCC LE706   ;   ? DECDEB                                         I 
	INY         ;                                                    I
LE706
	STA DECCIB     ;   dans DECCIB                                      I
	STY DECCIB+1      ;                                                   I
	JSR XDECAL_ROUTINE    ;  et on lib?re la place pour la ligne              I 
	CLC         ;                                                    I
	PLA        ;                                                     I
	PHA         ;   on prend la longueur                             I
	ADC $0F    ;    on calcule longueur nouvelle ligne               I
	STA $0F   ;     - longueur ligne pr?c?dente                      I
	BCC LE718 ;                                                      I 
	INC $10   ;     dans TR3-4 (compl?ment ? 2)                      I
LE718
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
LE729
	INY          ;                                                   I
	LDA (TR0,X) ;   et le contenu de la ligne                        I
	STA (RESB),Y  ;  ? la suite                                       I
	INC TR0     ;                                                    I
	BNE LE734  ;                                                     I
	INC TR1     ;                                                    I
LE734
	DEC $0E    ;    jusqu'? la fin                                   I
	BNE LE729 ;                                                     I
LE738
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
	STY RESB+1                                                          
LE753
	LDA (RES),Y ;   on lit le code <------------------------------    
	CMP #$30    ;   inf?rieur ? 0 ?                              I    
	BCC LE785  ;    oui -----------------------------------------+---- 
	CMP #$3A    ;   sup?rieur ? 9 ?                              I   I
	BCS LE785  ;   oui -----------------------------------------+---O 
	AND #$0F    ;   on isole le chiffre                          I   I
	PHA        ;    dans la pille                                I   I
	ASL RESB    ;    RESB*2                                       I   I
	ROL RESB+1     ;                                                I   I
	LDA RESB    ;    AX=RESB*2                                    I   I
	LDX RESB+1    ;                                                 I   I
	ASL RESB    ;    *4                                           I   I
	ROL RESB+1    ;                                                 I   I
	ASL RESB    ;    *8                                           I   I
	ROL RESB+1   ;                                                  I   I
	ADC RESB    ;    +RESB*2                                      I   I
	STA RESB    ;                                                 I   I
	TXA        ;                                                 I   I
	ADC RESB+1    ;                                                 I   I
	STA RESB+1     ;   = RESB*10                                    I   I
	PLA         ;   plus chiffre lu                              I   I
	ADC RESB     ;                                                I   I
	STA RESB     ;                                                I   I
	BCC LE782  ;                                                 I   I
	INC RESB+1    ;                                                 I   I
LE782
	INY       ;     on ajoute un chiffre lu                      I   I
	BNE LE753 ;     et on recommence  ----------------------------   I
LE785
	TYA       ;     nombre de chiffres lus <--------------------------
	TAX       ;     dans X                                            
	LDA RESB   ;     nombre dans AY et RESB                            
	LDY RESB+1    ;                                                      
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
	BPL LE798   ;   afin de conserver le pattern                     
	SEC 
LE798	
	ROL $56                                                          
	BCC Le7c0  ;    si b7 de $56   ? 0, on saute <-------------------- 
LE79C
	LDY $49     ;   sinon on prend X/6                               I
	LDA ($4B),Y ;   on lit le code actuel                            I
	ASL         ;   on sort b7                                       I
	BPL Le7c0   ;   pas pixel, on sort ------------------------------O
	LDX $4A     ;   on prend le reste de X/6                         I
	LDA data_for_hires_display,X  ;  on lit le bit correspondant                      I 
	BIT $57     ;   b7 de HRSFB ? 1 ?                                I
	BMI LE7BA   ;   b7 ? 1, donc 3 ou 2                              I 
	BVC L7B3   ;   FB=0 ----------------------------------------    I 
	ORA ($4B),Y  ;  FB=1, on ajoute le code                     I    I
	STA ($4B),Y ;   et on le place                              I    I
	RTS
L7B3        ;                                               I    I
	EOR #$7F    ;   on inverse le bit  <-------------------------    I
	AND ($4B),Y ;   et on l'?teint                                   I
	STA ($4B),Y  ;  avant de le placer                               I
	RTS         ;                                                    I
LE7BA
	BVS Le7c0   ;   FB=3, on sort -----------------------------------O 
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
	JSR XMUL40_ROUTINE  ;    on calcule 40*ligne                            
	STA $4B    ;                                           
	CLC                                                              
	TYA                                                              
	ADC #$A0    ;   et on ajoute $A000, ?cran HIRES                   
	STA $4C    ;    dans ADHRS                                        
	STX RES    ;    on met la colonne dans RES                        
	LDA #$06   ;    A=6                                               
	LDY #$00   ;    et Y=0  (dans RES+1)                              
	STY RES+1   ;     AY=6 et RES=colonne                               
	JSR XDIVIS_ROUTINE ;     on divise la colonne par 6                       
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
	STA DECCIB                                                          
	LDA $47                                                          
	STA DECFIN+1                                                          
	ADC $4F                                                          
	STA DECCIB+1     ;   dans DECCIB-09                                       
	BCC LE83A   ;   inconditionnel                                    
                                                                                
                                                	
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
LE830
	LDA $004D,Y  ;  de HRSx                                           
	STA $06,X    ;  dans $06-7-8-9                                    
	DEY                                                              
	DEY                                                              
	DEX                                                              
	BPL LE830
LE83A	
	LDX #$03     ;  on va tracer 4 traits                             
LE83C
	STX $05      ;  dans $05 <----------------------------------------
	LDA table_for_rect,X   ; on lit le code coordonn?es                       I 
	STA $04      ;  dans $04                                         I
	LDX #$06     ;  on va extraire 8 bits                            I
LE845
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
	BPL LE845   ;   on fait les 4 coordonn?es ADRAW -------------    I 
	JSR XDRAWA_ROUTINE  ;   on trace le trait en absolu                      I 
	LDX $05    ;                                                     I
	DEX        ;                                                     I
	BPL LE83C  ;   et on fait 4 traits ------------------------------ 
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
	JSR Le7f3   ;   on place le curseur en X,Y                         
	LDX #$FF    ;   on met -1 dans X pour un changement de signe      
	SEC         ;   ?ventuel dans les param?tres                      
	LDA $51     ;   on prend X2                                       
	SBC $4D     ;   -X1                                               
	STA $4D     ;   dans HRS1 (DX)                                    
	BCS LE87B   ;   si DX<0, on inverse le signe de HRS1              
	STX $4E     ;   DEC $4E aurait ?t? mieux...                       
	SEC
LE87B
	LDA $53      ;  on prend Y2                                       
	SBC $4F      ;  -Y1                                               
	STA $4F     ;   dans HRS2 (DY)                                    
	BCS XDRAWR_ROUTINE   ;   et si DY n?gatif, on met signe -1                 
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

; NOERROR
   
XDRAWR_ROUTINE
Le885                                                                             
	LDA $02AA  ;    sauve le pattern                                  
	STA $56    ;    dans HRS1+1                                       
	JSR Le942  ;    v?rifie la validit? de dX et dY                  
	STX $46    ;    X et Y contiennent HRSX+dX et HRSY+dY             
	STY $47     ;   dans HRSX et HRSY                                 
	BIT $4E    ;    dX n?gatif ?                                      
	BPL LE89D  ;    non ----------------------------------------------
	LDA $4D    ;    oui, on compl?mente                              I
	EOR #$FF    ;   dX                                               I
	STA $4D    ;                                                     I
	INC $4D    ;    ? 2                                              I
LE89D
	BIT $50    ;    dY n?gatif ? <------------------------------------
	BPL LE8A9  ;    non ---------------------------------------------- 
	LDA $4F    ;    oui on compl?mente                               I
	EOR #$FF   ;    dY                                               I
	STA $4F    ;                                                     I
	INC $4F    ;    ? 2                                              I
LE8A9
	LDA $4D    ;    on teste dX et dY <-------------------------------
	CMP $4F                                                          
	BCC LE8ED   ;   dX<dY -------------------------------------------- 
	PHP         ;   dX>=dY , on trace selon dX                       I
	LDA $4D     ;   on prends dX                                     I
	BEQ LE8EB  ;    dX=0, on sort -------------------------------    I 
	LDX $4F    ;    X=dY                                        I    I
	JSR Le921  ;    on calcule dY/dX                            I    I 
	PLP        ;                                                I    I
	BNE LE8C0  ;    dX<>dY -----------------------------------  I    I 
	LDA #$FF   ;    dX=dY, la tangente est 1                 I  I    I
	STA RES   ;     en fait, -1, mais c'est la m?me chose    I  I    I
LE8C0	
	BIT $4e ; I
	BPL LE8CA ; I   dX>0 -------------------------------------  I    I
	JSR XHRSCG_ROUTINE ; I   dX<0, on d?place le curseur ? gauche     I  I    I 
	JMP LE8CD ; I---                                         I  I    I  
LE8CA
	JSR XHRSCD_ROUTINE ; II  on on d?place le curseur ? droite <-------  I    I 
LE8CD
	CLC       ; I-->a-t-on parcouru une valeur de la tangente   I    I
	LDA RES   ; I                                               I    I
	ADC RESB   ; I   on stocke le r?sultat dans RESB              I    I
	STA RESB   ; I                                               I    I
	BCC LE8E3  ;I   non, on continue -------------------------  I    I 
	BIT $50   ; I   oui, dY<0 ?                              I  I    I
	BMI LE8E0 ; I   oui -------------------------------      I  I    I
	JSR XHRSCB_ROUTINE ; I   non, on d?place le curseur        I      I  I    I 
	JMP LE8E3  ;I---vers le bas                       I      I  I    I 
LE8E0
	JSR XHRSCH_ROUTINE ; II  on d?place vers le haut <----------      I  I    I
LE8E3
	JSR XHRSSE_ROUTINE	  ;I-->on affiche le point <---------------------  I    I 
	DEC $4D   ; I   on d?cremente dX,                           I    I
	BNE LE8C0 ; ----on n'a pas parcouru tout l'axe              I    I 
LE8EA
	RTS       ;  -->sinon, on sort                              I    I
LE8EB
	PLP      ;   I  <--------------------------------------------    I
	RTS       ;  I                                                   I
LE8ED
	LDA $4F   ;  I  on trace la droite selon dY <---------------------
	BEQ LE8EA  ; ---dY=0, on sort                                      
	LDX $4D   ;     X=dX                                              
	JSR Le921 ;     on calcule dX/dY dans RES                          
LE8F6
	BIT $50                                                          
	BPL LE900  ;    dY>0 --------------------------------------------- 
	JSR XHRSCH_ROUTINE ;    dY<0, on d?place vers le haut                    I 
	JMP LE903  ; ---et on saute                                      I 
LE900
	JSR XHRSCB_ROUTINE  ; I  on d?place vers le bas <-------------------------- 
LE903	
	CLC       ;  -->a-t-on parcouru la tangente ?                     
	LDA RES                                                          
	ADC RESB                                                          
	STA RESB     ;   (dans RESB)                                        
	BCC LE919   ;   non ---------------------------------------------- 
	BIT $4E     ;                                                    I
	BPL LE916   ;   dX>0 ------------------------------------        I
	JSR XHRSCG_ROUTINE   ;   dX<0, on d?place vers                   I        I 
	JMP LE919  ; ---la gauche                               I        I 
LE916	
	JSR XHRSCD_ROUTINE  ; I  on d?place vers la droite <--------------        I 
LE919	
	JSR XHRSSE_ROUTINE	 ; -->on affiche le point <----------------------------- 
	DEC $4F    ;    et on d?crit dY                                   
	BNE LE8F6                                                       ;
	RTS         ;   avant de sortir de longueur des lignes            

	;   CALCUL LA TANGENTE (*256) D'UN TRAIT                    
Le921

	STX RES+1     ;   dX (ou dY)*256 dans RES+1                         
	LDY #$00    ;   dY (ou dX) dans AY                                
	STY RES                                                          
	JSR XDIVIS_ROUTINE    ;  calcul dX*256/dY (ou dY/dX)                    
	LDA #$FF     ;  reste =-1                                         
	STA RESB    ;    resultat dans RES                                 
	RTS   

           ;                    ROUTINE CURSET                               
XCURSE_ROUTINE
Le92f                                                                               
	LDX $4D      ;  X=HRSX                                            
	LDY $4F     ;   Y=HRSY                                            
	JSR le94e    ;  on v?rifie les coordonn?es                   
LE936
	JSR Le7f3    ;  on place le curseur en X,Y                        

	JMP LE79C    ;  et on affiche sans g?rer pattern   	 
	
     ;                          ROUTINE CURMOV
XCURMO_ROUTINE
Le93c
	JSR Le942    ;  on v?rifie les param?tres                        
	JMP LE936   ;   et on d?place    	

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
	BCS LE957   ;   oui ---------------------------------------------- 
	CPY #$C8    ;   Y>=200 ?                                         I
	BCS LE957   ;  oui ---------------------------------------------O
	RTS         ;   coordonn?es ok, on sort.                         I
LE957
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
	BMI LE9A7     ; oui ---------------------------------------------- 
	STX $28       ; TEXT, on met le num?ro de fen?tre dans $28       I
	BCC LE971     ; si C=0, c'est PAPER                              I 
	STA $0240,X  ;  on stocke la couleur d'encre                     I
	BCS LE974    ;  si C=1 c'est INK                                 I 
LE971
	STA SCRCF,X  ;  ou la couleur de fond  
LE974	
	LDA $0248,X  ;  est on en 38 colonnes ?                          I
	AND #$10     ;                                                   I
	BNE LE987    ; mode 38 colonnes ------------------------------  I
	LDA #$0C     ;  mode 40 colonnes, on efface l'?cran           I  I
	JSR Ldbb5    ;  (on envoie CHR$(12))                          I  I 
	LDA #$1D     ;  et on passe en 38 colonnes                    I  I
	JSR Ldbb5    ;  (on envoie CHR$(29))                          I  I 
	LDX $28      ;  on prend X=num?ro de fen?tre                  I  I
LE987	
	LDA SCRDY,X  ;  on prend la ligne 0 de la fen?tre <------------  I
	JSR XMUL40_ROUTINE    ;  *40 dans RES                                     I 
	LDA SCRBAL,X  ;  AY=adresse de base de la fen?tre                 I
	LDY $023C,X  ;                                                   I
	JSR XADRES_ROUTINE   ;   on ajoute l'adresse ? RES (ligne 0 *40) dans RES I 
	LDY SCRDX,X  ;  on prend la premi?re colonne de la fen?tre       I
	DEY         ;   on enl?ve deux colonnes                          I
	DEY         ;                                                    I
	SEC         ;                                                    I
	LDA SCRFY,X ;   on calcule le nombre de lignes                   I
	SBC SCRDY,X ;   de la fen?tre                                    I
	TAX         ;   dans X                                           I
	INX         ;                                                    I
	TYA         ;   colonne 0 dans Y                                 I
	BCS LE9B3   ;   inconditionnel --------------------------------- I 
LE9A7
	LDA #$00     ;  <----------------------------------------------+--
	LDX #$A0     ;                                                 I  
	STA RES      ;  RES=$A000 , adresse HIRES                      I  
	STX RES+1     ;                                                  I  
	LDX #$C8    ;   X=200 pour 200 lignes                          I  
	LDA #$00    ;   A=0 pour colonne de d?but = colonne 0          I  
LE9B3
	PLP         ;   on sort C <-------------------------------------  
	ADC #$00    ;   A=A+C                                             
	TAY        ;    dans Y                                            
	PLA        ;    on sort le code                                   *
LE9B8
	STA (RES),Y; -->on le place dans la colonne correspondante        
	PHA        ; I  on le sauve                                       
	CLC        ; I                                                    
	LDA RES    ; I  on passe 28 colonnes                              
	ADC #$28    ;I  (donc une ligne)                                  
	STA RES     ;I                                                    
	BCC LE9C6  ; I                                                    
	INC RES+1    ; I                                                    
LE9C6
	PLA        ; I  on sort le code                                   
	DEX        ; I  on compte X lignes                                
	BNE LE9B8   ;---                                                 
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
	JSR Le7f3    ;  et on place le premier point du cercle (X,Y-R)      
	LDX #$08    ;   X=7+1 pour calculer N tel que Rayon<2^N.          
	LDA $4D     ;   on prend le rayon                                 
LE9E5
	DEX         ;   on enl?ve une puissance                           
	ASL         ;   on d?cale le rayon ? gauche                       
	BPL LE9E5   ;   jusqu'? ce qu'un bit se pr?sente dans b7            
	STX TR0    ;    exposant du rayon dans $0C                        
	LDA #$80    ;   A=$80 soit 0,5 en d?cimal                         
	STA $0E     ;   dans sfX                                          
	STA $10     ;   et sfY                                            
	ASL         ;   A=0                                               
	STA $0F     ;   dans seX                                          
	LDA $4D     ;   A=Rayon                                           
	STA $11     ;   dans seY                                          
LE9F8
	SEC                                                              

	ROR TR1     ;   on met b7 de TR1 ? 1 (ne pas afficherle point)    
	LDA $10    ;    AX=sY                                             
	LDX $11                                                          
	JSR Lea62   ;   on calcule sY/R (en fait sY/2^N)                
	CLC                                                              
	LDA $0E     ;   on calcule sX=sX+sY/R                             
	ADC $12                                                          
	STA $0E                                                          
	LDA $0F                                                          
	STA $12                                                          
	ADC TR7                                                          
	STA $0F      ;  la partie enti?re seX a boug? ?                   
	CMP $12                                                          
	BEQ Lea22   ;  non ----------------------------------------------    
	BCS Lea1d    ;  elle a augment? ----------------------------     I    
	JSR XHRSCD_ROUTINE    ;;  elle ? baiss?, on d?place le curseur       I     I  
	JMP Lea20  ; ---? droite                                   I     I  
Lea1d
	JSR XHRSCG_ROUTINE ;  I  on d?place le curseur ? gauche <------------     I  
Lea20	
	LSR TR1    ; -->on indique qu'il faut afficher le point          I
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
	SBC TR7                                                          
	STA $11    ;    seY ? chang? (faut-il se d?placer verticalement)? 
	CMP $12                                                          
	BEQ Lea4a  ;    non ----------------------------------------------    
	BCS Lea44 ;     on est mont? --------------------------------    I  
	JSR XHRSCB_ROUTINE ;     on est descendu, on d?place le curseur      I    I   
	JMP LEA4E ;  ---vers le bas et on affiche                   I    I   
Lea44
	JSR XHRSCH_ROUTINE ;  I  on d?place le curseur vers le haut <---------    I   
Lea41	
	JMP LEA4E ;  O--et on affiche                                    I 
Lea4a	
	BIT TR1   ;  I  faut-il afficher le point ? <---------------------
	BMI Lea51 ;  I  non, on passe  -----------------------------------    
LEA4E
	JSR XHRSSE_ROUTINE	 ;  -->on affiche le point nouvellement calcul?         I  
Lea51
	LDA $0F   ;     seX=0 ? <-----------------------------------------
	BNE LE9F8  ;    non, on boucle                                        
	LDA $11    ;    oui, seY=rayon?                                   
	CMP $4D                                                          
	BNE LE9F8  ;    non, on boucle                                  
	PLA        ;    oui, on a fait le tour                            
	TAY        ;    on reprend les coordonn?es du curseur sauv?es     
	PLA        ;    dans X et Y                                       
	TAX                                                              
	JMP Le7f3    ;  et on replace le curseur                           
  
/*
                       CALCUL LE DEPLACEMENT sX ou sY                       
                                                                                
Action:calcule dans $13,$12 la valeur de (X,A)/R, en fait (X,A)/2^N.            
*/

Lea62																				
	STA $12       ; on place la partie fractionnaire dans $12         
	STX TR7       ; et la partie enti?re dans $13                     
	LDX TR0       ; X=N tel que Rayon<2^N
Lea68	
	LDA TR7       ; on garde le signe du r?sultat                     
	ROL                                                              
	ROR TR7       ; et on divise par 2^X                              
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
	jsr LEAB5 
	ldy $50
	iny
	bne Lea9f

XCHAR_ROUTINE
LEAAF

	LDA $4D
	ASL
	LSR $4F
	ROR
LEAB5	
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
	JSR LE79C 
Leaed	
	JSR XHRSCD_ROUTINE 
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
	jmp LDB5D 
wait_code_on_SERIAL_BUFFER_INPUT
.(
loop
	jsr READ_A_SERIAL_BUFFER_CODE_INPUT 
	bcs loop
	rts
.)	
write_caracter_in_output_serial_buffer
	bit write_caracter_in_output_serial_buffer
	jmp LDB79

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
.)	
Lec49	
	bit Lec49
	jmp Ldb12 
	
send_A_to_serial_output_with_check
Lec4f
; MINITEL
	stx TR0
	sty TR1
	pha
	bit $5b
	bpl Lec5e 
	jsr send_a_to_minitel
	jmp LEC61
Lec5e	
	jsr write_caracter_in_output_serial_buffer
LEC61	
	pla
	eor $0e
	sta $0e
	ldx TR0
	ldy TR1
	
	rts
Lec6b
	STX TR0
	STY TR1
Lec6f	
	ASL $027E
	BCC Lec77
	PLA
	PLA
	RTS
Lec77	
	BIT $5B
	BMI Lec8b
	JSR READ_A_SERIAL_BUFFER_CODE_INPUT
	BCS Lec6f
LEC80
	PHA
	EOR $0E
	STA $0E
	PLA
	LDX TR0
	LDY TR1
	RTS
	
Lec8b
	JSR LECB4
	BCS Lec6f
	BIT $5B
	BVS LEC80 
	CMP #$20
	BCS LEC80
	PHA
	JSR LECB9 
	TAX
	PLA
	TAY
	TXA
	CPY #$01
	BNE LECA8 
	ORA #$80
	BMI LEC80
LECA8
	CMP #$40
	BCS LECB0 
	SBC #$1F
	BCS LEC80 
LECB0	
	ADC #$3F
	BCC LEC80
LECB4	
	LDX #$0C
	JMP XLISBU_ROUTINE
LECB9	
	JSR LECB4	 
	BCS LECB9 
	RTS
Lecbf
	sec
	.byt $24
LECC1	
	clc
	lda #$80
	jmp LDB5D 
LECC7
	sec 
	.byt $24
LECC9	
	clc
	lda #$80

	jmp LDB79 
LECCF
	sec
	.byt $24
LECD1
	clc
	lda #$80
	jmp LDAF7  
	
LECD7	
	sec
	.byt $24
LECD9	
	clc 
	lda #$80
	jmp Ldb12 

compute_file_size
Lecdf
	SEC
	LDA $052F ; FIXME
	SBC $052D ; FIXME
	STA $052A ; FIXME
	LDA $0530 ; FIXME
	SBC $052E ; FIXME
	STA $052B ; FIXME
	LDA $052D ; FIXME
	LDY $052E ; FIXME
	STA RES
	STY RES+1
	rts

send_serial_header_file
Lecfd
	LDX #$32
LECFF	
	LDA #$16
	JSR send_A_to_serial_output_with_check 
	DEX
	BNE LECFF
	LDA #$24
	JSR send_A_to_serial_output_with_check
	LDA #$00 
	STA $0E
	LDX #$00
LED12	
	LDA BUFNOM+1,X 
	JSR send_A_to_serial_output_with_check 
	INX
	CPX #$0C
	BNE LED12 
	LDA #$00
	JSR send_A_to_serial_output_with_check 
	
	LDX #$00
LED24	
	LDA $052C,X
	JSR send_A_to_serial_output_with_check
	
	
	
	INX
	CPX #$07
	BNE LED24 
	LDA $0E
	JMP send_A_to_serial_output_with_check 

read_header_file
Led34	
	
	JSR Lec6b
	CMP #$16
	BNE read_header_file
	LDX #$0A
LED3D	
	JSR Lec6b 

	CMP #$16
	BNE read_header_file

	DEX
	BNE LED3D 
LED47		
	JSR Lec6b
	CMP #$16
	BEQ LED47
	CMP #$24
	BNE read_header_file
	
	LDA #$00
	STA $0E
LED56	
	JSR Lec6b
	TAX
	BEQ LED62	
	JSR Ldbb5

	JMP LED56 
LED62		
	LDX #$00
LED64
	JSR Lec6b 
	STA $052C,X ; FIXME
	INX
	CPX #$07
	BNE LED64
	JSR Lec6b 
	ORA #$30
	JMP Ldbb5

XCONSO_ROUTINE	
Led77	
	JSR LECC1
	JSR LECC9
LED7D	
	JSR READ_A_SERIAL_BUFFER_CODE_INPUT 
	BCS LED85
	JSR Ldbb5
LED85	
	JSR XRD0_ROUTINE;
	BCS LED7D
	CMP #$03
	BEQ LED94
	JSR write_caracter_in_output_serial_buffer
	JMP LED7D  
LED94	
	JSR Lecbf 
	JMP LECC7 
	


Led9a
XSDUMP_ROUTINE
	JSR LECC1
LED9D	
	ASL $027E
	BCS LEDC7 
	JSR READ_A_SERIAL_BUFFER_CODE_INPUT
	BCS LED9D 
	TAX
	BMI LEDAE 
	CMP #$20
	BCS LEDC1
LEDAE	
	PHA
	LDA #$81
	JSR Ldbb5 
	PLA
	JSR XHEXA_ROUTINE 
	JSR Ldbb5 
	TYA 
	JSR Ldbb5 
	LDA #$87 
LEDC1	
	JSR Ldbb5
	JMP LED9D
LEDC7	
	JMP Lecbf 

XSSAVE_ROUTINE
Ledca
	ror $5b
	lsr $5b
	jsr LECC9
	jsr Lee0a 
	jmp LECC7 

XMSAVE_ROUTINE
Ledd7
	ror $5b
	sec
	ror $5b
	jsr LECD9 
	jsr Lee0a 
	jmp LECD7 

XSLOAD_ROUTINE 
Lede5
	ROR $5B
	LSR $5B
	LDA #$40
	STA V1IER
	JSR LECC1
	JSR read_a_file_rs232_minitel 
	LDA #$C0
	STA V1IER
	JMP Lecbf 
	
	
XMLOAD_ROUTINE	
	ROR $5B
	SEC
	ROR $5B
	jsr LECD1 
	jsr read_a_file_rs232_minitel 
	jmp LECCF 

;;;;;;;;;;;;;;;;;;	
save_file_rs232_minitel
Lee0a
	BIT $5B
	BVS LEE11 
	JSR send_serial_header_file 
LEE11	
	JSR compute_file_size

	LDA #$00
	STA $0E
LEE18	
	LDA $052A
	BEQ LEE2F 
	LDY #$00
	LDA (RES),Y
	JSR send_A_to_serial_output_with_check 
	DEC $052A
	INC RES
	BNE LEE18
	INC RES+1
	BNE LEE18 
LEE2F	
	LDA $052B
	BEQ LEE51 
	LDY #$00
LEE36	
	LDA (RES),Y
	JSR send_A_to_serial_output_with_check 
	INY
	BNE LEE36 
	DEC $052B
	INC RES+1
	BIT $5B
	BPL LEE2F 
	LDA #$30 
	STA $44
LEE4B	
	LDA $44
	BNE LEE4B 
	BEQ LEE2F
LEE51	
	LDA $0E
	JMP send_A_to_serial_output_with_check 
	
read_a_file_rs232_minitel
Lee56
	BIT $5B
	BVS LEE5D  
	JSR read_header_file  
LEE5D	
	JSR compute_file_size 
	BIT $5B
	BVC LEE6C  
	LDA #$FF
	STA $052A
	STA $052B
LEE6C	
	LDY #$00
	STY $0E
LEE70	
	LDA $052A
	BEQ LEE86  
	JSR Lec6b 
	STA (RES),Y
	DEC $052A
	INC RES
	BNE LEE70  
	INC RES+1
	JMP LEE70 
LEE86	
	LDA $052B
	BEQ LEE9D  
	LDY #$00
LEE8D	
	JSR Lec6b 
	STA (RES),Y
	INY
	BNE LEE8D  
	INC RES+1
	DEC $052B

	JMP LEE86  
LEE9D	
	JSR Lec6b  
	ORA #$30
	JMP Ldbb5  

XRING_ROUTINE
Leea5
; minitel (wait a ring on phone line)	
	lda #0
	sta $028c
LEEAA	
	lda #$10
	bit $032d
	bne Leee3 
	sec
	rts
; minitel detect ring on line
Leeb3
	LDA #$FF
	STA $0328
	STA $0329
LEEBB	
	LDA $0329
	CMP #$C5
	BCS LEEBB 
	BIT V2DRB 
LEEC5	
	LDA #$20
	AND $032D 
	BNE LEEDF 
	LDA #$10
	AND $032D
	BEQ LEEC5 
	
	LDA $0329
	CMP #$AD
	BCC LEEE1 
	CMP #$B5
	LDA #$01
	RTS
LEEDF	
	lda #0
LEEE1	
	sec
	rts
Leee3
; minitel
; detect_ring (next routine)
	sei
	ldx #4
LEEE6	
	jsr Leeb3 
	dex

	BNE LEEE6 
LEEEC
	JSR Leeb3 
	BEQ LEEFB 
	BCS LEEEC
	INX
	JMP LEEEC
LEEF7
	CLI
	JMP LEEAA 
LEEFB	
	CPX #$06 
	BCC LEEF7
LEEFF	
	JSR Leeb3 
	BCS LEEFF
	LDY #$1E
	LDX #$00
LEF08	
	JSR Leeb3 
	BCC LEF0E 
	INX
LEF0E	
	DEY
	BNE LEF08 
	CPX #$0F
	BCS LEEF7
	
	CLI
	LDA #$0A
	STA $44
LEF1A	
	LDA $44
	BNE LEF1A 
	CLC
	rts

XLIGNE_ROUTINE
Lef20
; minitel ; get the line
	jsr LECD9 
	lda #$6f
	jsr send_pro1_sequence_to_minitel
	lda #$68
	jsr send_pro1_sequence_to_minitel
	jmp LECD7
	
; minitel
; send pro1 sequence to minitel
send_pro1_sequence_to_minitel
Lef30
	jsr Lec49 
	; REMOVEME minitel
	rts
	
Lef3f
free_the_minitel_line
XDECON_ROUTINE
	jsr LECD9
	lda #$67
	jsr send_pro1_sequence_to_minitel
	jmp LECD7 
	

Lef4a

XWCXFI_ROUTINE
	; REMOVEME minitel
	jsr LECB4


	rts

XMOUT_ROUTINE
; minitel 
	pha
	jsr LECD9 
	pla
	jsr Lec49
	jmp LECD7


XSOUT_ROUTINE
; RS232 
	pha 
	jsr LECC9
	pla
	jsr write_caracter_in_output_serial_buffer
	jmp LECC7 
add_0_5_A_ACC1
	lda #<const_zero_dot_half 
	ldy #>const_zero_dot_half 
	jmp AY_add_acc1 ; AY+acc1
Lef97 	
	rts ; Don't know why there is this RTS !

	
ACC2_ACC1
	jsr LF1EC 
XA2NA1_ROUTINE	
	lda $65
	eor #$ff
	sta $65
	eor $6d
	sta $6e
	lda ACC1E
	jmp XA1PA2_ROUTINE
Lefaa
mantisse_A
	jsr LF0E5
	bcc next802 

AY_add_acc1
		jsr LF1EC 
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
LEFC2	
	tay 
	beq Lef97 
	sec
	sbc ACC1E
	beq next802	
	bcc next801 
	sty ACC1E
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
	
	jsr LF0FC 
next802	
	bit $6e
	bpl Lf049 
	ldy #$60
	cpx #$68
	beq LEFFA 
	ldy #$68
LEFFA
	sec
	eor #$ff
	adc $7f
	sta $66
	lda $0004,y
	sbc $04,x
	sta $64
	lda $0003,y
	sbc RESB+1,x
	sta $63
	lda $0002,y
	sbc RESB,x
	sta $62
	lda $0001,y
	sbc RES+1,x
	sta $61
LF01D
	bcs Lf022 
	
	jsr Lf090 
Lf022
	ldy #00
	tya
	clc
LF026	
	ldx $61
	
	bne LF074 
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
	bne LF026 
Lf042
	lda #0
	sta ACC1E
LF046
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
LF074	
	BPL Ld068
	
	SEC
	
	SBC ACC1E
	BCS Lf042 
	EOR #$FF
	ADC #$01
	STA ACC1E
Lf081
	BCC Lf08f
LF083	
	INC ACC1E
	
	BEQ LF0C7 
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
        ldy     RESB+1,x
        sty     $04,x
        ldy     RESB,x
        sty     RESB+1,x
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
        ror     RESB+1,x
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
const_sqr_two_divided_by_two
	.byt $80,$35,$04,$f3,$34 ; sqr(2/2) = 0.7071067812
const_sqr_two	
	.byt $81,$35,$04,$f3,$34 ; sqr(2) = 1.414213562
const_negative_zero_dot_five
	.byt $80,$80,$00,$00,$00 ; -0.5
const_ln_2	
	.byt $80,$31,$72,$17,$f8 ; ln(2) = 0.683147806
LF140
	rts
LF141	
	lda #2
	jmp LF0C9 

XLN_ROUTINE
Lf146

        tsx
        stx     $89
LF149:  jsr     LF3BD  
        beq     LF141
        bmi     LF141
        lda     ACC1E
        sbc     #$7F
        pha
        lda     #$80
        sta     ACC1E
        lda     #<const_for_ln 
        ldy     #>const_for_ln 
        jsr     AY_add_acc1  
        lda     #<const_sqr_two 
        ldy     #>const_sqr_two 
        jsr     Lf287  
        lda     #<const_atn_1 
        ldy     #>const_atn_1 
        jsr     ACC2_ACC1 
        lda     #<polynome_ln_coef
        ldy     #>polynome_ln_coef
        jsr     LF6E1  
        lda     #<const_negative_zero_dot_five 
        ldy     #>const_negative_zero_dot_five 
        jsr     AY_add_acc1  
        pla
        jsr     LF9E9  
        lda     #<const_ln_2	 
        ldy     #>const_ln_2	 



		
LF184:  jsr     LF1EC 
        beq     LF140
        bne     LF190
XA1MA2_ROUTINE		
        beq     LF140
LF18D		
        tsx
        stx     $89
LF190:  jsr     LF217 
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
        jsr     LF1BE  
        jmp     Lf301  

LF1B9:  bne     LF1BE
        jmp     LF0CF  

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
        lda     ACC1E
        rts
	

LF217:  lda     $68
LF219:  beq     LF237
        clc
        adc     ACC1E
        bcc     LF224 
        bmi     Lf23c
        clc
        .byte   $2C
LF224:  bpl     LF237
        adc     #$80
        sta     ACC1E
        beq     Lf23f
        lda     $6E
        sta     $65
        rts

LF231:  lda     $65
        eor     #$FF
        bmi    Lf23c
LF237:  pla
        pla
        jmp     Lf042 
Lf23c		
		jmp LF0C7 
Lf23f		
		jmp LF046 
; 10*acc1->acc1	
Lf242
	JSR XA1A2_ROUTINE
	TAX
	BEQ Lf258
	CLC
	ADC #$02
	BCS Lf23c
	LDX #$00
	STX $6E
	JSR LEFC2 
	INC ACC1E
	beq Lf23c
Lf258
	rts
	
Lf259
ten_in_floating_point
	.byt $84,$20,$00,$00,$00 ; Ten in floating point
Lf25e
acc1_1_divide_10_in_acc1
	jsr XA1A2_ROUTINE 
	ldx #0
	lda #<ten_in_floating_point
	ldy #>ten_in_floating_point
LF267
	stx $6e

	jsr Lf323 
	jmp XA2DA1_ROUTINE	
	
	
XLOG_ROUTINE
Lf26f
	tsx
	stx $89
	jsr LF149 
	jsr XA1A2_ROUTINE 
	lda #<const_ln_10
	ldy #>const_ln_10
	jsr Lf323 
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
	SBC ACC1E
	STA ACC1E
	JSR LF217 
	INC ACC1E
	BEQ Lf23c
	LDX #$FC
	LDA #$01
LF2A4	
	LDY $69
	CPY $61
	BNE LF2BA 
	LDY $6A
	CPY $62
	BNE LF2BA
	LDY $6B
	CPY $63
	BNE LF2BA
	LDY $6C
	CPY $64
LF2BA	
	PHP
	ROL
	BCC LF2CA 
	INX
	STA $72,X
	BEQ LF2C8
	BPL LF2F8 
	LDA #$01
Lf2c7	
	.byt $2c
LF2C8	
	lda #$40
	;
LF2CA	
	PLP
	BCS LF2DB 
LF2CD	
	ASL $6C
	ROL $6B
	ROL $6A
	ROL $69
	BCS LF2BA 
	BMI LF2A4 
	BPL LF2BA
LF2DB	
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
	JMP LF2CD 
LF2F8	
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
	jmp Lf022

XPI_ROUTINE	
Lf314
; pi->acc1
	jsr test_if_degree_mode 
	beq Lf31f ; is it in radian mode ?
	lda #<const_pi_degree
	ldy #>const_pi_degree
	bne Lf323
Lf31f	
	lda #<const_pi_radians 
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
	STA ACC1E
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
	LDA ACC1E
	STA ($7D),Y
	STY $66
	RTS
XA2A1_ROUTINE	
LF377
	lda $6d
LF379	
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
	lda     ACC1E
    beq     LF395
    asl     $66
    bcc     LF395
    jsr     LF0B8 
    bne     LF395
    jmp     LF083

XA1IAY_ROUTINE	
LF3A6	
    lda     $65
    bmi     LF3B8 
    lda     ACC1E
    cmp     #$91
    bcs     LF3B8
    jsr     LF439
    lda     $64
    ldy     $63
    rts	

LF3B8	
	lda #$0a
	jmp LF0C9	

LF3BD:  lda     ACC1E
        beq     LF3CA
LF3C1:  lda     $65
LF3C3:  rol   
        lda     #$FF
        bcs     LF3CA
        lda     #$01
LF3CA:  rts

	jsr LF3BD
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
	STX ACC1E
	STA $66
	STA $65
	JMP LF01D 

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
LF3F8	
	rts

LF3F9
	STA $7D
	STY $7E
	LDY #$00
	LDA ($7D),Y
	INY
	TAX 
	BEQ LF3BD  
	LDA ($7D),Y
	EOR $65
	BMI LF3C1 
	CPX ACC1E
	BNE LF430 
	LDA ($7D),Y
	ORA #$80
	CMP $61
	BNE LF430
	INY
	LDA ($7D),Y
	CMP $62
	BNE LF430
	INY
	LDA ($7D),Y
	CMP $63
	BNE LF430  
	INY
	LDA #$7F
	CMP $66
	LDA ($7D),Y
	SBC $64
	BEQ LF3F8 
LF430	
	LDA $65
	BCC LF436  
	EOR #$FF
LF436	
	JMP LF3C3 


LF439
	LDA ACC1E
	BEQ LF487
	SEC
	SBC #$A0
	BIT $65
	BPL LF44D 
	TAX
	LDA #$FF
	STA $67
	JSR LF096 
	TXA
LF44D	
	LDX #$60
	CMP #$F9
	BPL LF459 
	JSR LF0E5
	STY $67
	RTS

LF459	
	TAY
	LDA $65
	AND #$80
	LSR $61
	ORA $61
	STA $61
	JSR LF0FC 
	STY $67
LF469	
	RTS
	
XINT_ROUTINE	

LF46A
	LDA ACC1E
	CMP #$A0
	BCS LF469 
	JSR LF439 
	STY $66
	LDA $65
	STY $65
	EOR #$80
	ROL
	LDA #$A0
	STA ACC1E
	LDA $64
	STA $88
	JMP LF01D
LF487	
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
	jmp XWSTR0_ROUTINE 

XA1DEC_ROUTINE	
LF4A5
	LDY #$00
	LDA #$20
	BIT $65
	BPL LF4AF 
	LDA #$2D
LF4AF	
	STA $0100,Y
	STA $65
	STY $77
	INY
	LDA #$30
	LDX ACC1E
	BNE LF4C0 
	
	JMP LF5C8
LF4C0	
	LDA #$00
	CPX #$80
	BEQ LF4C8
	BCS LF4D1 
LF4C8	
	LDA #<const_for_decimal_convert 
	LDY #>const_for_decimal_convert
	JSR LF184 
	LDA #$F7 ; Should be indexed ?.= FIXME
LF4D1	
	STA $74
LF4D3	
	LDA #<LF5DA
	LDY #>LF5DA 
	JSR LF3F9 ;
	BEQ LF4FA 
	BPL LF4F0 
LF4DE	
	LDA #<const_999_999_dot_9
	LDY #>const_999_999_dot_9
	JSR LF3F9 ; 
	BEQ LF4E9 
	BPL LF4F7 
LF4E9	
	JSR Lf242 ;
	DEC $74 
	BNE LF4DE
LF4F0	
	JSR Lf25e ; 
	INC $74
	BNE LF4D3
LF4F7
	JSR add_0_5_A_ACC1
LF4FA	
	JSR LF439 
	LDX #$01
	LDA $74
	CLC
	ADC #$0A
	BMI LF50F 
	CMP #$0B
	BCS LF511
	ADC #$FF
	TAX
	LDA #$02
LF50F	
	SEC
LF511
	SBC #$02
	STA $75
	STX $74
	TXA
	BEQ LF51B 
	BPL LF52E 
LF51B	
	LDY $77
	LDA #$2E
	INY
	STA $0100,Y
	TXA
	BEQ LF52C 
	LDA #$30
	INY
	STA $0100,Y
LF52C	
	STY $77
LF52E

	LDY #$00
	LDX #$80
LF532	
	CLC
LF533	
	LDA $64
	ADC const_negative_100_000_000+3,Y 
	STA $64
	LDA $63
	ADC const_negative_100_000_000+2,Y 
	STA $63
	LDA $62
	ADC const_negative_100_000_000+1,Y
	STA $62
	LDA $61
	ADC const_negative_100_000_000,Y 
	STA $61
	INX
	BCS LF556
	BPL LF533
	BMI LF558
LF556	
	BMI LF532
LF558	
	TXA 
	BCC LF55F
	EOR #$FF
	ADC #$0A
LF55F	
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
	BNE LF57A 
	LDA #$2E
	INY
	STA $0100,Y
LF57A	
	STY $77
	LDY $76
	TXA
	EOR #$FF
	AND #$80
	TAX
	CPY #$24
	BNE LF533 
	LDY $77
LF58A	
	LDA $0100,Y
	DEY
	CMP #$30
	BEQ LF58A 
	CMP #$2E
	BEQ LF597
	INY
LF597	
	LDA #$2B
	LDX $75
	BEQ LF5CB 
	BPL LF5A7 
	LDA #$00
	SEC
	SBC $75
	TAX
	LDA #$2D
LF5A7	
	STA $0102,Y
	LDA #$45
	STA $0101,Y
	TXA
	LDX #$2F
	SEC
LF5B3	
	INX
	SBC #$0A
	BCS LF5B3 
	ADC #$3A
	STA $0104,Y
	TXA
	STA $0103,Y
	
	LDA #$00
	STA $0105,Y
	BEQ LF5D0 
LF5C8	
	STA $0100,Y
LF5CB	
	LDA #$00
	STA $0101,Y
LF5D0	
	LDA #$00
	LDY #$01
	RTS

LF5d5	
const_for_decimal_convert
const_one_billion	
	.byt $9e,$6e,$6b,$28,$00 ; 1 000 000 000  float
const_999_999_999
LF5DA	
	.byt $9e,$6e,$6b,$27,$fd ; 999 999 999
const_999_999_dot_9
	.byt $9b,$3e,$bc,$1f,$fd ; 999 999.9
const_zero_dot_half	
LF5E4	
	.byt $80,$00,$00,$00,$00 ; 0.5 
LF5E9 ; 
const_negative_100_000_000 ; 32 bits binary signed
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
	jmp Lf042
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
	BEQ LF60D 
	LDX #$80
	LDY #$00
	JSR XA1XY_ROUTINE 
	LDA $6D
	BPL LF63D  
	JSR XINT_ROUTINE  
	LDA #$80
	LDY #$00
	JSR LF3F9 
	BNE LF63D  
	TYA
	LDY $88
LF63D
	JSR LF379  
	TYA
	PHA
	JSR LF149  ; 
	LDA #$80
	LDY #$00
	JSR LF184  
	JSR  LF68F ; 
	PLA
	LSR
	BCC LF65D  

XNA1_ROUTINE	
LF653
	; negative number
	lda ACC1E
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
LF68F	
	LDA #<const_1_divide_ln_2
	LDY #>const_1_divide_ln_2
	JSR LF184 
	LDA $66
	ADC #$50
	BCC LF69F 
	JSR XAA1_ROUTINE 
LF69F	
	STA $7F
	JSR LF38A 
	LDA ACC1E
	CMP #$88
	BCC LF6AD
LF6AA	
	JSR LF231 
LF6AD	
	JSR XINT_ROUTINE 
	
	LDA $88
	CLC
	ADC #$81
	BEQ LF6AA
	SEC
	SBC #$01
	PHA
	LDX #$05
LF6BD	
	LDA $68,X
	LDY ACC1E,X
	STA ACC1E,X
	STY $68,X
	DEX
	BPL LF6BD 
	LDA $7F
	STA $66
	JSR XA2NA1_ROUTINE
	JSR XNA1_ROUTINE 
	LDA #<coef_polynome 
	LDY #>coef_polynome 
	JSR LF6F7 
	LDA #$00
	STA $6E
	PLA
	JMP LF219 


LF6E1	
	STA $85
LF6E3	
	STY $86
LF6E5	
	JSR LF348 
LF6E8	
	LDA #$73
	JSR LF184
	JSR LF6FB 
	LDA #$73
	LDY #$00
	JMP LF184	

	
LF6F7
	STA $85
	STY $86
LF6FB	
	JSR LF34B 
	LDA ($85),Y
	STA $87
	LDY $85
	INY
	TYA
	BNE LF70A 
	INC $86
LF70A	
	STA $85
	LDY $86
LD70E	
	JSR LF184 
	LDA $85
	LDY $86
	CLC
	ADC #$05
	BCC LF71B 
	INY
LF71B	
	STA $85
	STY $86
	JSR AY_add_acc1  
	LDA #$78
	LDY #$00
	DEC $87
	BNE LD70E  
LF72A
	rts
	

values_rnd
LF72B
const_11879546_for_rnd
	.byt $98,$35,$44,$7a,$6b ; 11879546,42
const_3_dot_92_for_rnd_etc
LF730
	.byt $68,$28,$b1,$46,$20 ;3.927678 E-08
	
XRND_ROUTINE
LF735
	JSR LF3BD
	TAX
	BMI LF753 
	LDA #$EF
	LDY #$02
	JSR Lf323
	TXA
	BEQ LF72A 
	LDA #<const_11879546_for_rnd
	LDY #>const_11879546_for_rnd
	JSR LF184
	LDA #<const_3_dot_92_for_rnd_etc
	LDY #>const_3_dot_92_for_rnd_etc
	JSR AY_add_acc1 
LF753	
	LDX $64
	LDA $61
	STA $64
	STX $61
	LDA #$00
	STA $65
	LDA ACC1E
	STA $66
	LDA #$80
	STA ACC1E
	JSR Lf022 
	LDX #$EF ; FIXME
	LDY #$02 ; FIXME
	JMP XA1XY_ROUTINE

XRAND_ROUTINE
LF771
	JSR LF348
	JSR XRND_ROUTINE 
	LDA #$73
	LDY #$00
	JSR LF184 
	JMP XINT_ROUTINE 

XCOS_ROUTINE
LF781
	JSR LF8B1 
	LDA #<CONST_PI_DIVIDED_BY_TWO
	LDY #>CONST_PI_DIVIDED_BY_TWO
	JSR AY_add_acc1
	JMP LF791

XSIN_ROUTINE
LF78E
	JSR LF8B1
LF791	
	JSR XA1A2_ROUTINE
	LDA #<const_pi_mult_by_two
	LDY #>const_pi_mult_by_two
	LDX $6D
	JSR LF267 
	JSR XA1A2_ROUTINE 
	JSR XINT_ROUTINE
	LDA #$00
	STA $6E
	JSR XA2NA1_ROUTINE
	LDA #<const_0_dot_twenty_five
	LDY #>const_0_dot_twenty_five 
	JSR ACC2_ACC1 
	LDA $65
	PHA
	BPL LF7C5
	JSR add_0_5_A_ACC1
	LDA $65
	BMI LF7C8 
	LDA $8A
	EOR #$FF
	STA $8A
	.byt $24
LF7C4	
	pha
LF7C5	
	JSR XNA1_ROUTINE 
LF7C8	
	LDA #<const_0_dot_twenty_five
	LDY #>const_0_dot_twenty_five
	JSR AY_add_acc1
	PLA
	BPL LF7D5
	JSR XNA1_ROUTINE
LF7D5	
	LDA #<coef_polynome_sin
	LDY #>coef_polynome_sin
	JMP LF6E1


	
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
	JSR LF8B1 
	JSR LF348
	LDA #$00
	STA $8A
	JSR LF791 
	LDX #$80
	LDY #$00
	JSR XA1XY_ROUTINE 
	LDA #$73
	LDY #$00
	JSR Lf323
	LDA #$00
	STA $65
	LDA $8A
	JSR LF7C4
	LDA #$80
	LDY #$00
	JMP Lf287


	
XATN_ROUTINE
LF835
	LDA $65
	PHA
	BPL LF83D 
	JSR XNA1_ROUTINE 
LF83D	
	LDA ACC1E
	PHA
	CMP #$81
	BCC LF84B
	LDA #<const_atn_1
	LDY #>const_atn_1 
	JSR Lf287 
LF84B	
	LDA #<const_coef_atn 
	LDY #>const_coef_atn 
	JSR LF6E1
	PLA
	CMP #$81 
	BCC LF85E  
	LDA #<CONST_SIN_AND_COS 
	LDY #>CONST_SIN_AND_COS
	JSR ACC2_ACC1
LF85E	
	PLA
	BPL LF864 
	JSR XNA1_ROUTINE
LF864	
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
	jsr     AY_add_acc1
	ldx     RES
	ldy     RES+1
	jmp     XA1XY_ROUTINE

LF8DB:  jsr     LF9FC 
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
LF8FF:  jsr     LF9FC
        bcs     LF915
        cmp     #$32
        bcs     LF915
        cmp     #$31
        rol     $62
        rol     $61
        bcs     LF912
        bcc     LF8FF
LF912:  jmp     LF0C7 

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
        sta     RESB+1
        sta     $66
        ldx     #$05
LF92F:  sta     ACC1E,x
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
LF94C:  stx     RESB+1
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
LF9B7:  jsr     Lf25e
        inc     $75
        bne     LF9B7
        beq     LF9C7
LF9C0:  jsr     Lf242 
        dec     $75
        bne     LF9C0
LF9C7:  lda     RESB+1
        bmi     LF9E1
        bpl     LF9E4
LF9CD:  pha
        bit     $76
        bpl     LF9D4
        inc     $74
LF9D4:  jsr     Lf242
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
        ldx     ACC1E
        jmp     XA1PA2_ROUTINE 

LF9FC:  inc     RESB
LF9FE:  ldy     RESB
        lda     (RES),y
        jsr     uppercase_char 
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
LFB8F
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
	LDA #<LFB8F 

	LDY #>LFB8F 
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
	
LFED8	
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
	jmp XDECAL_ROUTINE

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
	sta ADDRESS_READ_BETWEEN_BANK
	sty ADDRESS_READ_BETWEEN_BANK+1

	

Lff00
	ldy #00
	jsr Lff27 
	beq Lff26
	jsr Lff31
	inc ADDRESS_READ_BETWEEN_BANK
	bne Lff10
	inc ADDRESS_READ_BETWEEN_BANK+1
Lff10
	jsr Lff27 
	sta (RESB),y
	iny
	cpy #8
	bne Lff10
	tya
	clc
	adc ADDRESS_READ_BETWEEN_BANK
	sta ADDRESS_READ_BETWEEN_BANK
	bcc Lff00
	inc ADDRESS_READ_BETWEEN_BANK+1
	bcs Lff00
Lff26	
	rts
read_a_code_in_15_and_y	
Lff27
	bit RES
	bpl Lff2e
	lda (ADDRESS_READ_BETWEEN_BANK),y
	rts
Lff2e	
	jmp $411
ZADCHA_ROUTINE	
Lff31	
compute_address_of_a_char
	ldx #$13
	stx RESB+1
	asl
	rol RESB+1
	asl
	rol RESB+1
	asl
	rol RESB+1
	sta RESB
	bit FLGTEL
	bmi Lff4b
	lda RESB+1
	adc #$1c
	sta RESB+1
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
	lda FLGKBD
	and #$f9
	ora RES
	sta FLGKBD
	lda Lff90,x
	ldy Lff90+1,x
	sta ADKBD
	sty $2b
	jsr routine_to_define_22 
	lda FLGKBD
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

free_bytes ; 26 bytes
	.dsb $ffff-free_bytes-5,0 ; 5 because we have 5 bytes after
; fffa
END_ROM
NMI:
	.byt $00,$2f
; fffc
RESET:
	.byt $00,$c0
; fffe
BRK_IRQ:	
	.byt <VIRQ,>VIRQ

