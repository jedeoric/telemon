
; Page 0 variables

#define RES $00 ; address general usage
#define RESB $02 ; ???

#define DECDEB $04 ; param/shift
#define DECFIN $06 ; param/shift
#define DECCIB $08 ; param/shift
#define DECTRV $0A ; param/shift


#define TR0 $0c ; 
#define TR1 $0d ; 
#define TR2 $0e ; 
#define TR3 $0f ; 
#define TR4 $10 ; general usage 1 byte
#define TR5 $11 ; general usage 1 byte
#define TR6 $12 ; general usage 1 byte
#define TR7 $13 ; general usage 1 byte


#define DEFAFF $14 ; default value for decimal conversion

#define IRQSVA $21 ; SAVE A when it enters in IRQ
#define IRQSVX $22 ; SAVE X when it enters in IRQ
#define IRQSVY $23 ; SAVE X when it enters in IRQ

; SCR
#define ADSCR $26 ; Adress of begin line display 
#define SCRNB $28 ; number window screen

#define ADKBD $2a ; ASCII conversion table adress

#define RS232T $59
#define RS232C $5A


/* PAGE 2 TELEMON */

#define TABDRV $0208 ; Activating drive 0 if not connected, b7 equal double side

#define FLGTEL $020D ; b0=1 strated is missing

#define IOTAB0 $02ae ; activating channel 1

#define ADIOB $02be ; 48 bytes ? I/O management address


#define FLGRST $02ee
#define CSRND $02EF ; current value of random generator



#define LPRFX $0288 ; printer width

#define LPRX $0286 ; word cursor in the line
#define FLGLPR $028a ;; word b7 ready

#define KORAM $020f ; total Max ram Bytes	
#define BNKST $0200 ; RESB 8 value of bytes $fffb of each bank
#define KOROM $020E	; Ko ROM total

#define FLGKBD $0275	;Keyboard flag : b7 majn b6 if sound

#define SCRTXT $0256 ; desc scrtxt 6 bytes
#define SCRHIR $025C ; desc 6 bytes for HIres
#define SCRTRA $0262 ; desc 6 bytes for trace
#define VIRQ $02FA
#define VNMI $02F4
#define VAPLIC $2FD ; No banque adress



#define VEXBNK $414
#define BNKCIB $417





/*VECTORS used with brk */

#define XOP0 $00
#define XOP1 $01



#define XRD0 $08
#define XRD1 $09
#define XRD2 $0a
#define XRD3 $0b


#define XRDW0 $0c
#define XRDW1 $0d
#define XRDW2 $0e
#define XRDW3 $0f




#define XWR0 $10 ; put a character on channel 0
#define XWR1 $11 ; put a character on channel 1
#define XWR2 $12 ; put a character on channel 2
#define XWR3 $13 ; put a character on channel 3

#define XWSTR0 $14 ; put a str on channel 0
#define XWSTR1 $15 ; put a str on channel 1

/// @brief move memory 
/// This vector move memory 
/// @param A (accumulator)  [in] The desired low adress of the memory 
/// @param Y (register)  [in] The desired high adress of the memory


 

/*!
 Scroll one line in hires to the bottom
 
 lda #<$a000
 
 ldy #>$a000
 
 sta DECDEB
 
 sty DECDEB+1
 */

     


/// @see xxxxx
/// 

#define XDECAL $18 ; COPY mem

#define XTEXT 	$19 ; switch to text
#define XHIRES 	$1A ; switch to HIRES
#define XEFFHI 	$1B ; clear HIRES

#define XMINMA $1f ; A register converted to upper XY not changed

#define XFILLM $1C ; FILL

#define XMUL40 $20 ; AY=A*40
#define XMULT $22 ; AY*RES --> TR0-1-2-3  RES étant une adresse

#define XDIVIS $23 ; divide RES/AY=RES (RESB reste)

// XNOMFI : Length in X -> BUFNOM and (A and Y for str)
// X=0 0length
// X=1 if RAS C=1 if jokers
//  x=2 ; if drive by default has changed
//  x>127 ; incorrect name
#define XNOMFI $24


/// @brief Do CRLF
/// This vector send 0x0a and 0x0d

#define XCRLF $25 ; send on channel 0, RC and LF (Return and line feed)



#define XDECAY $26 ; AY : decimal, length =X

#define XBINDX $28 ; Convert to decimal : numer un AY 



#define XDECIM $29  ; same as BINDX but displayed on channel 0 return in TR5 

#define XHEXA $2A ; convert A in YA in HEX

#define XA1AFF $2b ; display ACC1 on channel 0

#define XMENU $2c ; manage a menu AY

#define XMENU $2d ; edit

#define XINSER $2e ; insert a line

#define XSCELG $2f ; search line

#define XCSSCR $35 ; display cursor (prompt)	, X equal to 0 : No window





/* PSG Working 1/2 */

#define XOUPS $42

#define XSOUND $44 
#define XMUSIC $45 
#define XZAP $46
#define XSHOOT $47

#define XALLKB $50 ; Get keyboard, --> KBDCOL

#define XA1PA2 $6A ; A1+A2 --> A1

/* GRAPHICS WORKING*/

#define XCIRCL $8f ; CIRCLE
#define XCURSE $90 ; CURSET
#define XCURMO $91 ; CURMOV
#define XPAPER $92 ; PAPER : X= no window, 128 if hires
#define XINK $93 ; A value 0-7 or 16-23
#define XBOX $94 ; BOX
#define XABOX $95 ; ABOX
#define XFILL $96 ; FILL
#define XCHAR $97 ; XCHAR
#define XSCHAR $98 ; SCHAR string adress in AY, length in X 

/*PSG working */

#define XEXPLO $9c ; EXPLODE  
#define XPING $9d ; ping


; PAGE 0 ? 

#define XMDS $8f ; minitel output


#define XGOKBD $52 

#define XKBD $80 ; Keyboard
#define XSCR $88 ; Screen window 0






; others
#define BUFROU $C500 ; Routines for buffers gestion




#define FUFTRV $0100; working Buffer 