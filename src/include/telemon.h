
; Page 0 variables

/********************************************************************** PAGE 0 VARIABLES */

/// @brief [VALUE_PAGE_0] 16 bits used for address usage
#define RES $00 
/// @brief [VALUE_PAGE_0] 16 bits used for address usage
#define RESB $02 

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
#define TR5 $11 ; general usage 1 byte
#define TR6 $12 ; general usage 1 byte
#define TR7 $13 ; general usage 1 byte




#define DEFAFF $14 ; default value for decimal conversion


#define work_channel $19

#define i_o_counter $1a

#define i_o_save $1b

#define TRANSITION_RS232 $1e 

/// @brief [VALUE_PAGE_0] save Accumulator used when there is an IRQ
#define IRQSVA $21 
/// @brief [VALUE_PAGE_0] save X used when there is an IRQ
#define IRQSVX $22 
/// @brief [VALUE_PAGE_0] save Y used when there is an IRQ
#define IRQSVY $23 
/// @brief [VALUE_PAGE_0] save P used when there is an IRQ but also used for some others backup values
#define IRQSVP $24 

/// @brief [VALUE_PAGE_0] low Adress of begin line display (16 bits)
#define ADSCR $26 

/// @brief [VALUE_PAGE_0] number window screen
#define SCRNB $28

#define ADKBD $2a ; ASCII conversion table adress

// VIDEOTEX HIRES

#define ADVDT $2c ; adress hires screen

#define ADASC $2e ; ascii table

#define ADATR $30 ;adress table attributes

#define VDTPAR $32 ; VDT work

#define VDTASC $33 ; VDT work


#define VDTX $38
#define VDTY $39
#define VDTGX $3a
#define VDTGY $3b
/// @brief [VALUE_PAGE_0] ?
#define FLGVD0 $3c



/// @brief [VALUE_PAGE_0] address to display clock
#define ADCLK $40 



/// @brief [VALUE_PAGE_0] decompteur utilisateur en secondes (16 bits)
#define TIMEUS $42

/// @brief [VALUE_PAGE_0] decompteur utilisateur en dixieme de secondes (16 bits)
#define TIMEUD $44

#define RS232T $59
#define RS232C $5A


/// @brief [VALUE_PAGE_0] 48 bytes 
#define VARMNB $60

#define MENDDX $61
#define MENDDY $62
#define MENDFY $63


/// @brief [VALUE_PAGE_0] 68 bytes for language
#define VARLNG $8c

/// @brief [VALUE_PAGE_0] 48 bytes for the application 
#define VARAPL $D0 

/********************************************************************** PAGE 2 VARIABLES */

#define BNKST $0200 ; RESB 8 value of bytes $fffb of each bank

/// @brief [VALUE_PAGE_2]  Activating drive 0 if not connected, b7 equal double side
#define TABDRV $0208 




/// @brief [VALUE_PAGE_2]  Default drive
#define DRVDEF $20C

/// @brief [VALUE_PAGE_2]  Flag telestrat

/// b7=1 HIRES mode
/// b6=1 Minitel mode
/// b5=1 degre mode : 0 (calcul radians )
/// b2=1 BONJOURCOM exists
/// b1=1 Printer detected 
/// b0=1 stratsed is missing

#define FLGTEL $020D

#define KOROM $020E	; Ko ROM total
#define KORAM $020f ; total Max ram Bytes	

/// @brief [VALUE_PAGE_2]  horloge 1/10
#define TIMED $210
/// @brief [VALUE_PAGE_2]  clock seconds
#define TIMES $211

/// @brief [VALUE_PAGE_2]  clock minutes
#define TIMEM $212

/// @brief [VALUE_PAGE_2]  clock hours
#define TIMEH $213

/// @brief [VALUE_PAGE_2]  clock flag
/// b7 display clock every seconds
#define FLGCLK $214

#define FLGCLK_FLAG $215 

/// @brief [VALUE_PAGE_2]  cursor management flag
#define FLGCUR $216

/// @brief [VALUE_PAGE_2]  cursor state flag
#define FLGCUR_STATE $217


/// @brief [VALUE_PAGE_2] low address of the screen 0 
#define ADSCRL $218

/// @brief [VALUE_PAGE_2] high address of the screen 0 
#define ADSCRH $21c


/// @brief [VALUE_PAGE_2]  screen X cursor screen 0
#define SCRX $220
/// @brief [VALUE_PAGE_2]  screen X cursor screen 1
#define SCRX1 $221
/// @brief [VALUE_PAGE_2]  screen X cursor screen 2
#define SCRX2 $222
/// @brief [VALUE_PAGE_2]  screen X cursor screen 3
#define SCRX3 $223

/// @brief [VALUE_PAGE_2]  screen Y cursor screen 0
#define SCRY $224
/// @brief [VALUE_PAGE_2]  screen Y cursor screen 1
#define SCRY1 $225
/// @brief [VALUE_PAGE_2]  screen Y cursor screen 2
#define SCRY2 $226
/// @brief [VALUE_PAGE_2]  screen Y cursor screen 3
#define SCRY3 $227

/// @brief [VALUE_PAGE_2]  beginning of the window
#define SCRDX $228
/// @brief [VALUE_PAGE_2]  beginning of the screen 0
#define SCRDY $230

/// @brief [VALUE_PAGE_2]  end of the screen 0
#define SCRFY $234 


/// @brief [VALUE_PAGE_2]  low address of the screen 0
#define SCRBAL $238 

/// @brief [VALUE_PAGE_2]  high address of the screen 0
#define SCRBAH $23c


/// @brief [VALUE_PAGE_2] ink color for screen 0
#define SCRCT $0240 

/// @brief [VALUE_PAGE_2] ink color for screen 1
#define SCRCT1 $0241

/// @brief [VALUE_PAGE_2] ink color for screen 2
#define SCRCT2 $0242

/// @brief [VALUE_PAGE_2] ink color for screen 3
#define SCRCT3 $0243

/// @brief [VALUE_PAGE_2] paper color for screen 0
#define SCRCF $244
/// @brief [VALUE_PAGE_2] paper color for screen 1
#define SCRCF1 $245
/// @brief [VALUE_PAGE_2] paper color for screen 2
#define SCRCF2 $246
/// @brief [VALUE_PAGE_2] paper color for screen 3
#define SCRCF3 $247


/// @brief [VALUE_PAGE_2]  flag for screen
/// b7 : display cursor
/// b6 : cursor does not blink
/// b5 : inverted video
/// b4 : 38/40 column mode ?
/// b3 : escape ?
/// b2 : US ?
/// b1 : double height
/// b0 : counter for us
#define FLGSCR $248


#define CURSCR $24c ; char on the cursor

#define SCRTXT $0256 ; desc scrtxt 6 bytes

#define SCRHIR $025C ; desc 6 bytes for HIres

#define SCRTRA $0262 ; desc 6 bytes for trace

/// @brief [VALUE_PAGE_2]  image of 8x8 keyboard matrix (8 bytes)
#define KBDCOL $268
/// @brief [VALUE_PAGE_2]  0 if no key pressed
#define KBDFLG_KEY $270

/// @brief [VALUE_PAGE_2]  contains key pressed
#define KBDKEY $271 

#define FLGKBD $0275	;Keyboard flag : b7 majn b6 if sound

#define LPRX $0286 ; word cursor in the line

#define LPRFX $0288 ; printer width

#define FLGLPR $028a ;; word b7 ready

/// @brief [VALUE_PAGE_2]  flag joystick
/// b6=1 if left joystick
/// b0=1 if  mouse
#define FLGJCK $028c

/// @brief [VALUE_PAGE_2]  value of left joystick
#define JCGVAL $028d

/// @brief [VALUE_PAGE_2]  value of right joystick
#define JCDVAL $028e

/// @brief [VALUE_PAGE_2] value of joystick by default : $0b $0a $20 $08 $09 $03 $03 these value must be verifyed 
#define JCKTAB $029d


/// @brief [VALUE_PAGE_2]  Activating channel 0
#define IOTAB0 $02ae ; activating channel 0
/// @brief [VALUE_PAGE_2]  Activating channel 1
#define IOTAB1 $02b2 ; activating channel 1
/// @brief [VALUE_PAGE_2]  Activating channel 2
#define IOTAB2 $02b6 ; activating channel 2
/// @brief [VALUE_PAGE_2]  Activating channel 3
#define IOTAB3 $02ba ; activating channel 3


#define ADIOB $02be ; 48 bytes ? I/O management address


#define FLGRST $02ee
#define CSRND $02EF ; current value of random generator









#define VIRQ $02FA
#define VNMI $02F4
#define VAPLIC $2FD ; No banque adress

/********************************************************************** PAGE 4 VARIABLES */

#define VEXBNK $414
#define BNKCIB $417

/********************************************************************** PAGE 5 VARIABLES */

#define BUFNOM $517
/// @brief [VALUE_PAGE_2]  edition buffer
#define BUFEDT $590

/********************************************************************** VECTORS used with brk */

/// @brief [VECTOR] open device on channel 0
#define XOP0 $00
/// @brief [VECTOR] open device on channel 1
#define XOP1 $01
/// @brief [VECTOR] open device on channel 2
#define XOP2 $02
/// @brief [VECTOR] open device on channel 3
#define XOP3 $03

/// @brief [VECTOR] close an I/O on a channel. If we try to close an I/O which is not open in the channel 0, the command is ignored 
#define XCL0 $04
/// @brief [VECTOR] close an I/O on a channel. If we try to close an I/O which is not open in the channel 1, the command is ignored 
#define XCL1 $05
/// @brief [VECTOR] close an I/O on a channel. If we try to close an I/O which is not open in the channel 2, the command is ignored 
#define XCL2 $06
/// @brief [VECTOR] close an I/O on a channel. If we try to close an I/O which is not open in the channel 3, the command is ignored 
#define XCL3 $07


/// @brief [VECTOR] test a char on channel 0 : if C=1 then no char detecxted, if C=0 then A accumulator contains ASCII char (X and Y not changed)
#define XRD0 $08
/// @brief [VECTOR] test a char on channel 1 : if C=1 then no char detecxted, if C=0 then A accumulator contains ASCII char (X and Y not changed)
#define XRD1 $09
/// @brief [VECTOR] test a char on channel 2 : if C=1 then no char detecxted, if C=0 then A accumulator contains ASCII char (X and Y not changed)
#define XRD2 $0a
/// @brief [VECTOR] test a char on channel 3 : if C=1 then no char detecxted, if C=0 then A accumulator contains ASCII char (X and Y not changed)
#define XRD3 $0b


#define XRDW0 $0c
#define XRDW1 $0d
#define XRDW2 $0e
#define XRDW3 $0f


/// @brief [VECTOR] display a char on text mode on channel 0
/// This vector move memory 
/// @param A (accumulator)  [in] ASCII value of the char
/*!
 lda #"A"
 
 brk XWR0
*/
#define XWR0 $10 

#define XWR1 $11 ; put a character on channel 1
#define XWR2 $12 ; put a character on channel 2
#define XWR3 $13 ; put a character on channel 3

/// @brief [VECTOR] display string on text mode on channel 0
/// @param A (accumulator)  [in] The desired low adress of the string
/// @param Y (register)  [in] The desired high adress of the string
/// string must be terminated by 0. Manage CRLF
#define XWSTR0 $14 

/// @brief [VECTOR] display string on text mode on channel 1
/// @param A (accumulator)  [in] The desired low adress of the string
/// @param Y (register)  [in] The desired high adress of the string
/// string must be terminated by 0. Manage CRLF
#define XWSTR1 $15


#define XWSTR2 $16
#define XWSTR3 $17

/// @brief [PRIMITIVE] move memory 
/// This vector move memory 
/// @param A (accumulator)  [in] The desired low adress of the memory 
/// @param Y (register)  [in] The desired high adress of the memory

/*!
 Scroll one line in hires to the bottom
 
 lda #<$a000
 
 ldy #>$a000
 
 sta DECDEB
 
 sty DECDEB+1
 
 lda #<$bf40
 
 ldy #>$bf40
 
 sta DECFIN 

 sty DECFIN+1

 lda #<$a028

 ldy #>$a028
 
 sta DECCIB 
 
 sty DECCIB+1-2-3
 
 BRK XDECAL
 
 */
 

 
/// @see DECFIN DECDEB DECCIB
/// 
#define XDECAL $18 

/// @brief [PRIMITIVE] Switch to text mode
#define XTEXT 	$19 

/// @brief [PRIMITIVE] Switch to hires mode
#define XHIRES 	$1A 

/// @brief [PRIMITIVE] Clear Hires
#define XEFFHI 	$1B





#define XFILLM $1C ; FILL

/// @brief [PRIMITIVE] found this adress of a char
#define ZADCHA $1d

/// @brief [PRIMITIVE] testing if printer is connected
#define XTSTLP $1e
#define XMINMA $1f ; A register converted to upper XY not changed


#define XMUL40 $20 ; AY=A*40
#define XMULT $21
#define XMULT $22 ; AY*RES --> TR0-1-2-3  RES étant une adresse

#define XDIVIS $23 ; divide RES/AY=RES (RESB reste)

// XNOMFI : Length in X -> BUFNOM and (A and Y for str)
// X=0 0length
// X=1 if RAS C=1 if jokers
//  x=2 ; if drive by default has changed
//  x>127 ; incorrect name
#define XNOMFI $24


/// @brief Do CRLF
/// This vector send 0x0a and 0x0d on channel 0
/*!
 return cursor at the beginning of next text line
 
 brk XCRLF
 */
#define XCRLF $25 



#define XDECAY $26 ; AY : decimal, length =X

#define XBINDX $28 ; Convert to decimal : numer un AY 

#define XDECIM $29  ; same as BINDX but displayed on channel 0 return in TR5 

#define XHEXA $2A ; convert A in YA in HEX

#define XA1AFF $2b ; display ACC1 on channel 0

#define XMENU $2c ; manage a menu AY

#define XEDT $2d

#define XINSER $2e ; insert a line

#define XSCELG $2f ; search line

#define XVDTCT $30 ; VDT Control MINITEL

/// @brief [PRIMITIVE] Minitel : display mosaic
#define XVDTG2 $31

#define XEDTIN $32

/// @brief [PRIMITIVE] Display prompt
#define XECRPR $33 ; display prompt

#define XCOSCR $34

/// @brief [PRIMITIVE] Display cursor (prompt)	, X equal to 0 : No window
#define XCSSCR $35 

#define XSCRSE $36

#define XSCROH $37

#define XSCROB $38

#define XSCRNE $39

#define XCLCL $3d

#define XWRCLK $3e


#define XSONPS $40

#define XEPSG $41

/* PSG Working 1/2 */

/// @brief [PRIMITIVE] Send OUPS sound in PSG
#define XOUPS $42

#define XPLAY $43

#define XSOUND $44 
#define XMUSIC $45 
/// @brief [PRIMITIVE] Send ZAP sound in PSG
#define XZAP $46
/// @brief [PRIMITIVE] Send SHOOT sound in PSG
#define XSHOOT $47

#define XALLKB $50 ; Get keyboard, --> KBDCOL

#define XA1PA2 $6A ; A1+A2 --> A1

/* GRAPHICS WORKING*/
/// @brief [PRIMITIVE] Do a circle un HIRES
#define XCIRCL $8f
/// @brief [PRIMITIVE] change position cursor in Hires  absolute
#define XCURSE $90 
/// @brief [PRIMITIVE] change position cursor in Hires relative
#define XCURMO $91 
/// @brief [PRIMITIVE] change paper X= no window, 128 if hires
#define XPAPER $92
/// @brief [PRIMITIVE] change ink A value 0-7 or 16-23
#define XINK $93
#define XBOX $94 ; BOX
#define XABOX $95 ; ABOX
#define XFILL $96 ; FILL
/// @brief [PRIMITIVE] put a char un Hires
#define XCHAR $97 ; XCHAR

/// @brief [PRIMITIVE] put a string un Hires
/// @param A (accumulator)  [in] The desired low adress of the memory 
/// @param Y (register)  [in] The desired high adress of the memory
/// @param X (register)  [in] Length of the string
#define XSCHAR $98 ; SCHAR string adress in AY, length in X 

/*PSG working */

/// @brief [PRIMITIVE] Send EXPLODE sound in PSG
#define XEXPLO $9c 

/// @brief [PRIMITIVE] Send PING sound in PSG
#define XPING $9d




#define XMDS $8f ; minitel output


#define XGOKBD $52 

#define XKBD $80 ; Keyboard
#define XSCR $88 ; Screen window 0






; others
#define BUFROU $C500 ; Routines for buffers gestion
#define XECRBU $C51D 
#define XLISBU $C518

#define XTSTBU $c50f


#define FUFTRV $0100; working Buffer 




