
/// @brief [VALUE_PAGE_2]  contains key pressed
#define KBDKEY $271 

/// @brief [VALUE_PAGE_2]  Manage function key



#define KBDSHT $278 ; Contains informations from key pressed

#define KBDKEY $279 ; ASCII key

#define KBDCTC $27E ; if ctrl+c is done if b7 equal to 1

#define LPRX $0286 ; word cursor in the line

#define LPRY $0287



#define LPRFX $0288 ; printer width

#define LPRFY $0289 ; 

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


#define KEYBOARD_COUNTER $02a6

#define HRSPAT $02aa

#define HRSERR $02ab

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










#define VNMI $02F4
#define VAPLIC $2FD ; No banque adress
#define VIRQ $02fa ; Updated for atmos compatibility
/********************************************************************** PAGE 4 VARIABLES */

#define VEXBNK $414
#define BNKCIB $417

/// @brief [VALUE_PAGE_4] Store the old bank (if we call telemon from bank4, the id of the bank is stored here)
#define BNKOLD $40F 


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



/// @brief [PRIMITIVE] Fill a memory bloc with A

#define XFILLM $1C ; FILL

/// @brief [PRIMITIVE] found an adress of a char
#define ZADCHA $1d

/// @brief [PRIMITIVE] testing if printer is connected
#define XTSTLP $1e
/// @brief [PRIMITIVE] Converting A from lower case to uppercase XY are not changed
/*!
 lda #"b"
 brk XMINMA
 */
#define XMINMA $1f 


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
///  
/// @param X (register)  [in] the number of the channel

/*!
	ldx #0
	BRK_TELEMON(XCSSCR)  ; display cursor
 */

#define XSCRSE $36
/// @brief [PRIMITIVE] Scroll high
#define XSCROH $37

/// @brief [PRIMITIVE] Scroll bottom
#define XSCROB $38

/// @brief [PRIMITIVE] Load charset from rom to ram 
#define XSCRNE $39 

/// @brief [PRIMITIVE] Wait CH376 response
#define XCH376_WAIT_RESPONSE $3a

/// @brief [PRIMITIVE] Set filename : input BUFNOM terminated by 0
#define XCH376_SET_FILE_NAME $3b

/// @brief [PRIMITIVE] Reset clock

#define XRECLK $3c 
/// @brief [PRIMITIVE] Close clock (delete display)
#define XCLCL $3d

/*Clock*/


/// @brief [PRIMITIVE] Update clock
#define XWRCLK $3e

/// @brief [PRIMITIVE] Open file which is send to the chip
#define XCH376_FILE_OPEN $3f

/// @brief [PRIMITIVE] Send 14 data (set in xy) in psg register (R0 to r13)
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

/// @brief [PRIMITIVE] Send A to the printer (or any hardware on parallel port)
#define XLPRBI $48


/// @brief [PRIMITIVE] Send CR/LF to the printer (or any hardware on parallel port)
#define XLPCRM $49

/// @brief [PRIMITIVE] hard copy of text window
#define XHCSCR $4a

/// @brief [PRIMITIVE] Set usb mode 
#define XCH376_SET_USB_MODE $4b

/// @brief [PRIMITIVE] hard copy of hires window
#define XHCHRS $4c

/// @brief [PRIMITIVE] Start disk mount
#define XCH376_DISK_MOUNT $4d

#define XALLKB $50 ; Get keyboard, --> KBDCOL

/// @brief [PRIMITIVE] Get keyboard with funct, ctrl or shift pressed
#define XKBDAS $51

/// @brief [PRIMITIVE] Change the keyboard type (A contains the keyboard)
#define XGOKBD $52

/// @brief [PRIMITIVE] Write A or AY in the buffer
#define XECRBU $54

/// @brief [PRIMITIVE] Read A or AY in the buffer
#define XLISBU $55

#define XTSTBU $56

/// @brief [PRIMITIVE] Flush the buffer 
#define XVIDBU $57

/// @brief [PRIMITIVE] Initialize the buffer X
#define XINIBU $58 

/// @brief [PRIMITIVE] Reset all value of the buffer
#define XDEFBU $59

/// @brief [PRIMITIVE] Test if the buffer is empty
#define XBUSY $5a

/// @brief [PRIMITIVE] Do a rs232 dump
#define XSDUMP $5c

/// @brief [PRIMITIVE] Do a rs232 dump and send it to the screen
#define XCONSO $5d

/// @brief [PRIMITIVE] Load a file from RS232
#define XSLOAD $5e


/// @brief [PRIMITIVE] Save a file to RS232
#define XSSAVE $5f

/// @brief [PRIMITIVE] Same as XSLOAD but input is minitel
#define XMLOAD $60

/// @brief [PRIMITIVE] Same as XSSAVE but input is minitel
#define XMSAVE $61


/// @brief [PRIMITIVE] Waiting for ring (minitel)
#define XRING $62

/// @brief [PRIMITIVE] Waiting for cnx/end from minitel (minitel)
#define XWCXFI $63

/// @brief [PRIMITIVE] Get the line (minitel)
#define XLIGNE $64

/// @brief [PRIMITIVE] Cut off the line (minitel)
#define XDECON $65

/// @brief [PRIMITIVE] Send A to the minitel (minitel)
#define XMOUT $66

/// @brief [PRIMITIVE] Send A to the rs232
#define XSOUT $67


/// @brief [PRIMITIVE] Convert ACC1 in BUFTRV
#define XA1DEC $68


/// @brief [PRIMITIVE] Convert un ACC1 the ascii number in AY. Binary value is converted quicker in this routine
#define XDECA1 $69

/// @brief [PRIMITIVE] Acc1+Acc2 = acc1
#define XA1PA2 $6A ; A1+A2 --> A1

/// @brief [PRIMITIVE] Acc1-Acc2 = acc1
#define XA2NA1 $6b


/// @brief [PRIMITIVE] Acc1*Acc2 = acc1
#define XA1MA2 $6c

/// @brief [PRIMITIVE] Acc2/Acc1 = acc1
#define XA2DA1 $6d

/// @brief [PRIMITIVE] Acc2^Acc1 = acc1
#define XA2EA1 $6e

/// @brief [PRIMITIVE] -ACC1
#define XNA1 $6f

#define XSIN $70

#define XCOS $71

#define XTAN $72

#define XATN $73

#define XEXP $74

#define XLN $75

#define XLOG $76

#define XRND $77

#define XSQR $78

#define XRAD $79

#define XDEG $7a

#define XINT $7b

#define XPI $7c

#define XRAND $7d

#define XA1A2 $7e

#define XA2A1 $7f

#define XIYAA1 $80

#define XAYA1 $81

#define XA1IAY $82

#define XA1XY $83

#define XAA1 $84

#define XADNXT $85

#define XINTEG $86

#define XHRSCG $88

#define XHRSCD $89

#define XHRSCB $8a

#define XHRSCH $8b

#define XHRSSE $8c

#define XDRAWA $8d

#define XDRAWR $8e

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

/// @brief [PRIMITIVE] Buffer minitel VDT attributes
#define BVDTAS $9000

/// @brief [PRIMITIVE] Work on numeric and alphanumeric
#define BUFTRV $100 





/********** MACROS **********/


#define BRK_TELEMON(value)\
	.byt 00,value;




