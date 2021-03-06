/********************************************************************** VECTORS used with brk */

/// @brief [VECTOR] open device on channel 0
#define XOP0        $00
/// @brief [VECTOR] open device on channel 1
#define XOP1        $01
/// @brief [VECTOR] open device on channel 2
#define XOP2        $02
/// @brief [VECTOR] open device on channel 3
#define XOP3        $03

/// @brief [VECTOR] close an I/O on a channel. If we try to close an I/O which is not open in the channel 0, the command is ignored 
#define XCL0        $04
/// @brief [VECTOR] close an I/O on a channel. If we try to close an I/O which is not open in the channel 1, the command is ignored 
#define XCL1        $05
/// @brief [VECTOR] close an I/O on a channel. If we try to close an I/O which is not open in the channel 2, the command is ignored 
#define XCL2        $06
/// @brief [VECTOR] close an I/O on a channel. If we try to close an I/O which is not open in the channel 3, the command is ignored 
#define XCL3        $07


/// @brief [VECTOR] test a char on channel 0 : if C=1 then no char detecxted, if C=0 then A accumulator contains ASCII char (X and Y not changed)
#define XRD0 $08
/// @brief [VECTOR] test a char on channel 1 : if C=1 then no char detecxted, if C=0 then A accumulator contains ASCII char (X and Y not changed)
#define XRD1 $09
/// @brief [VECTOR] test a char on channel 2 : if C=1 then no char detecxted, if C=0 then A accumulator contains ASCII char (X and Y not changed)
#define XRD2 $0A
/// @brief [VECTOR] test a char on channel 3 : if C=1 then no char detecxted, if C=0 then A accumulator contains ASCII char (X and Y not changed)
#define XRD3 $0B

#define XRDW0 $0C
#define XRDW1 $0D
#define XRDW2 $0E
#define XRDW3 $0F

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
/*!
 lda #<str
 ldy #>str
 BRK XWSTR0
str
 .asc "hello world !",0
*/
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
 brk XMINMA ; "b" will "B" in Accumulator
 */
#define XMINMA $1f 

#define XMUL40 $20 ; AY=A*40
#define XMULT $21
#define XADRES $22 ; AY*RES --> TR0-1-2-3  RES étant une adresse

#define XDIVIS $23 ; divide RES/AY=RES (RESB reste)

// Return the address of var
// For example, if we want to get PWD var
// ldx #PWD_PTR
// BRK_TELEMON(XVARS)
// A and Y contains low and high address of the vars
#define XVARS $24

/// @brief Do CRLF
/// This vector send 0x0a and 0x0d on channel 0
/*!
 return cursor at the beginning of next text line
 
 brk XCRLF
 */
#define XCRLF $25 

#define XDECAY $26 ; AY : decimal, length =X

/// @brief read bytes from sdcard need to have fileopen done
/*!
 lda #<$a000
 
 sta PTR_READ_DEST
 
 lda #>$a000
 
 sta PTR_READ_DEST+1 ; all bytes will in be $a000
 
 lda #$ff ; all bytes (65535 bytes)
 
 ldy #$ff
  
 brk XFREAD
 */
#define XFREAD $27 

#define XBINDX $28 ; Convert to decimal : numer un AY 

#define XDECIM $29  ; same as BINDX but displayed on channel 0 return in TR5 

#define XHEXA $2A ; convert A in YA in HEX

#define XA1AFF $2b ; display ACC1 on channel 0

#define XMENU $2c ; manage a menu AY

#define XEDT $2d

#define XINSER $2e ; insert a line

#define XSCELG $2f ; search line

/// @brief [PRIMITIVE] Open file from ch376 AY contains char * of the path
/*!
	lda #<volatile_str
	
	ldx #>volatile_str
	
	BRK_TELEMON(XOPEN)
*/	
#define XOPEN $30

/// @brief [PRIMITIVE] Open file from relative path depending of pwd, it opens only current path not a file
#define XOPENRELATIVE $31

#define XEDTIN $32

/// @brief [PRIMITIVE] Display prompt Char "#"
#define XECRPR $33 

/// @brief [PRIMITIVE] Switch off cursor
#define XCOSCR $34
///  
/// @param X (register)  [in] the number of the channel

/*!
	ldx #0
	
	BRK_TELEMON(XCOSCR)  ; switch off cursor
 */

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

/// @brief [PRIMITIVE] Close routine
#define XCLOSE $3a 

/// @brief [PRIMITIVE] Write from buffer AY : Length PTR_READ_DEST : WRITE buffer
#define XFWRITE $3b

/// @brief [PRIMITIVE] Reset clock
#define XRECLK $3c 

/// @brief [PRIMITIVE] Close clock (delete display)
#define XCLCL $3d

/*Clock*/

/// @brief [PRIMITIVE] Update clock
#define XWRCLK $3e

/// @brief [PRIMITIVE] seek file
/* 
; [IN] y whence
; [IN] AX position 
; [IN] RES fd
*/
#define XFSEEK $3F

/// @brief [PRIMITIVE] Open file which is send to the chip
//#define XCH376_FILE_OPEN $3f

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

/// @brief [PRIMITIVE] 
#define XMKDIR $4b

/// @brief [PRIMITIVE] hard copy of hires window
#define XHCHRS $4c

/// @brief [PRIMITIVE] remove file
#define XRM    $4D

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

/// @brief [PRIMITIVE] Cut off the line (minitel)
#define XDECON $65

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
/// @param Y (register)     [in] The desired high adress of the memory
/// @param X (register)     [in] Length of the string
#define XSCHAR $98 ; SCHAR string adress in AY, length in X 

/*PSG working */

/// @brief [PRIMITIVE] Send EXPLODE sound in PSG
#define XEXPLO $9c 

/// @brief [PRIMITIVE] Send PING sound in PSG
#define XPING $9d

// Remove me 
#define XMDS $8f ; minitel output

#define XGOKBD $52 

#define XKBD $80 ; Keyboard
#define XRSE $83 /* RS232 INPUT  */
#define XSCR $88 ; Screen window 0
#define XRSS $90 /* RS232 OUTPUT */

; others
#define MALLOC_TABLE $D000 ; RAM OVERLAY Only in telemon 3.0
//  RAM OVERLAY Only in telemon 3.0 length 
// FORMAT 
// $D000 to $D0FF : ptr to filed opened
// ex :
// $D000-$D001 = $D100 : 
#define FILE_OPEN_TABLE $D000 

#define BUF1   $C100 ; Stratsed buffer

#define BUFBUF $C080 ; buffers definition
#define BUFROU $C500 ; Routines for buffers gestion
#define XECRBU $C51D 
#define XLISBU $C518
#define XTSTBU $c50f

#define FUFTRV $0100; working Buffer 

/// @brief [PRIMITIVE] Buffer minitel VDT attributes
#define BVDTAS $9000

/// @brief [PRIMITIVE] Work on numeric and alphanumeric
#define BUFTRV $100 


/*  ORIX but telemon must know theses values */
#define PATH_CURRENT_MAX_LEVEL 4 ; Only in telemon 3.0 number of level, if we add more, we should have to many RAM, if you need to set more level add bytes here : ptr_path_current_low and ptr_path_current_high
#define MAX_LENGTH_OF_FILES 9 // We say 8 chars for directory and end of string


#define ORIX_MAX_PATH_LENGTH MAX_LENGTH_OF_FILES*PATH_CURRENT_MAX_LEVEL+PATH_CURRENT_MAX_LEVEL

#define MAX_LENGTH_BUFEDT 109

; Predefined file handles
#define STDIN_FILENO     0
#define STDOUT_FILENO    1
#define STDERR_FILENO    2

; File mode constants, must match the values in the C headers
#define O_RDONLY         $01
#define O_WRONLY         $02
#define O_RDWR           $03
#define O_CREAT          $10
#define O_TRUNC          $20
#define O_APPEND         $40
#define O_EXCL           $80

#define EOF             -1
#define FOPEN_MAX       8
#define SEEK_CUR        0
#define SEEK_END        1
#define SEEK_SET        2

/* Possible error codes */
#define ENOENT          1       /* No such file or directory */
#define ENOMEM          2       /* Out of memory */
#define EACCES          3       /* Permission denied */
#define ENODEV          4       /* No such device */
#define EMFILE          5       /* Too many open files */
#define EBUSY           6       /* Device or resource busy */
#define EINVAL          7       /* Invalid argument */
#define ENOSPC          8       /* No space left on device */
#define EEXIST          9       /* File exists */
#define EAGAIN          10      /* Try again */
#define EIO             11      /* I/O error */
#define EINTR           12      /* Interrupted system call */
#define ENOSYS          13      /* Function not implemented */
#define ESPIPE          14      /* Illegal seek */
#define ERANGE          15      /* Range error */
#define EBADF           16      /* Bad file number */
#define ENOEXEC         17      /* Exec format error */
#define EUNKNOWN        18      /* Unknown OS specific error */


// Below work in progress


/*
[IN] RES the string of the path 
[OUT] A contains the ID of the file ptr. IF A = 0 then there is an error
[OUT] X contains the ID of the error
*/
#define ORIX_REGISTER_FILEHANDLE $00
#define ORIX_GETENV $01


#define ORIX_ROUTINES $ffe0



#define ORIX_PWD_VAR $00 ; must be removed used in telemon instead


#define ORIX_MAX_FILEOPEN        5 ; /sdin, stdout,stderr
