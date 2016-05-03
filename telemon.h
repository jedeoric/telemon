
/*VECTORS*/

#define XOP0 $00
#define XOP1 $01

#define XWSTR0 $14 ; put a str on channel 0
#define XWSTR1 $15 ; put a str on channel 1


#define XWR0 $10 ; put a character on channel 0
#define XWR1 $11 ; put a character on channel 1
#define XWR1 $12 ; put a character on channel 2
#define XWR1 $13 ; put a character on channel 3


#define XTEXT 	$19 ; switch to text
#define XHIRES 	$1A ; switch to HIRES
#define XEFFHI 	$1B ; clear HIRES


#define XCRLF $25 ; send on channel 0, RC and LF (Return and line feed)

#define XCSSCR $35 ; display cursor (prompt)	, X equal to 0 : No window

#define XKBD $80 ; Keyboard
#define XSCR $88 ; Screen window 0


 
#define XZAP $46
#define XOUPS $42
#define XSHOOT $47





; XNOMFI : Length in X -> BUFNOM and (A and Y for str)
; X=0 0length
; X=1 if RAS C=1 if jokers
; x=2 ; if drive by default has changed
; x>127 ; incorrect name
#define XNOMFI $24


; others
#define BUFROU $C500 ; Routines for buffers gestion




#define FUFTRV $0100; working Buffer 