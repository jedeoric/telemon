
#define PATH_CURRENT_MAX_LEVEL 4 ; number of level, if we add more, we should have to many RAM, if you need to set more level add bytes here : ptr_path_current_low and ptr_path_current_high
#define MAX_LENGTH_OF_FILES 9 // We say 8 chars for directory and end of string

#define ORIX_MAX_OPEN_FILES 2

#define ORIX_MAX_PATH_LENGTH MAX_LENGTH_OF_FILES*PATH_CURRENT_MAX_LEVEL+PATH_CURRENT_MAX_LEVEL





; DON'T add .text directive !!!


.zero
*=$00

/// @brief [VALUE_PAGE_0] 16 bits used for address usage
RES
.dsb 2
/// @brief [VALUE_PAGE_0] 16 bits used for address usage
; $02
RESB
.dsb 2
; $04
DECDEB
.dsb 2
; $06
DECFIN
.dsb 2
;8
DECCIB
.dsb 2
;0a
DECTRV
.dsb 2
;0c
TR0
.dsb 1
TR1
.dsb 1
TR2
.dsb 1
TR3
.dsb 1
TR4
.dsb 1
TR5
.dsb 1
TR6
.dsb 1
TR7 ;$13
.dsb 1

; default value for decimal conversion
DEFAFF  ; $14
.dsb 1
ADDRESS_READ_BETWEEN_BANK ;$15
.dsb 2 ; FIXME
ADDRESS_VECTOR_FOR_ADIOB
.dsb 2
work_channel ;$19
.dsb 1
i_o_counter; $1a
.dsb 1
i_o_save; $1b
.dsb 3 ; FIXME
TRANSITION_RS232 ; $1e 
.dsb 3 ; FIXME

/// @brief [VALUE_PAGE_0] save Accumulator used when there is an IRQ
IRQSVA; $21 
.dsb 1
/// @brief [VALUE_PAGE_0] save X used when there is an IRQ
IRQSVX ;$22
.dsb 1 
/// @brief [VALUE_PAGE_0] save Y used when there is an IRQ
IRQSVY ;$23
.dsb 1 
/// @brief [VALUE_PAGE_0] save P used when there is an IRQ but also used for some others backup values
IRQSVP ;$24 
.dsb 1
.dsb 1 ; FIXME
/// @brief [VALUE_PAGE_0] low Adress of begin line display (16 bits)
ADSCR ;$26 
.dsb 2
/// @brief [VALUE_PAGE_0] number window screen
SCRNB ; $28
.dsb 2
ADKBD ; $2a ; ASCII conversion table adress
.dsb 2
// VIDEOTEX HIRES
PTR_READ_DEST ; PTR_READ for read routine : why no RES ? Because, it can be use during IRQ without corript
.dsb 2
ADASC ; $2e ; ascii table
.dsb 2
ADATR ; $30 ;adress table attributes
.dsb 2
ptr1 ; $32 ; VDT work
.dsb 2
VDTASC ; $33 ; VDT work
.dsb 5 ; FIXME
VDTX ;$38
.dsb 1
VDTY ;$39
.dsb 1
VDTGX ;$3a
.dsb 1
VDTGY ;$3b

/// @brief [VALUE_PAGE_0] ?
FLGVD0; $3c
.dsb 4



/// @brief [VALUE_PAGE_0] address to display clock
ADCLK ;$40 
.dsb 2


/// @brief [VALUE_PAGE_0] decompteur utilisateur en secondes (16 bits)
TIMEUS ;$42

.dsb 2
/// @brief [VALUE_PAGE_0] decompteur utilisateur en dixieme de secondes (16 bits)
TIMEUD ;$44

.dsb 2
; $46 
HRSX
.dsb 1 
; $47
HRSY
.dsb 1
; dont know FIXME
.dsb 1
HRSX40
; $49
.dsb 1
HRSX6
; 4a
.dsb 1
ADHRS
.dsb 2
; $48
HRS1 ; $4d
.dsb 2
HRS2 ;$4f
.dsb 2
HRS3; $51
.dsb 2
HRS4 ;$53
.dsb 2
HRS5
.dsb 2
HRSFB ; b7-b6)
.dsb 1

/// @brief [VALUE_PAGE_0] Used to backup value in buffer routine and joystick and to do some operation

VABKP1 ; $58
.dsb 1

RS232T ;$59 used in C code by orixcfg
.dsb 1
RS232C ; $5A used in C code by orixcfg
.dsb 1
; $5b
/// @brief [VALUE_PAGE_0] RS indicator b7=1 if minitel mode. 0= rs232
/// b6=1 entête or not
INDRS ; $5b
.dsb 5 ; FIXME one byte ?
/// @brief [VALUE_PAGE_0] Maths vars

ACC1E ; $60
MEN
.dsb 1
ACC1M
/// @brief [VALUE_PAGE_0] Floating variables also for menu vars
MENDDX ; $61
.dsb 1
MENDDY ; $62
.dsb 1
MENDFY ; $63
.dsb 2 ; FIXME
ACC1S ; $65
.dsb 3 ; FIXME
/// @brief [VALUE_PAGE_0] Working flag menu
FLGMEN ; $68
.dsb 1

/// @brief [VALUE_PAGE_0] choice table
ADMEN  ;$69

.dsb 16+6+11
; $8a
FLSGN
.dsb 1
; $8b
FLERR
.dsb 1
/// @brief [VALUE_PAGE_0] 44 bytes for language (used for hyperbasic for example)
VARLNG ; $8c
.dsb 44+24
/// @brief [VALUE_PAGE_0] 48 bytes for the application 
VARAPL ; $D0
.dsb 48


*=$200
/// @brief [VALUE_PAGE_2]  Save 8 bytes of the eack bank
; RESB 8 value of bytes $fffb of each bank
BNKST ; $0200 
.dsb 8
/// @brief [VALUE_PAGE_2]  Activating drive 0 if not connected, b7 equal double side
TABDRV ;$0208 
.dsb 4
/// @brief [VALUE_PAGE_2]  Default drive
DRVDEF ; $20C
 .dsb 1
 /// @brief [VALUE_PAGE_2]  Flag telestrat

/// b7=1 HIRES mode
/// b6=1 Minitel mode
/// b5=1 degre mode : 0 (calcul radians )
/// b2=1 BONJOURCOM exists
/// b1=1 Printer detected 
/// b0=1 stratsed is missing

FLGTEL; $020D
.dsb 1
KOROM ;$020E	; Ko ROM total
.dsb 1
KORAM ;$020f ; total Max ram Bytes	
.dsb 1

/// @brief [VALUE_PAGE_2]  horloge 1/10
TIMED ;$210
.dsb 1
/// @brief [VALUE_PAGE_2]  clock seconds
TIMES ;$211
.dsb 1

/// @brief [VALUE_PAGE_2]  clock minutes
TIMEM ;$212
.dsb 1
/// @brief [VALUE_PAGE_2]  clock hours
TIMEH ;$213
.dsb 1
/// @brief [VALUE_PAGE_2]  clock flag
/// b7 display clock every seconds
FLGCLK ;$214
.dsb 1
FLGCLK_FLAG ;;$215 
.dsb 1
/// @brief [VALUE_PAGE_2]  cursor management flag
FLGCUR ;$216
.dsb 1
/// @brief [VALUE_PAGE_2]  cursor state flag
FLGCUR_STATE ;$217
.dsb 1

/// @brief [VALUE_PAGE_2] low address of the screen 0 1 2 3 
ADSCRL ;$218
.dsb 4

/// @brief [VALUE_PAGE_2] high address of the screen 0 1 2 3 
ADSCRH; $21c
.dsb 4

/// @brief [VALUE_PAGE_2]  screen X cursor screen 0
SCRX ;$220
.dsb 1
/// @brief [VALUE_PAGE_2]  screen X cursor screen 1
SCRX1 ;$221
.dsb 1
/// @brief [VALUE_PAGE_2]  screen X cursor screen 2
SCRX2;$222
.dsb 1
/// @brief [VALUE_PAGE_2]  screen X cursor screen 3
SCRX3 ;$223
.dsb 1

/// @brief [VALUE_PAGE_2]  screen Y cursor screen 0
SCRY ;$224
.dsb 1
/// @brief [VALUE_PAGE_2]  screen Y cursor screen 1
SCRY1; $225
.dsb 1
/// @brief [VALUE_PAGE_2]  screen Y cursor screen 2
SCRY2;$226
.dsb 1
/// @brief [VALUE_PAGE_2]  screen Y cursor screen 3
SCRY3 ;$227
.dsb 1


/// @brief [VALUE_PAGE_2]  beginning of the window
SCRDX ;$228
.dsb 4
SCRFX ;$22c
.dsb 4
/// @brief [VALUE_PAGE_2]  beginning of the screen 0
SCRDY; $230
.dsb 4
/// @brief [VALUE_PAGE_2]  end of the screen 0
SCRFY ;$234 
.dsb 4

/// @brief [VALUE_PAGE_2]  low address of the screen 0
SCRBAL ;$238 
.dsb 4
/// @brief [VALUE_PAGE_2]  high address of the screen 0
SCRBAH ;$23c
.dsb 4

/// @brief [VALUE_PAGE_2] ink color for screen 0
SCRCT ;$0240 
.dsb 1
/// @brief [VALUE_PAGE_2] ink color for screen 1
SCRCT1 ;$0241
.dsb 1
/// @brief [VALUE_PAGE_2] ink color for screen 2
SCRCT2 ;$0242
.dsb 1
/// @brief [VALUE_PAGE_2] ink color for screen 3
 SCRCT3 ;$0243
.dsb 1

/// @brief [VALUE_PAGE_2] paper color for screen 0
SCRCF ;$244
.dsb 1
/// @brief [VALUE_PAGE_2] paper color for screen 1
SCRCF1 ;$245
.dsb 1
/// @brief [VALUE_PAGE_2] paper color for screen 2
SCRCF2 ;$246
.dsb 1
/// @brief [VALUE_PAGE_2] paper color for screen 3
SCRCF3 ;$247
.dsb 1
/// @brief [VALUE_PAGE_2]  flag for screen
/// b7 : display cursor
/// b6 : cursor does not blink
/// b5 : inverted video
/// b4 : 38/40 column mode ?
/// b3 : escape ?
/// b2 : US ?
/// b1 : double height
/// b0 : counter for us
FLGSCR ; $248
.dsb 1
.dsb 3 ; FIXME

CURSCR ;$24c ; char on the cursor
.dsb 4 ; FIXME

HARD_COPY_HIRES_VECTOR ; $250
.dsb 6 ; FIXME why 6 ? only 2 could be useful



SCRTXT ; $0256 desc scrtxt 6 bytes
.dsb 6 ; FIXME

SCRHIR ;$025C desc 6 bytes for HIres
.dsb 6 ; FIXME

SCRTRA ; $0262 desc 6 bytes for trace
.dsb 6 ; FIXME



/// @brief [VALUE_PAGE_2]  image of 8x8 keyboard matrix (8 bytes)
KBDCOL ; $268
.dsb 8



/// @brief [VALUE_PAGE_2]  0 if no key pressed
KBDFLG_KEY ; $270
.dsb 1

; FIXME KBDKEY here

; interrupt primitives

; Switch to keyboard, A containts value :
; 00 Qwerty
; 02 french
; 04 accent
; 01 azerty
; 03 bwana
; 05 accent off

; Page 0 variables

/********************************************************************** PAGE 0 VARIABLES */

/// @brief [VALUE_PAGE_2]  contains key pressed
KBD_UNKNOWN ; $271  FIXME
.dsb 1
/// @brief [VALUE_PAGE_2]  contains nb before repeat keyboard : it used to manage keyboard speed repetition
KBDVRR; $272 
.dsb 1
/// @brief [VALUE_PAGE_2]  Divide repetition keyboard : it used to manage keyboard speed repetition
KBDVRL; $273
.dsb 2 ; 2 bytes ?
FLGKBD ;$0275	;Keyboard flag : b7 majn b6 if sound
.dsb 1


/// @brief [VALUE_PAGE_2]  Manage function key

KBDFCT ;$0276 vector
.dsb 2

KBDSHT ;$278 ; Contains informations from key pressed
.dsb 1
KBDKEY ; $279 ; ASCII key FIXME
.dsb 5
KBDCTC ; $27E ; if ctrl+c is done if b7 equal to 1
.dsb 1
; FIXME
.dsb 7
LPRX ; $0286 ; word cursor in the line
.dsb 1
LPRY ; $0287
.dsb 1


LPRFX ; $0288 ; printer width
.dsb 1
LPRFY ; $0289 ; 
.dsb 1
FLGLPR ; $028a ;; word b7 ready
.dsb 1
.dsb 1 ; FIXME
/// @brief [VALUE_PAGE_2]  flag joystick
/// b6=1 if left joystick
/// b0=1 if  mouse
FLGJCK ; $028c
.dsb 1
/// @brief [VALUE_PAGE_2]  value of left joystick
JCGVAL ;$028d
.dsb 1
/// @brief [VALUE_PAGE_2]  value of right joystick
JCDVAL ;$028e
.dsb 1
VIA_UNKNOWN ; 028f seems tobe a backup of timer
.dsb 2
MOUSE_JOYSTICK_MANAGEMENT
.dsb 1 ; 291
.dsb 11 
/// @brief [VALUE_PAGE_2] value of joystick by default : $0b $0a $20 $08 $09 $03 $03 these value must be verifyed 
JCKTAB ;$029d FIXME could be wrong offset
.dsb 7
.dsb 2
KEYBOARD_COUNTER ;$02a6
.dsb 4 ; FIXME
HRSPAT ;$02aa
.dsb 1 
HRSERR ; $02ab
.dsb 1 
.dsb 2 ; FIXME
/// @brief [VALUE_PAGE_2]  Activating channel 0
IOTAB0 ; $02ae ; activating channel 0
.dsb 4
/// @brief [VALUE_PAGE_2]  Activating channel 1
IOTAB1 ; $02b2 ; activating channel 1
.dsb 4
/// @brief [VALUE_PAGE_2]  Activating channel 2
IOTAB2 ; $02b6 ; activating channel 2
.dsb 4
/// @brief [VALUE_PAGE_2]  Activating channel 3
IOTAB3 ; $02ba ; activating channel 3
.dsb 4

ADIOB ; $02be ; 48 bytes ? I/O management address
.dsb 48

FLGRST ; $02ee
.byt 0
CSRND ; $02EF ; current value of random generator
.dsb 5 ; FIXME

VNMI ; $02F4
.dsb 3 ; FIXME
ADIODB_VECTOR ; $2f7
.dsb 3
VIRQ ; $02fa ; Updated for atmos compatibility
.dsb 3

VAPLIC ;$2FD ; No banque adress
.dsb 3


/********************************************************************** PAGE 4 VARIABLES */

/// @brief [VALUE_PAGE_4] Store the old bank (if we call telemon from bank4, the id of the bank is stored here)
*=$40f
BNKOLD ; $40F 
.dsb 1
*=$414
VEXBNK ; $414
.dsb 3
BNKCIB ; $417
.dsb 1 ;?

*=$500
NUMBER_OPENED_FILES ; ALIAS but in stratsed it's $549
.dsb 1
TEMP_ORIX_2
.dsb 1
NUMBER_OF_COLUMNS
.dsb 1
GETOPT_PTR
.dsb 1 ; Store the index of the current opt
TEMP_ORIX_1
.dsb 1
ORIX_ARGC
.dsb 1
ORIX_GETOPT_PTR
.dsb 1
ORIX_PATH_CURRENT_POSITION
.dsb 1
ORIX_OFFSET_STRUCT_FILE_OPEN
ORIX_OFFSET_STRUCT_FILE_OPEN_LOW
.dsb ORIX_MAX_OPEN_FILES
ORIX_OFFSET_STRUCT_FILE_OPEN_HIGH
.dsb ORIX_MAX_OPEN_FILES
*=$517
BUFNOM ; $517
.dsb 13
ORIX_PATH_CURRENT
.dsb ORIX_MAX_PATH_LENGTH,0
ORIX_ARGV
.dsb 13

ORIX_FILE_OPEN_STRUCT
; OFFSET (16 bits)
.dsb 2*ORIX_MAX_OPEN_FILES
; flag (r or w)
;.dsb 1*ORIX_MAX_OPEN_FILES 
;.dsb (ORIX_MAX_PATH_LENGTH+12)*ORIX_MAX_OPEN_FILES 
ERRNO
  .dsb 1
BUFEDT_LENGTH
  .dsb 1

/// @brief [VALUE_PAGE_5]  edition buffer (length : 110 bytes)
*=$590
BUFEDT ; $590
.dsb MAX_LENGTH_BUFEDT
;BUFEDT_LENGTH
;.dsb 1 ; strlen of  BUFEDT_LENGTH
; send the errno

volatile_str
.dsb 50
TELEMON_end_variables
#print TELEMON_end_variables
