#include "src/include/telemon.h"
#include "src/include/macro.h"

        *=$1000-20
; include header
        .byt $01,$00		; non-C64 marker like o65 format
        .byt "o", "r", "i"      ; "ori" MAGIC number :$6f, $36, $35 like o65 format
        .byt $01                ; version of this header
cpu_mode
        .byt $00                ; CPU see below for description
language_type
        .byt $00	        ; reserved in the future, it will define if it's a Hyperbasic file, teleass file, forth file 
        .byt $00                ; reserved
        .byt $00		; reserved
        .byt $00	        ; operating system id for telemon $00 means telemon 3.0 version
        .byt $00	        ; reserved
        .byt $00                ; reserved
type_of_file
        .byt %01001001                   ; Auto, direct, data for stratsed, sedoric, ftdos compatibility
        .byt <start_adress,>start_adress ; loading adress
        .byt <EndOfMemory,>EndOfMemory   ; end of loading adress
        .byt <start_adress,>start_adress ; starting adress
 
start_adress
 
    *=$1000
      
    lda   #<str
    ldy   #>str
    BRK_TELEMON(XWSTR0)  

    lda   #<file
    ldy   #>file
    BRK_TELEMON(XWSTR0)      
    
    lda   #<file
    ldx   #>file
    BRK_TELEMON(XRM)
    rts
file
    .asc "/tests/xrm",0  
str  
    .asc "Removing myself :) ",0  
EndOfMemory

