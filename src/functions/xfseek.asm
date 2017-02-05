; [IN] X whence
; [IN] AY position 
; [IN] RES fd

/*
#define SEEK_CUR        0 
#define SEEK_END        1
#define SEEK_SET        2  	Beginning of file
*/

.(
  cpx #SEEK_CUR
  beq move
  rts
move  
  jsr _ch376_seek_file
  rts
.)

