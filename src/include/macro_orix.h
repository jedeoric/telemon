
#define BRK_TELEMON(value)\
	.byt 00,value;
	
#define PRINT(str)\
	lda #<str:\
	ldy #>str:\
	BRK_TELEMON(XWSTR0) 

#define STRCPY(str1,str2)\
	lda #<str1:\
	sta RES:\
	lda #>str1:\
	sta RES+1:\
	lda #<str2:\
	sta RESB:\
	lda #>str2:\
	sta RESB+1:\
	jsr _strcpy

	
// This macro copy AY address to str
#define  STRCPY_BY_AY_SRC(str)\
	sta RES:\
	sty RES+1:\
	lda #<str:\
	sta RESB:\
	lda #>str:\
	sta RESB+1:\
	jsr _strcpy
	

	

	
	