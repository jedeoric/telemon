
#define BRK_TELEMON(value)\
	.byt 00,value;

	
	
#define PRINT_CHAR(str)\
	pha:\
	sta TR6:\
	txa:\
	pha:\
	tya:\
	pha:\
	lda TR6:\
	BRK_TELEMON(XWR0):\
  pla:\
	tay:\
	pla:\
	txa:\
  pla	


#define PRINT(str)\
	pha:\
	txa:\
	pha:\
	tya:\
	pha:\
	lda #<str:\
	ldy #>str:\
	BRK_TELEMON(XWSTR0):\
  pla:\
	tay:\
	pla:\
	txa:\
  pla		
	


#define RETURN_LINE_INTO_TELEMON\
	pha:\
	txa:\
	pha:\
	tya:\
	pha:\
	lda RES:\
	pha:\
	lda RES+1:\
	pha:\
	jsr XCRLF_ROUTINE : \
	pla:\
	sta RES+1:\
	pla:\
	sta RES:\
	pla:\
	tay:\
	pla:\
	txa:\
	pla
	
#define PRINT_INTO_TELEMON(str)\
	pha:\
	txa:\
	pha:\
	tya:\
	pha:\
	lda RES:\
	pha:\
	lda RES+1:\
	pha:\
	lda #<str:\
	ldy #>str:\
	jsr XWSTR0_ROUTINE : \
	pla:\
	sta RES+1:\
	pla:\
	sta RES:\
	pla:\
	tay:\
	pla:\
	txa:\
	pla
	
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
  