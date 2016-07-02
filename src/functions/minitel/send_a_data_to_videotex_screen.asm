XVDTDA_ROUTINE
send_a_data_to_videotex_screen
XVDTAH_ROUTINE
Ld4f1
vdt_to_hires
Ld530	
	; REMOVEME minitel
	rts
	



; MINITEL
stop_videotex_emulation
	jsr switch_text
	jmp switch_off_cursor_videotex 

	

	
	