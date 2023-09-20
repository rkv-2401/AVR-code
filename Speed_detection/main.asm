;
; Task.asm
;
; Created: 08-11-2022 17:55:37
; Author : rkvig
;

;PINS (final) - OpE to + 5V
;OpO to RDX4 (PORT D bit 0) - INT0
;MOT to POT
;and the same ports for display as previous task

.include "m2560def.inc"
; workspace - r16 - leave it free
.def interrupt_count = r18		;
.def rev_count = r19			;
.def temp = r20					;
.def temp1 = r21				;
.def temp2 = r22				;

.def byte_count_l = r24			; needed for 1ms loop
.def byte_count_h = r25			;


.cseg							; start of code segment in AVR assembler
;.org 0x0000					; another way of doing it - neither is necessary
	jmp RESET					;	
.org INT0addr					; highest priority interrupt
	jmp EXT_INT0				; jump to external macro
.org OVF0addr					; timer0 overflow interrupt has been triggered
	jmp TIMER0OVF				; macro for handling that situation 
	;jmp wait					; 

;useful macros
.macro do_lcd_command           ; transfer command to LCD
	ldi r16, @0                 ; load data @0 to r16
	rcall lcd_command           ; rcall lcd_command
	rcall lcd_wait              ; rcall lcd_wait
.endmacro

.macro do_lcd_data				; transfer data to LCD
	mov r16, @0					; move data @0 to r16
	rcall lcd_data				; rcall lcd_data
	rcall lcd_wait				; rcall lcd_wait
.endmacro

.macro do_lcd_data_debug		; we can use either one
	ldi r16, @0					; 
	rcall lcd_data				;
	rcall lcd_wait				;
.endmacro

.macro lcd_data_from_reg
	mov	r16, @0					;
	subi r16, -48				;48 - ASCII character for 0, LCD needs ASCII value to display		
	rcall lcd_data				;
	rcall lcd_wait				;
.endmacro

;this resets the LCD - call it within the RESET function
.macro LCD_RESET
	ser r16
	out DDRF, r16
	out DDRA, r16
	clr r16
	out PORTF, r16
	out PORTA, r16

	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink
	do_lcd_data_debug '$'	  ;
.endmacro

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

; useful macros from the sample code for the display 
.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro

; copied from LCD command macros
lcd_command:			; Send a command to the LCD (r26)
	out PORTF, r16
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	ret

lcd_data:
	out PORTF, r16
	lcd_set LCD_RS
	nop
	nop
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	lcd_clr LCD_RS
	ret

lcd_wait:
		push r16
		clr r16
		out DDRF, r16
		out PORTF, r16
		lcd_set LCD_RW

lcd_wait_loop:
		nop
		lcd_set LCD_E
		nop
		nop
			nop
		in r16, PINF
		lcd_clr LCD_E
		sbrc r16, 7
		rjmp lcd_wait_loop
		lcd_clr LCD_RW
		ser r16
		out DDRF, r16
		pop r16
		ret

; useful definitions from previous task
.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4 ;
; 4 cycles per iteration

sleep_1ms:
		push r24				;
		push r25				;
		ldi r25, high(DELAY_1MS);
		ldi r24, low(DELAY_1MS)	;

delay_1ms_macro:
		sbiw r25:r24, 1		; subtract 1 from the immediate word
		brne delay_1ms_macro; branch back to it if 2 things are not equal
		pop r25				;
		pop r24				;
		ret

sleep_5ms:
		rcall sleep_1ms
		rcall sleep_1ms
		rcall sleep_1ms
		rcall sleep_1ms
		rcall sleep_1ms
		ret

wait:
	rjmp wait					; loops forever, just waiting for interrupts (story of my life)

RESET:
	clr interrupt_count			; if interrupt count reaches 4 times - one rps
	clr rev_count				;
	;initalizing stack pointer
	ldi r16, low(RAMEND)
	out SPL, r16
	ldi r16, high(RAMEND)
	out SPH, r16
	LCD_RESET					;
	; stack pointer initialized, LCD cleared, basic LCD commands done
	; left shift 2 by val(ISC00) times - i.e. 1, 0 : raises interrupt on falling edge
	ldi temp, (2 << ISC00)		; set INT0 as falling edge interrupt - https://exploreembedded.com/wiki/AVR_External_Interrupts
	sts EICRA, temp				; stores value of temp in EICRA (external interrupt control register for INT3:0)
	in temp, EIMSK				; value of EIMSK (Ext. Int Mask Register) is stored in temp to left shift
	ori temp, (1 << INT0)		; enabling INT0 interrupt
	; above code - OR between temp and (1 left shifted INT0 (interrupt) times)
	out EIMSK, temp				; writing left-shifted value back to EIMSK
	
	;setting control bits
	ldi temp, 0b00000000		;
	out TCCR0A, temp			; write 0s to timer 0 control register A
	;ldi temp, 0b00000011		; 011 - prescaling value of 64 - (256 * 64)/16 ~= 1024Us = 1ms - e.g.1 lecture notes 
	ldi temp, 0b00000101		; 1024 = 16ms
	out TCCR0B, temp			; write 011 to ctrl reg. B
	ldi temp, 1 << TOIE0		; (TOIE0 - bit 0 of TIMSK)
	sts TIMSK0, temp			; enabling the interrupt for T/C0
	sei							; enabled global interrupt
	jmp wait					;

EXT_INT0:
	push temp2					; save register
	in temp2, SREG				;
	push temp2					;

	inc interrupt_count			;
	cpi interrupt_count, 4		; 
	brne not_eq					;
	inc rev_count				; every 4 interrupts - 1 revolution
	clr interrupt_count			; clear the interrupts after adding the rev
	not_eq:
		pop temp2				;
		out SREG, temp2			;
		pop temp2				;
		reti					; return from interrupt

;display code
.macro ones_scale
	   mov r17, @0					;
	   cpi r17, 10					;
	   brsh bigger_scale 			;if r17 >= 10
	   brlo same_scale				;brlo - less than 10
	   bigger_scale:
			tens_scale
	   same_scale:
			lcd_data_from_reg r17		;if lower or equal
.endmacro
.macro tens_scale
		;mov r16, @0				;
		cpi r17, 100				;90 is the max. value of the tens scale 
		brsh bigger_scale 			;r17 is directly acted upon
		brlo same_scale				;
		bigger_scale:
			hundreds_scale			;
		same_scale:
			ldi temp1, 0
			loop:
				cpi r17, 10				;
				brlo scale_done			;
				subi r17, 10			;watch for underflow
				inc temp1			;
				cpi r17, 10				;
				brsh loop				;does this brlo check
				brlo scale_done			;brlo - less than 10
				scale_done:
					lcd_data_from_reg temp1	
.endmacro
.macro hundreds_scale
		;mov r17, @0				;r23
		cpi r17, 255				;200 is the max. accepted value of the hundreds scale, but it goes out of range at 255
		breq normal					;
		brsh out_of_range			;if it's not equal but brsh, r17 > 255, jump here
		brlo normal					;
		out_of_range:
			rjmp wait				;
		normal:
			ldi temp1, 0			;reset it here
			loop:
				cpi r17, 100			;
				brlo scale_done		;
				subi r17, 100			;
				inc temp1			    ;
				cpi r17, 100			;
				brsh loop				  ;
				brlo scale_done		;
				scale_done:
					lcd_data_from_reg temp1	
.endmacro

TIMER0OVF:						; interrupt subroutine for Timer0
;byte_count_L = r24, byte_count_H = r25;
	adiw r25:r24, 1				; Increase the temporary counter by one - 1ms 
	cpi r24, low(64)			; Check (r25:r24) = 1000 - 1000ms = 1 second
	brne return_intp			; 
	cpi r25, high(64)			; everytime this timer overflows, it's one second anyway
	brne return_intp
	do_lcd_command 0b00000001	; clear and return to first place in the first line
	; calculate tens, hundreds and zeros scale to display onto the LCD
	; rev_count - final revolutions count at a RPS. Passing to one's scale should get it to display
	ones_scale rev_count
	clr rev_count				  ; once that iteration is displayed, clear everything
	clr r17						    ; this thing needs to update every second or so
	clr byte_count_H			;
	clr byte_count_L			;
	return_intp:
		reti					; and return back to the waiting loop

;jump here when (if?) the program ends
halt:
	rjmp halt		
