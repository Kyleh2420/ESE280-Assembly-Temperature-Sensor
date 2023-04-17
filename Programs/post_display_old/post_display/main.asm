;
; post_display.asm
;
; Created: 11/20/2021 1:27:11 PM
; Author : Kyle Han
;

.equ PERIOD = 100 //Looking for a 40Hz refresh rate

.dseg
bcd_entries: .byte 4
led_display: .byte 4
digit_num: .byte 1
//Mode controls which part of the display we're doing.
//Either 0 - main loop (Normal multiplexing) or any other number - POST setup (POSTING, number indictaes digit)
//Since I assume TCA0 will controll both loops
mode: .byte 1
//The above can be modified to only use digit_num - If the number is 0xFC or above,
//Then it should be posting. Once it rolls over to 0x00+, then it should be regularly multiplexing the display


.cseg

reset:
	rjmp start

.org TCA0_OVF_vect
	jmp display

start:
	//Set all of PORTD to outputs (For the 7seg)
	ldi r16, 0xFF
	sts PORTD_DIR, r16

	//Set PA7-PA4 as outputs (For the multiplexer)
	ldi r16, 0xF0
	sts PORTA_DIR, r16

//Sets up TCA0 timer for the interrupt. 40Hz
	ldi r16, TCA_SINGLE_WGMODE_NORMAL_gc	;WGMODE normal
	sts TCA0_SINGLE_CTRLB, r16

	ldi r16, TCA_SINGLE_OVF_bm		;enable overflow interrupt
	sts TCA0_SINGLE_INTCTRL, r16

	;load period low byte then high byte
	ldi r16, LOW(PERIOD)		;set the period
	sts TCA0_SINGLE_PER, r16
	ldi r16, HIGH(PERIOD)
	sts TCA0_SINGLE_PER + 1, r16

	;set clock and start timer
	ldi r16, TCA_SINGLE_CLKSEL_DIV256_gc | TCA_SINGLE_ENABLE_bm
	sts TCA0_SINGLE_CTRLA, r16

	sei

//Setup loop. Will stay here, posting the display until we're done. Then skips to main loop
//We will use mode as our decrement counter and our mode selector
//As a reminder, a 0 in mode means we operate normally, any other number indicates the digit we're in for the display
	ldi r16, 0x04
	sts mode, r16
setup_loop:
	lds r16, mode
	cpi r16, 0x00
	brne setup_loop

main_loop:
	nop
	rjmp main_loop



//This subroutine is intended to be used with both the display segment and the POSTING segment
//(That is, assuming that TCA0 will control the timing for both posting and multiplexing the display
//Therefore, we use the counter mode in dseg as a flag
display:
	push r16
	push r17
	push r18
	in r16, CPU_SREG
	push r16
	push XL
	push XH

	//handles and resets the interrupt and timer
	ldi r16, TCA_SINGLE_OVF_bm	;clear OVF flag
	sts TCA0_SINGLE_INTFLAGS, r16

	ldi r16, 0xFF
	sts PORTA_OUT, r16

	lds r16, mode
	cpi r16, 0x00
	breq multiplex_display
	rjmp post_display

//This subroutine will POST the display, called when TCA0 timer generates an interrupt
post_display:
	ldi r17, 0x00	//A value of 0x00 on the LED display will result in everything turning on
	sts PORTD_OUT, r17
	
	dec r16
	sts mode, r16
	cpi r16, 0x03 //First one (Display 1)
	breq dig0
	cpi r16, 0x02	//Digit 2
	breq dig1
	cpi r16, 0x01	//Digit 3
	breq dig2
	cpi r16, 0x00	//Digit 4
	breq dig3

return_from_display:
	pop XH
	pop XL
	pop r16
	out CPU_SREG, r16
	pop r18
	pop r17
	pop r16
	reti

dig0:
	ldi r17, 0x7F
	sts PORTA_OUT, r17
	rjmp return_from_display
dig1:
	ldi r17, 0xBF
	sts PORTA_OUT, r17
	rjmp return_from_display
dig2:
	ldi r17, 0xDF
	sts PORTA_OUT, r17
	rjmp return_from_display
dig3:
	ldi r17, 0xEF
	sts PORTA_OUT, r17
	rjmp return_from_display


multiplex_display:
	lds r17, digit_num
	inc r17
	//If we are at position 4, return to position 0
	cpi r17, 0x04
	brsh overflow

output:
	sts digit_num, r17
	ldi XH, HIGH(led_display)
	ldi XL, LOW(led_display)
	add XL, r17
	ld r18, X
	//rcall hex_to_7seg
	sts PORTD_OUT, r18

//Will check digit_num to decide which display on the 7seg is ebing outputted
checking_dig:
	cpi r17, 0x00
	breq dig0
	cpi r17, 0x01
	breq dig1
	cpi r17, 0x02
	breq dig2
	cpi r17, 0x03
	breq dig3

//Sets digit_num back to 0, since we only have 4 digits
overflow:
	ldi r17, 0x00
	sts digit_num, r17
	ldi XH, HIGH(led_display)
	ldi XL, LOW(led_display)
	rjmp output 
