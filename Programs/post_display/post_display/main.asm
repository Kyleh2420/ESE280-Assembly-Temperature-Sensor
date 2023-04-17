;
; post_display.asm
;
; Created: 11/30/2021 1:55:34 AM
; Author : Kyle Han
;


.equ PERIOD = 97 //((4MHz * (1/160))/256)-1


.dseg
bcd_entries: .byte 4
led_display: .byte 4
digit_num: .byte 1


.cseg
reset:
	rjmp start
.org TCA0_OVF_vect
	rjmp display


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

	//POST the display. Once posted, then start the timer and continue as normal.
	rcall post_display

	//set clock and start timer
	ldi r16, TCA_SINGLE_CLKSEL_DIV256_gc | TCA_SINGLE_ENABLE_bm
	sts TCA0_SINGLE_CTRLA, r16

	sei

loop:
	nop
	rjmp loop

;***************************************************************************
;* 
;* "post_display" - Power On Self Test
;*
;* Description: Will POST the 7segment display hooked up to port D, multiplexed by PA7-PA4.
;*Individually turns on each segment for a brief moment, totalling 1 second
;* Author: Kyle Han
;* Version: 0.1
;* Last updated: 11/29/2021
;* Target: AVR128DB48
;* Number of words:
;* Number of cycles:
;* Low registers modified: r16-r20
;* High registers modified:
;*
;* Parameters: None
;*
;* Returns: None
;*
;* Notes: Requires digit_num to be declared in .dseg
;*
;***************************************************************************
post_display:
	//A value of 0 turns on our LED displays
	ldi r16, 0x00
	out VPORTD_OUT, r16
	sts digit_num, r16 ;Store 0x00 to the current digit to be displayed
repeatPost:
	in r17, VPORTA_OUT
	ori r17, 0xF0
	out VPORTA_OUT, r17
	ldi r19, 0b00010000 ;This 1 will turn on a specific digit.
leftShiftPOST:
	cpi r16, 0
	breq postDispOn
	lsl r19
	dec r16
	rjmp leftShiftPOST
postDispOn:
	in r17, VPORTA_OUT
	eor r17, r19
	out VPORTA_OUT, r17
	ldi r20, 255
	rcall oneSecDelay
	//Compares digit_num to 0x04. If greater than or equal to, we've finished posting
	lds r16, digit_num
	inc r16
	sts digit_num, r16
	cpi r16, 0x04
	brlo repeatPost

	in r17, VPORTA_OUT
	ori r17, 0xF0
	out VPORTA_OUT, r17
	ret

;***************************************************************************
;* 
;* "oneSecDelay" - Delay the microcontroller by 1 second by occupying CPU time
;*
;* Description: Loops through and occupies CPU time to delay by 1 second
;*
;* Author: Kyle Han
;* Version: 1.0
;* Last updated: 11/29/2021
;* Target: AVR128DB48
;* Number of words:
;* Number of cycles:
;* Low registers modified:r17
;* High registers modified:
;*
;* Parameters: r20 * .1ms
;*
;* Returns: Nothing
;*
;* Notes: 
;*
;***************************************************************************
oneSecDelay:
outer_loop:
	ldi r17, 255
inner_loop:
	dec r17
	brne inner_loop
	dec r20
	brne outer_loop
	ret

display:
	reti