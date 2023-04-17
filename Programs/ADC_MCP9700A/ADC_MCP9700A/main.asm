;
; ADC_MCP9700A.asm
;
; Created: 11/30/2021 4:21:51 AM
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
	rjmp multiplex_display
.org ADC0_RESRDY_vect
	rjmp ADC0_response

start:

//This section deals with port configurations
	//Set all of PORTD to outputs (For the 7seg)
	ldi r16, 0xFF
	sts PORTD_DIR, r16

	//Set PA7-PA4 as outputs (For the multiplexer)
	ldi r16, 0xF0
	sts PORTA_DIR, r16

	//Configures all of PORTE as inputs
	ldi r16, 0x00
	sts PORTE_DIR, r16

//This section deals with timer TCA0
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


//This section deals with Analog-to-Digital-Converter ADC0
	//Set the controls as a 12 bit resolution, and enable the ADC
	ldi r16, ADC_RESSEL_12BIT_gc | ADC_ENABLE_bm
	sts ADC0_CTRLA, r16

	//Config the voltage ref to 2.5V
	ldi r16, VREF_REFSEL_2V500_gc
	sts VREF_ADC0REF, r16

	//Sets the prescaler to 64 bits
	ldi r16, ADC_PRESC_DIV64_gc
	sts ADC0_CTRLC, r16

	//Configs PE3 as the positive analog input to the ADC
	//AIN11 = PE3
	ldi r16, ADC_MUXPOS_AIN11_gc
	sts ADC0_MUXPOS, r16

	//Configures the interrupt for the ADC
	ldi r16, ADC_RESRDY_bm
	sts ADC0_INTCTRL, r16

	//POST the display. Once posted, then start the timer and continue as normal.
	rcall post_display

//Starting line
	//set clock and start timer
	ldi r16, TCA_SINGLE_CLKSEL_DIV256_gc | TCA_SINGLE_ENABLE_bm
	sts TCA0_SINGLE_CTRLA, r16

	//Start ADC0's conversion
	ldi r16, ADC_STCONV_bm
	sts ADC0_COMMAND, r16
	sei

loop:
	nop
	rjmp loop


;***************************************************************************
;* 
;* "ADC0_response" - Read the result of ADC0
;*
;* Description: Read in the value, and update bcd_entries/led_display accordingly
;* 
;* Author: Kyle Han
;* Version: 0.1
;* Last updated: 11/29/2021
;* Target: AVR128DB48
;* Number of words:
;* Number of cycles:
;* Low registers modified: r16-r18
;* High registers modified:
;*
;* Parameters: None
;*
;* Returns: None
;*
;* Notes: Requires digit_num, bcd_entries, and led_display to be declared in .dseg
;*
;***************************************************************************
ADC0_response:
	//Saving things to the stack
	push r16
	push r17
	push r18
	push r19

	//Past this point, we know that a conversion is ready for us to read in.
	lds r19, ADC0_RESL
	lds r17, ADC0_RESH

//Will copy everything needed into bcd_entries, and create the corresponding values in led_display
	//Entry 0
	mov r18, r17
	andi r18, 0xF0
	//We need a shift right to get this back to bcd (Since that only requires 4 bits)
	lsr r18
	lsr r18	
	lsr r18	
	lsr r18
	sts bcd_entries, r18
	rcall hex_to_7seg
	sts led_display, r18

	//Entry 1
	mov r18, r17
	andi r18, 0x0F
	sts bcd_entries+1, r18
	rcall hex_to_7seg
	sts led_display+1, r18

	//Entry 2
	mov r18, r19
	andi r18, 0xF0
	lsr r18
	lsr r18	
	lsr r18	
	lsr r18
	sts bcd_entries+2, r18
	rcall hex_to_7seg
	sts led_display+2, r18

	//Entry 3
	mov r18, r19
	andi r18, 0x0F
	sts bcd_entries+3, r18
	rcall hex_to_7seg
	sts led_display+3, r18

//Returns from the ADC subroutine. Saves things and make sure the registers are left unmodified.
retADC:
	pop r19
	pop r18
	pop r17
	pop r16

	//Restart ADC0's conversion
	ldi r16, ADC_STCONV_bm
	sts ADC0_COMMAND, r16
	reti

;***************************************************************************
;* 
;* "hex_to_7seg" - Hexadecimal to Seven Segment Conversion
;*
;* Description: Converts a right justified hexadecimal digit to the seven
;* segment pattern required to display it. Pattern is right justified a
;* through g. Pattern uses 0s to turn segments on ON.
;*
;* Author:			Ken Short
;* Version:			0.1						
;* Last updated:		101221
;* Target:			AVR128DB48
;* Number of words:
;* Number of cycles:
;* Low registers modified:
;* High registers modified:
;*
;* Parameters: r18: hex digit to be converted
;* Returns: r18: seven segment pattern. 0 turns segment ON
;*
;* Notes: 
;*
;***************************************************************************
hex_to_7seg:
	push ZH
	push ZL
    ldi ZH, HIGH(hextable * 2)  ;set Z to point to start of table
    ldi ZL, LOW(hextable * 2)
    ldi r16, $00                ;add offset to Z pointer
	andi r18, 0x0F				;mask for low nibble
    add ZL, r18
    adc ZH, r16
    lpm r18, Z                  ;load byte from table pointed to by Z
	pop ZL
	pop ZH
	ret

    ;Table of segment values to display digits 0 - F
    ;!!! seven values must be added
hextable: .db $01, $4F, $12, $06, $4C, $24, $20, $0F, $00, $04, $08, $60, $31, $42, $30, $38


;***************************************************************************
;* 
;* Multiplex_display
;*
;* Description: reads everything in the memory "array" and converts it to a 7seg interpetatoin.
;* 
;* 
;*
;* Author:			Kyle Han
;* Version:			0.1						
;* Last updated:		11032021
;* Target:			AVR128DB48
;* Number of words:
;* Number of cycles:
;* Low registers modified:
;* High registers modified:
;*
;* Parameters: A pointer called array, and pointer X available
;* Returns: Everything inside pointer X
;*
;* Notes: 
;*
;***************************************************************************


multiplex_display:

//Turns off the whole display by outputting 1s to PORTA. Then, check with digit_num to see which display should be lit
turn_off:

	//First, we must push all the registers we are using to the stack. This is so that the original values are restored later on
	push r16
	push r17
	push r18
	in r16, CPU_SREG
	push r16
	push XL
	push XH

	//Handles the interrupt and resets the timer
	ldi r16, TCA_SINGLE_OVF_bm	;clear OVF flag
	sts TCA0_SINGLE_INTFLAGS, r16

	ldi r16, 0xFF
	sts PORTA_OUT, r16
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

	//r18 stores the value of the digit to be displayed
dig0:
	ldi r18, 0x70	//PA7
	sts PORTA_OUT, r18	
	rjmp restore

dig1:
	ldi r18, 0xB0	//PA6
	sts PORTA_OUT, r18
	rjmp restore

dig2:
	ldi r18, 0xD0	//PA5
	sts PORTA_OUT, r18
	rjmp restore

dig3:
	ldi r18, 0xE0 //PA4
	sts PORTA_OUT, r18	
	rjmp restore

//Reads off the stack the values that we preserved inside turn_off
restore:
	pop XH
	pop XL
	pop r16
	out CPU_SREG, r16
	pop r18
	pop r17
	pop r16

	reti
	
//Sets digit_num back to 0, since we only have 4 digits
overflow:
	ldi r17, 0x00
	sts digit_num, r17
	ldi XH, HIGH(led_display)
	ldi XL, LOW(led_display)
	rjmp output 


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
	ldi r20, 10
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
	ldi r17, 133
inner_loop:
	dec r17
	brne inner_loop
	dec r20
	brne outer_loop
	ret