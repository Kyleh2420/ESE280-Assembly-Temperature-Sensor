# ESE280-Assembly-Temperature-Sensor
A project for Stony Brook University's ESE 280: Embedded Microcontroller Systems which will read temperature and report it to a 7 segment display in fahrenheit, celsius, and hex.

This project was done in Fall of 2021 and utilized an AVR128DB48 as the target microcontroller. Additional equipment includes 4 ZTX-555 transistors, a MCP9700A temperature sensor, and a 4 digit seven segment display. 

Everything is programmed in assembly. There are several .asm programs listed here: the main one is temp_meas.asm, which utilizes the other files as subroutines that will get called.
