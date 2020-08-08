
As we know, PIC16F84A microcontroller doesn't have built in UART module, so we can create UART functionality in software (bit-banging).
In these examples software UART is  configured for 9600 baud 8N1.
UART baudrate is currently set to 9600 bps, but you can change it to your desired value. 
In "RX-TX.asm" RB1 pin is being used as TX pin and RB0 pin is the RX pin of the software UART. External interrupt on pin RB0 is enabled in the code. Whenever a high to low transition is received on RB0 pin (i-e starting bit for UART) then PIC16F84A goes into interrupt service routine and received character is echoed back. 



