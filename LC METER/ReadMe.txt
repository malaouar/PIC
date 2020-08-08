
 In this version of the very famous accuarate, PIC16F84 based, LC meter we use a terminal program (putty, hyperterminal, cutecom or minicom) as a display instead of the 2x16 LCD !!
  - config: 9600 8N1  
  - TX = PORTB4 (we can use any other  PIN)
 You may need an inverter after the TX output. Use a simple transistor inverter to connect it to RX of a USB to COM cable or a true COM port.

 Other newty is the use of an NPN transistor instead of the reed relay (difficult to find in 3rd world countries like mmine!!).


Note:
ASSEMBLE WITH CASE SENSITIVE OPTION ! In MBASM select "case sensitive"