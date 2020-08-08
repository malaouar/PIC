
* JDM programmer:
  - A simple JDM clone, built many times and works as a charm.
  - If the power supply is garanteed between 12 and 13.5V we can omit the 100 Ohm resistor and 12V zener.
  
* Mini PICKIT2 programmer:
  - A simple version of the famous PK2 but only for 5V chips and no programmer-to-go (PTG) feature.
  - We can use it to program AVRs with avrdude program.
  - It can be used as a logic tool, including 3 channel logic analyzer and Three freely programmable ports. 
  - or as a  UART tool (Tx and RX only).
  Hardware:
  - We can use any low power transistors.
  - Respect the value of resistors R4 and R5,  4.7k and 2.7k respictively.
  - LEDs and their associate resistors are optional.
  - L:  any value between 220uH and 1mH.
  - We can use any crystal with frequency dividable by 4 (4, 8, 12, 16, 20).
    just dont forget to change the PLLDIV (divide by 3 when using a 12Mhz crystal for example) 
	but when  a 20Mhz crystal is  used no need to changed anything on the PICkit2 firmware.
  
  http://www.elproducts.com/build-your-own-pickit-2.html
  
  