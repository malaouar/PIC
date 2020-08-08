

The DiSEqC ( DIgital Satellite EQuipment Control) is a communication protocol between satellite receivers and  peripheral equipments (switchs, dish motors ...), using only the existing coaxial cable.
The DiSEqC concept is based on extending the present 22 kHz tone signalling method. Si it's backwards compatible with 13/18 volt and 22 kHz tone switching.

DiSEqC uses base-band timings of 500µs (±100µs) for a one-third bit PWK (Pulse Width Keying) coded signal period on a nominal 22 kHz (±20 %) carrier. The end of each DiSEqCä message is signalled by a minimum of 6 ms of silence.

The nominal 22 kHz signalling amplitude is 650 mV (±250 mV) peak-peak. The maximum recommended amplitude to be applied to the bus is 1 volt peak to peak.
the 22 kHz time envelope for each bit transmitted, with nominally 22 cycles for a Bit ‘0’ and 11 cycles for a Bit ‘1’.


A program to decode a diseqc message and display the 4 bytes
LCD data on PORT B, RB0-RB3
LCD data on PORT A,RA2= E, RA3= RS
Diseqc input = RA4