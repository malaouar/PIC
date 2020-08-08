;  Decode NEC commands with 16F84A
;  Command Relays on PORTB and/or PORTA
;  4MHz crystal
; ************************************************
    LIST         p=16F84A                   
    #INCLUDE <P16F84A.inc>                  
    __config _CP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC

;------------------------------------------------------------------
	IR	EQU	 0		; IR INPUT on RB0

	; vars:
	BYTECNT EQU 0x0C	 	; BYTE COUNTER
	BITCNT	EQU 0x0D		; BIT COUNTER
	BYTE	EQU 0x0E		; STORE FOR RECEIVED BYTES

	IRBASE	EQU 0x0F		; BASE OF IR TABLE
	CBLOCK 0x0F             ; Declare variable addresses in RAM starting at 0x0F 
	address_L, address_H, command, Ncommand
	ENDC 

;----------------------------------
; Remote commands  ( TV)

	command1    EQU    0x5A   ; Arrow Up
	command2    EQU    0x5B   ; Arrow Down
	command3    EQU    0x5D   ; Arrow Left
	command4    EQU    0x5E   ; Arrow right

;***********************************
	ORG     0000		; RESET VECTOR
	GOTO INIT

;-----------------------------------	
	org H'0004'    		; vecteur d'interruption 
	MOVLW	IRBASE		; INITIALISE IR TABLE ADDRESS
	MOVWF	FSR
	
	MOVLW	4			; START COLLECTING DATA ( 4 BYTES=32Bits NEC)
	MOVWF	BYTECNT  
	CLRF BYTE   		; CLEAR received byte
  
; is it Start Bit ?
WHigh	; wait for High level
	BTFSS	PORTB,IR	; 
	GOTO 	WHigh
	CLRF	TMR0  		; CLEAR Timer0

WLow	; wait for Low level
	BTFSC	PORTB,IR	; 
	GOTO 	WLow
	MOVLW	D'67'		; 67 x 64 = 4288 us = 4,29ms 
	SUBWF	TMR0,w		; W= (TMR0) - (W): if (TMR0) > or =  (W)   then C=1 
	BTFSS	STATUS,C	;  if C=1 (> or = 4,3ms ) then Skip
	GOTO FIN  			; Else Not NEC then return
	MOVLW	D'72'		; 72 x 64 = 4606 us = 4,6ms 
	SUBWF	TMR0,w		; W= (TMR0) - (W): if (TMR0) > or =  (W)   then C=1 
	BTFSC	STATUS,C	; if C=0 (< 4,6ms ) OK (NEC pulse) then Skip
	GOTO FIN  			; Else Not NEC then return
  
NEXTBYTE	
	MOVLW	8			; 8 BITS PER BYTE 
	MOVWF	BITCNT

NEXTBIT
WHigh_B	; wait for High level
	BTFSS	PORTB,IR	; 
	GOTO 	WHigh_B
	CLRF	TMR0  		; CLEAR Timer0

WLow_B	; wait for Low level
	BTFSC	PORTB,IR	; 
	GOTO 	WLow_B
	
	MOVLW	D'24'		  ; 24 x 64 = 1536 us = 1,5ms 
	SUBWF	TMR0,w		; W= (TMR0) - (W): if (TMR0) > or =  (W)   then C=1 
	BTFSS	STATUS,C	;  if C=1 (> or = 1,5ms ) then bit is 1
	GOTO ZERO       	; Else (C=0) this bit is 0
	MOVLW	D'27'		  ; 27 x 64 = 1728 us = 1,7ms 
	SUBWF	TMR0,w		; W= (TMR0) - (W): if (TMR0) > or =  (W)   then C=1 
	BTFSC	STATUS,C	;  if C=0 (<1,7ms ) OK (bit=1) then Skip
	GOTO FIN        	; Else Not NEC then return
	;if bit is 1
	bsf status, c
   
ZERO    ; else we have already C=0 
   RRF BYTE       		; STORE bit, Rotate Right through Carry (cause LSB first) 
  
	DECFSZ	BITCNT		; Last bit? DECREMENT BIT COUNT and skip if zero
	GOTO	NEXTBIT		;  No, then get Next Bit
	MOVF	BYTE, W   	; Yes, STORE Byte IN TABLE
	MOVWF	INDF		  ; 

	INCF	FSR		    ; point to NEXT BYTE IN TABLE
	DECFSZ	BYTECNT		; Last Byte?	
	GOTO	NEXTBYTE	; NO, THEREFORE GET NEXT BYTE

; ********* Yes, DATA COLLECTION FINISHED  ****************
	; Which command?
;TEST_C1
	movlw command1   	 ; is it commande1?
	subwf	command, W	 ; compare with received command (if C=1 AND Z=1 then equal)
	BTFSS	STATUS,C	 ;  if C=1 test Z bit
	GOTO TEST_C2     	 ; else is it commande2?
	BTFSC	STATUS,Z	 ;  if Z=0 skip 
	GOTO C1_ACT     	 ; else the received command = command1
TEST_C2
	movlw command2   	 ; is it commande2?
	subwf	command, W	 ; compare with received command
	BTFSS	STATUS,C	 ;  if C=1 test Z bit
	GOTO TEST_C3      	 ; else is it commande3?
	BTFSC	STATUS,Z	 ;  if Z=0 skip 
	GOTO C2_ACT      	 ; else the received command = command2
TEST_C3
	movlw command3   	 ; is it commande3?
	subwf	command, W	 ; compare with received command
	BTFSS	STATUS,C	 ;  if C=1 test Z bit
	GOTO TEST_C4      	 ; else is it commande3?
	BTFSC	STATUS,Z	 ;  if Z=0 skip 
	GOTO C3_ACT      	 ; else the received command = command3
;TEST_C4  ; add same thing  for other commands
	GOTO FIN         	 ; if No one of our predefined commands then quit
  
C1_ACT    ; Action to do when command 1 is received
	BTFSS PORTB, 1    ; Read the state of PIN B1
	GOTO SET1         ; If is clear then set it
	BCF PORTB, 1      ; else clear it
	GOTO FIN
SET1
	BSF PORTB, 1      
	GOTO FIN
  
C2_ACT    ; Action to do when command 2 is received
  BTFSS PORTB, 2    ; Read the state of PIN B2
  GOTO SET2         ; If is clear then set it
  BCF PORTB, 2      ; else clear it
  GOTO FIN
SET2
  BSF PORTB, 2      
  GOTO FIN
   
C3_ACT    ; Action to do when command 3 is received
  BTFSS PORTB, 3    ; Read the state of PIN B3
  GOTO SET3         ; If is clear then set it
  BCF PORTB, 3      ; else clear it
  GOTO FIN
SET3
  BSF PORTB, 3      
	
FIN
   bcf INTCON, INTF  
   retfie 				
;===============================
INIT
	BSF	STATUS, RP0	; SELECT BANK 1

	movlw   b'00000001'     ;
	movwf   TRISB           ; RB7 - 1 outputs for relays, RB0 input for IR receiver
	
  ; Timer0 Frequency = FCYC / 64 = 1MHz / 64 = 15.625KHz  (T=64uS)
	MOVLW 	B'10000101'		;  disable pull-ups, INT0 on falling edge, COUNTER SOURCE=CLCK, PRESCALER=64 
	MOVWF	OPTION_REG			
	
	BCF	STATUS, RP0			; SELECT BANK 0 
	
	movlw 0xFF
	movwf   PORTB       	; all relays off
  
	movlw B'10010000'   	; Enable global interupts, Enable the INT0 intFINupt
	movwf INTCON

; wait forever for incoming command
WAIT
	SLEEP
	nop
	nop
	GOTO WAIT   ; after Interrupt return to sleep

; ****************
  END