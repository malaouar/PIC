; 	Display NEC IR protocol commands
;  LCD on  PORTB
;  4MHz crystal
; ************************************************
    LIST         p=16F84A                   
    #INCLUDE <P16F84A.inc>                  
    __config _CP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC

;------------------------------------------------------------------
	IR	EQU	 0				; IR INPUT on RB0

; vars:
	BYTECNT EQU 0x0C	 	; BYTE COUNTER
	BITCNT	EQU 0x0D		; BIT COUNTER
	BYTE	EQU 0x0E		; STORE FOR RECEIVED BYTES

	IRBASE	EQU 0x0F		; BASE OF IR TABLE
	CBLOCK 0x0F             ; Declare variable addresses in RAM starting at 0x0F 
    address_L, address_H, command, Ncommand, tmp, r1, r2, r3, r4, r5
	ENDC 

;----------------------------------
; LCD Module commands

	FUNCTION_SET EQU  0x028   ; 4 bits, 2 lines, 5x7 Font 
	DISP_ON      EQU  0x00C   ; Display on
	CLR_DISP     EQU  0x001   ; Clear the Display
	ENTRY_INC    EQU  0x006   ; Address increment, no scrolling


	LCD_PORT     EQU  PORTB   ;  data on  RB4-RB7, RB3= E, RB2= RS

;
; LCD Display Commands and Control Signal names.
;
	E  EQU     3      ; LCD Enable control line on RB3
	RS EQU     2      ; LCD Register Select control line on RB2
;
;***********************************
	ORG     0000		; RESET VECTOR
	GOTO INIT

;-----------------------------------	
	 org H'0004'    	; interrupt vector
	; Microchips sequence
    ;movwf W_TEMP   
    ;swapf STATUS, W
    ;movwf STATUS_TEMP
    ;======================
	MOVLW	IRBASE		; INITIALISE IR TABLE ADDRESS
	MOVWF	FSR
	
	MOVLW	4			; START COLLECTING DATA ( 4 BYTES=32Bits NEC)
	MOVWF	BYTECNT  
	CLRF BYTE  			 ; CLEAR received byte
  
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
	GOTO ERR  			; Else Not NEC then return
	MOVLW	D'72'		; 72 x 64 = 4606 us = 4,6ms 
	SUBWF	TMR0,w		; W= (TMR0) - (W): if (TMR0) > or =  (W)   then C=1 
	BTFSC	STATUS,C	;  if C=0 (< 4,6ms ) OK (NEC pulse) then Skip
	GOTO ERR  			; Else Not NEC then return
  
  
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
	
	MOVLW	D'24'		 ; 24 x 64 = 1536 us = 1,5ms 
	SUBWF	TMR0,w		; W= (TMR0) - (W): if (TMR0) > or =  (W)   then C=1 
	BTFSS	STATUS,C	;  if C=1 (> or = 1,5ms ) then bit is 1
	GOTO ZERO       	; Else (C=0) this bit is 0
	MOVLW	D'27'		 ; 27 x 64 = 1728 us = 1,7ms 
	SUBWF	TMR0,w		; W= (TMR0) - (W): if (TMR0) > or =  (W)   then C=1 
	BTFSC	STATUS,C	;  if C=0 (<1,7ms ) OK (bit=1) then Skip
	GOTO ERR        	; Else Not NEC then return
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
	movlw   CLR_DISP        ; 0x01: Clear LCD 
	call    Send_Cmd
	
	movlw   'N'
	call    Send_Char
	movlw   'E'
	call    Send_Char
	movlw   'C'
	call    Send_Char

	movlw   0xC0       	 	; line2
	call    Send_Cmd
	MOVF	address_H, W	; GET address High BYTE
	call DISPLAY
	
	MOVF	address_L, W	; GET address Low BYTE
	call DISPLAY

	movlw   ' '
	call    Send_Char

	MOVF	command, W		; GET command BYTE
	call DISPLAY

	COMF	Ncommand, W		; GET NOT_command BYTE
	call DISPLAY
	goto FIN
	
ERR
;error 
	movlw   CLR_DISP        ; 0x01: Clear LCD 
	call    Send_Cmd

	movlw   'E'
	call    Send_Char
	movlw   'R'
	call    Send_Char
	movlw   'R'
	call    Send_Char
FIN
	bcf INTCON, INTF  ; remettre le drapeau INTF à 0
    ;========================
    ; restauration du registre STATUS puis du registre W :
    ;swapf STATUS_TEMP, W
    ;movwf STATUS
    ;swapf W_TEMP, f
    ;swapf W_TEMP, W
    retfie ; retour d'interruption 
;===============================
INIT
	BSF	STATUS, RP0			; SELECT BANK 1

	movlw   b'00000011'     ;
	movwf   TRISB           ; RB7 - 2 outputs, RB1 - 0 inputs
	
  ; Timer0 Frequency = FCYC / 64 = 1MHz / 64 = 15.625KHz  (T=64uS)
	MOVLW 	B'10000101'		; disable pull-ups, INT0 on falling edge, COUNTER SOURCE=CLCK, PRESCALER=64 
	MOVWF	OPTION_REG			
	
	BCF	STATUS, RP0			; SELECT BANK 0
	
;*******************************************************
	call    LCD_Init        ; Set up the LCD Module	
;Send a message 
	movlw   'N'
	call    Send_Char
	movlw   'E'
	call    Send_Char
	movlw   'C'
	call    Send_Char 
	movlw B'10010000'  		 ; Enable global interupts, Enable the INT0 interrupt
	movwf INTCON

; wait forever for incoming command
WAIT
  SLEEP
  nop
  nop
  GOTO WAIT   ; after Interrupt return to sleep
	

; ****************
DISPLAY
;	Convert  hex to ascii  and display
			
	movwf	r5				;W to buffer
	swapf	r5,W			;
	andlw	b'00001111'		;mask upper nibble
	call	hex2ascii		;convert
	call    Send_Char
	;
	movfw	r5			
	andlw	b'00001111'		;
	call	hex2ascii		;
	call    Send_Char		;
	return


;***************************
hex2ascii
    addwf   PCL,F
    dt      "0123456789ABCDEF"

;*******************************************************************
;* The LCD Module Subroutines                                      *
;*******************************************************************
;
; Initilize the LCD Display Module
;
LCD_Init:
	call    Delay15000      ; Wait for 30ms for LCD to get powered up
	call    Delay15000
	movlw   0x20            ; Command for 4-bit interface 
	movwf   LCD_PORT        ; Send data to LCD      
	call    e_pulse
	call    Delay4100
	movlw   FUNCTION_SET    ; 0x28: 0b00101000 = Function set, 4 wire, 2 lines, 5x7 font
	call    Send_Cmd        ; Can now use the Send_Cmd routine since  in 4bit mode.

	movlw   DISP_ON         ; 0x0c: 0b00001100 = Display on, no cursor, no blink 
	call    Send_Cmd

	movlw   ENTRY_INC       ; 0x06: 0b00000110 = Address increment, no scrolling 
	call    Send_Cmd

	movlw   CLR_DISP        ; 0x01: Clear LCD 
	call    Send_Cmd
; LCD Module is now initalized
	return

;*******************************************************************
;*SendChar - Sends character to LCD                                *
;*This routine splits the character into the upper and lower       * 
;*nibbles and sends them to the LCD, upper nibble first. 
;* Character to be sent is in W                                    *
;*******************************************************************
;
Send_Char
	movwf   tmp            	; save W
	andlw	0xF0
	movwf   LCD_PORT   		; msb: upper 4bits
	bsf     LCD_PORT,RS     ; Set LCD to DATA mode       
	call    e_pulse

	swapf   tmp, w        	; lsb: lower 4bits
	andlw	0xF0
	movwf   LCD_PORT
	bsf     LCD_PORT,RS     ; Set LCD to DATA mode
	call    e_pulse
	call    Delay4100
	return
;
	
;*******************************************************************
;* Send_Cmd - Sends command to LCD                                 *
;* This routine splits the command into the upper and lower        * 
;* nibbles and sends them to the LCD, upper nibble first.          *
;* command to be sent is in W                                      *
;*******************************************************************

Send_Cmd
	movwf   tmp          	; save W
	andlw	0xF0
	movwf   LCD_PORT  		; msb: upper 4bits        	
	call    e_pulse
	swapf   tmp, w          ; lsb: lower 4bits
	andlw	0xF0
	movwf   LCD_PORT
	call    e_pulse
	call    Delay4100
	return

;*******************************************
;   pulse on E
;***********************
e_pulse:
	nop
	nop
	bsf   LCD_PORT,E
	nop
	nop
	bcf   LCD_PORT,E
	return

;*******************************************
;
; Delay routines.   based on 4Mhz 

Delay4100:              ; 4,1 ms
	movlw   d'252'
	movwf   r1
	movlw   4
	movwf   r2
	movlw   1
	movwf   r3
	movlw   1
	movwf   r4
 
delay_loop:
	nop             
	decfsz  r1,f
	goto    delay_loop
	decfsz  r2,f
	goto    delay_loop
	decfsz  r3,f
	goto    delay_loop
	decfsz  r4,f
	goto    delay_loop
	return

Delay15000:             ;15 ms
	movlw   d'156'
	movwf   r1
	movlw   15
	movwf   r2
	movlw   1
	movwf   r3
	movlw   1
	movwf   r4 
	goto    delay_loop
	
;***********

  END