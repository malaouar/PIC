;*************************************************************
; A program to decode a diseqc message and display the 4 bytes
; LCD data on PORT B, RB0-RB3
; LCD data on PORT A,RA2= E, RA3= RS
; Diseqc input = RA4
;*************************************************************

         LIST         p=16F628A                   
        #INCLUDE <P16F628A.inc>                  
        __config _CP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC & _LVP_OFF & _DATA_CP_OFF & _BOREN_OFF & _MCLRE_ON


;------------------------------------------------------------------

DISEQ	EQU	4		; DISEQC INPUT 

; vars:
BYTECNT EQU 20	 	; BYTE COUNTER
BITCNT	EQU 21		; BIT COUNTER
BYTE	EQU 22		; STORE FOR RECEIVED BYTES
PARITY	EQU	23		; ACCUMULATOR FOR PARITY BITS
LCNT1	EQU	24		; LOOP COUNTER 1
LCNT2 	EQU	25		; LOOP COUNTER 2
DSBASE	EQU	26		; BASE OF DISEQC TABLE

  CBLOCK 0x26              
    fram, address, command, data1, data2, data3, data4, tmp, tmp1, r1, r2, r3, r4, r5, r6
  ENDC 

;----------------------------------
; LCD Module commands
;
LINE1                   EQU             0x080   ; Set display to line 1 character 0
LINE2                   EQU             0x0C0   ; Set display to line 2 character 0
FUNCTION_SET            EQU             0x028   ; 4 bits, 2 lines, 5x7 Font 
DISP_ON                 EQU             0x00C   ; Display on
DISP_ON_C               EQU             0x00E   ; Display on, Cursor on
DISP_ON_B               EQU             0x00F   ; Display on, Cursor on, Blink cursor
DISP_OFF                EQU             0x008   ; Display off
CLR_DISP                EQU             0x001   ; Clear the Display
ENTRY_INC               EQU             0x006   ;
ENTRY_INC_S             EQU             0x007   ;
ENTRY_DEC               EQU             0x004   ;
ENTRY_DEC_S             EQU             0x005   ;
DD_RAM_ADDR             EQU             0x080   ; Least Significant 7-bit are for address
DD_RAM_UL               EQU             0x080   ; Upper Left coner of the Display
;


LCD_DATA         EQU     PORTB   ; LCD data on PORT B, RB0-RB3
LCD_CNTL         EQU     PORTA   ; LCD data on PORT A,RA2= E, RA3= RS
;
;
;
; LCD Display Commands and Control Signal names.
;
E       EQU     3      ; LCD Enable control line on RA3
RS      EQU     2      ; LCD Register Select control line on RA2
;
;***********************************

	ORG     0000		; RESET VECTOR

; *********** INITALISE PORTS AND TMR0 *************

	clrf    PORTA          		 ; ALL PORT output should output Low.
	clrf    PORTB

  	movlw B'00000111'
	movwf CMCON			; inactivation des comparateurs analogiques

	BSF	STATUS, RP0	; SELECT BANK 1

	MOVLW   0xF3		; 11110011
	MOVWF	TRISA		; SET  pins RA0, 1 and 4 AS inputs and RA2, RA3 outputs

	movlw   0xF0           		 ;
	movwf   TRISB           		; RB7 - 4 inputs, RB3 - 0 outputs for LCD data
	
	MOVLW 	B'00101000'	; ENABLE PULL-UPS, PRESCALER=1, COUNTER SOURCE=RA4
	MOVWF	OPTION_REG			

	BCF	STATUS, RP0	; SELECT BANK 0
;*******************************************************

	call    LCD_Init        ; Set up the LCD Module
	
;Display a greeting message 
	movlw   'D'
	call    Send_Char
	movlw   'I'
	call    Send_Char
	movlw   'S'
	call    Send_Char
	movlw   'E'
	call    Send_Char
	movlw   'Q'
	call    Send_Char
	movlw   'C'
	call    Send_Char	
;----------------------------------------------
START
  	MOVLW	DSBASE		; INITIALISE DISEQC TABLE ADDRESS
	MOVWF	FSR
	
	MOVLW	4			; START COLLECTING DATA - 4 BYTES
	MOVWF	BYTECNT


WGAP	; wait for gap
	CALL	READ22		; COUNTS 22KHZ CYCLES
	BTFSS	STATUS, Z	; WAIT FOR LEADING GAP
	GOTO 	WGAP

NEXTBYTE	
	MOVLW	9			; 9 BITS PER BYTE INCLUDING PARITY
	MOVWF	BITCNT
	CLRF	PARITY		; CLEAR PARITY BIT COUNTER
store	; Next Bit
	RLF	BYTE			; SAVE RECEIVED BIT (Rotate Left through carry)		
WDSQ	; wait for diseqc message
	BTFSS	PORTA, DISEQ	; WAIT FOR START OF SEQUENCE ON PA4
	GOTO 	WDSQ

	CALL	READ22		; GET NEXT BIT
	MOVLW	D'12'		; TEST FOR 0 OR 1, LESS THAN 12 IS A '1'
	SUBWF	TMR0,w		; W= TMR0 - (W): if TMR0 > or =  (W) (BIT =0)  then C=1 (we correct this later)

		
	BTFSS	STATUS,C	; Skip if C=1 (BIT =0)
	INCF	PARITY		; INCREMENT PARITY IF BIT=1

  
	DECFSZ	BITCNT		; DECREMENT BIT COUNT and skip if zero
	GOTO	store		; STORE AND CONTINUE for Next Bit

	BTFSS	PARITY,0	; ALL 9 BITS COLLECTED, CHECK PARITY
	GOTO	START		; ABORT ON FAILED PARITY
	
	COMF	BYTE		; INVERT TO CORRECT POLARITY (cause we have  BIT =0 --> C=1 )
	MOVF	BYTE, W
	MOVWF	INDF		; STORE IN TABLE

	INCF	FSR			; NEXT BYTE IN TABLE
	DECFSZ	BYTECNT		
	GOTO	NEXTBYTE	; NOT ZERO THEREFORE GET NEXT BYTE

; ********* DATA COLLECTION FINISHED  ****************

	movlw   CLR_DISP       	 ; 0x01: Clear LCD 
	call    Send_Cmd
	
	MOVF	fram, W		; GET frame BYTE and display it
	call bin2asc
	movf r5, w
	call    Send_Char
	movf r6, w
	call    Send_Char
	


	MOVF	address, W	; GET adress BYTE
	call bin2asc
	movf r5, w
	call    Send_Char
	movf r6, w
	call    Send_Char


	MOVF	command, W	; GET command BYTE
	call bin2asc
	movf r5, w
	call    Send_Char
	movf r6, w
	call    Send_Char


	MOVF	data1, W	; GET data1 BYTE
	call bin2asc
	movf r5, w
	call    Send_Char
	movf r6, w
	call    Send_Char


	goto  START		; again

;****************************************
bin2asc
  movwf tmp   ; save W
  ; low nibble
  andlw 0x0F  ; low nibble
  movwf tmp1  ; save low nible
  sublw 0x09  ; compare with 9: W= 9 - (W)
  btfss STATUS,C  ; if (w) = or < 9 skip
  goto L1
  movf tmp1, w  ; restore low nibble
  addlw 0x30    ; < 10 we add 0x30 to get ascii code
  movwf r6
  goto hnib
  
L1
  movf tmp1, w  ; restore low nibble
  addlw 0x37    ; = or > 10 we add 0x37 to get ascii code
  movwf r6
  
hnib    ; high nibble
  swapf tmp, w    ; restore and swap W
  andlw 0x0F  ; 
  movwf tmp1  ; save high nible
  sublw 0x09  ; compare with 9: W= 9 - (W)
  btfss STATUS,C  ; if (w) = or < 9 skip
  goto L2
  movf tmp1, w  ; restore low nibble
  addlw 0x30    ; < 10 we add 0x30 to get ascii code
  movwf r5
  goto OK
  
L2
  movf tmp1, w  ; restore low nibble
  addlw 0x37    ; = or > 10 we add 0x37 to get ascii code
  movwf r5

OK
  return
  
; *************** COUNT CYCLES OF 22KHZ  ***************
READ22	
	CLRF	TMR0		; CLEAR COUNTER
;------------Time loop for 1.2 millisec = 1200 cycles @ 4Mhz crystal

	;1193 cycles
	movlw	0xEE
	movwf	r5
	movlw	0x01
	movwf	r6
Delay_0
	decfsz	r5, f
	goto	$+2
	decfsz	r6, f
	goto	Delay_0

	;3 cycles
	goto	$+1

	;4 cycles (including call)
	return

;*******************************************************************
;* The LCD Module Subroutines                                      *
;*******************************************************************
;
; Initilize the LCD Display Module
;
LCD_Init:

	call    Delay15000      ; Wait for 30ms for LCD to get powered up
	call    Delay15000


	movlw   0x02            ; Command for 4-bit interface 
	movwf   LCD_DATA        ; Send data to LCD      

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
	movwf   tmp            ; save W
	swapf   tmp, w         ; msb: upper 4bits
	movwf   LCD_DATA          
	call    e_pulse

	movf    tmp, w         ; lsb: lower 4bits
	movwf   LCD_DATA
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
	movwf   tmp            ; save W
	swapf   tmp, w         ; msb: upper 4bits
	movwf   LCD_DATA          
	bcf     LCD_CNTL,RS     ; Set LCD to command mode
	call    e_pulse

	movf    tmp, w         ; lsb: lower 4bits
	movwf   LCD_DATA
	call    e_pulse
	call    Delay4100
	bsf     LCD_CNTL,RS     ; quit command mode

	return

;*******************************************
;   pulse on E
;***********************

e_pulse:
	nop
	nop
	bsf   LCD_CNTL,E
	nop
	nop
	bcf   LCD_CNTL,E
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