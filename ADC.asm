;*****************************************
;  16F88 ADC TEST ( potentimetre connected to RA1)
;  LCD data on PORT B, RB0-RB3
;  LCD control on PORT A, RA3= E, RA2= RS
;  4MHz crystal
; ************************************************
    LIST         p=16F88                   
    #INCLUDE <p16F88.inc>                  
    __CONFIG    _CONFIG1, _CP_OFF & _CCP1_RB0 & _DEBUG_OFF & _WRT_PROTECT_OFF & _CPD_OFF & _LVP_OFF & _BODEN_ON & _MCLR_ON & _PWRTE_ON & _WDT_OFF & _HS_OSC

	__CONFIG    _CONFIG2, _IESO_OFF & _FCMEN_OFF
 

;------------------------------------------------------------------
; variables:
;------------------------
	CBLOCK 0x070		
    tmp, tmp1, r1, r2, r3, r4, r5, r6
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

; LCD Display Commands and Control Signal names.
;
LCD_DATA         EQU     PORTB   ; LCD data on PORT B, RB0-RB3
LCD_CNTL         EQU     PORTA   ; LCD control on PORT A, RA3= E, RA2= RS
E       EQU     3      ; LCD Enable control line on RA3
RS      EQU     2      ; LCD Register Select control line on RA2
;

;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
; macros
;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

bank1	macro		
	bsf STATUS,RP0 
	endm

bank0	macro		
	bcf STATUS,RP0
	endm

;***********************************

	ORG     0000		; RESET VECTOR

; *********** INITALISE PORTS AND ADC *************
	clrf    PORTA          	; ALL PORT output should output Low.
	clrf    PORTB 
	bank1	; SELECT BANK 1

	MOVLW   0xF3			; 11110011
	MOVWF	TRISA			; SET  pins  RA2, RA3 as outputs, others as inputs

	movlw   0xF0           		 
	movwf   TRISB           ;  RB3 - 0 outputs for LCD data, RB7 - 4 inputs,
	
	movlw B'00000010'
	movwf ANSEL	
	; bit 7 of ANSEL = 0 : non implemented
	; bit 1 of ANSEL = 1 : pin RA1/AN1 analog input 
	; other pins = digital inputs
;------------------------------------------------		
	; Configuration du module ADC

	; tension de référence haute : VDD (5 V)
	; tension de référence basse : VSS (0 V)
	; by default VCFG1 = 0, VCFG0 = 0 :
    ; bcf ADCON1 , VCFG0 	; VCFG0 = 0
    ;bcf ADCON1 , VCFG1 	; VCFG1 = 0
    
	; Choix du format du résultat de la conversion
	; ADRESH = (0 0 0 0 0 0 b9 b8)
	; ADRESL = (b7 b6 b5 b4 b3 b2 b1 b0)

	bsf ADCON1 , ADFM
		
		
	; Choix de la fréquence d'horloge du convertisseur ADC
	; F AD = F OSC (4MHz)/ 8 = 500 kHz
	; T AD = 1/500KHz = 2 µs
	bank0
	bSf ADCON0 , ADCS0 ; ADCS0 = 1, (by default: ADCS1 = 0, ADCS2 (in ADCON1) = 0)
	


	;*****************
	; Main program
	;*****************
	call    LCD_Init        ; Set up the LCD Module
	
;Send a message 
	movlw   'A'
	call    Send_Char
	movlw   'D'
	call    Send_Char
	movlw   'C'
	call    Send_Char
	
	
;==========================================		
	; Sélection du canal 1 (RA1):
	;bcf ADCON0 , CHS2     ; by default CHS2 = 0
	;bcf ADCON0 , CHS1     ; by default CHS1 = 0
	bsf ADCON0 , CHS0      ; CHS0 = 1

	; Mise en service du convertisseur ADC
	bsf ADCON0 , ADON       ; ADON = 1
	
; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	
START	
	; Attente pendant la phase d'acquisition (environ 19,7 µs)
	; 19,7 µs  = 20 cycles (avec un quartz 4 MHz: 4/4= 1us)

	movlw 20
	movwf tmp ; 
attente  
	decfsz tmp
	goto attente

	; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	; Lancement de la phase de conversion de l'ADC
	bsf ADCON0 , GO ; GO = 1

; attente de la fin de la conversion (GO =0)
attend
	btfsc ADCON0 , GO
	goto attend
  
  
	movlw   LINE2         ; Set display to line 2 character 0
	call    Send_Cmd

  
	; lecture  du résultat de la conversion et affichage:
	; High byte
	movf ADRESH , W      ; W = (ADRESH)
	call bin2asc      ; convert MSByte value to ascii
	movf r5, w        ; high nible
	call    Send_Char
	movf r6, w        ; low nible
	call    Send_Char
	; Low byte
	bank1
	movf ADRESL , W       ; W = (ADRESL)
	bank0
	call bin2asc        ; convert LSByte value to ascii
	movf r5, w
	call    Send_Char
	movf r6, w
	call    Send_Char
	goto  START	    	; again
	
	
	
;****************************************
; bin to ascii converter:  
; input:  bin value in w
; output: ascii for high nible in r5 and ascii for low nible in r6
;---------------------------
bin2asc
  movwf tmp   		; save W
  ; low nibble
  andlw 0x0F  		; low nibble
  movwf tmp1  		; save low nible
  sublw 0x09  		; compare with 9: W= 9 - (W)
  btfss STATUS,C 	; if (w) = or < 9 skip
  goto L1
  movf tmp1, w  	; restore low nibble
  addlw 0x30    	; < 10 we add 0x30 to get ascii code
  movwf r6
  goto hnib
  
L1
  movf tmp1, w  	; restore low nibble
  addlw 0x37   		; = or > 10 we add 0x37 to get ascii code
  movwf r6
  
hnib    ; high nibble
  swapf tmp, w   	 ; restore and swap W
  andlw 0x0F  	 
  movwf tmp1  		; save high nible
  sublw 0x09  		; compare with 9: W= 9 - (W)
  btfss STATUS,C 	; if (w) = or < 10 skip
  goto L2
  movf tmp1, w  	; restore low nibble
  addlw 0x30    	; < 10 we add 0x30 to get ascii code
  movwf r5
  goto OK
  
L2
  movf tmp1, w 		; restore low nibble
  addlw 0x37    	; = or > 10 we add 0x37 to get ascii code
  movwf r5

OK
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