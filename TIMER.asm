; week timer scheduler.
; Control 4 relays on a periode of a week 
; 127 programs/week saved in flash (254 octets).
; for each program we choose: day, hour, min (multiple of 4) and the relay to turn ON/OFF.
; 2 bytes per programme: 
;   - 1 byte day (3bits) and hour (5 lower bits)
;   - 1 byte minutes in multiple of 4 (high nible)  and relays to control (low nible)
;  minutes 4, 8, 12 ....56.
; each minute we scan the program array in flash.
; we use the variable schedul to point the location in the array  and R1 as a counter (254 MAX).

; 16F84 @ 32,768 KHz (crystal from a clock)

; ************************************************
    LIST         p=16F84A                   
    #INCLUDE <P16F84A.inc>                  
    __config _CP_OFF & _WDT_OFF & _PWRTE_ON & _LP_OSC

;------------------------------------------------------------------
; vars:
  CBLOCK 0x0C             
    min, heur, day, secondes, tmp, tmp2, tmp3, CounterA, schedul, r1, r2, r3, r4, r5, r6
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
LCD_CNTL         EQU     PORTA   ; LCD data on PORT A,RA3= E, RA2= RS
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
	goto init

; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
; Routine d'interruption 
; 1 source d'interruption
; - TMR0 en mode timer
; Cette interruption a lieu toutes les  secondes exactement
; quartz 32768 Hz prescaler 1:32 
;
; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

	org H'0004'	 			; vecteur d'interruption
;-------------------------
; routine executé chaque  seconde
; mise à jour de l'heure
; increment minute, hour and day  counters

	incf secondes,f		; increment secondes
	movlw D'60'			; 60 secondes (for test purpose use lower value ex 2)
	subwf secondes,W
	btfss STATUS,Z 		; on a compté 60s ??	
	goto fin1			; Non : sortir
	clrf secondes		; Oui: on efface le compteur	
	incf min			; et on incremente les minutes 
	movlw D'30'			; for test, must be 60
	subwf min,W			; 60 minutes passées?
	btfss STATUS,Z 		
	goto scheduler		; Non est-ce que c l'heure du programme
	clrf min			; OUi:  remise à zero du compteur minutes 
	incf heur			; et on incrémente l'heure
	movlw D'24'			; 24 hours (use 3 for test)
	subwf heur,W		; 24  heures passées?
	btfss STATUS,Z 
	goto scheduler		; Non est-ce que c l'heure du programme
	clrf heur			; OUi:  remise à zero du compteur heures
	incf day			; increment day counter
	movlw D'07'
	subwf day,W			; 7 days passées?
	btfss STATUS,Z 		
	goto scheduler		; Non est-ce que c l'heure du programme
	clrf day			; OUi:  remise à zero du compteur jours
	 

scheduler	
	movlw   0x80		       	 ;  goto Line1
	call    Send_Cmd
	movf day, w	; 
	call convert
	call    Send_Char
	movlw   ' '
	call    Send_Char	
	swapf heur, w
	andlw 0x0F
	call convert
	call    Send_Char
	movf heur, w	; 
	andlw 0x0F
	call convert
	call    Send_Char
	movlw   ' '
	call    Send_Char
	swapf min, w
	andlw 0x0F
	call convert
	call    Send_Char
	movf min, w	;
	andlw 0x0F 
	call convert
	call    Send_Char
;--------------------------------
;read flash and display
	movlw 0xFE			; 254 bytes for program
	movwf r1			; array pointer

read
	movlw 0xC0			; Set display to line 2 character 0
	call    Send_Cmd

	movf pclath,w		; save PCLATH
	movwf r5

	movlw 3				; cause array is at 0x300
	movwf PCLATH

	movf schedul, w	
	call TAB	; 
 	movwf   tmp2   		 ; save W
	movwf  tmp3

	movf r5,w			; restore PCLATH
	movwf pclath

	swapf tmp3
	rrf tmp3			; shift right
	movf tmp3, w
	andlw 0x07			; get day (3bits)
	subwf day, w
	btfss status, z
	goto skip

	movf tmp2, w
	andlw 0x1F			; get hour (5bits)
	subwf heur, w
	btfss status, z
	goto skip

	decfsz r1			; get next byte is it the last one?
	goto mins			; no: then next byte (minutes + relays)
	goto fin1			; yes: quit
mins
	movf pclath,w		; save PCLATH
	movwf r5

	movlw 3				; cause array is at 0x300
	movwf PCLATH

	incf schedul
	movf schedul, w
	call TAB			; 
	movwf   tmp2   		; save W

	movf r5,w			; restore PCLATH
	movwf pclath

	movf tmp2,w
	andlw 0x0F 			; Low nible (min)
	movwf  tmp3
	BCF status,C  		; C=0
	RLF tmp3,F  		; tmp3= tmp3X2
	BCF status,C
	RLF tmp3,w  		; W= tmp3X2 (so tmp3X4)

	subwf min, w		; actual min = prgrammed min?
	btfss status, z		; Yes: 
	goto nextp			; No: chek next program
		
	btfss tmp2,7		; Relay ON?
	goto zero			; No
	movlw   'O'
	call    Send_Char
	movlw   'N'
	call    Send_Char
	bsf porta, 1

	goto fin1			; quit
	
zero
	movlw   'O'
	call    Send_Char
	movlw   'F'
	call    Send_Char
	bcf porta, 1

	goto fin1
		
skip	; skip byte (minutes + relays)
	decfsz r1			;  r1 - 1 =0?	
	goto suite			; No then skip
	goto fin1			; Yes: quit
suite
	incf schedul		; skip byte 	
nextp	
	
	decfsz r1			; next program: 64 bytes tested?
	goto again			; No: then continue
	goto fin1			; oui: alors fin
again
	incf  schedul		; next program
	goto read			; 


;---------------------------	
fin1
	clrf schedul		; start from begining of EEPROM
	bcf INTCON,T0IF		; on efface le drapeau T0IF	
	retfie				; retour d'interruption
	
	
;*********************************************
; RESET VECTOR	
;********************************************
;  INITALISE PORTS AND TMR0 
init
	clrf    PORTA       ; ALL PORT output should output Low.
	clrf    PORTB

	BSF	STATUS, RP0		; SELECT BANK 1

	MOVLW   0xF1		; 11110001
	MOVWF	TRISA		; SET  pins RA0  and 4 AS input and RA1, RA2, RA3 outputs

	movlw   0xF0           		 
	movwf   TRISB       ; RB7 - 4 inputs, RB3 - 0 outputs for LCD data
	
	movlw B'00000100'
	movwf OPTION_REG
	; bit 7 (/RBPU) = 0 : activation des résistances de pull-up du port B
	; bit 6 (INTEDG)= 0 : (non utilisée)
	; bit 5 (T0CS) = 0 : TMR0 Clock Source Select = internal CLK/4
	; bit 4 (T0SE) = 0 :  
	; bit 3 (PSA) = 0  : Prescaler attribué au timer
	; bit 2 (PS2)= 1	  
	; bit 1 (PS1) = 0 
	; bit 0 (PS0) = 1 : 100 --> Facteur de division du prescaler = 1/32
	; (32768/4)/32 = 256Hz	--> 256/256 (TIMER0)= 1s		

	BCF	STATUS, RP0	; SELECT BANK 0
;----------------------------------------------

	call    LCD_Init        ; Set up the LCD Module
	
;Display a message 
	movlw   'C'
	call    Send_Char
	movlw   'L'
	call    Send_Char
	movlw   'O'
	call    Send_Char
	movlw   'C'
	call    Send_Char
	movlw   'K'
	call    Send_Char


;------------
	clrf min	
	clrf heur
	clrf day
	
	clrf secondes
	clrf schedul		; scheduler indice
	
	; Autoriser les interruptions en general et celui du Timer0
	movlw B'10100000' 
	movwf INTCON
	; bit 7 (GIE) = 1 : autorisation globale des interruptions
	; bit 5 (T0IE) = 1 : autorisation de l'interruption TMR0
	; bit 2 (T0IF)= 0 : on efface le drapeau de l'interruption TMR0
	; les autres bits sont inutilisés (valeur par défaut = 0)
	

	clrf TMR0			; mise à zero du timer0
	
;===================================================================
hang
	goto  hang		; loop for ever



;***************************************************************
; binary to hex converter
convert	
	addwf pcl,f
	dt "0123456789ABCDEF"


;*******************************************************************
;* The LCD Module Subroutines                                      *
;*******************************************************************
;
; Initilize the LCD Display Module
;
LCD_Init
	call    Delay15      	; Wait for 30ms for LCD to get powered up
	call    Delay15
	movlw   0x02            ; Command for 4-bit interface 
	movwf   LCD_DATA        ; Send data to LCD      
	call    e_pulse
	call    Delay5
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
	call    Delay5
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
	call    Delay5
	bsf     LCD_CNTL,RS     ; quit command mode

	return

;*******************************************
;   Send pulse on E
;***********************
e_pulse
	;nop
	bsf   LCD_CNTL,E
	;nop
	bcf   LCD_CNTL,E
	return

;*******************************************
; Delay routines.   based on 32768 Hz 

Delay15             ;15 ms
;PIC Time Delay = 0.014892578 s with Osc = 32768 Hz
	movlw	D'39'
	movwf	CounterA
loop1		
	decfsz	CounterA,1
	goto	loop1
	return

Delay5             ;5 ms
;PIC Time Delay = 0.004638672 s with Osc = 32768 Hz
	movlw	D'11'
	movwf	CounterA
loop2		
	decfsz	CounterA,1
	goto	loop2
	return


;------------------------
; 2 octets par programme: 
;   - 1 octet jour (3bits haut) et heure (5bits)
;   - 1 octet minutes en multiple de 4 (low nible)  et relais à actionner (high nible)
; dans cet exemple: jour 0 heure 0 et 4 minute relais ON puis à 12 minutes OFF (regarder la fin du tableau)
; ensuite ON à 20 min et OFF à 28 min 
; on passe au jour 1 heure 1 et 8 min relais ON puis à 16 min OFF
; noubliez pas qu'on miltuplie les minutes par 4
; pas besoin de mettre le programme en ordre
; le dernier octet n'est pas utilisable

	org 0x300

TAB
    addwf   PCL, F
 	dt 0x00, 0xF1, 0xFF, 0xFF,0x00, 0xF5, 0x00, 0x07,0x21, 0xF2, 0x21, 0x04,0xFF, 0xFF, 0xFF, 0xFF
	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
 	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
 	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
 	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
 	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
 	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
 	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
 	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF
	dt 0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0xFF, 0xFF, 0xFF, 0xFF,0x00, 0x03, 0xFF

;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
; Initialisation de la mémoire EEPROM (facultatif)
; Cela met dans l'EEPROM le message :
; (C) Mahmoud LAOUAR 25/09/2015
; (en code ASCII)
; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	org H'2100'		; début de zone EEPROM du PIC 16F84A

	DE '(' ; ou DE H'28' (code ASCII correspondant)
	DE 'C'		
	DE ')',' '
	DE 'M' ,'a','h','m','o','u','d',' '		
	DE 'L','O','U','A','R',' '
	DE '2','5','/','0','9','/','2','0','1','5',' '
	DE 'V' ,'0','.','1'

;***********

  END