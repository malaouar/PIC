;**********************************************************************
;    test RS232  9600 8N1: 
;    RB0 --> RX  (interrupt driven reception)
;    RB1 --> TX                        
;   PIC 16F84A à 4 MHz                                          
;**********************************************************************
	LIST      p=16F84A      ; 
	#include p16f84A.inc	;  
				
	__CONFIG   _CP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC

;*********************************************************************
;                              ASSIGNATIONS                          *
;*********************************************************************

RS_port		EQU PORTB 
RS_tx		EQU	1
RS_rx		EQU	0	
RS_qtz		EQU	.4000000					; Crystal frequency (4 MHz)
RS_delay	EQU (((RS_qtz/4)/.9600)/3-2)	; 9600 = baud 
RS_delay5	EQU (((RS_qtz*3/8)/.9600)/3-4)	; delay for 1.5 bit

;*********************************************************************
;                   DECLARATIONS DE VARIABLES                        *
;*********************************************************************
	CBLOCK 0x020		; début de la zone variables		

	RS_buff			; RS232 : buffer ( 1 octet ) pour l'envoi ou la reception
	RS_count		; RS232 : Nbr de bits de données de la liaison série
	RS_tmp			; RS232 : variable temporaire pour boucle de tempo
	Sav_W,Sav_S,env
	endc			; Fin de la zone 

;**********************************************************************
;                       RESET                                         *
;**********************************************************************

	ORG 0x000		 
	goto init		

;**********************************************************************
;                      INTERRUPTION   ROUTINE                         *
;**********************************************************************
; ISR
	org 0x004				; interrupt vector
	;movwf   Sav_W  		; save  W
	;swapf	STATUS,w		; swap status with w
	;clrf	STATUS
 	;movwf	Sav_S			; save swapped status

	call	RSrec
	;btfsc	env,0			; byte received verification
	call 	RSsend			; send received byte (echo)
	
	;clrf	env
	bcf	INTCON,INTF
  
	;swapf	Sav_S,W
	;movwf	STATUS
	;swapf	Sav_W,F
	;swapf	Sav_W,W
	retfie	

;========================================	
; INITIALISATION
; ---------------
init		
	bsf	STATUS,RP0
	bcf OPTION_REG, 6   	; Interrupt on falling edge of RB0/INT pin
	bcf	TRISB,1		     	; B1 output (TX)
	bcf	STATUS,RP0
	

;*********************************************************************
;                    Main  Program                                   *
;*********************************************************************

	call RSinit
		
	movlw 'L'
	movwf	RS_buff
	call 	RSsend
	movlw 'A'
	movwf	RS_buff
	call 	RSsend
	movlw 'O'
	movwf	RS_buff
	call 	RSsend
	movlw 'U'
	movwf	RS_buff
	call 	RSsend
	movlw 'A'
	movwf	RS_buff
	call 	RSsend
	movlw 'R'
	movwf	RS_buff
	call 	RSsend

  ; Enable interrupt
	movlw	H'0090'
	movwf	INTCON        ; GIE=1, INTE=1
 	
start
	nop
	goto start			  ; hang

;*********************************************************************
;                 RS232   init                                       *
;*********************************************************************
RSinit	
	bsf	RS_port,RS_tx	; init Tx ( line @ 1 in idle state)
	return

;********************************************************************
;                       send one byte (in W)                        *
;********************************************************************
RSsend	
	movwf	RS_buff
	movlw	8		    ; 8 bits
	movwf	RS_count	; sent bits counter
	bcf	TX	        	; start bit
	call	RSdelai		; tempo
	rrf	RS_buff,f	  	; C = bit to send
	btfsc	STATUS,C	; bit = 1 ?
	goto	$+3		    ; yes
	bcf	RS_port,RS_tx	; else 0
	goto	$+2		    ; jump to  tempo
	bsf	RS_port,RS_tx	; bit = 1
	call	RSdelai		; tempo
	decfsz	RS_count,f	; decremente sent bits counter 
	goto	$-8		    ; next bits
	bsf	RS_port,RS_tx	; stop bit
	call	RSdelai		; tempo for stop  bit
	call	RSdelai		; tempo for safety
	return			    ; all 8 bits sent


;********************************************************************
;                       Receive 1 Byte on RB0                       *
;********************************************************************
RSrec
	call	RSde5			; 1 bit 1/2 tempo
	movlw	8		   		; 8 bits
	movwf	RS_count		; received bits counter
	clrf	RS_buff			; reset buffer
	bcf	STATUS,C			; clear carry
	btfsc	RS_port,RS_rx	; bit = 1 ?
	bsf	STATUS,C			; yes received bit = 1
	rrf	RS_buff,f			; save in buffer
	call	RSdelai			; tempo
	decfsz	RS_count,f		; decrement reeived bits counter
	goto	$-6				; next bits
	;bsf	env,0			; set byte received flag
	return					; all 8 bits are received

;********************************************************************
;                        temporisation                              *
;********************************************************************
RSdelai 
	movlw	RS_delay		; 1 bit tempo
	movwf	RS_tmp
	decfsz	RS_tmp,f
	goto	$-1
	return
;-------------------------------------------------------
RSde5						; 1.5 bit tempo
	movlw	RS_delay5
	movwf	RS_tmp
	decfsz	RS_tmp,f
	goto	$-1
	return

;********************************************************************	
	END
