;**********************************************************************
;   RS232  9600 8N1  TX mode only: 
;   PORTA2 --> TX                        
;   PIC 16F84A @ 4 MHz                                          
;**********************************************************************
	LIST      p=16F84A      
	#include p16f84A.inc	 
				
	
	__CONFIG   _CP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC

;*********************************************************************
;                              ASSIGNATIONS                          *
;*********************************************************************

	RS_qtz		EQU	.4000000					; Crystal frequency (4 MHz)
	RS_delay	EQU (((RS_qtz/4)/.9600)/3-2)	; 9600 = baud 

	#define TX PORTA, 2     	; TX PIN

	CONSTANT LF =	d'10'		; line feed
	CONSTANT CR =	d'13'		; carriage return
	CONSTANT TAB =	d'9'		; tabulator
	CONSTANT BS =	d'8'		; backspace

;*********************************************************************
;                    VARIABLES   DECLARATION                         *
;*********************************************************************
	CBLOCK 0x020	; start of variables zone	

	RS_buff			; RS232 : buffer ( 1 byte ) for TX/RX
	RS_count		; RS232 : Nbr of bits 
	RS_tmp			; RS232 : temporary variable  for tempo loop

	endc			; end of zone 

;**********************************************************************
;                      DEMARRAGE SUR RESET                            *
;**********************************************************************

	ORG 0x000			; reset vector 
; INITIALISATION
; ---------------
	bsf	STATUS,RP0      ; BANK1
	bcf TRISA, 2        ; TX pin as output
	bcf	STATUS,RP0      ; BANK0
	bsf	PORTA, 2        ; TX pin = 1
	

;*********************************************************************
;                   Main  Program                                    *
;*********************************************************************

  ; New line
	movlw LF 		    ; line feed
	call 	RSsend	
	movlw CR 		    ; carriage return
	call 	RSsend
		
	movlw 'L'
	call 	RSsend
	movlw 'A'
	call 	RSsend
	movlw 'O'
	call 	RSsend
	movlw 'U'
	call 	RSsend
	movlw 'A'
	call 	RSsend
	movlw 'R'
	call 	RSsend

	
fin
	goto $				; hang


;********************************************************************
;                       send one byte (in W )                       *
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
	bcf	TX	        	; else 0
	goto	$+2		    ; jump to  tempo
	bsf	TX	        	; bit = 1
	call	RSdelai		; tempo
	decfsz	RS_count,f	; decremente sent bits counter 
	goto	$-8		    ; next bits
	bsf	TX	        	; stop bit
	call	RSdelai		; tempo for stop  bit
	call	RSdelai		; tempo for safety
	return			    ; all 8 bits sent


;********************************************************************
;      tempo delay   104 uS pour 9600                               *
;********************************************************************
RSdelai 
	movlw	RS_delay	; 1bit tempo
	movwf	RS_tmp
	decfsz	RS_tmp,f
	goto	$-1
	return

;********************************************************************	
	END
