;*******************************************************************
; !!!!!!!!!!!!!!! ASSEMBLE WITH CASE SENSITIVE OPTION !!!!!!!!!!!!!!
; In MBASM select "case sensitive"

;*******************************************************************
;	CPU configuration
	processor	16f84a
	include		<p16f84a.inc>
	__config	_HS_OSC & _PWRTE_ON & _WDT_OFF

;******************************************
BANK0 macro
	bcf STATUS,RP0
	endm

BANK1 macro
	bsf STATUS,RP0
	endm

;**********************************************************
;	I/O Assignments. 

	#define relay	PORTA,0x01	 	; 1 = energise relay (NPN transistor)
	#define	functn	PORTB,0x07		; Pin 13, M0DE select: 0 = L; 1 (floating) = C

	#define INITSTATUSMASK b'11000000'
	#define BANKSEL	STATUS,RP0

;===============================================================
; RS232 staf
	RS_qtz		EQU	.4000000		; Crystal frequency (4 MHz)
	RS_delay	EQU (((RS_qtz/4)/.9600)/3-2)	; 9600 baud 

	#define TX PORTB, 4    		; TX PIN

	CONSTANT LF =	d'10'		; line feed
	CONSTANT CR =	d'13'		; carriage return
	CONSTANT TAB =	d'9'		; tabulator
	CONSTANT BS =	d'8'		; backspace

;*******************************************************************
;	file register declarations: uses only registers in bank0
;	bank 0 file registers begin at 0x0c in the 16F84
;*******************************************************************

	cblock	0x0C
;Floating Point Stack and other locations used by FP.TXT
	AARGB4
	AARGB3
	AARGB2
	AARGB1
	AARGB0
	AEXP			  ; 8 bit biased exponent for argument A
	SIGN			  ; save location for sign in MSB

	FPFLAGS			; floating point library exception flags
                	
	BARGB2
	BARGB1
	BARGB0
	BEXP			  ; 8 bit biased exponent for argument B

	TEMPB3			; 1 Unused byte
	TEMPB2			; 1 Unused byte
	TEMPB1			; Used
	TEMPB0			; 1 Unused byte

;	LOOPCOUNT		; loop counter (Unused ??)

	CARGB1
	CARGB0			; most significant byte of argument C
	CEXP			  ; 8 bit biased exponent for argument C

;	"Main" Program Storage
 
	COUNT			  ; Bin to BCD convert (bit count)
	cnt			    ; (BCD BYTES)

	COUNT1			; Used by delay routines
              ; and "prescaler flush"
	COUNT2			; Timing (100ms)

	INITSTATUS	; Function (PORTB) at Init

	F1:2
	F2:2
	F3:2
	
	bcd:4			  ; BCD, MSD first 

	Charcount		; number of chars displayed
	Etemp			  ; Repalces use of EEADR - PA0EJH
	
	RS_buff			; RS232 : buffer ( 1 octet ) pour l'envoi ou la reception
	RS_count		; RS232 : Nbr de bits de données de la liaison série
	RS_tmp			; RS232 : variable temporaire pour boucle de tempo

	
	endc

EXP	  equ	AEXP		; Used by FP.TXT
TEMP	equ	TEMPB0

;*******************************************************************
;       GENERAL MATH LIBRARY DEFINITIONS
;	define assembler constants

B0		equ	0
B1		equ	1
B2		equ	2
B3		equ	3
B4		equ	4
B5		equ	5
B6		equ	6
B7		equ	7

MSB		equ	7
LSB		equ	0

;     STATUS bit definitions

#define	_C	STATUS,0
#define	_Z	STATUS,2

;*******************************************************************
;       FLOATING POINT literal constants

EXPBIAS         equ     D'127'

;       floating point library exception flags

IOV   equ     0       ; bit0 = integer overflow flag
FOV   equ     1       ; bit1 = floating point overflow flag
FUN   equ     2       ; bit2 = floating point underflow flag
FDZ   equ     3       ; bit3 = floating point divide by zero flag
NAN	  equ	  4	      ; bit4 = not-a-number exception flag
DOM	  equ	  5	      ; bit5 = domain error exception flag
RND   equ     6       ; bit6 = floating point rounding flag, 0 = truncation
                      ; 1 = unbiased rounding to nearest LSB
SAT   equ     7       ; bit7 = floating point saturate flag, 0 = terminate on
                      ; exception without saturation, 1 = terminate on
                      ; exception with saturation to appropriate value

#define BADFPFLAGS b'1111'
;**********************************************************
;	Motorola syntax branches

#define	beq	bz 
#define	BEQ	bz
#define	BNE	bnz
#define	bne	bnz
#define	BCC	bnc
#define	bcc	bnc
#define	BCS	bc
#define	bcs	bc

#define	BRA	goto
#define	bra	goto

	org H'2100' ; Initialize EEPROM Data
eestart
ovr		  de	"*???*",0
ovrmax	de	"*max*",0
Unit1	  de	" nF",0
Unit2	  de	" pF",0
Unit3	  de	" mH",0
Unit4	  de	" uH",0
Cintro	de	" C = ",0
Lintro	de	" L = ",0
Calibr  de	"calibrating",0

;**********************************************************
;	On RESET Begin Executable Stuff
	org	0

START	
;********************************************************************
;       Initialise Input & Output devices
;********************************************************************
	bsf	BANKSEL		    ; Select Bank1

	errorlevel	-302	; Dont complain about BANK 1 Registers cose we know exactly what we are doing

	movlw	0x37		    ; Option register
	movwf	OPTION_REG	; Port B weak pull-up enabled
                    ; INTDEG Don't care
                    ; Count RA4/T0CKI
                    ; Count on falling edge
                    ; Prescale Timer/counter
                    ; divide Timer/counter by 256
	; PORTA:-
	movlw	0x1C		    ; initialise data direction
                    ; 1 = input
                    ; 0 = output
                    ; PORTA has 5 pins     4 3 2 1 0
                    ; 0x10       =   0 0 0 1 1 1 0 0
	movwf	TRISA		    ; PORTA<0>   = CLAMP count input
                    ; PORTA<1>   = Relay. 1 = energise
                    ; PORTA<2>   = input
                    ; PORTA<3>   = input
                    ; PORTA<4>   = Count Input
                    ; PORTA<5:7> = not implemented in 16F84

  ; PORTB:-
	movlw	0xEF		    ; initialise data direction
                    ; PORTB has 8 pins
                    ; port pin       7 6 5 4 3 2 1 0
                    ; 0xf0       =   1 1 1 0 1 1 1 1

	movwf	TRISB		    ; PORTB<0>   = input
                    ; PORTB<1>   = input
                    ; PORTB<2>   = input
                    ; PORTB<3>   = input
                    ; PORTB<4>   = output RS232 TX
                    ; PORTB<5>   = Input
                    ; PORTB<6>   = Input
                    ; PORTB<7>   = Input

	errorlevel	+302

	bcf	BANKSEL		    ; Re-select Bank0

	CLRF	PORTA		  ; 
;**********************************************************
;	Main Program	
  bcf	  relay		      ; de-energise relay (NPN transistor)
	movlw INITSTATUSMASK
	andwf PORTB,w
	movwf INITSTATUS		

	CLRF	PORTB
	bsf TX              ; TX=1, other pins =0

	
;	"Zero" the meter.

Chk4Z:
	CALL	CLEAR	 	      ; CLEAR  terminal screen
	call 	MS200

	movlw 'C'
	btfss	functn		    ; which mode? 0=Inductor
	movlw 'L'
	call PRINTCHAR
	movlw ':'
	call PRINTCHAR
	
	MOVLW   Calibr-eestart		; Display's " Calibrating "
	call	pmsg		      ; to entertain the punters

	call	Measure		    ; Dummy Run to stabilise oscillator.
	call	MS200		      ; was MS300
	movf	F3,W		      ; Copy to F1
	beq		Chk4Z		      ; F < 2560Hz ?

	call	Measure		    ; Get freq in F3

	movf	F3,W		      ; Copy to F1
	beq		Chk4Z		      ; F < 2560Hz ?
	movwf	F1
	movf	F3+1,W
	movwf	F1+1

	bsf	relay		        ; Add standard capacitor
	call	MS200

	call	Measure

	movf	F3,W		      ; Copy to F2
	beq		Chk4Z		      ; F < 2560Hz ?
	movwf	F2
	movf	F3+1,W
	movwf	F2+1
	
	bcf	relay		        ; Remove standard capacitor
	call	MS200
	CALL	CLEAR	 	      ; CLEAR  LCD MODULE	

;	Now we resume our regular program

M_F3:
	call CLEARREST	
  call	HOME

	movlw	Cintro-eestart		; C =
	btfss	functn		        ; 0=Inductor
	movlw	Lintro-eestart		; L =
	call	pmsg

	call	Measure		        ; Measure F3 & leave it there

	movf	F3,w		          ; test for "too low" frequency
	beq	OORange		          ; F < 2560Hz ?
	
	btfss	INTCON,T0IF	      ; test for "too high" frequency
	goto	OK2GO		          ; F > 655359Hz ?

OORange:	
  MOVLW	ovr-eestart		    ; Over/Under range message
	call	pmsg
	goto	M_F3

O1Range:	
  MOVLW	ovrmax-eestart		; Over/Under range message
	call	pmsg
	goto	M_F3

;	Precompute major bracketed terms cos
;	we need 'em both for all calculations

OK2GO:	
	call	F1_F2
	call	F1_F3

	clrf FPFLAGS

;	See what mode we are in
	
	btfss	functn		    ; 0=Inductor
	goto	Do_Ind

;	OK, we've been told it's a capacitor

Do_Cap:	
 	call	C_calc

	movlw   BADFPFLAGS
	andwf   FPFLAGS,w
	btfss   STATUS,Z   
	goto    O1Range
	 	
	call	C_disp
	goto	M_F3

;	Now,  it's an inductor

Do_Ind:	
  call	L_calc

	movlw   BADFPFLAGS
	andwf   FPFLAGS,w
	btfss   STATUS,Z   
	goto    O1Range
	
	call	L_disp
	goto	M_F3

;**********************************************************
;	Print String addressed by W
;	Note: Strings are in program space

pmsg    
; uses EEDATA and EEADR
	movwf EEADR
pm1:
	BANK1
	BSF EECON1, RD    ; EE Read
	BANK0
	movfw EEDATA
	btfsc STATUS,Z		; NULL = All done
	return
	call	PRINTCHAR
	incf	EEADR,F
	goto	pm1


;**********************************************************
;	Delay for about 200ms or 300ms (untrimmed)

MS300	
  call	MS100
MS200	
  call	MS100

;**********************************************************
;	Delay for about 100ms

MS100	
	MOVLW	0x7e		  ; Count up
	MOVWF	COUNT1		; to roll-over
	MOVLW	0x20		  ; was 0x19, then 0x25, then 1f
	MOVWF	COUNT2			
L3	
	INCFSZ	COUNT2,F
	GOTO	L3
	INCFSZ	COUNT1,F
	GOTO	L3
	RETLW	0

;**********************************************************
;	Put a BCD nybble to display

PutNyb	
	ANDLW	0x0F		; MASK OFF OTHER PACKED BCD DIGIT
	ADDLW	0x30		; Convert BIN to ASCII

;**********************************************************
;	Put a byte to display

PRINTCHAR	
	incf Charcount,f	
	call 	RSsend
	return

;******************************************************************
;	Convert 24-bit binary number at <AARGB0,1,2> into a bcd number
;	at <bcd>. Uses Mike Keitz's procedure for handling bcd 
;	adjust; Modified Microchip AN526 for 24-bits.

B2_BCD   
        movlw   .24		  ; 24-bits
        movwf   COUNT		; make cycle counter
        clrf    bcd+0		; clear result area
        clrf    bcd+1
        clrf    bcd+2
        clrf    bcd+3
b2bcd2  
        movlw   bcd 		; make pointer
        movwf   FSR
        movlw   .4
        movwf   cnt

; Mike's routine:

b2bcd3  
        movlw   0x33            
        addwf   INDF,f          ; add to both nibbles
        btfsc   INDF,3          ; test if low result > 7
        andlw   0xf0            ; low result >7 so take the 3 out
        btfsc   INDF,7          ; test if high result > 7
        andlw   0x0f            ; high result > 7 so ok
        subwf   INDF,f          ; any results <= 7, subtract back
        incf    FSR,f           ; point to next
        decfsz  cnt,f
        goto    b2bcd3
        rlf     AARGB2,f	      ; get another bit
        rlf     AARGB1,f
        rlf     AARGB0,f
        rlf     bcd+3,f         ; put it into bcd
        rlf     bcd+2,f
        rlf     bcd+1,f
        rlf     bcd+0,f
        decfsz  COUNT,f         ; all done?
        goto    b2bcd2          ; no, loop
        return                  ; yes

;**************** CLEAR terminal screen *************************
CLEAR	
	movlw 0x1B
	call 	RSsend
	movlw '['
	call 	RSsend
	movlw '2'
	call 	RSsend
	movlw 'J'
	call 	RSsend
	return

;*********** MOVE TO HOME *****************************

HOME	
  ; terminal
  	movlw ' '
	call 	RSsend
	
	; Home Cursor 
	movlw 0x1B
	call 	RSsend
	movlw '['
	call 	RSsend
	movlw 'H'
	call 	RSsend
	clrf 	Charcount
	return

;**********************************
CLEARREST
	btfsc Charcount,4 ; 16 chars displayed?
	return
	movlw ' '
	call PRINTCHAR
	goto CLEARREST
	

;**********************************************************
;	Measure Frequency. Stash in "F3 and F3+1"

Measure	
	movlw INITSTATUSMASK
	andwf PORTB,w
	subwf INITSTATUS,w
	btfss STATUS,Z
	goto START 	

	bcf	INTCON,T0IF	    ; Declare "Not yet Over-range"

	CLRF	TMR0		      ; RESET INTERNAL COUNT (INCLUDING PRESCALER)
                      ; See page 27 Section 6.0

	bsf	PORTA,0		      ; Part of Osc gating

	CLRF	F3		        ; Ready to receive 16 bit number
	CLRF	F3+1

	; OPEN GATE

	bsf	BANKSEL		      ; Select Bank1

	errorlevel	-302	  ; Dont complain about BANK 1 Registers
                      ; cos we know exactly what we are doing?

	; PORTA:-
	movlw	0x11		      ; initialise data direction
                      ; 1 = input
                      ; 0 = output
                      ; PORTA has 5 pins     4 3 2 1 0
                      ; 0x10       =   0 0 0 1 0 0 0 1

	movwf	TRISA		      ; PORTA<0>   = 
                      ; PORTA<1>   = 
                      ; PORTA<2>   = LCD "E"
                      ; PORTA<3>   = LCD "RS"
                      ; PORTA<4>   = Input
                       ; PORTA<5:7> = not implemented in 16F84

	CALL	MS100		      ; 100MS DELAY

	; CLOSE GATE (COUNT COMPLETE)

	; PORTA:-
	movlw	0x10		      ; initialise data direction
                      ; 1 = input
                      ; 0 = output
                      ; PORTA has 5 pins     4 3 2 1 0
                      ; 0x10       =   0 0 0 1 0 0 0 0

	movwf	TRISA		      ; PORTA<0>   = 
                      ; PORTA<1>   = 
                      ; PORTA<2>   = LCD "E"
                      ; PORTA<3>   = LCD "RS"
                      ; PORTA<4>   = Input
                      ; PORTA<5:7> = not implemented in 16F84

	errorlevel	+302

	bcf	BANKSEL		      ; Re-select Bank0
	MOVF	TMR0,W		    ; GET HIGH BYTE		
	MOVWF	F3		        ; Copy to Big end of 16 bit result

; The 311 "outputting" a 1 'cos we've forced it high
; so T0CKI=1.

PSC1	
	bsf	BANKSEL		      ; Select Bank1

	errorlevel	-302	  ; Dont complain about BANK 1 Registers
                      ; cos we know exactly what we are doing?

	bsf	OPTION_REG,T0SE	; Clock the prescaler
	nop
	bcf	OPTION_REG,T0SE
	
	errorlevel	+302

	bcf	BANKSEL		      ; Re-select Bank0
	DECF	F3+1,F		    ; Decrement the counter
	movf	TMR0,W		    ; Has TMR0 changed?
	xorwf	F3,W		      ; if unchanged, XOR -> 0
	beq	PSC1
	return			        ; F3 : F3+1 now holds 16 bit result


;**********************************************************
;	Formatted display of BCD work area for Capacitor

C_disp
F_C1	
	MOVF	bcd+0,W
	ANDLW	0x0F
	beq	F_C2

	CALL	PutNyb
	call	Swap1
	call	Move1
	CALL	DoDP		; Print DP
	call	Swap2
	goto	F_C3U

;--------------------------------------------------

F_C2	
	swapf	bcd+1,W
	ANDLW	0x0F
	beq	F_C3

	CALL	PutNyb
	call	Move1
	CALL	DoDP		; Print DP
	call	Swap2
	call	Move2
	goto	F_C3U		; print nF. includes RETURN

;--------------------------------------------------

F_C3	
	MOVF	bcd+1,W
	ANDLW	0x0F
	beq	F_C4

	CALL	PutNyb
	CALL	DoDP		        ; Print DP
	call	Swap2
	call	Move2
	call	Swap3

F_C3U	
	movlw	Unit1-eestart		; nF
	goto	pmsg		        ; includes RETURN

;--------------------------------------------------

F_C4	
	SWAPF	bcd+2,W		  ; Digit1 == 0 ?
	ANDLW	0x0F
	bne	NoB1_C
	MOVLW	0x20		    ; YES PRINT A SPACE
	call	PRINTCHAR
	MOVF	bcd+2,W		  ; Digit2 == 0 ?
	ANDLW	0x0F
	bne	NoB2_C
	MOVLW	0x20	    	; YES PRINT A SPACE
	call	PRINTCHAR
	bra	NoB3_C

NoB1_C	
	call	Swap2		    ; 1
NoB2_C	
	call	Move2		    ; 2
NoB3_C	
	call	Swap3		    ; 3
	CALL	DoDP		    ; Print DP
	call	Move3		    ; 4

	movlw	Unit2-eestart		; pF
	goto	pmsg	        	; includes RETURN

;**********************************************************
;	Formatted display of BCD work area for Inductor

L_disp
F_L1	
	MOVF	bcd+0,W
	ANDLW	0x0F
	beq	F_L2

	CALL	PutNyb
	call	Swap1
	CALL	DoDP		; Print DP
	call	Move1
	call	Swap2
	goto	F_L2U		; Print mH. includes RETURN

;--------------------------------------------------

F_L2	
	swapf	bcd+1,W
	ANDLW	0x0F
	beq	F_L3

	CALL	PutNyb
	CALL	DoDP		        ; Print DP
	call	Move1
	call	Swap2
	call	Move2
	
F_L2U	
	movlw	Unit3-eestart		; mH
	goto	pmsg		        ; includes RETURN

;--------------------------------------------------

F_L3	
	MOVF	bcd+1,W
	ANDLW	0x0F
	beq	F_L4

	CALL	PutNyb
	call	Swap2
	call	Move2
	CALL	DoDP		; Print DP
	call	Swap3
	goto	F_L4U		; Print uH. includes RETURN

;--------------------------------------------------

F_L4	
	SWAPF	bcd+2,W		; Digit1 == 0 ?
	ANDLW	0x0F
	bne	NoB1_L
	MOVLW	0x20		  ; YES PRINT A SPACE
	call	PRINTCHAR
	goto	NoB2_L

NoB1_L	
	call	Swap2		; 1
NoB2_L	
	call	Move2		; 2
	CALL	DoDP		; Print DP
	call	Swap3		; 3
	call	Move3		; 4

F_L4U	
	movlw	Unit4-eestart		; uH
	goto	pmsg		        ; includes RETURN

;--------------------------------------------------
;	Common subroutine for formatted output

DoDP	
  MOVLW	'.'		      ; Print DP
	goto	PRINTCHAR		; Return from PRINTCHAR

Swap1	
  SWAPF	bcd+1,W
	goto	PutNyb

Move1	
  MOVF	bcd+1,W
	goto	PutNyb

Swap2	
  SWAPF	bcd+2,W
	goto	PutNyb

Move2	
  MOVF	bcd+2,W
	goto	PutNyb

Swap3	
  SWAPF	bcd+3,W
	goto	PutNyb

Move3	
  MOVF	bcd+3,W
	goto	PutNyb

;********************************************************************
;	Stack operations
;********************************************************************	

subtract	
  call	FPS24
	goto	S_fix

divide		
  call	FPD24
	goto	S_fix

multiply	
  call	FPM24

;	Fix stack after add, subtract, divide & multiply

S_fix	
	MOVF	CARGB1,W	    ; C -> B
	MOVWF	BARGB1

	MOVF	CARGB0,W
	MOVWF	BARGB0

	MOVF	CEXP,W
	MOVWF	BEXP

	return

;	Push stack (duplicates TOS)

S_push	
	MOVF	BARGB1,W	    ; B -> C
	MOVWF	CARGB1

	MOVF	BARGB0,W
	MOVWF	CARGB0

	MOVF	BEXP,W
	MOVWF	CEXP

	MOVF	AARGB1,W	    ; A -> B
	MOVWF	BARGB1

	MOVF	AARGB0,W
	MOVWF	BARGB0

	MOVF	AEXP,W
	MOVWF	BEXP

	return

;	Swap A and B

S_swap	
	MOVF	AARGB1,W	    ; A -> Etemp (temp)
	MOVWF	Etemp
	MOVF	BARGB1,W	    ; B -> A
	MOVWF	AARGB1
	MOVF	Etemp,W		    ; Etemp (temp) -> B
	MOVWF	BARGB1
;--------------------------------------------------
	MOVF	AARGB0,W	    ; A -> Etemp (temp)
	MOVWF	Etemp
	MOVF	BARGB0,W	    ; B -> A
	MOVWF	AARGB0
	MOVF	Etemp,W		    ; Etemp (temp) -> B
	MOVWF	BARGB0
;--------------------------------------------------
	MOVF	AEXP,W		    ; A -> Etemp (temp)
	MOVWF	Etemp
	MOVF	BEXP,W		    ; B -> A
	MOVWF	AEXP
	MOVF	Etemp,W		    ; Etemp (temp) -> B
	MOVWF	BEXP
	return
;********************************************************************
;	Calculate Unknown Capacitance OR inductance
;       Output: 24 bit positive integer (scaled)
;	right justified in AARGB0, AARGB1, AARGB2
;	also as BCD in bcd:bcd+1:bcd+2:bcd+3
;********************************************************************	

C_calc	
    call	divide
	call	Get_Ccal	      ; Times 10,000 ( = 1000.0pF)
	call	multiply
	goto	PorM		        ; includes return
;--------------------------------------------------------------------
L_calc	
    call	multiply
	call	Get_Lcal	      ; Precomputed 1/(Ccal*4*PI*PI)
	call	multiply
L_divF1	call	Get_F1		; Divide by F1^2
	call	S_push
	call	multiply
	call	S_swap
	call	divide

;	Handle space or - in front of FP number

PorM	
  btfss	AARGB0,7	    ; test sign
	goto	Pplus
	
Pminus	
  movlw	0x2d		      ; minus
	goto	PMdisp

Pplus	
  movlw	0x20		      ; plus

PMdisp	
  call	PRINTCHAR		  ; print it
	bcf	AARGB0,7	      ; make plus anyway

;	Format as raw BCD string in bcd:bcd+1:bcd+2:bcd+3

	call	INT2424		    ; To INT in AARGB0 etc.
	goto	B2_BCD		    ; includes return

;********************************************************************
;	Calculate (F1/F3)^2-1, leave result on stack
;********************************************************************	
F1_F3	
  call	Get_F3
	goto	F1_F1

;********************************************************************
;	Calculate (F1/F2)^2-1, leave result on stack
;********************************************************************	

F1_F2	
	call	Get_F2
F1_F1	
	call	Get_F1
	call	divide		; F1/Fx
	call	S_push
	call	multiply	; (F1/Fx)^2
	call	Get_One
	call	S_swap
	goto	subtract	; (F1/Fx)^2-1
						; includes return

;********************************************************************
;	Fetch assorted things used for the calculation
;	of Unknown L and C
;********************************************************************	

Get_Lcal	
    call	S_push		  ; make room first

	movlw	0xAB		  ; 2.53303e+13
	movwf	AEXP		  ; Create FP version of
	movlw	0x38		  ; Precomputed 1/(Ccal*4*PI*PI)
	movwf	AARGB0		  ; times any needed
	movlw	0x4D		  ; fiddle factor (1/100)
	goto	B1_2_stak
	
Get_Ccal	
    call	S_push		  ; make room first

	movlw	0x8c		  ; 10,000
	movwf	AEXP		  ; Create FP version of
	movlw	0x1C		  ; Precomputed Ccal
	movwf	AARGB0		  ; times any needed
	movlw	0x40		  ; fiddle factor
B1_2_stak	
    movwf	AARGB1
	return

Get_One		
    call	S_push		  ; make room first
	clrf	AEXP		  ; Create a binary 1
	clrf	AARGB0
	clrf	AARGB1
	movlw	0x01
	goto	LSB2stak
	
Get_F1		
    movlw	F1		    ; Includes stack push
	goto	W2stak
Get_F2		
    movlw	F2		    ; Includes stack push
	goto	W2stak
Get_F3		
    movlw	F3		    ; Includes stack push

;********************************************************************
;	Copy 16 bit number, pointed to by W, to stack
;	and convert to FP (positive value only)
;	via a 24 bit number in AARGB0,1,2
;********************************************************************	

W2stak		
    movwf	FSR
	call	S_push		; make room first
	clrf	AEXP
	clrf	AARGB0
	movf	INDF,W		; Big Byte first
	movwf	AARGB1
	incf	FSR,F		; then little byte
	movf	INDF,W
LSB2stak	
    movwf	AARGB2
	goto	FLO2424		; 24 bit int -> 24 bit FP

;********************************************************************
;                       send one byte (in W )                       *
;********************************************************************
RSsend	
	movwf	RS_buff
	movlw	8		    ; 8 bits
	movwf	RS_count	; compteur de bits envoyés
	bcf	TX	        	; bit de start
	call	RSdelai		; tempo
	rrf	RS_buff,f	  	; on recupere dans C le bit à envoyer
	btfsc	STATUS,C	; bit à envoyer = 1 ?
	goto	$+3		    ; oui
	bcf	TX	        	; sinon 0
	goto	$+2		    ; on continue sur la tempo
	bsf	TX	        	; bit à 1
	call	RSdelai		; tempo
	decfsz	RS_count,f	; on decremente le compteur de bits envoyés
	goto	$-8		    ; on continue sur les bits suivants
	bsf	TX	        	; bit de stop
	call	RSdelai		; tempo pour bit de stop
	call	RSdelai		; tempo de sécurité
	return			    ; les 8 bits sont envoyés


;********************************************************************
;      Delais de temporisation   104 uS pour 9600                   *
;********************************************************************
RSdelai 
	movlw	RS_delay	; temporisation de la durée d'un bit
	movwf	RS_tmp
	decfsz	RS_tmp,f
	goto	$-1
	return
;********************************************************************	
 	INCLUDE <FP.TXT>
;********************************************************************
 	END

