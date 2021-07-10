;********************************************************************************
;*																				*
;*		Frequency counter 0.2													*
;*		Date		:	Sunday, 23 Feb 2007										*
;*		Author		:	C. V. Niras/ VU3CNS										*
;*		Copyright	:	(C)	2007 C. V. Niras									*
;*		Emain		:	cvniras@hamradio.in										*
;*																				*
;********************************************************************************

			LIST	P=16F628A
#INCLUDE	<P16F628A.INC>
 __CONFIG _WDT_OFF & _LVP_OFF & _HS_OSC & _BODEN_ON & _PWRTE_ON & _MCLRE_ON
 
#DEFINE		SINGLE_LINE

#DEFINE		RESET_VECTOR	0x00			; PIC Reset vector.
#DEFINE		INT_VECTOR		0x04			; PIC Int vector
#DEFINE		GATE_TRIS		TRISA,3			; Gate tris
#DEFINE		GATE			PORTA,3			; Gate
#DEFINE		ARG0			ARG0_3			; Simpler definition of ARGs
#DEFINE		ARG1			ARG0_8			; Simpler definition of ARGs
#DEFINE		OFFSET			OFFSET_3		; Simpler definition of ARGs
;#DEFINE		PRE_TMR1		D'24288'		; Pre TMR1 value
#DEFINE		PRE_TMR1		D'48576'		; Pre TMR1 value for Gate time 1S
#DEFINE		PRE_TMR2		D'06072'		; Pre TMR1 value for Gate time 125mS

#DEFINE		LCD_RS			PORTA,0			; LCD Reg select. 0 = instruction 1 = data
#DEFINE		LCD_E			PORTA,2			; LCD Enable. Latch on -edge.
#DEFINE		MIN_ON			D'1'			; 130 mS 65 x 1	
#DEFINE		MAX_ON			D'9'			; 590 mS 65 x 9

#DEFINE		NEGATIVE		FLAG,0			; 
#DEFINE		LEADING_FLAG	FLAG,1			; 
#DEFINE		LCD_UPDATE		FLAG,2			; Need to update LCD
#DEFINE		WRITE_FLAG		FLAG,3			; Write EEPROM Flag
#DEFINE		PRESSED			FLAG,4			; A valid key press
#DEFINE		EEPROM_UPDATE	FLAG,5			; Indicate EEPROM need to Update
#DEFINE		DIRECT_CNT		FLAG,6			; Indicate direct counting
#DEFINE		TIME_125MS		FLAG,7			; Indicate 125 mS gate time

#DEFINE		PUSH_SW			KEY_SW,0		;
#DEFINE		SUB_SW			KEY_SW,1		;
#DEFINE		ADD_SW			KEY_SW,2		;	
#DEFINE		NOR_REV_SW		KEY_SW,3		;
				
				CBLOCK	0x20				; Bank 0 ram 96 (18 + 16 shared) bytes. 20-7F hex.
				ARG0_3
				ARG0_2						;
				ARG0_1						;
				ARG0_0						;
				
;				ARG1_9						;
				ARG1_8						;
				ARG1_7						;
				ARG1_6						;
				ARG1_5						;
				ARG1_4						;
				ARG1_3						;
				ARG1_2						;
				ARG1_1						;
				ARG1_0						;
				
				COUNT_3						; 
				COUNT_2						; For LCD routines, and delay loop
				COUNT_1						;
				COUNT_0						;
				
				TEMP_3						; For LCD routines
				TEMP_2						; For LCD routines	
				TEMP_1						;
				TEMP_0						;
				
				KEY_SW						;
				SW_PUSH_CNT					;
				SW_PUSH_CNT_OLD				;
					
				ADD_BUFFER					;
				
				OFFSET_3					;
				OFFSET_2					;
				OFFSET_1					;
				OFFSET_0					;
							
				OFFSET_ADR					; Do not change the order
				FLAG						; General Purpose Flags
				
				SB_OFFSET_H					;
				SB_OFFSET_L					;
								
				ENDC
				
				CBLOCK	0x70				; Shared ram 16 bytes (all 4 banks)
				SAVE_W						; Int context is saved here
				SAVE_STATUS					;
				SAVE_PORTB					;
				
				TMR0_H						;
				TMR0_L						;
				
				T1COUNT						;
				EE_TIMER					; EEPROM Update count
				ENDC
				
;*****************************************************************************************
;               MACRO'S
;*****************************************************************************************

#DEFINE         LABEL_OFFSET    (($-TABLE_START)& H'FF')+($ & H'FF00')   
                noexpand

TEXT_STRING     MACRO   NAME,TEXT,TERM  ;       Text string macro.
NAME            EQU     LABEL_OFFSET    ;       Set the label offset.
                expand
                DT TEXT, TERM
                noexpand                ;       Define the text table.
                ENDM

;-----------------------------------------------------------------------------------------

				ORG	RESET_VECTOR

RESET:			CLRF	PORTA				; Cleat PORTA
				CLRF	PORTB				; Clear PORTB
				BSF		STATUS,RP0			; Select Bank 1
				GOTO	CONTINUE			; Jump over interrupt routines & text table.
			
				ORG	INT_VECTOR				; interrupt vector location
				GOTO	INT_SERVICE			;
				
;*****************************************************************************************
;
;               Text table.
;
;               Each string terminates when a 0 is reached.
;               Strings with "" (nothing) terminate at the end of the next string.
;
;*****************************************************************************************

TABLE:			ADDWF	PCL,F				; Jump to character pointed to in W reg.

;				LABLE			STRING					TERMINATION
TABLE_START:
 TEXT_STRING	VER:	,		"FC 0.51"				,0
 TEXT_STRING	MHZ:	,		"MHz"					,0
 TEXT_STRING	GTIME:	,		"GATE TIM"				,0
 TEXT_STRING	T125MS:	,		"E 125 mS"				,0
 TEXT_STRING	T1S:	,		"E 1 Sec "				,0
TABLE_END
#IF ($ > 255)
	MESSG	ERR0R : TABLE OVERFLOWS
#ENDIF
				ORG	TABLE_END
				
CONTINUE:		MOVLW	B'00010000'			; RA4 is I/P, RA3 is Gate (Now Closed)
				MOVWF	TRISA				;
				MOVLW	0x0F				; RB0-RB3 are I/P and RB4-RB7 are O/P
				MOVWF	TRISB				;
				MOVLW	B'10100111'			; Pull Up DIS, INTEDG, T0CS, T0SE, PS2:PS0
;				MOVLW	B'10000000'			;
				MOVWF	OPTION_REG			;
				MOVLW	B'01000000'			; GIE, PEIE, T0IE, INTE, RBIE, T0IF, INTF, RBIF
				MOVWF	INTCON				; (mapped in Both banks)
				MOVLW	B'00000001'			; Enable Timer1 interrupt 
				MOVWF	PIE1				;
				BCF		STATUS,RP0			; Back to Bank0
				MOVLW	B'00000111'			;
				MOVWF	CMCON				;
;				MOVLW	B'00010000'			; Prescale 1:2, Disable Timer1
				MOVLW	B'00000000'			; Prescale 1:1, Disable Timer1
				MOVWF	T1CON				;
				
				CALL	INIT_LCD			; initialise LCD
				MOVLW	0x80				;	
				CALL	LCD_CMD
				MOVLW	VER					;
				CALL	LCD_TEXT			;
				
				MOVLW	0x20				; Starting address of Bank0
				MOVWF	FSR					;
				
CLEAR_BANK0:	MOVLW	D'11'				; Delay loop, and clear all Bank 0 bytes
				CALL	MS_WAIT				;	
				CLRF	INDF				;
				INCF	FSR,W				;
				MOVWF	FSR					;
				SUBLW	0x7F				;
				BTFSC	STATUS,C			;
				GOTO	CLEAR_BANK0			;
					
INITIALISE:		MOVLW	0x04				; Get Side band offsets and last used
				MOVWF	COUNT_1				; Offset location, Gate Time
				MOVLW	OFFSET_ADR			;
				MOVWF	FSR					;
				MOVLW	EE_OFFSET_ADR		;
				CALL	EEPROM_MOVE			;
				MOVLW	B'11000000'			; Clear all flags except TIME_125MS Flag
				ANDWF	FLAG,F				; and DIRECT_CNT				
				CALL	GET_OFFSET			;
				CALL	LCD_CLEAR			; Clear LCD
				
				BSF		LCD_UPDATE			; Go to Update LCD and start the timer 1
				BSF		INTCON,GIE			;
				
MAIN:			BTFSS	LCD_UPDATE			;
				GOTO	SKP_UPDATE			;
				
				BTFSC	DIRECT_CNT			; If direct counting
				GOTO	SKP_CHK_KEYS		; no need to check key status
				
CHK_KEYS:		MOVF	PORTB,W				;
				ANDLW	B'00001110'			;
				MOVWF	KEY_SW				;
				
				ANDLW	B'00000110'			; Check that both sub/add switches are open
				XORLW	B'00000110'			;
				BTFSS	STATUS,Z			; N.
				GOTO	SKP_SUB_IF_VF0		; 
											;
				CALL	NEG_ARG0			; Y. ARG0 = ARG0 - OFFSET
				CALL	ADD_ARG0_OFFSET		;
				GOTO	SKP_ADD				;
				
SKP_SUB_IF_VF0:	BTFSC	SUB_SW				; Is Sub switch closed?
				GOTO	SKP_SUB				; N. Skip		
				
				CALL	SUB_ARG0_OFFSET		; Y. ARG0 = ARG0 - OFFSET
				GOTO	SKP_ADD				; Skip add	
				
SKP_SUB:		BTFSC	ADD_SW				; Is Add switch closed?
				GOTO	SKP_ADD				; N Skip 
				CALL	ADD_ARG0_OFFSET		; Y. Add OFFSET
				
SKP_ADD:		BTFSC	NOR_REV_SW			;
				CALL	ADD_SB_OFFSET		;
				BTFSS	NOR_REV_SW			;
				CALL	SUB_SB_OFFSET		;
				
SKP_CHK_KEYS:	BTFSS	EEPROM_UPDATE		;
				GOTO	SHOW_FREQ			;
				MOVLW	D'2'				;
				MOVWF	COUNT_1				;
				BSF		WRITE_FLAG			;
				MOVLW	OFFSET_ADR			;
				MOVWF	FSR					; Source address
				MOVLW	EE_OFFSET_ADR		; Destination address
				CALL	EEPROM_MOVE			; Write to EEPROM
				
SHOW_FREQ:		CALL	DISPLAY_FREQ		;
;				CALL	CHK_OFFSET_ADR		; Show the last used Offset No
								
START_COUNT:	MOVLW	HIGH(PRE_TMR1+D'12'); Initialise Timer 1 values
				BTFSC	TIME_125MS			;
				MOVLW	HIGH(PRE_TMR2+D'12');
				MOVWF	TMR1H				;
				MOVLW	LOW(PRE_TMR1+D'12')	; 10 = 12 - 2
				BTFSC	TIME_125MS			;
				MOVLW	LOW(PRE_TMR2+D'12')	;
				MOVWF	TMR1L				;
				CLRF	TMR0				;
				CLRF	TMR0_L				;
				CLRF	TMR0_H				;
				MOVLW	D'16'				;
				BTFSC	TIME_125MS			;
				MOVLW	D'2'				;
				MOVWF	T1COUNT				;
				BSF		T1CON,TMR1ON		; Start Timer 1
				BSF		STATUS,RP0			; Select Bank 1
				BSF		GATE_TRIS			; Open Gate, Start counting 				
				BCF		STATUS,RP0			; Back to bank 0 (TMR1 read now TMR1_PRE+2)

SKP_UPDATE:		CALL	CHK_PUSH_BRIEF		;
				BTFSS	PRESSED				;
				GOTO	CHK_PUSH_1S			;
				
				INCF	OFFSET_ADR,F		;
				CALL	CHK_OFFSET_ADR		;

				CALL	GET_OFFSET			;
				GOTO	RESET_COUNT			;

CHK_PUSH_1S:	BTFSS	SW_PUSH_CNT,4		; Has it pressed for 1 sec
				GOTO	SKP_CHK_PUSH		;
				
				COMF	FLAG,W				; Comliment TIME_125MS flag
				ANDLW	B'10000000'			;
				BCF		TIME_125MS			;
				IORWF	FLAG,F				;
				MOVLW	0x80				;
				CALL	LCD_CMD				;
				MOVLW	GTIME				; Show the Gate Time
				CALL	LCD_TEXT			;
#IFDEF	SINGLE_LINE
				MOVLW	0xC0				;
				CALL	LCD_CMD				;
#ENDIF
				MOVLW	T125MS				;
				BTFSS	TIME_125MS			;
				MOVLW	T1S					;
				CALL	LCD_TEXT			;
				BTFSS	PORTB,0				;
				GOTO	$-1					; Wait to release Push button
				CLRF	SW_PUSH_CNT			;
				CALL	LCD_CLEAR			;

RESET_COUNT:	CLRF	EE_TIMER			; Update EEPROM after 2 sec
				CLRF	ARG0_3				;
				CLRF	ARG0_2				;
				CLRF	ARG0_1				;
				CLRF	ARG0_0				;
				BCF		T1CON,TMR1ON		; Stop Timer1
				BSF		STATUS,RP0			; Select Bank1
				BCF		GATE_TRIS			; Stop counting
				BCF		STATUS,RP0			; Back to Bank0
CLR_PULSE:		BSF		GATE     			; _| false inputs
				BCF		GATE				;    |_
				NOP							;
				MOVF	TMR0,W				; actual TMR0 -> W
				SUBWF	ARG0_1,W			;
				BTFSC	STATUS,Z			;
				GOTO	CLR_PULSE			;								
				GOTO	START_COUNT			; Reset counting

SKP_CHK_PUSH:				
				CALL	CHK_TMR0			;		
				GOTO	MAIN				;
				
;################################################################################

CHK_TMR0:		BTFSS	INTCON,T0IF			; Check for timer 0 over flow
				RETURN						;
				BCF		INTCON,T0IF			; Clear the over flow flag
				INCF	TMR0_L,F			; Inc low byte
				BTFSC	STATUS,Z				; 
				INCF	TMR0_H,F			; Inc the high byte
				RETURN
;================================================================================
CHK_OFFSET_ADR:	MOVLW	0xC5				;
				CALL	LCD_CMD				;
				GOTO	DISP_OFFSET			;
;				MOVLW	'<'					;
;				CALL	LCD_CHR				;
;				
;				BCF		DIRECT_CNT			;
;				MOVF	OFFSET_ADR,W		;
;				SUBLW	D'10'				;
;				BTFSC	STATUS,Z			; If OFFSET_ADR = 10 Direct counting
;				BSF		DIRECT_CNT			;
;				BTFSS	STATUS,C			; If OFFSET_ADR > 10, OFFSET_ADR = 0
;				CLRF	OFFSET_ADR			;
;				MOVLW	'0'					;
;				ADDWF	OFFSET_ADR,W		;
;				BTFSC	DIRECT_CNT			;
;				MOVLW	'D'					;
;				CALL	LCD_CHR				;
;				RETURN						;
;================================================================================

CHK_PUSH_BRIEF: MOVLW   SW_PUSH_CNT_OLD		;
CHECK_TIMING:   MOVWF   FSR					;
                MOVF    INDF,W				; Get old button count.
                MOVWF   TEMP_1				; Save the old button state.
                DECF    FSR,F				; Select current count.
                SUBWF   INDF,W				; Has button been released ? (current-old)
                MOVF    INDF,W				;
                INCF    FSR,F				; Select old count.
                MOVWF   INDF				; Copy current state to old state.
                BTFSC   STATUS,C			;
                GOTO    FALSE_FLAG			; N. Return a false flag.
                MOVF    TEMP_1,W			; Y. Was the button on less than min ?
                SUBLW   MIN_ON				; (min on period - old)
                BTFSC   STATUS,C			;
                GOTO    FALSE_FLAG			; Y. Return a false flag.
                MOVF    TEMP_1,W			; N. Was the button on less than max ?
                SUBLW   MAX_ON				; (max on period - old)
RETURN_FLAG:    BSF     PRESSED				; Y. Return true flag.
                BTFSS   STATUS,C			;
FALSE_FLAG:     BCF     PRESSED				; N. Return false flag.
                RETURN						;

;********************************************************************************
;
;      NAME:    DISPLAY_FREQ
;
;   PURPOSE:    Display the frequency in ARG0 on the LCD.
;               Including negative sign, decimal point and leading zero blanking,
;               and MHz text. 
;
;     INPUT:    ARG0 = frequency.
;
;    OUTPUT:    None.
;
; STACK USE:    3
;
;********************************************************************************
#IFNDEF	SINGLE_LINE
DISPLAY_FREQ:	BCF     NEGATIVE			; Clear the negative flag.
				BTFSS   ARG0,7				; Is ARG0 a negative number?
                GOTO    DISP_FREQ			;  N. Just continue.
                BSF     NEGATIVE			;  Y. Set the negative flag.
                CALL    NEG_ARG0			; (1) Convert it to a + number.
DISP_FREQ:      CALL    BIN_TO_DEC			; (2) Convert the number to ASCII decimal.
                CALL    SET_ARG1			; (1) Set FSR = ARG0_8, COUNT_1 = 9.
                MOVLW   H'80'				; Position the cursor, line 1 pos 1.
DISP_POSITION:  CALL    LCD_CMD				; (2)
DISP_LOOP:      MOVF    INDF,W				; Get char pointed to by FSR.
                CALL    LCD_CHR				; (2) Display the char.
                MOVLW   D'7'				;
                SUBWF   COUNT_1,W			;  Y. Is it time to insert a decimal point?
                MOVLW   H'A5'				;
                BTFSC   STATUS,Z			;
                CALL    LCD_CHR				; (2) Y. Display a decimal point.
NO_DP:          INCF    FSR,F				; Inc the pointer.
                DECFSZ  COUNT_1,F			; All done ?
                GOTO    DISP_LOOP			;	N. Continue.
											;
                BTFSC   NEGATIVE			; Was ARG2 negative ?
                CALL    NEG_ARG2			; (1) Y. Return it to a negative number.
											;
                CALL    LCD_SPACE			; (2) Display a space.
              	MOVLW   MHZ					; 
               	CALL	LCD_TEXT			; (3) Display MHz  
                BCF     NEGATIVE			; Clear the negative flag.
                BCF		LCD_UPDATE			; Clear the LCD Update flag
                RETURN						;
 #ELSE
;--------------------------------------------------------------------------------
;		Routine for single line displays
;--------------------------------------------------------------------------------
DISPLAY_FREQ:	BCF     NEGATIVE			; Clear the negative flag.
				BTFSS   ARG0,7				; Is ARG2 a negative number?
                GOTO    DISP_FREQ			;  N. Just continue.
                BSF     NEGATIVE			;  Y. Set the negative flag.
                CALL    NEG_ARG0			; (1) Convert it to a + number.
DISP_FREQ:      CALL    BIN_TO_DEC			; (2) Convert the number to ASCII decimal.
;                CALL    SET_ARG1			; (1) Set FSR = ARG1_8, COUNT_1 = 9.
				MOVLW	ARG1_8				;
				MOVWF	FSR					;
				MOVLW	D'7'				;
				MOVWF	COUNT_1				;
                MOVLW   H'80'				; Position the cursor, line 1 pos 1.
DISP_POSITION:  CALL    LCD_CMD				; (2)
DISP_LOOP:      MOVF    INDF,W				; Get char pointed to by FSR.
                CALL    LCD_CHR				; (2) Display the char.
                MOVLW   D'5'				;
                SUBWF   COUNT_1,W			;  Y. Is it time to insert a decimal point?
                MOVLW   H'A5'				;
                BTFSC   STATUS,Z			;
                CALL    LCD_CHR				; (2) Y. Display a decimal point.
NO_DP:          INCF    FSR,F				; Inc the pointer.
                DECFSZ  COUNT_1,F			; All done ?
                GOTO    DISP_LOOP			;	N. Continue.

				MOVLW	0xC0				;
				CALL	LCD_CMD				;
				MOVLW	D'2'				;
				MOVWF	COUNT_1				;
NEXT_PART:		MOVF	INDF,W				;
				CALL	LCD_CHR				;
				
				INCF	FSR,F				;
				DECFSZ	COUNT_1,F			;
				GOTO	NEXT_PART			;		
											;
                BTFSC   NEGATIVE			; Was ARG2 negative ?
                CALL    NEG_ARG0			; (1) Y. Return it to a negative number.
											;
;                CALL    LCD_SPACE			; (2) Display a space.
              	MOVLW   MHZ					; 
               	CALL	LCD_TEXT			; (3) Display MHz  
                BCF     NEGATIVE			; Clear the negative flag.
                
DISP_OFFSET:	MOVLW	'<'					;
                CALL	LCD_CHR				;
											;
                BCF		DIRECT_CNT			;
				MOVF	OFFSET_ADR,W		;
				SUBLW	D'10'				;
				BTFSC	STATUS,Z			; If OFFSET_ADR = 10 Direct counting
				BSF		DIRECT_CNT			;
				BTFSS	STATUS,C			; If OFFSET_ADR > 10, OFFSET_ADR = 0
				CLRF	OFFSET_ADR			;
				MOVLW	'0'					;
				ADDWF	OFFSET_ADR,W		;
				BTFSC	DIRECT_CNT			;
				MOVLW	'D'					;
				CALL	LCD_CHR				;
											;
				MOVLW	'R'					;
				BTFSC	NOR_REV_SW			;
				MOVLW	'N'					;
				BTFSC	DIRECT_CNT			;
				MOVLW	' '					;
				CALL	LCD_CHR				;
                BCF		LCD_UPDATE			; Clear the LCD Update flag
                RETURN						;
#ENDIF
;********************************************************************************
;
;      NAME:    BIN_TO_DEC
;
;   PURPOSE:    Convert a 32 bit binary number to a 10 digit ASCII decimal number.
;               First the 32 bit binary number is converted to 10 digit decimal.
;               It is then converted to ASCII, including decimal point, leading zero
;               blanking and negative sign (if NEGATIVE flag set), ready for display on
;               the LCD.
;
;     INPUT:    ARG0 = frequency. (+ numbers only )
;
;    OUTPUT:    10 digit ASCII decimal number in ARG1_10..0. (MSD..LSD)
;               (ARG0 unchanged)
;
; VARIABLES:    FSR, COUNT_0, COUNT_1, TEMP_1.
;
; STACK USE:    1
;
;********************************************************************************

;BIN_TO_DEC_FB:  BSF     FULL_BLANKING   	; Enable full leading blanking.
BIN_TO_DEC:     CALL    CLEAR_ARG1      	;
                MOVLW   D'32'              	;
                MOVWF   COUNT_0         	; 32 bits to process.
                GOTO    SHIFT_TO_DEC    	;
                                        	;
LOOP32:         CALL    SET_ARG1        	; Set FSR = ARG1_8, COUNT_1 = 9.
DEC_LOOP:       MOVLW   D'3'               	;
                ADDWF   INDF,W          	; If num + 3 > 7 then num = num + 3.
                MOVWF   TEMP_1          	; Add 3 to num. (adds 6, after next shift)
                BTFSS   TEMP_1,3        	; Is result > 7 ? (bit 3 set)
                GOTO    NO_ADJ          	;
                ADDLW   B'01111000'     	; Y. Move bit 3 to bit 7.
                MOVWF   INDF            	; Put the number back in the buffer.
NO_ADJ:         INCF    FSR,F           	; Point to next byte in buffer.
                DECFSZ  COUNT_1,F 		    ;
                GOTO    DEC_LOOP        	; Loop until all 11 bytes done.
                                        	;
SHIFT_TO_DEC:   RLF     ARG0_3,W        	; Shift 32 bit number left by one bit
                RLF     ARG0_0,F        	; into dec buffer.
                RLF     ARG0_1,F        	;
                RLF     ARG0_2,F        	; After 32 shifts ARG2 unchanged.
                RLF     ARG0_3,F        	; MSD of ARG0	
       
                RLF     ARG1_0,F        	;       LSD of ARG1
                RLF     ARG1_1,F        	;
                RLF     ARG1_2,F        	;
                RLF     ARG1_3,F        	;
                RLF     ARG1_4,F        	;
                RLF     ARG1_5,F        	;
                RLF     ARG1_6,F        	;
                RLF     ARG1_7,F        	;
                RLF     ARG1_8,F        	;
;                RLF     ARG1_9,F        	; MSD of ARG1
                DECFSZ  COUNT_0,F       	;
                GOTO    LOOP32          	; Loop until all 40 bits processed.
                                        	;
                BCF     LEADING_FLAG    	;       Blank leading zero’s until flag is set.
                                        	;
                CALL    SET_ARG1        	; Set FSR = ARG1_8, COUNT_1 = 9.
ASCII_LOOP:		MOVF    INDF,W          	; Get the number.
                BTFSC   STATUS,Z            	; Is it = 0 ?
                GOTO    BLANK           	;
NO_BLANK        ADDLW   H'30'           	; N. Convert it to ASCII.
                MOVWF   INDF            	;
                BSF     LEADING_FLAG    	; Set the leading blank flag,
                GOTO    NUM_DONE        	; following zero’s will not be blanked.
                
BLANK:          BTFSC   LEADING_FLAG    	; Y. Is the leading blank flag set.
                GOTO    NO_BLANK        	;  Y. Convert it to ASCII.
                MOVF    FSR,W           	;  N. Save the location so that a - sign
                MOVWF   COUNT_0         	;  can be inserted.
BLANK_IT:       MOVLW   ' '             	;
                MOVWF   INDF            	;  Blank the byte.
NUM_DONE:       INCF    FSR,F           	; Point to the next byte.
                MOVLW   D'8'               	;
                SUBWF   COUNT_1,W       	; Time to cancel leading blanking?
                BTFSC   STATUS,Z            	;
                BSF     LEADING_FLAG    	; Y. Always display char before DP.
                DECFSZ  COUNT_1,F       	;
                GOTO    ASCII_LOOP      	;
                
				MOVLW	' '					; Blank the last digit, if Gate time is
				BTFSC	TIME_125MS			; 125mS
				MOVWF	ARG1_0				;
				
                BTFSS   NEGATIVE        	; or was this a negative number?
                RETURN						; N. Just return.
NEG_DISP:       MOVF    COUNT_0,W			; Y. Get position of last blank.
                MOVWF   FSR					;
                MOVLW   '-'					; Place - sign in front of the number.
                MOVWF   INDF				;
                RETURN						;				

;--------------------------------------------------------------------------------
SET_ARG1:       MOVLW   ARG1_8				; Set FSR = ARG1_8, COUNT_1 = 9.
                MOVWF   FSR					;
                MOVLW   D'9'				;
                MOVWF   COUNT_1				;
                RETURN						;
;--------------------------------------------------------------------------------
CLEAR_ARG1:		CALL	SET_ARG1			;
CLEAR_BYTES:	CLRF	INDF				;
				INCF	FSR,F				;
				DECFSZ	COUNT_1,F			;
				GOTO	CLEAR_BYTES			;
				RETURN						;
;********************************************************************************
;				ADD_ARG0_OFFSET
;				ARG0 = ARG0 + OFFSET
;********************************************************************************				
ADD_ARG0_OFFSET:MOVF	OFFSET_0,W			;
				ADDWF	ARG0_0,F			;
				MOVF	OFFSET_1,W			;	
				BTFSC	STATUS,C			;
				INCFSZ	OFFSET_1,W			;
											;
				ADDWF	ARG0_1,F			;
				MOVF	OFFSET_2,W			;	
				BTFSC	STATUS,C			;
				INCFSZ	OFFSET_2,W			;
											;
				ADDWF	ARG0_2,F			;
				MOVF	OFFSET_3,W			;
				BTFSC	STATUS,C			;
				INCFSZ	OFFSET_3,W			;
											;
				ADDWF	ARG0_3,F			;
				RETURN						;
;================================================================================
;     NAMES:    SUB_ARG1_OFFSET           ARG1 = ARG1 - OFFSET.
;================================================================================
SUB_ARG0_OFFSET:MOVF    OFFSET_0,W			;
                SUBWF   ARG0_0,F			; Sub the 1st (LSD) bytes.
                MOVF    OFFSET_1,W			;
                BTFSS   STATUS,C			;
                INCFSZ  OFFSET_1,W			; If there was a carry, inc the next byte.
											;
                SUBWF   ARG0_1,F			; Sub the 2nd bytes.
                MOVF    OFFSET_2,W			;
                BTFSS   STATUS,C			;
                INCFSZ  OFFSET_2,W			; If there was a carry, inc the next byte.
											;
                SUBWF   ARG0_2,F			; Sub the 3rd bytes.
                MOVF    OFFSET_3,W			;
                BTFSS   STATUS,C			;
                INCFSZ  OFFSET_3,W			; If there was a carry, inc the next byte.
											;
                SUBWF   ARG0_3,F			; Sub the 4th bytes.
                RETURN
;================================================================================
;		ADD_SB_OFFSET 
;		Add side band offset(SB_OFFSET) to ARG0
;================================================================================
ADD_SB_OFFSET:	CLRF	ADD_BUFFER			;
				BTFSC	SB_OFFSET_H,7		;
				DECF	ADD_BUFFER,F		;
											;
				MOVF	SB_OFFSET_L,W		;
				ADDWF	ARG0_0,F			;
				MOVF	SB_OFFSET_H,W		;
				BTFSC	STATUS,C			;
				INCFSZ	SB_OFFSET_H,W		;
											;
				ADDWF	ARG0_1,F			;
				MOVF	ADD_BUFFER,W		;
				BTFSC	STATUS,C			;
				INCFSZ	ADD_BUFFER,W		;
											;
				ADDWF	ARG0_2,F			;
				MOVF	ADD_BUFFER,W		;
				BTFSC	STATUS,C			;
				INCFSZ	ADD_BUFFER,W		;
											;
				ADDWF	ARG0_3,F			;
				RETURN						;
;================================================================================
;			SUB_SB_OFFSET 
;			Substract side band offset(SB_OFFSET) from ARG0
;================================================================================
SUB_SB_OFFSET:	CLRF	ADD_BUFFER			;
				BTFSC	SB_OFFSET_H,7		;
				DECF	ADD_BUFFER,F		;
				
				MOVF	SB_OFFSET_L,W		;
				SUBWF	ARG0_0,F			;
				MOVF	SB_OFFSET_H,W		;	
				BTFSS	STATUS,C			;
				INCFSZ	SB_OFFSET_H,W		;

				SUBWF	ARG0_1,F			;
				MOVF	ADD_BUFFER,W		;	
				BTFSS	STATUS,C			;
				INCFSZ	ADD_BUFFER,W		;
										
				SUBWF	ARG0_2,F			;
				MOVF	ADD_BUFFER,W		;
				BTFSS	STATUS,C			;
				INCFSZ	ADD_BUFFER,W		;
										
				SUBWF	ARG0_3,F			;
				RETURN						; 				
;********************************************************************************
;				NEG_ARG
;				ARG = -ARG
;*********************************************************************************
NEG_ARG0:       MOVLW   ARG0            	;
NEG_X:          MOVWF   FSR             ;       Place address of ARG in FSR
                COMF    INDF,F          ;       Compliment the arg.
                INCF    FSR,F           ;
                COMF    INDF,F          ;
                INCF    FSR,F           ;
                COMF    INDF,F          ;
                INCF    FSR,F           ;
                COMF    INDF,F          ;
                                        ;
                INCFSZ  INDF,F          ;       Increment the arg.
                RETURN                  ;
                DECF    FSR,F           ;
                INCFSZ  INDF,F          ;
                RETURN                  ;
                DECF    FSR,F           ;
                INCFSZ  INDF,F          ;
                RETURN                  ;
                DECF    FSR,F           ;
                INCF    INDF,F          ;
                RETURN                  ;       ARGx = -ARGx.	
                
;********************************************************************************				
;*																				*		
;*				Interrupt service routine										*
;*																				*
;********************************************************************************			
INT_SERVICE:	MOVWF	SAVE_W				; save off current W register contents
				MOVF	STATUS,W			; move status register into W register	
				MOVWF	SAVE_STATUS			; save off contents of STATUS register	
				
				BCF		STATUS,RP0			;
;				BTFSS	PIR1,TM1IF			; Is it a Timer1 interrupt?
;				GOTO	RBO_INT				; N. Check for RB0 interrupt
				DECFSZ	T1COUNT,F			; Has it reached zero?	
				GOTO	EXIT_TMR1_INT		; No exit from interrupt
				
				BSF		STATUS,RP0			; Change to Bank1
				BCF		GATE_TRIS			; Close the gate now
				BCF		STATUS,RP0			; Back to Bank0 (12 th instruction from int)
				BCF		T1CON,TMR1ON		; Stops the Timer1
				
				CALL	CHK_TMR0			;
				MOVF	TMR0_H,W			; Move the counter value
				MOVWF	ARG0_3				;
				MOVF	TMR0_L,W			;
				MOVWF	ARG0_2				;
				MOVF	TMR0,W				; 
				MOVWF	ARG0_1				;
; -------------------------------------------------------------------------------
; 		Get the precounter vale and store in ARG0_0
; -------------------------------------------------------------------------------
#IF 0
				CLRF	ARG0_0				;
				CLRF	TMR0				; Clear the timer
NEXT_PULSE:		BSF		GATE				; Pulse the counter i/p
				BCF		GATE				;
				DECF	ARG0_0,F			;
				BTFSS	TMR0,0				;
				GOTO	NEXT_PULSE			;
#ELSE
            	CLRF	ARG0_0				;
NEXT_PULSE:     DECF	ARG0_0,F			;
				BSF		GATE     			; _| false imputs
				BCF		GATE				;    |_
				NOP							;
				MOVF	TMR0,W				; actual TMR0 -> W
				SUBWF	ARG0_1,W			;
				BTFSC	STATUS,Z			;
				GOTO	NEXT_PULSE			;
				BCF		INTCON,T0IF			; Clear Timer 0 interrupt flag
#ENDIF
				BTFSS	TIME_125MS			; Is the gate time 125mS
				GOTO	SKP_MULT_8			; N
				MOVLW	D'3'				; Y. Multiply by 8
				MOVWF	COUNT_1				; 
				
MULT_CONT:		BCF		STATUS,C			; Multiply by 8
				RLF		ARG0_0,F			;
				RLF		ARG0_1,F			;
				RLF		ARG0_2,F			;
				RLF		ARG0_3,F			;
				DECFSZ	COUNT_1,F			;
				GOTO	MULT_CONT			;
SKP_MULT_8:									;
				BSF		LCD_UPDATE			; Set flag to LCD Update
EXIT_TMR1_INT:	BCF		PIR1,TMR1IF			; Clear Timer 1 Int Flag

				
				
INC_PUSH_SW:	INCF	SW_PUSH_CNT,W		; Inc SW_PUSH_CNT, but not more than 16
				BTFSS	SW_PUSH_CNT,4		;
				MOVWF	SW_PUSH_CNT			;
				BTFSC	PORTB,0				;
				CLRF	SW_PUSH_CNT			;			
INC_EE_TIMER:	INCFSZ	EE_TIMER,W			; Inc EE_TIMER, but not past 255.
				MOVWF	EE_TIMER			;
				SUBLW	D'32'				; Is it 32 (i.e. 2 sec)?
				BTFSC	STATUS,Z			;
				BSF		EEPROM_UPDATE		; Y. Update eeprom
EXIT_INTERRUPT:	
				MOVF	SAVE_STATUS,W		; retrieve copy of STATUS register
				MOVWF	STATUS				; restore pre-isr STATUS register contents
				SWAPF	SAVE_W,F			; restore pre-isr W register contents
				SWAPF	SAVE_W,W			;
				RETFIE						; return from interrupt	
				
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#INCLUDE		<lcd.asm>
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

GET_OFFSET:		BTFSC	DIRECT_CNT			; If direct counting, nothing to do
				RETURN						;
				BCF		WRITE_FLAG			;
				MOVLW	0x04				;
				MOVWF	COUNT_1				; No. of bytes to move
				MOVLW	OFFSET				;
				MOVWF	FSR					;
				BCF		STATUS,C			;
				RLF		OFFSET_ADR,F		;
				RLF		OFFSET_ADR,W		;
				RRF		OFFSET_ADR,F		; W = OFFSET X 4
				ADDLW	EE_MEM_START		; W = OFFSET location
				GOTO	EEPROM_MOVE			; Return via goto
				
				
;********************************************************************************
;
;      NAME:    EEPROM_MOVE
;
;   PURPOSE:    Move several bytes, between RAM and EEPROM, or EEPROM and RAM.
;               If the RAM and EEPROM data are the same, the EEPROM is not reprogrammed.
;               Assumes the number of bytes to move COUNT_1 has been set,
;               and the RAM source/destination address FSR has been set,
;               and the EEPROM source/destination address is in W,
;               Setting the WRITE_FLAG before calling EEPROM_MOVE indicates that the
;               direction will be from RAM to EEPROM, otherwise it will be EEPROM to RAM.
;
;     INPUT:    WRITE_FLAG = direction. (Set = RAM to EEPROM)
;                  COUNT_1 = bytes to move.
;                      FSR = RAM address.
;                        W = EEPROM address.
;
;    OUTPUT:    None.
;
; VARIABLES:    FSR,  COUNT_1.
;
; STACK USE:    2
;
;********************************************************************************

EEPROM_MOVE:    BSF     STATUS,RP0      ; Select bank 1 for EEPROM reg access.
                MOVWF   EEADR           ; Save the EEPROM address.
                BCF     STATUS,RP0      ; Return to bank 0 for port access.

EEPROM_LOOP:    BTFSC   WRITE_FLAG      ; Do we want to write ? (WRITE_FLAG set)
                CALL    WRITE_EEPROM    ; (2) Y. Write a byte from RAM to EEPROM.
                CALL    READ_EEPROM     ; (1) Get a byte from EEPROM.
                MOVWF   INDF            ;     Place it in RAM.
                BSF     STATUS,RP0		;     Select bank 1 for EEPROM reg access.
                INCF    EEADR,F         ;     Inc the EEPROM address.
                BCF     STATUS,RP0      ;     Return to bank 0 for port access.
                INCF    FSR,F           ;     Inc the RAM address.
                DECFSZ  COUNT_1,F       ;
                GOTO    EEPROM_LOOP     ;     Loop until all bytes moved.
                BCF     WRITE_FLAG      ;
                RETURN                  ;
;********************************************************************************
;
;      NAME:    WRITE_EEPROM
;
;   PURPOSE:    Write the byte of RAM pointed to by FSR, to the EEPROM address pointed to 
;				by EEADR.If the RAM and EEPROM data are the same, the EEPROM is not 
;				reprogrammed. Assumes the source address FSR has been set, and the 
;				destination address EEADR has been set.
;     INPUT:    FSR, EEADR(Bank 1).
; VARIABLES:    FSR.
;
; STACK USE:    1
;********************************************************************************
WRITE_EEPROM:   CALL    READ_EEPROM     ; (1) Get the current byte from EEPROM.
                SUBWF   INDF,W          ;
                BTFSC   STATUS,Z        ; Is it the same as it is in RAM ?
                GOTO    NO_CHANGE       ;
                MOVF    INDF,W          ; N. Get the data byte from RAM.
                BCF     PIR1,EEIF       ;  Clear the EEPROM write done flag.
                BSF     STATUS,RP0      ;  Select bank 1 for EEPROM reg access.
                MOVWF   EEDATA          ;  Put it in the buffer.
                BSF     EECON1,WREN     ;  Enable EEPROM write.
                BCF     INTCON,GIE      ;  Disable interrupts.
                MOVLW   H'55'           ;  Perform required safety steps.
                MOVWF   EECON2          ;
                MOVLW   H'AA'           ;
                MOVWF   EECON2          ;
                BSF     EECON1,WR       ;  Begin the write.
                BSF     INTCON,GIE      ;  Enable interrupts.
                BCF     STATUS,RP0      ;  Return to bank 0 for port access.
WRITE_LOOP:     BTFSS   PIR1,EEIF       ;  Wait until the write is complete.
                GOTO    WRITE_LOOP      ;
NO_CHANGE:      ;BCF     EEPROM_UPDATE  ; Y. Clear the update required flag.
                RETURN                  ;

;********************************************************************************
;
;      NAME:    READ_EEPROM
;
;   PURPOSE:    Read the byte of EEPROM data pointed to by EEADR.
;               Assumes the read address EEADR has been set.
;
;     INPUT:    EEADR.
;
;    OUTPUT:    W = The byte read from EEPROM.
;
; VARIABLES:    None.
;
;********************************************************************************
READ_EEPROM:    BSF     STATUS,RP0		;       Select bank 1 for EEPROM reg access.
                BSF     EECON1,RD		;       Perform an EEPROM read.
                MOVF    EEDATA,W		;       Get the read byte.
                BCF     STATUS,RP0		;       Return to bank 0 for port access.
                RETURN					;

;********************************************************************************
;                       EEPROM VARIABLE MACROs
;********************************************************************************
VAR_1           MACRO   NAME, VAL       ;       1 Byte variable definition.
NAME            EQU $ - EE_VAR_START    ;
                DE (VAL) & H'FF'        ;
                ENDM                    ;

VAR_2           MACRO   NAME, VAL       ;       2 Byte variable definition.
NAME            EQU $ - EE_VAR_START    ;
                DE (VAL>>8) & H'FF'     ;
                DE (VAL) & H'FF'        ;
                ENDM                    ;

VAR_4           MACRO   NAME, VAL       ;       4 Byte variable definition.
NAME            EQU $ - EE_VAR_START    ;
                DE (VAL>>D'24') & H'FF' ;
                DE (VAL>>D'16') & H'FF' ;
                DE (VAL>>8) & H'FF'     ;
                DE (VAL) & H'FF'        ;
                ENDM                    ;

;********************************************************************************
;                       EEPROM variable definitions.
;********************************************************************************

 ORG     H'2100'

EE_VAR_START
; size          name                          value       	comment
 VAR_1          EE_OFFSET_ADR:			, D'10'				; Not set more than 10
 VAR_1			EE_FLAG					, 0x40				; Direct Counting
 VAR_2          EE_SB_OFFSET:			, D'1500'			; Side band offset
; Do not change order of above registers

EE_MEM_START    EQU $ - EE_VAR_START
 VAR_4          OFFSET_FREQ0:			,  D'00455000'		; Offsets
 VAR_4          OFFSET_FREQ1:			,  D'04915000'
 VAR_4          OFFSET_FREQ2:			,  D'09000000'
 VAR_4          OFFSET_FREQ3:			,  D'10000000'
 VAR_4          OFFSET_FREQ4:			,  D'10700000'
 VAR_4          OFFSET_FREQ5:			,  D'04333000'
 VAR_4          OFFSET_FREQ6:			,  D'10245000'
 VAR_4          OFFSET_FREQ7:			,  D'10000000'
 VAR_4          OFFSET_FREQ8:			,  D'10000000'
 VAR_4          OFFSET_FREQ9:			,  D'10000000'
				END
			
			
	
