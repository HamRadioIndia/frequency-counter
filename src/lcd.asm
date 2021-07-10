

#DEFINE LCD_UPPER
#DEFINE LCD_DATA_PORT PORTB

;*****************************************************************************************
;
;      NAME:    LCD_TEXT
;
;   PURPOSE:    Display on the LCD, the string of characters pointed to by W.
;               The last char of the string must be a 0.
;
;     INPUT:    W = String offset address. (offset from the base of the text table)
;
;    OUTPUT:    None.
;
; VARIABLES:    TEMP1, TEMP_2, COUNT_2.
;
; STACK USE:    2
;
;*****************************************************************************************

LCD_TEXT:		MOVWF   TEMP_3          ;       Save the text start address.
                CALL    TABLE         	; (1)
                ANDLW   H'FF'           ;
                BTFSC   STATUS,Z        ;       At end of message ? (0 returned at end)
				RETURN
                CALL    LCD_CHR         ; (2)   N. Display character.
                INCF    TEMP_3,W        ;          Point to next character.
				GOTO	LCD_TEXT		;

;*****************************************************************************************
;
;     NAMES:    LCD_CLEAR       Clear the display and home the cursor.
;               LCD_SPACES      Display W spaces.
;               LCD_16_SPACES   Display 16 spaces.
;                               (if RECALL_CHK flag is set, no data is sent to the LCD)
;
; VARIABLES:    TEMP_2, COUNT_2.
;
; STACK USE:    2
;
;*****************************************************************************************
;
;     NAMES:    WAIT_8MS        Do nothing for 8 mS.
;               MS_WAIT         W = The number of mS to wait.
;
; VARIABLES:    COUNT_2.
;
; STACK USE:    1
;
;*****************************************************************************************

LCD_16_SPACES:  MOVLW   D'16'
LCD_SPACES:     MOVWF   COUNT_2         ;       Save the number of spaces.
SPACE_LOOP:     CALL    LCD_SPACE       ; (2)   Display a space.
                DECFSZ  COUNT_2,F       ;
                GOTO    SPACE_LOOP      ;       Loop until count = 0.
                RETURN                  ;

LCD_CLEAR:      MOVLW   H'01'           ;       Clear the display. Then wait 8mS.
                CALL    LCD_CMD         ; (2)
WAIT_8MS:       MOVLW   D'8'            ;       Set the outer loop to 8mS.
MS_WAIT:        MOVWF   COUNT_2         ;       Save the number of mS delay.
OUTER_LOOP:     CLRW                    ;       Set the wait loop to 256.
                CALL    WAIT_LOOP       ; (1)   256 loops x 4 x 1uS = 1.024mS.
                DECFSZ  COUNT_2,F       ;       Dec the mS counter.
                GOTO    OUTER_LOOP      ;
                RETURN                  ;

;*****************************************************************************************
;
;     NAMES:    LCD_HOME        Home the cursor.
;               LCD_SPACE       Display a space.
;               LCD_CMD         Send the command byte in W to the LCD.
;               LCD_CHR         Send the data byte in W to the LCD.
;
; VARIABLES:    TEMP_2
;
; STACK USE:    1
;
;*****************************************************************************************

LCD_HOME:		MOVLW	H'80'			; Position the cursor, line 1 pos 1.
LCD_CMD:        BCF     LCD_RS          ; Select the instruction register.
                GOTO    LCD_BYTE        ; Send the byte to the LCD.

LCD_SPACE:      MOVLW   " "				; Display a space.
LCD_CHR:        BSF     LCD_RS			; Select display data register.
LCD_BYTE:       MOVWF   TEMP_2			; Save the byte to send to the LCD.
#IFDEF	LCD_UPPER	
				MOVLW	0x0F			; Clear high nibble of LCD data port
				ANDWF	LCD_DATA_PORT,F	; 
				MOVF	TEMP_2,W		; Send high nibble
				ANDLW	0xF0			; 
				IORWF	LCD_DATA_PORT,F	;
				CALL	PULSE_LCD_E		;
				MOVLW	0x0F			; Clear high nibble of LCD data port
				ANDWF	LCD_DATA_PORT,F	;
				SWAPF	TEMP_2,W		; Send lower nibble
				ANDLW	0xF0			;
				IORWF	LCD_DATA_PORT,F	;
#ELSE	
				MOVLW	0xF0			; Clear low nibble of LCD data port
				ANDWF	LCD_DATA_PORT,F	; 
				SWAPF	TEMP_2,W		; Send high nibble
				ANDLW	0x0F			; 
				IORWF	LCD_DATA_PORT,F	;
				CALL	PULSE_LCD_E		;
				MOVLW	0xF0			; Clear low nibble of LCD data port
				ANDWF	LCD_DATA_PORT,F	;
				MOVF	TEMP_2,W		; Send lower nibble
				ANDLW	0x0F			;
				IORWF	LCD_DATA_PORT,F	;
#ENDIF
				
;#IF LCD_UPPER
;               BCF     INTCON,GIE      ;       Disable interrupts.
;                CALL    SEND_NIBBLE     ; (1)   Send the high nibble to the LCD.
;                SWAPF   TEMP_2,F        ;
;                CALL    SEND_NIBBLE     ; (1)   Send the low nibble to the LCD.
;               BSF     INTCON,GIE		;       Enable interrupts.
;#ELSE
;				SWAPF	TEMP_2,F		; send high niblle to LCD
;				CALL	SEND_NIBBLE		;
;				SWAPF	TEMP_2,F		; send low nibble to LCD
;				CALL	SEND_NIBBLE		;
;#ENDIF
;                RETURN                  ;
;
;-----------------------------------------------------------------------------------------
;#IF	LCD_UPPER
;SEND_NIBBLE:    MOVLW   B'00001111'     ;
;                ANDWF   PORTB,F         ;       Clear the high nibble of port B.
;                MOVF    TEMP_2,W        ;       Get the byte of data.
;                ANDLW   B'11110000'     ;       Keep only the high nibble.
;                IORWF   PORTB,F         ;       Send the nibble to port B.
;#ELSE
;SEND_NIBBLE:    MOVLW   B'11110000'     ;
;                ANDWF   PORTB,F         ;       Clear the low nibble of port B.
;                MOVF    TEMP_2,W        ;       Get the byte of data.
;                ANDLW   B'00001111'     ;       Keep only the low nibble.
;                IORWF   PORTB,F         ;       Send the nibble to port B
;#ENDIF
PULSE_LCD_E:    MOVLW   D'256'-D'50'	;       Set the wait loop to 200uS.
                BSF     LCD_E           ;       Pulse the LCD_E lead low.
                GOTO	$+1				;
                NOP						;
                BCF     LCD_E           ;
WAIT_LOOP:      ADDLW   1               ;       1 CLK cycles \
                BTFSS   STATUS,Z        ;       1     "       > 4 CLK’s x 1uS
                GOTO    WAIT_LOOP       ;       2     "      /
                RETURN                  ;

;*****************************************************************************************
;
;      NAME:    INIT_LCD
;
;   PURPOSE:    Initialise the 16 x 2 liquid crystal display.
;               The LCD controller chip must be equivalent to the HITACHI 44780.
;
;     INPUT:    None.
;
;    OUTPUT:    None.
;
; VARIABLES:    COUNT_2.
;
; STACK USE:    3
;
;*****************************************************************************************

INIT_LCD:		CALL    LCD_RESET       ; (2)   Reset the LCD.
                CALL    LCD_RESET       ; (2)   Reset the LCD.
                CALL    LCD_RESET       ; (2)   Reset the LCD.
#IFDEF	LCD_UPPER   
				MOVLW	0x0F			;
				ANDWF	LCD_DATA_PORT,F	; LCD 4 bit mode command.
				MOVLW	B'00100000'		;
				IORWF	LCD_DATA_PORT,F	; 
#ELSE
				MOVLW	0xF0			;
				ANDWF	LCD_DATA_PORT,F	; LCD 4 bit mode command.
				MOVLW	B'00000010'		;
				IORWF	LCD_DATA_PORT,F	; 
#ENDIF
                CALL    PULSE_LCD_E     ; (1)   Pulse the LCD E lead.
                MOVLW   B'00101000'     ;       Set LCD to 2 line, 5x7 dot.
                CALL    LCD_CMD         ; (2)
                MOVLW   B'00001000'     ;       Turn the display off.
                CALL    LCD_CMD         ; (2)
                CALL    LCD_CLEAR       ; (3)   Clear the display.
                MOVLW   B'00000110'     ;       Cursor increments, no display shift.
                CALL    LCD_CMD         ; (2)
LCD_NORM:       MOVLW   B'00001100'     ;       Turn the display on.
                GOTO    LCD_CMD         ; (2)   Return via "GOTO".

;-----------------------------------------------------------------------------------------

LCD_RESET:      BCF		LCD_RS			;
				BCF		LCD_E			;
#IFDEF	LCD_UPPER
				MOVLW	0x0F			;
				ANDWF	LCD_DATA_PORT,F	; Clear high nibble				
				MOVLW   B'00110000'     ; LCD reset command.
                IORWF   LCD_DATA_PORT,F	;
#ELSE
				MOVLW	0xF0			;
				ANDWF	LCD_DATA_PORT,F	; Clear lower nibble
				MOVLW	B'00000011'		; LCD reset command.
				IORWF	LCD_DATA_PORT,F	;
#ENDIF				
                CALL    PULSE_LCD_E     ; (1)   Pulse the LCD E lead low.
                GOTO    WAIT_8MS        ; (1)   Return via "GOTO".
;------------------------------------------------------------------------------------------
