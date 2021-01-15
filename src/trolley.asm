;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                Trolley Problem
;                            (c)2021, Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the tokenization of the following BASIC program, which
; runs this game
;     42 SYS4110
* = $1001
BASIC:      .byte $0b,$10,$2a,$00,$9e,$34,$31,$31
            .byte $30,$00,$00,$00,$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants - Game Configuration
SCRCOLVAL   = 142               ; Screen color
UP          = $01               ; Directional constants
RIGHT       = $02               ; ,,
DOWN        = $03               ; ,,
LEFT        = $04               ; ,,
FIRE        = $05               ; Joystick fire button pressed

; Score constants
PICKUP      = 20                ; Score when a passenger is picked up
DROPOFF     = 100               ; Score when a passenger is dropped off
TIME_BONUS  = 1000              ; Starting time bonus

; Character constants
TRACK_COL   = 1                 ; Track color

; System Resources
CINV        = $0314             ; ISR vector
;NMINV       = $0318             ; Release NMI vector
NMINV       = $fffe             ; Development NMI non-vector
SCREEN      = $1e00             ; Screen character memory (unexpanded)
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eabf             ; System ISR   
BASRND      = $e094             ; Routine for BASIC's RND() function
RNDNUM      = $8d               ; Result storage location for RND()
VICCR5      = $9005             ; Character map register
VOICEH      = $900c             ; High sound register
VOICEM      = $900b             ; Mid sound register
VOICEL      = $900a             ; Low sound register
FX_VOICE    = $900c             ; Sound effects voice
NOISE       = $900d             ; Noise register
VOLUME      = $900e             ; Sound volume register/aux color
SCRCOL      = $900f             ; Screen color
VIA1DD      = $9113             ; Data direction register for joystick
VIA1PA      = $9111             ; Joystick port (up, down, left, fire)
VIA2DD      = $9122             ; Data direction register for joystick
VIA2PB      = $9120             ; Joystick port (for right)
CLSR        = $e55f             ; Clear screen/home
HOME        = $e581             ; Home text
PRTSTR      = $cb1e             ; Print from data (Y,A)
CASECT      = $0291             ; Disable Commodore case
PRTFIX      = $ddcd             ; Decimal display routine (A,X)
CHROUT      = $ffd2             ; Output one character
TIME_L      = $a2               ; Jiffy counter low
TIME_M      = $a1               ; Jiffy counter middle  

; Game Memory
LEVEL       = $f7               ; Level pointer (2 bytes)
TROLLEY     = $f9               ; Trolley location (2 bytes)
CURSOR      = $f9               ; [LEVEL DRAW] Track cursor
SWITCH      = $fb               ; Current switch location (2 bytes)
UNDER       = $fd               ; Track character under trolley
COLUMN      = $fd               ; [LEVEL DRAW] Column number
JOYREG      = $fe               ; Joystick register storage
TRMASK      = $fe               ; [LEVEL DRAW] Track mask
SCORE       = $45               ; Score
HISCORE     = $47               ; High score
PLAY_FL     = $49               ; Play flag (determines behavior of ISR)

; Sound Memory
FX_REG      = $033c             ; Current effect frequency register
FX_LEN      = $033d             ; Current effect length
FX_COUNT    = $033e             ; Effect countdown for current effect
FX_DIR      = $033f             ; Effect direction ($00 = left, $80 = right)
FX_SPEED    = $0340             ; Effect countdown reset value

; Music Memory
THEME       = $0341             ; Music shift register theme (2 bytes)
TEMPO       = $0343             ; Tempo (lower numbers are faster)
MUCD        = $0344             ; Tempo countdown
MUSIC_F     = $0345             ; Music is playing
FADE        = $0346             ; Fadeout volume

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Startup:    jsr SetupHW         ; Set up hardware           
            lda #$00            ; Initialize high score
            sta HISCORE         ; ,,
            sta HISCORE+1       ; ,,
           
; Welcome Screen
; Show intro, set level, then show manual page      
Welcome:    clc                 ; Disable the playing flag, in case we get
            ror PLAY_FL         ;   here from the NMI
            lda #$00            ; Shut off all sounds
            sta VOICEL          ; ,,
            sta VOICEM          ; ,,
            sta VOICEH          ; ,,
            sta NOISE           ; ,,
            lda #<Intro         ; Show Intro
            ldy #>Intro         ; ,,
            jsr PRTSTR          ; ,,
            jsr Wait4Fire       ; Wait for Fire to be pressed
            lda #<Manual        ; Show the game manual
            ldy #>Manual        ; ,,
            jsr PRTSTR          ; ,,

; Start a new game            
Start:      jsr Wait4Fire
            jsr InitGame        ; Initialize game and draw first level

; Main loop            
Main:       jsr Joystick        ; Read the joystick
            bne Main            ; If no movement do nothing
ch_fire:    cmp #FIRE           ; Has fire been pressed?
            bne handle_dir      ; If not, handle a direction
            jsr TogSwitch       ; If fire is pressed, toggle current switch
debounce:   jsr Joystick        ; Debounce fire button:
            cmp #FIRE           ;   Check joystick for fire button release
            beq debounce        ;   before returning to Main
            bne Main            ;   ,,
handle_dir: ; TODO Handle controls
            jmp Main
            
 ; Custom ISR for music player and day counting
ISR:        bit PLAY_FL         ; If the game is over, don't do anything
            bpl isr_r           ;   in this routine
            jsr FXService       ; Service sound effect
            jsr MusService      ; Service shift register music
isr_r:      jmp IRQ           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME MECHANICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Toggle Current Switch
TogSwitch:  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wait for Fire
Wait4Fire:  jsr Joystick        ; Wait for fire to be released
            cpx #FIRE           ; ,,
            beq Wait4Fire       ; ,,
wait_fire:  jsr Joystick        ; Wait for the fire button
            cpx #FIRE           ; ,,
            bne wait_fire       ; ,,
            rts
            
; Read the Joystick
; Return the direction in A
; 1=North, 2=East, 3=South, 4=West, 5=Fire, 0=None
Joystick:   lda VIA1PA          ; Read VIA1 port
            and #$3c            ; Keep track of bits 2,3,4,5
            sta JOYREG
            lda VIA2PB          ; Combine with read of bit 7
            and #$80            ;   from VIA2-B
            ora JOYREG
            eor #$bc            ; Flip each joystick bit in the
                                ;   combined read byte, so that
                                ;   on = 1
            sta JOYREG          ; Store temporary direction
            ldx #$05            ; Check five directions (5=fire)
-loop:      lda JoyTable,x      ; Is the joystick pointed in the
            bit JOYREG          ;   direction indexed by X?
            bne found_dir       ; If so, set that as the joystick direction
            dex                 ; Loop back until found, or 0
            bne loop            ; ,,
found_dir:  txa
            rts         

; Delay A Jiffies
Delay:      clc
            adc TIME_L
-loop:      cmp TIME_L
            bne loop
            rts  

; Add Score
; Add score in Accumulator, then fall through to show score
AddScore:   clc
            adc SCORE
            sta SCORE
            bcc check_hi
            inc SCORE+1
check_hi:   lda HISCORE+1       ; Is the last score greater than
            cmp SCORE+1         ;   the high score?
            bcc new_hs          ;   ,,
            bne ShowScore       ;   ,,
            lda SCORE           ;   ,,
            cmp HISCORE         ;   ,,
            bcc ShowScore       ;   ,,
new_hs:     lda SCORE           ; A new high score has been
            sta HISCORE         ; achived; update high score
            lda SCORE+1         ; ,,
            sta HISCORE+1       ; ,,                
            ; Fall through to ShowScore

ShowScore:  rts

; Reset Cursor
; To the top of the board
ResetCur:   lda #$5b            ; Set starting point for level
            sta CURSOR          ; ,,
            lda #>SCREEN        ; ,,
            sta CURSOR+1        ; ,,
            ldx #$00
            rts

; Move Cursor
; In the direction specified by X
MoveCursor: tya                 ; Preserve Y
            pha                 ; ,,
            lda DirTable,x
            pha
            and #%10000000      ; Extend the sign to 16 bits
            beq sign            ; ,,
            ora #$ff            ; ,,
sign:       tay                 ; ,,
            pla                 ; Get original direction value
            clc                 ; Add (or subtract) the signed direction
            adc CURSOR          ;   and update CURSOR
            sta CURSOR          ;   ,,
            tya                 ;   ,,
            adc CURSOR+1        ;   ,,
            sta CURSOR+1        ;   ,,
            pla                 ; Restore Y
            tay                 ; ,,
            rts   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup Hardware
SetupHW:    lda #$ff            ; Set custom character location
            sta VICCR5          ; ,,
            lda #SCRCOLVAL      ; Set background color
            sta SCRCOL          ; ,,
            lda #$7f            ; Set DDR to read East
            sta VIA2DD          ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            lda #<Welcome       ; Install the custom NMI (restart)
            sta NMINV           ; ,, 
            lda #>Welcome       ; ,,
            sta NMINV+1         ; ,,
            sei                 ; Install the custom ISR
            lda #<ISR           ; ,,
            sta CINV            ; ,,
            lda #>ISR           ; ,,
            sta CINV+1          ; ,,
            cli                 ; ,,
            rts

InitGame:   lda #<Levels        ; Set the starting level
            sta LEVEL           ; ,,
            lda #>Levels        ; ,,
            sta LEVEL+1         ; ,,
            lda #$00            ; Set the theme
            sta FX_LEN          ; and effects length
            jsr SelTheme        ; ,,
            lda #$07            ; Set tempo
            sta TEMPO           ; ,,
            jsr MPlay           ; Start the music
            sec                 ; Set the game play flag
            ror PLAY_FL         ; ,,
            lda #$18            ; Set volume and aux color
            sta VOLUME          ; ,,
            ; Fall through to DrawLevel

DrawLevel:  jsr CLSR            ; Clear screen
            jsr ResetCur        ; Reset cursor to top left
            ldy #$ff            ; Initialize level data index
next_row:   lda #$02            ; Reset column number
            sta COLUMN          ; ,,
next_byte:  iny                 ; Prepare for the next byte
            lda #$80            ; Reset the track mask to bit 7
            sta TRMASK          ; ,,
            lda (LEVEL),y       ; Get level data byte
next_bit:   bit TRMASK          ; Is the track mask bit set?
            beq adv_cursor
            pha 
            lda #$00            ; Placeholder
            sta (CURSOR,x)      ; Store placeholder at cursor position
            pla 
adv_cursor: inc CURSOR
            bne shift_m
            inc CURSOR+1
shift_m:    lsr TRMASK          ; Shift the mask right
            bcc next_bit        ; Carry is set when bit 0 is shifted off
            dec COLUMN          ; Advance to next column
            bne next_byte       ;
            lda #$06            ; Advance cursor to the next row
            clc                 ; ,,
            adc CURSOR          ; ,,
            sta CURSOR          ; ,,
            bcc check_end       ; ,,
            inc CURSOR+1        ; ,,
check_end:  cpy #$1f            ; Has all the track been drawn?
            bne next_row        ; If not, start a new row
            ; Fall through to GenTrack

; Generate Track
; Scan the screen for the Track Placeholder character and replace it with
; track, based on 
GenTrack:   jsr ResetCur        ; Reset cursor to top left
-loop:      ldx #$00
            lda (CURSOR,x)      ; Get character at cursor
            bne next_cell       ; If not a track placeholder, go to next cell
            stx TRMASK          ; Initialize occupied track mask
            ldx #$04            ; Look each direction in the order W/S/E/N
            lda #$08            ; Respectively having values of    8/4/2/1
next_dir:   pha                 ; Preserve the direction value while using A
            lda CURSOR          ; Preserve the cursor while compiling a list
            pha                 ;   of adjacent cells occupied by track
            lda CURSOR+1        ;   ,,
            pha                 ;   ,,
            jsr MoveCursor      ; Look in the next cardinal direction in order
            ldy #$00            ; ,,
            lda (CURSOR),y      ; ,,
            tay                 ; Y is now the value of the adjacent cell
            pla                 ; Put the cursor back to the current cell
            sta CURSOR+1        ; ,,
            pla                 ; ,,
            sta CURSOR          ; ,,
            pla                 ; Restore the direction value
            pha                 ;   while keeping it on the stack
            cpy #$20            ; Unoccupied if there's space here
            beq unoccupied
            ora TRMASK          ; Set the occupied track mask with the current
            sta TRMASK          ;   value, if adjacent cell is occupied
unoccupied: pla                 ; Get the direction valu back for the shift
            lsr                 ; Shift the cell value right
            dex                 ; Decrement the direction index
            bne next_dir        ; Check next direction if more to do
            lda TRMASK          ; Once the 4 adjacent cells are examined,
            ora #$20            ;   add a character offset and populate the
            sta (CURSOR,x)      ;   current cell
next_cell:  inc CURSOR          ; Advance the cursor to the next cell
            bne check_end2      ; ,,
            inc CURSOR+1        ; ,,
check_end2: lda CURSOR+1        ; Have we reached the end of the screen?
            cmp #$20            ; ,,
            bne loop            ; If more to do, go back for the next cell
            ; Fall through to AddPieces

; Add Pieces to Board
; Passengers and Switches
AddPieces:  rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; EFFECTS SERVICE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
; Play Next Sound Effect
; Rotates the 8-bit sound effect register and
; plays the pitch      
FXService:  lda FX_LEN          ; Has the sound been launched?
            beq fx_end          ; If unlaunched, kill voice and return
            dec FX_LEN          ; Decrement both length
            dec FX_COUNT        ;   and countdown
            bne fx_r            ; If countdown has elapsed,
            lda FX_SPEED        ;   reset it with the current effect speed
            sta FX_COUNT        ;   ,,
            bit FX_DIR          ; Rotate the register, based on the direction
            bmi fx_right        ;   specified by the direction flag
fx_left:    lda #$00
            asl FX_REG          ; Rotate the register left if flag = $00
            adc FX_REG          ; If carry was set, set bit 0
            jmp fx_update       ; Update and play the new frequency
fx_right:   lsr FX_REG          ; Rotate the register right if flag = $80
            lda FX_REG          ; ,,
            bcc fx_play         ; If carry was set, set bit 7
            lda #%10000000      ; ,,
            ora FX_REG          ; ,,
fx_update:  sta FX_REG          ; ,,
fx_play:    ora #$80            ; Gate the high voice
fx_end:     sta FX_VOICE        ; ,,
fx_r:       rts 

; Wait for End
; Do nothing until the current effect ends
FXWait:     lda FX_LEN
            bne FXWait
            beq fx_end
        
; Launch Sound Effect
; Preparations
;     A - The sound effect index
FXLaunch:   sei                 ; Don't play anything while setting up
            asl                 ; Each effect has two bytes in the table
            tax
            lda FXTable,x       ; Get the register byte
            sta FX_DIR          ; Set the direction (only bit 7 will be used)
            and #$7f            ; Mask away the direction bit to get the 7-bit
            sta FX_REG          ;   frequency
            lda FXTable+1,x     ; Get the length byte
            tax                 ;   and preserve it
            and #$f0            ; Length is in bits 4-7 of the length byte
            sta FX_LEN          ;  ,,
            txa
            and #$0f            ; Speed (jiffies per rotation) is in the low
            sta FX_SPEED        ;   nybble of the length byte
            sta FX_COUNT        ;   ,,
            cli                 ; Go! 
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MUSIC SERVICE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
; Start the music player
MPlay:      sec
            ror MUSIC_F
            lda #$00
            sta FADE
            lda TEMPO
            sta MUCD
            rts
    
; Stop the music player
MStop:      lda #$00
            sta VOICEL
            sta MUSIC_F
            rts
            
; Select Music
; Set a musical theme 
;
; Preparations
;     A is the theme index
SelTheme:   asl                 ; Multiply level by 2 for theme index
            tax
            lda Themes,X        ; Set the musical theme
            sta THEME 
            lda Themes+1,X
            sta THEME+1
            rts            
                                
; Play Next Note
; Rotates the 16-bit register one bit to the left
; and plays the note
MusService: bit MUSIC_F
            bpl note_r
            dec MUCD 
            bne note_r
            lda TEMPO
            sta MUCD
            lda #$00            ; Shift the register left
            asl THEME           ; ,,
            rol THEME+1         ; ,,
            adc THEME           ; ,,
            sta THEME           ; ,,
            ora #$80            ; Gate the voice
            sta VOICEL          ; ,,

            lda FADE            ; Fade is a volume override. If fade is
            beq volreg          ;   set, it will decrease every note,
            dec FADE            ;   and the music will stop when it
            bne vol             ;   reaches zero
            jmp MStop
volreg:     lda THEME+1         ; Set the music volume and flash
vol:        sta VOLUME          ;   the aux color
note_r:     rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Intro:      .asc $93,$0d,$0d,$0d,$0d,$0d,$0d
            .asc $1c,$21,$05,"    TROLLEY",$0d
            .asc $1c,$21,$05,"        PROBLEM",$1c,$21
            .asc $0d,$0d,$0d,$9f,"BY JASON JUSTIAN  2021",$0d,$0d
            .asc "      PRESS FIRE",$00

; Manual Text            
Manual:     .asc $93,$05,"AVAST",$0d,$0d,$0d
            .asc "OUR UNDERSEA BASE IS",$0d,$0d
            .asc "IN DANGER",$0d,$0d,$0d
            .asc $9e,$29,$05," NAVIGATE YOUR SUB",$0d,$0d
            .asc $1c,$21,$05," PICK UP A MED PACK",$0d,$0d
            .asc "  FROM THE LIGHTHOUSE",$0d,$0d
            .asc "@ DELIVER TO THE BASE",$0d,$0d
            .asc $1e,$28,$05," AVOID SEA LIFE ",$0d,$0d
            .asc $3a," WATCH YOUR OXYGEN ",$0d,$0d,$0d
            .asc "            AGENT ANZU",$00

; Score Bar
ScoreTx:    .asc $13,$05,"SCORE ",$00
HiTx:       .asc "  HIGH ",$00
GameOverTx: .asc $13,$11,$11,$11,$11,$11,$11,$11,$11
            .asc $1d,$1d,$1d,$1d,$1d,$1d
            .asc $05," GAME OVER ",$00
            
; Direction Tables                       
DirTable:   .byte $01,$ea,$01,$16,$ff
JoyTable:   .byte 0,$04,$80,$08,$10,$20            ; Corresponding direction bit

; Sound effects for the sound effects player
; Each effect has four parameters (DFFFFFFF LLLLSSSS)
;   (1) Bit 7 (D) of the first byte is the direction
;       * Unset=shift left, or a rising sound 
;       * Set=shift right, or a falling sound
;   (2) Bits 0-6 (F) of the first byte is the frequency register
;   (3) High nybble of the second byte (L) is the length in jiffies x 16
;       * Between approx. 1/4 sec and 4 sec in length
;   (4) Low nybble of second byte (S) is speed in jiffies
FXTable:    .byte $8a,$13       ; 0- Med Pack Pickup
            .byte $07,$34       ; 1- Med Pack Delivery
            .byte $f0,$26       ; 2- Game Over
            .byte $55,$24       ; 3- Low O2 Warning
            .byte $87,$11       ; 4- Missing Med Pack

; Musical Themes
Themes:     .word $5523
            .word $5555

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LEVEL TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; There are 45 bytes per level
; 32 bytes represent a 16x16 character grid for track placeholders
; 13 bytes specify the locations of passengers and switches
;    Passengers are first, and all entries are passengers until $00 is reached
;    Then, the rest of the entries are switches
;    So, the sum of passengers + switches is 12
Levels:     .byte $ff,$ff,$81,$01,$81,$01,$81,$01
            .byte $81,$01,$81,$01,$ff,$ff,$81,$01
            .byte $81,$01,$81,$01,$81,$01,$81,$01
            .byte $81,$01,$81,$01,$81,$01,$ff,$ff

Padding:    .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "012345678901234567890123456789012345678901234567890123456789"
            .asc "01234567890123456"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CUSTOM CHARACTER SET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The character set must start at $1C00. If you change anything
; anywhere, you must account for this. The easiest way is to use
; padding bytes immediately before this character data.
;
; The easiest way to tell if you've done this right is to
; make sure that the object code is exactly 3583 bytes. This is
; a reliable method as long as you don't add anything AFTER this
; character data.
;
CharSet:    .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Placeholder
            .byte $00,$3c,$18,$24,$42,$7e,$46,$42 ; A
            .byte $00,$7c,$22,$3c,$22,$22,$22,$7c ; B
            .byte $00,$1a,$26,$42,$40,$40,$22,$1c ; C
            .byte $00,$7c,$22,$22,$22,$22,$22,$7c ; D
            .byte $00,$7e,$20,$2c,$32,$22,$20,$7e ; E
            .byte $00,$7e,$20,$2c,$32,$22,$20,$60 ; F
            .byte $00,$3c,$42,$40,$4e,$52,$42,$3c ; G
            .byte $00,$66,$42,$5a,$66,$42,$42,$66 ; H
            .byte $00,$7c,$38,$10,$10,$10,$38,$7c ; I
            .byte $00,$1e,$14,$04,$04,$44,$44,$38 ; J
            .byte $00,$44,$48,$50,$60,$58,$44,$44 ; K
            .byte $00,$40,$40,$40,$40,$42,$44,$7c ; L
            .byte $00,$62,$56,$4a,$4a,$4a,$42,$42 ; M
            .byte $00,$46,$62,$72,$5a,$4e,$46,$62 ; N
            .byte $00,$3c,$42,$42,$42,$3c,$00,$3c ; O
            .byte $00,$7c,$42,$42,$42,$7c,$40,$e0 ; P
            .byte $00,$3c,$66,$42,$42,$66,$3c,$07 ; Q
            .byte $00,$7c,$42,$42,$42,$7c,$44,$e2 ; R
            .byte $02,$3e,$42,$40,$3c,$02,$42,$3c ; S
            .byte $00,$7c,$54,$10,$10,$10,$10,$38 ; T
            .byte $00,$66,$42,$42,$42,$42,$42,$3c ; U
            .byte $00,$66,$42,$42,$42,$42,$24,$18 ; V
            .byte $00,$66,$42,$42,$4a,$56,$62,$42 ; W
            .byte $00,$42,$62,$34,$18,$2c,$46,$42 ; X
            .byte $00,$42,$62,$34,$18,$18,$18,$3c ; Y
            .byte $00,$7e,$46,$0c,$18,$30,$62,$7e ; Z
            .byte $00,$18,$18,$00,$3c,$3c,$18,$18 ; Passenger           ($1b)
            .byte $00,$18,$18,$40,$3c,$1c,$18,$18 ; Passenger - Wave 1  ($1c)
            .byte $00,$18,$18,$00,$7c,$1c,$18,$18 ; Passenger - Wave 2  ($1d)
            .byte $00,$00,$10,$10,$10,$10,$38,$7c ; Switch off          ($1e)
            .byte $00,$00,$02,$04,$08,$10,$38,$7c ; Switch on           ($1f)
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Space               ($20)
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Bitmap Source       ($21)
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Bitmap Destination  ($22)
            .byte $24,$3c,$27,$2a,$12,$0f,$00,$00 ; Curve E / N         ($23)
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; <unused>            ($24)
            .byte $24,$3c,$24,$3c,$24,$3c,$24,$3c ; N <-> S             ($25)
            .byte $00,$00,$0f,$12,$2a,$27,$3c,$24 ; Curve E / S         ($26)
            .byte $24,$3c,$27,$3d,$25,$3f,$24,$3c ; E switchable        ($27)
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; <unused>            ($28)
            .byte $24,$3c,$e4,$54,$48,$f0,$00,$00 ; Curve W / N         ($29)
            .byte $00,$00,$ff,$55,$55,$ff,$00,$00 ; W <-> E             ($2a)
            .byte $24,$24,$ff,$55,$55,$ff,$00,$00 ; N switchable        ($2b)
            .byte $00,$00,$f8,$4c,$54,$e4,$3c,$24 ; Curve W / S         ($2c)
            .byte $24,$3c,$e4,$7c,$64,$fc,$24,$3c ; W switchable        ($2d)
            .byte $00,$00,$ff,$55,$55,$ff,$24,$3c ; S switchable        ($2e)
            .byte $24,$3c,$e7,$59,$59,$e7,$24,$3c ; Cross               ($2f)
            .byte $00,$3c,$42,$42,$42,$42,$42,$3c ; 0
            .byte $00,$10,$30,$10,$10,$10,$10,$38 ; 1 
            .byte $00,$3c,$42,$46,$0c,$18,$32,$7e ; 2 
            .byte $00,$5c,$62,$42,$0a,$14,$42,$3c ; 3 
            .byte $00,$40,$24,$24,$44,$7e,$04,$0e ; 4
            .byte $02,$7e,$42,$40,$3c,$02,$42,$3c ; 5
            .byte $02,$3e,$42,$40,$7c,$42,$42,$3c ; 6
            .byte $00,$7e,$42,$02,$0c,$18,$10,$38 ; 7
            .byte $00,$3c,$42,$72,$3c,$4e,$42,$3c ; 8
            .byte $00,$3c,$42,$42,$62,$1e,$42,$3c ; 9
            .byte $00,$00,$3c,$7e,$7e,$3c,$00,$00 ; Trolley W-E         ($3a)
            .byte $00,$18,$3c,$3c,$3c,$3c,$18,$00 ; Trolley N-S         ($3b)
            .byte $00,$0c,$1e,$3e,$7c,$78,$30,$00 ; Trolley SW-NE       ($3c)
            .byte $00,$30,$78,$7c,$3e,$1e,$0c,$00 ; Trolley NW-SW       ($3d)
            .byte $00,$78,$48,$7e,$4a,$7e,$6a,$ff ; Building            ($3e)
            .byte $00,$10,$38,$6c,$fe,$54,$74,$ff ; Depot               ($3f)
