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
SCRCOLVAL   = 136               ; Screen color
NORTH       = $01               ; ,,
EAST        = $02               ; ,,
SOUTH       = $03               ; ,,
WEST        = $04               ; ,,
FIRE        = $05               ; Joystick fire button pressed
DEF_SPEED   = 6                 ; Default speed
MAX_SPEED   = 3                 ; Maximum speed
MIN_SPEED   = 12                ; Minimum speed
MAX_PASS    = 4                 ; Maximum passengers on trolley
TR_COLOR    = 6                 ; Track/Trolley color

; Score constants
PICKUP      = 10                ; Score when a passenger is picked up
DROPOFF     = 50                ; Score when a passenger is dropped off
TIME_BONUS  = 5                 ; Time bonus

; Character constants
PASS1       = $1b
PASSENGER   = $1d
SWITCH_OFF  = $1f
SWITCH_ON   = $1e
BUILDING    = $22
DEPOT       = $21
START_DEPOT = $24
TROLLEY_WE  = $28
TROLLEY_NS  = $1c
TROLLEY_FW  = $1a               ; Trolley that goes like a slash
TROLLEY_BK  = $3d               ; Trolley that goes like a backslash
B_SRC       = $3e               ; Bipmap source character
B_DEST      = $3f               ; Bipmap destination character

; System Resources
CINV        = $0314             ; ISR vector
;NMINV       = $0318             ; Release NMI vector
NMINV       = $fffe             ; Development NMI non-vector
SCREEN      = $1e00             ; Screen character memory (unexpanded)
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eabf             ; System ISR   
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
PLOT        = $fff0             ; PLOT 

; Game Memory
LEVEL       = $f7               ; Level pointer (2 bytes)
TROLLEY     = $f9               ; Trolley location (2 bytes)
CURSOR      = $00               ; [LEVEL DRAW] Track cursor
SWITCH      = $fb               ; Current switch location (2 bytes)
UNDER_S     = $fd               ; Track character at trolley source
COLUMN      = $fd               ; [LEVEL DRAW] Column number
JOYREG      = $fe               ; Joystick register storage
TRMASK      = $fe               ; [LEVEL DRAW] Track mask
SCORE       = $45               ; Score (2 bytes)
HISCORE     = $b2               ; High score (2 bytes)
PLAY_FL     = $49               ; Play flag (determines behavior of ISR)
WAITING     = $02               ; Passengers waiting
RIDING      = $03               ; Passenger Count
DIR         = $04               ; Tolley direction
TIME        = $05               ; Time remaining (seconds)
UNDER_D     = $06               ; Character at trolley destination
ZP_SOURCE   = $07               ; Zero page copy source (2 bytes)
ZP_DEST     = $09               ; Zero page copy destination (2 bytes)
C_SWITCH    = $0b               ; Current switch pointer (2 bytes)
SW_COL      = $b0               ; Current switch color pointer (2 bytes)
LEVEL_NUM   = $0f               ; Current level number - 1
JSREAD      = $10               ; Last joystick read
SEC_COUNT   = $11               ; Second countdown
TIMER_FL    = $12               ; Timer display flag
TR_SOURCE   = $0350             ; Trolley-only source bitmap (8 bytes)
TR_DEST     = $0358             ; Trolley-only destination bitmap (8 bytes)
TK_SOURCE   = $0360             ; Track-only source bitmap  (8 bytes)
TK_DEST     = $0368             ; Track-only source bitmap (8 bytes)
MV_COUNT    = $0370             ; Bitmap movement countdown
MOVE_FL     = $0371             ; Move flag
SPEED       = $0372             ; Current speed
SPEED_COUNT = $0373             ; Speed countdown
SWITCH_FL   = $0374             ; Switch on flag

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
            
NextLevel:  sei
            jsr InitLevel       ; Initialize Level
            cli
            jsr PrepMove

; Main loop            
Main:       bit MOVE_FL         ; If move flag is set, move the trolley
            bpl timer_disp      ;   one pixel in the appropriate direction
            jsr MoveBitmap      ;   ,,
            jsr BitMerge        ;   ,,
            lsr MOVE_FL         ; Clear move flag
            lda TIME            ; Check for timer elapsed
            bne timer_disp      ; ,,
            jmp GameOver        ; ,,
timer_disp: bit TIMER_FL        ; If the timer flag is set, show the score
            bpl read_js         ; ,,
            jsr ShowScore       ; ,,
            lsr TIMER_FL        ; Clear the timer flag
read_js:    jsr Joystick        ; Read the joystick
            beq Main            ; If no movement do nothing
ch_fire:    cmp #FIRE           ; Has fire been pressed?
            bne handle_dir      ; If not, handle a direction
            jsr NextSwitch      ; If fire is pressed, toggle current switch
            jmp Main            ;   ,,
handle_dir: cmp #NORTH
            beq faster
            cmp #SOUTH
            beq slower
            cmp #EAST
            beq sw_on
            cmp #WEST
            bne Main
sw_off:     lda #SWITCH_OFF
            .byte $3c           ; Skip word
sw_on:      lda #SWITCH_ON
            ldx #$00
            sta (C_SWITCH,x)
            lda #$01            ; Launch switch effect
            jsr FXLaunch        ; ,,
            bne Main
faster:     dec SPEED
            dec SPEED
            dec SPEED
            lda SPEED
            cmp #MAX_SPEED
            bcs Main
slower:     inc SPEED
            inc SPEED
            inc SPEED
            lda SPEED
            cmp #MIN_SPEED
            bcc Main
            bcs faster

 ; Custom ISR for music player and day counting
ISR:        bit PLAY_FL         ; If the game is over, don't do anything
            bpl isr_r           ;   in this routine
            jsr FXService       ; Service sound effect
            jsr MusService      ; Service shift register music
            dec SEC_COUNT       ; Countdown to the next second
            bne c_speed         ; ,,
            dec TIME            ; Decrement the timer
            sec                 ; Show and check the timer
            ror TIMER_FL        ; ,,
            lda #60             ; Reset the second counter
            sta SEC_COUNT       ; ,,
c_speed:    dec SPEED_COUNT     ; Handle speed countdown for trolley
            bne isr_r           ; ,, 
            lda SPEED           ; Reset the speed countdown
            sta SPEED_COUNT     ; ,,
            sec                 ; Set the movement flag
            ror MOVE_FL         ; ,,
isr_r:      jmp IRQ           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wait for Fire
Wait4Fire:  jsr Joystick
            cmp #FIRE           ; ,,
            beq Wait4Fire       ; ,,
wait_fire:  jsr Joystick        ; Wait for the fire button
            cmp #FIRE           ; ,,
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
            cmp JSREAD          ; Debounce the joystick by setting
            bne joystick_r      ;   the read value to 0 if it's the
            lda #$00            ;   same as the last value
            rts                 ;   ,,
joystick_r: sta JSREAD          ; This is a new value, so return it and
            rts                 ;   set the last read value

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

ShowScore:  lda #<ScoreBar      ; Set up score bar
            ldy #>ScoreBar      ; ,,
            jsr PRTSTR          ; ,,
            lda LEVEL_NUM       ; Show level number
            clc                 ; ,,
            adc #$30            ; ,,
            jsr CHROUT          ; ,,
            lda #$20            ; Space before score
            jsr CHROUT          ; ,,
            ldx SCORE           ; Show the score
            lda SCORE+1         ; ,,
            jsr PRTFIX          ; ,,
            ldx #$03
            ldy #$0a
            clc
            jsr PLOT
            ldx TIME            ; Get the time remaining
            cpx #$ff            ; Check race conditions that involve
            bne show_time       ;   the ISR reducing time below zero while
            ldx #$00            ;   something else is going on
show_time:  lda #$00            ; Show the time remaining
            jsr PRTFIX          ; ,,
            lda #$20            ; Space to clear unused tens
            jsr CHROUT          ;   places
            rts
            
; Reset Cursor
; To the top of the board
ResetCur:   lda #$71            ; Set starting point for level
            sta CURSOR          ; ,,
            lda #>SCREEN        ; ,,
            sta CURSOR+1        ; ,,
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
            
; Place Character
; In a 16x16 grid, with the coordinates in A
;   High nybble = Y position
;   Low nybble = X position
; And the character code in X            
PlaceChar:  pha
            jsr ResetCur
            pla
            pha
            and #$0f            ; Mask for X coordinate
            clc
            adc CURSOR
            sta CURSOR
            bcc do_x
            inc CURSOR+1
do_x:       pla                 ; Shift for Y coordinate
            lsr                 ; ,,
            lsr                 ; ,,
            lsr                 ; ,,
            lsr                 ; ,,
            tay                 ; Move downward Y times
            iny                 ; ,,
            txa                 ; A is now the character to place
            pha                 ; ,,
-loop:      ldx #SOUTH
            jsr MoveCursor
            dey
            bne loop
            ldx #NORTH          ; Move north once to compensate for the
            jsr MoveCursor      ;   extra down
            ldx #$00            ;   and place it in the cursor position
            lda (CURSOR,x)      ; If the space is occupied, do not
            cmp #$20            ;   draw anything
            bne place_r         ;   ,,
draw_char:  pla                 ; Get the character back and draw it
            sta (CURSOR,x)      ; ,,
            pha                 ; Preserve X for caller
            lda CURSOR+1
            clc
            adc #$78
            sta CURSOR+1
            pla
            pha
            and #$07
            sta (CURSOR,x);
place_r:    pla                 ; Restore X for caller
            tax                 ; ,,
            rts
            
; Get Character
; At direction specified by X
GetChar:    txa                 ; Preserve X against MoveCursor
            pha                 ; ,,
            lda CURSOR          ; Preserve cursor
            pha                 ; ,,
            lda CURSOR+1        ; ,,
            pha                 ; ,,
            jsr MoveCursor      ; Move cursor in X direction
            ldx #$00            ; Get character there
            lda (CURSOR,x)      ; ,,
            tay                 ; Stash it in Y
            pla                 ; Bring the cursor back to center
            sta CURSOR+1        ; ,,
            pla                 ; ,,
            sta CURSOR          ; ,,
            pla                 ; Restore X
            tax                 ; ,,
            tya                 ; Restore A as return value
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
            lda #$01            ; Starting level number of new game
            sta LEVEL_NUM       ; ,,
InitCont:   lda #$00            ; Set the theme
            sta FX_LEN          ;   and effects length
            sta SCORE           ;   and score
            sta SCORE+1         ;   ,,
            sta TIMER_FL        ;   and timer flag
            sta MOVE_FL         ;   and movement flag
            jsr SelTheme        ; Set theme
            lda #$07            ; Set tempo
            sta TEMPO           ; ,,
            jsr MPlay           ; Start the music
            sec                 ; Set the game play flag
            ror PLAY_FL         ; ,,
            lda #$18            ; Set volume and aux color
            sta VOLUME          ; ,,
            rts
            
InitLevel:  ldy #$21            ; Get the time for the level
            lda (LEVEL),y       ;   and set remaining time
            sta TIME            ;   ,,
            lda #EAST           ; All levels start out with the
            sta DIR             ;   trolley moving east
            lda #$00            ; Clear the trolley
            sta RIDING          ; ,,
            sta MOVE_FL         ; and movement flag
            sta MV_COUNT        ; and move count
            lda #DEF_SPEED      ; Set starting speed
            sta SPEED           ; ,,
            sta SPEED_COUNT     ; ,,
            jsr CLSR            ; ,,
            ldy #$00            ; Set track/trolley color
            lda #TR_COLOR       ;   ,,
-loop:      sta $9600,y         ;   ,,
            sta $9700,y         ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,      
            lda #<ScoreTx       ; Show the top banner
            ldy #>ScoreTx       ; ,,
            jsr PRTSTR          ; ,,
            lda #60             ; Set second countdown timer
            sta SEC_COUNT       ; ,,
            jsr MPlay           ; Start music
            ; Fall through to DrawLevel

DrawLevel:  jsr ResetCur        ; Reset cursor to top left
            ldx #$00            ; Initialize ZP cursor index
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
            cmp #START_DEPOT    ; Did we place the starting depot?
            bne next_cell       ;
            lda CURSOR          ; The trolley's starting position is
            sta TROLLEY         ;   one row below (22 cells after)
            ldx CURSOR+1        ;   the starting depot
            stx TROLLEY+1       ;   ,,
            lda #$16            ;   ,,
            clc                 ;   ,,
            adc TROLLEY         ;   ,,
            sta TROLLEY         ;   ,,
            bcc next_cell       ;   ,,
            inc TROLLEY+1       ;   ,,
next_cell:  inc CURSOR          ; Advance the cursor to the next cell
            bne check_end2      ; ,,
            inc CURSOR+1        ; ,,
check_end2: lda CURSOR+1        ; Have we reached the end of the screen?
            cmp #$20            ; ,,
            bne loop            ; If more to do, go back for the next cell
            ldx #$00            ; Set the Under Destination character in
            lda (TROLLEY,x)     ;   preparation for the first move
            sta UNDER_D         ;   ,,
            ; Fall through to AddPieces

; Add Pieces to Board
; Switches and Passengers
AddPieces:  ldy #$22            ; Start adding pieces after track data
            ldx #SWITCH_OFF     ; First section is switch location data
-loop:      tya                 ; So we don't need to worry about Y in
            pha                 ;   PlaceChar
            lda (LEVEL),y       ; Get piece
            bne place           ; If the position is 0, change from
            ldx #PASS1          ;   switches to passengers
            sta WAITING         ; Reset waiting count
            bne next_piece
place:      inc WAITING         ; Count waiting passengers
            jsr PlaceChar       ; Place character
next_piece: pla
            tay
            iny
            cpy #$30            ; Reached end of level data
            bcc loop
            lda #<SCREEN        ; Locate the first switch from the top
            sta C_SWITCH        ;   of the screen
            lda #>SCREEN        ;   ,,
            sta C_SWITCH+1      ;   ,,
            jsr NextSwitch      ;   ,,
            ; Fall through to CityScape
            
; Draw Buildings
; Use some trig constants as pseudorandom coordinates for
; buildings
CityScape:  ldy #$20            ; Coordinates of depot
            ldx #DEPOT
            lda (LEVEL),y
            jsr PlaceChar
            ldx #BUILDING
            ldy #$21
-loop:      lda $e2ec,y
            sty TRMASK
            adc LEVEL           ; Add the level to mix it up a bit
            jsr PlaceChar
            ldy TRMASK
            dey
            bne loop
            jsr ShowScore       ; Show score at end of level draw
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME MECHANICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NextSwitch: lda C_SWITCH        ; Set the current switch back to yellow
            sta SW_COL          ; ,,
            lda C_SWITCH+1      ; ,,
            clc                 ; ,,
            adc #$78            ; ,,
            sta SW_COL+1        ; ,,
            ldx #$00            ; ,,
            lda #$07            ; ,,
            sta (SW_COL,x)      ; ,,
look_more:  inc C_SWITCH
            bne is_switch
            inc C_SWITCH+1
            lda C_SWITCH+1
            cmp #$20
            bne is_switch
            lda #<SCREEN
            sta C_SWITCH
            lda #>SCREEN
            sta C_SWITCH+1
is_switch:  ldx #$00
            lda (C_SWITCH,x)
            cmp #SWITCH_OFF
            beq next_sw_r
            cmp #SWITCH_ON
            bne look_more
next_sw_r:  lda C_SWITCH        ; Set the active switch color pointer
            sta SW_COL          ; ,,
            lda C_SWITCH+1      ; ,,
            clc                 ; ,,
            adc #$78            ; ,,
            sta SW_COL+1        ; ,,
            rts 

; Pick up Passenger
Pickup:     jsr MoveCursor      ; Move cursor to where the passenger is
            ldy #$00            ; If there's room on the trolley, pick up
            lda #$20            ;   the passenger by incrementing the riding
            sta (CURSOR),y      ;   count and decrementing the waiting count
            inc RIDING          ;   ,,
            dec WAITING         ;   ,,
            lda #PICKUP         ; Add score for the pickup
            jsr AddScore        ; ,,
            lda #PASSENGER      ; Add a passenger to the display
            ldy RIDING          ; ,,
            sta $1e50,y         ; ,,
            lda #$03            ; Add the color
            sta $9650,y         ; ,,
            lda #$00            ; Launch sound effect for pickup
            jsr FXLaunch        ; ,,
            lda #$01            ; Flip the low shift register music bit on
            eor THEME           ;   each pickup
            sta THEME           ;   ,,
            rts
       
; Game Over            
GameOver:   jsr ShowScore       ; Show final score
            lda #<GameOverTx    ; Show Game Over text
            ldy #>GameOverTx    ; ,,
            jsr PRTSTR          ; ,,
            lda #<HiScoreTx     ; Show High Score text
            ldy #>HiScoreTx     ; ,,
            jsr PRTSTR          ; ,,
            ldx HISCORE         ; Show the high score
            lda HISCORE+1       ; ,,
            jsr PRTFIX          ; ,,            
            lda #$0f            ; Fade out music at end of game
            sta FADE            ; ,,
            lda #$80            ; Delay for fade-out
            jsr Delay           ; ,,
            lsr PLAY_FL         ; Stop the play
            lda LEVEL_NUM       ; If the player lost or won level 12,
            cmp #$0c            ;   pressing fire will start a new
            bne continue        ;   game at level 1. You cannot continue
            jmp Start           ;   level 12!
continue:   jsr Wait4Fire       ; If the fire button is pressed here,
            jsr InitCont        ;   continue at the current level
            sei                 ;   ,,
            jsr InitLevel       ;   ,,
            cli                 ;   ,,
            jsr PrepMove        ; Prep move at the last level
            jmp Main            ;   and back to Main
            
; Check Adjacent Cells
Adjacent:   lda WAVING+3        ; A couple graphical tasks on each
            eor #$40            ;   movement. First, change some bits in the
            sta WAVING+3        ;   waiting passenger character so that
            lda WAVING+4        ;   they look like they're waving
            eor #$40            ;   ,,
            sta WAVING+4        ;   ,,
            ldx #$00            ; Then, flash the active switch color
            lda #$02            ;   between yellow and green
            eor (SW_COL,x)      ; ,,
            sta (SW_COL,x)      ; ,,
            lsr SWITCH_FL       ; Clear the switch flag
            ldx #WEST           ; Start with WEST and go counterclockwise
-loop:      jsr GetChar         ; Get charater in the X direction
            cmp #SWITCH_ON      ; If it's a switch that's turned on, set the
            bne ch_pass         ;   switch on flag
            sec                 ;   ,,
            ror SWITCH_FL       ;   ,,
            rts
ch_pass:    cmp #PASS1          ; If it's a passenger, check how many riders
            bne ch_depot        ;   are currently onboard. If it's less than
            lda RIDING          ;   the maximum, allow a new rider aboard
            cmp #MAX_PASS       ;   ,,
            bcs ch_depot        ;   ,,
            jmp Pickup          ;   ,,
ch_depot:   cmp #DEPOT          ; If it's the depot, clear the trolley and
            bne ch_start        ;   increase the score
            ldy RIDING          ; How many riders?
            beq next_adj        ; If none, back to loop
            jsr MStop           ; Stop music during dropoff
-loop2:     lda #$20            ; Replace a rider with a space
            sta $1e50,y         ; ,,
            tya                 ; Preserve iterator
            pha                 ; ,,
            lda #DROPOFF        ; Add the score for each dropoff
            jsr AddScore
            lda #$02
            jsr FXLaunch
            lda #$10
            jsr Delay
            pla                 ; Restore iterator
            tay                 ; ,,
            dey
            bne loop2
            sty RIDING          ; Reset the riders counter
            jsr MPlay
            lda WAITING         ; If all the riders are dropped off,
            bne adj_r           ;   advance to the next level
            jmp Advance         ;   ,,
ch_start:   cmp #START_DEPOT    ; If it's the start depot, turn the trolley
            bne next_adj        ;   around
            lda #SOUTH          ;   ,,
            sta DIR             ;   ,,
            rts
next_adj:   dex
            bne loop
adj_r:      rts
  
; Advance to Next Level
Advance:    jsr MStop           ; Stop music for bonus count
            lda #$18
            jsr Delay
-loop:      lda #$03            ; Launch bonus sound
            jsr FXLaunch        ; ,,
            lda #TIME_BONUS
            jsr AddScore
            lda #$05
            jsr Delay
            dec TIME
            bpl loop
            lda #$00            ; Zero out time
            sta TIME            ; ,,
            jsr ShowScore       ; Show score to display 0 time
            lda #60
            jsr Delay
            inc LEVEL_NUM
            lda LEVEL_NUM       ; If the player finishes Level 12, the game is
            cmp #$0d            ;   over
            bne lv_data
            lda #<VictoryTx     ; Show Victory text
            ldy #>VictoryTx     ; ,,
            jsr PRTSTR          ; ,,        
            dec LEVEL_NUM       ; Reduce the level number for display
            jmp GameOver        ; ,,
lv_data:    lda #$30
            clc
            adc LEVEL
            sta LEVEL
            bcc advance_r
            inc LEVEL+1
advance_r:  jmp NextLevel           
                                    
; Handle New Cell
; Check for several things upon entering a new cell
;   * Pick up new passengers
;   * Drop off passengers at depot
;   * Change direction based on tracks and switches
HandleCell: jsr Adjacent        ; Handle passengers, depot, switches  
srch_track: ldy #$00            ; Search track for table entry
-loop:      lda TrackTurn,y
            bmi PrepMove        ; End search if table ends
            cmp UNDER_D         ; This is the newly-entered track character
            beq found
            iny                 ; Advance to next table entry
            iny                 ; ,,
            iny                 ; ,,
            iny                 ; ,,
            iny                 ; ,,
            bne loop
found:      tya                 ; A is the table index
            clc                 ;   for addition
            adc DIR             ; Add the current direction for the actual index
            tay                 ; Put the index back in Y
            lda TrackTurn,y     ; Get the new directon
            cmp #$08            ; Is bit 3 set for the new direction?
            bcc no_switch       ; If so, it depends on whether there's an
            bit SWITCH_FL       ;   adjacent switch turned Om
            bpl PrepMove        ; If no switch is on, continue to go straight
            and #$07            ; Mask off bit 3
no_switch:  cmp DIR             ; Is it a direction change?
            beq PrepMove        ; If not, prepare for next movement
            pha                 ; Store new direction during turn
            clc                 ; Add old direction and new direction. If the
            adc DIR             ;   sum is 5, then the turn is done with
            cmp #$05            ;   the north-to-south (backslash) trolley.
            beq bk              ;   If the sum is 3 or 7, the turn is done
            lda #TROLLEY_FW     ;   with the south-to-north (forward slash)
            .byte $3c           ;   trolley.
bk:         lda #TROLLEY_BK     ;   ,,
            ldx #<TR_DEST       ; OR merge the current track with the turning
            ldy #>TR_DEST       ;   trolley characher
            jsr SetBitSrc       ;   ,,
            jsr BitMerge        ;   ,,
            lda SPEED           ; Delay on the turn for twice the speed
            asl                 ; ,,
            jsr Delay           ; ,,
            pla                 ; Get back the direction and set it as the
            sta DIR             ;   new direction            
            ; Fall through to PrepMove
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MOVEMENT ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; Prep for trolley movement in direction specified in DIR
PrepMove:   lda TROLLEY         ; The TROLLEY pointer will contain the current
            sta CURSOR          ;   trolley location, and the CURSOR pointer
            lda TROLLEY+1       ;   will contain the trolley's destination
            sta CURSOR+1        ;   ,,
            ldx DIR             ;   ,,
            jsr MoveCursor      ;   ,,
            lda UNDER_D         ; Move the previous under track to the
            sta UNDER_S         ;   new under track
            ldy #$00            ; Set the new destination under; now we have
            lda (CURSOR),y      ;   both UNDER characters, source and
            sta UNDER_D         ;   destination
            
            ; Copies to be done to bitmap memory
            ;   1) Character data from UNDER_S to TK_SOURCE memory
            ;   2) Character data from UNDER_D to TK_DEST memory
            ;   3) Character data from trolley to TR_SOURCE memory
            ;   4) Zero out TR_DEST memory (using a space character)
            lda UNDER_S         ; Fill the bitmap memory starting at TK_SOURCE
            ldx #<TK_SOURCE     ; ,,
            ldy #>TK_SOURCE     ; ,,
            jsr SetBitSrc       ; ,,
            lda UNDER_D         ; Fill the bitmap memory starting at TK_DEST
            ldx #<TK_DEST       ; ,,
            ldy #>TK_DEST       ; ,,
            jsr SetBitSrc       ; ,,
            ldx #TROLLEY_WE     ; Choose trolley character, default to west/east,
            lda DIR             ;   based on trolley direction
            and #$01            ; North and South have bit 0 set
            beq copy_tr         ;   ,,
            ldx #TROLLEY_NS     ;   ,,
copy_tr:    txa                 ; Fill the bitmap memory starting at TR_SOURCE
            ldx #<TR_SOURCE     ; ,,
            ldy #>TR_SOURCE     ; ,,
            jsr SetBitSrc       ; ,,
            lda #$20            ; Fill the bitmap memory starting at TR_DEST
            ldx #<TR_DEST       ; ,,
            ldy #>TR_DEST       ; ,,
            jsr SetBitSrc       ; ,,
            jsr BitMerge        ; Merge the two bitmaps
            ldy #$00            ; Update locations of bitmap characters
            lda #B_SRC          ; ,,
            sta (TROLLEY),y     ; ,,
            lda #B_DEST         ; ,,
            sta (CURSOR),y      ; ,,
            lda #$07            ; Reset movement counter
            sta MV_COUNT        ; ,,
            rts
            
; Turn
; Perform a turning maneuver in the direction (RIGHT, LEFT) in Accumulator
Turn:       rts

; Move Bitmap
MoveBitmap: lda MV_COUNT        ; Are there any more moves to do?
            bne shift_bit       ; 
            lda UNDER_S         ; Restore the Under Source character to
            ldy #$00            ;   the old trolley's position
            sta (TROLLEY),y     ;   ,,
            lda CURSOR          ; Update the trolley position
            sta TROLLEY         ; ,,
            lda CURSOR+1        ; ,,
            sta TROLLEY+1       ; ,,
            jmp HandleCell      ; Handle new cell
shift_bit:  dec MV_COUNT
            lda DIR
            cmp #NORTH
            beq mv_north
            cmp #EAST
            beq mv_east
            cmp #SOUTH
            beq mv_south
            ; Fall through to mv_west
mv_west:    ldy #$07            ; For each byte
-loop:      lda TR_DEST,y       ; Shift the destination character left
            asl                 ; ,,
            sta TR_DEST,y       ; ,,
            lda TR_SOURCE,y     ; Shift the source character left
            asl                 ; ,,
            sta TR_SOURCE,y     ; ,,
            lda #$00            ; If the high bit of the source byte was 1,
            adc TR_DEST,y       ;   then add 1 to the destination byte
            sta TR_DEST,y       ;   and save it
            dey
            bpl loop
            rts
mv_east:    ldy #$07            ; For each byte
-loop:      lda TR_DEST,y       ; Shift the destination character right
            lsr                 ; ,,
            sta TR_DEST,y       ; ,,
            lda TR_SOURCE,y     ; Shift the source character right
            lsr                 ; ,,
            sta TR_SOURCE,y     ; ,,
            bcc east_next       ; 
            lda TR_DEST,y       ; If the low bit of the source byte was 1,
            ora #$80            ;   then set the high bit of the destination
            sta TR_DEST,y       ;   and save it
east_next:  dey
            bpl loop
            rts
mv_north:   ldy #$00
-loop:      lda TR_DEST+1,y     ; Copy each byte from the byte below it
            sta TR_DEST,y       ; ,,
            iny
            cpy #$07
            bne loop
            lda TR_SOURCE       ; Move the top byte of the source character to
            sta TR_DEST+7       ;   the bottom of the destination character
            ldy #$00
-loop:      lda TR_SOURCE+1,y
            sta TR_SOURCE,y
            iny
            cpy #$07
            bne loop
            lda #$00            ; Clear the bottom byte of the source character
            sta TR_SOURCE+7     ; ,,
            rts
mv_south:   ldy #$06            ; Copy each byte from the byte above it
-loop:      lda TR_DEST,y       ; ,,
            sta TR_DEST+1,y     ; ,,
            dey
            bpl loop
            lda TR_SOURCE+7     ; Copy the bottom of the source to the top of
            sta TR_DEST         ;   the destination
            ldy #$06
-loop:      lda TR_SOURCE,y
            sta TR_SOURCE+1,y
            dey
            bpl loop                    
            lda #$00            ; Clear the top byte of the source character
            sta TR_SOURCE       ; ,, 
            rts

; Bitmap Merge
; Merge the bitmaps to TR_ and TK_ to the visible bitmap data
BitMerge:   ldx #$07
-loop:      lda TR_SOURCE,X
            ora TK_SOURCE,x
            sta BITMAP_S,x 
            lda TR_DEST,x
            ora TK_DEST,x
            sta BITMAP_D,x
            dex
            bpl loop
            rts
            
; Set bitmap source
; A = Character number
; X = Low byte of destination
; Y = High byte of destination
SetBitSrc:  stx ZP_DEST         ; Set the destination pointer
            sty ZP_DEST+1       ; ,,          
            ldy #<CharSet       ; Set the source pointer
            sty ZP_SOURCE       ; ,,
            ldy #>CharSet       ; ,,
            sty ZP_SOURCE+1     ; ,,
            tay                 ; Add 8 this many times
            beq transcribe
            dey
-loop:      lda #$08
            clc
            adc ZP_SOURCE
            sta ZP_SOURCE
            bcc next_inc
            inc ZP_SOURCE+1
next_inc:   dey
            bpl loop
transcribe: ldy #$07
-loop:      lda (ZP_SOURCE),y
            sta (ZP_DEST),y
            dey
            bpl loop
            rts           
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; EFFECTS SERVICE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
; Play Next Sound Effect
; Rotates the 8-bit sound effect register and
; plays the pitch      
FXService:  lda FX_LEN          ; Has the sound been launched?
            beq fx_end          ; If unlaunched, kill voice and return
            lda #$18            ; Make sure sound effects are not affected by
            sta VOLUME          ;   music player's volume changes
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
Intro:      .asc $93,$0d,$0d,$0d,$0d,$1f
            .asc     "   &**************,",$0d
            .asc     "   %              %",$0d
            .asc $1f,"   %  TROLLEY  ",$5b,"  %",$0d
            .asc     "   %              %",$0d
            .asc     "   %  ",$5b,"  PROBLEM  %",$0d
            .asc     "   %              %",$0d
            .asc     "   #**************)",$0d
            .asc $0d,$0d,"  2021 JASON JUSTIAN",$0d,$0d,$0d,$0d,$0d
            .asc "      PRESS FIRE",$00

; Manual Text            
Manual:     .asc $93,$0d,$1f,"      ALL ABOARD",$0d,$0d
            .asc " ",$9f,$5f,$1f,"SWITCHES GUIDE THE",$0d
            .asc "   TROLLEY TO PICK UP",$0d,$0d
            .asc " ",$9f,$5b,$1f,"PASSENGERS AND DROP",$0d
            .asc "   THEM OFF AT THE",$0d,$0d
            .asc " ",$9f,"!",$1f,"DEPOT",$0d,$0d,$0d,$0d,$0d
            .asc " FIRE SELECTS SWITCH",$0d,$0d
            .asc " LEFT AND RIGHT SET",$0d
            .asc "   STRAIGHT OR TURN",$0d,$0d
            .asc " UP AND DOWN CONTROL",$0d
            .asc "   TROLLEY SPEED",$00

; Score Bar

ScoreTx:    .asc $11,$1f,"   TROLLEY  PROBLEM",$0d
            .asc $90," LV SCORE TIME RIDERS",$00
ScoreBar:   .asc $13,$11,$11,$11,"  ",$90,$00
GameOverTx: .asc $13,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
            .asc $1d,$1d,$1d,$1d,$1d,$1d,$1d,$1d
            .asc "      ",$0d
            .asc $1d,$1d,$1d,$1d,$1d,$1d,$1d,$1d
            .asc $90," GAME ",$0d
            .asc $1d,$1d,$1d,$1d,$1d,$1d,$1d,$1d
            .asc " OVER ",$0d
            .asc $1d,$1d,$1d,$1d,$1d,$1d,$1d,$1d
            .asc "      ",$00    
VictoryTx   .asc $13,$11,$9e,"       VICTORY",$9f,$5b,"     ",$00                   
HiScoreTx:  .asc $13,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
            .asc $11,$11,$11,$11,$11,$11,$11,$11,$11,"    HIGH SCORE ",$00
            
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
FXTable:    .byte $36,$22       ; 0- Passenger pickup
            .byte $84,$11       ; 1- Switch Switch
            .byte $05,$13       ; 2- Drop off passengers
            .byte $55,$12       ; 3- Bonus sound

; Musical Themes
Themes:     .word $5523
            
; Track Turns
; This table describes what turns are made when each kind of track is
; entered from each direction. The first column specifies the track
; character being entered. The next four columns represent the
; direction* the trolley is moving when the cell is entered. The value
; in the column specifies the new direction* that the trolley should 
; move. If the value is 0, there's no change in direction. If bit 3 is
; set, the direction change should only happen if an adjacent switch is
; on, and bit 3 is masked away.
;
; If a track does not appear in this table, no direction changes will happen
; for that track.
;
; * 1=North 2=East 3=South 4=West
;
;                 TRK  1  2  3  4
TrackTurn:  .byte $23, 0, 0, 2, 1   ; Curve E / N
            .byte $26, 2, 0, 0, 3   ; Curve E / S
            .byte $29, 0, 1, 4, 0   ; Curve W / N
            .byte $2c, 4, 3, 0, 0   ; Curve W / S
            .byte $27,10, 0,10, 1   ; E switchable
            .byte $2b, 0, 9, 4, 9   ; N switchable
            .byte $2d,12, 3,12, 0   ; W switchable
            .byte $2e, 2,11, 0,11   ; S switchable            
            .byte $ff               ; End of table

Padding:    .asc "JJ"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LEVEL TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; There are 48 bytes per level
; 32 bytes represent a 16x16 character grid for track placeholders
;  1 byte specifies the coordinates of the goal depot
;  1 byte specifies the level time limit in seconds
; 14 bytes specify the locations of switches and passengers
;    Switches are first, and all entries are switches until $00 is reached
;    Then, the rest of the entries are passengers
;    * So, the sum of passengers + switches is 13
;    * The coordinate format is Y,X
;    * To get a proper passenger count, put the passengers at the end of the
;      data, using multiple $00s, if necessary
Levels:
Level0:     .byte $80,$00,$ff,$fe,$01,$02,$01,$02
            .byte $01,$02,$01,$02,$7f,$02,$41,$02
            .byte $41,$02,$41,$02,$7f,$f2,$01,$12
            .byte $01,$12,$01,$fe,$00,$00,$00,$00
            .byte $7d,$3c,$68,$07,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$2a,$80,$99,$e8

Level1:     .byte $00,$00,$07,$f8,$04,$08,$1e,$0e
            .byte $12,$0a,$3a,$0a,$aa,$0a,$fa,$0a
            .byte $22,$0a,$22,$0a,$22,$0a,$3f,$fa
            .byte $00,$22,$00,$22,$00,$22,$00,$3e
            .byte $6d,$3c,$63,$c6,$aa,$00,$00,$00
            .byte $47,$57,$67,$77,$8d,$c3,$c8,$ec

Level2:     .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58

Level3:     .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58
      
Level4:     .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58            
            
Level5:     .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58            
            
Level6:     .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58

Level7:     .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58
                        
Level8:     .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58

Level9:     .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58

Level10:    .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58

Level11:    .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $00,$00,$80,$00,$ff,$c0,$04,$40
            .byte $04,$40,$07,$c8,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$00,$00
            .byte $8a,$14,$11,$00,$00,$00,$00,$00
            .byte $00,$00,$00,$00,$00,$00,$57,$58            
            
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
            .byte $00,$66,$4a,$4a,$4a,$56,$62,$42 ; W
            .byte $00,$42,$62,$34,$18,$2c,$46,$42 ; X
            .byte $00,$42,$62,$34,$18,$18,$18,$3c ; Y
;           .byte $00,$7e,$46,$0c,$18,$30,$62,$7e ; Z
            .byte $08,$1e,$3e,$7f,$fe,$7c,$78,$10 ; Trolley SW-NE   /   ($1a)
WAVING:     .byte $00,$18,$18,$40,$3c,$1c,$18,$18 ; Passenger - Waving  ($1b)
            .byte $3c,$7e,$7e,$7e,$7e,$7e,$3e,$00 ; Trolley N-S         ($1c)
            .byte $00,$18,$18,$00,$3c,$3c,$18,$18 ; Passenger           ($1d)
            .byte $00,$02,$04,$08,$10,$38,$7c,$7c ; Switch on/turn      ($1e)
            .byte $00,$10,$10,$10,$10,$38,$7c,$7c ; Switch off/straight ($1f)
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Space               ($20)
            .byte $00,$10,$38,$6c,$fe,$54,$74,$ff ; Drop-off depot      ($21)
            .byte $00,$78,$48,$7e,$4a,$7e,$6a,$ff ; Building            ($22)
            .byte $24,$3c,$27,$2a,$12,$0f,$00,$00 ; Curve E / N         ($23)
            .byte $00,$10,$38,$7e,$6a,$7e,$5e,$00 ; Starting depot      ($24)
            .byte $24,$3c,$24,$3c,$24,$3c,$24,$3c ; N <-> S             ($25)
            .byte $00,$00,$0f,$12,$2a,$27,$3c,$24 ; Curve E / S         ($26)
            .byte $26,$3d,$25,$3f,$25,$3c,$24,$3c ; E switchable        ($27)
            .byte $00,$7c,$fe,$fe,$fe,$fe,$7c,$00 ; Trolley W-E         ($28)
            .byte $24,$3c,$e4,$54,$48,$f0,$00,$00 ; Curve W / N         ($29)
            .byte $00,$00,$ff,$55,$55,$ff,$00,$00 ; W <-> E             ($2a)
            .byte $48,$f0,$5f,$55,$55,$ff,$00,$00 ; N switchable        ($2b)
            .byte $00,$00,$f8,$4c,$54,$e4,$3c,$24 ; Curve W / S         ($2c)
            .byte $24,$3c,$24,$bc,$64,$7c,$c4,$7c ; W switchable        ($2d)
            .byte $00,$00,$ff,$55,$55,$ff,$09,$1e ; S switchable        ($2e)
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
            .byte $00,$4e,$ca,$51,$51,$51,$4a,$ee ; 10
            .byte $00,$44,$cc,$44,$44,$44,$44,$ee ; 11
            .byte $00,$46,$c9,$41,$41,$46,$48,$ef ; 12            
            .byte $10,$78,$7c,$fe,$7f,$3e,$1e,$08 ; Trolley NW-SE   \   ($3d)
BITMAP_S:   .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Bitmap Source       ($3e)
BITMAP_D:   .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Bitmap Destination  ($3f)
