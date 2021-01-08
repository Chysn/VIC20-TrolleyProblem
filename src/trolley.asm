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
SCRCOM      = 142               ; Screen color
TXTCOL      = $01               ; Text color
UP          = $01               ; Directional constants
RIGHT       = $02               ; ,,
DOWN        = $03               ; ,,
LEFT        = $04               ; ,,
FIRE        = $05               ; Joystick fire button pressed
LO_SPEED    = $06               ; Player speed (delay per pixel, in jiffies)
SEALIFE_GEN = $30               ; Sealife generation frequency (jiffies)
O2_START    = $20               ; Starting oxygen
O2_RATE     = $80               ; Oxygen depletion rate (jiffies)

; Score constants
PICKUP      = 50                ; Score when a med pack is picked up
DELIVERY    = 100               ; Score when a med pack is delivered
O2_ADD      = 12                ; Oxygen added when surfacing

; Character constants
CO_PLAYER   = $07               ; Player color
CHAR_S      = $2e               ; Source bitmap character code
CHAR_D      = $2f               ; Destination bitmap character code
LANDCHAR    = $25               ; Land and sky character
TOPLAND     = $26               ; Top land character
CO_SKY      = $03               ; Sky color
MEDPACK     = $1b               ; Med Pack
SEABASE     = $00               ; Sea base
FISH        = $27               ; School of fish
CO_FISH     = $05               ; Fish color
O2_CHAR     = $3a               ; Oxygen icon

; wAxScore Constants
NO_EFFECT   = $0f
LEGATO_ON   = $3f
LEGATO_OFF  = $4f

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
BACKGD      = $900f             ; Background color
VIA1DD      = $9113             ; Data direction register for joystick
VIA1PA      = $9111             ; Joystick port (up, down, left, fire)
VIA2DD      = $9122             ; Data direction register for joystick
VIA2PB      = $9120             ; Joystick port (for right)
CLSR        = $e55f             ; Clear screen/home
HOME        = $e581             ; Home text
TCOLOR      = $0286             ; Text color
PRTSTR      = $cb1e             ; Print from data (Y,A)
CASECT      = $0291             ; Disable Commodore case
PRTFIX      = $ddcd             ; Decimal display routine (A,X)
CHROUT      = $ffd2             ; Output one character
TIME_L      = $a2               ; Jiffy counter low
TIME_M      = $a1               ; Jiffy counter middle  

; Game Memory
CURSOR      = $f9               ; Cursor (2 bytes)
PLAYER      = $fb               ; Player location (2 bytes)
BASE_COL    = $fd               ; Base color pointer (2 bytes)            

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
            bne show_score      ;   ,,
            lda SCORE           ;   ,,
            cmp HISCORE         ;   ,,
            bcc ShowScore       ;   ,,
new_hs:     lda SCORE           ; A new high score has been
            sta HISCORE         ; achived; update high score
            lda SCORE+1         ; ,,
            sta HISCORE+1       ; ,,                
            ; Fall through to ShowScore

ShowScore:  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup Hardware
SetupHW:    lda #$ff            ; Set custom character location
            sta VICCR5          ; ,,
            lda #$00            ; Initialize sound registers
            sta VOICEM          ; ,,
            sta VOICEH          ; ,,
            sta VOICEL          ; ,,
            sta NOISE           ; ,,
            lda #$7f            ; Set DDR to read East
            sta VIA2DD          ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            lda #TXTCOL         ; Set color of screen text, like
            sta TCOLOR          ;   intro, game over, score, etc.
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
CharSet:    .byte $e7,$81,$81,$18,$18,$81,$81,$e7 ; Track Placeholder Diagnostic
            ;byte $00,$00,$00,$00,$00,$00,$00,$00 ; Track Placeholder
            .byte $00,$3c,$18,$24,$42,$7e,$46,$42 ; A
            .byte $00,$7c,$22,$22,$3c,$22,$22,$7c ; B
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
            .byte $00,$00,$3c,$42,$42,$42,$3c,$00 ; O
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
            .byte $24,$3c,$e4,$54,$48,$f0,$00,$00 ; Curve E / N         ($23)
            .byte $66,$bb,$dd,$55,$65,$b9,$de,$57 ; Water (multicolor)  ($24)
            .byte $24,$3c,$24,$3c,$24,$3c,$24,$3c ; N <-> S             ($25)
            .byte $00,$00,$f8,$4c,$54,$e4,$3c,$24 ; Curve E / S         ($26)
            .byte $24,$3c,$27,$3d,$25,$3f,$24,$3c ; E                   ($27)
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; <unused>            ($28)
            .byte $24,$3c,$27,$2a,$12,$0f,$00,$00 ; Curve W / N         ($29)
            .byte $00,$00,$ff,$55,$55,$ff,$00,$00 ; W <-> E             ($2a)
            .byte $24,$24,$ff,$55,$55,$ff,$00,$00 ; N                   ($2b)
            .byte $00,$00,$0f,$12,$2a,$27,$3c,$24 ; Curve W / S         ($2c)
            .byte $24,$3c,$e4,$7c,$64,$fc,$24,$3c ; W                   ($2d)
            .byte $00,$00,$ff,$55,$55,$ff,$24,$3c ; S                   ($2e)
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
