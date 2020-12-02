.ifndef VSYNC
.include "atari2600.inc"
.endif
.include "bank.inc"
.include "ram.inc"
.include "sprites.inc"
.include "playfield.inc"

.org $F000
.segment "BANK7"
Reset7:
   bit BANK0
   nop
   nop
   nop
jump_b7_b1:
   bit BANK1
   nop
   nop
   nop
jump_b7_b2:
   bit BANK2
   nop
   nop
   nop
jump_b7_b3:
   bit BANK3
   nop
   nop
   nop
jump_b7_b4:
   bit BANK4
   nop
   nop
   nop
jump_b7_b5:
   bit BANK5
   nop
   nop
   nop
jump_b7_b6:
   bit BANK6
   nop
   nop
   nop
   nop
   nop
   nop
   ; Bank 7 entry:
   jmp start_bank7

   ; Graphics Data

digits1_7:
   DIGITS_1

digits02_7:
   DIGITS_02

; Pre-level b7 init

start_bank7:
   lda #0
   sta FRAME_CTR
   sta AUDV0

level_b7:

; Start of vertical blank processing
   lda #0
   sta VBLANK
   sta PF0
   sta PF1
   sta PF2
   sta GRP0
   sta GRP1

   lda #2
   sta VSYNC
; 3 scanlines of VSYNCH signal...
   sta WSYNC
   sta WSYNC
   sta WSYNC
   lda #0
   sta VSYNC

; 37 scanlines of vertical blank...

   ldx #36
@vblank_loop:
   sta WSYNC
   dex
   bne @vblank_loop

; 192 scanlines of picture...
@score:
   sta WSYNC
   SCORE digits1_2, digits02_2

; just blank
   ldx #176
@screen_loop:
   sta WSYNC
   dex
   bne @screen_loop

; 30 lines overscan
   ldx #29
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop

@finish_oscan:
   sta WSYNC
   jmp level_b7

; More graphics

.org $FFFA
.segment "VECTORS7"
.word Reset7          ; NMI
.word Reset7          ; RESET
.word Reset7          ; IRQ
