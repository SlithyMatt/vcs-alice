.ifndef VSYNC
.include "atari2600.inc"
.endif
.include "bank.inc"
.include "ram.inc"
.include "sprites.inc"
.include "playfield.inc"

.org $9000
.segment "BANK4"
Reset4:
   bit BANK0
   nop
   nop
   nop
jump_b4_b1:
   bit BANK1
   nop
   nop
   nop
jump_b4_b2:
   bit BANK2
   nop
   nop
   nop
jump_b4_b3:
   bit BANK3
   nop
   nop
   nop
   nop
   nop
   nop
   ; Bank 4 entry:
   jmp start_bank4
jump_b4_b5:
   bit BANK5

   ; Graphics Data

digits1_4:
   DIGITS_1

digits02_4:
   DIGITS_02

; Pre-level b4 init

start_bank4:
   lda #0
   sta FRAME_CTR
   sta AUDV0

level_b4:

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
   SCORE digits1_4, digits02_4

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
   jmp level_b4

; More graphics

.org $9FFA
.segment "VECTORS4"
.word Reset4          ; NMI
.word Reset4          ; RESET
.word Reset4          ; IRQ
