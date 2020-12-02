.ifndef VSYNC
.include "atari2600.inc"
.endif
.include "bank.inc"
.include "ram.inc"
.include "sprites.inc"
.include "playfield.inc"

.org $D000
.segment "BANK6"
Reset6:
   bit BANK0
   nop
   nop
   nop
jump_b6_b1:
   bit BANK1
   nop
   nop
   nop
jump_b6_b2:
   bit BANK2
   nop
   nop
   nop
jump_b6_b3:
   bit BANK3
   nop
   nop
   nop
jump_b6_b4:
   bit BANK4
   nop
   nop
   nop
jump_b6_b5:
   bit BANK5
   nop
   nop
   nop
   nop
   nop
   nop
   ; Bank 6 entry:
   jmp start_bank6
jump_b6_b7:
   bit BANK7

   ; Graphics Data

digits1_6:
   DIGITS_1

digits02_6:
   DIGITS_02

; Pre-level b6 init

start_bank6:
   lda #0
   sta FRAME_CTR
   sta AUDV0

level_b6:

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
   jmp level_b6

; More graphics

.org $DFFA
.segment "VECTORS6"
.word Reset6          ; NMI
.word Reset6          ; RESET
.word Reset6          ; IRQ
