.ifndef VSYNC
.include "atari2600.inc"
.endif
.include "bank.inc"
.include "ram.inc"
.include "sprites.inc"
.include "playfield.inc"

.org $7000
.segment "BANK3"
Reset3:
   bit BANK0
   nop
   nop
   nop
jump_b3_b1:
   bit BANK1
   nop
   nop
   nop
jump_b3_b2:
   bit BANK2
   nop
   nop
   nop
   nop
   nop
   nop
   ; Bank 3 entry:
   jmp start_bank3
jump_b3_b4:
   bit BANK4

   ; Graphics Data

digits1_3:
   DIGITS_1

digits02_3:
   DIGITS_02

; Pre-level b3 init

start_bank3:
   lda #0
   sta FRAME_CTR
   sta AUDV0

level_b3:

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
   jmp level_b3

; More graphics

.org $7FFA
.segment "VECTORS3"
.word Reset3          ; NMI
.word Reset3          ; RESET
.word Reset3          ; IRQ
