.ifndef VSYNC
.include "atari2600.inc"
.endif
.include "bank.inc"
.include "ram.inc"
.include "sprites.inc"
.include "playfield.inc"

.org $5000
.segment "BANK2"
Reset2:
   bit BANK0
   nop
   nop
   nop
jump_b2_b1:
   bit BANK1
   nop
   nop
   nop
   nop
   nop
   nop
   ; Bank 2 entry:
   jmp start_bank2
jump_b2_b3:
   bit BANK3

   ; Graphics Data

digits1_2:
   DIGITS_1

digits02_2:
   DIGITS_02

; Pre-level 1 init

start_bank2:
   lda #0
   sta FRAME_CTR
   sta AUDV0
   sta PF0
   sta PF1
   sta PF2
   sta GRP0
   sta GRP1
   sta COLUBK
   lda #192
   sta OFFSET

flooding:

; Start of vertical blank processing
   lda #0
   sta VBLANK

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

; black
   ldx OFFSET
@black_loop:
   sta WSYNC
   dex
   bne @black_loop

   lda #$94 ; sea blue
   sta COLUBK
   lda #192
   sec
   sbc OFFSET
   tax
@blue_loop:
   sta WSYNC
   dex
   bne @blue_loop

; 30 lines overscan
   lda #0 ; black
   sta COLUBK

   ldx #29
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop

@finish_oscan:
   sta WSYNC
   dec OFFSET
   beq receding
   jmp flooding

receding:
; Start of vertical blank processing
   lda #0
   sta VBLANK

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
   ldx #0
   lda #$9C ; sky blue
   sta COLUBK
   lda #$1C ; yellow
   sta COLUPF
@sky_above_sun:
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  cpx #28
   bne @sky_above_sun
@sun_top:
   lda #0
   sta PF2
   ldy #5
:  dey
   bne :-
   lda #$20
   sta PF2
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  cpx #33
   bne @sun_top
@sun_middle:
   lda #0
   sta PF2
   ldy #6
:  dey
   bne :-
   lda #$70
   sta PF2
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  cpx #43
   bne @sun_middle
@sun_bottom:
   lda #0
   sta PF2
   ldy #6
:  dey
   bne :-
   lda #$20
   sta PF2
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  cpx #48
   bne @sun_bottom
   lda #0
   sta PF2
@below_sun:
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  cpx #78
   bne @below_sun
   lda #$94 ; sea blue
   sta COLUBK
@flat_sea:
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  cpx #97
   bne @flat_sea
   lda #$9E ; foam blue
   sta COLUPF
   lda #$30
   sta PF0
   sta PF2
   lda #$0C
   sta PF1
   ldy #2
:  dey
   bne :-
   lda #$03
   sta PF0
   sta PF2
   lda #$C0
   sta PF1
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  lda #$CC
   sta PF0
   sta PF2
   lda #$33
   sta PF1
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  lda #0
   sta PF0
   sta PF1
   sta PF2
@below_waves:
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  cpx #105
   bne @below_waves
   lda #$18 ; light brown
   sta COLUPF
   lda #$81
   sta PF0
   sta PF1
   sta PF2
   ldy #3
:  dey
   bne :-
   lda #$18
   sta PF0
   sta PF1
   nop
   sta PF2
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  lda #$E7
   sta PF0
   sta PF1
   sta PF2
   ldy #3
:  dey
   bne :-
   lda #$7E
   sta PF0
   sta PF1
   nop
   nop
   sta PF2
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  lda #$18 ; light brown
   sta COLUBK
   lda #0
   sta PF0
   sta PF1
   sta PF2
@above_dunes:
   sta WSYNC
   inx
   cpx OFFSET
   bne :+
   jmp @start_flood
:  cpx #113
   bne @above_dunes
   lda #$12 ; brown
   sta COLUPF
   lda #$60
   sta PF1
   lda #$06
   sta PF2
   ldy #2
   :  dey
   bne :-
   sta PF1
   lda #$60
   sta PF0
   sta PF2
   sta WSYNC
   inx
   cpx OFFSET
   beq @start_flood
   lda #$99
   sta PF0
   sta PF1
   sta PF2
   sta WSYNC
   inx
   cpx OFFSET
   beq @start_flood
   lda #0
   sta PF0
   sta PF1
   sta PF2
@between_dunes:
   sta WSYNC
   inx
   cpx OFFSET
   beq @start_flood
   cpx #182
   bne @between_dunes
   lda #$06
   sta PF1
   lda #$60
   sta PF0
   sta PF2
   ldy #2
:  dey
   bne :-
   lda #$06
   sta PF0
   sta PF2
   lda #$60
   sta PF1
   sta WSYNC
   inx
   cpx OFFSET
   beq @start_flood
   lda #$99
   sta PF0
   sta PF1
   sta PF2
   sta WSYNC
   inx
   cpx OFFSET
   beq @start_flood
   lda #0
   sta PF0
   sta PF1
   sta PF2
@below_dunes:
   sta WSYNC
   inx
   cpx OFFSET
   beq @start_flood
   bne @below_dunes

@start_flood:
   lda #$94 ; sea blue
   sta COLUBK
   lda #0
   sta PF0
   sta PF1
   sta PF2
   lda #192
   sec
   sbc OFFSET
   tax
@flood_loop:
   sta WSYNC
   dex
   bne @flood_loop

; 30 lines overscan
   lda #0 ; black
   sta COLUBK

   ldx #29
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop

@finish_oscan:
   sta WSYNC
   inc OFFSET
   lda OFFSET
   cmp #192
   beq level3
   jmp receding

level3:
; Start of vertical blank processing
   lda #0
   sta VBLANK

   lda #2
   sta VSYNC
; 3 scanlines of VSYNCH signal...
   sta WSYNC
   sta WSYNC
   sta WSYNC
   lda #0
   sta VSYNC

; 37 scanlines of vertical blank...

   ldx #37
@vblank_loop:
   sta WSYNC
   dex
   bne @vblank_loop

; 192 visible scanlines
   SCORE digits1_2, digits02_2

   ldx #176
:  sta WSYNC
   dex
   bne :-

; 30 lines overscan
   ldx #30
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop
   jmp level3


; More graphics

.org $5FFA
.segment "VECTORS2"
.word Reset2          ; NMI
.word Reset2          ; RESET
.word Reset2          ; IRQ
