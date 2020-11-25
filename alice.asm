.include "atari2600.inc"
.include "bank.inc"
.include "ram.inc"
.include "sprites.inc"
.include "playfield.inc"
.include "score.inc"

; Constants


.org $1000
.segment "STARTUP"

Reset:
   jmp start
   jmp start
jump_b0b1:
   bit BANK1

start:
   ldx #0
   lda #0
Clear:
   sta 0,x
   inx
   bne Clear

   ; Initialize graphics
   lda #<side_sprites_0
   sta PTR1
   lda #>side_sprites_0
   sta PTR1+1
   lda #<rabbit_sprites_0
   sta PTR2
   lda #>rabbit_sprites_0
   sta PTR2+1
   lda #$02
   sta SHOW_BL
   sta SHOW_M0
   sta SHOW_M1

StartOfFrame:

; Start of vertical blank processing
   lda #%01000000
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

; 192 scanlines of picture...

   lda #$0E ; white
   sta COLUBK
   lda #0 ; black
   sta COLUPF
   ldx #4
:  sta WSYNC
   dex
   bne :-
   ; "slithy GAMES"
   PFLINE 2,$B0,$2E,$55,$E0,$76,$DB
   PFLINE 2,$90,$2E,$55,$20,$55,$4A
   PFLINE 2,$B0,$24,$77,$A0,$75,$DA
   PFLINE 2,$A0,$A4,$25,$A0,$54,$8A
   PFLINE 2,$B0,$A4,$25,$E0,$54,$DA
   lda #0
   sta PF0
   sta PF1
   sta PF2
   ldx #4
:  sta WSYNC
   dex
   bne :-
   lda #0 ; black
   sta COLUBK
   lda #$96 ; blue
   sta COLUPF
   ldx #20
:  sta WSYNC
   dex
   bne :-
   ; "ALICE"
   PFLINE 4,$00,$02,$82,$30,$31,$1F
   PFLINE 4,$00,$06,$82,$30,$79,$1F
   PFLINE 4,$00,$0E,$02,$10,$CC,$11
   PFLINE 4,$00,$0A,$02,$10,$84,$11
   PFLINE 4,$00,$1A,$02,$90,$84,$01
   PFLINE 4,$00,$32,$02,$90,$00,$01
   PFLINE 6,$00,$22,$02,$90,$00,$01
   PFLINE 6,$00,$7E,$02,$90,$00,$07
   PFLINE 4,$00,$42,$02,$90,$00,$01
   PFLINE 4,$00,$C2,$02,$90,$00,$01
   PFLINE 4,$00,$82,$02,$90,$00,$01
   PFLINE 4,$80,$82,$02,$90,$00,$01
   PFLINE 4,$80,$02,$02,$90,$84,$01
   PFLINE 4,$80,$02,$02,$10,$84,$01
   PFLINE 4,$80,$02,$02,$10,$CC,$11
   PFLINE 4,$80,$02,$02,$10,$48,$11
   PFLINE 4,$80,$02,$BE,$30,$79,$1F
   PFLINE 4,$80,$02,$BE,$30,$31,$1F
   lda #0
   sta PF0
   sta PF1
   sta PF2
   sta WSYNC
   lda #$0E ; white
   sta COLUPF
   sta WSYNC
   sta WSYNC
   sta WSYNC
   ; "IN"
   PFLINE 2,$00,$00,$70,$90,$00,$00
   PFLINE 4,$00,$00,$20,$B0,$00,$00
   PFLINE 2,$00,$00,$20,$D0,$00,$00
   PFLINE 2,$00,$00,$70,$90,$00,$00
   lda #0
   sta PF0
   lda #$1C ; yellow
   sta COLUPF
   ldx #6
:  sta WSYNC
   dex
   bne :-
   ; "WONDERLAND"
   PFLINE 2,$10,$92,$8A,$D0,$00,$00
   PFLINE 2,$10,$92,$9A,$D0,$00,$00
   PFLINE 2,$50,$BA,$BA,$C0,$80,$00
   PFLINE 2,$50,$AB,$AA,$40,$80,$00
   PFLINE 2,$50,$AB,$AB,$40,$80,$00
   PFLINE 2,$50,$AA,$AB,$D0,$80,$00
   PFLINE 4,$F0,$AA,$AA,$D0,$00,$00
   PFLINE 2,$F0,$AA,$AA,$C0,$89,$14
   PFLINE 2,$B0,$AA,$AA,$40,$89,$34
   PFLINE 2,$B0,$BA,$AA,$40,$9D,$74
   PFLINE 2,$B0,$BA,$BA,$40,$95,$55
   PFLINE 2,$A0,$12,$9A,$50,$95,$57
   PFLINE 2,$A0,$12,$8A,$50,$95,$56
   PFLINE 4,$00,$00,$00,$00,$9D,$54
   PFLINE 2,$00,$00,$00,$00,$95,$54
   PFLINE 2,$00,$00,$00,$00,$95,$74
   PFLINE 2,$00,$00,$00,$00,$D5,$34
   PFLINE 2,$00,$00,$00,$00,$D5,$14
   lda #0
   sta PF1
   sta PF2
   ldx #18
:  sta WSYNC
   dex
   bne :-

   lda #%01000010
   sta VBLANK                     ; end of screen - enter blanking

   ; 30 scanlines of overscan...
   ldx #30
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop

   bit INPT4
   bpl level1

   jmp StartOfFrame

; Level 1: Above ground, chasing white rabbit
level1:
   lda #0
   sta VBLANK
   sta OFFSET
   sta HMP0
   sta HMP1
   lda #2
   sta VSYNC
; 3 scanlines of VSYNCH signal...
   inc FRAME_CTR
   sta WSYNC
   lda JUMP_CD
   beq @check_jump
   dec JUMP_CD
@jump:
   lda #96
   sta OFFSET
   jmp @alice_frame_set
@check_jump:
   bit INPT4
   bmi @release
   lda RELEASED
   beq @walk
   lda #10
   sta JUMP_CD
   lda #0
   sta RELEASED
   jmp @jump
@release:
   lda #1
   sta RELEASED
@walk:
   bit SWCHA
   bmi @alice_frame_set
   lda #$08
   bit FRAME_CTR
   beq @alice_frame_set
   lda #48
   sta OFFSET
@alice_frame_set:
   sta WSYNC
   clc
   lda #<side_sprites_0
   adc OFFSET
   sta PTR1
   lda #>side_sprites_0
   adc #0
   sta PTR1+1
   lda #0
   sta OFFSET
   lda COUNTER
   cmp #100
   bmi @pause_rabbit
   cmp #157
   bpl @stop_rabbit
   lda #$07
   bit FRAME_CTR
   bne @rabbit_frame_set
   inc COUNTER
   lda #$08
   bit FRAME_CTR
   bne @rabbit_frame_set
   lda #48
   sta OFFSET
   lda #$D0
   sta HMP1
   jmp @rabbit_frame_set
@pause_rabbit:
   sta CXCLR
   inc COUNTER
   jmp @rabbit_frame_set
@stop_rabbit:
   lda #96
   sta OFFSET
@rabbit_frame_set:
   sta WSYNC
   lda #$07
   bit FRAME_CTR
   bne @do_vsync
   clc
   lda #<rabbit_sprites_0
   adc OFFSET
   sta PTR2
   lda #>rabbit_sprites_0
   adc #0
   sta PTR2+1
@do_vsync:
   lda #0
   sta VSYNC
; 37 scanlines of vertical blank...
   sta WSYNC
   bit SWCHA
   bmi @alice_movement_set
   lda #$07
   bit FRAME_CTR
   bne @alice_movement_set
   lda #$F0
   sta HMP0
@alice_movement_set:
   sta WSYNC
   sta WSYNC
   sta HMOVE
   sta WSYNC
   ldx #32
@vblank_loop:
   sta WSYNC
   dex
   bne @vblank_loop
   sta WSYNC

   ; playfield
   SCORE digits1_0, digits02_0

   lda #$9C
   sta COLUBK
   sta WSYNC
   sta WSYNC
   sta WSYNC
   lda #$C4 ; dark green
   sta COLUPF
   PFLINE 1,0,$02,$01
   PFLINE 1,0,$01,$00
   PFLINE 1,0,$07,$05
   PFLINE 1,0,$02,$07
   PFLINE 1,0,$0D,$0B
   PFLINE 1,0,$0B,$0D
   PFLINE 1,0,$15,$16
   PFLINE 1,0,$1F,$1F
   PFLINE 1,0,$0F,$0B
   PFLINE 1,0,$1F,$1F
   PFLINE 1,0,$17,$1F
   PFLINE 1,0,$3F,$37
   PFLINE 1,0,$2F,$2F
   PFLINE 1,0,$3F,$3F
   PFLINE 1,0,$1F,$1F
   PFLINE 2,0,$3F,$3F
   PFLINE 1,0,$5F,$7F
   PFLINE 1,0,$3F,$5F
   PFLINE 1,0,$7F,$7F
   PFLINE 1,0,$3F,$3F
   PFLINE 1,0,$7F,$7F
   PFLINE 1,0,$5F,$3F
   PFLINE 1,0,$7F,$5F
   PFLINE 1,0,$7F,$3F
   PFLINE 1,0,$3F,$7F
   PFLINE 1,0,$7F,$7F
   PFLINE 1,0,$BF,$3F
   PFLINE 1,0,$5F,$7F
   PFLINE 1,0,$FF,$BF
   PFLINE 1,0,$BF,$5F
   PFLINE 1,0,$7F,$FF
   PFLINE 1,0,$BF,$5F
   PFLINE 1,0,$7F,$BF
   PFLINE 1,0,$5F,$FF
   PFLINE 1,0,$FF,$3F
   PFLINE 1,0,$5F,$DF
   PFLINE 1,0,$BF,$7F
   PFLINE 1,0,$7F,$3F
   PFLINE 1,0,$5F,$FF
   PFLINE 1,0,$BF,$5F
   PFLINE 1,0,$57,$AB
   lda #$14 ; medium brown
   sta COLUPF
   PFLINE 27,0,$03,$01
   lda #$C8 ; light green
   sta COLUBK
   PFLINE 36,0,$03,$01
   PFLINE 3,0,$07,$03
   PFLINE 1,0,$0F,$07
   PFLINE 1,0,$0C,$06
   PFLINE 1,0,$18,$0C
   PFLINE 1,0,$10,$08
   lda #0
   sta PF1
   sta PF2
   sta NUSIZ0
   sta NUSIZ1
   sta CTRLPF
   ldx #16
:  sta WSYNC
   dex
   bne :-
   lda #$18 ; light brown
   sta COLUBK
   sta WSYNC
   lda SHOW_M0
   sta ENAM0
   lda SHOW_M1
   sta ENAM1
   lda SHOW_BL
   sta ENABL
   lda #$66 ; purple
   sta COLUP0
   sta COLUP1
   sta COLUPF
   ldx #4
:  dex
   bne :-
   sta RESM0
   nop
   nop
   sta RESM1
   nop
   nop
   sta RESBL
   sta WSYNC
   sta WSYNC
   lda #$10
   sta NUSIZ0
   sta NUSIZ1
   sta CTRLPF

   ldy #0
   lda (PTR1),y
   sta COLUP0
   lda (PTR2),y
   sta COLUP1
   iny
   lda (PTR1),y
   sta GRP0
   lda (PTR2),y
   sta GRP1
   iny
   sta WSYNC
   lda START
   bne @move
   ldx #5
:  dex
   bne :-
   sta RESP0
   ldx #1
:  dex
   bne :-
   sta RESP1
   lda #1
   sta START
@move:
   sta WSYNC
   lda (PTR1),y
   sta COLUP0
   lda (PTR2),y
   sta COLUP1
   iny
   lda (PTR1),y
   sta GRP0
   lda (PTR2),y
   sta GRP1
   iny
   sta WSYNC
   lda (PTR1),y
   sta COLUP0
   lda (PTR2),y
   sta COLUP1
   iny
   lda (PTR1),y
   sta GRP0
   lda (PTR2),y
   sta GRP1
   iny
   lda #0
   sta ENAM0
   sta ENAM1
   sta ENABL
   sta WSYNC
@sprite_loop:
   lda (PTR1),y
   sta COLUP0
   lda (PTR2),y
   sta COLUP1
   iny
   lda (PTR1),y
   sta GRP0
   lda (PTR2),y
   sta GRP1
   iny
   sta WSYNC
   cpy #48
   bne @sprite_loop

   lda #0
   sta GRP0
   sta GRP1
   sta WSYNC
   sta WSYNC
   sta WSYNC
   sta WSYNC
   lda #$C8 ; light green
   sta COLUBK
   ldx #14
:  sta WSYNC
   dex
   bne :-

   lda #%00000010
   sta VBLANK                     ; end of screen - enter blanking

   lda #0
   sta COLUBK

   ; 30 scanlines of overscan...
   ldx #26
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop

   bit CXPPMM
   bpl @check_m0
   sta CXCLR
   ADD_SCORE 100
   jmp jump_b0b1
@check_m0:
   sta WSYNC
   bit CXM0P
   bvc @check_m1
   lda #0
   sta SHOW_M0
   ADD_SCORE 10
@check_m1:
   sta WSYNC
   bit CXM1P
   bpl @check_ball
   lda #0
   sta SHOW_M1
   ADD_SCORE 10
@check_ball:
   sta WSYNC
   bit CXP0FB
   bvc @continue_level1
   lda #0
   sta SHOW_BL
   ADD_SCORE 10
@continue_level1:
   sta WSYNC
   sta CXCLR
   jmp level1


; Sprites
side_sprites_0:
SIDE_SPRITES

rabbit_sprites_0:
WHITE_RABBIT_SPRITES

; Score digits
.org $1F5A
.segment "DIGITS"
digits1_0:
DIGITS_1

digits02_0:
DIGITS_02

.org $1FFA
.segment "VECTORS"

.word Reset          ; NMI
.word Reset          ; RESET
.word Reset          ; IRQ

.include "alice_bank1.asm"
.include "alice_bank2.asm"
.segment "BANK3"
.segment "VECTORS3"
.segment "BANK4"
.segment "VECTORS4"
.segment "BANK5"
.segment "VECTORS5"
.segment "BANK6"
.segment "VECTORS6"
.segment "BANK7"
.segment "VECTORS7"
