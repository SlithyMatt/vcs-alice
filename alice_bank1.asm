.ifndef VSYNC
.include "atari2600.inc"
.endif
.include "bank.inc"
.include "ram.inc"
.include "sprites.inc"
.include "playfield.inc"

.macro LEVEL1_LOOP_INC_OFFSET
   inx
   cpx #22
   bne :+
   ldx #0
:  lda #0
.endmacro

.org $3000
.segment "BANK1"
Reset1:
   bit BANK0
   nop
   nop
   nop
   nop
   nop
   nop
   ; Bank 1 entry:
   jmp start_bank1
   bit BANK2

   ; Graphics Data

digits1_1:
   DIGITS_1

digits02_1:
   DIGITS_02

; Pre-level 1 init

start_bank1:
   lda #<falling_sprites_1
   sta PTR1
   lda #>falling_sprites_1
   sta PTR1+1
   lda #0
   sta FRAME_CTR
   sta OFFSET
   sta JUMP_CD
   sta RELEASED
   sta START
   sta COUNTER
   sta COLUBK
   sta HIDE_CAKE_12
   sta HIDE_CAKE_34
   sta HIDE_UMBRELLA
   lda #$10 ; dark brown
   sta COLUPF
   lda #$66 ; purple
   sta COLUP1

level2:

; Start of vertical blank processing
   lda #0
   sta VBLANK
   sta PF0
   sta PF1
   sta PF2
   sta GRP1

   lda #2
   sta VSYNC
; 3 scanlines of VSYNCH signal...
   sta WSYNC
   lda START
   bne @started
   ldx #6
   stx START
:  dex
   bne :-
   sta RESP0
@started:
   sta WSYNC
   bit DEAD
   bmi @frame_set
   inc FRAME_CTR
   lda #$07
   bit FRAME_CTR
   bne @frame_set
   inc OFFSET
   lda COUNTER
   cmp #6
   bpl @check_stop
   lda OFFSET
   cmp #22
   bne @frame_set
   lda #0
   sta OFFSET
   inc COUNTER
   jmp @frame_set
@check_stop:
   lda OFFSET
   cmp #61
   bne @frame_set
   dec OFFSET
@frame_set:
   lda OFFSET
   cmp #60
   bne @play
   jmp end_level2
@play:
   sta WSYNC
   lda #0
   sta VSYNC
   sta HMP0



; 37 scanlines of vertical blank...

   ldx #31
@vblank_loop:
   sta WSYNC
   dex
   bne @vblank_loop

   lda #$FF
   sta INDEX
   bit HIDE_CAKE_12
   bmi @check_bonus_2
   lda COUNTER
   cmp #0
   bne @check_bonus_1
   lda OFFSET
   cmp #13
   bmi @no_bonus
   jmp @p1_set_right
@check_bonus_1:
   lda COUNTER
   cmp #1
   bne @check_bonus_2
   lda OFFSET
   cmp #13
   bpl @no_bonus
   jmp @p1_set_right
@check_bonus_2:
   bit HIDE_CAKE_12
   bvs @check_counter_3
   lda COUNTER
   cmp #2
   bne @check_counter_3
   lda OFFSET
   cmp #5
   bmi @no_bonus
   jmp @p1_set_left
@check_counter_3:
   lda COUNTER
   cmp #3
   bne @check_bonus_3
   bit HIDE_CAKE_12
   bvs @no_bonus_2
   lda OFFSET
   cmp #5
   bpl @no_bonus_2
   jmp @p1_set_left
@no_bonus:
   sta WSYNC
   jmp @p1_set
@no_bonus_2:
   bit HIDE_CAKE_34
   bmi @no_bonus
   lda OFFSET
   cmp #13
   bmi @no_bonus
   jmp @p1_set_right
@check_bonus_3:
   bit HIDE_CAKE_34
   bmi @check_bonus_4
   lda COUNTER
   cmp #4
   bne @check_bonus_4
   lda OFFSET
   cmp #13
   bmi @p1_set_right
   sta WSYNC
   jmp @p1_set
@check_bonus_4:
   bit HIDE_CAKE_34
   bvs @check_umbrella
   lda COUNTER
   cmp #5
   bne @check_counter_6
   lda OFFSET
   cmp #5
   bpl @p1_set_left
   sta WSYNC
   jmp @p1_set
@check_counter_6:
   lda COUNTER
   cmp #6
   bne @no_bonus
   lda OFFSET
   cmp #5
   bmi @p1_set_left
@check_umbrella:
   bit HIDE_UMBRELLA
   bmi @p1_set
   lda OFFSET
   cmp #22
   bpl @set_umbrella
   sta WSYNC
   jmp @p1_set
@set_umbrella:
   lda #$1C
   sta COLUP1
   sta WSYNC
   ldx #5
:  dex
   bne :-
   sta RESP1
   lda #42
   sta INDEX
   lda OFFSET
   cmp #40
   bmi @set_umbrella_up
   cmp #42
   bpl @set_umbrella_up
   lda #<level2_umbrella_down
   sta PTR2
   lda #>level2_umbrella_down
   sta PTR2+1
   jmp @p1_set
@set_umbrella_up:
   lda #<level2_umbrella_up
   sta PTR2
   lda #>level2_umbrella_up
   sta PTR2+1
   jmp @p1_set
@p1_set_left:
   lda #4
   sta INDEX
   sta WSYNC
   ldx #6
:  dex
   bne :-
   sta RESP1
   jmp @set_cake_ptr
@p1_set_right:
   lda #12
   sta INDEX
   sta WSYNC
   ldx #11
:  dex
   bne :-
   nop
   nop
   sta RESP1
@set_cake_ptr:
   lda INDEX
   sec
   sbc OFFSET
   cmp #1
   beq @set_cake_down
   cmp #2
   beq @set_cake_down
   lda #<level2_cake_up
   sta PTR2
   lda #>level2_cake_up
   sta PTR2+1
   jmp @p1_set
@set_cake_down:
   lda #<level2_cake_down
   sta PTR2
   lda #>level2_cake_down
   sta PTR2+1
@p1_set:
   sta WSYNC
   bit DEAD
   bmi @movement_set

   bit SWCHA
   bmi @check_left
   lda #$F0
   sta HMP0
   jmp @movement_set
@check_left:
   bvs @movement_set
   lda #$10
   sta HMP0
@movement_set:
   sta WSYNC
   sta HMOVE
   lda #$07
   bit FRAME_CTR
   bne @alice_frame_set
   lda #<falling_sprites_1
   sta PTR1
   lda #>falling_sprites_1
   sta PTR1+1
   lda #$08
   bit FRAME_CTR
   beq @alice_frame_set
   clc
   lda PTR1
   adc #32
   sta PTR1
   lda PTR1+1
   adc #0
   sta PTR1+1
@alice_frame_set:
   sta WSYNC
   lda #0
   sta PF2_R
   ldx OFFSET
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1_R
   rol PF2_R
   sta WSYNC

; 192 scanlines of picture...

   SCORE digits1_1, digits02_1
   lda #$10 ; dark brown
   sta COLUPF

   ; First 8 lines: just playfield
   ldx OFFSET
   ldy #8
   lda COUNTER
   cmp #6
   bmi @top8
   jmp @top8_bottom
@top8:
   sta WSYNC
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   dey
   cpx INDEX
   bne @top8_no_p1
   lda (PTR2),y
   sta GRP1
   jmp @top8_p1_set
@top8_no_p1:
   lda #0
   sta GRP1
@top8_p1_set:
   nop
   nop
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   cpy #0
   bne @top8
   LEVEL1_LOOP_INC_OFFSET
   sta PF2_R
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1_R
   sta PF1
   rol PF2_R
   lda PF2_R
   sta PF2
   lda (PTR1),y
   sta COLUP0
   iny
   lda (PTR1),y
   sta GRP0
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   iny

   ; Lines 8-15: playfield + upper sprite
@start_alice:
   cpx INDEX
   bne @alice_no_p1
   lda (PTR2),y
   jmp @alice_p1_set
@alice_no_p1:
   lda #0
@alice_p1_set:
   sta WSYNC
   sta GRP1
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   lda (PTR1),y
   sta COLUP0
   iny
   lda (PTR1),y
   sta GRP0
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   iny
   cpy #16
   bne @start_alice
   LEVEL1_LOOP_INC_OFFSET
   sta PF2_R
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1
   sta PF1_R
   rol PF2_R
   lda PF2_R
   sta PF2
   iny
   lda (PTR1),y
   sta GRP0
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   iny

   ; Lines 16-31: playfield + lower sprite
@start_lower_alice:
   cpx INDEX
   bne @lower_alice_no_p1
   lda (PTR2),y
   jmp @lower_alice_p1_set
@lower_alice_no_p1:
   lda #0
@lower_alice_p1_set:
   sta WSYNC
   sta GRP1
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   lda (PTR1),y
   sta COLUP0
   iny
   lda (PTR1),y
   sta GRP0
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   iny
   cpy #32
   bne @start_lower_alice
   LEVEL1_LOOP_INC_OFFSET
   sta PF2_R
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1_R
   sta PF1
   rol PF2_R
   lda PF2_R
   sta PF2
   lda #0
   sta GRP0
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   sta WSYNC

; Lines 32-192: Below Alice
   ldy #7
   jmp @below_row_skip_wsync
@below_row_loop:
   sta WSYNC
@below_row_skip_wsync:
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   cpx INDEX
   bne @loop_no_p1
   lda (PTR2),y
   sta GRP1
   jmp @loop_p1_set
@loop_no_p1:
   lda #0
   sta GRP1
@loop_p1_set:
   nop
   nop
   nop
   nop
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   dey
   bne @below_row_loop
   LEVEL1_LOOP_INC_OFFSET
   cpx OFFSET
   beq @end_screen
   sta PF2_R
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1_R
   sta PF1
   rol PF2_R
   lda PF2_R
   sta PF2
   nop
   nop
   nop
   nop
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   ldy #7
   jmp @below_row_loop

@end_screen:
   sta WSYNC
   lda #%00000010
   sta VBLANK                     ; end of screen - enter blanking

; 30 scanlines of overscan...
   ldx #27
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop

   bit DEAD
   bpl @check_pf
   bit INPT4
   bmi @check_pf
   lda #0
   sta DEAD
   sta SCORE_1
   sta SCORE_100
   sta SCORE_10K
   sta WSYNC
   sta WSYNC
   sta WSYNC
   jmp start_bank1
@check_pf:
   sta WSYNC
   bit CXP0FB
   bpl @check_p1
   lda #$80
   sta DEAD
   sta WSYNC
   jmp @continue_level2
@check_p1:
   bit CXPPMM
   bpl @continue_level2
   ADD_SCORE 25
   sta WSYNC
   lda OFFSET
   cmp #10
   bmi @take_left_cake
   cmp #41
   bpl @take_umbrella
   lda COUNTER
   cmp #1
   beq @take_cake_1
   lda #$80
   sta HIDE_CAKE_34
   jmp @continue_level2
@take_cake_1:
   lda #$80
   sta HIDE_CAKE_12
   jmp @continue_level2
@take_left_cake:
   lda COUNTER
   cmp #3
   beq @take_cake_2
   lda #$C0
   sta HIDE_CAKE_34
   jmp @continue_level2
@take_cake_2:
   lda #$C0
   sta HIDE_CAKE_12
   jmp @continue_level2
@take_umbrella:
   ADD_SCORE 175
   lda #$80
   sta HIDE_UMBRELLA
@continue_level2:
   sta WSYNC
   sta CXCLR
   jmp level2

@top8_bottom:
   sta WSYNC
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   dey
   clc
   lda OFFSET
   adc #22
   sta LAST
   cpx INDEX
   bne @top8_bottom_no_p1
   lda (PTR2),y
   sta GRP1
   jmp @top8_bottom_p1_set
@top8_bottom_no_p1:
   lda #0
   sta GRP1
@top8_bottom_p1_set:
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   cpy #0
   bne @top8_bottom
   inx
   lda #0
   sta PF2_R
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1
   sta PF1_R
   rol PF2_R
   lda PF2_R
   sta PF2
   lda (PTR1),y
   sta COLUP0
   iny
   lda (PTR1),y
   sta GRP0
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   iny

   ; Lines 8-15: playfield + upper sprite
@start_alice_bottom:
   cpx INDEX
   bne @alice_bottom_no_p1
   lda (PTR2),y
   jmp @alice_bottom_p1_set
@alice_bottom_no_p1:
   lda #0
@alice_bottom_p1_set:
   sta WSYNC
   sta GRP1
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   lda (PTR1),y
   sta COLUP0
   iny
   lda (PTR1),y
   sta GRP0
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   iny
   cpy #16
   bne @start_alice_bottom
   inx
   lda #0
   sta PF2_R
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1
   sta PF1_R
   rol PF2_R
   lda PF2_R
   sta PF2
   iny
   lda (PTR1),y
   sta GRP0
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   iny

   ; Lines 16-31: playfield + lower sprite
@start_lower_alice_bottom:
   cpx INDEX
   bne @lower_alice_bottom_no_p1
   lda (PTR2),y
   jmp @lower_alice_bottom_p1_set
@lower_alice_bottom_no_p1:
   lda #0
@lower_alice_bottom_p1_set:
   sta WSYNC
   sta GRP1
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   lda (PTR1),y
   sta COLUP0
   iny
   lda (PTR1),y
   sta GRP0
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   iny
   cpy #32
   bne @start_lower_alice_bottom
   inx
   lda #0
   sta PF2_R
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1_R
   sta PF1
   rol PF2_R
   lda PF2_R
   sta PF2
   lda #0
   sta GRP0
   nop
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   sta WSYNC

   ldy #7
   jmp @below_row_skip_wsync_bottom
@below_row_loop_bottom:
   sta WSYNC
@below_row_skip_wsync_bottom:
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   cpx #73
   beq @floor
   cpx INDEX
   bne @bottom_loop_no_p1
   lda (PTR2),y
   sta GRP1
   jmp @bottom_loop_p1_set
@bottom_loop_no_p1:
   lda #0
   sta GRP1
@bottom_loop_p1_set:
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   dey
   bne @below_row_loop_bottom
   inx
   cpx LAST
   beq @end_screen_bottom
   lda #0
   sta PF2_R
   lda level1_terrain,x
   sec
   ror
   rol PF2_R
   sec
   ror
   sta PF1_R
   sta PF1
   rol PF2_R
   lda PF2_R
   sta PF2
   nop
   nop
   nop
   nop
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   ldy #7
   jmp @below_row_loop_bottom
@end_screen_bottom:
   jmp @end_screen

@floor:
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   dey
@last_terrain:
   sta WSYNC
   lda PF1_R
   sta PF1
   lda PF2_R
   sta PF2
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   lda PF1_R
   eor #$ff
   sta PF1
   lda PF2_R
   eor #$0f
   sta PF2
   dey
   bne @last_terrain
   inx
   cpx LAST
   beq @end_screen_bottom
@above_door:
   ldy #8
@above_door_row:
   sta WSYNC
   lda #$FF
   sta PF1
   lda #0
   sta PF2
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   sta PF1
   dey
   bne @above_door_row
   inx
   cpx LAST
   beq @end_screen_bottom
   cpx #79
   bne @above_door
@floor_start:
   ldy #8
@floor_row:
   sta WSYNC
   lda #0
   sta PF0
   lda #$FF
   sta PF1
   sta PF2
   nop
   nop
   nop
   nop
   nop
   nop
   lda #$F0
   sta PF0
   dey
   bne @floor_row
   inx
   cpx LAST
   bne @floor_start
   jmp @end_screen_bottom

end_level2:
   sta WSYNC
   lda #0
   sta VSYNC
   sta HMP0

; 37 scanlines of vertical blank...

   ldx #36
@vblank_loop:
   sta WSYNC
   dex
   bne @vblank_loop

   ldx #60

@above_alice:
   sta WSYNC

; TODO screen lines
   ldx #192
@screen_line:
   sta WSYNC
   dex
   bne @screen_line

   lda #$02
   sta VBLANK

; 30 lines overscan
   ldx #30
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop

   jmp level2

; More graphics

falling_sprites_1:
   FALLING_SPRITES

.res 13 ; filler

level1_terrain:
.byte $F0
.byte $F0
.byte $E0
.byte $C0
.byte $C0
.byte $E0
.byte $E0
.byte $F0
.byte $F8
.byte $F8
.byte $FC
.byte $FE
.byte $FE
.byte $FE
.byte $FC
.byte $FC
.byte $FC
.byte $F8
.byte $F8
.byte $F8
.byte $F0
.byte $F0
.byte $E0
.byte $C0
.byte $80
.byte $00
.byte $00
.byte $00
.byte $00
.byte $80
.byte $80
.byte $C0
.byte $E0
.byte $F0
.byte $F8
.byte $FC
.byte $FE
.byte $FF
.byte $F0
.byte $C0
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00
.byte $80
.byte $80
.byte $E0
.byte $F8
.byte $FE
.byte $FE
.byte $FF
.byte $FF
.byte $FF
.byte $FE
.byte $FE
.byte $FC
.byte $FC
.byte $FC
.byte $F8
.byte $F8
.byte $F8
.byte $F0
.byte $F0
.byte $F0
.byte $F0
.byte $F0
.byte $F0
.byte $F0
.byte $F0
.byte $F0
.byte $F8


level2_cake_up:
.byte $D8
.byte $D8
.byte 0
.byte 0
.byte $D8
.byte $D8
.byte $20
.byte 0

level2_cake_down:
.repeat 2
.byte 0,0
.byte $20,0
.byte $D8,0
.byte $D8,0
.byte 0,0
.byte 0,0
.byte $D8,0
.byte $D8,0
.endrepeat

level2_umbrella_up:
.byte $0C
.byte $08
.byte $08
.byte $08
.byte $2A
.byte $1C
.byte $08
.byte 0

level2_umbrella_down:
.repeat 2
.byte 0,0
.byte $08,0
.byte $1C,0
.byte $2A,0
.byte $08,0
.byte $08,0
.byte $08,0
.byte $0C,0
.endrepeat


.org $3FFA
.segment "VECTORS1"
.word Reset1          ; NMI
.word Reset1          ; RESET
.word Reset1          ; IRQ
