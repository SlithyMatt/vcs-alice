.ifndef SCORE_INC
SCORE_INC = 1

; 16 lines of display
.macro SCORE digits1, digits02
   lda #0
   sta COLUBK
   sta PF0
   sta PF1
   sta PF2
   lda SCORE_10K
   and #$0F
   asl
   asl
   asl
   tax
   ldy #0
:  lda digits1,x
   and #$F0
   sta SCORE_PF1,y
   inx
   iny
   cpy #5
   bne :-
   lda #$96 ; blue
   sta COLUPF
   lda SCORE_100
   and #$F0
   lsr
   tax
   ldy #0
:  lda digits1,x
   and #$0F
   ora SCORE_PF1,y
   sta SCORE_PF1,y
   inx
   iny
   cpy #5
   bne :-
   lda SCORE_100
   and #$0F
   asl
   asl
   asl
   tax
   ldy #0
:  lda digits02,x
   and #$F0
   sta SCORE_PF2,y
   inx
   iny
   cpy #5
   bne :-
   lda SCORE_1
   and #$F0
   lsr
   tax
   ldy #0
:  lda digits02,x
   and #$0F
   ora SCORE_PF2,y
   sta SCORE_PF2,y
   inx
   iny
   cpy #5
   bne :-
   lda SCORE_1
   and #$0F
   asl
   asl
   asl
   tax
   ldy #0
:  lda digits02,x
   and #$F0
   sta SCORE_PF0,y
   inx
   iny
   cpy #5
   bne :-
   ldx #0
:  ldy #2
:  lda SCORE_PF1,x
   sta PF1
   lda SCORE_PF2,x
   sta PF2
   nop
   lda SCORE_PF0,x
   sta PF0
   nop
   nop
   lda #0
   sta PF1
   nop
   nop
   sta PF2
   nop
   nop
   sta PF0
   sta WSYNC
   nop
   nop
   dey
   bne :-
   inx
   cpx #5
   bne :--
   sta WSYNC
.endmacro

.macro DIGITS_1
.byte $77
.byte $55
.byte $55
.byte $55
.byte $77
.byte 0,0,0
.byte $66
.byte $22
.byte $22
.byte $22
.byte $77
.byte 0,0,0
.byte $77
.byte $11
.byte $77
.byte $44
.byte $77
.byte 0,0,0
.byte $77
.byte $11
.byte $77
.byte $11
.byte $77
.byte 0,0,0
.byte $55
.byte $55
.byte $77
.byte $11
.byte $11
.byte 0,0,0
.byte $77
.byte $66
.byte $77
.byte $11
.byte $77
.byte 0,0,0
.byte $77
.byte $44
.byte $77
.byte $55
.byte $77
.byte 0,0,0
.byte $77
.byte $11
.byte $11
.byte $11
.byte $11
.byte 0,0,0
.byte $77
.byte $55
.byte $77
.byte $55
.byte $77
.byte 0,0,0
.byte $77
.byte $55
.byte $77
.byte $11
.byte $77
.endmacro

.macro DIGITS_02
.byte $EE
.byte $AA
.byte $AA
.byte $AA
.byte $EE
.byte 0,0,0
.byte $66
.byte $44
.byte $44
.byte $44
.byte $EE
.byte 0,0,0
.byte $EE
.byte $88
.byte $EE
.byte $22
.byte $EE
.byte 0,0,0
.byte $EE
.byte $88
.byte $EE
.byte $88
.byte $EE
.byte 0,0,0
.byte $AA
.byte $AA
.byte $EE
.byte $88
.byte $88
.byte 0,0,0
.byte $EE
.byte $22
.byte $EE
.byte $88
.byte $EE
.byte 0,0,0
.byte $EE
.byte $22
.byte $EE
.byte $AA
.byte $EE
.byte 0,0,0
.byte $EE
.byte $88
.byte $88
.byte $88
.byte $88
.byte 0,0,0
.byte $EE
.byte $AA
.byte $EE
.byte $AA
.byte $EE
.byte 0,0,0
.byte $EE
.byte $AA
.byte $EE
.byte $88
.byte $EE
.endmacro


.endif
