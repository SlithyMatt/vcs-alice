.include "atari2600.inc"

; RAM Variables
PTR1 = $80
PTR2 = $82

; Constants


; Macros
.macro PFLINE n, l0, l1, l2, r0, r1, r2
   ldx #n
:  lda #l0
   sta PF0
   lda #l1
   sta PF1
   lda #l2
   sta PF2
   ldy #3
:  dey
   bne :-
   lda #r0
   sta PF0
   lda #r1
   sta PF1
   lda #r2
   sta PF2
   sta WSYNC
   nop
   nop
   dex
   bne :--
.endmacro

.org $1000
.segment "STARTUP"

Reset:
   ldx #0
   lda #0
Clear:
   sta 0,x
   inx
   bne Clear

   ; Initialize graphics

StartOfFrame:

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

; 192 scanlines of picture...

   lda #$0E ; white
   sta COLUBK
   lda #0 ; black
   sta COLUPF
   sta WSYNC
   sta WSYNC
   ; "slithy GAMES"
   PFLINE 2,$00,$89,$14,$10,$91,$26
   PFLINE 2,$00,$89,$14,$10,$9B,$76
   PFLINE 2,$00,$A9,$94,$B0,$DB,$52
   PFLINE 2,$00,$A9,$9C,$A0,$55,$12
   PFLINE 2,$00,$8D,$89,$80,$55,$36
   PFLINE 2,$40,$8D,$8B,$80,$D5,$26
   PFLINE 2,$60,$A9,$8A,$B0,$D1,$66
   PFLINE 2,$20,$A9,$8A,$A0,$51,$42
   PFLINE 2,$60,$A9,$8A,$A0,$51,$52
   PFLINE 2,$40,$A9,$8A,$B0,$51,$52
   PFLINE 2,$60,$A9,$0A,$90,$51,$76
   PFLINE 2,$20,$A9,$0A,$90,$51,$26
   PFLINE 2,$00,$00,$08,$00,$00,$00
   PFLINE 4,$00,$00,$0C,$00,$00,$00
   sta WSYNC
   sta WSYNC
   lda #0 ; black
   sta COLUBK
   lda #$0E ; white
   sta COLUPF
   sta WSYNC
   sta WSYNC
   sta WSYNC
   sta WSYNC
   ; "PRESENTS"
   PFLINE 2,$00,$33,$4C,$B0,$2E,$02
   PFLINE 2,$00,$2A,$25,$90,$A4,$01
   PFLINE 2,$00,$3B,$6C,$B0,$64,$03
   PFLINE 2,$00,$22,$45,$90,$24,$02
   PFLINE 2,$00,$22,$2D,$B0,$24,$01
   lda #0
   sta PF0
   sta PF1
   sta PF2
   ldx #6
:  sta WSYNC
   dex
   bne :-
   lda #$96 ; blue
   sta COLUPF
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
   PFLINE 2,$00,$00,$20,$B0,$00,$00
   PFLINE 2,$00,$00,$20,$F0,$00,$00
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
   sta WSYNC
   sta WSYNC

   lda #%01000010
   sta VBLANK                     ; end of screen - enter blanking

   ; 30 scanlines of overscan...
   ldx #30
@oscan_loop:
   sta WSYNC
   dex
   bne @oscan_loop

   jmp StartOfFrame

; Pattern Data


.org $1FFA
.segment "VECTORS"

.word Reset          ; NMI
.word Reset          ; RESET
.word Reset          ; IRQ
