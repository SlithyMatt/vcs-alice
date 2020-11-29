.ifndef MUSIC_INC
MUSIC_INC = 1

.macro SET_MUSIC_TEMPO bpm
SIXTEENTH = 900/bpm
TRIPLET8  = 1200/bpm
D_SIXTNTH = 1350/bpm
EIGHTH    = 1800/bpm
TRIPLET4  = 2400/bpm
D_EIGHTH  = 2700/bpm
QUARTER   = 3600/bpm
D_QUARTER = 5400/bpm
HALF      = 7200/bpm
D_HALF    = 10800/bpm
WHOLE     = 14400/bpm
.endmacro

.macro SET_MUSIC_VOLUME volume
MUSIC_VOLUME = volume
.endmacro

.macro END_MUSIC loopstart
.byte $80
.addr loopstart
.endmacro

.macro INIT_MUSIC start_addr
   lda #<start_addr
   sta MUSIC_PTR
   lda #>start_addr
   sta MUSIC_PTR+1
   lda #1
   sta MUSIC_CTR
.endmacro

; takes up 2 scanlines
.macro PLAY_MUSIC
   ldy #0
   dec MUSIC_CTR
   lda MUSIC_CTR
   beq :+
   sta WSYNC
   jmp :+++
:  lda (MUSIC_PTR),y
   bmi :+
   sta AUDF0
   iny
   lda (MUSIC_PTR),y
   sta AUDC0
   iny
   lda (MUSIC_PTR),y
   sta AUDV0
   iny
   sta WSYNC
   lda (MUSIC_PTR),y
   sta MUSIC_CTR
   lda MUSIC_PTR
   clc
   adc #4
   sta MUSIC_PTR
   lda MUSIC_PTR+1
   adc #0
   sta MUSIC_PTR+1
   jmp :++
:  sta WSYNC
   iny
   lda (MUSIC_PTR),y
   tax
   iny
   lda (MUSIC_PTR),y
   sta MUSIC_PTR+1
   txa
   sta MUSIC_PTR
   lda #1
   sta MUSIC_CTR
:  sta WSYNC
.endmacro

.macro REST duration
.byte 0,0,0,duration
.endmacro

.macro D_SHARP3 duration
.byte 31,12,MUSIC_VOLUME,duration
.endmacro

.macro E3 duration
.byte 5,6,MUSIC_VOLUME,duration
.endmacro

.macro F3 duration
.byte 28,12,MUSIC_VOLUME,duration
.endmacro

.macro F3_SHARP duration
.byte 26,12,MUSIC_VOLUME,duration
.endmacro

.macro G3 duration
.byte 4,6,MUSIC_VOLUME,duration
.endmacro

.macro G_SHARP3 duration
.byte 23,12,MUSIC_VOLUME,duration
.endmacro

.macro A3 duration
.byte 22,12,MUSIC_VOLUME,duration
.endmacro

.macro A_SHARP3 duration
.byte 20,12,MUSIC_VOLUME,duration
.endmacro

.macro B3 duration
.byte 19,12,MUSIC_VOLUME,duration
.endmacro

.macro C4 duration
.byte 18,12,MUSIC_VOLUME,duration
.endmacro

.macro C_SHARP4 duration
.byte 17,12,MUSIC_VOLUME,duration
.endmacro

.macro D4 duration
.byte 16,12,MUSIC_VOLUME,duration
.endmacro

.macro D_SHARP4 duration
.byte 15,12,MUSIC_VOLUME,duration
.endmacro

.macro E4 duration
.byte 14,12,MUSIC_VOLUME,duration
.endmacro

.macro F4 duration
.byte 13,12,MUSIC_VOLUME,duration
.endmacro

.macro G4 duration
.byte 12,12,MUSIC_VOLUME,duration
.endmacro

.macro G_SHARP4 duration
.byte 11,12,MUSIC_VOLUME,duration
.endmacro

.macro A_SHARP4 duration
.byte 31,4,MUSIC_VOLUME,duration
.endmacro

.macro B4 duration
.byte 9,12,MUSIC_VOLUME,duration
.endmacro

.macro C5 duration
.byte 28,4,MUSIC_VOLUME,duration
.endmacro

.macro C_SHARP5 duration
.byte 8,12,MUSIC_VOLUME,duration
.endmacro

.macro D5 duration
.byte 25,4,MUSIC_VOLUME,duration
.endmacro

.macro D_SHARP5 duration
.byte 7,12,MUSIC_VOLUME,duration
.endmacro

.macro E5 duration
.byte 22,4,MUSIC_VOLUME,duration
.endmacro

.macro F5 duration
.byte 6,12,MUSIC_VOLUME,duration
.endmacro

.macro F_SHARP5 duration
.byte 19,4,MUSIC_VOLUME,duration
.endmacro

.macro G5 duration
.byte 18,4,MUSIC_VOLUME,duration
.endmacro

.macro G_SHARP5 duration
.byte 5,12,MUSIC_VOLUME,duration
.endmacro

.macro A5 duration
.byte 16,4,MUSIC_VOLUME,duration
.endmacro

.macro A_SHARP5 duration
.byte 15,4,MUSIC_VOLUME,duration
.endmacro

.macro B5 duration
.byte 4,12,MUSIC_VOLUME,duration
.endmacro

.macro C6 duration
.byte 13,4,MUSIC_VOLUME,duration
.endmacro

.macro C_SHARP6 duration
.byte 26,0,MUSIC_VOLUME,duration
.endmacro

.macro D6 duration
.byte 12,4,MUSIC_VOLUME,duration
.endmacro

.macro D_SHARP6 duration
.byte 3,12,MUSIC_VOLUME,duration
.endmacro

.macro E6 duration
.byte 22,0,MUSIC_VOLUME,duration
.endmacro

.macro F6 duration
.byte 10,4,MUSIC_VOLUME,duration
.endmacro

.macro F_SHARP6 duration
.byte 9,4,MUSIC_VOLUME,duration
.endmacro

.macro G6 duration
.byte 18,0,MUSIC_VOLUME,duration
.endmacro

.macro G_SHARP6 duration
.byte 2,12,MUSIC_VOLUME,duration
.endmacro

.macro A6 duration
.byte 16,0,MUSIC_VOLUME,duration
.endmacro

.macro A_SHARP6 duration
.byte 7,4,MUSIC_VOLUME,duration
.endmacro

.macro B6 duration
.byte 14,0,MUSIC_VOLUME,duration
.endmacro

.macro C7 duration
.byte 6,4,MUSIC_VOLUME,duration
.endmacro


.endif
