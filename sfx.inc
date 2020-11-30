.ifndef SFX_INC
SFX_INC = 1

.macro STOP_SFX
.byte $80
.endmacro

.macro END_SFX
.byte 0,0,0,1 ; 1 jiffy of silence
STOP_SFX
.endmacro

.macro START_SFX start_addr
   lda #<start_addr
   sta SFX_PTR
   lda #>start_addr
   sta SFX_PTR+1
   lda #1
   sta SFX_CTR
.endmacro

; takes up 2 scanlines
.macro PLAY_SFX
   ldy #0
   lda (SFX_PTR),y
   bpl :+
   sta WSYNC
   jmp :+++
:  dec SFX_CTR
   beq :+
   sta WSYNC
   jmp :++
:  sta AUDF1
   iny
   lda (SFX_PTR),y
   sta AUDC1
   iny
   lda (SFX_PTR),y
   sta AUDV1
   iny
   sta WSYNC
   lda (SFX_PTR),y
   sta SFX_CTR
   lda SFX_PTR
   clc
   adc #4
   sta SFX_PTR
   lda SFX_PTR+1
   adc #0
   sta SFX_PTR+1
:  sta WSYNC
.endmacro


.endif
