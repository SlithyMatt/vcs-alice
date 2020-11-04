.ifndef PLAYFIELD_INC
PLAYFIELD_INC  = 1

; Macros
.macro PFLINE n, l0, l1, l2, r0, r1, r2
.scope
.if n > 1
   ldx #n
.endif
gfx_loop:
   lda #l0
   sta PF0
   lda #l1
   sta PF1
   lda #l2
   sta PF2
.ifnblank r0
   ldy #3
delay:
   dey
   bne delay
   lda #r0
   sta PF0
   lda #r1
   sta PF1
   lda #r2
   sta PF2
   sta WSYNC
.if n > 1
   nop
   nop
   dex
   bne gfx_loop
.endif
.else
eol:
   sta WSYNC
.if n > 1
   dex
   bne eol
.endif
.endif
.endscope
.endmacro


.endif
