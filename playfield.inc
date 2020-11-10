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
.if l0 <> l1
   lda #l1
.endif
   sta PF1
   lda #l2
   sta PF2
.ifnblank r0
   ldy #3
delay:
   dey
   bne delay
.if r0 <> l0
.if r0 <> l2
   lda #r0
.endif
   sta PF0
.endif
.if r1 <> l1
.if r0 <> r1 || r0 = l0
   lda #r1
.endif
   sta PF1
.endif
.if r2 <> l2
.if r2 <> r1 || r1 = l1
   lda #r2
.endif
   sta PF2
.endif
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
