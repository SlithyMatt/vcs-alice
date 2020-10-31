.ifndef VSYNC
.include "atari2600.inc"
.endif
.include "bank.inc"
.include "ram.inc"

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

start_bank1:
   nop
   jmp start_bank1;



.org $3FFA
.segment "VECTORS1"
.word Reset1          ; NMI
.word Reset1          ; RESET
.word Reset1          ; IRQ
