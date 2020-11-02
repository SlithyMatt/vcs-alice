.ifndef SPRITES_INC
SPRITES_INC = 1

.macro SIDE_SPRITES
; Standing Frame
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte $1C, $0C
.byte $1C, $0E
.byte $1C, $1C
.byte $1C, $14
.byte $0E, $0C
.byte $0E, $0C
.byte $0E, $0C
.byte $0E, $0C
.byte $96, $1E
.byte $96, $1E
.byte $96, $1E
.byte $96, $3F
.byte $96, $3F
.byte $96, $3F
.byte $0E, $08
.byte $0E, $08
.byte $12, $0C
; Running Frame
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte $1C, $0C
.byte $1C, $0E
.byte $1C, $1C
.byte $1C, $14
.byte $0E, $0C
.byte $0E, $1E
.byte $0E, $2D
.byte $0E, $0C
.byte $96, $1E
.byte $96, $1E
.byte $96, $1E
.byte $96, $3F
.byte $96, $3F
.byte $96, $3F
.byte $0E, $0C
.byte $0E, $12
.byte $12, $13
; Jumping Frame
.byte $1C, $0C
.byte $1C, $0E
.byte $1C, $1C
.byte $1C, $14
.byte $0E, $2C
.byte $0E, $1C
.byte $0E, $0C
.byte $0E, $0C
.byte $96, $1E
.byte $96, $1E
.byte $96, $1E
.byte $96, $3F
.byte $96, $3F
.byte $96, $3F
.byte $0E, $18
.byte $12, $10
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.byte 0,0
.endmacro

.endif
