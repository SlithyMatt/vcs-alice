.ifndef RAM_INC
RAM_INC = 1

; RAM Variables
PTR1        = $80 ; scratch pointer address
PTR2        = $82 ; scratch pointer address
FRAME_CTR   = $84
OFFSET      = $85
JUMP_CD     = $86
RELEASED    = $87
START       = $88
COUNTER     = $89
PF0_R       = $8A
PF1_R       = $8B
PF2_R       = $8C
SCORE_1     = $8D
SCORE_100   = $8E
SCORE_10K   = $8F
SCORE_PF0   = $90 ; 5 bytes
SCORE_PF1   = $95 ; 5 bytes
SCORE_PF2   = $9A ; 5 bytes

.endif
