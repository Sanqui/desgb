SECTION "wram", WRAMX, BANK[$1], ALIGN[8]
wM: ds 8 ;  must be aligned
ds 8
wK: ds 8
ds 8

wKPlus: ds 6
wZero:  ds 1
ds 1 + 8
wC0: ds 4
wD0: ds 4
wCDn: ds 4*16*2

SECTION "wKn", WRAMX, BANK[$1], ALIGN[8]
wKn: ds 8*16

wMIP: ds 8

ds 8


wRtmp: ds 6
ds 2
wER: ds 8
wSB: ds 4
wF: ds 4

SECTION "wRL", WRAMX, BANK[$1], ALIGN[8]
wL0: ds 4
wR0: ds 4
wLRn: ds 2*4*16

wR16L16: ds 8
wIPNeg1: ds 8

; aux variables

wCurBitMask: ds 1
wKPlusPtrLow: ds 1
wCDnPtrLow: ds 1
wKnPtrLow:
    ds 1
wKnCounter: ds 1
wMIPPtrLow: ds 1
wCurPtrLow: ds 1
wCurBlock: ds 1

SECTION "Bit Lookup Table", ROMX, ALIGN[8]
BitLookupTable: ; must be aligned
    
    db %10000000
    db %01000000
    db %00100000
    db %00010000
    db %00001000
    db %00000100
    db %00000010
    db %00000001

SECTION "PC1 Table", ROMX, ALIGN[8]

PC1Table:
; 65s are manual for byte alignment (hard zeroes)
    db 57, 49, 41, 33, 25, 17, 09,  65
    db 01, 58, 50, 42, 34, 26, 18,  65
    db 10, 02, 59, 51, 43, 35, 27,  65
    db 19, 11, 03, 60, 52, 44, 36,  65
    db 63, 55, 47, 39, 31, 23, 15,  65
    db 07, 62, 54, 46, 38, 30, 22,  65
    db 14, 06, 61, 53, 45, 37, 29,  65
    db 21, 13, 05, 28, 20, 12, 04,  65
PC1TableEnd

PC2Table:
; 0s are for byte alignment (hard zeroes)
    db 14, 17, 11, 24,  1,  5,   0, 0
    db 3,  28, 15,  6, 21, 10,   0, 0
    db 23, 19, 12,  4, 26,  8,   0, 0
    db 16,  7, 27, 20, 13,  2,   0, 0
    db 41, 52, 31, 37, 47, 55,   0, 0
    db 30, 40, 51, 45, 33, 48,   0, 0
    db 44, 49, 39, 56, 34, 53,   0, 0
    db 46, 42, 50, 36, 29, 32,   0, 0
PC2TableEnd

SECTION "IP Table", ROMX, ALIGN[8]
IPTable:
    db 58, 50, 42, 34, 26, 18, 10,  2
    db 60, 52, 44, 36, 28, 20, 12,  4
    db 62, 54, 46, 38, 30, 22, 14,  6
    db 64, 56, 48, 40, 32, 24, 16,  8
    db 57, 49, 41, 33, 25, 17,  9,  1
    db 59, 51, 43, 35, 27, 19, 11,  3
    db 61, 53, 45, 37, 29, 21, 13,  5
    db 63, 55, 47, 39, 31, 23, 15,  7
IPTableEnd

ETable:
    db 32, 1,   2,  3,  4,  5,  0,0
    db 4,  5,   6,  7,  8,  9,  0,0
    db 8,  9,  10, 11, 12, 13,  0,0
    db 12, 13, 14, 15, 16, 17,  0,0
    db 16, 17, 18, 19, 20, 21,  0,0
    db 20, 21, 22, 23, 24, 25,  0,0
    db 24, 25, 26, 27, 28, 29,  0,0
    db 28, 29, 30, 31, 32,  1,  0,0
ETableEnd

SECTION "S Boxes", ROMX, ALIGN[8]
SBox1Table:
    db 14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7
    db  0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8
    db  4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0
    db 15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13

SBox2Table:
    db 15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10
    db  3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5
    db  0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15
    db 13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9
    
SBox3Table:
    db 10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2, 8
    db 13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1
    db 13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7
    db  1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12

SBox4Table:
    db  7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15
    db 13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9
    db 10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4
    db  3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14

SBox5Table:
    db  2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9
    db 14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6
    db  4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14
    db 11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3

SBox6Table:
    db 12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11
    db 10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8
    db  9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6
    db  4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13

SBox7Table:
    db  4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1
    db 13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6
    db  1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2
    db  6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12

SBox8Table:
    db 13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7
    db  1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2
    db  7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8
    db  2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11


SECTION "P Table", ROMX, ALIGN[8]
PTable:
    db 16,  7, 20, 21
    db 29, 12, 28, 17
    db  1, 15, 23, 26
    db  5, 18, 31, 10
    db  2,  8, 24, 14
    db 32, 27,  3,  9
    db 19, 13, 30,  6
    db 22, 11,  4, 25
PTableEnd

IPNeg1Table:
    db 40, 8, 48, 16, 56, 24, 64, 32
    db 39, 7, 47, 15, 55, 23, 63, 31
    db 38, 6, 46, 14, 54, 22, 62, 30
    db 37, 5, 45, 13, 53, 21, 61, 29
    db 36, 4, 44, 12, 52, 20, 60, 28
    db 35, 3, 43, 11, 51, 19, 59, 27
    db 34, 2, 42, 10, 50, 18, 58, 26
    db 33, 1, 41,  9, 49, 17, 57, 25
IPNeg1TableEnd

SECTION "DES Code", ROMX

TestMessage:
    db $01, $23, $45, $67, $89, $ab, $cd, $ef

DefaultKey:
    db "GAME BOY"
;    db $13, $34, $57, $79, $9B, $BC, $DF, $F1
    
; 133457799BBCDFF1

GetBit:
; reads ath bit in [hl]
; hl needs not cross a xx00 boundary
    cp -1
    ret z
    ld b, a
    sra a
    sra a
    sra a
    add l
    ld l, a
    ld a, b
    
    and %00000111
    ld b, HIGH(BitLookupTable)
    ld c, a
    ld a, [bc]
    
    and [hl]
    ret

GetBit7:
; reads ath bit in [hl], skipping over every 7th bit
; hl needs not cross a xx00 boundary
    cp -1
    ret z
    ld c, 0
.divloop
    sub 7
    inc c
    jr nc, .divloop
    dec c
    add 7
    ld b, a
    ld a, c
    
    add l
    ld l, a
    ld a, b
    
    ld b, HIGH(BitLookupTable)
    ld c, a
    ld a, [bc]
    
    and [hl]
    ret

WriteBit:
; takes z flag
    jr z, .skipbit
    ld a, [wCurBitMask]
    or [hl]
    ld [hl], a
    jr .written
    
.skipbit
    ld a, [wCurBitMask]
    xor $ff
    and [hl]
    ld [hl], a
.written
    ld a, [wCurBitMask]
    rr a
    ld [wCurBitMask], a
    ret nc
.nextbyte
    ld a, %10000000
    ld [wCurBitMask], a
    inc hl
    ret

RotateBlock:
    push hl
    ld bc, 4+4+3
    add hl, bc
    ld e, l
    ld d, h
    pop hl
    ld a, [hl]
    rlc a
    inc hl
    inc hl
    inc hl
rept 4
    ld a, [hld]
    adc 0
    sla a
    ld [de], a
    dec de
endr
    inc hl
    ret

RotateBlockInPlace:
    ld a, [hl]
    rlc a
    inc hl
    inc hl
    inc hl
rept 4
    ld a, [hl]
    adc 0
    sla a
    ld [hld], a
endr
    inc hl
    ret

rotateiteration: MACRO
    call RotateBlock
    ld bc, 8
    add hl, bc
rept \1-1
    call RotateBlockInPlace
endr
ENDM

Do16Rotations:
    rotateiteration 1
    rotateiteration 1
    rotateiteration 2
    rotateiteration 2
    rotateiteration 2
    rotateiteration 2
    rotateiteration 2
    rotateiteration 2
    rotateiteration 1
    rotateiteration 2
    rotateiteration 2
    rotateiteration 2
    rotateiteration 2
    rotateiteration 2
    rotateiteration 2
    rotateiteration 1
    ret

genroutine: MACRO
    ld a, %10000000
    ld [wCurBitMask], a
    
    ld de, \1
    ld a, LOW(\3)
    ld [wCurPtrLow], a
.loop
    ld a, [de]
    dec a ; fdsjklfdsjfklds
    ld hl, \2
    call GetBit
    ld h, HIGH(\3)
    lda l, [wCurPtrLow]
    call WriteBit
    lda [wCurPtrLow], l
    inc de
    ld a, e
    cp LOW(\1End)
    jr nz, .loop
    ret
ENDM

GenKPlus:
    genroutine PC1Table, wK, wKPlus

GenKn:
    lda [wCurBitMask], %10000000
    ld de, PC2Table
.loop
    ld a, [de]
    dec a ; fdsjklfdsjfklds 2.0
    ld b, a
    ld h, HIGH(wCDn)
    lda l, [wCDnPtrLow]
    ld a, b
    call GetBit7
    ld h, HIGH(wKn)
    lda l, [wKnPtrLow]
    call WriteBit
    lda [wKnPtrLow], l
    inc de
    ld a, e
    cp LOW(PC2TableEnd)
    jr nz, .loop
    ret

GenMIP:
    genroutine IPTable, wM, wMIP

GenERn:
    genroutine ETable, wRtmp, wER

GenF:
    genroutine PTable, wSB, wF

GenIPNeg1:
    genroutine IPNeg1Table, wR16L16, wIPNeg1

GenAllKn:
    
    lda [wCDnPtrLow], LOW(wCDn)
    lda [wKnPtrLow], LOW(wKn)
    ld a, 16
.knloop
    ld [wKnCounter], a
    call GenKn
    
    ld a, [wCDnPtrLow]
    add 8
    ld [wCDnPtrLow], a
    ld a, [wKnCounter]
    dec a
    jr nz, .knloop
    ret

XorData:
.loop
    ld a, [de]
    xor [hl]
    ld [de], a
    inc hl
    inc de
    dec b
    jr nz, .loop
    ret



GetSBoxValue:
; destroys b, hl
    ld b, a
    and %01111000
    srl a
    srl a
    srl a
    add l
    ld l, a
    ld a, b
    and %00000100
    sla a
    sla a
    add l
    ld l, a
    ld a, b
    and %10000000
    srl a
    srl a
    add l
    ld l, a
    ld a, [hl]
    ret

sboxpair: MACRO
    ld hl, SBox1Table + (SBox2Table-SBox1Table)*(\1*2)
    ld a, [wER+(\1*2)]
    call GetSBoxValue
    swap a
    ld c, a
    ld hl, SBox1Table + (SBox2Table-SBox1Table)*(\1*2+1)
    ld a, [wER+(\1*2)+1]
    call GetSBoxValue
    or c
    ld [wSB+\1], a
ENDM

DoSBoxes:
    sboxpair 0
    sboxpair 1
    sboxpair 2
    sboxpair 3
    ret

CalcLR:
    ld hl, wR0
    ld a, [wCurBlock]
    sla a
    sla a
    sla a
    add l
    ld l, a
    push hl
    ld de, wRtmp
    ld bc, 4
    call CopyData
    pop hl
    ld a, l
    add 4
    ld e, a
    ld d, h
    ld bc, 4
    call CopyData
    
    call GenERn
    ld de, wER
    ld hl, wKn
    ld a, [wCurBlock]
    sla a
    sla a
    sla a
    add l
    ld l, a
    ld b, 8
    call XorData
    
    call DoSBoxes
    call GenF
    
    ld a, [wCurBlock]
    inc a
    ld hl, wR0
    sla a
    sla a
    sla a
    add l
    ld l, a
    push hl
    ld e, l
    ld d, h
    ld hl, wF
    ld bc, 4
    call CopyData
    pop de
    
    ld a, [wCurBlock]
    ld hl, wL0
    sla a
    sla a
    sla a
    add l
    ld l, a
    ld b, 4
    
    call XorData
    ret

DoDES:
    ;ld bc, 8
    ;ld hl, TestMessage
    ;ld de, wM
    ;call CopyData
    
    ld bc, 8
    ld hl, DefaultKey
    ld de, wK
    call CopyData
    
    ; ===
    ; Step 1: Create 16 subkeys, each of which is 48-bits long.
    ; ===
    
    call GenKPlus
    
    ; wKPlus == f0 66 2a 5e 54 b2 9e 1e
    
    ld hl, wKPlus
    ld de, wC0
    ld bc, 4
    call CopyData
    
    ld hl, wKPlus+4
    ld de, wD0
    ld bc, 4
    call CopyData
    
    ld hl, wC0
    call Do16Rotations
    
    ld hl, wD0
    call Do16Rotations

    ; C0: F0 66 2A 5E
    ; C1: E0 CC 54 BE
    ; C16: F0 66 2A 5E
    ; 
    ; D0: 54 B2 9E 1E
    ; D1: AA 66 3C 3C
    ; D16: 54 B2 9E 1E
    ; 
    
    call GenAllKn
    
    ; K1-K16 seem right!
    ; K16 might be C8 C D8 2C 0C 84 7C d4
    
    ; ===
    ; Step 2: Encode each 64-bit block of data.
    
    
    call GenMIP
    
    ; MIP is CC 00 CC FF F0 AA F0 AA
    
    ld hl, wMIP
    ld de, wL0
    ld bc, 8
    call CopyData
    
    ld a, 0
    ld [wCurBlock], a
    
.blockloop
    call CalcLR
    
    ld a, [wCurBlock]
    inc a
    ld [wCurBlock], a
    cp 16
    jr nz, .blockloop
    
    ld hl, wLRn + 8 * 15
    ld de, wR16L16+4
    ld bc, 4
    call CopyData
    
    ld hl, wLRn + 8 * 15 + 4
    ld de, wR16L16
    ld bc, 4
    call CopyData
    
    call GenIPNeg1
    
    ret

