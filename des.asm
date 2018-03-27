SECTION "wram", WRAMX, BANK[$1], ALIGN[8]
wMessageBlock: ds 8 ;  must be aligned
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

; aux variables

wCurBitMask: ds 1
wKPlusPtrLow: ds 1
wCDnPtrLow: ds 1
wKnPtrLow:
    ds 1
wKnCounter: ds 1

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

SECTION "PC2 Table", ROMX, ALIGN[8]
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

SECTION "DES Code", ROMX

TestMessage:
    db $01, $23, $45, $67, $89, $ab, $cd, $ef

DefaultKey:
    db $13, $34, $57, $79, $9B, $BC, $DF, $F1

GetBit:
; reads ath bit in [hl]
; hl needs not cross a xx00 boundary
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
    
.skipbit
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

GenKPlus:
    ld a, %10000000
    ld [wCurBitMask], a
    
    ld de, PC1Table
    ld a, LOW(wKPlus)
    ld [wKPlusPtrLow], a
.loop
    ld a, [de]
    dec a ; fdsjklfdsjfklds
    ld hl, wK
    call GetBit
    ld h, HIGH(wKPlus)
    lda l, [wKPlusPtrLow]
    call WriteBit
    lda [wKPlusPtrLow], l
    inc de
    ld a, e
    cp LOW(PC1TableEnd)
    jr nz, .loop
    ret

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

DoDES:
    ld bc, 8
    ld hl, TestMessage
    ld de, wMessageBlock
    call CopyData
    
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
    ld b, b
    
    call GenAllKn
    
    ; K1-K16 seem right!
    
    ; ===
    ; Step 2: Encode each 64-bit block of data.
    ; ===
    
    ret
