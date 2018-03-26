SECTION "wram", WRAMX[$d000], BANK[$1]
wMessageBlock: ds 8 ;  must be aligned
ds 8
wK: ds 8
ds 8

wKPlus: ds 6
wZero:  ds 1
ds 1 + 8
wC0: ds 4
WCn: ds 4*16
ds 4+8
wD0: ds 4
wDn: ds 4*16

wCurBitMask: ds 1
wKPlusPtrLow: ds 1

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
    ld a, [hl]
    rlc a
    inc hl
    inc hl
    inc hl
    ld e, l
    ld d, h
    inc de
    inc de
    inc de
    inc de
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
    ld bc, 4
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
    
    ; wKPlus == f0 66 2a 5e 54 b2 9e 1e
    ld b, b
    
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
    ; C2: C2 98 AA 7E
    ; C3: 0C 64 AA FE
    ; C4: 32 94 AE F8
    ; C5: CC 54 BE E0
    ; C6: 32 54 FE 86
    ; C7: CA 56 FC 18
    ; C8: 2A 5E F0 66
    ; C9: 54 BE E0 CC
    ; C10: 54 FE 86 32
    ; C11: 56 FC 18 CA
    ; C12: 5E F0 66 2A
    ; C13: 7E C2 98 AA
    ; C14: FE 0C 64 AA
    ; C15: F8 32 94 AE
    ; C16: F0 66 2A 5E
    ; 
    ; D0: 54 B2 9E 1E
    ; D1: AA 66 3C 3C
    ; D2: 54 CC 78 7A
    ; D3: 56 32 E2 EA
    ; D4: 58 CE 8E AA
    ; D5: 66 3C 3C AA
    ; D6: 98 F0 F4 AA
    ; D7: 66 C6 D4 AC
    ; D8: 9E 1E 54 B2
    ; D9: 3C 3C AA 66
    ; D10: F0 F4 AA 98
    ; D11: C6 D4 AC 66
    ; D12: 1E 54 B2 9E
    ; D13: 7A 54 CC 78
    ; D14: EA 56 32 E2
    ; D15: AA 58 CE 8E
    ; D16: 54 B2 9E 1E
    ; 
    
    
    
    ret
