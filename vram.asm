
VBlankHandler:
    push af
    push bc
    push de
    push hl
    call CopyTilemap
    call ReadJoypadRegister
    ld hl, H_TIMER
    inc [hl]
    ld a, [rDIV]
    ld b, a
    
    ; derp
    ld a, $00
    ld [$FF1A], a
    
    pop hl
    pop de
    pop bc
    pop af
    reti

CopyTilemap: ; We can copy just 8 lines per vblank.
; Contains an unrolled loop for speed.
    ;ld de, $9800
    ;ld hl, W_MAP
    ld hl, H_VCOPY_D
    ld a, [hli]
    ld d, a
    ld a, [hli]
    ld e, a
    ld a, [hli]
    ld c, a
    ld a, [hl]
    ld l, a
    ld h, c
    ld a, [H_VCOPY_ROWS]
    ld c, a
.row

    dec c
    jr z, .done

    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    ld a, [hli]
    ld [de], a
    inc de
    
    ld a, e
    add $c
    ld e, a
    jr nc, .row
;carry
    inc d
    jr .row
.done
    ld a, [H_VCOPY_TIMES]
    inc a
    ld [H_VCOPY_TIMES], a
    cp a, $03
    jr z, .reset
    cp a, $02
    jr nz, .eightrows
    ; only 5 rows left
    ld a, $5
    ld [H_VCOPY_ROWS], a

.eightrows
    ld a, d
    ld [H_VCOPY_D], a
    ld a, e
    ld [H_VCOPY_E], a
    ld a, h
    ld [H_VCOPY_H], a
    ld a, l
    ld [H_VCOPY_L], a
    ret
.reset
    ld a, $98
    ld [H_VCOPY_D], a
    xor a
    ld [H_VCOPY_E], a
    ld a, $C0
    ld [H_VCOPY_H], a
    xor a
    ld [H_VCOPY_L], a
    ld [H_VCOPY_TIMES], a
    ld a, $8
    ld [H_VCOPY_ROWS], a
    ret

DisableLCD: ; $0061
	xor a
	ld [$ff0f],a
	ld a,[$ffff]
	ld b,a
	res 0,a
	ld [$ffff],a
.waitVBlank
	ld a,[$ff44]
	cp a,$91
	jr nz,.waitVBlank
	ld a,[$ff40]
	and a,$7f	; res 7,a
	ld [$ff40],a
	ld a,b
	ld [$ffff],a
	ret

EnableLCD:
	ld a,[$ff40]
	set 7,a
	ld [$ff40],a
	ret
