INCLUDE "constants.asm"

; rst vectors go unused
SECTION "rst00",ROM0[0]
    ret

SECTION "rst08",ROM0[8]
    ret

SECTION "rst10",ROM0[$10]
    ret

SECTION "rst18",ROM0[$18]
    ret

SECTION "rst20",ROM0[$20]
    ret

SECTION "rst30",ROM0[$30]
    ret

SECTION "rst38",ROM0[$38]
    ret

SECTION "vblank",ROM0[$40]
	jp VBlankHandler
SECTION "lcdc",ROM0[$48]
	reti
SECTION "timer",ROM0[$50]
	reti
SECTION "serial",ROM0[$58]
	reti
SECTION "joypad",ROM0[$60]
	reti

SECTION "bank0",ROM0[$61]

SECTION "romheader",ROM0[$100]
    nop
    jp Start

SECTION "start",ROM0[$150]

INCLUDE "vram.asm"

CopyData:
; copy bc bytes of data from hl to de
	ld a,[hli]
	ld [de],a
	inc de
	dec bc
	ld a,c
	or b
	jr nz,CopyData
	ret

CopyDataFF:
; copy data from hl to de ending with $ff (inclusive)
	ld a,[hli]
	ld [de],a
	inc de
	inc a
	ret z
	jr CopyDataFF

WriteDataInc:
; write data in hl increasing a until b.
.loop
    ld [hli], a
    inc a
    cp a, b
    jr nz, .loop
    ret

WriteBTimes:
; write a in hl b times
.loop
    ld [hli], a
    dec b
    jr nz, .loop
    ret

; copypasta:
; this function directly reads the joypad I/O register
; it reads many times in order to give the joypad a chance to stabilize
; it saves a result in [$fff8] in the following format
; (set bit indicates pressed button)
; bit 0 - A button
; bit 1 - B button
; bit 2 - Select button
; bit 3 - Start button
; bit 4 - Right
; bit 5 - Left
; bit 6 - Up
; bit 7 - Down
ReadJoypadRegister: ; 15F
    ld a, [H_JOY]
    ld [H_JOYOLD], a
	ld a,%00100000 ; select direction keys
	ld c,$00
	ld [rJOYP],a
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	cpl ; complement the result so that a set bit indicates a pressed key
	and a,%00001111
	swap a ; put direction keys in upper nibble
	ld b,a
	ld a,%00010000 ; select button keys
	ld [rJOYP],a
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	ld a,[rJOYP]
	cpl ; complement the result so that a set bit indicates a pressed key
	and a,%00001111
	or b ; put button keys in lower nibble
	ld [$fff8],a ; save joypad state
	ld a,%00110000 ; unselect all keys
	ld [rJOYP],a
	
	ld a, [H_JOY]
	ld b, a
	ld a, [H_JOYOLD]
	xor $ff
	and b
	ld [H_JOYNEW], a
	ret

GetTileAddr: ; bc = xy
    push bc
    push de
    inc c
    ld hl, W_MAP
    ld e, $14
.loop
    dec c
    jr z, .end
    ld a, l
    add e
    ld l, a
    jr nc, .loop
    inc h
    jr .loop
.end
    ld a, l
    add b
    ld l, a
    jr nc, .nc
    inc h
.nc
    pop de
    pop bc
    ret

DrawBox: ; draws a box from bc to de
    call GetTileAddr ; top left corner
    ld a, $10
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $11
    call WriteBTimes
    ld a, $12
    ld [hli], a
    pop bc
    ; top drawn
.mid
    inc c
    ld a, c
    cp e
    jr z, .last
    call GetTileAddr
    ld a, $13
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $14
    call WriteBTimes
    ld a, $15
    ld [hli], a
    pop bc
    jr .mid
    
.last
    call GetTileAddr ; top left corner
    ld a, $16
    ld [hli], a
    ld a, d
    sub b
    dec a
    push bc
    ld b, a
    ld a, $17
    call WriteBTimes
    ld a, $18
    ld [hli], a
    pop bc
    ret
    

WriteString:
.loop
    ld a, [hli]
    cp a, "@"
    jr z, .done
    cp a, " "
    jr nz, .nospace
    ld a, 0
.nospace
    ld [de], a
    inc de
    jr .loop
.done
    ret
   
ModuloB:
.loop
    cp a, b
    ret c
    sub a, b
    jr .loop

DivB:
    ld c, 0
.loop
    cp a, b
    jr c, .ret
    sub a, b
    inc c
    jr .loop
.ret
    ld a, c
    ret
    
WriteNumber: ; writes number to de
    push af
    ld b, 10
    call DivB
    add a, $30
    ld [de], a
    inc de
    pop af
    call ModuloB
    add a, $30
    ld [de], a
    ret

WriteHexNumber: ; writes hex number to de
    push af
    swap a
    and a, $0f
    cp $a
    jr nc, .ten
    add a, $30
    jr .write1
.ten
    add a, $37
.write1
    ld [de], a
    inc de
    pop af
    and a, $0f
    cp $a
    jr nc, .ten2
    add a, $30
    jr .write2
.ten2
    add a, $37
.write2
    ld [de], a
    inc de
    ret

ClearScreen:
    ld hl, $c000
.loop
    xor a
    ld [hli], a
    ld a, h
    cp $c1
    jr nz, .loop
    ld a, l
    cp $68
    jr nz, .loop
    ret

AButtonSound: ; worst audio ever
    ;ld a, %10111111
    ;ld [$FF11], a
    ;ld a, $80
    ;ld [$FF13], a
    ;ld a, %10111111
    ;ld [$FF14], a
    ld a, $ff
    ld [$FF1A], a
    ld a, %10111111
    ld [$FF1E], a
    ret

Start:
    di
    ld sp, $dffe
    
    ; palettes
    ld a, %11100100
    ld [rBGP], a
    ld [rOBP0], a
    
    ld a, 0
    ld [rSCX], a
    ld [rSCY], a
    
    ld a, %11000001
    ld [rLCDC], a
    
    ei
    
    call DisableLCD
    
    ld hl, $C000
.loop
    ld a, 0
    ld [hli], a
    ld a, h
    cp $e0
    jr nz, .loop

    ld hl, $ff80
.loop2
    ld a, 0
    ld [hli], a
    ld a, h
    cp $00
    jr nz, .loop2
    
    ld a, $98
    ld [H_VCOPY_D], a
    ld a, $C0
    ld [H_VCOPY_H], a
    ld a, $8
    ld [H_VCOPY_ROWS], a
    
    ld hl, Tiles
    ld de, $9000
    ld bc, TilesEnd-Tiles
    call CopyData
    
    call EnableLCD
    ld a, %00000001
    ld [$ffff], a
    ei
    
    ; sound crap
    ld a, $ff
    ld [$FF26], a
    ld [$FF24], a
    
.begin
    ld hl, W_INPUT
    ld a, "_"
    ld [hli], a
    ld a, "@"
    ld [hl], a
    
    xor a
    ld [H_SELECTION], a
    ld [H_POSITION], a
    ld [H_TIMER], a
    

.waitloop
    halt
    
    ld a, [H_SELECTION]
    ld e, a
    
    ld a, [H_JOYNEW]
    ld d, a
    ; down up left right
    and a, %10000000
    jr nz, .down
    ld a, d
    and a, %01000000
    jr nz, .up
    ld a, d
    and a, %00100000
    jr nz, .left
    ld a, d
    and a, %00010000
    jr nz, .right
    ld a, d
    and a, %00000001
    jr nz, .a
    ld a, d
    and a, %00000010
    jr nz, .b
    ld a, d
    and a, %00001000
    jr nz, .start
    jr .nokey
.down
    ld a, e
    cp a, 20
    jr nc, .nokey
    add a, 6
    jr .moved
.up
    ld a, e
    cp a, 6
    jr c, .nokey
    sub a, 6
    jr .moved
.left
    ld a, e
    and a
    jr z, .nokey
    dec a
    jr .moved
.right
    ld a, e
    cp a, NUM_LETTERS+1
    jr z, .nokey
    inc a
    jr .moved
.moved
    ld [H_SELECTION], a
    jr .nokey
.a
    ; play some sound???
    call AButtonSound
    
    ld hl, W_INPUT
    ld a, [H_POSITION]
    cp NUM_CHARS
    jr z, .nokey
    add l
    ld l, a
    ld a, [H_SELECTION]
    cp NUM_LETTERS+1
    jr nz, .notspace
    ld a, " "
    jr .gotchar
.notspace
    add a, $41
.gotchar
    ld [hli], a
    ld a, [H_POSITION]
    cp NUM_CHARS-1
    jr nc, .nounderscore
    ld a, "_"
    ld [hli], a
.nounderscore
    ld a, "@"
    ld [hl], a
    
    ld a, [H_POSITION]
    inc a
    ld [H_POSITION], a
    jr .nokey
.b
    
    ld hl, W_INPUT
    ld a, [H_POSITION]
    and a
    jr z, .nokey
    add l
    ld l, a
    ld a, "@"
    ld [hld], a
    ld a, "_"
    ld [hl], a
    ld a, [H_POSITION]
    dec a
    ld [H_POSITION], a
    jr .nokey

.start
    call Enter
    jp .begin

.nokey

    ;call ClearScreen
    ld a, [H_TIMER]
    cp $14
    jr nz, .timerok
    xor a
    ld [H_TIMER], a
    ld a, [H_CURSOR]
    cp $19
    jr z, .frame2
    ld a, $19
    ld [H_CURSOR], a
    jr .timerok
.frame2
    ld a, $1a
    ld [H_CURSOR], a

.timerok
    ld bc, $0100
    ld de, $1204
    call DrawBox
    ld hl, EnterPasswordString
    decoord 1, 2
    call WriteString
    
    ld hl, PressStartString
    decoord $10, 4
    call WriteString
    ld hl, ToSubmitString
    decoord $11, 5
    call WriteString
    
    ld hl, W_INPUT
    decoord 3, 3
    call WriteString
    
    ld bc, $0005
    ld de, $130f
    call DrawBox
    
    ld b, 0
    ld c, 0
    ld de, Letters
    hlcoord 6, 1
.letter
    ld a, [H_SELECTION]
    cp b
    jr nz, .notselect
    ld a, [H_CURSOR]
    ld [hl], a
.notselect
    inc hl
    ld a, [de]
    cp "@"
    jr z, .end
    ld [hli], a
    inc hl
    inc de
    inc b
    inc c
    ld a, c
    cp 6
    jr nz, .letter
    ld a, l
    add $16
    ld l, a
    xor a
    ld c, a
    jr .letter
.end
    
    
    jp .waitloop

CompareStrings:
; compares hl with de
; sets carry if same
.loop
    ld a, [hli]
    ld b, a
    ld a, [de]
    inc de
    cp "@"
    jr z, .same
    cp b
    jr nz, .notsame
    jr .loop

.same
    scf
    ret
.notsame
    xor a
    ret

DoDESWithHL:
    ld b, 8
    ld de, wM
.copyloop
    ld a, [hli]
    cp "_"
    jr z, .fillloop
    ld [de], a
    inc de
    dec b
    jr nz, .copyloop
.fillloop
    ld a, b
    and a
    jr z, .donecopy
    xor a
    ld [de], a
    inc de
    dec b
    jr nz, .fillloop
.donecopy
    
    jp DoDES

WriteHexWord:
    ld b, 8
.writeloop
    ld a, [hli]
    call WriteHexNumber
    dec b
    jr nz, .writeloop
    ret

Enter:
    ld de, W_INPUT
    ld a, [de]
    cp "_"
    jp z, .empty
    ld a, [H_POSITION]
    cp 8+1
    jr nc, .drawlargerbox
    ld bc, $0107
    ld de, $120b
    call DrawBox
    jr .drewbox
.drawlargerbox
    ld bc, $0107
    ld de, $120c
    call DrawBox
    
.drewbox
    ld hl, W_INPUT
    
    call DoDESWithHL
    
    decoord 9, 2
    ld hl, wIPNeg1
    call WriteHexWord
    
    ld a, [H_POSITION]
    cp 9
    jr c, .done
    
    ld hl, W_INPUT+8
    
    call DoDESWithHL
    decoord 10, 2
    ld hl, wIPNeg1
    call WriteHexWord
    
.done
    jr .loop

.empty
    ld bc, $0207
    ld de, $110b
    call DrawBox
    
    decoord 9, 3
    ld hl, EmptyString
    call WriteString
    jr .loop
    
.loop
    halt
    ld a, [H_JOYNEW]
    and a
    jr z, .loop
    ret

EnterPasswordString:
    db "ENTER TEXT:@"

PressStartString:
    db "PRESS START@"
ToSubmitString:
    db "TO ENCRYPT@"

EmptyString:
    db "EMPTY@"

Letters:
    db "ABCDEFGHIJKLMNOPQRSTUVWXYZ@"
    

Tiles:
    INCBIN "gfx/tiles.2bpp"
TilesEnd

INCLUDE "des.asm"
