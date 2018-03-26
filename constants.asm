rJOYP EQU $ff00 ; Joypad (R/W)
rSB EQU $ff01 ; Serial transfer data (R/W)
rSC EQU $ff02 ; Serial Transfer Control (R/W)
rDIV EQU $ff04 ; Divider Register (R/W)
rTIMA EQU $ff05 ; Timer counter (R/W)
rTMA EQU $ff06 ; Timer Modulo (R/W)
rTAC EQU $ff07 ; Timer Control (R/W)
rIF EQU $ff0f ; Interrupt Flag (R/W)
rNR10 EQU $ff10 ; Channel 1 Sweep register (R/W)
rNR11 EQU $ff11 ; Channel 1 Sound length/Wave pattern duty (R/W)
rNR12 EQU $ff12 ; Channel 1 Volume Envelope (R/W)
rNR13 EQU $ff13 ; Channel 1 Frequency lo (Write Only)
rNR14 EQU $ff14 ; Channel 1 Frequency hi (R/W)
rNR21 EQU $ff16 ; Channel 2 Sound Length/Wave Pattern Duty (R/W)
rNR22 EQU $ff17 ; Channel 2 Volume Envelope (R/W)
rNR23 EQU $ff18 ; Channel 2 Frequency lo data (W)
rNR24 EQU $ff19 ; Channel 2 Frequency hi data (R/W)
rNR30 EQU $ff1a ; Channel 3 Sound on/off (R/W)
rNR31 EQU $ff1b ; Channel 3 Sound Length
rNR32 EQU $ff1c ; Channel 3 Select output level (R/W)
rNR33 EQU $ff1d ; Channel 3 Frequency's lower data (W)
rNR34 EQU $ff1e ; Channel 3 Frequency's higher data (R/W)
rNR41 EQU $ff20 ; Channel 4 Sound Length (R/W)
rNR42 EQU $ff21 ; Channel 4 Volume Envelope (R/W)
rNR43 EQU $ff22 ; Channel 4 Polynomial Counter (R/W)
rNR44 EQU $ff23 ; Channel 4 Counter/consecutive; Inital (R/W)
rNR50 EQU $ff24 ; Channel control / ON-OFF / Volume (R/W)
rNR51 EQU $ff25 ; Selection of Sound output terminal (R/W)
rNR52 EQU $ff26 ; Sound on/off
rLCDC EQU $ff40 ; LCD Control (R/W)
rSTAT EQU $ff41 ; LCDC Status (R/W)
rSCY EQU $ff42 ; Scroll Y (R/W)
rSCX EQU $ff43 ; Scroll X (R/W)
rLY EQU $ff44 ; LCDC Y-Coordinate (R)
rLYC EQU $ff45 ; LY Compare (R/W)
rDMA EQU $ff46 ; DMA Transfer and Start Address (W)
rBGP EQU $ff47 ; BG Palette Data (R/W) - Non CGB Mode Only
rOBP0 EQU $ff48 ; Object Palette 0 Data (R/W) - Non CGB Mode Only
rOBP1 EQU $ff49 ; Object Palette 1 Data (R/W) - Non CGB Mode Only
rWY EQU $ff4a ; Window Y Position (R/W)
rWX EQU $ff4b ; Window X Position minus 7 (R/W)
rKEY1 EQU $ff4d ; CGB Mode Only - Prepare Speed Switch
rVBK EQU $ff4f ; CGB Mode Only - VRAM Bank
rHDMA1 EQU $ff51 ; CGB Mode Only - New DMA Source, High
rHDMA2 EQU $ff52 ; CGB Mode Only - New DMA Source, Low
rHDMA3 EQU $ff53 ; CGB Mode Only - New DMA Destination, High
rHDMA4 EQU $ff54 ; CGB Mode Only - New DMA Destination, Low
rHDMA5 EQU $ff55 ; CGB Mode Only - New DMA Length/Mode/Start
rRP EQU $ff56 ; CGB Mode Only - Infrared Communications Port
rBGPI EQU $ff68 ; CGB Mode Only - Background Palette Index
rBGPD EQU $ff69 ; CGB Mode Only - Background Palette Data
rOBPI EQU $ff6a ; CGB Mode Only - Sprite Palette Index
rOBPD EQU $ff6b ; CGB Mode Only - Sprite Palette Data
rUNKNOWN1 EQU $ff6c ; (FEh) Bit 0 (Read/Write) - CGB Mode Only
rSVBK EQU $ff70 ; CGB Mode Only - WRAM Bank
rUNKNOWN2 EQU $ff72 ; (00h) - Bit 0-7 (Read/Write)
rUNKNOWN3 EQU $ff73 ; (00h) - Bit 0-7 (Read/Write)
rUNKNOWN4 EQU $ff74 ; (00h) - Bit 0-7 (Read/Write) - CGB Mode Only
rUNKNOWN5 EQU $ff75 ; (8Fh) - Bit 4-6 (Read/Write)
rUNKNOWN6 EQU $ff76 ; (00h) - Always 00h (Read Only)
rUNKNOWN7 EQU $ff77 ; (00h) - Always 00h (Read Only)
rIE EQU $ffff ; Interrupt Enable (R/W)



; Custom macro for easily selecting coordinates on screen
W_MAP EQU $C000

;\1 = X
;\2 = Y
FuncCoord: MACRO
Coord = W_MAP + 20 * \2 + \1
	ENDM

bccoord: MACRO
    FuncCoord \2, \1
    ld de, Coord
    ENDM
	
decoord: MACRO
    FuncCoord \2, \1
    ld de, Coord
    ENDM

hlcoord: MACRO
    FuncCoord \2, \1
    ld hl, Coord
    ENDM

lda: MACRO
    ld a, \2
    ld \1, a
ENDM

; Other variables

W_INPUT EQU $C500
W_DECR EQU $C600

H_VCOPY_D EQU $FF90
H_VCOPY_E EQU $FF91
H_VCOPY_H EQU $FF92
H_VCOPY_L EQU $FF93
H_VCOPY_TIMES EQU $FF94
H_VCOPY_ROWS EQU $FF95

H_SELECTION EQU $FFC0
H_POSITION EQU $FFC1
H_CURSOR EQU $FFC2

H_DECTMP EQU $FFE0

H_TIMER EQU $FFF0
H_RNG1 EQU $FFF1
H_JOY EQU $fff8
H_JOYOLD EQU $fff9
H_JOYNEW EQU $fffA
