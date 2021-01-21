INCLUDE "hardware.inc"

SECTION "Header", ROM0[$100]

EntryPoint: ; This is where execution begins
  di ; Disable interrupts. That way we can avoid dealing with them, especially since we didn't talk about them yet :p
  jp Start ; Leave this tiny space

REPT $150 - $104
  db 0
ENDR


SECTION "Game code", ROM0

Start:
    ; Turn off the LCD
.waitVBlank1
    ld a, [rLY]
    cp 144 ; Check if the LCD is past VBlank
    jr c, .waitVBlank1

    xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
    ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.


    ld hl, $9000
    ld de, FontTiles
    ld bc, FontTilesEnd - FontTiles
.copyFont
    ld a, [de] ; Grab 1 byte from the source
    ld [hli], a ; Place it at the destination, incrementing hl
    inc de ; Move to next byte
    dec bc ; Decrement count
    ld a, b ; Check if count is 0, since `dec bc` doesn't update flags
    or c
    jr nz, .copyFont

    ld hl, $8800
    ld de, GameTiles
    ld bc, GameTilesEnd - GameTiles
.copyGameStuff
    ld a, [de] ; Grab 1 byte from the source
    ld [hli], a ; Place it at the destination, incrementing hl
    inc de ; Move to next byte
    dec bc ; Decrement count
    ld a, b ; Check if count is 0, since `dec bc` doesn't update flags
    or c
    jr nz, .copyGameStuff


;==============================================
;Draw All The Pins(For the ball to bounce on)
;==============================================


    ld a, $80
    ld b, 0  ;counter used to remember when to change lines
    ld c, 15  ;counter to remember how far to move in memory when switching lines
    ld hl, $98C1
.placePins   ;there's 20x18 tiles can be shown at a time, but 32x32 in total
    ld a, $82
    ld [hli], a
    inc hl        ;increment twice to skip every other tile

    inc b
    ld a, b
    cp a, 9   ;switch lines if drew 9 pins (checks if b and c are equal)

    jr z, .pinsSwitchLine   ;if result 0, switch line
    jr .placePins          ;otherwise continue

.pinsSwitchLine
    ld a, l    ;switch lines (16bit addition)
    add a, c   ;add value in c (either 14 or 15)
    ld l, a
    ld a, h
    adc a, 0
    ld h, a

    ld a, c
    xor a, %00000010  ;flip flops c between 13 and 15 (to change how far to skip in memory)
    ld c, a

    ld b, 0  ;set b = 0   ;reset counter b

    ld a, h
    cp a, $9A  ;check to see if drawn pins all the way to line $9A
    jr nz, .placePins   ;if not, continue drawing pins

;====================================
;Put Middle point coords of pins in Work Ram
;====================================

    ld hl, $D000   ;start of second WR bank (we'll store x's here)
.clearWRX          ;This'll be a clear space to save a copy of the screen's
                   ;background tiles with real x instead of tile x
                   ;(for ball hit detection)
    xor a
    ld [hli], a
    ld a, l
    cp a, $A0          ;Gameboy is 160 pixels wide
    jr nz, .clearWRX


    ld hl, $D100   ;start of WR bank for storing y's
.clearWRY          ;clear space for storing real y's
    xor a
    ld [hli], a
    ld a, l
    cp a, $90          ;Gameboy is 144 pixels wide
    jr nz, .clearWRY

;After clearing area in work RAM, place the x and y locations of the pins in the
;respective areas of WR. ($D000 for x's, $D1000 for y's)

    ld hl, $D000 ;x pin map
    ld d, 20     ;x of top left pin
    ld a, 20     ;to remember x start
    ld [$C00F], a
    ld b, 68     ;y of top left pin
    ld c, 0      ;line counter
    ld e, 0      ;pin counter

.pinXY
    ld h, $D0    ;x WR
    ld l, d
    ld [hl], d   ;ld x value into that location of WR

    ld a, d
    add a, 16    ;add 16 to x
    ld d, a

    ld h, $D1    ;y WR
    ld l, b
    ld [hl], b   ;ld y value into that location of WR


    inc c        ;line counter
    inc e        ;pin counter

    ;check for how many pins written for
    ld a, e
    cp a, 90     ;for 90 pins
    jr z, .exit  ;if e = 90 goto exit else keep adding pins

    ld a, c
    cp a, 9
    jr nz, .pinXY

    ;if 9 then switch line
    ld a, [$C00F]
    xor a, %00001000    ;reset x to left (flip flop between 20, and 28)
    ld [$C00F], a
    ld d, a

    ld a, b             ;switch line so y + 8 pixels
    add a, 8
    ld b, a

    ld c, 0             ;reset counter

    jr .pinXY           ;then continue

.exit


;==============================================
;Side Walls Drawing
;==============================================

    ld hl, $9800
.drawWalls
    ld a, $81
    ld [hl], a

    ld a, l    ;switch lines (16bit addition)
    add a, $20   ;add value in c (either 14 or 15)
    ld l, a
    ld a, h
    adc a, 0
    ld h, a

    ld a, h
    cp a, $9A  ;check to see if drawn pins all the way to line $9A
    jr nz, .drawWalls   ;if not, continue drawing pins

    ld a, l    ;next check lower nibble
    cp a, $40  ;if l = $40 then continue, else go back
    jr nz, .drawWalls


    ld hl, $9813
.drawWallsRight
    ld a, $80
    ld [hl], a

    ld a, l    ;switch lines (16bit addition)
    add a, $20   ;add value in c (either 14 or 15)
    ld l, a
    ld a, h
    adc a, 0
    ld h, a

    ld a, h
    cp a, $9A  ;check to see if drawn pins all the way to line $9A
    jr nz, .drawWallsRight   ;if not, continue drawing pins

    ld a, l    ;next check lower nibble
    cp a, $53  ;if l = $40 then continue, else go back
    jr nz, .drawWallsRight


;========================================
;Ball Variables Setup
;========================================
    ld a, 84      ;x + 4   current ball x  (+4 to get middle of ball)
    ld [$C000], a

    ld a, 54      ;y + 4   current ball y  (+4 to get middle of ball)
    ld [$C001], a

    ;the ball variables will be updated after every render


    ld a, 100       ;starting x and y velocity
    ld [$C002], a ;x velocity
    ld [$C003], a ;y velocity

    ld a, 1
    ld [$C004], a ;%11111111=negative, 1=positive x velocity
    ld [$C005], a ;%11111111=negative, 1=positive y velocity

    ld a, 20
    ld [$C006], a ;gravity speed counter




   ;Init display registers
   ld a, %11100100
   ld [rBGP], a

   xor a ; ld a, 0
   ld [rSCY], a
   ld [rSCX], a


   ld hl, $FE00 ;start of OAM
.clearOAM
    xor a
    ld [hli], a
    ld a, l
    cp a, $9f
    jr nz, .clearOAM



   ld h, $D1
   ld l, 76
   ld a, [hl]
   ld [$FE04], a  ;y pos

   ld h, $D0
   ld l, 52
   ld a, 150
   ld [$FE05], a  ;x pos

   ld a, $81
   ld [$FE06], a  ;pattern number

   ld a, 0  ;special sprite settings (bit 7 = 0, puts sprite over background and window)
   ld [$FE07], a




   ;sprite data
   ld a, 34
   ld [_OAMRAM + OAMA_Y], a  ;y pos

   ld a, 20
   ld [_OAMRAM + OAMA_X], a  ;x pos

   ld a, $82
   ld [_OAMRAM + OAMA_TILEID], a  ;pattern number

   ld a, 0  ;special sprite settings (bit 7 = 0, puts sprite over background and window)
   ld [_OAMRAM + OAMA_FLAGS], a





   ; Shut sound down
   ld [rNR52], a

   ; Turn screen on, display background
   ;ld a, (LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON )
   ld a, (LCDCF_ON | LCDCF_BG8800 | LCDCF_BGON | LCDCF_OBJON) ;display sprites too
   ld [rLCDC], a


;================================================
;Dropper Script
;================================================

  ld b, 0    ;counter
Dropper:
;Button/ Dpad testing
    ;ld a, %00100000   ;looking for d-pad input
    ld a, %00010000   ;looking for button input
    ld [$FF00], a


    ld a, [$FF00]  ;good to check joypad register multiple times
    ld a, [$FF00]
    ld a, [$FF00]


    ;cp a, %11101101  ;looking for left press
    ;cp a, %11101110   ;looking for right press
    ;cp a, %11101011   ;looking for up press
    ;cp a, %11100111   ;looking for down press

    ;cp a, %11011101  ;looking for  b
    cp a, %11011110   ;looking for  a
    ;cp a, %11011011   ;looking for  select
    ;cp a, %11010111   ;looking for  start
    jr z, .ballDropped ;if a pressed drop ball




.waitVBlankDropper
    ld a, [rLY]
    cp 144 ; Check if the LCD is in VBlank
    jr c, .waitVBlankDropper

    ;Dropper Movement Delay
    inc b
    ld a, b
    cp a, 30
    jr nz, Dropper

    ld b, 0    ;reset counter

;move ball back and forth with dropper
;first check if ball near wall
    ld a, [$C000]                     ;ball x
    cp a, 150                         ;150 = slightly left of right wall
    jr nc, .flipDropperTrajectory     ;if a >= 150

    ;if not hitting right wall, check if hitting left
    cp a, 14                          ;slightly right of left wall
    jr z, .flipDropperTrajectory      ;if a ==
    jr c, .flipDropperTrajectory      ;if a <

    ;else, move dropper
    jr .moveDropper

.flipDropperTrajectory
    ld a, [$C004]
    xor a, %11111110    ;flip direction between 1 and negative 1
    ld [$C004], a       ;load new direction back into WRAM

.moveDropper
    ld hl, $C000        ;ball x
    ld a, [$C004]       ;x neg or pos
    add a, [hl]         ;add 1 or -1
    ld [hl], a          ;load new x into WRAM

    ld [$FE01], a       ;and load x into OAM

    jr Dropper


.ballDropped

;update WRAM ball X ball Y
    ld a, [$FE01]
    ld [$C000], a
    ld a, [$FE00]
    ld [$C001], a




;=====================================
;Main Looooop
;=====================================

   ld b, 0   ;counter for velocity x  (speed)
   ld c, 0   ;counter for velocity y  (speed)
   ld d, 0   ;counter for gravity
   ld e, 0   ;second counter for gravity
MainLoop:

;=====================
;Collision Calculations
;=====================

;Check if Hitting Walls First
   ld a, [$C000]           ;ball x
   cp a, 156               ;156 = right wall
   jr nc, .hitWall         ;if a >= 156

   ;if not hitting right wall, check if hitting left
   cp a, 4             ;4 = left wall
   jr z, .hitWall      ;if a == 4
   jr c, .hitWall      ;if a < 4

;Next Check if hitting a Pin
   ;first check x's
   ld hl, $C004  ;x pos or neg velocity
   add a, [hl]   ;+ or - 2 for ball's width
   add a, [hl]

   ld h, $D0
   ld l, a          ;x location in WRAM
   xor a            ;set a to zero
   cp a, [hl]       ;0 - [hl]  (check [hl] != 0) which means there's a pin there
   jr nz, .Animate  ;no zero flag set == not equal to 0, x not a pin, go to animate

   ;if x equals a possible pin x, check y
   ld a, [$C001]    ;ld a with ball's y
   ld hl, $C005     ;y pos or neg velocity
   add a, [hl]      ;+ or - 2 for ball's height
   add a, [hl]

   ld h, $D1
   ld l, a
   xor a
   cp a, [hl]       ;0 - [hl]  (check [hl] != 0) which means there's a pin there
   jr nz, .Animate

   ;If the Program Counter Gets here, that means the ball's x & y were over
   ;a pin, so bounce!

;Bounce
    ld a, %11111110
    ld hl, $C004     ;x neg or pos velocity
    xor a, [hl]      ;flip between %11111111 and %00000001 (add %11111111 == -1)
    ld [hl], a       ;store flipped value back in memory

    ld a, %11111110
    ld hl, $C005     ;y neg or pos velocity
    xor a, [hl]      ;flip between %11111111 and %00000001 (add %11111111 == -1)
    ld [hl], a       ;store flipped value back in memory

    ld d, 0          ;gravity back to 0
    ld e, 0

    jr .Animate


.hitWall           ;if hit wall, flip x velocity
  ld a, %11111110
  ld hl, $C004     ;x neg or pos velocity
  xor a, [hl]      ;flip between %11111111 and %00000001 (add %11111111 == -1)
  ld [hl], a       ;store neg or pos in x velcity flag variable
  jr .Animate






   ;Button/ Dpad testing
   ;ld a, %00100000   ;looking for d-pad input
   ;ld a, %00010000   ;looking for button input
   ;ld [$FF00], a

   ;.directionInput
    ;ld a, [$FF00]  ;good to check register multiple times?
    ;ld a, [$FF00]
    ;ld a, [$FF00]


    ;cp a, %11101101  ;looking for right press
    ;jr nz, .directionInput






;=====================
.Animate
;=====================

;1140 M-cycles can be performed by the cpu during vblank

.waitVBlank
   ld a, [rLY]
   cp 144 ; Check if the LCD is in VBlank
   jr c, .waitVBlank

.velocityX
    dec b              ;counter for x velocity
                       ;(decrement so the Bigger $C002 is, the quicker the speed)
    ld a, [$C002]      ;x speed (velocity)
    cp a, b
    jr nz, .velocityY   ;if != , jump to y velocity

    ;else add or subtract 1 to x, and reset b  (subtract = add %11111111)
    ld hl, $C000       ;ball x
    ld a, [$C004]      ;neg or pos x velocity
    add a, [hl]        ;increment velocity in WRAM
    ld [hl], a
    ld [$FE01], a      ;and increment in OAM
    ld b, 0            ;reset counter


.velocityY
    dec c              ;counter for y velocity
    ld a, [$C003]      ;y speed (velocity)
    cp a, c
    jr nz, .gravity    ;if != , jump to gravity

    ;else add or subtract 1 to y, and reset c  (subtract = add %11111111)
    ld hl, $C001       ;ball y
    ld a, [$C005]      ;neg or pos y velocity
    add a, [hl]        ;increment velocity in WRAM
    ld [hl], a
    ld [$FE00], a      ;and increment in OAM
    ld c, 0            ;reset counter y velocity


.gravity

    dec d              ;gravity counter
    ld a, d
    ld hl, $C006       ;gravity speed
    cp a, [hl]
    jr nz, .draw       ;if != then goto .draw

    ;else do gravity
    ld a, [$FE00]
    inc a              ;move sprite in OAM
    ld [$FE00], a

    ld a, [$C001]
    inc a              ;move sprite y in WRAM
    ld [$C001], a

    ld d, 0            ;reset counter



.draw
    ld a, [$FE01]
    ld [$C000], a
    ld a, [$FE00]
    ld [$C001], a


    jp MainLoop   ;return to main loop


SECTION "Font", ROM0

GameTiles:
INCBIN "GameTiles.2bpp"
GameTilesEnd:

FontTiles:
INCBIN "font.chr"
FontTilesEnd:



SECTION "Hello World string", ROM0

HelloWorldStr:
    db "Hello World! wassuuuppppppp", 0
