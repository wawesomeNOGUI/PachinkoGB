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
    cp a, $FE          ;Gameboy is 160 pixels wide, Clear All $D000 - $D0FE
    jr nz, .clearWRX


    ld hl, $D100   ;start of WR bank for storing y's
.clearWRY          ;clear space for storing real y's
    xor a
    ld [hli], a
    ld a, l
    cp a, $FE          ;Gameboy is 144 pixels wide, clear $D100 - $D1FE
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
    ld a, d       ;can only do hli with a
    ld [hli], a   ;ld x value into that location of WR & inc hl,
    inc a         ;do 4 times to give more x's for pin hit detection
    ;ld [hli], a
    ;inc a
    ;ld [hli], a
    ;inc a
    ;ld [hli], a
    inc a
    ld [hl], a   ;24
    ld d, a      ;set d to a

    ld a, d
    add a, 12    ;add 12 to x (usually 16, but four adds above)
    ld d, a

    ld h, $D1     ;y WR
    ld l, b
  ;  ld a, b       ;can only do hli with a
    ld [hl], b   ;ld y value into that location of WR
  ;  inc a         ;do 4 times to give more y's for pin hit detection
  ;  ld [hli], a
  ;  inc a
  ;  ld [hli], a
  ;  inc a
  ;  ld [hli], a
  ;  inc a
  ;  ld [hl], a    ;72
  ;  ld b, a       ;set b to a

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
    ld a, 80      ;current ball x (do +4 to get middle of ball)
    ld [$C000], a

    ld a, 50      ;current ball y  (do +4 to get middle of ball)
    ld [$C001], a

    ;the ball variables will be updated after every render


    ld a, 30       ;starting x velocity
    ld [$C002], a  ;x velocity
    ld a, 255      ;starting y velocity
    ld [$C003], a  ;y velocity

    ld a, 1
    ld [$C004], a ;%11111111=negative, 1=positive x velocity
    ld [$C005], a ;%11111111=negative, 1=positive y velocity

    ld a, 164
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

   ;ld a, $81
   ;ld [$FE06], a  ;pattern number

   ;ld a, 0  ;special sprite settings (bit 7 = 0, puts sprite over background and window)
   ;ld [$FE07], a




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
    cp a, 30        ;30 starting x speed in [$C002]
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

    ld a, [$FE00]       ;ld y
    ld [$C001], a

    jr Dropper


.ballDropped







;=====================================
;Main Looooop
;=====================================

   ld b, 0   ;counter for velocity x & y (speed)
   ld c, 0   ;counter for loop
   ld d, 0   ;counter for gravity
   ld e, 0   ;second counter for gravity
MainLoop:

;=====================
;Collision Calculations
;=====================

;Check if Hitting Walls First
   ld a, [$C000]           ;ball x
   cp a, 159               ;159 = right wall
   jr nc, .hitWall         ;if a >= 159

   ;if not hitting right wall, check if hitting left
   cp a, 9             ;9 = left wall
   jr z, .hitWall      ;if a == 9
   jr c, .hitWall      ;if a < 9

;Next Check if hitting a Pin
   ;first check x's
.CheckX
;   ld hl, $C004   ;x pos or neg velocity
;   add a, 2       ;offset to get center of circle instead of top left
;   add a, [hl]    ;+ or - 1 for ball's width

   ld h, $D0
   add a, 4          ;get middle of ball
   ld l, a           ;x location in WRAM ($C000)

   cp a, [hl]        ;x - [hl]  (check if [hl] == [$C000]) which means there's a pin there

   ;jr z, .CheckY     ;z == pin x there
   jr nz, .Animate   ;if [hl] and [$C000] aren't the same, goto animate



   ;if x equals a possible pin x, check y
.CheckY
   ld a, [$C001]    ;ld a with ball's y

   ld h, $D1
   add a, 4         ;get middle of ball
   ld l, a          ;y location in WRAM ($C001)


   cp a, [hl]       ;y - [hl]  (check if [hl] == [$C001]) which means there's a pin there
   jr z, .bounce    ;z == pin there
   jr nz, .Animate

   ;If the Program Counter Gets here, that means the ball's x & y were over
   ;a pin, so bounce!

.bounce
    ld a, %11111110
    ld hl, $C004     ;x neg or pos velocity
    xor a, [hl]      ;flip between %11111111 and %00000001 (add %11111111 == -1)
    ld [hl], a       ;store flipped value back in memory

    ld a, [$C000]    ;bump ball left or right 2
    add a, [hl]
    add a, [hl]
    ld [$C000], a

    ld a, %11111110
    ld hl, $C005     ;y neg or pos velocity
    xor a, [hl]      ;flip between %11111111 and %00000001 (add %11111111 == -1)
    ld [hl], a       ;store flipped value back in memory

    ld a, [$C001]    ;bump ball up or down 2
    add a, [hl]
    add a, [hl]
    ld [$C001], a

    ld a, [$C003]     ;take some y velocity away from bounce loss
    cp a, 240
    jr nc, .xTake

    ld a, %11111111
    cp a, [hl]        ;check if ball going up
    jr nz, .xTake

    ld a, [$C003]
    add a, 15         ;if going up take away velocity
    ld [$C003], a

    .xTake            ;take some x velocity away
    ld a, [$C002]
    cp a, 240
    jr nc, .Animate
    add a, 15
    ld [$C002], a

    jr .Animate


.hitWall           ;if hit wall, flip x velocity
  ld a, %11111110
  ld hl, $C004     ;x neg or pos velocity
  xor a, [hl]      ;flip between %11111111 and %00000001 (add %11111111 == -1)
  ld [hl], a       ;store neg or pos in x velocity flag variable

  ld a, [$C000]    ;move ball away from wall by two
  add a, [hl]
  add a, [hl]
  ld [$C000], a

;  .waitVBlankT
;     ld a, [rLY]
;     cp 144 ; Check if the LCD is in VBlank
;     jr c, .waitVBlankT
;
;      ld a, [$C004]
;      ld [$FE04], a

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

;First Update Ball x, y With Velocities

.velocityX
    inc b               ;x speed counter
    ld a, b
    ld hl, $C002
    cp a, [hl]
    jr nz, .velocityY

    ld b, 0    ;reset counter

  ;  dec b              ;counter for x velocity
                       ;(decrement so the Bigger $C002 is, the quicker the speed)
  ;  ld a, [$C002]      ;x speed (velocity)
  ;  cp a, b
  ;  jr nz, .draw   ;if != , jump to gravity

    ;else add or subtract 1 to x, and reset b  (subtract = add %11111111)
    ld hl, $C000       ;ball x
    ld a, [$C004]      ;neg or pos x velocity
    add a, [hl]        ;increment ball x in WRAM
    ld [hl], a


.velocityY
    inc c              ;y speed counter
    ld a, c
    ld hl, $C003       ;y speed
    cp a, [hl]
    jr nz, .draw

    ld c, 0    ;reset counter

    ;If ball goin up do grav to turn it around
    ld a, [$C005]      ;neg or pos y velocity
    cp a, %11111111    ;negative y velocity (ball moving up)
    jr nz, .reg        ;else do downward accel

    ;Do grav
    ld a, [$C003]
    ;inc [hl]           ;slow down upward movement
    add a, 15
    ld [$C003], a

    cp a, 255          ;max slowness
    jr nz, .resolve

    ;set y speed counter to slowest speed (if $C003 not 255)
    ;ld [hl], 255

    ld a, %11111110    ;flip velocity direction
    ld hl, $C005
    xor a, [hl]
    ld [hl], a

    .reg
    ld a, [$C003]
    cp a, 0
    jr z, .resolve
    sub a, 15
    ld [$C003], a

    .resolve
    ld a, [$C001]
    ld hl, $C005
    add a, [hl]        ;increment velocity in WRAM
    ld [$C001], a


.draw
    ld   hl,$FF41     ;-STAT Register
    .wait:           ;
    bit  1,[hl]       ; Wait until Mode is 0 or 1
    jr   nz,.wait    ;



     ;1140 M-cycles can be performed by the cpu during vblank

    ld a, [$C000]   ;x
    ld [$FE01], a
    ld a, [$C001]   ;y
    ld [$FE00], a


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
