kup=1
cpu 65816
opt showtickcount-
org $2000

if kup=1
; F256 kup header
    byte   $f2,$56     ; signature
    byte   <(1+(endofsource-$2000) >> 13)   ; block count
    byte   <($2000) >> 13)                    ; start slot
    word   main        ; exec addr
    byte   1                 ; CHANGED, structure version
    byte   0                 ; reserved
    byte   0                 ; reserved
    byte   0                 ; reserved
    bytez 'spr128'; name
endif

define setaxl rep #$30
define setaxs sep #$30
define setal rep #$20
define setas sep #$20
define setxl rep #$10
define setxs sep #$10

define proc8 'mem 8 : index 8 : proc'
define proc16 'mem 16 : index 16 : proc'
define proc816 'mem 8 : index 16 : proc'
define proc168 'mem 16 : index 8 : proc'

directions = enum (updown=$01,leftright=$02)

proc8 main
    ; invalidate kup header
if kup=1
    stz $2000
endif

    stz $01
    php
    sei
    clc
    xce
    setaxs
;-
;    ; spinner
;    setas
;    lda #$40+2
;    sta $01
;    inc $c000
;    lda #$40
;    sta $01
;    setal
;    bra -
    
    ; init graphics mode
    lda $d000   ; text/overlay/graphics/sprites
    ora #($01|$02|$04|$20)
    sta $d000
    lda #$00    ; disable border
    sta $d004
    stz $d00d
    stz $d00e
    stz $d00f
    
    jsr clear_screen
    jsr init_clut0
    jsr build_sprites
    jsr init_sprites
    
    setaxl

restart:
    jsr init_bounce_values
    lda #64*3
    sta framecount
    
mode1:
    lda #$0000
--
    cmp $d01a
    bne --
    
    jsr move_balls_sideways
    dec framecount
    bne mode1
    
    lda #64*3
    sta framecount

mode2:
    lda #$0000
--
    cmp $d01a
    bne --
    
    jsr move_balls_diagonally
    dec framecount
    bne mode2
    
    lda #64*8
    sta framecount

mode3:
    lda #$0000
--
    cmp $d01a
    bne --

    jsr bounce_balls
    dec framecount
    bne mode3

    jmp restart

    framecount resw 1
endproc

proc16 init_bounce_values
    setas
    lda #1      ; enable random number generator
    sta $d6a6

    ldx #0
-
    lda $d6a4
    and #%11
    sta ball_dir,x
    lda $d6a5
    and #7
    inc
    sta ball_vel,x
    sta ball_delay,x
    inx
    cpx #128
    bne -

    setal
    rts
endproc

proc16 bounce_balls
tvel    = $24
tdir    = $26

    ; sprite block 0
    setas
    lda #$00
    sta $01
    setal
    lda #$d900
    sta $20

    ldx #0
-
    lda ball_delay,x
    and #$00ff
    dec
    bne +++
    
;    lda ball_vel,x
;    and #$00ff
    lda #1
    sta tvel
    lda ball_dir,x
    and #$00ff
    sta tdir
    bit #directions.updown
    bne +
    jsr move_up
    bra ++
+
    jsr move_down
++
    lda tdir
    bit #directions.leftright
    bne +
    jsr move_left
    bra ++
+
    jsr move_right
++
    setas
    lda tdir
    sta ball_dir,x
    lda ball_vel,x
;    setal
+++
    setas
    sta ball_delay,x
    setal
    clc
    lda $20
    adc #8
    sta $20
    inx
    cpx #64
    bne -

    ; sprite block 1
    setas
    lda #$40
    sta $01
    setal
    lda #$d900
    sta $20

-
    lda ball_delay,x
    and #$00ff
    dec
    bne +++
    
;    lda ball_vel,x
;    and #$00ff
    lda #1
    sta tvel
    lda ball_dir,x
    and #$00ff
    sta tdir
    bit #directions.updown
    bne +
    jsr move_up
    bra ++
+
    jsr move_down
++
    lda tdir
    bit #directions.leftright
    bne +
    jsr move_left
    bra ++
+
    jsr move_right
++
    setas
    lda tdir
    sta ball_dir,x
    lda ball_vel,x
;    setal
+++
    setas
    sta ball_delay,x
    setal
    clc
    lda $20
    adc #8
    sta $20
    inx
    cpx #128
    bne -
    
    rts

move_right:
    ldy #4
    clc
    lda ($20),y
    adc tvel
    cmp #(32-16+320)
    bcc +   ; less than
    sec
    sbc #(32-16+320+320-1)
    eor #$ffff
    inc
    clc
    adc #16
    sta ($20),y
    lda tdir    ; flip direction
    eor #directions.leftright
    sta tdir
    rts
+    
    sta ($20),y
    rts

move_left:    
    ldy #4
    sec
    lda ($20),y
    sbc tvel
    cmp #(32)
    bcs +   ; greater than or equal
    sec
    sbc #(32)
    eor #$ffff
    clc
    adc #(32+1)
    sta ($20),y
    lda tdir    ; flip direction
    eor #directions.leftright
    sta tdir
    rts
+    
    sta ($20),y
    rts

move_down:
    ldy #6
    clc
    lda ($20),y
    adc tvel
    cmp #(32-16+240)
    bcc +   ; less than
    sec
    sbc #(32-16+240+240-1)
    eor #$ffff
    inc
    clc
    adc #16
    sta ($20),y
    lda tdir    ; flip direction
    eor #directions.updown
    sta tdir
    rts
+    
    sta ($20),y
    rts

move_up:    
    ldy #6
    sec
    lda ($20),y
    sbc tvel
    cmp #(32)
    bcs +   ; greater than or equal
    sec
    sbc #(32)
    eor #$ffff
    clc
    adc #(32+1)
    sta ($20),y
    
    lda tdir    ; flip direction
    eor #directions.updown
    sta tdir
    rts
+    
    sta ($20),y
    rts
endproc

macro add1_ball
    clc
    lda ($20),y
    adc #1
endmacro

macro sub1_ball
    sec
    lda ($20),y
    sbc #1
endmacro

proc16 move_balls_diagonally
    ; sprite block 0
    setas
    lda #$00
    sta $01
    setal
    ldx #0
    lda #$d900
    sta $20
-
    ; xpos
    ldy #4
    sub1_ball()
    cmp #16
    bge +
    ; wrap around
    lda #(32+320)
+    
    sta ($20),y

    ; ypos
    ldy #6
    sub1_ball()
    cmp #16
    bge +
    ; wrap around
    lda #(32+240)
+    
    sta ($20),y
    
    clc
    lda $20
    adc #8
    sta $20
    inx
    cpx #64
    bne -

    ; sprite block 1
    setas
    lda #$40
    sta $01
    setal
    lda #$d900
    sta $20
-
    ; xpos
    ldy #4
    add1_ball()
    cmp #(32+320)
    blt +
    ; wrap around
    lda #16
+    
    sta ($20),y

    ; ypos
    ldy #6
    add1_ball()
    cmp #(32+240)
    blt +
    ; wrap around
    lda #16
+    
    sta ($20),y

    clc
    lda $20
    adc #8
    sta $20
    inx
    cpx #128
    bne -

    setas
    stz $01
    setal
    
    rts
endproc

proc16 move_balls_sideways
    ; sprite block 0
    setas
    lda #$00
    sta $01
    setal
    ldx #0
    lda #$d900
    sta $20
-
    ldy #4
    sub1_ball()
    cmp #16
    bge +
    ; wrap around
    lda #(32+320)
+    
    sta ($20),y
    
    clc
    lda $20
    adc #8
    sta $20
    inx
    cpx #64
    bne -

    ; sprite block 1
    setas
    lda #$40
    sta $01
    setal
    lda #$d900
    sta $20
-
    ldy #4
    add1_ball()
    cmp #(32+320)
    blt +
    ; wrap around
    lda #16
+    
    sta ($20),y

    clc
    lda $20
    adc #8
    sta $20
    inx
    cpx #128
    bne -

    setas
    stz $01
    setal
    
    rts
endproc

proc8 clear_screen
    php
    lda #2
    sta $01
    setxl
    lda #' '

    ldx #0
-
    sta $c000,x
    inx
    cpx #80*60
    bne -

    stz $01
    plp
    rts
endproc

proc8 init_clut0
    lda #1
    sta $01

    ; #0 black
    stz $d000
    stz $d001
    stz $d002
    stz $d003
    ; #1 dark blue
    lda #$80
    sta $d004
    stz $d005
    stz $d006
    stz $d007
    ; #2 white
    lda #$ff
    sta $d008
    sta $d009
    sta $d00a
    sta $d00b
    ; #3 grey
    lda #$60
    sta $d00c
    sta $d00d
    sta $d00e
    sta $d00f
    ; #4 light blue
    lda #$c0
    sta $d010
    stz $d011
    stz $d012
    stz $d013

    stz $01
    rts
endproc

proc8 init_sprites
    ; enable sprite bank 0
    lda #$00
    sta $01

    setaxl
    lda #numbersprites
    sta $20
    
    ; sprite bank 0
    lda #$d900
    sta $22
    ldx #0
-
    phx
    
    setas
    ; ctrl
    ldy #0
    lda #($01|(%10<<5))
    sta ($22),y
    ; vram bank address
    ldy #3
    lda #0
    sta ($22),y
    setal
    ; vram address
    ldy #1
    txa
    xba ; *256
    clc
    adc #numbersprites
    sta ($22),y

    txa
    asl : asl ; *4
    tax
    ; xpos
    ldy #4
    lda sprstartpos+0,x
    sta ($22),y
    ; ypos
    ldy #6
    lda sprstartpos+2,x
    sta ($22),y
    
    clc
    lda $22
    adc #8
    sta $22
    
    plx
    inx
    cpx #64
    bne -
    setaxs
    
    ; enable sprite bank 1
    lda #$40
    sta $01

    setaxl
    ; sprite bank 1
    lda #$d900
    sta $22
-
    phx

    ; ctrl
    ldy #0
    lda #($01|(%10<<5))
    sta ($22),y
    ; vram bank address
    ldy #3
    lda #0
    sta ($22),y
    setal
    ; vram address
    ldy #1
    txa
    xba ; *256
    clc
    adc #numbersprites
    sta ($22),y

    txa
    asl : asl ; *4
    tax
    ; xpos
    lda sprstartpos+0,x
    ldy #4
    sta ($22),y
    ; ypos
    lda sprstartpos+2,x
    ldy #6
    sta ($22),y
    
    clc
    lda $22
    adc #8
    sta $22
    
    plx
    inx
    cpx #128
    bne -
    setaxs

    stz $01
    rts
    sprstartpos:
    for spry = 0 to 7
        for sprx = 0 to 15
            word 32+(sprx*20),32+24+(spry*24)
        next
    next
endproc

proc8 build_sprites
    php
    setaxl
    
    ldx #0
-
    ; copy oval sprite
    txa
    xba ; *256
    clc
    adc #numbersprites
    sta $20
    jsr copy_oval

    ; copy first digit
    txa
    xba
    clc
    adc #(numbersprites+1+4*16)
    sta $22
    txa
    lsr : lsr : lsr : lsr   ; /16
    xba : lsr : lsr ; *64
    clc
    adc #digits
    sta $20
    jsr copy_digit

    ; copy second digit
    txa
    xba
    clc
    adc #(numbersprites+8+5*16)
    sta $22
    txa
    and #$0f
    xba : lsr : lsr ; *64
    clc
    adc #digits
    sta $20
    jsr copy_digit
    
    ; increase to next sprite
    inx
    cpx #128
    bne -

    plp
    rts

copy_oval:
    ldy #0
-
    lda oval,y
    sta ($20),y
    iny
    iny
    cpy #256
    bne -
    rts

copy_digit:
    setas
    ldy #0
-
    lda ($20)
    beq +
    sta ($22),y
+
    setal
    inc $20
    iny
    tya
    bit #8
    beq +
    clc
    adc #8
    tay
+
    setas
    cpy #128
    bne -
    setal
    rts
endproc
    
oval:
hexbytes
00 00 00 00 04 04 04 04 04 04 04 04 00 00 00 00
00 00 04 04 01 01 01 01 01 01 01 01 04 04 00 00
00 04 01 01 01 01 01 01 01 01 01 01 01 01 04 00
00 04 01 01 01 01 01 01 01 01 01 01 01 01 04 00
04 01 01 01 01 01 01 01 01 01 01 01 01 01 01 04
04 01 01 01 01 01 01 01 01 01 01 01 01 01 01 04
04 01 01 01 01 01 01 01 01 01 01 01 01 01 01 04
04 01 01 01 01 01 01 01 01 01 01 01 01 01 01 04
04 01 01 01 01 01 01 01 01 01 01 01 01 01 01 04
04 01 01 01 01 01 01 01 01 01 01 01 01 01 01 04
04 01 01 01 01 01 01 01 01 01 01 01 01 01 01 04
04 01 01 01 01 01 01 01 01 01 01 01 01 01 01 04
00 04 01 01 01 01 01 01 01 01 01 01 01 01 04 00
00 04 01 01 01 01 01 01 01 01 01 01 01 01 04 00
00 00 04 04 01 01 01 01 01 01 01 01 04 04 00 00
00 00 00 00 04 04 04 04 04 04 04 04 00 00 00 00
endhexbytes

digits:
hexbytes
;0
00 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
02 02 00 00 00 02 02 00
02 02 00 00 00 02 02 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 03 00
00 03 03 03 03 03 00 00
;1
00 00 00 02 02 00 00 00
00 00 02 02 02 00 00 00
00 00 02 02 02 00 00 00
00 00 03 02 02 00 00 00
00 00 00 02 02 00 00 00
00 00 00 02 02 00 00 00
00 00 02 02 02 02 00 00
00 00 03 03 03 03 00 00
;2
00 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
03 03 00 00 00 02 02 00
00 00 02 02 02 02 03 00
00 02 02 03 03 03 00 00
02 02 00 00 00 00 00 00
02 02 02 02 02 02 02 00
03 03 03 03 03 03 03 00
;3
00 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
03 03 00 00 00 02 02 00
00 00 00 02 02 02 03 00
00 00 00 03 03 02 02 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 03 00
00 03 03 03 03 03 00 00
;4
00 02 02 00 02 02 00 00
00 02 02 00 02 02 00 00
02 02 03 00 02 02 00 00
02 02 00 00 02 02 00 00
02 02 02 02 02 02 02 00
03 03 03 03 02 02 03 00
00 00 00 00 02 02 00 00
00 00 00 00 03 03 00 00
;5
02 02 02 02 02 02 02 00
02 02 03 03 03 03 03 00
02 02 00 00 00 00 00 00
02 02 02 02 02 02 00 00
03 03 03 03 03 02 02 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 03 00
00 03 03 03 03 03 00 00
;6
00 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 03 03 00
02 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 03 00
00 03 03 03 03 03 00 00
;7
02 02 02 02 02 02 02 00
03 03 03 03 03 02 02 00
00 00 00 00 02 02 03 00
00 00 00 00 02 02 00 00
00 00 00 02 02 03 00 00
00 00 00 02 02 00 00 00
00 00 00 02 02 00 00 00
00 00 00 03 03 00 00 00
;8
00 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 03 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 03 00
00 03 03 03 03 03 00 00
;9
00 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 02 00
00 03 03 03 03 02 02 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 03 00
00 03 03 03 03 03 00 00
;A
00 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
02 02 02 02 02 02 02 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
02 02 00 00 00 02 02 00
03 03 00 00 00 03 03 00
;B
02 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
02 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 02 02 00
02 02 02 02 02 02 03 00
03 03 03 03 03 03 00 00
;C
00 02 02 02 02 02 00 00
02 02 03 03 03 02 02 00
02 02 00 00 00 03 03 00
02 02 00 00 00 00 00 00
02 02 00 00 00 00 00 00
02 02 00 00 00 02 02 00
03 02 02 02 02 02 03 00
00 03 03 03 03 03 00 00
;D
02 02 02 02 02 00 00 00
02 02 03 03 02 02 00 00
02 02 00 00 03 02 02 00
02 02 00 00 00 02 02 00
02 02 00 00 00 02 02 00
02 02 00 00 02 02 03 00
02 02 02 02 02 03 00 00
03 03 03 03 03 00 00 00
;E
02 02 02 02 02 02 02 00
02 02 03 03 03 03 03 00
02 02 00 00 00 00 00 00
02 02 02 02 02 00 00 00
02 02 03 03 00 00 00 00
02 02 00 00 00 00 00 00
02 02 02 02 02 02 02 00
03 03 03 03 03 03 03 00
;F
02 02 02 02 02 02 02 00
02 02 03 03 03 03 03 00
02 02 00 00 00 00 00 00
02 02 02 02 02 00 00 00
02 02 03 03 00 00 00 00
02 02 00 00 00 00 00 00
02 02 00 00 00 00 00 00
03 03 00 00 00 00 00 00
endhexbytes

endofsource:

virtual *
ball_dir resb 128
ball_vel resb 128
ball_delay resb 128

align 2
numbersprites resb 128*16*16
endvirtual

savepgz 'spr128.pgz',$2000 to ~highestaddress,entry=main
