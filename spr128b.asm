cpu 65816
org $2000

; default assembler setting auto-sets 8/16 bit registers with rep/sep instruction
define setaxl rep #$30
define setaxs sep #$30
define setal rep #$20
define setas sep #$20
define setxl rep #$10
define setxs sep #$10

; shortcut for starting a procedure with 8/16 bit registers
define proc8 'mem 8 : index 8 : proc'
define proc16 'mem 16 : index 16 : proc'
define proc816 'mem 8 : index 16 : proc'
define proc168 'mem 16 : index 8 : proc'


define screen_width         320
define screen_height        240
define sprite_screen_offset 32
define sprite_width         16
define sprite_height        16
define min_sprite_xpos      sprite_screen_offset
define max_sprite_xpos      sprite_screen_offset+screen_width
define min_sprite_ypos      sprite_screen_offset
define max_sprite_ypos      sprite_screen_offset+screen_height

directions = enum ( updown      = %01 ,
                    leftright   = %10)

macro flip_direction(adir)
    lda tdir
    eor #adir
    sta tdir
endmacro

proc8 main
    macro wait_for_sof
        lda #0
        -
            cmp $d01a
            bne -
    endmacro
    
        stz $01
        php
        sei
        clc
        xce
        setaxs
        
        ; init graphics mode
        lda $d000   ; text/overlay/graphics/sprites
        ora #($01|$02|$04|$20)
        sta $d000
        lda #$00    ; disable border
        sta $d004
        stz $d00d
        stz $d00e
        stz $d00f
        lda #1      ; enable random number generator
        sta $d6a6
        
        jsr clear_screen
        jsr init_clut0
        jsr build_ball_sprites
        jsr init_ball_sprites
        
        setaxl
    
    restart:
        lda #64*3
        sta framecount
    mode1:
        wait_for_sof()
        jsr move_balls_sideways
        dec framecount
        bne mode1
        
        lda #64*3
        sta framecount
    mode2:
        wait_for_sof()
        jsr move_balls_diagonally
        dec framecount
        bne mode2
        
        jsr init_bounce_balls
        lda #64*6
        sta framecount
    mode3:
        wait_for_sof()
        jsr bounce_balls
        dec framecount
        bne mode3

        jsr init_gravity_balls
    mode4:
        wait_for_sof()
        jsr gravity_balls
        cmp #128
        bne mode4
    
        lda #32
        sta framecount
    mode5:
        wait_for_sof()
        jsr balls_fall_off
        dec framecount
        bne mode5

        jmp main;restart
    
    framecount resw 1
endproc

proc16 balls_fall_off
    virtual $20
        sprptr  resw 1
    endvirtual
        setas
        ldx #0
        lda #$00
    
    updateblock:
        sta $01
        setal
        lda #$d900
        sta sprptr
    
    updatesprite:
        ldy #6
        lda (sprptr),y
        inc
        sta (sprptr),y

        clc
        lda sprptr
        adc #8
        sta sprptr
        inx
        cpx #128
        beq done
        cpx #64
        bne updatesprite

        ; next sprite block
        setas
        lda #$40
        bra updateblock
        mem 16
        
    done:
        rts
endproc

proc16 init_gravity_balls
        ; initialize ball_maxy from current ball yposition
        ldx #0
        setas
    -
        lda ball_vel,x
        pha
        asl
        cmp #8
        bcc +
        lda #8
    +
        sta ball_vel,x
        sta ball_delay,x
        pla
        sta ball_yvel,x
        sec
        lda #8
        sbc ball_yvel,x
        lsr
        sta ball_yvel,x
        lda #0
        sta ball_ysubv,x
        lda ball_dir,x
        bit #directions.updown
        bne +
        lda ball_yvel,x
        eor #$ff
        inc
        sta ball_yvel,x
        lda #$ff
        sta ball_ysubv,x
    +
        inx
        cpx #128
        bne -
        setal
        rts
endproc

proc16 gravity_balls
    gravity     = 23
    virtual $20
        sprptr  resw 1
        tvel    resw 1
        tdir    resw 1
    endvirtual

    ; start gravity bounce
        setas
        ldx #0
        stz deadballs
        lda #$00
    
    updateblock:
        sta $01
        setal
        lda #$d900
        sta sprptr
    
    updatesprite:
        lda ball_delay,x
        and #$00ff
        dec
        bne setdelay
        
        lda #1
        sta tvel
        lda ball_dir,x
        and #$00ff
        sta tdir
        bit #directions.leftright
        bne .moveright
        jsr move_left
        bra +
    
    .moveright:
        jsr move_right
    +
        setas
        lda tdir
        sta ball_dir,x
        lda ball_vel,x

    setdelay:
        setas
        sta ball_delay,x
        
        clc
        lda ball_ysubv,x
        adc #gravity
        sta ball_ysubv,x
        lda ball_yvel,x
        adc #0
        cmp #9
        bne +
        lda #8
    +
        sta ball_yvel,x
        sta tvel
        stz tvel+1
        bit #$80
        beq +
        dec tvel+1
    +

        setal
        ldy #6
        clc
        lda (sprptr),y
        adc tvel
        cmp #(max_sprite_ypos-sprite_height)    ; highest sprite y-position visisble on screen
        bcs .offscreen
        sta (sprptr),y
        bra +

    .offscreen:
        ; reverse velocity
        setas
        lda ball_yvel,x
        dec
        eor #$ff
        inc
        sta ball_yvel,x
        bne ++
        inc deadballs
        lda #0
        sta ball_vel,x
        ++
        lda ball_ysubv,x
        eor #$ff
        inc
        sta ball_ysubv,x
        setal
    +
        clc
        lda sprptr
        adc #8
        sta sprptr
        inx
        cpx #128
        beq done
        cpx #64
        beq +
        jmp updatesprite
    +
        ; next sprite block
        setas
        lda #$40
        jmp updateblock
        mem 16
        
    done:
        lda deadballs
        and #$00ff
        rts

    deadballs resw 1
    
    move_right:
        ldy #4
        clc
        lda (sprptr),y
        adc tvel
        cmp #(max_sprite_xpos - sprite_width)   ; highest sprite x-position visble on screen
        bcs .hitedge
        sta (sprptr),y
        rts

    .hitedge:
        ; off screen, bounce in opposite direction
        sec
        sbc tvel
        sta (sprptr),y
        flip_direction(directions.leftright)
        rts
    
    move_left:    
        ldy #4
        sec
        lda (sprptr),y
        sbc tvel
        cmp #(min_sprite_xpos)  ; lowest sprite x-position visible on screen
        bcc .hitedge
        sta (sprptr),y
        rts

    .hitedge:
        ; off screen, bounce in opposite direction
        clc
        adc tvel
        sta (sprptr),y
        flip_direction(directions.leftright)
        rts
    
    move_down:
        ldy #6
        clc
        lda (sprptr),y
        adc tvel
        cmp #(max_sprite_ypos-sprite_height)    ; highest sprite y-position visisble on screee
        bcs .offscreen
        sta (sprptr),y
        rts

    .offscreen:
        lda ball_yvel,x
        eor #$ff
        inc
        sta ball_yvel,x
        lda ball_ysubv,x
        eor #$ff
        inc
        sta ball_ysubv,x
        flip_direction(directions.updown)
        rts
    
    move_up:    
        ldy #6
        clc
        lda (sprptr),y
        adc tvel
        cmp #(min_sprite_ypos)  ; lowest sprite-y position
        bcc .offscreen
        sta (sprptr),y
        rts

    .offscreen:
        lda #0
        sta ball_yvel,x
        sta ball_ysubv,x
        flip_direction(directions.updown)
        rts
endproc

proc16 init_bounce_balls
        setas
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
    virtual $20
        sprptr  resw 1
        tdir    resw 1
    endvirtual
    
        setas
        ldx #0
        lda #$00
    
    updateblock:
        sta $01
        setal
        lda #$d900
        sta sprptr
    
    updatesprite:
        lda ball_delay,x
        and #$00ff
        dec
        bne setdelay
        
        lda ball_dir,x
        and #$00ff
        sta tdir
        bit #directions.updown
        bne movedown
        jsr move_up
        bra checkleftright
    
    movedown:
        jsr move_down
    
    checkleftright:
        lda tdir
        bit #directions.leftright
        bne moveright
        jsr move_left
        bra setdirection
    
    moveright:
        jsr move_right
    
    setdirection:
        setas
        lda tdir
        sta ball_dir,x
        lda ball_vel,x
    
    setdelay:
        setas
        sta ball_delay,x
        setal
        clc
        lda sprptr
        adc #8
        sta sprptr
        inx
        cpx #128
        beq done
        cpx #64
        bne updatesprite

        ; next sprite block
        setas
        lda #$40
        bra updateblock
        mem 16
        
    done:
        rts
    
    move_right:
        ldy #4
        clc
        lda (sprptr),y
        adc #1
        cmp #(max_sprite_xpos - sprite_width)   ; highest sprite x-position visble on screen
        bcs hitedgebounceleftright
        sta (sprptr),y
        rts
    
    move_left:    
        ldy #4
        sec
        lda (sprptr),y
        sbc #1
        cmp #(min_sprite_xpos)  ; lowest sprite x-position visible on screen
        bcc hitedgebounceleftright
        sta (sprptr),y
        rts

    hitedgebounceleftright:
        flip_direction(directions.leftright)   ; bounce in opposite direction
        rts
    
    move_down:
        ldy #6
        clc
        lda (sprptr),y
        adc #1
        cmp #(max_sprite_ypos-sprite_height)    ; highest sprite y-position visisble on screee
        bcs hitedgebounceupdown
        sta (sprptr),y
        rts
    
    move_up:    
        ldy #6
        sec
        lda (sprptr),y
        sbc #1
        cmp #(min_sprite_ypos)  ; lowest sprite-y position
        bcc hitedgebounceupdown
        sta (sprptr),y
        rts

    hitedgebounceupdown:
        flip_direction(directions.updown)
        rts
endproc


proc16 move_balls_diagonally
    virtual $20
        sprptr  resw 1
        tvel    resw 1
    endvirtual

    macro addtvel_ball
        clc
        lda (sprptr),y
        adc tvel
    endmacro

        setas
        ldy #-1
        sty tvel
        ldx #0
        lda #$00        ; set sprite block 0

    updateblock:
        sta $01
        setal
        lda #$d900
        sta sprptr

    updatesprite:
        ; xpos
        ldy #4
        addtvel_ball()
        cmp #(max_sprite_xpos)
        bcc +
        lda #(min_sprite_xpos-sprite_width) ; wrap on left side
        bra setxpos
    +
        cmp #(min_sprite_xpos-sprite_width)
        bcs setxpos
        ; wrap around
        lda #(max_sprite_xpos)
    setxpos:
        sta (sprptr),y
    
        ; ypos
        ldy #6
        addtvel_ball()
        cmp #(32+240)
        bcc +
        lda #(32-16)
        bra setypos
    +
        cmp #(32-16)
        bcs setypos
        ; wrap around
        lda #(32+240)
    setypos:   
        sta (sprptr),y
        
        clc
        lda sprptr
        adc #8
        sta sprptr
        inx
        cpx #128
        beq done
        cpx #64
        bne updatesprite

        setas
        ldy #1
        sty tvel
        lda #$40        ; set sprite block 1
        bra updateblock
        mem 16

    done:
        rts
endproc

proc16 move_balls_sideways
    virtual $20
        sprptr  resw 1
        tvel    resw 1
    endvirtual

    macro addtvel_ball
        clc
        lda (sprptr),y
        adc tvel
    endmacro
        
        setas
        ldy #-1
        sty tvel
        ldx #0
        lda #$00        ; sprite block 0

    updateblock:
        sta $01
        setal
        lda #$d900
        sta sprptr

    updatesprite:
        ldy #4
        addtvel_ball()
        cmp #(32+320)
        bcc +
        lda #(32-16)
        bra setxpos
    +
        cmp #(32-16)
        bcs setxpos
        ; wrap around
        lda #(32+320)
    setxpos:
        sta (sprptr),y
        
        clc
        lda sprptr
        adc #8
        sta sprptr
        inx
        cpx #128
        beq done
        cpx #64
        bne updatesprite
    
        
        setas
        ldy #1
        sty tvel
        lda #$40        ; sprite block 1
        bra updateblock
        mem 16

    done:
        rts
endproc

proc8 clear_screen
        lda #2
        sta $01
        lda #' '
        setxl

        ldx #0
    -
        sta $c000,x
        inx
        cpx #80*60
        bne -

        setxs
        stz $01
        rts
endproc

proc8 init_clut0
        lda #1
        sta $01
    
        ldx #0
    -
        lda paldata,x
        sta $d000,x
        inx
        cpx #(paldataend-paldata)
        bne -
        
        stz $01
        rts
    paldata:
    hexbytes
        00 00 00 00 ; 00 black
        80 00 00 00 ; 01 dark blue
        ff ff ff 00 ; 02 white
        60 60 60 00 ; 03 grey
        60 00 00 00 ; 04 light blue
        00 00 80 00 ; 05 dark red
        00 00 c0 00 ; 06 light red
        00 80 00 00 ; 07 dark green
        00 c0 00 00 ; 08 light green
        00 b0 b0 00 ; 09 dark yellow
        00 f0 f0 00 ; 0a light yellow
    endhexbytes
    paldataend:
        
endproc

proc8 init_ball_sprites
    virtual $20
        sprptr  resw 1
    endvirtual
       
        lda #$00        ; enable sprite bank 0
        sta $01
    
        setaxl
        
        ; sprite bank 0
        lda #$d900
        sta sprptr
        ldx #0
    -
        phx
        
        setas
        ; ctrl
        ldy #0
        lda #($01|(%10<<5))
        sta (sprptr),y
        ; vram bank address
        ldy #3
        lda #0
        sta (sprptr),y
        setal
        ; vram address
        ldy #1
        txa
        xba ; *256
        clc
        adc #numbersprites
        sta (sprptr),y
    
        txa
        asl : asl ; *4
        tax
        ; xpos
        ldy #4
        lda sprstartpos+0,x
        sta (sprptr),y
        ; ypos
        ldy #6
        lda sprstartpos+2,x
        sta (sprptr),y
        
        clc
        lda sprptr
        adc #8
        sta sprptr
        
        plx
        inx
        cpx #64
        bne -
        setaxs
        
        
        lda #$40        ; enable sprite bank 1
        sta $01
    
        setaxl

        ; sprite bank 1
        lda #$d900
        sta sprptr
    -
        phx
    
        ; ctrl
        ldy #0
        lda #($01|(%10<<5))
        sta (sprptr),y
        ; vram bank address
        ldy #3
        lda #0
        sta (sprptr),y
        setal
        ; vram address
        ldy #1
        txa
        xba ; *256
        clc
        adc #numbersprites
        sta (sprptr),y
    
        txa
        asl : asl ; *4
        tax
        ; xpos
        lda sprstartpos+0,x
        ldy #4
        sta (sprptr),y
        ; ypos
        lda sprstartpos+2,x
        ldy #6
        sta (sprptr),y
        
        clc
        lda sprptr
        adc #8
        sta sprptr
        
        plx
        inx
        cpx #128
        bne -
        setaxs
    
        stz $01
        rts

    ; create initial sprite x/y positions table
    sprstartpos:
        for spry = 0 to 7
            for sprx = 0 to 15
                word 32+(sprx*20),32+24+(spry*24)
            next
        next
endproc

proc8 build_ball_sprites
    virtual $20
        sprptr      resw 1
        ovalptr     resw 1
        digitptr    resw 1
    endvirtual
        setaxl
        
        ; copy oval sprite
        ldx #0              ; start at sprite ball #0
    -
        txa
        xba ; *256
        clc
        adc #numbersprites  ; sprdata dest=sprite ball number*256+#numbersprite
        sta sprptr
        jsr copy_oval
    
        ; copy first digit/high nibble $x0
        txa
        xba ; *256
        clc
        adc #(numbersprites+1+4*16) ; high nibble sprdata dest=sprite ball number*256+#numbersprite+1 pixel over+4 pixels down
        sta sprptr
        txa
        lsr : lsr : lsr : lsr   ; /16
        asl : asl : asl : asl : asl : asl  ; *64
        clc
        adc #digits ; digit char data=sprite ball number/16*256/4
        sta digitptr
        jsr copy_digit
    
        ; copy second digit/low nibble $0x
        txa
        xba
        clc
        adc #(numbersprites+8+5*16) ; low nibble sprdata dest=sprite ball number*256+#numbersprite+8 pixel over+5 pixels down
        sta sprptr
        txa
        and #$0f
        xba : lsr : lsr ; *256/4
        clc
        adc #digits ; digit char data=sprite ball number/16*256/4
        sta digitptr
        jsr copy_digit
        
        ; increase to next sprite
        inx
        cpx #128
        bne -
    
        setaxs
        rts
    
    mem 16
    index 16

    copy_oval:
        txa
        and #3
        xba
        adc #ovals
        sta ovalptr
        ldy #0
    -
        lda (ovalptr),y
        sta (sprptr),y
        iny
        iny
        cpy #(16*16)
        bne -
        rts
    
    copy_digit:
        setas
        ldy #0
    -
        lda (digitptr)
        beq +
        sta (sprptr),y
    +
        setal
        inc digitptr
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

    ovals:
    ; blue
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
    ; red
        hexbytes
        00 00 00 00 06 06 06 06 06 06 06 06 00 00 00 00
        00 00 06 06 05 05 05 05 05 05 05 05 06 06 00 00
        00 06 05 05 05 05 05 05 05 05 05 05 05 05 06 00
        00 06 05 05 05 05 05 05 05 05 05 05 05 05 06 00
        06 05 05 05 05 05 05 05 05 05 05 05 05 05 05 06
        06 05 05 05 05 05 05 05 05 05 05 05 05 05 05 06
        06 05 05 05 05 05 05 05 05 05 05 05 05 05 05 06
        06 05 05 05 05 05 05 05 05 05 05 05 05 05 05 06
        06 05 05 05 05 05 05 05 05 05 05 05 05 05 05 06
        06 05 05 05 05 05 05 05 05 05 05 05 05 05 05 06
        06 05 05 05 05 05 05 05 05 05 05 05 05 05 05 06
        06 05 05 05 05 05 05 05 05 05 05 05 05 05 05 06
        00 06 05 05 05 05 05 05 05 05 05 05 05 05 06 00
        00 06 05 05 05 05 05 05 05 05 05 05 05 05 06 00
        00 00 06 06 05 05 05 05 05 05 05 05 06 06 00 00
        00 00 00 00 06 06 06 06 06 06 06 06 00 00 00 00
        endhexbytes
    ; green
        hexbytes
        00 00 00 00 08 08 08 08 08 08 08 08 00 00 00 00
        00 00 08 08 07 07 07 07 07 07 07 07 08 08 00 00
        00 08 07 07 07 07 07 07 07 07 07 07 07 07 08 00
        00 08 07 07 07 07 07 07 07 07 07 07 07 07 08 00
        08 07 07 07 07 07 07 07 07 07 07 07 07 07 07 08
        08 07 07 07 07 07 07 07 07 07 07 07 07 07 07 08
        08 07 07 07 07 07 07 07 07 07 07 07 07 07 07 08
        08 07 07 07 07 07 07 07 07 07 07 07 07 07 07 08
        08 07 07 07 07 07 07 07 07 07 07 07 07 07 07 08
        08 07 07 07 07 07 07 07 07 07 07 07 07 07 07 08
        08 07 07 07 07 07 07 07 07 07 07 07 07 07 07 08
        08 07 07 07 07 07 07 07 07 07 07 07 07 07 07 08
        00 08 07 07 07 07 07 07 07 07 07 07 07 07 08 00
        00 08 07 07 07 07 07 07 07 07 07 07 07 07 08 00
        00 00 08 08 07 07 07 07 07 07 07 07 08 08 00 00
        00 00 00 00 08 08 08 08 08 08 08 08 00 00 00 00
        endhexbytes
    ; yellow
        hexbytes
        00 00 00 00 0a 0a 0a 0a 0a 0a 0a 0a 00 00 00 00
        00 00 0a 0a 09 09 09 09 09 09 09 09 0a 0a 00 00
        00 0a 09 09 09 09 09 09 09 09 09 09 09 09 0a 00
        00 0a 09 09 09 09 09 09 09 09 09 09 09 09 0a 00
        0a 09 09 09 09 09 09 09 09 09 09 09 09 09 09 0a
        0a 09 09 09 09 09 09 09 09 09 09 09 09 09 09 0a
        0a 09 09 09 09 09 09 09 09 09 09 09 09 09 09 0a
        0a 09 09 09 09 09 09 09 09 09 09 09 09 09 09 0a
        0a 09 09 09 09 09 09 09 09 09 09 09 09 09 09 0a
        0a 09 09 09 09 09 09 09 09 09 09 09 09 09 09 0a
        0a 09 09 09 09 09 09 09 09 09 09 09 09 09 09 0a
        0a 09 09 09 09 09 09 09 09 09 09 09 09 09 09 0a
        00 0a 09 09 09 09 09 09 09 09 09 09 09 09 0a 00
        00 0a 09 09 09 09 09 09 09 09 09 09 09 09 0a 00
        00 00 0a 0a 09 09 09 09 09 09 09 09 0a 0a 00 00
        00 00 00 00 0a 0a 0a 0a 0a 0a 0a 0a 00 00 00 00
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
endproc
    

endofsource:

virtual *
    ball_dir    resb 128
    ball_vel    resb 128
    ball_delay  resb 128
    ball_yvel   resb 128
    ball_ysubv  resb 128
    
    align 2     ; make sure sprite data falls on an even address
    numbersprites resb 128*16*16    ; 128 sprite balls of 16x16 pixel sprite dimensions
endvirtual

savepgz 'spr128b.pgz',$2000 to ~highestaddress,entry=main
