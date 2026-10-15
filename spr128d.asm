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

define jeq(aaddr) 'bne ~pcnext2 : jmp aaddr'
define jne(aaddr) 'beq ~pcnext2 : jmp aaddr'

define screen_width         320
define screen_height        240
define sprite_screen_offset 32
define sprite_width         16
define sprite_height        16
define max_sprites          128
define min_sprite_xpos      sprite_screen_offset
define max_sprite_xpos      sprite_screen_offset+screen_width
define min_sprite_ypos      sprite_screen_offset
define max_sprite_ypos      sprite_screen_offset+screen_height

define refreshline          0

directions = enum ( updown      = %01 ,
                    leftright   = %10)

macro flip_direction(adir)
    lda tdir
    eor #adir
    sta tdir
endmacro

macro add8ptr(aptr)
        clc
        lda aptr
        adc #8
        sta aptr
endmacro

macro if_keypressed_goto(alabel)
    lda $ddc2   ; check if k2 optical keyboard buffer count
    bne alabel  ; not equal to zero so branch to alabel
endmacro

macro wait_for_scanline(ascanline)
        lda #ascanline
    -
        cmp $d01a
        bne -
endmacro

proc8 main
        stz $01
        php
        sei
        clc     ; 65816 native mode
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
        
        jsr init_clut0
        
        setaxl
        jsr clear_screen
        jsr build_ball_sprites
        jsr init_ball_sprites
        ; wait for k2 optical keyboard buffer to be empty
    -
        lda $ddc0   ; read byte to clear buffer
        and #$0100
        beq -
            
    restart:

        lda #64*3
        sta framecount
    seg1:
        if_keypressed_goto(exit)
        wait_for_scanline(refreshline)
        jsr move_balls_sideways
        dec framecount
        bne seg1


        lda #64*3
        sta framecount
    seg2:
        if_keypressed_goto(exit)
        wait_for_scanline(refreshline)
        jsr move_balls_diagonally
        dec framecount
        bne seg2
        
        jsr init_bounce_balls
        lda #64*6
        sta framecount
    seg3:
        if_keypressed_goto(exit)
        wait_for_scanline(refreshline)
        jsr bounce_balls
        dec framecount
        bne seg3

        jsr init_gravity_balls
    seg4:
        if_keypressed_goto(exit)
        wait_for_scanline(refreshline)
        jsr gravity_balls
        cmp #max_sprites        ; compare number of deadballs
        bne seg4
    
        lda #32
        sta framecount
    seg5:
        if_keypressed_goto(exit)
        wait_for_scanline(refreshline)
        jsr balls_fall_off
        dec framecount
        bne seg5

        jmp main

    exit:
        sei
        setaxs
        lda #$de
        sta $d6a2
        lda #$ad
        sta $d6a3
        lda #$80
        sta $d6a0
        stz $d6a0
    
    framecount resw 1
endproc

proc16 balls_fall_off
    virtual $20
        sprptr  resw 1
    endvirtual
        ldx #0
        setas
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

        add8ptr(sprptr)
        inx
        cpx #max_sprites
        beq done
        cpx #max_sprites/2
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
        cpx #max_sprites
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
        and #$7fff  ; mask off invert bit
        adc tvel
        cmp #(max_sprite_ypos-sprite_height)    ; highest sprite y-position visisble on screen
        bcs hitbottom
        bit tvel        ; test sign value for tvel
        bpl +
            ora #$8000  ; invert bit based on velocity sign bit
    +
        sta (sprptr),y
        bra nextsprite

    hitbottom:
        ; reverse velocity
        setas
        lda ball_yvel,x
        dec
        eor #$ff
        inc
        sta ball_yvel,x
        bne stillmoving
        inc deadballs
        lda #0
        sta ball_vel,x

    stillmoving:
        lda ball_ysubv,x
        eor #$ff
        inc
        sta ball_ysubv,x
        setal

    nextsprite:
        add8ptr(sprptr)
        inx
        cpx #max_sprites
        beq done
        cpx #max_sprites/2
        jne updatesprite

        ; next sprite block
        setas
        lda #$40
        jmp updateblock
        mem 16
        
    done:
        lda deadballs
        and #$00ff
        rts
    
    move_right:
        ldy #4
        clc
        lda (sprptr),y
        and #$7fff  ; mask off mirror bit
        adc tvel
        cmp #(max_sprite_xpos - sprite_width)   ; highest sprite x-position visble on screen
        bcs hitedgebounceleftright
        asl             ; prepare to shift mirror bit into A / xpos
        pha
        lsr : lsr : lsr ; shift bit 2 into carry for mirror bit
        pla
        ror             ; shift carry back into A for mirror bit
        sta (sprptr),y
        rts
    
    move_left:    
        ldy #4
        sec
        lda (sprptr),y
        and #$7fff  ; mask off mirror bit
        sbc tvel
        cmp #(min_sprite_xpos)  ; lowest sprite x-position visible on screen
        bcc hitedgebounceleftright
        asl             ; prepare to shift mirror bit into A / xpos
        pha
        lsr : lsr : lsr ; shift bit 2 into carry for mirror bit
        pla
        ror             ; shift carry back into A for mirror bit
        sta (sprptr),y
        rts

    hitedgebounceleftright:
        flip_direction(directions.leftright)   ; bounce in opposite direction
        rts

    deadballs resw 1
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
        cpx #max_sprites
        bne -
    
        setal
        rts
endproc

proc16 bounce_balls
    virtual $20
        sprptr  resw 1
        tvel    resw 1
        tdir    resw 1
    endvirtual
    
        lda #1
        sta tvel
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
        add8ptr(sprptr)
        inx
        cpx #max_sprites
        beq done
        cpx #max_sprites/2
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
        and #$7fff  ; mask off mirror bit
        adc tvel
        cmp #(max_sprite_xpos - sprite_width)   ; highest sprite x-position visble on screen
        bcs hitedgebounceleftright
        sta (sprptr),y
        rts
    
    move_left:    
        ldy #4
        sec
        lda (sprptr),y
        and #$7fff  ; mask off mirror bit
        sbc tvel
        cmp #(min_sprite_xpos)  ; lowest sprite x-position visible on screen
        bcc hitedgebounceleftright
        ora #$8000  ; new k2 mirror image
        sta (sprptr),y
        rts

    hitedgebounceleftright:
        flip_direction(directions.leftright)   ; bounce in opposite direction
        rts
    
    move_down:
        ldy #6
        clc
        lda (sprptr),y
        and #$7fff  ; mask off invert bit
        adc tvel
        cmp #(max_sprite_ypos-sprite_height)    ; highest sprite y-position visisble on screee
        bcs hitedgebounceupdown
        sta (sprptr),y
        rts
    
    move_up:    
        ldy #6
        sec
        lda (sprptr),y
        and #$7fff  ; mask off invert bit
        sbc tvel
        cmp #(min_sprite_ypos)  ; lowest sprite-y position
        bcc hitedgebounceupdown
        ora #$8000  ; new k2 invert image
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
        cmp #(max_sprite_ypos)
        bcc +
        lda #(min_sprite_ypos-sprite_height)
        bra setypos
    +
        cmp #(min_sprite_ypos-sprite_height)
        bcs setypos
        ; wrap around
        lda #(max_sprite_ypos)
    setypos:   
        sta (sprptr),y
        
        add8ptr(sprptr)
        inx
        cpx #max_sprites
        beq done
        cpx #max_sprites/2
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
        
        ldy #-1
        sty tvel
        setas
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
        cmp #(max_sprite_xpos)
        bcc +
        lda #(min_sprite_xpos-sprite_width)
        bra setxpos
    +
        cmp #(min_sprite_xpos-sprite_width)
        bcs setxpos
        ; wrap around
        lda #(max_sprite_xpos)
    setxpos:
        sta (sprptr),y
        
        add8ptr(sprptr)
        inx
        cpx #max_sprites
        beq done
        cpx #max_sprites/2
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

proc16 clear_screen
        setas
        lda #2
        sta $01
        lda #' '

        ldx #0
    -
        sta $c000,x
        inx
        cpx #80*60
        bne -

        stz $01
        
        ldx #0
    -
        lda #2
        sta $01
        lda tx_pressanykey,x
        sta $c000+1*80+(80-sizeof(tx_pressanykey))/2,x
        lda #3
        sta $01
        lda #1<<4
        sta $c000+1*80+(80-sizeof(tx_pressanykey))/2,x
        inx
        cpx #sizeof(tx_pressanykey)
        bne -
        setal
        rts

    tx_pressanykey text '*** Press any key to exit ***'
endproc

proc8 init_clut0
        lda #1
        sta $01
    
        ldx #0
    -
        lda paldata,x
        sta $d000,x
        inx
        cpx #sizeof(paldata);(paldataend-paldata)
        bne -
        
        stz $01
        rts

    paldata hex 00 00 00 00 , // 00 black
                80 00 00 00 , // 01 dark blue
                ff ff ff 00 , // 02 white
                60 60 60 00 , // 03 grey
                60 00 00 00 , // 04 light blue
                00 00 80 00 , // 05 dark red
                00 00 c0 00 , // 06 light red
                00 80 00 00 , // 07 dark green
                00 c0 00 00 , // 08 light green
                00 b0 b0 00 , // 09 dark yellow
                00 f0 f0 00   // 0a light yellow
endproc

proc16 init_ball_sprites
    virtual $20
        sprptr  resw 1
    endvirtual

        ldx #0
        lda #$00        ; enable sprite bank 0
        
    updateblock:
        setas
        sta $01
        setal
        
        ; sprite bank 0
        lda #$d900
        sta sprptr
    -
        setas
        phx
        
        ; ctrl
        ldy #0
        lda #($01|(%10<<5))
        sta (sprptr),y
        ; vram bank address
        ldy #3
        lda #0
        sta (sprptr),y
        ldy #1
        setal
        ; vram address
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
        add8ptr(sprptr)
        plx
        inx
        cpx #max_sprites
        beq done
        cpx #max_sprites/2
        bne -
        lda #$40        ; enable sprite bank 1
        bra updateblock
        
    done:
        setas
        stz $01
        setal
        rts

    ; create initial sprite x/y positions table
    sprstartpos:
        for spry = 0 to 7
            for sprx = 0 to 15
                word sprite_screen_offset+(sprx*20) , sprite_screen_offset+24+(spry*24)
            next
        next
endproc

proc16 build_ball_sprites
    virtual $20
        sprptr      resw 1
        ovalptr     resw 1
        digitptr    resw 1
    endvirtual
        
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
        and #%11110000 : asl : asl ; /16*64
;        lsr : lsr : lsr : lsr   ; /16
;        asl : asl : asl : asl : asl : asl  ; *64
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
        cpx #max_sprites
        bne -
        rts
    
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
        cpy #(8*8*2)
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
    ball_dir    resb max_sprites
    ball_vel    resb max_sprites
    ball_delay  resb max_sprites
    ball_yvel   resb max_sprites
    ball_ysubv  resb max_sprites
    
    align 2     ; make sure sprite data falls on an even address
;    opt CONVERTC64STUDIOBINARYNUMBERS+
;    byte %....####
;    opt CONVERTC64STUDIOBINARYNUMBERS-
    resb 1
    numbersprites resb max_sprites*16*16    ; 128 sprite balls of 16x16 pixel sprite dimensions
endvirtual

print ~cpu

savepgz 'spr128d.pgz',$2000 to ~highestaddress,entry=main
