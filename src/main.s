.define emptychar			$ff80							; size = 64

.define screen0				$b000
.define screen1				$e000

.define palette				$c000

.define lineartable			$c800
.define slope_top			$ca00

.define screenchars0		$10000
.define screenchars1		$20000
.define screenchars2		$30000
.define screenchars3		$40000

.define moddata				$52000

.define spritemem			$50000

.define zp0					$04								; size = 4

.define yto					$08
.define yfrom				$0a
.define xto					$0c
.define xfrom				$0d

.define shift				$0e

.define flipflop			$0f

.define MULTINA				$d770
.define MULTINB				$d774
.define MULTOUT				$d778
.define DIVOUTWHOLE			$d768
.define DIVOUTFRACT			$d76c

; 2*(28+1)	; 2 screens, both having 28 chars and 1 gotox
.define CHARSPERROW			58*2

; the screen is 28 chars wide and high because
; we're rendering to a 240x240 area, but we chop off the left/right and top/bottom chars because they contain
; garbage, so 240-8-8=224 and 224/8 = 28

; ----------------------------------------------------------------------------------------------------

.segment "PALETTE"
		.incbin "../bin/bitmap_pal0.bin"

.segment "SPRITES"
		.incbin "../bin/sprites_chars0.bin"

.segment "SONG"
		.incbin "../bin/song.mod"

.segment "MAIN"

entry_main

		sei

		lda #$35
		sta $01

		lda #%10000000									; Clear bit 7 - HOTREG
		trb $d05d

		lda #$00										; unmap
		tax
		tay
		taz
		map
		eom

		lda #$41										; enable 40MHz
		sta $00

		lda #$47										; enable C65GS/VIC-IV IO registers
		sta $d02f
		lda #$53
		sta $d02f
		eom

		lda #%10000000									; force PAL mode, because I can't be bothered with fixing it for NTSC
		trb $d06f										; clear bit 7 for PAL ; trb $d06f 
		;tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

		lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		sta $d054

		lda #%10100000									; CLEAR bit7=40 column, bit5=Enable extended attributes and 8 bit colour entries
		trb $d031

		lda #80											; set to 80 for etherload
		sta $d05e

		ldx #$00
		lda #$00
:		sta emptychar,x
		inx
		cpx #$40
		bne :-

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$00
		sta $d012
		lda #<fastload_irq_handler
		sta $fffe
		lda #>fastload_irq_handler
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli

/*
		jsr fl_init
		jsr fl_waiting
		FLOPPY_IFFL_FAST_LOAD_INIT "MEGATRN.IFFLCRCH"
		FLOPPY_IFFL_FAST_LOAD
		FLOPPY_IFFL_FAST_LOAD
		jsr fl_exit
*/
		sei

		lda #$35
		sta $01

		lda #<.loword(moddata)
		sta adrPepMODL+0
		lda #>.loword(moddata)
		sta adrPepMODL+1
		lda #<.hiword(moddata)
		sta adrPepMODH+0
		lda #>.hiword(moddata)
		sta adrPepMODH+1

		jsr peppitoInit

		lda #$00
		sta $d020
		lda #$00
		sta $d021

		lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		sta $d054

		lda #%10100000									; Clear bit7=40 column, bit5=disable ...?
		trb $d031

		lda #CHARSPERROW								; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #$50										; set TEXTXPOS to same as SDBDRWDLSB
		sta $d04c

		lda #$55 ; #$55									; CHRXSCL - we want to scale 240 up to 320 and the default value of xscale is 120 (why not 128?), so (120*(240/320) = 90)
		sta $d05a

		
		lda #$40										; pal y border start
		sta verticalcenter+0
		lda #0
		sta verticalcenter+1
		lda #$44										; Y Position Where Character Display Starts ($D04E LSB, 0–3 of $D04F MSB)
		sta $d04e

		bit $d06f
		bpl pal

ntsc	lda #<55										; ntsc y border start
		sta verticalcenter+0
		lda #>55
		sta verticalcenter+1
		lda #$44										; Y Position Where Character Display Starts ($D04E LSB, 0–3 of $D04F MSB)
		sta $d04e

pal		lda verticalcenter+0
		sta $d048
		lda #%00001111
		trb $d049
		lda verticalcenter+1
		tsb $d049

		lda #28											; set number of rows
		sta $d07b
		lda #$08										; start of bottom border
		sta $d04a
		lda #$02
		sta $d04b
		lda #$50										; set TEXTXPOS
		sta $d04c
		lda #$4b										; set left border
		sta $d05c


		lda #<$0800										; set (offset!) pointer to colour ram
		sta $d064
		lda #>$0800
		sta $d065

		DMA_RUN_JOB clearcolorramjob
		DMA_RUN_JOB clearbitmap0job
		DMA_RUN_JOB clearbitmap1job
		DMA_RUN_JOB clearbitmap2job
		DMA_RUN_JOB clearbitmap3job

		lda #<.loword(screen0)							; set pointer to screen ram
		sta $d060
		lda #>.loword(screen0)
		sta $d061
		lda #<.hiword(screen0)
		sta $d062
		lda #>.hiword(screen0)
		sta $d063

		;  create linear table for slope/delta sprite plotting
		ldx #$00
setuplt:
		txa
		sta lineartable,x
		inx
		bne setuplt

		; higher bytes for all text screens
		lda #0
		sta zp0+2
		sta zp0+3

		; screen 1 'left'
		ldx #<(screen0+0)	; low byte destination
		ldy #>(screen0+0)	; high byte destination
		stx zp0+0
		sty zp0+1
		ldx #<(screenchars0 / 64 + 2*32 + 2)				; add two columns and 2 rows
		ldy #>(screenchars0 / 64 + 2*32 + 2)
		jsr setuptextscreen

		; screen 1 'right'
		ldx #<(screen0+2*28+2)	; low byte destination (+1 for gotox)
		ldy #>(screen0+2*28+2)	; high byte destination
		stx zp0+0
		sty zp0+1
		ldx #<(screenchars2 / 64 + 29*32 + 2)				; add two columns and 2 rows
		ldy #>(screenchars2 / 64 + 29*32 + 2)
		jsr setupinversetextscreen


		; screen 2 'left'
		ldx #<(screen1+0)	; low byte destination
		ldy #>(screen1+0)	; high byte destination
		stx zp0+0
		sty zp0+1
		ldx #<(screenchars1 / 64 + 2*32 + 2)
		ldy #>(screenchars1 / 64 + 2*32 + 2)
		jsr setuptextscreen

		; screen 2 'right'
		ldx #<(screen1+2*28+2)	; low byte destination (+1 for gotox)
		ldy #>(screen1+2*28+2)	; high byte destination
		stx zp0+0
		sty zp0+1
		ldx #<(screenchars3 / 64 + 29*32 + 2)			; add two columns and 2 rows
		ldy #>(screenchars3 / 64 + 29*32 + 2)
		jsr setupinversetextscreen


		; higher bytes for all colour screens
		lda #<.hiword(SAFE_COLOR_RAM)
		sta zp0+2
		lda #>.hiword(SAFE_COLOR_RAM)
		sta zp0+3
		; colour screen 1 'right', flip chars
		ldx #<(2*28+2)	; low byte destination (+1 for gotox)
		lda #>(2*28+2)	; high byte destination
		ora #$08
		tay
		stx zp0+0
		sty zp0+1
		ldx #%01000000	; horizontally flip char
		ldy #%00000000
		jsr setupcolourscreen


		; set colour ram to gotox and transparency
		lda #<.loword(SAFE_COLOR_RAM)
		ldx #>.loword(SAFE_COLOR_RAM)
		ldy #<.hiword(SAFE_COLOR_RAM)
		ldz #>.hiword(SAFE_COLOR_RAM)
		sta zp0+0
		stx zp0+1
		sty zp0+2
		stz zp0+3

		ldx #0
gotoxloop:
		ldz #28*2+0
		lda #%10010000					; set gotox and transparency
		sta [zp0],z
		ldz #28*2+1
		lda #%00000000					; set pixel row flag mask to 0
		sta [zp0],z
		ldz #57*2+0
		lda #%10010000					; set gotox and transparency
		sta [zp0],z
		ldz #57*2+1
		lda #%00000000					; set pixel row flag mask to 0
		sta [zp0],z
		clc
		lda zp0+0
		adc #CHARSPERROW
		sta zp0+0
		lda zp0+1
		adc #0
		sta zp0+1
		inx
		cpx #28
		bne gotoxloop

		lda #<.loword(screen0)
		ldx #>.loword(screen0)
		ldy #<.hiword(screen0)
		ldz #>.hiword(screen0)
		sta zp0+0
		stx zp0+1
		sty zp0+2
		stz zp0+3

		ldx #0
gotoxposscreen1loop:
		ldz #28*2+0
		lda #<0							; set first gotox position to 0
		sta [zp0],z
		ldz #28*2+1
		lda #>0							; set first gotox position to 0
		sta [zp0],z
		ldz #57*2+0
		lda #<400 ; (28*8+4)			; set second (and last) gotox position to the right side of the screen (+4 for the borders)
		sta [zp0],z
		ldz #57*2+1
		lda #>400 ; (28*8+4)			; set second (and last) gotox position to the right side of the screen (+4 for the borders)
		sta [zp0],z
		clc
		lda zp0+0
		adc #CHARSPERROW
		sta zp0+0
		lda zp0+1
		adc #0
		sta zp0+1
		inx
		cpx #28
		bne gotoxposscreen1loop

		lda #<.loword(screen1)
		ldx #>.loword(screen1)
		ldy #<.hiword(screen1)
		ldz #>.hiword(screen1)
		sta zp0+0
		stx zp0+1
		sty zp0+2
		stz zp0+3

		ldx #0
gotoxposscreen2loop:
		ldz #28*2+0
		lda #<0					; set first gotox position to 0
		sta [zp0],z
		ldz #28*2+1
		lda #>0					; set first gotox position to 0
		sta [zp0],z
		ldz #57*2+0
		lda #<400 ; (28*8+4)			; set second (and last) gotox position to the right side of the screen (+4 for the borders)
		sta [zp0],z
		ldz #57*2+1
		lda #>400 ; (28*8+4)			; set second (and last) gotox position to the right side of the screen (+4 for the borders)
		sta [zp0],z
		clc
		lda zp0+0
		adc #CHARSPERROW
		sta zp0+0
		lda zp0+1
		adc #0
		sta zp0+1
		inx
		cpx #28
		bne gotoxposscreen2loop




		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

		ldx #$00										; set bitmap palette
:		lda palette+$0000,x
		sta $d100,x
		lda palette+$0100,x
		sta $d200,x
		lda palette+$0200,x
		sta $d300,x
		inx
		bne :-

		lda #255
		sta flipflop

		lda $d070
		and #%11001111									; clear bits 4 and 5 (BTPALSEL) so bitmap uses palette 0
		sta $d070

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$20										; setup IRQ interrupt
		sta $d012
		lda #<irq1
		sta $fffe
		lda #>irq1
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli
		
loop
		lda $d020
		jmp loop

; ----------------------------------------------------------------------------------------------------

.define txtstartrow		0
.define txtstartcolumn	0
.define txtheight		28
.define txtwidth		28

setuptextscreen:

		lda zp0+0
		sta stsdstlo+1
		lda zp0+1
		sta stsdsthi+1

		lda #txtstartrow
		sta screenrow
		lda #txtstartcolumn
		sta screencolumn

stsloop:

		txa
		ldz #0
		sta [zp0],z

		tya
		ldz #1
		sta [zp0],z

		inx												; add 1 to character address

		clc												; and move to next row
		lda zp0+0
		adc #CHARSPERROW
		sta zp0+0
		lda zp0+1
		adc #0
		sta zp0+1

		inc screenrow									; increase row
		lda screenrow
		cmp #(txtstartrow + txtheight)					; have we reached the end row?
		bne stsloop

		lda #txtstartrow								; we have reached the end row
		sta screenrow									; reset row back to 0
		inc screencolumn								; and increase the column
		inc screencolumn
		lda screencolumn
		cmp #(2*(txtstartcolumn+txtwidth))				; have we reached the end column?
		beq endscreenplot

stsdstlo:	lda #<$c0de									; reset start position
			sta zp0+0
stsdsthi:	lda #>$c0de
			sta zp0+1

		clc												; and set new column position
		lda zp0+0
		adc screencolumn
		sta zp0+0

		clc												; add remainder to character address
		txa
		adc #<(32-txtheight)
		tax
		tya
		adc #$00
		tay

		jmp stsloop

endscreenplot:
		rts

; ----------------------------------------------------------------------------------------------------

setupinversetextscreen:

		lda zp0+0
		sta sitsdstlo+1
		lda zp0+1
		sta sitsdsthi+1

		lda #txtstartrow
		sta screenrow
		lda #txtstartcolumn
		sta screencolumn

sitsloop:

		txa
		ldz #0
		sta [zp0],z

		tya
		ldz #1
		sta [zp0],z

		inx												; add 1 to character address

		clc												; and move to next row
		lda zp0+0
		adc #CHARSPERROW
		sta zp0+0
		lda zp0+1
		adc #0
		sta zp0+1

		inc screenrow									; increase row
		lda screenrow
		cmp #(txtstartrow + txtheight)					; have we reached the end row?
		bne sitsloop

		lda #txtstartrow								; we have reached the end row
		sta screenrow									; reset row back to 0
		inc screencolumn								; and increase the column
		inc screencolumn
		lda screencolumn
		cmp #(2*(txtstartcolumn+txtwidth))				; have we reached the end column?
		beq endinversescreenplot

sitsdstlo:	lda #<$c0de									; reset start position
			sta zp0+0
sitsdsthi:	lda #>$c0de
			sta zp0+1

		clc												; and set new column position
		lda zp0+0
		adc screencolumn
		sta zp0+0

		sec												; subtract remainder to character address
		txa
		sbc #<(32+txtheight)
		tax
		tya
		sbc #$00
		tay

		jmp sitsloop

endinversescreenplot:
		rts

; ----------------------------------------------------------------------------------------------------

setupcolourscreen:

		lda zp0+0
		sta scsdstlo+1
		lda zp0+1
		sta scsdsthi+1

		lda #txtstartrow
		sta screenrow
		lda #txtstartcolumn
		sta screencolumn

scsloop:

		txa
		ldz #0
		sta [zp0],z

		tya
		ldz #1
		sta [zp0],z

		clc												; and move to next row
		lda zp0+0
		adc #CHARSPERROW
		sta zp0+0
		lda zp0+1
		adc #0
		sta zp0+1

		inc screenrow									; increase row
		lda screenrow
		cmp #(txtstartrow + txtheight)					; have we reached the end row?
		bne scsloop

		lda #txtstartrow								; we have reached the end row
		sta screenrow									; reset row back to 0
		inc screencolumn								; and increase the column
		inc screencolumn
		lda screencolumn
		cmp #(2*(txtstartcolumn+txtwidth))				; have we reached the end column?
		beq endcolourplot

scsdstlo:	lda #<$c0de									; reset start position
			sta zp0+0
scsdsthi:	lda #>$c0de
			sta zp0+1

		clc												; and set new column position
		lda zp0+0
		adc screencolumn
		sta zp0+0

		jmp scsloop

endcolourplot:
		rts

; ----------------------------------------------------------------------------------------------------		

.align 256

plotx			.byte 0
ploty			.byte 0
cursin			.byte 0
curcos			.byte 0
tempbyte		.byte 0
particlecolour	.byte 0
particlesize	.byte 2

calcplotpixel
		;lda #<.loword((offsetx/8) * (256*8) + (offsetx & 7) + offsety * 8)
		;sta zp0+0
		;lda #>.loword((offsetx/8) * (256*8) + (offsetx & 7) + offsety * 8)
		;sta zp0+1

		;lda #((offsetx & %00000111) + (offsety<<3)) ; multiply offsety by 8
		;sta zp0+0
		;lda #((offsetx & %11111000) + (offsety>>5)) ; divide offsety by 32
		;sta zp0+1

		clc
		tya
		asl
		asl
		asl
		sta zp0+0
		txa
		and #%00000111
		ora zp0+0
		sta zp0+0
		
		clc
		tya
		lsr
		lsr
		lsr
		lsr
		lsr
		sta zp0+1
		txa
		and #%11111000
		ora zp0+1
		sta zp0+1
		rts

.macro PLOTCOLOURPIXEL offsetx, offsety, coladd
		ldx #offsetx
		ldy #offsety
		lda #coladd
		sta plotcoladd

		jsr calcplotpixel
		jsr plotcolourpixel
.endmacro

plotcolourpixel

		ldz #$00
		lda plotcol
		clc
		adc plotcoladd
		and #$7f
		ora #$80
		sta [zp0],z
		rts

; ----------------------------------------------------------------------------------------------------

drawparticle

		ldy #0
plotyloop:

		ldx #0
plotxloop:
		phy
		phx

		clc
		txa
		adc cursin
		tax

		clc
		tya
		adc curcos
		tay

		jsr calcplotpixel
		;txa
		;sta plotcoladd
		;jsr plotcolourpixel

		plx
		ply

		clc
		txa
		sta tempbyte
		tya
		adc tempbyte
		adc particlecolour
		sta [zp0],z

		inx
		cpx particlesize
		bne plotxloop

		iny
		cpy particlesize
		bne plotyloop
		rts

; ----------------------------------------------------------------------------------------------------

irq1
		pha

		jsr peppitoPlay

		lda #$88
		sta $d020

		lda #0
		sta zp0+3

		lda flipflop
		eor #255
		sta flipflop

		bne doublebuffer1
doublebuffer0:
		lda #<.hiword(screenchars1)						; render to screen 1
		sta scr1_to+2
		lda #<.hiword(screenchars0)
		sta zp0+2
		sta scr1_from+2
		lda #<.loword(screen0)							; show screen 0
		sta $d060
		lda #>.loword(screen0)
		sta $d061
		lda #<.hiword(screen0)
		sta $d062
		lda #>.hiword(screen0)
		sta $d063
		bra doublebufferend
doublebuffer1:
		lda #<.hiword(screenchars0)						; render to screen 0
		sta scr1_to+2
		lda #<.hiword(screenchars1)
		sta zp0+2
		sta scr1_from+2
		lda #<.loword(screen1)							; show screen 1
		sta $d060
		lda #>.loword(screen1)
		sta $d061
		lda #<.hiword(screen1)
		sta $d062
		lda #>.hiword(screen1)
		sta $d063
doublebufferend:

		lda frame
		and #%00000001
		bne :+
		inc plotcol
:

		PLOTCOLOURPIXEL 126, 126, 0
		PLOTCOLOURPIXEL 127, 126, 8
		PLOTCOLOURPIXEL 128, 126, 16

		PLOTCOLOURPIXEL 126, 127, 0
		PLOTCOLOURPIXEL 127, 127, 8
		PLOTCOLOURPIXEL 128, 127, 16

		PLOTCOLOURPIXEL 126, 128, 0
		PLOTCOLOURPIXEL 127, 128, 8
		PLOTCOLOURPIXEL 128, 128, 16

		;jmp endirq

		jsr dochaosscreen1

		lda flipflop
		bne doublebuffer3
doublebuffer2:
		lda #<.hiword(screenchars3)						; render to screen 1
		sta scr2_to+2
		sta dll2_lindadrbnk
		lda #<.hiword(screenchars2)
		sta zp0+2
		sta scr2_from+2
		bra doublebufferend2
doublebuffer3:
		lda #<.hiword(screenchars2)						; render to screen 0
		sta scr2_to+2
		sta dll2_lindadrbnk
		lda #<.hiword(screenchars3)
		sta zp0+2
		sta scr2_from+2
doublebufferend2:

/*
		lda #06
		sta particlesize
		lda #$00
		sta particlecolour

		lda frame
		eor #255
		asl
		asl
		tax
		lda sine,x
		lsr
		lsr
		lsr
		adc #127-16
		sta cursin

		lda frame
		eor #255
		asl
		tax
		lda sine+64,x
		lsr
		lsr
		lsr
		adc #127-24
		sta curcos

		jsr drawparticle

		lda #06
		sta particlesize
		lda #$10
		sta particlecolour

		lda frame
		adc #4
		eor #255
		asl
		asl
		tax
		lda sine,x
		lsr
		lsr
		adc #127-32
		sta cursin

		lda frame
		adc #4
		eor #255
		asl
		tax
		lda sine+64,x
		lsr
		lsr
		adc #127-40
		sta curcos

		jsr drawparticle

		lda #06
		sta particlesize
		lda #$20
		sta particlecolour

		lda frame
		adc #4
		eor #255
		asl
		asl
		tax
		lda sine+32,x
		lsr
		lsr
		adc #127-32
		sta cursin

		lda frame
		adc #4
		eor #255
		asl
		tax
		lda sine+64,x
		lsr
		lsr
		adc #127-40
		sta curcos

		jsr drawparticle


		lda #$88
		sta $d020


		jsr dochaosscreen2
*/

		lda #0
		sta simplesprite_src_xpos
		lda #8
		sta simplesprite_src_ypos
		lda #16
		sta simplesprite_src_width
		sta simplesprite_src_height
		lda #64										; DRUNK YOU, BE CAREFUL BECAUSE THIS LAYER HAS X/Y FLIPPED CHARACTERS!
		sta simplesprite_dst_xpos+2
		sta simplesprite_dst_ypos+2
		ldx frame
		lda sine,x
		lsr
		lsr
		clc
		adc #1
		sta simplesprite_dst_width
		sta simplesprite_dst_height

		jsr simplesprite_draw
		;jsr dochaosscreen2

		lda #$88
		sta $d020

		;lda #$10
		;sta $d020

		clc
		lda $d012
		cmp maxd012
		bcc :+
		sta maxd012
		sta $c000
:		

		;DMA_RUN_JOB clearbitmap0checkeredjob
		;DMA_RUN_JOB clearbitmap1checkeredjob

endirq:

		inc frame

		pla
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------

dochaosscreen1:

		lda frame
		and #%00001111
		tax
		lda shifts,x
		sta shift

		lda shifts,x
		asl								; multiply shift by 8 to get yshift
		asl
		asl
		sta yto+0
		sta yfrom+0

		lda #$00
		sta yto+1
		sta yfrom+1

		;lda #$1c
		;sta $d020

		ldy #14							; loop x 15 times

scr1_yloop:		
			;clc
			tya
			adc shift
			sta xfrom
			lda shift					; get shift again but assign to xfrom/to
			sta xto

			ldx #14						; loop y 15 times
scr1_xloop:

				;clc
				;lda xto
				and #%00000111
				adc yto+0
				sta scr1_to+0

				lda xto
				and #%11111000
				adc yto+1
				sta scr1_to+1

				;clc
				lda xfrom
				and #%00000111
				adc yfrom+0
				sta scr1_from+0

				lda xfrom
				and #%11111000
				adc yfrom+1
				sta scr1_from+1

				ldz #16
scr1_zloop:
					;inc $d020

					sta $d707										; inline DMA copy
					;.byte $06										; Disable use of transparent value
					;.byte $07										; Enable use of transparent value					;.byte $82, $00									; Source skip rate (256ths of bytes)
					.byte $83, $08									; Source skip rate (whole bytes)
					;.byte $84, $00									; Destination skip rate (256ths of bytes)
					.byte $85, $08									; Destination skip rate (whole bytes)
					.byte $00										; end of job options
					.byte $00										; copy
					.word 16										; count
scr1_from			.word $0000										; src
					.byte <.hiword(screenchars0)					; src bank and flags
scr1_to				.word $0000										; dst
					.byte <.hiword(screenchars1)					; dst bank and flags
					.byte $00										; cmd hi
					.word $0000										; modulo, ignored

					dez
					beq scr1_exit_zloop

					inc scr1_from+0
					inc scr1_to+0

					lda scr1_from+0
					and #7
					bne scr1_from_not_crossed

					lda scr1_from+0
					bne scr1_from_not_crossed_hi
					inc scr1_from+1
scr1_from_not_crossed_hi:
					;clc
					adc #<(((256/8)*64)-8)			; add $0800-8 (32 char * 64 pixels per char)
					sta scr1_from+0
					lda scr1_from+1
					adc #>(((256/8)*64)-8)
					sta scr1_from+1
scr1_from_not_crossed:

					lda scr1_to+0
					and #7
					bne scr1_zloop

					lda scr1_to+0
					bne scr1_to_not_crossed_hi
					inc scr1_to+1
scr1_to_not_crossed_hi:
					;clc
					adc #<(((256/8)*64)-8)
					sta scr1_to+0
					lda scr1_to+1
					adc #>(((256/8)*64)-8)
					sta scr1_to+1
scr1_to_not_crossed:

					bra scr1_zloop

scr1_exit_zloop:

				;clc
				lda xfrom
				adc #15							; was 16, but I was subtracting one again afterwards
				sta xfrom

				;clc
				lda yfrom+0
				adc #$08
				sta yfrom+0
				lda yfrom+1
				adc #$00
				sta yfrom+1
				
				;clc
				lda xto
				adc #16
				sta xto

				dex
				bmi scr1_exit_xloop
				jmp scr1_xloop

scr1_exit_xloop:

			dey
			bmi scr1_exit_yloop

			;clc
			lda yto+0
			adc #128				; add 2*64 to get to next row, 2 characters below this one
			sta yto+0
			lda yto+1
			adc #0
			sta yto+1

			jmp scr1_yloop

scr1_exit_yloop:

			rts

; ----------------------------------------------------------------------------------------------------


dochaosscreen2:

		; yfrom = shift
		; yto   = shift

		;  y = 14 to 0
		;  {
		;      xto   = 0 + shift
		;      xfrom = y + shift
		;
		;      x = 14 to 0
		;      {
		;          to   = address(  xto, yto  )
		;          from = address(xfrom, yfrom)
		;
		;          z = 16 to 0
		;          {
		;		       plot square (yto doesn't have to change because because it's always the same at the start of the vertical DMA plot)
		;          }
		;
		;          xto   += 16
		;          xfrom += 15 ; move to the next square, xfrom -= 1
		;          yfrom += 8  ;                          yfrom += 1
		;      }
		;
		;      yto += 128 (2 chars down = 2*64)
		; }

		;   (xfrom - xto) (yfrom - yto)

		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  14     |  13  01 |  12  02 |  11  03 |  10  04 |  09  05 |  08  06 |  07  07 |  06  08 |  05  09 |  04  10 |  03  11 |  02  12 |  01  13 |      14 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  13 -01 |  12     |  11  01 |  10  02 |  09  03 |  08  04 |  07  05 |  06  06 |  05  07 |  04  08 |  03  09 |  02  10 |  01  11 |      12 | -01  13 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  12 -02 |  11 -01 |  10     |  09  01 |  08  02 |  07  03 |  06  04 |  05  05 |  04  06 |  03  07 |  02  08 |  01  09 |      10 | -01  11 | -02  12 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  11 -03 |  10 -02 |  09 -01 |  08     |  07  01 |  06  02 |  05  03 |  04  04 |  03  05 |  02  06 |  01  07 |      08 | -01  09 | -02  10 | -03  11 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  10 -04 |  09 -03 |  08 -02 |  07 -01 |  06     |  05  01 |  04  02 |  03  03 |  02  04 |  01  05 |      06 | -01  07 | -02  08 | -03  09 | -04  10 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  09 -05 |  08 -04 |  07 -03 |  06 -02 |  05 -01 |  04     |  03  01 |  02  02 |  01  03 |      04 | -01  05 | -02  06 | -03  07 | -04  08 | -05  09 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  08 -06 |  07 -05 |  06 -04 |  05 -03 |  04 -02 |  03 -01 |  02     |  01  01 |      02 | -01  03 | -02  04 | -03  05 | -04  06 | -05  07 | -06  08 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  07 -07 |  06 -06 |  05 -05 |  04 -04 |  03 -03 |  02 -02 |  01 -01 |         | -01  01 | -02  02 | -03  03 | -04  04 | -05  05 | -06  06 | -07  07 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  06 -08 |  05 -07 |  04 -06 |  03 -05 |  02 -04 |  01 -03 |     -02 | -01 -01 | -02     | -03  01 | -04  02 | -05  03 | -06  04 | -07  05 | -08  06 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  05 -09 |  04 -08 |  03 -07 |  02 -06 |  01 -05 |     -04 | -01 -03 | -02 -02 | -03 -01 | -04     | -05  01 | -06  02 | -07  03 | -08  04 | -09  05 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  04 -10 |  03 -09 |  02 -08 |  01 -07 |     -06 | -01 -05 | -02 -04 | -03 -03 | -04 -02 | -05 -01 | -06     | -07  01 | -08  02 | -09  03 | -10  04 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  03 -11 |  02 -10 |  01 -09 |     -08 | -01 -07 | -02 -06 | -03 -05 | -04 -04 | -05 -03 | -06 -02 | -07 -01 | -08     | -09  01 | -10  02 | -11  03 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  02 -12 |  01 -11 |     -10 | -01 -09 | -02 -08 | -03 -07 | -04 -06 | -05 -05 | -06 -04 | -07 -03 | -08 -02 | -09 -01 | -10     | -11  01 | -12  02 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |  01 -13 |     -12 | -01 -11 | -02 -10 | -03 -09 | -04 -08 | -05 -07 | -06 -06 | -07 -05 | -08 -04 | -09 -03 | -10 -02 | -11 -01 | -12     | -13  01 |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
		; |     -14 | -01 -13 | -02 -12 | -03 -11 | -04 -10 | -05 -09 | -06 -08 | -07 -07 | -08 -06 | -09 -05 | -10 -04 | -11 -03 | -12 -02 | -13 -01 | -14     |
		; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+


		lda frame
		and #%00001111
		tax
		lda overlayshifts,x
		sta shift

		lda #$00
		sta yto+1
		sta yfrom+1

		lda overlayshifts,x
		asl								; multiply shift by 8 to get yshift
		asl
		asl
		sta yto+0
		sta yfrom+0

		ldy #14							; loop x 15 times

scr2_yloop:		
			;clc
			tya
			adc shift
			sta xfrom
			lda shift					; get shift again but assign to xfrom/to
			sta xto

			ldx #14						; loop y 15 times
scr2_xloop:

				;clc
				;lda xto
				and #%00000111
				adc yto+0
				sta scr2_to+0

				lda xto
				and #%11111000
				adc yto+1
				sta scr2_to+1

				;clc
				lda xfrom
				and #%00000111
				adc yfrom+0
				sta scr2_from+0

				lda xfrom
				and #%11111000
				adc yfrom+1
				sta scr2_from+1

				ldz #16
scr2_zloop:
					;inc $d020

					sta $d707										; inline DMA copy
					;.byte $06										; Disable use of transparent value
					;.byte $07										; Enable use of transparent value					;.byte $82, $00									; Source skip rate (256ths of bytes)
					.byte $83, $08									; Source skip rate (whole bytes)
					;.byte $84, $00									; Destination skip rate (256ths of bytes)
					.byte $85, $08									; Destination skip rate (whole bytes)
					.byte $00										; end of job options
					.byte $00										; copy
					.word 16										; count
scr2_from			.word $0000										; src
					.byte <.hiword(screenchars2)					; src bank and flags
scr2_to				.word $0000										; dst
					.byte <.hiword(screenchars3)					; dst bank and flags
					.byte $00										; cmd hi
					.word $0000										; modulo, ignored

					dez
					beq scr2_exit_zloop

					inc scr2_from+0
					inc scr2_to+0

					lda scr2_from+0
					and #7
					bne scr2_from_not_crossed

					lda scr2_from+0
					bne scr2_from_not_crossed_hi
					inc scr2_from+1
scr2_from_not_crossed_hi:
					;clc
					adc #<(((256/8)*64)-8)			; add $0800-8 (32 char * 64 pixels per char)
					sta scr2_from+0
					lda scr2_from+1
					adc #>(((256/8)*64)-8)
					sta scr2_from+1
scr2_from_not_crossed:

					lda scr2_to+0
					and #7
					bne scr2_zloop

					lda scr2_to+0
					bne scr2_to_not_crossed_hi
					inc scr2_to+1
scr2_to_not_crossed_hi:
					;clc
					adc #<(((256/8)*64)-8)
					sta scr2_to+0
					lda scr2_to+1
					adc #>(((256/8)*64)-8)
					sta scr2_to+1
scr2_to_not_crossed:

					bra scr2_zloop

scr2_exit_zloop:

				;clc
				lda xfrom
				adc #15							; was 16, but I was subtracting one again afterwards
				sta xfrom

				;clc
				lda yfrom+0
				adc #$08
				sta yfrom+0
				lda yfrom+1
				adc #$00
				sta yfrom+1
				
				;clc
				lda xto
				adc #16
				sta xto

				dex
				bmi scr2_exit_xloop
				jmp scr2_xloop

scr2_exit_xloop:

			dey
			bmi scr2_exit_yloop

			;clc
			lda yto+0
			adc #128				; add 2*64 to get to next row, 2 characters below this one
			sta yto+0
			lda yto+1
			adc #0
			sta yto+1

			jmp scr2_yloop

scr2_exit_yloop:

			rts

; ----------------------------------------------------------------------------------------------------

simplesprite_draw:

				lda simplesprite_src_xpos			; set left point for src address
				sta gsp1_linsadr+0
				lda simplesprite_src_ypos			; set source xpos
				sta dll2_linsadr+0

				ldq q0
				stq MULTINA
				stq MULTINB

				lda simplesprite_src_width			; 16 / 32 = 0.5
				sta MULTINA+2
				lda simplesprite_dst_width
				sta MULTINB+0

				lda $d020
				sta $d020
				lda $d020
				sta $d020
				lda $d020
				sta $d020

				lda DIVOUTFRACT+1
				sta gsp1_linskiplo+1
				lda DIVOUTFRACT+2
				sta gsp1_linskiphi+1

				lda simplesprite_src_height			; DMA skip = srcheight/dstheight
				sta MULTINA+2
				lda simplesprite_dst_height
				sta MULTINB+0
				sta dll2_linsize

				lda $d020
				sta $d020
				lda $d020
				sta $d020
				lda $d020
				sta $d020

				lda DIVOUTFRACT+1
				sta dll2_linskiplo+1
				lda DIVOUTFRACT+2
				sta dll2_linskiphi+1

				lda simplesprite_dst_width				; sample x points where x is destination width
				sta gsp1_linsize+0

				sta $d707								; inline DMA
				.byte $06								; Disable use of transparent value
gsp1_linskiplo:	.byte $82, 0							; Source skip rate (256ths of bytes)
gsp1_linskiphi:	.byte $83, 1							; Source skip rate (whole bytes)
				.byte $84, 0							; Destination skip rate (256ths of bytes)
				.byte $85, 1							; Destination skip rate (whole bytes) skip 8 bytes to get to next vertical pixel
				.byte $8f, %00000000					; bit 7 = enable DESTINATION line drawing, Bit 6 = select X or Y direction, Bit 5 = slope is negative.
				.byte $00								; end of job options

				.byte %00000000							; copy (bit 5 = invert source, bit 6 = invert destination)
gsp1_linsize:	.word 16								; count - needs initialising
gsp1_linsadr:	.word lineartable						; src
				.byte $00								; src bank and flags
gsp1_lindadr:	.word slope_top							; dst
				.byte $00								; dst bank and flags
				.byte $00								; cmd hi
				.word $0000								; modulo, ignored

				; for VERTICAL drawing, sprites need to have their columns laid out HORIZONTALLY/LINEAR in memory
				; so use d1:1 = PixelTopBottomLeftRight for the sprite gfx in makefile
				; make the sprite sheet 256 high, so increasing source address high byte will move to the next line

				ldq q0
				stq MULTINA
				stq MULTINB

				lda simplesprite_dst_xpos+2				; calculate destination column
				sta MULTINA+0
				lda simplesprite_dst_xpos+3
				sta MULTINA+1

				lsr MULTINA+1
				ror MULTINA+0
				lsr MULTINA+1
				ror MULTINA+0
				lsr MULTINA+1
				ror MULTINA+0

				lda #<((256/8)*64)
				sta MULTINB+0
				lda #>((256/8)*64)
				sta MULTINB+1

				lda MULTOUT+0
				sta dll2_lindadr+0
				lda MULTOUT+1
				sta dll2_lindadr+1

				; calculate destination pixel in char
				lda simplesprite_dst_xpos+2	
				and #$07
				ora dll2_lindadr+0
				sta dll2_lindadr+0

				; calculate and add destination row
				lda #<8
				sta MULTINA+0
				;lda #>8
				;sta MULTINA+1

				ldq simplesprite_dst_ypos
				stq MULTINB+0

				clc
				lda dll2_lindadr+0
				adc MULTOUT+2
				sta dll2_lindadr+0
				lda dll2_lindadr+1
				adc MULTOUT+3
				sta dll2_lindadr+1

				ldx #0
draw_simplesprite_loop:
				lda slope_top,x
				sta dll2_linsadr+1

				sta $d707								; inline DMA
				.byte $07								; Enable use of transparent value
dll2_linskiplo:	.byte $82, $80							; Source skip rate (256ths of bytes)
dll2_linskiphi:	.byte $83, $00							; Source skip rate (whole bytes) ; ignored when drawing lines (but destination is NOT drawing lines)
				.byte $84, 0							; Destination skip rate (256ths of bytes)
				.byte $85, 8							; Destination skip rate (whole bytes) skip 8 bytes to get to next vertical pixel
				.byte $8f, %00000000					; bit 7 = enable DESTINATION line drawing, Bit 6 = select X or Y direction, Bit 5 = slope is negative.
				.byte $9f, %00000000					; bit 7 = enable SOURCE line drawing, Bit 6 = select X or Y direction, Bit 5 = slope is negative.
				.byte $00								; end of job options

dll2_lincmd:	.byte %00000000							; copy (bit 5 = invert source, bit 6 = invert destination)
dll2_linsize:	.word 16								; count - needs initialising
dll2_linsadr:	.word $0000								; src
				.byte ((spritemem >> 16) & $0f)			; src bank and flags
dll2_lindadr:	.word $0000								; dst
dll2_lindadrbnk:.byte $03								; dst bank and flags
				.byte $00								; cmd hi
				.word $0000								; modulo, ignored

				inc dll2_lindadr+0
				lda dll2_lindadr+0

				and #$07
				bne dll2_not_crossed      ; if not zero, didn’t cross a boundary
;dll2_crossed:
				lda dll2_lindadr+0
				bne dll2_not_crossed_hi
				inc dll2_lindadr+1
dll2_not_crossed_hi:
				clc
				adc #<(((256/8)*64)-8)
				sta dll2_lindadr+0
				lda dll2_lindadr+1
				adc #>(((256/8)*64)-8)
				sta dll2_lindadr+1

dll2_not_crossed:

				inx
				cpx simplesprite_dst_width
				bne draw_simplesprite_loop

draw_simplesprite_done:
				rts

; ----------------------------------------------------------------------------------------------------

frame				.byte 0
screenrow			.byte 0
screencolumn		.byte 0
verticalcenter		.byte 0

plotcol				.byte 0
plotcoladd			.byte 0

maxd012				.byte 0

shifts
					.byte 0, 8, 4, 12, 2, 10, 6, 14
					.byte 1, 9, 5, 13, 3, 11, 7, 15

overlayshifts
					.byte 0, 8, 4, 12, 2, 10, 6, 14
					.byte 1, 9, 5, 13, 3, 11, 7, 15

simplesprite_dst_scale:		.word $0100

simplesprite_src_xpos:		.byte 0
simplesprite_src_ypos:		.byte 0

simplesprite_src_width:		.byte 16
simplesprite_src_height:	.byte 16

simplesprite_dst_xpos:		.dword 32
simplesprite_dst_ypos:		.dword 32

simplesprite_dst_width:		.byte 0
simplesprite_dst_height:	.byte 0

q0							.byte 0,0,0,0

; ----------------------------------------------------------------------------------------------------

clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, ((SAFE_COLOR_RAM) >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word CHARSPERROW*28							; Count LSB + Count MSB

				.word %00000000									; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((SAFE_COLOR_RAM) & $ffff)				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word CHARSPERROW*28							; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((SAFE_COLOR_RAM)+1) & $ffff				; Destination Address LSB + Destination Address MSB
				.byte ((((SAFE_COLOR_RAM)+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

clearbitmap0job:
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, (screenchars0 >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000011									; fill and don't chain

				.word 0 ; 240*256									; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word $0000										; Destination Address LSB + Destination Address MSB
				.byte ((screenchars0 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

clearbitmap1job:
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, (screenchars1 >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $82, $00									; Source skip rate (256ths of bytes)
				.byte $83, $01									; Source skip rate (whole bytes)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000011									; fill and don't chain

				.word 0 ; 240*256								; Count LSB + Count MSB

				.word $0020										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word $0000										; Destination Address LSB + Destination Address MSB
				.byte ((screenchars1 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

clearbitmap2job:
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, (screenchars2 >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $82, $00									; Source skip rate (256ths of bytes)
				.byte $83, $01									; Source skip rate (whole bytes)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000011									; fill and don't chain

				.word 0 ; 240*256								; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word $0000										; Destination Address LSB + Destination Address MSB
				.byte ((screenchars2 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

clearbitmap3job:
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, (screenchars3 >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $82, $00									; Source skip rate (256ths of bytes)
				.byte $83, $01									; Source skip rate (whole bytes)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000011									; fill and don't chain

				.word 0 ; 240*256								; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word $0000										; Destination Address LSB + Destination Address MSB
				.byte ((screenchars2 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

clearbitmap0checkeredjob:
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, (screenchars0 >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, 16									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000011									; fill and don't chain

				.word 16*256										; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word $0000										; Destination Address LSB + Destination Address MSB
				.byte ((screenchars0 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

clearbitmap1checkeredjob:
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, (screenchars0 >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, 16									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000011									; fill and don't chain

				.word 16*256										; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word $0000										; Destination Address LSB + Destination Address MSB
				.byte ((screenchars1 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

		.align 256

sine
		.byte 255, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
		.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
		.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
		.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
		.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
		.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
		.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
		.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254

		.byte 254, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
		.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
		.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
		.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
		.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
		.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
		.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
		.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254
