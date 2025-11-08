.define emptychar			$ff80							; size = 64

.define screen0				$b000
.define screen1				$e000

.define palette				$c000

.define screenchars0		$10000
.define screenchars1		$20000
.define screenchars2		$30000
.define screenchars3		$40000

.define moddata				$50000

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

; 2*(28+1)	; 2 screens, both having 28 chars and 1 gotox
.define CHARSPERROW			58*2

; the screen is 28 chars wide and high because
; we're rendering to a 240x240 area, but we chop off the left/right and top/bottom chars because they contain
; garbage, so 240-8-8=224 and 224/8 = 28

; ----------------------------------------------------------------------------------------------------

.segment "PALETTE"
		.incbin "../bin/bitmap_pal0.bin"

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

		lda #$40										; start of top border
		sta $d048
		lda #$44										; Y Position Where Character Display Starts ($D04E LSB, 0–3 of $D04F MSB)
		sta $d04e
		lda #28											; set number of rows
		sta $d07b
		lda #$08										; start of bottom border
		sta $d04a
		lda #$02
		sta $d04b
		lda #$4e										; set TEXTXPOS
		sta $d04c
		lda #$4a										; set left border
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

		; pal y border start
		lda #<104
		sta verticalcenter+0
		lda #>104
		sta verticalcenter+1

		bit $d06f
		bpl pal

ntsc	lda #<55
		sta verticalcenter+0
		lda #>55
		sta verticalcenter+1

pal		lda verticalcenter+0
		sta $d048
		lda #%00001111
		trb $d049
		lda verticalcenter+1
		tsb $d049

		lda #<.loword(screen0)							; set pointer to screen ram
		sta $d060
		lda #>.loword(screen0)
		sta $d061
		lda #<.hiword(screen0)
		sta $d062
		lda #>.hiword(screen0)
		sta $d063



		; screen 1 'left'
		ldx #<(screenchars0 / 64 + 2*32 + 2)				; add two columns and 2 rows
		ldy #>(screenchars0 / 64 + 2*32 + 2)
		lda #>screen0	; high byte destination
		ldz #0			; low byte destination
		jsr setuptextscreen

		; screen 1 'right'
		ldx #<(screenchars2 / 64 + 2*32 + 2)				; add two columns and 2 rows
		ldy #>(screenchars2 / 64 + 2*32 + 2)
		lda #>screen0	; high byte destination
		ldz #2*28+2		; low byte destination (+1 for gotox)
		jsr setuptextscreen


		; screen 2 'left'
		ldx #<(screenchars1 / 64 + 2*32 + 2)
		ldy #>(screenchars1 / 64 + 2*32 + 2)
		lda #>screen1	; high byte destination
		ldz #0			; low byte destination
		jsr setuptextscreen

		; screen 2 'right'
		ldx #<(screenchars3 / 64 + 2*32 + 2)				; add two columns and 2 rows
		ldy #>(screenchars3 / 64 + 2*32 + 2)
		lda #>screen1	; high byte destination
		ldz #2*28+2		; low byte destination (+1 for gotox)
		jsr setuptextscreen




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
		lda #%10010000			; set gotox and transparency
		sta [zp0],z
		ldz #28*2+1
		lda #%00000000			; set pixel row flag mask to 0
		sta [zp0],z
		ldz #57*2+0
		lda #%10010000			; set gotox and transparency
		sta [zp0],z
		ldz #57*2+1
		lda #%00000000			; set pixel row flag mask to 0
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

		stz put10+1
		stz put12+1
		inz
		stz put11+1

		sta put10+2
		sta put11+2
		sta put13+1

		lda #txtstartrow
		sta screenrow
		lda #txtstartcolumn
		sta screencolumn

put10	stx screen0+0
put11	sty screen0+1

		clc												; add 1 to character address
		txa
		adc #$01
		tax
		tya
		adc #$00
		tay

		clc												; and move to next row
		lda put10+1
		adc #CHARSPERROW
		sta put10+1
		sta put11+1
		lda put10+2
		adc #0
		sta put10+2
		sta put11+2
		inc put11+1

		inc screenrow									;  have we reached the end row?
		lda screenrow
		cmp #(txtstartrow + txtheight)
		bne put10

		lda #txtstartrow								; we have reached the end row
		sta screenrow									; reset row back to 0
		inc screencolumn								; and increase the column
		inc screencolumn
		lda screencolumn
		cmp #(txtstartcolumn+txtwidth)*2				; have we reached the end column?
		beq endscreenplot

		; calculate new start position here

put12	ldz #0
		stz put10+1
		inz
		stz put11+1
put13	lda #>screen0									; reset the destination high byte
		sta put10+2
		sta put11+2

		clc
		lda put10+1
		adc screencolumn
		sta put10+1
		lda put11+1
		adc screencolumn
		sta put11+1

		clc												; add remainder to character address
		txa
		adc #<(32-txtheight)
		tax
		tya
		adc #$00
		tay

		jmp put10

endscreenplot
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

.macro PLOTCOLOURPIXEL offsetx, offsety, coladd
		ldx #offsetx
		ldy #offsety
		lda #coladd
		sta plotcoladd

		jsr calcplotpixel
		jsr plotcolourpixel
.endmacro

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

plotcolourpixel

		ldz #$00
		lda plotcol
		clc
		adc plotcoladd
		and #$7f
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

		lda #$08
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
		lda #<.hiword(screenchars2)
		sta zp0+2
		sta scr2_from+2
		bra doublebufferend2
doublebuffer3:
		lda #<.hiword(screenchars2)						; render to screen 0
		sta scr2_to+2
		lda #<.hiword(screenchars3)
		sta zp0+2
		sta scr2_from+2
doublebufferend2:

		ldx frame
		lda sine,x
		lsr
		lsr
		lsr
		lsr
		lsr
		lsr
		adc #$02
		sta particlesize

		lda frame
		eor #255
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
		adc #127-16
		sta curcos

		lda #$80
		sta particlecolour
		jsr drawparticle

		ldx frame
		lda sine+64,x
		lsr
		lsr
		lsr
		lsr
		lsr
		lsr
		adc #$04
		sta particlesize

		lda frame
		adc #4
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
		adc #4
		eor #255
		asl
		tax
		lda sine+64,x
		lsr
		lsr
		adc #127-32
		sta curcos

		lda #$90
		sta particlecolour
		jsr drawparticle

		lda frame
		asl
		asl
		lda sine+64,x
		lsr
		lsr
		lsr
		lsr
		lsr
		lsr
		lsr
		adc #$06
		sta particlesize

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
		adc #127-64
		sta curcos

		lda #$a0
		sta particlecolour
		jsr drawparticle


		lda #$e0
		sta $d020

		jsr dochaosscreen2

		lda #$10
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

		;jsr peppitoPlay

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

		lda frame
		and #%00001111
		tax
		lda overlayshifts,x
		sta shift

		lda overlayshifts,x
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



		; yfrom = shift
		; yto   = shift

		;  y = 14 to 0
		;  {
		;      xfrom = shift + y
		;      xto   = shift
		;
		;      x = 14 to 0
		;      {
		;          from = address(xfrom, yfrom)
		;          to   = address(  xto, yto  )
		;
		;          z = 16 to 0
		;          {
		;		       plot square (yto doesn't have to change because because it's always the same at the start of the vertical DMA plot)
		;          }
		;
		;          xfrom += 15 ; move to the next square
		;          xto   += 16
		;          yfrom += 8 (why is yfrom incremented, though. My memory is failing again)
		;      }
		;
		;      yto += 128 (2 chars down = 2*64)
		; }


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
					;.byte 0, 8, 4, 12, 2, 10, 6, 14
					;.byte 1, 9, 5, 13, 3, 11, 7, 15

					.byte 14, 6, 10, 2, 12, 4, 8, 0
					.byte 15, 7, 11, 3, 13, 5, 9, 1

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

				.word $0007										; this is normally the source addres, but contains the fill value now
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
