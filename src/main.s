.define emptychar			$ff80							; size = 64

.define screen0				$b000
.define screen1				$e000

.define palette				$c000

.define screenchars0		$10000
.define screenchars1		$20000

.define screenchars0div64	screenchars0/64
.define screenchars1div64	screenchars1/64

.define moddata				$30000

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

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #$50										; set TEXTXPOS to same as SDBDRWDLSB
		sta $d04c

		DMA_RUN_JOB clearcolorramjob
		DMA_RUN_JOB clearbitmap0job
		DMA_RUN_JOB clearbitmap1job

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




		ldx #<(screenchars0 / 64 + 2*32 + 2)				; add two columns and 2 rows
		ldy #>(screenchars0 / 64 + 2*32 + 2)
		lda #>screen0
		jsr setuptextscreen

		ldx #<(screenchars1 / 64 + 2*32 + 2)
		ldy #>(screenchars1 / 64 + 2*32 + 2)
		lda #>screen1
		jsr setuptextscreen



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

		lda #$10										; setup IRQ interrupt
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

		sta put10+2
		sta put11+2
		sta put12+1
		lda #0
		sta put10+1
		lda #1
		sta put11+1

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
		adc #80
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

put12	lda #>screen0									; reset the destination high byte
		sta put10+2
		sta put11+2
		clc
		lda screencolumn
		sta put10+1
		adc #$01
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

.macro plotpixel offset, coladd
		lda #<.loword(offset)
		sta zp0+0
		lda #>.loword(offset)
		sta zp0+1

		ldz #$00
		lda plotcol
		clc
		adc coladd
		sta [zp0],z
.endmacro

irq1
		pha

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
		sta to+2
		lda #<.hiword(screenchars0)
		sta zp0+2
		sta from+2
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
		sta to+2
		lda #<.hiword(screenchars1)
		sta zp0+2
		sta from+2
		lda #<.loword(screen1)							; show screen 1
		sta $d060
		lda #>.loword(screen1)
		sta $d061
		lda #<.hiword(screen1)
		sta $d062
		lda #>.hiword(screen1)
		sta $d063
doublebufferend:

		jsr peppitoPlay

		plotpixel ((16-1)*(256*8)+16*64-2-8),  0
		plotpixel ((16-1)*(256*8)+16*64-2-0),  0
		plotpixel ((16-1)*(256*8)+16*64-2+8),  0
		plotpixel ((16-1)*(256*8)+16*64-1-8),  0
		plotpixel ((16-1)*(256*8)+16*64-1-0),  1
		plotpixel ((16-1)*(256*8)+16*64-1+8),  1
		plotpixel ((16+0)*(256*8)+16*64+0-16), 1
		plotpixel ((16+0)*(256*8)+16*64+0-8),  1
		plotpixel ((16+0)*(256*8)+16*64+0-0),  1

		lda frame
		and #%00000001
		bne :+
		inc plotcol
:

		;lda #$08
		;sta $d020

		lda frame
		and #%00001111
		tax
		lda shifts,x
		sta shift

		asl
		asl
		asl
		sta yto+0
		sta yfrom+0

		lda #$00
		sta yto+1
		sta yfrom+1

		ldy #14

yloop		lda shift
			sta xto
			sta xfrom
			tya
			;clc
			adc xfrom
			sta xfrom

			ldx #14
xloop

				;clc
				lda xto
				and #$07
				adc yto+0
				sta to+0

				lda xto
				and #(256-8)
				adc yto+1
				sta to+1

				;clc
				lda xfrom
				and #$07
				adc yfrom+0
				sta from+0

				lda xfrom
				and #(256-8)
				adc yfrom+1
				sta from+1

				ldz #16
zloop:
					sta $d707										; inline DMA copy
					;.byte $06										; Disable use of transparent value
					;.byte $07										; Enable use of transparent value					;.byte $82, $00									; Source skip rate (256ths of bytes)
					.byte $83, $08									; Source skip rate (whole bytes)
					;.byte $84, $00									; Destination skip rate (256ths of bytes)
					.byte $85, $08									; Destination skip rate (whole bytes)
					.byte $00										; end of job options
					.byte $00										; copy
					.word 16										; count
from				.word $0000										; src
					.byte <.hiword(screenchars0)					; src bank and flags
to					.word $0000										; dst
					.byte <.hiword(screenchars1)					; dst bank and flags
					.byte $00										; cmd hi
					.word $0000										; modulo, ignored

					dez
					beq exit_zloop

					inc from+0
					inc to+0

					lda from+0
					and #7
					bne from_not_crossed
;from_crossed
					lda from+0
					bne from_not_crossed_hi
					inc from+1
from_not_crossed_hi	;clc
					adc #<(((256/8)*64)-8)			; add $07f8
					sta from+0
					lda from+1
					adc #>(((256/8)*64)-8)
					sta from+1
from_not_crossed

					lda to+0
					and #7
					bne to_not_crossed
;to_crossed
					lda to+0
					bne to_not_crossed_hi
					inc to+1
to_not_crossed_hi	;clc
					adc #<(((256/8)*64)-8)
					sta to+0
					lda to+1
					adc #>(((256/8)*64)-8)
					sta to+1
to_not_crossed
					bra zloop

exit_zloop:

				;clc
				lda xto
				adc #16
				sta xto

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
				
				dex
				bmi exit_xloop
				jmp xloop

exit_xloop:

			dey
			bmi exit_yloop

			;clc
			lda yto+0
			adc #128
			sta yto+0
			lda yto+1
			adc #0
			sta yto+1

			jmp yloop

exit_yloop:			

		lda #$94
		sta $d020

		;DMA_RUN_JOB clearbitmap0checkeredjob
		;DMA_RUN_JOB clearbitmap1checkeredjob

		lda #$84
		sta $d020

		inc frame

		pla
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------

frame				.byte 0
screenrow			.byte 0
screencolumn		.byte 0
verticalcenter		.byte 0

plotcol				.byte 0

shifts				.byte 0, 8, 4, 12, 2, 10, 6, 14
					.byte 1, 9, 5, 13, 3, 11, 7, 15

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

				.word 40*36										; Count LSB + Count MSB

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

				.word 40*36										; Count LSB + Count MSB

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
				;.byte $84, $00									; Destination skip rate (256ths of bytes)
				;.byte $85, $01									; Destination skip rate (whole bytes)

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
				;.byte $84, $00									; Destination skip rate (256ths of bytes)
				;.byte $85, $01									; Destination skip rate (whole bytes)

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
