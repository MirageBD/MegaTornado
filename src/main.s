.define emptychar		$ff80							; size = 64

.define screen1			$b000

.define palette			$c000

.define screenchars0	$10000
.define screenchars1	$20000

.define moddata			$30000

.define zp0				$04								; size = 4

.define yto				$08
.define yfrom			$0a
.define xto				$0c
.define xfrom			$0d

.define shift			$0e

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
		DMA_RUN_JOB clearbitmapjob
		DMA_RUN_JOB clearscreenjob

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

		lda #<.loword(screen1)							; set pointer to screen ram
		sta $d060
		lda #>.loword(screen1)
		sta $d061
		lda #<.hiword(screen1)
		sta $d062
		lda #>.hiword(screen1)
		sta $d063

		lda #$00
		sta screenrow
		sta screencolumn

		ldx #<(screenchars0 / 64 + 2*32)
		ldy #>(screenchars0 / 64 + 2*32)

put10	stx screen1+0
put11	sty screen1+1

		clc
		txa
		adc #$01
		tax
		tya
		adc #$00
		tay

		clc
		lda put10+1
		adc #80
		sta put10+1
		lda put10+2
		adc #0
		sta put10+2

		clc
		lda put11+1
		adc #80
		sta put11+1
		lda put11+2
		adc #0
		sta put11+2

		inc screenrow
		lda screenrow
		cmp #32
		bne put10

		lda #0
		sta screenrow
		inc screencolumn
		inc screencolumn
		lda screencolumn
		cmp #28*2
		beq endscreenplot1

		lda #>screen1
		sta put10+2
		sta put11+2
		clc
		lda screencolumn
		sta put10+1
		adc #$01
		sta put11+1

		jmp put10

endscreenplot1

		lda #$55										; CHRXSCL
		sta $d05a

		lda #$22										; Y Position Where Character Display Starts ($D04E LSB, 0–3 of $D04F MSB)
		sta $d04e
		lda #35											; set number of rows
		sta $d07b
		lda #$40										; reposition start of top border to what's juuuuust visible on my monitor
		sta $d048
		lda #$08										; reposition start of bottom border to what's juuuuust visible on my monitor
		sta $d04a
		lda #$02
		sta $d04b

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

.align 256

.macro plotpixel offset, coladd
		lda #<.loword(screenchars0 + offset)
		sta zp0+0
		lda #>.loword(screenchars0 + offset)
		sta zp0+1
		lda #<.hiword(screenchars0 + offset)
		sta zp0+2
		lda #>.hiword(screenchars0 + offset)
		sta zp0+3

		ldz #$00
		lda plotcol
		clc
		adc coladd
		sta [zp0],z
.endmacro

.macro	copyline
.scope
		lda yto+0
		sta to+0
		lda yto+1
		sta to+1
		lda xto
		and #$07
		;clc
		adc to+0
		sta to+0
		lda xto
		and #$f8
		adc to+1
		sta to+1

		lda yfrom+0
		sta from+0
		lda yfrom+1
		sta from+1
		lda xfrom
		and #$07
		;clc
		adc from+0
		sta from+0
		lda xfrom
		and #$f8
		adc from+1
		sta from+1

		sta $d707										; inline DMA copy
		;.byte $82, $00									; Source skip rate (256ths of bytes)
		.byte $83, $08									; Source skip rate (whole bytes)
		;.byte $84, $00									; Destination skip rate (256ths of bytes)
		.byte $85, $08									; Destination skip rate (whole bytes)
		.byte $00										; end of job options
		.byte $00										; copy
		.word $0010										; count
from	.word $0000										; src
		.byte <.hiword(screenchars0)					; src bank and flags
to		.word $0000										; dst
		.byte <.hiword(screenchars1)					; dst bank and flags
		.byte $00										; cmd hi
		.word $0000										; modulo, ignored

.endscope		
.endmacro

irq1
		pha

		jsr peppitoPlay

		plotpixel ((16-1)*(256*8)+16*64-2-8), 0
		plotpixel ((16-1)*(256*8)+16*64-2-0), 0
		plotpixel ((16-1)*(256*8)+16*64-2+8), 0
		plotpixel ((16-1)*(256*8)+16*64-1-8), 0
		plotpixel ((16-1)*(256*8)+16*64-1-0), 1
		plotpixel ((16-1)*(256*8)+16*64-1+8), 1
		plotpixel ((16+0)*(256*8)+16*64+0-16), 1
		plotpixel ((16+0)*(256*8)+16*64+0-8), 1
		plotpixel ((16+0)*(256*8)+16*64+0-0), 1

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
			clc
			adc xfrom
			sta xfrom
			ldx #14
xloop			.repeat 16
					copyline
					inc xto
					inc xfrom
				.endrepeat
				;clc
				lda yfrom+0
				adc #$08
				sta yfrom+0
				lda yfrom+1
				adc #$00
				sta yfrom+1
				dec xfrom
				dex
				bmi :+
				jmp xloop
:			;clc
			lda yto+0
			adc #128
			sta yto+0
			lda yto+1
			adc #0
			sta yto+1
			dey
			bmi :+
			jmp yloop
:			

		;lda #$15
		;sta $d020

		DMA_RUN_JOB copybufferjob

		;lda #$00
		;sta $d020

		inc frame

		pla
		asl $d019
		rti

frame			.byte 0
screenrow		.byte 0
screencolumn	.byte 0
verticalcenter	.byte 0

plotcol			.byte 0


shifts			.byte 0, 8, 4, 12, 2, 10, 6, 14
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

clearscreenjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (screen1 >> 20)						; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
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

				.word <(emptychar/64)							; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((screen1) & $ffff)						; Destination Address LSB + Destination Address MSB
				.byte (((screen1) >> 16) & $0f)					; Destination Address BANK and FLAGS (copy to rbBaseMem)
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

				.word >(emptychar/64)							; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((screen1)+1) & $ffff						; Destination Address LSB + Destination Address MSB
				.byte ((((screen1)+1) >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

clearbitmapjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, (screenchars0 >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				;.byte $84, $00									; Destination skip rate (256ths of bytes)
				;.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000011									; fill and don't chain

				.word 40*25*64									; Count LSB + Count MSB

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

; -------------------------------------------------------------------------------------------------

copybufferjob
				;DMA_HEADER $20000 >> 20, $30000 >> 20
				; f018a = 11 bytes, f018b is 12 bytes
				.byte $0a ; Request format is F018A
				.byte $80, (screenchars1 >> 20) ; sourcebank
				.byte $81, (screenchars0 >> 20) ; destbank

				.byte $82, 0 ; Source skip rate (256ths of bytes)
				.byte $83, 1 ; Source skip rate (whole bytes)

				.byte $84, 0 ; Destination skip rate (256ths of bytes)
				.byte $85, 1 ; Destination skip rate (whole bytes)

				.byte $00 ; No more options

				.byte $00 ; Copy and last request
				.word 32*32*64-1 ; Size of Copy

				.word screenchars1 & $ffff
				.byte (screenchars1 >> 16)

				.word screenchars0 & $ffff
				.byte ((screenchars0 >> 16) & $0f)

				.word $0000
