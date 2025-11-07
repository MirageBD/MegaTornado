# -----------------------------------------------------------------------------

megabuild		= 0
attachdebugger	= 0

# -----------------------------------------------------------------------------

MAKE			= make
CP				= cp
MV				= mv
RM				= rm -f
CAT				= cat

SRC_DIR			= ./src
UI_SRC_DIR		= ./src/ui
UIELT_SRC_DIR	= ./src/ui/uielements
DRVRS_SRC_DIR	= ./src/drivers
EXE_DIR			= ./exe
BIN_DIR			= ./bin

# mega65 fork of ca65: https://github.com/dillof/cc65
AS				= ca65mega
ASFLAGS			= -g -D megabuild=$(megabuild) --cpu 45GS02 -U --feature force_range -I ./exe
LD				= ld65
C1541			= c1541
CC1541			= cc1541
SED				= sed
PU				= pucrunch
BBMEGA			= b2mega
LC				= crush 6
GCC				= gcc
MC				= MegaConvert
MEGAADDRESS		= megatool -a
MEGACRUNCH		= megatool -c
MEGAIFFL		= megatool -i
MEGAMOD			= MegaMod
EL				= etherload
XMEGA65			= D:\PCTOOLS\xemu\xmega65.exe
MEGAFTP			= mega65_ftp -e

CONVERTBREAK	= 's/al [0-9A-F]* \.br_\([a-z]*\)/\0\nbreak \.br_\1/'
CONVERTWATCH	= 's/al [0-9A-F]* \.wh_\([a-z]*\)/\0\nwatch store \.wh_\1/'

CONVERTVICEMAP	= 's/al //'

.SUFFIXES: .o .s .out .bin .pu .b2 .a

default: all

OBJS = $(EXE_DIR)/boot.o $(EXE_DIR)/main.o

BINFILES  = $(BIN_DIR)/bitmap_pal0.bin
BINFILES += $(BIN_DIR)/song.mod

# -----------------------------------------------------------------------------

$(BIN_DIR)/bitmap_pal0.bin: $(BIN_DIR)/bitmap.bin
	$(MC) $< cm1:1 d1:3 cl1:10000 rc1:0

$(EXE_DIR)/boot.o:	$(SRC_DIR)/boot.s \
					$(SRC_DIR)/main.s \
					$(SRC_DIR)/irqload.s \
					$(SRC_DIR)/decruncher.s \
					$(SRC_DIR)/macros.s \
					$(SRC_DIR)/modplay.s \
					Makefile Linkfile
	$(AS) $(ASFLAGS) -o $@ $<

$(EXE_DIR)/boot.prg.addr.mc: $(BINFILES) $(EXE_DIR)/boot.o Linkfile
	$(LD) -Ln $(EXE_DIR)/boot.maptemp --dbgfile $(EXE_DIR)/boot.dbg -C Linkfile -o $(EXE_DIR)/boot.prg $(EXE_DIR)/boot.o
	$(MEGAADDRESS) $(EXE_DIR)/boot.prg 00002100
	$(MEGACRUNCH) -e 00002100 $(EXE_DIR)/boot.prg.addr

$(EXE_DIR)/megatorn.d81: $(EXE_DIR)/boot.prg.addr.mc $(BINFILES)
	$(RM) $@
	$(CC1541) -n "megatornado" -i " 2023" -d 19 -v\
	 \
	 -f "megatorn" -w $(EXE_DIR)/boot.prg.addr.mc \
	$@

# -----------------------------------------------------------------------------

run: $(EXE_DIR)/megatorn.d81

ifeq ($(megabuild), 1)
	$(MEGAFTP) -c "put D:\Mega\MegaTornado\exe\megatorn.d81 megatorn.d81" -c "quit"
	$(EL) -m MEGATORN.D81 -r $(EXE_DIR)/boot.prg.addr.mc
ifeq ($(attachdebugger), 1)
	m65dbg --device /dev/ttyS2
endif
else
	cmd.exe /c $(XMEGA65) -autoload -8 $(EXE_DIR)/megatorn.d81
endif

clean:
	$(RM) $(EXE_DIR)/*.*
	$(RM) $(EXE_DIR)/*
