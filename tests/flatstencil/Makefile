DIRS = spu 
PROGRAM_ppu = flatstencil

IMPORTS = spu/lib_flatstencil_spu.a -lpthread -lspe2 -lm -lc -lmisc -lnuma
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer -ggdb
include /opt/cell/sdk/make.footer

