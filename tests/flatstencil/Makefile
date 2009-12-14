DIRS = spu 
PROGRAM_ppu = stencil

IMPORTS = spu/lib_stencil_spu.a -lpthread -lspe2 -lm -lc -lmisc -lnuma
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer -ggdb
include /opt/cell/sdk/make.footer

