DIRS = spu  
PROGRAM_ppu = fglite

IMPORTS = spu/lib_fglite_spu.a \
	  -lpthread -lspe2 -lm -lc -lmisc
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer
include /opt/ibm/cell-sdk/prototype/make.footer
