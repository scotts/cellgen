DIRS = spu  
PROGRAM_ppu = row2d

IMPORTS = spu/lib_row2d_spu.a \
	  -lpthread -lspe2 -lm -lc -lmisc
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer
include /opt/ibm/cell-sdk/prototype/make.footer
