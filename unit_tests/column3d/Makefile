DIRS = spu  
PROGRAM_ppu = column3d

IMPORTS = spu/lib_column3d_spu.a \
	  -lpthread -lspe2 -lm -lc -lmisc
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer
include /opt/ibm/cell-sdk/prototype/make.footer
