DIRS = spu  
PROGRAM_ppu = vecscalar

IMPORTS = spu/lib_vecscalar_spu.a \
	  -lpthread -lspe2 -lm -lc
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer
INCLUDE = -I/opt/cell/sdk/usr/include/
include /opt/cell/sdk/make.footer
