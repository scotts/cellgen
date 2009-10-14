DIRS = spu  
PROGRAM_ppu = vecdot

IMPORTS = spu/lib_vecdot_spu.a \
	  -lpthread -lspe2 -lm -lc
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer
INCLUDE = -I/opt/cell/sdk/usr/include/
include /opt/cell/sdk/make.footer
