DIRS = spu  
PROGRAM_ppu = matmul 

IMPORTS = spu/lib_matmul_spu.a \
	  -lpthread -lspe2 -lm -lc -lmisc
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer
INCLUDE = -I/opt/cell/sdk/usr/include/
include /opt/cell/sdk/make.footer
