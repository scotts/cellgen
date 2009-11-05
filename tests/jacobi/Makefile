DIRS = spu 
PROGRAM_ppu = jacobi

IMPORTS = spu/lib_jacobi_spu.a -lpthread -lspe2 -lm -lc -lmisc -lnuma
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer -ggdb
include /opt/cell/sdk/make.footer

