DIRS = spu  
PROGRAM_ppu = vecdot

IMPORTS = spu/lib_vecdot_spu.a -lpthread -lspe2 -lm -lc -lnuma
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer -ggdb
INCLUDE = -I/opt/cell/sdk/usr/include/
include /opt/cell/sdk/buildutils/make.footer
