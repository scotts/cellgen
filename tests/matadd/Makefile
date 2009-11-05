DIRS = spu  
PROGRAM_ppu = matadd

IMPORTS = spu/lib_matadd_spu.a -lpthread -lspe2 -lm -lc -lmisc -lnuma
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer
INCLUDE = -I/opt/cell/sdk/usr/include/
include /opt/cell/sdk/make.footer
