DIRS = spu  
PROGRAM_ppu = conv2d

IMPORTS = spu/lib_conv2d_spu.a -lpthread -lspe2 -lm -lc -lmisc -lnuma
      
CFLAGS = -O3 -DUNIX -fomit-frame-pointer
include /opt/cell/sdk/buildutils/make.footer

