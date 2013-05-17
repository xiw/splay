CC		= gcc
CFLAGS		= -O2 -finline-functions -fno-strict-aliasing -g
CFLAGS		+= -Wall -Wwrite-strings
ALL_CFLAGS	= `llvm-config --cflags` $(CFLAGS)
CXX		= g++
ALL_CXXFLAGS	= `llvm-config --cxxflags` $(CFLAGS) $(CXXFLAGS)

AR		= ar
LD		= $(CXX)
LDFLAGS		+= -g `llvm-config --ldflags`

LITFLAGS	= -s -v

V		= @
Q		= $(V:1=)
QUIET_CC	= $(Q:@=@echo    '     CC       '$@;)
QUIET_CXX	= $(Q:@=@echo    '     CXX      '$@;)
QUIET_GEN	= $(Q:@=@echo    '     GEN      '$@;)
QUIET_LINK	= $(Q:@=@echo    '     LINK     '$@;)

PROGRAMS	= splay

-include local.mk

all: $(PROGRAMS)

splay: splay.o constant.o function.o type.o llvm.o sparse/libsparse.a
	$(QUIET_LINK)$(LD) -o $@ $^ `llvm-config --libs core analysis` $(LDFLAGS)

%.o: %.c
	$(QUIET_CC)$(CC) -o $@ -c $(ALL_CFLAGS) $<

%.o: %.cc
	$(QUIET_CXX)$(CXX) -o $@ -c $(ALL_CXXFLAGS) $<

sparse/libsparse.a:
	$(QUIET_GEN)cd sparse && $(MAKE) $(notdir $@) > /dev/null

check: all
	$(Q)`llvm-config --src-root`/utils/lit/lit.py $(LITFLAGS) test

clean:
	rm -f *.o $(PROGRAMS)

distclean: clean
	cd sparse && $(MAKE) clean
