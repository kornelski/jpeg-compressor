OBJSB         = encoder.o
OBJSL         = jpge.o jpgd.o
OBJS          = $(OBJSB) $(OBJSL)
CXXFLAGS     ?= -O3 -ffast-math -fno-signed-zeros
LDFLAGS       = -s
BIN           = jpge
LIB           = lib$(BIN)
PREFIX        = /usr/local
INCPREFIX     = $(PREFIX)/include/$(BIN)
LIBPREFIX     = $(PREFIX)/lib
MANPREFIX     = $(PREFIX)/share/man/man1
INSTALL       = install
LN            = ln -fs
VER           = 0
VERREL        = 1.04
LIBNAME       = $(LIB).so.$(VER)

$(BIN): $(OBJSB) $(LIBNAME)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

$(LIBNAME): $(OBJSL)
	$(CXX) -shared -Wl,-soname,$@ $(CXXFLAGS) $(LDFLAGS) -o $@ $^
	chmod 644 $@
	mv $@ $@.$(VERREL)
	$(LN) -s $@.$(VERREL) $@
	$(LN) -s $@ $(LIB).so

clean:
	rm $(OBJS) $(BIN) $(LIB)*

install: $(BIN)
	$(INSTALL) -d $(LIBPREFIX)
	$(INSTALL) -m 0644 $(LIBNAME).$(VERREL) $(LIBPREFIX)/$(LIBNAME).$(VERREL)
	$(LN)  $(LIBPREFIX)/$(LIBNAME).$(VERREL)  $(LIBPREFIX)/$(LIBNAME)
	$(LN)  $(LIBPREFIX)/$(LIBNAME)  $(LIBPREFIX)/$(LIB).so
	$(INSTALL) -d $(INCPREFIX)
	$(INSTALL) -m 0644 *.h $(INCPREFIX)
	$(INSTALL) -d $(PREFIX)/bin
	$(INSTALL) -m 0755 $(BIN) $(PREFIX)/bin/
	$(INSTALL) -d $(MANPREFIX)
	$(INSTALL) -m 0644 ../man/man1/*.1 $(MANPREFIX)
