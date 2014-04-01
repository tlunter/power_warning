CC=ghc
CFLAGS=
LDFLAGS=
SRCDIR=src
OBJDIR=obj
BINDIR=build
SOURCES=$(wildcard $(SRCDIR)/*.hs)
EXECUTABLE=power_warning
ROOT?=

all: out $(EXECUTABLE)

$(EXECUTABLE):
	$(CC) $(LDFLAGS) $(SOURCES) -o $(BINDIR)/$@

out:
	mkdir -p $(SRCDIR) $(BINDIR)

install:
	mkdir -p $(ROOT)/usr/bin
	install $(BINDIR)/$(EXECUTABLE) $(ROOT)/usr/bin/$(EXECUTABLE)

clean:
	rm -rf $(BINDIR)/$(EXECUTABLE)
