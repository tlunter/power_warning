CC=ghc
CFLAGS=
LDFLAGS=
SRCDIR=src
OBJDIR=obj
BINDIR=build
SOURCES=$(wildcard $(SRCDIR)/*.hs)
EXECUTABLE=power_warning

all: out $(EXECUTABLE)

$(EXECUTABLE):
	$(CC) $(LDFLAGS) $(SOURCES) -o $(BINDIR)/$@

out:
	mkdir -p $(SRCDIR) $(BINDIR)

clean:
	rm -rf $(BINDIR)/$(EXECUTABLE)
