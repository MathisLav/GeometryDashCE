SPASM=spasm
TILP="sudo tilp"
ZIP=zip
BINARIES := $(wildcard bin/*.8x[pv])


spasm:
	$(SPASM) -E src/GD.z80 bin/AGD.8xp

send:
	$(TILP) -n -s AGD.8xp

all:
	$(SPASM) -E src/GD.z80 bin/AGD.8xp
	$(TILP) -n -s bin/AGD.8xp

sendall:
	$(TILP) -n -s levels/*.8xv bin/*.8x{p,v}


release: 
	rm -f GeometryDash.zip
	cp $(BINARIES) .
	$(ZIP) -r GeometryDash.zip include/ levels/ images/ src/ *.8x[vp] README.md
	rm -f $(subst bin/,,$(BINARIES))
