SPASM=spasm
TILP="sudo tilp"
ZIP=zip

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
	$(ZIP) -r GeometryDash.zip include/ levels/ bin/ levels/ images/ README.md 
