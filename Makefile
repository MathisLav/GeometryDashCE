WINE=wine64

spasm:
	$(WINE) spasm64.exe -E -L src/GD.z80 AGD.8xp

send:
	sudo tilp -n -s AGD.8xp

all:
	$(WINE) spasm64.exe -E -L src/GD.z80 AGD.8xp
	sudo tilp -n -s AGD.8xp

sendall:
	sudo tilp -n -s AGD.8xp levels/*.8xv ./*.8xv

release:
	rm GeometryDash.zip
	zip -r GeometryDash.zip include/* levels/* src/* *.8xp *.8xv *.png LISEZMOI.txt README.md 
