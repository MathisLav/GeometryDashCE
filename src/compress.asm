.nolist
	#include "ti84pce.inc"
	#define db .db
	#define dw .dw
	#define dl .dl
	#macro bcall(x)
		call x
	#endmacro
	#DEFINE BGCl	$7F
.list
	.org userMem-2
	.db tExtTok,tAsm84CeCmp

	ld de,pixelshadow+3
	ld hl,tiles
	ld ix,0
startingCompression:
	ld c,(hl)
	ld b,0
countPxls:
	inc hl
	inc b
	ld a,b
	or a
	jr z,endCount
	ld a,(hl)
	cp c
	jr z,countPxls
endCount:
	ex de,hl
	ld (hl),b
	inc hl
	ld (hl),c
	inc hl
	ex de,hl
	inc ix
	ld a,(hl)
	cp -1
	jr nz,startingCompression
	ld (pixelshadow),ix

	ld hl,AppVarName
	call _mov9toop1
	call _chkfindsym
	call nc,_DELVAR
	ld hl,(pixelshadow)
	add hl,hl
	inc hl
	inc hl
	inc hl
	push hl
	call _createAppVar
	inc de
	inc de
	ld hl,pixelshadow
	pop bc
	ldir
	ret

AppVarName:
	.db appvarObj,"GDGrphc",0
AppVarName2:
	.db appvarObj,"GDMenu",0

menu:
;#include "spritesGD/GDFont.asm"
;#include "spritesGD/menu.asm"
;#include "spritesGD/sides.asm"
;#include "spritesGD/header.asm"
	.db -1
.echo $-menu

tiles:
#include "spritesGD/tiles.asm"
#include "spritesGD/jump.asm"
#include "spritesGD/spaceship.asm"
	.db -1
.echo $-tiles

.end
