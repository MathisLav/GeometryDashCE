drawSprite:
	; INPUT : 
	;	IX : sprite data
	;	HL : Position (ADDR)
	;	DE : width of the buffer/screen
	ld bc,(ix)
	inc ix
	inc ix
drawSpriteY:
	push bc
	push de
	ld b,c
drawSpriteX:
	ld a,(ix)
	cp BGCl_menu
	jr z,noDraw
	ld (hl),a
noDraw:
	inc hl
	inc ix
	djnz drawSpriteX
	pop de
	add hl,de
	inc.s bc
	ld b,0
	dec c
	or a
	sbc hl,bc
	pop bc
	djnz drawSpriteY
	ret

drawSpriteMirror:
	; INPUT : 
	;	IX : sprite data
	;	HL : Position (ADDR)
	ld bc,(ix)
	inc ix
	ld de,0
	ld e,c
	add ix,de
drawSpriteMirrorY:
	push bc
	ld b,c
drawSpriteMirrorX:
	ld a,(ix)
	cp BGCl_menu
	jr z,MIR_noDraw
	ld (hl),a
MIR_noDraw:
	inc hl
	dec ix
	djnz drawSpriteMirrorX
	ld de,320
	add hl,de
	ld d,0
	ld e,c
	or a
	sbc hl,de
	sla e
	add ix,de
	pop bc
	djnz drawSpriteMirrorY
	ret


drawMiniTile:
	; INPUT : 
	;	IX : sprite data
	;	HL : Position (ADDR)
	ld b,TIL_sizeY/2
MINI_drawMiniTileY:
	push bc
	ld b,TIL_sizeX/2
MINI_drawMiniTileX:
	ld a,(ix)
	cp BGCl
	jr z,MINI_noDrawMini
	ld (hl),a
MINI_noDrawMini:
	inc hl
	inc ix
	inc ix
	djnz MINI_drawMiniTileX
	ld de,320
	add hl,de
	ld bc,-(TIL_sizeX/2)
	add hl,bc
	ld bc,TIL_sizeX+1
	add ix,bc
	pop bc
	djnz MINI_drawMiniTileY
	ret


drawTile:
	; INPUT : 
	;	IX : sprite data
	;	HL : Position (ADDR)
	ld b,TIL_sizeY
TILE_drawTileY:
	push bc
	ld b,TIL_sizeX
TILE_drawTileX:
	ld a,(ix)
	cp BGCl
	jr z,TILE_noDraw
	ld (hl),a
TILE_noDraw:
	inc hl
	inc ix
	djnz TILE_drawTileX
	ld de,320
	add hl,de
	ld bc,-TIL_sizeX
	add hl,bc
	pop bc
	djnz TILE_drawTileY
	ret


putGDFont:
	; INPUT :
	;	HL : pos
	;	IX : string
	ld a,(ix)
	or a
	ret z
	ld b,a
	inc ix
putGDFont_loop:
	ld a,(ix)
	cp ' '
	jr nz,notSpace
	ld de,10
	add hl,de
	inc ix
	djnz putGDFont_loop
	ret
notSpace:
	push bc
	dec a
	push hl
	ld e,a
	or a
	rla
	add a,e
	ld hl,_GDfont_tiles
	ld de,0
	ld e,a
	add hl,de
	ld de,(hl)
	pop hl

	ld b,28
drawLetterY:
	push bc
	ld b,28
drawLetterX:
	ld a,(de)
	cp BGCl_menu
	jr z,emptyPxl
	ld (hl),a
emptyPxl:
	inc hl
	inc de
	djnz drawLetterX
	ld bc,250-28
	add hl,bc
	pop bc
	djnz drawLetterY
	ld bc,-(28*250)+25
	add hl,bc
	inc ix
	pop bc
	djnz putGDFont_loop
	ret


centerText:
	; INPUT :
	;	HL : beginning
	;	DE : Space available (pxl)
	;	IX : string
	; OUTPUT :
	;	HL : new display location
	push hl
	ld hl,0
	ld b,(ix)
	inc ix
	push de
calculatingSpace:
	ld a,(ix)
	inc ix
	cp ' '
	jr nz,CEN_letter
	ld de,10
	add hl,de
	djnz calculatingSpace
	jr end_calculatingSpace
CEN_letter:
	ld de,25
	add hl,de
	djnz calculatingSpace
end_calculatingSpace:
	pop de
	ex de,hl
	or a
	sbc hl,de
	srl l
	pop de
	add hl,de
	ret


extractRLE:
	; INPUT :
	;	BC : number of "colors"
	;	HL : FROM
	;	DE : TO
	push bc
	ld a,(hl)
	ld b,a
	inc hl
	ld a,(hl)
copyXpxls:
	ld (de),a
	inc de
	djnz copyXpxls
	inc hl
	pop bc
	dec bc
	xor a
	cp c
	jr nz,extractRLE
	cp b
	jr nz,extractRLE
	ret


Fcircle:
	push iy
	push hl
	push de
	push bc
	push af
	ld  (FCircleCenterPos_SMC),hl
	ld  bc,0
	ld  c,a
	sbc     hl,bc
	ld  (FCircleLdirpos_SMC),hl
	ex  de,hl                               ; de = x coord - radius
	ld  hl,colorCircle               ; hl = color of circle
	push    de
	ldi
	pop hl
	ret     po
	rlc     c
	inc c
	ldir
	ld  (FCircleLddrpos_SMC),hl
	ld  b,a
	inc a
	ld  d,a
	ld  e,a
	mlt     de
	ld  iy,0
	add iy,de
	ld  c,a
Fory:
	lea hl,iy+0                     ; kind of For(y,R,1,-1
	ld  a,c
	ld  d,b
	ld  e,b
	mlt     de                          ; de = y²
	sbc     hl,de
	ex  de,hl                       ; de = (R²-y²)

Forx:
	ld  h,a                         ; kind of For(x,R,y,-1
	ld  l,a
	mlt     hl                          ; hl = x²
	sbc     hl,de                       ; x² < (R² - y²) ?
	dec a
	jr  nc,Forx                     ; no?   then loop
	push    bc                          ; yes?  here we go!
FCircleCenterPos_SMC =$+1
	ld  hl,0                        ; hl = 'on-screen' center pos
	ld  c,lcdWidth/2
	mlt     bc
	push    bc                          ; bc = 160*y
	add hl,bc
	add hl,bc
	ld  b,0
	ld  c,a                         ; bc = x
	add hl,bc
	ex  de,hl                       ; de = 'on-screen' horizontal drawing beginning address
FCircleLddrpos_SMC =$+1
	ld  hl,0                        ; hl = pointer to color data
	ld  b,0
	inc a
	rlca
	ld  c,a                         ; bc = drawing length
	lddr                                ; trace 1st horizontal line (bottom)
	pop hl                          ; now, calculate mirrored position...
	add hl,hl
	add hl,hl                       ; hl = 160*y*4
	inc de
	ex  de,hl
	sbc     hl,de
	ex  de,hl                       ; de = 'on-screen' horizontal drawing beginning address
	; added
	ld hl,vRam
	or a
	sbc hl,de
	jr nc,dontDisp
	; *****
FCircleLdirpos_SMC =$+1
	ld  hl,0                        ; hl = pointer to color data
	ld  c,a                         ; bc = drawing length
	ldir                                ; trace 2nd horizontal line (top)
dontDisp:
	pop bc
	djnz    Fory
	pop af
	pop bc
	pop de
	pop hl
	pop iy
	ret
colorCircle:
	.db 4


getkeyLetter:
	; INPUT :
	;	NONE
	; OUTPUT :
	;	A : char or -1 if DEL is pressed or -2 if ENTER is pressed
	ld hl,$F50000
	ld (hl),2
	xor a
GKL_scanWait:
	cp (hl)
	jr nz,GKL_scanWait

	ld a,(kbdG1)
	ld c,DEL
	bit kbitDel, a
	ret nz

	ld a,(kbdG2)
	ld c,'X' - 40h
	bit kbitStore, a
	ret nz
	ld c,'S' - 40h
	bit kbitLn, a
	ret nz
	ld c,'N' - 40h
	bit kbitLog, a
	ret nz
	ld c,'I' - 40h
	bit kbitSquare, a
	ret nz
	ld c,'D' - 40h
	bit kbitRecip, a
	ret nz
	ld c,'A' - 40h
	bit kbitMath, a
	ret nz

	ld a,(kbdG3)
	bit kbit0, a
	ld c,' '
	ret nz
	ld c,'Y' - 40h
	bit kbit1, a
	ret nz
	ld c,'T' - 40h
	bit kbit4, a
	ret nz
	ld c,'O' - 40h
	bit kbit7, a
	ret nz
	ld c,'J' - 40h
	bit kbitComma, a
	ret nz
	ld c,'E' - 40h
	bit kbitSin, a
	ret nz
	ld c,'B' - 40h
	bit kbitApps, a
	ret nz

	ld a,(kbdG4)
	ld c,'Z' - 40h
	bit kbit2, a
	ret nz
	ld c,'U' - 40h
	bit kbit5, a
	ret nz
	ld c,'P' - 40h
	bit kbit8, a
	ret nz
	ld c,'K' - 40h
	bit kbitLParen, a
	ret nz
	ld c,'F' - 40h
	bit kbitCos, a
	ret nz
	ld c,'C' - 40h
	bit kbitPrgm, a
	ret nz

	ld a,(kbdG5)
	ld c,'V' - 40h
	bit kbit6, a
	ret nz
	ld c,'Q' - 40h
	bit kbit9, a
	ret nz
	ld c,'L' - 40h
	bit kbitRParen, a
	ret nz
	ld c,'G' - 40h
	bit kbitTan, a
	ret nz

	ld a,(kbdG6)
	ld c,'W' - 40h
	bit kbitSub, a
	ret nz
	ld c,'R' - 40h
	bit kbitMul, a
	ret nz
	ld c,'M' - 40h
	bit kbitDiv, a
	ret nz
	ld c,'H' - 40h
	bit kbitPower, a
	ret nz
	ld c,ENTER
	bit kbitEnter, a
	ret nz
	ld c,CLEAR
	bit kbitClear, a
	ret nz

	ld a,(kbdG7)
	ld c,UP
	bit kbitUp, a
	ret nz
	ld c,DOWN
	bit kbitDown, a
	ret nz

	jp getkeyLetter
