; DEFINE for the edition
#DEFINE NBBLOCKSX 15

; A FAIRE :
;	- copier/coller (sto -> coller)


editLevel:
	call _poprealo5
	ld hl,AppVarGraphics
	call _mov9ToOp1
	call _chkFindSym
	ld hl,19
	add hl,de
	ld bc,(hl)
	inc hl
	inc hl
	inc hl
	ld de,tilesExtracted
	call extractRLE

	ld de,mpLcdPalette
	ld hl,_TILES_pal
	ld bc,512
	ldir
	ld hl,vRam
	ld (mpLcdBase),hl
	ld hl,vRam+(320*240)
	ld (currentDrawLoc),hl
	ld hl,vRam
	ld (hl),BGCl
	ld de,vRam+1
	ld bc,320*240*2
	ldir
	ld hl,vRam+(320*TIL_sizeY*MAP_sizeY)
	ld (hl),06
	ld de,vRam+(320*TIL_sizeY*MAP_sizeY)+1
	ld bc,320*1
	ldir
	ld (hl),$7E
	ld bc,320*240-((320*TIL_sizeY*MAP_sizeY)+(320*1))
	ldir
	ld hl,vRam+(320*TIL_sizeY*MAP_sizeY)+(320*240)
	ld (hl),06
	ld de,vRam+(320*TIL_sizeY*MAP_sizeY)+(320*240)+1
	ld bc,320*1
	ldir
	ld (hl),$7E
	ld bc,320*240-((320*TIL_sizeY*MAP_sizeY)+(320*1))
	ldir

	ld hl,(addrLvlEdited)
	ld (addrMAP_sizeX),hl
	ld de,(hl)
	ld (LV_MAP_sizeX),de
	push de
	inc hl
	inc hl
	inc hl
	ld (addrMAP_sizeY),hl
	push hl
	ld a,(hl)
	inc hl
	ld (LV_beginningMap),hl
	push hl
	call _mult16by8
	ld (LV_bytesToSkip),hl
	ld (LV_maxBytesToSkip),hl
	ex de,hl
	pop hl
	push hl
	add hl,de
	ld (LV_fstBlock),hl
	pop ix
	pop hl
	ld a,(hl)
	add a,10
	pop de
	call _mult16by8
	ex de,hl
	add ix,de
	ld (LV_addrGravity),ix
	ld a,(ix)
	ld c,a
	add a,c
	add a,c
	inc a
	ld bc,0
	ld c,a
	add ix,bc
	ld (LV_addrSpaceship),ix

	ld hl,$000505
	ld (posCursorScrnX),hl
	ld a,1
	ld (currentBlock),a
	res FLG_twoPartTile, (iy+asm_flag1)


EDIT_mainloop:
	call EDIT_displayLevel
	call EDIT_displayBlockChoice
	ld hl,(LV_addrGravity)
	ld ix,tilesExtracted+(21*21*56)+(900*11)+(22*22*13)+5822+132
	call EDIT_dispChgCtxt
	ld hl,(LV_addrSpaceship)
	ld ix,tilesExtracted+(21*21*56)+(900*11)+(22*22*13)+5822+132+(21*21)+2
	call EDIT_dispChgCtxt
	call EDIT_dispCursor
	call EDIT_swapBuffers

	ld hl,$F50000
	ld (hl),2
	xor a
EDIT_scanWait:
	cp (hl)
	jr nz,EDIT_scanWait

	ld a,(kbdG7)
	and %00001111
	jr nz,EDIT_arrowPressed
	ld a,(kbdG1)
	and %00010001
	jr nz,EDIT_arrowPressed
	res FLG_blockArrows, (iy+asm_flag1)
	ld a,5
	ld (timerArrow),a
EDIT_arrowPressed:

	ld a,(kbdG7)
	bit kbitLeft, a
	call nz, EDIT_goLeft
	ld a,(kbdG7)
	bit kbitRight, a
	call nz, EDIT_goRight
	ld a,(kbdG7)
	bit kbitUp, a
	call nz, EDIT_goUp
	ld a,(kbdG7)
	bit kbitDown, a
	call nz, EDIT_goDown
	ld a,(kbdG1)
	bit kbit2nd, a
	jp nz, EDIT_insertCurrentBlock
	bit kbitMode, a
	jp nz, EDIT_quit
	bit kbitYequ, a
	call nz, EDIT_prevBlock
	ld a,(kbdG1)
	bit kbitGraph, a
	call nz, EDIT_nextBlock
	ld a,(kbdG1)
	bit kbitDel, a
	jp nz, EDIT_delBlock

	ld a,(kbdG3)
	bit kbit1, a
	jr nz, EDIT_insertBotLeft
	bit kbit4, a
	jr nz, EDIT_insertMidLeft
	bit kbit7, a
	jr nz, EDIT_insertUpLeft
	ld a,(kbdG4)
	bit kbit2, a
	jr nz, EDIT_insertBotMid
	bit kbit5, a
	jr nz, EDIT_insertCenter
	bit kbit8, a
	jr nz, EDIT_insertUpMid
	bit kbitPrgm, a
	jp nz, EDIT_playLevel
	ld a,(kbdG5)
	bit kbit3, a
	jr nz, EDIT_insertBotRight
	bit kbit6, a
	jr nz, EDIT_insertMidRight
	bit kbit9, a
	jr nz, EDIT_insertUpRight
	res FLG_toogleCtxt, (iy+asm_flag1)
	bit kbitTan, a
	jp nz, EDIT_toogleCtxt
	set FLG_toogleCtxt, (iy+asm_flag1)
	ld a,(kbdG2)
	bit kbitLn, a
	jp nz,EDIT_toogleCtxt

	ld a,(kbdG6)
	bit kbitAdd, a
	jp nz, EDIT_extendMapX
	bit kbitSub, a
	jp nz, EDIT_reduceMapX
	bit kbitMul, a
	jp nz, EDIT_extendMapY
	bit kbitDiv, a
	jp nz, EDIT_reduceMapY
	res FLG_blockInsDel, (iy+asm_flag1)
	res FLG_blockMode, (iy+asm_flag1)
	bit kbitClear, a
	jp z,EDIT_mainloop
	jp speedQuit


EDIT_insertBotLeft:
	ld a,26
	jr EDIT_insertBlock
EDIT_insertMidLeft:
	ld a,13
	jr EDIT_insertBlock
EDIT_insertUpLeft:
	ld a,4
	jr EDIT_insertBlock
EDIT_insertBotMid:
	ld a,30
	jr EDIT_insertBlock
EDIT_insertCenter:
	ld a,23
	jr EDIT_insertBlock
EDIT_insertUpMid:
	ld a,22
	jr EDIT_insertBlock
EDIT_insertBotRight:
	ld a,35
	jr EDIT_insertBlock
EDIT_insertMidRight:
	ld a,14
	jr EDIT_insertBlock
EDIT_insertUpRight:
	ld a,5
	jr EDIT_insertBlock

EDIT_insertBlock:
	; INPUT :
	;	A : Block to insert
	ex af,af'
	call EDIT_getAddrBehindCursor
	ex af,af'
	ld (hl),a
	jp EDIT_mainloop

EDIT_insertCurrentBlock:
	call EDIT_getAddrBehindCursor
	ld a,(currentBlock)
	ld (hl),a
	bit FLG_twoPartTile, (iy+asm_flag1)
	jp z,EDIT_mainloop
	ld de,(LV_MAP_sizeX)
	or a
	sbc hl,de
	push hl
	ld de,(LV_beginningMap)
	sbc hl,de
	pop hl
	jp c,EDIT_mainloop
	ld a,(currentBlock2)
	ld (hl),a
	jp EDIT_mainloop

EDIT_delBlock:
	call EDIT_getAddrBehindCursor
	ld a,(hl)
	ld (hl),0
	;cp 6
	;jr z,EDIT_del2PartBlkTop
	;cp 7
	;jr z,EDIT_del2PartBlkTop
	;cp 8
	;jr z,EDIT_del2PartBlkTop
	;cp 15
	;jr z,EDIT_del2PartBlkBottom
	;cp 16
	;jr z,EDIT_del2PartBlkBottom
	;cp 17
	;jr z,EDIT_del2PartBlkBottom
	;cp 39
	;jr z,EDIT_del2PartBlkTop
	;cp 48
	;jr z,EDIT_del2PartBlkBottom
	;cp 54
	;jr z,EDIT_del2PartBlkTop
	;cp 55
	;jr z,EDIT_del2PartBlkBottom
	;ret
	;EDIT_del2PartBlkTop:
	;ld de,(LV_MAP_sizeX)
	;add hl,de
	;ld (hl),0
	;ret
	;EDIT_del2PartBlkBottom:
	;ld de,(LV_MAP_sizeX)
	;or a
	;sbc hl,de
	;ld (hl),0
	jp EDIT_mainloop

EDIT_prevBlock:
	call EDIT_blockArrows
	ret nz
	ld a,(currentBlock)
	ld b,-1
	or a
	jr nz,EDIT_changeCurrentBlock
	ld a,58
	ld (currentBlock),a
	ret
EDIT_nextBlock:
	call EDIT_blockArrows
	ret nz
	ld a,(currentBlock)
	ld b,1
	cp 58
	jr nz,EDIT_changeCurrentBlock
	xor a
	ld (currentBlock),a
	ret
EDIT_changeCurrentBlock:
	; INPUT
	;	B : direction
	add a,b
	cp 29
	jr z,EDIT_changeCurrentBlock
	cp 6
	jr z,EDIT_changeCurrentBlock
	cp 7
	jr z,EDIT_changeCurrentBlock
	cp 8
	jr z,EDIT_changeCurrentBlock
	cp 39
	jr z,EDIT_changeCurrentBlock
	cp 54
	jr z,EDIT_changeCurrentBlock
	ld (currentBlock),a
	cp 48
	jr z,EDIT_twoPartTile
	cp 15
	jr z,EDIT_twoPartTile
	cp 16
	jr z,EDIT_twoPartTile
	cp 17
	jr z,EDIT_twoPartTile
	cp 55
	jr z,EDIT_twoPartTile
	res FLG_twoPartTile, (iy+asm_flag1)
	ret
EDIT_twoPartTile:
	set FLG_twoPartTile, (iy+asm_flag1)
	call EDIT_searchOtherPart
	inc hl
	ld a,(hl)
	ld (currentBlock2),a
	ret

EDIT_quit:
	bit FLG_blockMode, (iy+asm_flag1)
	jp nz,EDIT_mainloop
	call _op5toop1
	jp _ARC_UNARC

EDIT_goLeft:
	call EDIT_blockArrows
	ret nz
	ld a,(kbdG2)
	bit kbitAlpha, a
	jr nz,EDIT_goBegMap
	ld a,(posCursorScrnX)
	cp 3
	jr nc,EDIT_noScrollLeft
	call EDIT_scrollLeft
	ret nz
	or a
	ret z
EDIT_noScrollLeft:
	dec a
	ld (posCursorScrnX),a
	ret
EDIT_goBegMap:
	ld hl,(LV_beginningMap)
	ld de,(LV_bytesToSkip)
	add hl,de
	ld (LV_fstblock),hl
	xor a
	ld (posCursorScrnX),a
	ret

EDIT_goRight:
	call EDIT_blockArrows
	ret nz
	ld a,(kbdG2)
	bit kbitAlpha, a
	jr nz,EDIT_goEndMap
	ld a,(posCursorScrnX)
	cp NBBLOCKSX-3
	jr c,EDIT_noScrollRight
	call EDIT_scrollRight
	ret c
	cp NBBLOCKSX-1
	ret z
EDIT_noScrollRight:
	inc a
	ld (posCursorScrnX),a
	ret
EDIT_goEndMap:
	ld hl,(LV_beginningMap)
	ld de,(LV_MAP_sizeX)
	add hl,de
	ld de,-17-NBBLOCKSX
	add hl,de
	ld de,(LV_bytesToSkip)
	add hl,de
	ld (LV_fstblock),hl
	ld a,NBBLOCKSX-1
	ld (posCursorScrnX),a
	ret

EDIT_goUp:
	call EDIT_blockArrows
	ret nz
	ld a,(posCursorScrnY)
	cp 3
	jr nc,EDIT_noScrollUp
	call EDIT_scrollUp
	ret nz
	or a
	ret z
EDIT_noScrollUp:
	dec a
	ld (posCursorScrnY),a
	ret

EDIT_goDown:
	call EDIT_blockArrows
	ret nz
	ld a,(posCursorScrnY)
	cp WIN_SIZEY-3
	jr c,EDIT_noScrollDown
	call EDIT_scrollDown
	ret nz
	cp WIN_SIZEY-1
	ret z
EDIT_noScrollDown:
	inc a
	ld (posCursorScrnY),a
	ret

EDIT_scrollLeft:
	; OUTPUT :
	;	Returns Z if we touched the corner, NZ else
	ld hl,(LV_fstBlock)
	push hl
	ld de,(LV_bytesToSkip)
	or a
	sbc hl,de
	ld de,(LV_beginningMap)
	sbc hl,de
	pop hl
	ret z
	dec hl
	ld (LV_fstBlock),hl
	ret

EDIT_scrollRight:
	; OUTPUT :
	;	Returns NC if we are at the end of the level, C else
	ld hl,(LV_fstBlock)
	push hl
	ld de,(LV_bytesToSkip)
	or a
	sbc hl,de
	ld de,(LV_beginningMap)
	sbc hl,de
	ld de,17+NBBLOCKSX
	add hl,de
	ld de,(LV_MAP_sizeX)
	sbc hl,de
	pop hl
	ret nc

	inc hl
	ld (LV_fstBlock),hl
	ret

EDIT_scrollUp:
	; OUTPUT :
	;	Returns Z if we touched the corner, NZ else
	ld hl,(LV_bytesToSkip)
	ld de,0
	or a
	sbc hl,de
	ret z
	ld de,(LV_MAP_sizeX)
	sbc hl,de
	ld (LV_bytesToSkip),hl
	ld hl,(LV_fstblock)
	sbc hl,de
	ld (LV_fstblock),hl
	ret

EDIT_scrollDown:
	; OUTPUT :
	;	Returns Z if we touched the corner, NZ else
	ld hl,(LV_maxBytesToSkip)
	ld de,(LV_bytesToSkip)
	or a
	sbc hl,de
	ret z
	ld hl,(LV_MAP_sizeX)
	ex de,hl
	add hl,de
	ld (LV_bytesToSkip),hl
	ld hl,(LV_fstblock)
	add hl,de
	ld (LV_fstblock),hl
	ret

EDIT_dispCursor:
	ld a,(posCursorScrnY)
	ld de,320*TIL_sizeY
	call _mult16by8
	ld a,(posCursorScrnX)
	ld e,a
	ld d,TIL_sizeX
	mlt de
	add hl,de
	ld de,(currentDrawLoc)
	add hl,de
	inc hl
	inc hl

	ld ix,tilesExtracted+(21*21*56)+(900*11)+(22*22*13)+5822
	ld de,320
	call drawSprite
	ld bc,320*(21-(2*8))
	add hl,bc
	ld ix,tilesExtracted+(21*21*56)+(900*11)+(22*22*13)+5822+66
	call drawSprite

	ld de,-320*21+21-8
	add hl,de
	ld ix,tilesExtracted+(21*21*56)+(900*11)+(22*22*13)+5822
	call drawSpriteMirror

	ld de,320*(21-(2*8))
	add hl,de
	ld ix,tilesExtracted+(21*21*56)+(900*11)+(22*22*13)+5822+66
	call drawSpriteMirror
	ret

EDIT_playLevel:
	ld hl,0
	ld a,(kbdG2)
	bit kbitAlpha, a
	jr z,EDIT_fromBeg
	ld hl,(LV_fstblock)
	ld de,(LV_beginningMap)
	or a
	sbc hl,de
	ld de,(LV_bytesToSkip)
	sbc hl,de
	ld a,(posCursorScrnX)
	ld de,0
	ld e,a
	add hl,de
EDIT_fromBeg:
	ld (begLvlToPlay),hl

	set FLG_inEditorMode, (iy+asm_flag2)
	ld a,(iy+asm_flag1)
	ld (playMem),a
	call playLevel
	ld a,(playMem)
	ld (iy+asm_flag1),a

	xor a
	ld (mpLcdRange+11),a
	ld hl,$EF7802
	ld (mpLcdRange+8),hl
	ld hl,vRam
	ld (mpLcdBase),hl

	ld hl,vRam
	ld (hl),BGCl
	ld de,vRam+1
	ld bc,320*(TIL_sizeY*WIN_SIZEY)
	ldir
	ld hl,vRam+(320*240)
	ld (hl),BGCl
	ld de,vRam+(320*240)+1
	ld bc,320*(TIL_sizeY*WIN_SIZEY)
	ldir
	ld hl,vRam+(320*TIL_sizeY*MAP_sizeY)
	ld (hl),06
	ld de,vRam+(320*TIL_sizeY*MAP_sizeY)+1
	ld bc,320*1
	ldir
	ld hl,vRam+(320*TIL_sizeY*MAP_sizeY)+(320*240)
	ld (hl),06
	ld de,vRam+(320*TIL_sizeY*MAP_sizeY)+(320*240)+1
	ld bc,320*1
	ldir
	set FLG_blockMode, (iy+asm_flag1)
	jp EDIT_mainloop

EDIT_extendMapX:
	bit FLG_blockInsDel, (iy+asm_flag1)
	jp nz,EDIT_mainloop
	set FLG_blockInsDel, (iy+asm_flag1)

	res FLG_insDel10, (iy+asm_flag1)
	ld hl,(addrMAP_sizeY)
	ld a,(hl)
	ld hl,10 ; we clear H and HLU
	add a,l
	ld l,a
	ld a,(kbdG2)
	bit kbitAlpha, a
	jr z,EDIT_insOnlyOneColumnX
	ld h,10
	mlt hl
	set FLG_insDel10, (iy+asm_flag1)
EDIT_insOnlyOneColumnX:
	push hl
	call _EnoughMem
	pop hl
	jp c,EDIT_mainloop
	push hl
	call _op5toop1
	call _chkFindSym ; in ram
	ex de,hl
	ld de,(hl) ; space
	pop ix
	push ix
	add ix,de
	ld (hl),ix
	ld bc,1
	bit FLG_insDel10, (iy+asm_flag1)
	jr z,+_
	ld bc,10
_:
	call EDIT_updateContexts
	pop de
	ld hl,(LV_addrSpaceship)
	add hl,de
	ld (LV_addrSpaceship),hl
	ld hl,(LV_addrGravity)
	add hl,de
	ld (LV_addrGravity),hl

	ld a,(posCursorScrnX)
	ld de,0
	ld e,a
	ld hl,(LV_fstBlock)
	add hl,de
	ld de,(LV_bytesToSkip)
	sbc hl,de

	ld de,1
	bit FLG_insDel10, (iy+asm_flag1)
	jr z,EDIT_insOnlyOneColumnXBis
	ld de,10
EDIT_insOnlyOneColumnXBis:
	ld bc,(addrMAP_sizeY)
	ld a,(bc)
	ld b,10
	add a,b
	ld b,a
	ex de,hl
	; HL : nb to insert per line
	; DE : addr to insert
	; B : nb lines to process
EDIT_insertLoopX:
	push bc
	push hl
	call _InsertMEM
	ex de,hl
	ld (hl),0
	ex de,hl
	bit FLG_insDel10, (iy+asm_flag1)
	jr z,EDIT_insOnlyOneColumnX3
	push de
	pop hl
	inc de
	ld bc,9
	ldir
	ld ix,(LV_MAP_sizeX)
	add ix,de
	lea de,ix+0
	pop hl
	pop bc
	djnz EDIT_insertLoopX
	jr EDIT_afterInsertLoopX
EDIT_insOnlyOneColumnX3:
	ld hl,(LV_MAP_sizeX)
	add hl,de
	inc hl
	ex de,hl
	pop hl
	pop bc
	djnz EDIT_insertLoopX
EDIT_afterInsertLoopX:
	ex de,hl
	ld hl,(addrMAP_sizeX)
	ld ix,(hl)
	add ix,de
	ld (hl),ix
	ld (LV_MAP_sizeX),ix
	ld hl,(addrMAP_sizeY)
	push de
	ld d,(hl)
	mlt de
	ld hl,(LV_maxBytesToSkip)
	add hl,de
	ld (LV_maxBytesToSkip),hl
	ld hl,(LV_bytesToSkip)
	ld de,(LV_MAP_sizeX)
	call divHLbyDE_naive
	pop de
	ld d,a
	mlt de
	ld hl,(LV_bytesToSkip)
	add hl,de
	ld (LV_bytesToSkip),hl
	ld hl,(LV_fstBlock)
	add hl,de
	ld (LV_fstblock),hl
	jp EDIT_mainloop

EDIT_reduceMapX:
	bit FLG_blockInsDel, (iy+asm_flag1)
	jp nz,EDIT_mainloop
	set FLG_blockInsDel, (iy+asm_flag1)
	; etend map cut (EMC)
	ld hl,(LV_MAP_sizeX)
	ld de,41
	or a
	sbc hl,de
	jp c,EDIT_mainloop
	ld bc,-1
	call EDIT_updateContexts
	ld hl,(addrMAP_sizeY)
	ld a,(hl)
	ld de,10
	add a,e
	ld e,a
	ld hl,(LV_addrSpaceship)
	sbc hl,de
	ld (LV_addrSpaceship),hl
	ld hl,(LV_addrGravity)
	sbc hl,de
	ld (LV_addrGravity),hl

	ld hl,(addrMAP_sizeY)
	ld a,(hl)
	ld hl,10 ; we clear H and HLU
	add a,l
	ld l,a
	; EMC
	push hl
	; EMC
	call _op5toop1
	call _chkFindSym ; in ram
	ex de,hl
	ld de,(hl) ; space
	call _setDEUTo0
	ex de,hl
	pop bc
	or a
	sbc hl,bc
	ex de,hl
	ld (hl),de
	inc hl
	inc hl
	ld (hl),$FF ; erased by the LD

	ld a,(posCursorScrnX)
	ld de,0
	ld e,a
	ld hl,(LV_fstBlock)
	add hl,de
	ld de,(LV_bytesToSkip)
	sbc hl,de

	; EMC
	ld bc,(addrMAP_sizeY)
	ld a,(bc)
	ld b,10
	add a,b
	ld b,a
	; DE : nb to insert per line
	; HL : addr to insert
	; B : nb lines to process
EDIT_deleteLoopX:
	push bc
	push hl
	ld de,1
	call _DelMEM
	; EMC
	pop hl
	ld de,(LV_MAP_sizeX)
	dec de
	add hl,de
	pop bc
	djnz EDIT_deleteLoopX

	ld hl,(addrMAP_sizeY)
	ld de,0
	ld e,(hl)
	ld hl,(LV_maxBytesToSkip)
	or a
	sbc hl,de
	ld (LV_maxBytesToSkip),hl
	ld hl,(LV_bytesToSkip)
	ld de,(LV_MAP_sizeX)
	call divHLbyDE_naive
	ld de,0
	ld e,a
	ld hl,(LV_bytesToSkip)
	or a
	sbc hl,de
	ld (LV_bytesToSkip),hl
	ld hl,(LV_fstBlock)
	sbc hl,de
	ld (LV_fstblock),hl

	ld hl,(LV_fstBlock) ; if we are at the end of the level...
	ld de,(LV_bytesToSkip)
	or a
	sbc hl,de
	ld de,(LV_beginningMap)
	sbc hl,de
	ld de,17+NBBLOCKSX
	add hl,de
	ld de,(LV_MAP_sizeX)
	sbc hl,de
	call nc,EDIT_scrollLeft

	ld hl,(addrMAP_sizeX)
	ld ix,(hl)
	dec ix
	ld (hl),ix
	ld (LV_MAP_sizeX),ix
	jp EDIT_mainloop

EDIT_extendMapY:
	bit FLG_blockInsDel, (iy+asm_flag1)
	jp nz,EDIT_mainloop
	set FLG_blockInsDel, (iy+asm_flag1)

	call _op5toop1
	call _chkFindSym
	ex de,hl
	ld ix,(hl)
	ld de,(LV_MAP_sizeX)
	add ix,de
	ld (hl),ix
	ex de,hl
	ld de,(LV_beginningMap)
	call _InsertMEM
	ex de,hl
	ld (hl),0
	push hl
	pop de
	inc de
	ld bc,(LV_MAP_sizeX)
	dec bc
	ldir
	ld hl,(LV_maxBytesToSkip)
	ld de,(LV_MAP_sizeX)
	push de
	add hl,de
	ld (LV_maxBytesToSkip),hl
	ld hl,(addrMAP_sizeY)
	inc (hl)
	pop de
	ld hl,(LV_addrSpaceship)
	add hl,de
	ld (LV_addrSpaceship),hl
	ld hl,(LV_addrGravity)
	add hl,de
	ld (LV_addrGravity),hl
	jp EDIT_mainloop

EDIT_reduceMapY:
	ld a,(kbdG6)
	bit kbitEnter, a
	jr nz,EDIT_reduceSuit
	res FLG_blockInsDel, (iy+asm_flag1)
	jp EDIT_mainloop
EDIT_reduceSuit:
	bit FLG_blockInsDel, (iy+asm_flag1)
	jp nz,EDIT_mainloop
	set FLG_blockInsDel, (iy+asm_flag1)

	ld hl,(LV_maxBytesToSkip)
	ld de,0
	or a
	sbc hl,de
	jp z,EDIT_mainloop
	ld de,(LV_MAP_sizeX)
	sbc hl,de
	ld (LV_maxBytesToSkip),hl
	call _op5toop1
	call _chkFindSym
	ex de,hl
	push hl
	ld hl,(hl)
	ld de,(LV_MAP_sizeX)
	or a
	sbc hl,de
	ex de,hl
	pop hl
	ld (hl),de
	ld hl,(LV_beginningMap)
	ld de,(LV_MAP_sizeX)
	push de
	call _DelMEM
	pop de
	ld hl,(LV_addrSpaceship)
	or a
	sbc hl,de
	ld (LV_addrSpaceship),hl
	ld hl,(LV_addrGravity)
	sbc hl,de
	ld (LV_addrGravity),hl
	ld hl,(addrMAP_sizeY)
	dec (hl)
	ld hl,(LV_maxBytesToSkip)
	push hl
	ld de,(LV_bytesToSkip)
	or a
	sbc hl,de
	pop hl
	jp nc,EDIT_mainloop
	ld (LV_bytesToSkip),hl

	ld hl,(LV_fstblock)
	ld de,(LV_MAP_sizeX)
	or a
	sbc hl,de
	ld (LV_fstblock),hl
	jp EDIT_mainloop

EDIT_updateContexts:
	; Update contexts after a insertion/deletion of a column
	; INPUT
	;	- BC : nb to add to the contexts below the current position
	ld hl,(LV_fstblock)
	ld de,(LV_bytesToSkip)
	or a
	sbc hl,de
	ld de,(LV_beginningMap)
	sbc hl,de
	ld de,11
	add hl,de
	ld a,(posCursorScrnX)
	ld e,a
	add hl,de
	ex de,hl
	ld hl,(LV_addrGravity)
	ld a,2
EDIT_updateLoop:
	ex af,af'
	ld a,(hl)
	inc hl
	or a
	jr z,EDIT_noCtxt
EDIT_updateCtxtLoop:
	push hl
	ld ix,(hl)
	lea hl,ix+0
	or a
	sbc hl,de
	pop hl
	jr c,+_
	add ix,bc
	ld (hl),ix
_:
	inc hl
	inc hl
	inc hl
	dec a
	jr nz,EDIT_updateCtxtLoop
EDIT_noCtxt:
	ex af,af'
	dec a
	jr nz,EDIT_updateLoop
	ret

EDIT_displayLevel:
	; Display the level
	;ld hl,(currentDrawLoc)
	;ld (hl),0
	;push hl
	;pop de
	;inc de
	;ld bc,320*TIL_sizeY*MAP_sizeY-1
	;ldir
	
	ld de,(currentDrawLoc)
	inc de
	inc de
	ld hl,(LV_fstBlock)
	ld b,WIN_SIZEY
EDIT_DISP_mapY:
	push bc
	ld b,NBBLOCKSX
EDIT_DISP_mapX:
	push bc
	push hl

	ld a,(hl)
	ld bc,0
	ld c,a
	sla a
	add a,c
	ld hl,_GD_tiles
	ld c,a
	add hl,bc
	ld hl,(hl)
	ld a,TIL_sizeY
EDIT_DISP_blockY:
	ld bc,TIL_sizeX
	ldir
	ex de,hl
	ld bc,320-TIL_sizeX
	add hl,bc
	ex de,hl
	dec a
	jr nz,EDIT_DISP_blockY
	ld hl,-320*TIL_sizeY+TIL_sizeX
	add hl,de
	ex de,hl
	pop hl
	pop bc
	inc hl
	djnz EDIT_DISP_mapX
	ld bc,(LV_MAP_sizeX)
	add hl,bc
	ld bc,-NBBLOCKSX
	add hl,bc
	ex de,hl
	ld bc,320*TIL_sizeY - (NBBLOCKSX*TIL_sizeX)
	add hl,bc
	ex de,hl
	pop bc
	djnz EDIT_DISP_mapY
	ret

EDIT_displayBlockChoice:
	ld hl,(currentDrawLoc)
	ld de,(320*TIL_sizeY*MAP_sizeY)+(320*1)
	add hl,de
	ld (hl),$7E
	push hl
	pop de
	inc de
	ld bc,320*240-((320*TIL_sizeY*MAP_sizeY)+(320*1))-1
	ldir

	ld hl,(currentDrawLoc)
	ld de,(320*213)+147
	add hl,de
	call EDIT_drawSquareSelection

	ld a,(currentBlock)
	ld hl,(currentDrawLoc)
	ld de,(320*216)+150
	add hl,de
	call EDIT_drawTile
	call EDIT_prevBlock+5
	ld a,(currentBlock)
	ld hl,(currentDrawLoc)
	ld de,(320*216)+80
	add hl,de
	call EDIT_drawTile
	call EDIT_nextBlock+5
	call EDIT_nextBlock+5
	ld a,(currentBlock)
	ld hl,(currentDrawLoc)
	ld de,(320*216)+220
	add hl,de
	call EDIT_drawTile
	jp EDIT_prevBlock+5
EDIT_drawTile:
	; INPUT
	;	- A : n° tile
	;	- HL : Location
	push hl
	ld l,a
	ld h,3
	mlt hl
	ld de,_GD_tiles
	add hl,de
	ld ix,(hl)
	pop hl
	bit FLG_twoPartTile, (iy+asm_flag1)
	jr nz,EDIT_drawTwoPartTile
	jp drawTile
EDIT_drawTwoPartTile:
	push hl
	call EDIT_searchOtherPart
	inc hl
	ld l,(hl)
	ld h,3
	mlt hl
	ld de,_GD_tiles
	add hl,de
	ld ix,(hl)
	pop hl
	ld de,6 ; recenter
	add hl,de
	ex af,af'
	call drawMiniTile
	push hl
	ex af,af'
	ld l,a
	ld h,3
	mlt hl
	ld de,_GD_tiles
	add hl,de
	ld ix,(hl)
	pop hl
	jp drawMiniTile
EDIT_drawSquareSelection:
	; DESC
	;	Draw a 23*23px-long-square at coords HL
	; INPUT
	;	HL : addr to start
	ld (hl),0
	push hl
	pop de
	inc de
	ld bc,26
	ldir
	ld bc,320-26
	add hl,bc
	ex de,hl
	add hl,bc
	ex de,hl
	ld (hl),0
	ld bc,26
	ldir
	ld bc,320-26
	add hl,bc

	ld b,23
EDIT_drawSquareSides:
	ld (hl),0
	inc hl
	ld (hl),0
	ld de,24
	add hl,de
	ld (hl),0
	inc hl
	ld (hl),0
	ld de,320-26
	add hl,de
	djnz EDIT_drawSquareSides

	ld (hl),0
	push hl
	pop de
	inc de
	ld bc,26
	ldir
	ld bc,320-26
	add hl,bc
	ex de,hl
	add hl,bc
	ex de,hl
	ld (hl),0
	ld bc,26
	ldir
	ret

EDIT_dispChgCtxt:
	; INPUT
	;	- HL : data context
	;	- IX : sprite to display to inform of a changing of the current contexte
	push hl
	ld hl,(LV_fstblock)
	ld de,(LV_bytesToSkip)
	or a
	sbc hl,de
	ld de,(LV_beginningMap)
	sbc hl,de
	ex de,hl
	pop hl
	ld a,(hl)
	or a
	ret z
	ld b,a
EDIT_searchCtxtInScreen:
	inc hl
	push hl
	push de
	push bc
	ld hl,(hl)
	ld bc,-11
	add hl,bc
	or a
	sbc hl,de
	jr c,+_
	ex de,hl
	ld hl,14
	sbc hl,de
	jr c,+_
	; E = pos in screen
	ld d,TIL_sizeX
	mlt de
	ld hl,(currentDrawLoc)
	add hl,de
	inc hl
	inc hl
	ld de,320
	push ix
	call drawSprite
	pop ix
_:
	pop bc
	pop de
	pop hl
	inc hl
	inc hl
	djnz EDIT_searchCtxtInScreen
	ret

EDIT_toogleCtxt:
	; INPUT
	;	FLG_toogleCtxt: 0=gravity, 1=spaceship
	; OUTPUT
	;	Data of the level modified
	bit FLG_blockInsDel, (iy+asm_flag1)
	jp nz,EDIT_mainloop
	set FLG_blockInsDel, (iy+asm_flag1)
	ld hl,(LV_fstblock)
	ld de,(LV_bytesToSkip)
	or a
	sbc hl,de
	ld de,(LV_beginningMap)
	sbc hl,de
	ld a,(posCursorScrnX)
	ld de,11
	add a,e
	ld e,a
	add hl,de
	push hl
	pop bc
	ld hl,(LV_addrGravity)
	bit FLG_toogleCtxt, (iy+asm_flag1)
	jr z,+_
	ld hl,(LV_addrSpaceship)
_:
	push hl
	ld a,(hl)
	inc hl
	or a
	jr z,EDIT_addCtxt
	; HL : addr current ctxt
	; BC : addr behind cursor + 11
	; A  : ctxts to check
EDIT_ctxtLoop:
	ld de,(hl)
	ex de,hl
	or a
	sbc hl,bc
	jr z,EDIT_delCtxt
	ex de,hl
	jr nc,EDIT_addCtxt
	inc hl
	inc hl
	inc hl
	dec a
	jr nz,EDIT_ctxtLoop
EDIT_addCtxt:
	; INPUT
	;	DE : addr behind cursor + 11
	ex de,hl
	ld hl,3
	push bc
	call _InsertMEM
	pop hl
	ex de,hl
	ld (hl),de
	call _op5toop1
	call _chkFindSym
	ex de,hl
	ld de,(hl)
	inc de
	inc de
	inc de
	ld (hl),de
	pop hl
	inc (hl)
	bit FLG_toogleCtxt, (iy+asm_flag1)
	jp nz,EDIT_mainloop
	ld hl,(LV_addrSpaceship)
	inc hl
	inc hl
	inc hl
	ld (LV_addrSpaceship),hl
	jp EDIT_mainloop
EDIT_delCtxt:
	; INPUT
	;	DE : addr behind cursor + 11
	ex de,hl
	ld de,3
	call _DelMEM
	call _op5toop1
	call _chkFindSym
	ex de,hl
	ld de,(hl)
	dec de
	dec de
	dec de
	ld (hl),de
	pop hl
	dec (hl)
	bit FLG_toogleCtxt, (iy+asm_flag1)
	jp nz,EDIT_mainloop
	ld hl,(LV_addrSpaceship)
	dec hl
	dec hl
	dec hl
	ld (LV_addrSpaceship),hl
	jp EDIT_mainloop

EDIT_getAddrBehindCursor:
	; OUTPUT
	;	HL : ADDR of the block behind the cursor
	;	DE : value of LV_fstBlock
	ld a,(posCursorScrnY)
	ld de,(LV_MAP_sizeX)
	call _mult16by8
	ld a,(posCursorScrnX)
	ld d,0
	ld e,a
	add hl,de
	ld de,(LV_fstBlock)
	add hl,de
	ret

EDIT_blockArrows:
	bit FLG_blockArrows, (iy+asm_flag1)
	set FLG_blockArrows, (iy+asm_flag1)
	ret z
	ld a,(timerArrow)
	or a
	ret z
	dec a
	ld (timerArrow),a
	ret ; nz

EDIT_swapBuffers:
	; Swaping Vram Buffers (double-Buffering)
	ld hl,vRam
	ld bc,(mpLcdBase)
	or a
	sbc hl,bc  
	add hl,bc              ; Is the current buffer the screen buffer?
	jr nz,EDIT_notCurrBuf
	ld hl,vram+(lcdWidth*lcdHeight)
EDIT_notCurrBuf: 
	ld (currentDrawLoc),bc ; Set the new buffer location to the old screen pointer
	ld bc,mpLcdIcr
	ld a,(bc)
	or a,4 ; bitLcdLnBuim
	ld (bc),a
	ld (mpLcdBase),hl
EDIT_waitForSync: ; Wait fot the LCD to be ready to update
	ld a,(mpLcdRis)
	and a,4 ; bitLcdLnBuim
	jr z,EDIT_waitForSync
	ret

EDIT_searchOtherPart:
	; INPUT
	;	- A : tile to search
	; OUTPUT
	;	- (hl+1) : correspondance
	ld b,(LUT_TPT_end - LUT_twoPartTiles)/2
	ld hl,LUT_twoPartTiles
_:	
	ld c,(hl)
	cp c
	ret z
	inc hl
	inc hl
	djnz -_
	ret

divHLbyDE_naive:
	; OUTPUT :
	;	A = HL/DE
	xor a
	ld bc,0
	sbc hl,bc
	ret z
DIV_loop:
	inc a
	sbc hl,de
	ret z
	jr nc,DIV_loop
	ret


LUT_twoPartTiles:
	.db 15,6
	.db 16,7
	.db 17,8
	.db 55,54
	.db 48,39
LUT_TPT_end: