level_done:
	; New High Score
	ld a,$FF ; end of level
	call addHighScore

	DI
	ld hl,vRam+38
	ld de,vRam+39
	ld a,210
eraseEntireTailBrut:
	ld (hl),BGCl
	ld bc,73
	ldir
	ld bc,320-73
	add hl,bc
	ex de,hl
	add hl,bc
	ex de,hl
	dec a
	jr nz,eraseEntireTailBrut

	ld hl,vRam+(320*240)+38
	ld de,vRam+(320*240)+39
	ld a,210
eraseEntireTailBrut2:
	ld (hl),BGCl
	ld bc,73
	ldir
	ld bc,320-73
	add hl,bc
	ex de,hl
	add hl,bc
	ex de,hl
	dec a
	jr nz,eraseEntireTailBrut2

	bit FLG_spaceshipOn, (iy+asm_flag1)
	jp nz,DONE_animSpaceship

	ld a,12
DONE_spriteMoveForward:
	push af
	ld hl,(Vcharacter_posY)
	ld de,4
	add hl,de
	ld (Vcharacter_posY),hl

; Pseudo random
	ld a, l
	and a, %00000111 ; 7 max
	ld de, 0
	ld e, a

	ld bc,(currentDrawLoc)
	add hl,bc
	ld bc,(SPR_sizeY-11)*320+2
	add hl,bc
	add hl,de

	ld a,r
	srl a
	srl a
	srl a
	srl a
	inc a
	ld de,0
	ld e,a
	or a
	sbc hl,de
	ex de,hl
	ld hl,(currentTailEl)
	push hl
	; size of TailEl
	ld a,r
	srl a
	srl a
	srl a
	srl a
	srl a
	inc a
	ld (hl),a
	inc hl
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld de,tailEffectBufPos+(TAIL_nb*4)+4
	ex de,hl
	or a
	sbc hl,de
	ex de,hl
	jr nz,DONE_TAIL_bufNotLastEl
	ld hl,tailEffectBufPos
DONE_TAIL_bufNotLastEl:
	ld (currentTailEl),hl
	pop hl
	ld a,(hl)
	inc hl
	ld hl,(hl)
	ld b,a
DONE_drawingElTail:
	ld e,b
	ld b,a
	push hl
DONE_drawingElTailX:
	ld (hl),TAIL_fstClr
	inc hl
	djnz DONE_drawingElTailX
	pop hl
	ld bc,320
	add hl,bc
	ld b,e
	djnz DONE_drawingElTail
DONE_jumping:

	call DONE_dispStuffs
	ld a,30
MVF_slowDown:
	ld b,0
MVF_slowDownIn:
	ld de,(0)
	djnz MVF_slowDownIn
	dec a
	jr nz,MVF_slowDown

	pop af
	dec a
	jp nz,DONE_spriteMoveForward

	; **** Erasing Tail (again)
	ld b,TAIL_nb+1
	ld hl,tailEffectBufPos
eraseTail2:
	push bc
	push hl
	ld a,(hl)
	inc hl
	ld hl,(hl)
	ld b,a
DONE_erasingTail2:
	ld e,b
	ld b,a
	push hl
DONE_erasingTailX2:
	ld (hl),BGCl
	inc hl
	djnz DONE_erasingTailX2
	pop hl
	ld bc,320
	add hl,bc
	ld b,e
	djnz DONE_erasingTail2
	pop hl
	inc hl
	inc hl
	inc hl
	inc hl
	pop bc
	djnz eraseTail2
	
	; **** Aspiration
	ld hl,-320+5
	ld (jmp_speed),hl
	ld a,99 ; slowDown
	ld (scrapMem),a
	ld b,27
	ld de,-320
DONE_aspiration:
	push bc
	ld a,(scrapMem)
ASP_slowDown:
	ld b,0
ASP_slowDownIn:
	ld hl,(0)
	djnz ASP_slowDownIn
	dec a
	jr nz,ASP_slowDown

	ld hl,(jmp_speed)
	add hl,de
	ld (jmp_speed),hl
	ld bc,(Vcharacter_posY)
	add hl,bc
	ld (Vcharacter_posY),hl
	push de
	call DONE_dispStuffs
	pop de
	pop bc
	ld a,18
	cp b
	jr nz,staySameSTG
	set 7, (iy+asm_flag1)
	ld de,320
	ld hl,(jmp_speed)
	inc hl
	inc hl
	ld (jmp_speed),hl
staySameSTG:
	djnz DONE_aspiration

	call DONE_dispPartSprite
	call pageFlipScreen
	call DONE_eraseSprite

DRW_drawLines:
	; First line
	ld hl,(currentDrawLoc)
	inc hl
	inc hl
	inc hl
	ld de,323
	ld c,60
	ld a,55
DRW_1st_drawLineLoop:
	ld b,2
DRW_1st_drawLineY:
	push bc
	ld b,c
	push hl
DRW_1st_drawLineX:
	ld (hl),6
	inc hl
	djnz DRW_1st_drawLineX
	pop hl
	add hl,de
	pop bc
	djnz DRW_1st_drawLineY
	dec c
	dec a
	jr nz,DRW_1st_drawLineLoop

	ld bc,-320*7-13
	add hl,bc
	ld b,7
	ld de,320
DRW_1st_eraseEndOfLineY:
	ld a,20
	push hl
DRW_1st_eraseEndOfLineX:
	ld (hl),BGCl
	inc hl
	dec a
	jr nz,DRW_1st_eraseEndOfLineX
	pop hl
	add hl,de
	djnz DRW_1st_eraseEndOfLineY

	; Second line
	ld hl,(currentDrawLoc)
	push hl
	ld de,(40*320)-143
	add hl,de
	ld de,327
	ld c,143
	ld a,68
DRW_2nd_drawLineLoop:
	ld b,1
DRW_2nd_drawLineY:
	push bc
	ld b,c
	push hl
DRW_2nd_drawLineX:
	ld (hl),6
	inc hl
	djnz DRW_2nd_drawLineX
	pop hl
	add hl,de
	pop bc
	djnz DRW_2nd_drawLineY
	dec c
	dec c
	dec a
	jr nz,DRW_2nd_drawLineLoop

	ld bc,-320*4-13
	add hl,bc
	ld b,4
	ld de,320
DRW_2nd_eraseEndOfLineY:
	ld a,15
	push hl
DRW_2nd_eraseEndOfLineX:
	ld (hl),BGCl
	inc hl
	dec a
	jr nz,DRW_2nd_eraseEndOfLineX
	pop hl
	add hl,de
	djnz DRW_2nd_eraseEndOfLineY

	pop hl
	ld bc,(40*320)-143
	add hl,bc
	ld de,327
	ld b,143/7+1
	ld c,143
DRW_2nd_eraseBeginingY:
	push bc
	ld b,c
	push hl
DRW_2nd_eraseBeginingX:
	ld (hl),BGCl
	inc hl
	djnz DRW_2nd_eraseBeginingX
	pop hl
	add hl,de
	pop bc
	ld a,-7
	add a,c
	ld c,a
	djnz DRW_2nd_eraseBeginingY

	; Third line
	ld hl,(currentDrawLoc)
	ld de,210
	add hl,de
	ld de,321
	ld c,35
	ld a,33
DRW_3rd_drawLineLoop:
	ld b,3
DRW_3rd_drawLineY:
	push bc
	ld b,c
	push hl
DRW_3rd_drawLineX:
	ld (hl),6
	inc hl
	djnz DRW_3rd_drawLineX
	pop hl
	add hl,de
	pop bc
	djnz DRW_3rd_drawLineY
	dec c
	dec a
	jr nz,DRW_3rd_drawLineLoop

	ld hl,(currentDrawLoc)
	ld de,320*105
	add hl,de
	ex de,hl
	push de
	pop bc
	ld hl,-320
	add hl,de
	ex de,hl
	ld a,105
makeMirorLinesY:
	push af
	ld hl,320
makeMirorLinesX:
	ld a,(de)
	ld (bc),a
	inc de
	inc bc
	push de
	ld de,1
	or a
	sbc hl,de
	pop de
	jr nz,makeMirorLinesX
	ld hl,-320*2
	add hl,de
	ex de,hl
	pop af
	dec a
	jr nz,makeMirorLinesY

	call DONE_dispPartSprite
	call pageFlipScreen

	ld hl,(mpLcdBase)
	ld de,(currentDrawLoc)
	ld bc,320*240
	ldir

	ld a,$80
	call _DelayTenTimesAms
	ret


DONE_dispPartSprite:
	ld hl,(spr_state)
	ld de,(hl)
	ld hl,(currentDrawLoc)
	ld bc,320*90-13
	bit FLG_spaceshipOn, (iy+asm_flag1)
	jr z,DONE_noSphp
	ld bc,320*96-13
DONE_noSphp:
	add hl,bc
	ld a,(spriteSize)
	ld b,a
DONE_dispPartSpriteY:
	push bc
	ld b,13
DONE_dispPartSpriteX:
	ld a,(de)
	cp BGCl
	jr z,DONE_dontDisp
	ld (hl),a
DONE_dontDisp:
	inc hl
	inc de
	djnz DONE_dispPartSpriteX
	ld bc,320-13
	add hl,bc
	ex de,hl
	ld bc,(spriteSize)
	add hl,bc
	ld bc,-13
	add hl,bc
	ex de,hl
	pop bc
	djnz DONE_dispPartSpriteY
	ret


DONE_dispStuffs:
	;	Displaying character
	exx
	ld de,(currentSprBuf)
	exx
	ld hl,(spr_state)
	ld de,(hl)
	ld bc,(Vcharacter_posY)
	ld hl,(currentDrawLoc)
	add hl,bc
	ex de,hl
	ld a,(spriteSize)
	ld b,a
DONE_dispSpriteLoop:
	push bc
	push de
	exx
	pop hl
	ld bc,(spriteSize)
	ldir
	exx

	ld a,(spriteSize)
	ld b,a
DONE_dispSprite:
	ld a,(hl)
	cp BGCl
	jr z,DONE_empyPxl
	ld (de),a
DONE_empyPxl:
	inc hl
	inc de
	djnz DONE_dispSprite
	
	ex de,hl
	ld bc,320
	add hl,bc
	ld bc,(spriteSize)
	sbc hl,bc
	ex de,hl
	pop bc
	djnz DONE_dispSpriteLoop
	;	Swaping buffers
	ld hl,(currentSprBuf)
	ld de,behindSprite1
	or a
	sbc hl,de
	jr nz,DONE_firstBuf
	ld de,behindSprite2
DONE_firstBuf:
	ld (currentSprBuf),de

	call pageFlipScreen

	ld hl,(currentTailEl)
	ld a,(hl)
	inc hl
	ld hl,(hl)
	ld b,a
DONE_erasingElTail:
	ld e,b
	ld b,a
	push hl
DONE_erasingElTailX:
	ld (hl),BGCl
	inc hl
	djnz DONE_erasingElTailX
	pop hl
	ld bc,320
	add hl,bc
	ld b,e
	djnz DONE_erasingElTail
DONE_eraseSprite:
	;	Erasing previous square
	ld bc,(prev_posY)
	ld hl,(currentDrawLoc)
	add hl,bc
	ex de,hl
	ld hl,(currentSprBuf)
	ld a,(spriteSize)
DONE_eraseSpriteY:
	ld bc,(spriteSize)
	ldir
	ex de,hl
	ld bc,320
	add hl,bc
	ld bc,(spriteSize)
	sbc hl,bc
	ex de,hl
	dec a
	jr nz,DONE_eraseSpriteY

	ld hl,(spr_state)
	ld (prev_sprState),hl
	ld hl,(Vcharacter_posY)
	ld (prev_posY),hl
	ret

DONE_animSpaceship:
	DI
	ld hl,(Vcharacter_posY)
	ld bc,-320
	ld de,$0000FF
	inc e
	add hl,bc
	jr c,$-2
	ex de,hl
	ld de,-105+(SPHP_sizeY/2)
	add hl,de
	ex de,hl
	ld hl,$800000
	or a
	sbc hl,de
	ld ix,320
	ld a,l
	ld de,-3
	ld (scrapMem),de
	jr c,posAboveMid
	ld de,3
	ld (scrapMem),de
	ld ix,-320
	neg
	ld l,a
posAboveMid:
	ld hl,0
	ld l,a
	ld e,1
	cp 70
	jr c,SPHP_3per3
	lea bc,ix+0
	add ix,ix
	add ix,bc
	ld e,3
	jr SPHP_2per2
SPHP_3per3:
	cp 35
	jr c,SPHP_2per2
	add ix,ix
	ld e,2
SPHP_2per2:
	ld a,e
	call _divHLbyA
	ld a,l
	ex af,af'
	ld b,44
	ld iy,0
SPHP_animDone:
	push bc
	ld hl,(Vcharacter_posY)
	ex af,af'
	or a
	jr z,+_
	dec a
	lea de,ix+0
	add hl,de
_:
	ex af,af'
	lea de,iy+0
	srl e
	srl e
	add hl,de
	ld a,28
	cp e
	jr z,SPHP_enoughSpeed
	inc iy
SPHP_enoughSpeed:
	ld (Vcharacter_posY),hl

	bit FLG_spaceshipOn, (iy+asm_flag1)
	jr z,DONE_noChangingOfState
	ld hl,(spr_state)
	ld de,(scrapMem)
	add hl,de
	ex de,hl
	ld hl,_spaceship_tiles_end
	or a
	sbc hl,de
	jr c,DONE_noChangingOfState
	ld hl,_spaceship_tiles_beg
	or a
	sbc hl,de
	jr nc,DONE_noChangingOfState
	ld (spr_state),de
DONE_noChangingOfState:
	call DONE_dispStuffs

	ld a,$50
SPHP_slowDown:
	ld b,0
SPHP_slowDownIn:
	ld de,(0)
	djnz SPHP_slowDownIn
	dec a
	jr nz,SPHP_slowDown

	pop bc
	djnz SPHP_animDone
	ld iy,flags
	jp DRW_drawLines

pageFlipScreen:
	ld hl,vRam
	ld bc,(mpLcdBase)
	or a
	sbc hl,bc
	add hl,bc
	jr nz,DONE_notCurrBuf
	ld hl,vram+(lcdWidth*lcdHeight)
DONE_notCurrBuf:
	ld (currentDrawLoc),bc
	ld bc,mpLcdIcr
	ld a,(bc)
	or a,4 ; bitLcdLnBuim
	ld (bc),a
	ld (mpLcdBase),hl
DONE_waitForSync:
	ld a,(mpLcdRis)
	and a,4 ; bitLcdLnBuim
	jr z,DONE_waitForSync
	ret
