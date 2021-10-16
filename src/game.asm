playLevel:
	; ********* Préparation du jeu (extraction des tilesets, formatage des variables, des ports...)
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

	;ld a,$00
	;ld (mpLcdRange+11),a
	;ld hl,$EF7800
	;ld (mpLcdRange+8),hl
	ld a,$01
	ld (mpLcdRange+11),a
	ld hl,$007800
	ld (mpLcdRange+8),hl

playLevel_noextract:
	; dessin du jeu
	ld hl,vRam
	ld (mpLcdBase),hl
	ld hl,vRam+(320*240)
	ld (currentDrawLoc),hl

	ld hl,vRam
	ld (hl),BGCl
	ld de,vRam+1
	ld bc,320*240*2
	ldir
	
	ld de,mpLcdPalette
	ld hl,_TILES_pal
	ld bc,512
	ldir

	ld hl,vRam+(320*TIL_sizeY*MAP_sizeY)
	ld (hl),06
	ld de,vRam+(320*TIL_sizeY*MAP_sizeY)+1
	ld bc,320*1
	ldir
	ld (hl),$7e
	ld bc,320*240-((320*TIL_sizeY*MAP_sizeY)+(320*1))
	ldir
	ld hl,vRam+(320*TIL_sizeY*MAP_sizeY)+(320*240)
	ld (hl),06
	ld de,vRam+(320*TIL_sizeY*MAP_sizeY)+(320*240)+1
	ld bc,320*1
	ldir
	ld (hl),$7e
	ld bc,320*240-((320*TIL_sizeY*MAP_sizeY)+(320*1))
	ldir

	ld hl,(current_level)
	inc hl
	inc hl
	inc hl
	ld hl,(hl)
	inc hl
	inc hl
	inc hl
	push hl
	ld a,(hl)
	push af
	inc hl
	inc hl
	ld de,(hl)
	ld (MAP_sizeX),de
	inc hl
	inc hl
	inc hl
	ld a,(hl)
	inc hl
	ld (beginningMap),hl
	push hl
	call _mult16by8
	ld (bytesToSkip),hl
	ld (maxBytesToSkip),hl
	ex de,hl
	pop hl
	add hl,de
	push hl
	ld de,(begLvlToPlay)
	add hl,de

;	; DEBUG ***********
;	push hl
;	call _XName
;	call _chkFindSym
;	ld de,0
;	jr c,DEBUG_notFound
;	call _rclVarSym
;	call _convop1
;DEBUG_notFound:
;	pop hl
;	add hl,de
;	; *****************

	ld (fstBlock),hl
	ld a,10
	ld de,(MAP_sizeX)
	call _mult16by8
	pop de
	add hl,de
	ld a,(hl)
	ld (nbChngGrvtyRmng),a
	inc hl
	ld (addrGravity),hl
	ld b,a
	pop af
	res FLG_sphpAvlble, (iy+asm_flag2)
	or a
	jr z,oldVersion
	ld a,b
	add a,b
	add a,b
	ld bc,0
	ld c,a
	add hl,bc
	ld a,(hl)
	ld (nbChngSphpRmng),a
	ld (playMem+1),a
	inc hl
	ld (addrSpaceship),hl
	set FLG_sphpAvlble, (iy+asm_flag2)

	; if we don't start the level from the beginning (we need to adjust somme things with the contexts)
	ld hl,(begLvlToPlay)
	ld bc,0
	or a
	sbc hl,bc
	jr z,oldVersion
	; it means we are editing a level so LV_addrGravity and LV_addrSpaceship are set
	ld hl,(begLvlToPlay)
	ld de,8
	add hl,de
	ex de,hl
	ld hl,(LV_addrGravity)
	; HL = addr beg of gravity contexts
	; DE = position of the beginning of the level
	ld a,(hl)
	inc hl
	or a
	jr z,+_
findNextGravityChangeLoop:
	push hl
	ld hl,(hl)
	or a
	sbc hl,de
	pop hl
	jr c,GVTLP_before
	ld (addrGravity),hl
	ld (nbChngGrvtyRmng),a
	jr +_
GVTLP_before:
	inc hl
	inc hl
	inc hl
	dec a
	jr nz,findNextGravityChangeLoop
	ld (nbChngGrvtyRmng),a ; a=0
_:
	ld hl,(LV_addrSpaceship)
	ld a,(hl)
	inc hl
	or a
	jr z,oldVersion
findNextSpaceshipChangeLoop:
	push hl
	ld hl,(hl)
	or a
	sbc hl,de
	pop hl
	jr c,SPSLP_before
	ld (addrSpaceship),hl
	ld (nbChngSphpRmng),a
	jr oldVersion
SPSLP_before:
	inc hl
	inc hl
	inc hl
	dec a
	jr nz,findNextSpaceshipChangeLoop
	ld (nbChngSphpRmng),a ; a=0
oldVersion:
	; End adjustment

	ld hl,0
	ld (dispBlkFrmX),hl
	ld (dispBlkFrmY),hl
	ld (prev_speed),hl
	ld (prv_spdDiv320),hl
	ld hl,SPR_sizeX
	ld (spriteSize),hl
	ld hl,jmpSpeedLUT_0
	ld (jmp_speed),hl
	ld hl,_jump_tiles
	ld (spr_state),hl
	ld (prev_sprState),hl
	ld hl,(320*TIL_sizeY*7)+SPR_posX ; flying when spawning
	ld (Vcharacter_posY),hl
	ld (prev_posY),hl
	ld hl,tailEffectBufPos
	ld (currentTailEl),hl
	ld (hl),1
	inc hl
	ld de,pixelshadow
	ld (hl),de
	dec hl
	push hl
	pop de
	inc de
	inc de
	inc de
	inc de
	ld bc,TAIL_nb*5-1
	ldir
	; filling SprBufs with BGCl
	ld hl,behindSprite1
	ld (currentSprBuf),hl
	ex de,hl
	or a
	sbc hl,hl
	add hl,de
	ld (hl),BGCl
	inc de
	ld bc,30*30*2
	ldir
	res FLG_jmpAgain, (iy+asm_flag1)
	res FLG_rvrsGravity, (iy+asm_flag1)
	res FLG_topReached, (iy+asm_flag1)
	res FLG_botReached, (iy+asm_flag1)
	res FLG_spaceshipOn, (iy+asm_flag1)
	res FLG_alrdyErased, (iy+asm_flag1)
	res FLG_pauseLeft, (iy+asm_flag2)
	res FLG_jmpUsed, (iy+asm_flag2)
	set FLG_jumping, (iy+asm_flag1)
	;res FLG_colorTRS, (iy+asm_flag2)


	; animation which makes the sprite spawn
	ld de,(_jump_tiles)
	ld bc,(Vcharacter_posY)
	ld hl,(mpLcdBase)
	add hl,bc
	ld b,SPR_sizeY
ANIM_dispSpriteY:
	push bc
	ld b,SPR_sizeX
ANIM_dispSpriteX:
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	exx
	ld hl,$F50000
	ld (hl),2
	xor a
ANIM_scanWait:
	cp (hl)
	jr nz,ANIM_scanWait
	ld a,(kbdG6)
	bit kbitClear, a
	jp nz,speedQuit-2
	exx
	djnz ANIM_dispSpriteX
	ld bc,320-SPR_sizeX
	add hl,bc
	pop bc
	djnz ANIM_dispSpriteY

	ld hl,(Vcharacter_posY)
	ld (prev_posY),hl

	; initializing the counter
	xor a
	ld (mpTmr2Counter+3),a
	ld (mpTmr2Load+3),a
	ld hl,$000000
	ld (mpTmr2Counter),hl
	ld (mpTmr2Load),hl
	pop hl
	ld a,0Eh
	bit FLG_sphpAvlble, (iy+asm_flag2)
	jr z,INIT_oldVersionWhithoutSpeed
	inc hl
	ld a,(hl)
INIT_oldVersionWhithoutSpeed:
	ld (mpTmr2Counter+2),a
	ld (mpTmr2Load+2),a
	ld a,(mpTmrCtrl+1)
	and %11111011 ; count down
	ld (mpTmrCtrl+1),a
	ld a,(mpTmrCtrl)
	res 4, a ; using CPU clock
	set 5, a ; generate interrupts when it reaches 0
	set 3, a ; enable timer
	ld (mpTmrCtrl),a

	ld a,(nbChngSphpRmng)
	ld b,a
	ld a,(playMem+1)
	sub a,b
	bit 0, a ; if a is even
	call nz,spaceshipOn
	ld a,(nbChngGrvtyRmng)
	bit 0, a ; if a is even
	jr z,loop

	set FLG_rvrsGravity, (iy+asm_flag1)
	ld hl,(Vcharacter_posY)
	ld de,-320*2
	add hl,de
	ld (Vcharacter_posY),hl

	; On affiche des plateformes pour que (puisque l'écran est vide au début) le personage ne s'écrase pas sur le haut
	ld hl,_GD_tiles
	ld de,25*3
	add hl,de
	ld ix,(hl)
	ld hl,vRam+(320*42)+13
	exx
	ld hl,vRam+(320*240)+(320*42)+13
	exx
	ld a,14
loopDrawTileReversedMode:
	ex af,af'
	push ix
	call drawTile
	pop ix
	ld de,-(320*TIL_sizeY)+TIL_sizeX
	add hl,de
	exx
	push ix
	call drawTile
	pop ix
	ld de,-(320*TIL_sizeY)+TIL_sizeX
	add hl,de
	exx
	ex af,af'
	dec a
	jr nz,loopDrawTileReversedMode


; **************** BOUCLE PRINCIPALE	
loop: 
	; --------------------------------
	ld hl,$F50000
	ld (hl),2
	; waiting for the next frame
waitFrame:
	ld a,(mpTmrIntStatus)
	bit 5, a
	jr z,waitFrame
	ld a,%00111000
	ld (mpTmrIntStatus),a
	; ---------------------------------
	ld a,($F50012)
	bit 5, a ; 2nd
	jp nz,SPR_jump
	res FLG_jmpUsed, (iy+asm_flag2)
	bit kbitMode, a ; suppr
	ret nz
	ld a,($F5001C)
	bit 6, a ; clear
	jp nz,speedQuit
	bit 0, a ; enter
	;jp nz,initTransition
	jp nz,game_pause
	res FLG_pauseLeft, (iy+asm_flag2)
	; si on n'a pas appuyé sur 2nde et qu'on est en mode spaceship...
	bit FLG_spaceshipOn, (iy+asm_flag1)
	jp nz,SPCSHP_down
rsuit:
	;bit FLG_colorTRS, (iy+asm_flag2)
	;call nz,palette1TransitionPalette2

	ld a,(dispBlkFrmX)
	.fill SCROLL_spd/2,$3C ; inc a
	cp TIL_sizeX
	jr c,SCROLL_noOverflow
	ld hl,(fstBlock)
	inc hl
	ld (fstBlock),hl
	ld b,TIL_sizeX
	sub a,b
SCROLL_noOverflow:
	ld (dispBlkFrmX),a

	ld hl,(fstBlock)
	ld de,(bytesToSkip)
	or a
	sbc hl,de
	ld de,(beginningMap)
	sbc hl,de
	push hl
	ld a,(nbChngGrvtyRmng)
	or a
	jr z,noChangingOfGravity
	ex de,hl
	ld hl,(addrGravity)
	ld hl,(hl)
	sbc hl,de
	jr nc,noChangingOfGravity

	ld hl,(addrGravity)
	inc hl
	inc hl
	inc hl
	ld (addrGravity),hl
	ld hl,nbChngGrvtyRmng
	dec (hl)

	bit FLG_rvrsGravity, (iy+asm_flag1)
	jr nz,gravityRvrsd
	set FLG_rvrsGravity, (iy+asm_flag1)
	ld hl,(Vcharacter_posY)
	ld de,-320*2
	add hl,de
	ld (Vcharacter_posY),hl
	jr noChangingOfGravity
gravityRvrsd:
	res FLG_rvrsGravity, (iy+asm_flag1)
noChangingOfGravity:

	bit FLG_sphpAvlble, (iy+asm_flag2)
	jr z,afterChangingOfVehicle
	pop hl
	push hl
	ld a,(nbChngSphpRmng)
	or a
	jr z,afterChangingOfVehicle
	ex de,hl
	ld hl,(addrSpaceship)
	ld hl,(hl)
	sbc hl,de
	jr nz,afterChangingOfVehicle

	ld hl,(addrSpaceship)
	inc hl
	inc hl
	inc hl
	ld (addrSpaceship),hl
	ld hl,nbChngSphpRmng
	dec (hl)

	bit FLG_spaceshipOn, (iy+asm_flag1)
	jp nz,spaceshipOff
	call spaceshipOn
afterChangingOfVehicle:

	; Checking whether or not the level is done
	pop hl
	ld de,(MAP_sizeX)
	dec de
	or a
	sbc hl,de
	jp nc,level_done ; in endlevel.z80

	; Scrolling
	bit FLG_spaceshipOn, (iy+asm_flag1)
	jr nz,onlyScrollRight
	bit FLG_topReached, (iy+asm_flag1)
	jp nz,SCRUP_scrollUpSyncOtherBuf
	bit FLG_botReached, (iy+asm_flag1)
	jp nz,SCRDW_scrollDownSyncOtherBuf
	bit FLG_jumping, (iy+asm_flag1)
	jp z,onlyScrollRight
	ld de,(Vcharacter_posY)
	ld hl,320*40
	or a
	sbc hl,de
	jr c,checkScrollDwn
	push de
	ld hl,(jmp_speed)
	ld de,jmpSpeedLUT_0
	sbc hl,de
	pop de
	jp p,onlyScrollRight
	ld a,(bytesToSkip+1)
	or a
	jp nz,scrollUp
	ld a,(dispBlkFrmY)
	or a
	jp nz,scrollUp

checkScrollDwn:
	ld hl,320*140
	or a
	sbc hl,de
	jr nc,onlyScrollRight
	push de
	ld hl,(jmp_speed)
	ld de,jmpSpeedLUT_0
	or a
	sbc hl,de
	pop de
	jp m,onlyScrollRight
	ld a,(dispBlkFrmY)
	or a
	jp nz,scrollDown
	ld hl,(bytesToSkip)
	ld de,(maxBytesToSkip)
	sbc hl,de
	jp nz,scrollDown

onlyScrollRight:
	ld de,(currentDrawLoc)
	ld hl,SCROLL_spd
	add hl,de
	ld bc,320*(MAP_sizeY*TIL_sizeY)
	ldir
afterScroll:

	; displaying new blocks
	ld b,MAP_sizeY+1
	ld ix,(fstBlock)
	ld de,320-SCROLL_spd
	ld hl,(currentDrawLoc)
	add hl,de
	ld a,(dispBlkFrmX)
	cp TIL_sizeX-SCROLL_spd+1
	jp nc,NEW_dispTwoTiles
NEW_dispTiles:
	push bc
	push hl
	ld a,(ix)
	ld de,0
	ld e,a
	sla a
	add a,e
	ld e,a
	ld hl,_GD_tiles
	add hl,de
	ld hl,(hl)
	ld a,(dispBlkFrmX)
	ld e,a
	add hl,de
	pop de
	ld a,b
	cp MAP_sizeY+1
	jr nz,NEW_ifLastTile
	ld a,(dispBlkFrmY)
	ld c,a
	ld b,TIL_sizeX
	mlt bc
	add hl,bc
	ld b,a
	ld a,21
	sub a,b
	or a
	jr nz,NEW_putTileV
	ld bc,(MAP_sizeX)
	add ix,bc
	ex de,hl
	pop bc
	djnz NEW_dispTiles
NEW_ifLastTile:
	cp 1
	ld a,TIL_sizeY
	jr nz,NEW_putTileV
	ld a,(dispBlkFrmY)
	or a
	jr nz,NEW_putTileV
	pop bc ; don't disp
	jr endNEW_dispTiles
NEW_putTileV:
	ld bc,(hl)
	ex de,hl
	ld (hl),bc
	ex de,hl
	inc de
	inc de
	inc de
	inc hl
	inc hl
	inc hl
	ld bc,(hl)
	ex de,hl
	ld (hl),bc
	ex de,hl
	inc de
	inc de
	inc de
	inc hl
	inc hl
	inc hl
	ldi
	ldi

	ex de,hl
	ld bc,320-8
	add hl,bc
	ex de,hl
	ld bc,TIL_sizeX-8
	add hl,bc
	dec a
	jp nz,NEW_putTileV
	ex de,hl
	ld de,(MAP_sizeX)
	add ix,de
	pop bc
	dec b
	jp nz,NEW_dispTiles
endNEW_dispTiles:
	; end new blocks
	; Tiles have been displayed
	; ************************
	; Square
	;	makes the sprite jump
	bit FLG_rvrsGravity, (iy+asm_flag1)
	jp nz,RVRS_gravity
	bit FLG_spaceshipOn, (iy+asm_flag1)
	jp nz,SPCSHP_mode
	jp NORM_gravity

dispCharacter:
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
	push hl
	ld hl,vRam
	sbc hl,de
	jp nc,dieHard
	ld hl,vRam+(320*235*2)
	or a
	sbc hl,de
	jp c,dieHard
	pop hl
	ld a,(spriteSize)
	ld b,a
dispSpriteLoop:
	push bc
	push de
	exx
	pop hl
	ld bc,(spriteSize)
	ldir
	exx

	ld a,(spriteSize)
	ld b,a
dispSprite:
	ld a,(hl)
	cp BGCl
	jr z,empyPxl
	ld (de),a
empyPxl:
	inc hl
	inc de
	djnz dispSprite
	
	ex de,hl
	ld bc,320
	add hl,bc
	ld bc,(spriteSize)
	sbc hl,bc
	ex de,hl
	pop bc
	djnz dispSpriteLoop
	
	;	Swaping buffers
	ld hl,(currentSprBuf)
	ld de,behindSprite1
	or a
	sbc hl,de
	jr nz,firstBuf
	ld de,behindSprite2
firstBuf:
	ld (currentSprBuf),de
	
dspTail: ; Displaying the "tail" (TAIL_nb points) behind the SPRITE
	ld hl,(Vcharacter_posY)
	ld de,(currentDrawLoc)
	add hl,de
	ld de,(SPR_sizeY-12)*320+2
	bit FLG_rvrsGravity, (iy+asm_flag1)
	jr z,TAIL_noRVRSGravity
	ld de,(320*6)+2
TAIL_noRVRSGravity:
	bit FLG_spaceshipOn, (iy+asm_flag1)
	jr z,TAIL_noSpaceship
	ld de,320*14
TAIL_noSpaceship:
	add hl,de
	ex de,hl
	ld a,r
	ld h,0
	ld l,a
	srl l
	srl l
	srl l
	srl l
	srl l

	sla l
	sla l
	sla l
	sla l
	sla l
	sla l
	rl h
	push hl
	pop bc
	sla l
	rl h
	sla l
	rl h
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
	rrd		; more random
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
	jr nz,TAIL_bufNotLastEl
	ld hl,tailEffectBufPos
TAIL_bufNotLastEl:
	ld (currentTailEl),hl
	pop hl

	bit FLG_spaceshipOn, (iy+asm_flag1)
	jr nz,dispTail
	bit FLG_jumping, (iy+asm_flag1)
	jr nz,notDispTail
dispTail:
	ld a,(hl)
	inc hl
	ld hl,(hl)
	ld b,a
drawingElTail:
	ld e,b
	ld b,a
	push hl
drawingElTailX:
	ld (hl),TAIL_fstClr
	inc hl
	djnz drawingElTailX
	pop hl
	ld bc,320
	add hl,bc
	ld b,e
	djnz drawingElTail
notDispTail:

	; Swaping Vram Buffers (double-Buffering)
	ld hl,vRam
	ld bc,(mpLcdBase)
	or a
	sbc hl,bc  
	add hl,bc              ; Is the current buffer the screen buffer?
	jr nz,notCurrBuf
	ld hl,vram+(lcdWidth*lcdHeight)
notCurrBuf: 
	ld (currentDrawLoc),bc ; Set the new buffer location to the old screen pointer
	ld bc,mpLcdIcr
	ld a,(bc)
	or a,4 ; bitLcdLnBuim
	ld (bc),a
	ld (mpLcdBase),hl
waitForSync:            ; Wait for the LCD to be ready to update
	ld a,(mpLcdRis)
	and a,4 ; bitLcdLnBuim
	jr z,waitForSync
	;	Erasing element of the tail
	ld hl,(currentTailEl)
	ld a,(hl)
	inc hl
	ld hl,(hl)
	ld de,-(TAIL_nb*SCROLL_spd)/2
	add hl,de
	ld b,a
erasingElTail:
	ld e,b
	ld b,a
	push hl
erasingElTailX:
	ld (hl),BGCl
	inc hl
	djnz erasingElTailX
	pop hl
	ld bc,320
	add hl,bc
	ld b,e
	djnz erasingElTail

	;	Erasing previous square
	bit FLG_alrdyErased, (iy+asm_flag1)
	jr nz,spriteAlreadyErased
	ld bc,(prev_posY)
	ld hl,(currentDrawLoc)
	add hl,bc
	ex de,hl
	ld hl,vRam
	sbc hl,de
	jp nc,die
	ld hl,vRam+(320*235*2)
	or a
	sbc hl,de
	jp c,die
	ld hl,(currentSprBuf)
	ld a,(spriteSize)
eraseSpriteY:
	ld bc,(spriteSize)
	ldir
	ex de,hl
	ld bc,320
	add hl,bc
	ld bc,(spriteSize)
	sbc hl,bc
	ex de,hl
	dec a
	jr nz,eraseSpriteY
spriteAlreadyErased:
	ld hl,(spr_state)
	ld (prev_sprState),hl
	ld hl,(Vcharacter_posY)
	ld (prev_posY),hl

	res FLG_alrdyErased, (iy+asm_flag1)
	bit FLG_jmpAgain, (iy+asm_flag1)
	jp z,loop
	bit FLG_jumping, (iy+asm_flag1)
	jp z,SPR_jump
	jp loop
; ************** FIN DE LA BOUCLE PRINCIPALE


NEW_dispTwoTiles:
	push bc
	push hl
	;second Block ADDR
	ld a,(ix+1)
	ld de,0
	ld e,a
	sla a
	add a,e
	ld e,a
	ld hl,_GD_tiles
	add hl,de
	ld hl,(hl)
	ld (scrapMem),hl
	; first Block ADDR
	ld a,(ix)
	ld e,a
	sla a
	add a,e
	ld e,a
	ld hl,_GD_tiles
	add hl,de
	ld hl,(hl)
	ld a,(dispBlkFrmX)
	ld e,a
	add hl,de
	ex de,hl
	; **
	pop hl
	ld a,b
	cp MAP_sizeY+1
	jr nz,NEW_ifLastTwoTile
	ld a,(dispBlkFrmY)
	ld c,a
	ld b,TIL_sizeX
	mlt bc
	ex de,hl
	add hl,bc
	ex de,hl
	push hl
	ld hl,(scrapMem)
	add hl,bc
	ld (scrapMem),hl
	pop hl
	ld b,a
	ld a,21
	sub a,b
	or a
	jr nz,NEW_putTwoTileV
	ld de,(MAP_sizeX)
	add ix,de
	pop bc
	djnz NEW_dispTwoTiles
NEW_ifLastTwoTile:
	cp 1
	ld a,TIL_sizeY
	jr nz,NEW_putTwoTileV
	ld a,(dispBlkFrmY)
	or a
	jr nz,NEW_putTwoTileV
	pop bc ; don't disp
	jp endNEW_dispTiles
NEW_putTwoTileV:
	push af
	push de
	ld a,(dispBlkFrmX)
	ld b,a
	ld a,TIL_sizeX
	sub a,b
	ld b,a
NEW_dispFstTile:
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	djnz NEW_dispFstTile

	ex de,hl
	ld hl,(scrapMem)
	push hl
	ld a,(dispBlkFrmX)
	ld b,TIL_sizeX-SCROLL_spd
	sub a,b
	ld b,a
NEW_dispScnTile:
	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz NEW_dispScnTile
	pop hl
	ld bc,TIL_sizeX
	add hl,bc
	ld (scrapMem),hl
	ex de,hl

	pop de
	ld bc,320-SCROLL_spd
	add hl,bc
	push hl
	ld hl,TIL_sizeX
	add hl,de
	ex de,hl
	pop hl
	pop af
	dec a
	jp nz,NEW_putTwoTileV
	ld de,(MAP_sizeX)
	add ix,de
	pop bc
	dec b
	jp nz,NEW_dispTwoTiles
	jp endNEW_dispTiles


SPR_jump:
	bit FLG_spaceshipOn, (iy+asm_flag1)
	jp nz,SPCSHP_up

	; we don't want the sprite to jump when it is at the end of the level
	ld hl,(fstBlock)
	ld de,(bytesToSkip)
	or a
	sbc hl,de
	ld de,(beginningMap)
	sbc hl,de
	ld de,6
	add hl,de
	ld de,(MAP_sizeX)
	sbc hl,de
	jp nc,rsuit

	bit FLG_jumping, (iy+asm_flag1)
	jr nz,planNewJump

	res FLG_jmpAgain, (iy+asm_flag1)
	set FLG_jmpUsed, (iy+asm_flag2)
	set FLG_jumping, (iy+asm_flag1)
	ld hl,jmpSpeedLUT
	bit FLG_rvrsGravity, (iy+asm_flag1)
	jr z,noRVRSGravity
	ld hl,jmpSpeedLUT_rvrs
noRVRSGravity:
	ld (jmp_speed),hl
	jp rsuit
planNewJump:
	ld hl,(jmp_speed)
	ld de,(hl)
	bit FLG_rvrsGravity, (iy+asm_flag1)
	jr nz,JAG_gravityRvrsd
	ld hl,320*4
	sbc hl,de
	jp p,rsuit
	set FLG_jmpAgain, (iy+asm_flag1)
	jp rsuit
JAG_gravityRvrsd:
	ld hl,-320*4
	sbc hl,de
	jp m,rsuit
	set FLG_jmpAgain, (iy+asm_flag1)
	jp rsuit

;initTransition:
	;TOURS : 64 → 0
	;
	;	ROUGE		VERT		BLEU
	;	12			29			8
	;	0			25			31
	;	-12			-4			23
	;0-(-12*x/64) 25-(-4*x/64) 31-(23*x/64)
	;
	;F-(DC*x/T)
		
	; ix = _tiles_pal_color
	; R DR G DG B DB = 6o*256 = 1536o
	;call DEBUG
	;ld a,64
	;ld (TRS_counter),a
	;
	;ld ix,TRS_paletteSRC
	;ld de,mpLcdPalette
	;ld hl,_tiles_pal_violet
	;ld b,0
;initTransitionLoop:
	;ld a,(hl)
	;and a,%00011111
	;ld (ix+5),a
	;ex af,af'
	;ld a,(de)
	;and a,%00011111
	;ld c,a
	;ex af,af'
	;sub a,c
	;ld (ix+4),a
	;
	;ld a,(hl)
	;srl a
	;srl a
	;srl a
	;srl a
	;and a,%00001110
	;inc hl
	;ld a,(hl)
	;and a,%10000011
	;ld c,a
	;sla c
	;ex af,af' ; saving last bit of C in carry flag
	;sla c
	;sla c
	;ex af,af'
	;rl c
	;or a,c
	; trouver un moyen de mettre sauvegarder le résultat de and a,%00001110 jsuqu'à là pour le OR
	; Puis faire le même chose pour ce qui suit
	; Enfin, adapter l'autre loop en conséquence
	
	;ld (ix+3),a
	;ex af,af'
	;ld a,(de)
	;srl a
	;srl a
	;srl a
	;srl a
	;srl a
	;ld c,a
	;inc de
	;ld a,(de)
	;sla a
	;sla a
	;sla a
	;or a,c
	;ld c,a
	;ex af,af'
	;sub a,c
	;ld (ix+2),a
	;
	;ld a,(de)
	;ld c,a
	;srl c
	;srl c
	;ld a,(hl)
	;srl a
	;srl a
	;ld (ix+1),a
	;sub a,c
	;ld (ix),a
	;
	;lea ix,ix+6
	;inc hl
	;inc de
	;djnz initTransitionLoop
	
	
	;ld hl,mpLcdPalette
	;ld (TRS_paletteDST),hl
	;ld hl,_tiles_pal_violet
	;ld (TRS_paletteSRC),hl
	;set FLG_colorTRS, (iy+asm_flag2)
	;jp rsuit

;palette1TransitionPalette2:
	;ld a,(TRS_counter)
	;dec a
	;push af
	;ex af,af'
	;ld bc,0
	;ld (pixelshadow),sp
	;ld hl,TRS_paletteSRC
	;ld iy,mpLcdPalette
;transitionLoop:
	;ld a,(hl)
	;ld e,a
	;ex af,af'
	;ld d,a
	;ex af,af'
	;mlt de
	;ld a,e
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;ld e,a
	;inc hl
	;ld a,(hl)
	;sub a,e
	;or a
	;rla
	;rla
	;ld ixh,a
	;inc hl
	;
	;ld de,(hl)
	;mlt de
	;ld a,e
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;ld e,a
	;inc hl
	;ld a,(hl)
	;sub a,e
	;ld e,a
	;sla e
	;sla e
	;sla e
	;sla e
	;sla e
	;ld ixl,e
	;srl a
	;srl a
	;srl a
	;srl a
	;srl a
	;srl a
	;or a,ixh
	;ld ixh,a
	;inc hl
	;
	;ld de,(hl)
	;mlt de
	;ld a,e
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;srl d
	;rra
	;ld e,a
	;inc hl
	;ld a,(hl)
	;sub a,e
	;or a,ixl
	;ld ixl,a
	;inc hl
	;
	;ld a,ixl
	;ld (iy),a
	;inc iy
	;ld a,ixh
	;ld (iy),a
	;inc iy
	;dec b
	;jp nz,transitionLoop
	;
	;ld iy,flags
	;pop af
	;ret nz
	;
	;res FLG_colorTRS, (iy+asm_flag2)
	
	;ld hl,(TRS_paletteDST)
	;ld de,(TRS_paletteSRC)
	;ld a,(de)
	;ld (hl),a
	;inc hl
	;inc de
	;ld a,(de)
	;ld (hl),a
	;inc hl
	;inc de
	;ld (TRS_paletteDST),hl
	;ld (TRS_paletteSRC),de
	;ld de,mpLcdPalette+(256*2)
	;sbc hl,de
	;ret nz
	;res FLG_colorTRS, (iy+asm_flag2)

	;ret


NORM_gravity:
	bit FLG_jumping, (iy+asm_flag1)
	jp z,NORM_endJumping
	ld hl,(spr_state)
	inc hl
	inc hl
	inc hl
	ld de,_jump_tiles+(11*3)
	ex de,hl
	sbc hl,de
	jr nz,NORM_notLastState
	ld de,_jump_tiles
NORM_notLastState:
	ld (spr_state),de

	bit FLG_botReached, (iy+asm_flag1)
	jr z,NORM_botNotReached
	ld hl,(Vcharacter_posY)
	ld de,(mn) ; value to add (end of scrollDown, Down reached)
	add hl,de
	ld (Vcharacter_posY),hl
	jp NORM_noVerticalScroll
NORM_botNotReached:
	ld hl,(jmp_speed)
	inc hl
	inc hl
	inc hl
	ld (jmp_speed),hl
	ld bc,(hl)
	ld de,(Vcharacter_posY)
	ld hl,320*40
	or a
	sbc hl,de
	jr c,NORM_checkScrollDwn
	bit FLG_topReached, (iy+asm_flag1)
	jr z,NORM_topNotReached
	ld hl,(mn) ; value to add (end of scrollUp, top reached)
	add hl,de
	ld (Vcharacter_posY),hl
	jr NORM_noVerticalScroll
NORM_topNotReached:
	push de
	ld hl,(jmp_speed)
	ld de,jmpSpeedLUT_0
	or a
	sbc hl,de
	pop de
	jp p,NORM_notNearTop
	ld a,(bytesToSkip+1)
	or a
	jr nz,NORM_noVerticalScroll
	ld a,(dispBlkFrmY)
	or a
	jr nz,NORM_noVerticalScroll
	jr NORM_notNearTop
NORM_checkScrollDwn:
	bit FLG_jumping, (iy+asm_flag1)
	jr z,NORM_notNearTop
	ld hl,320*140
	or a
	sbc hl,de
	jr nc,NORM_notNearTop
	
	push de
	ld hl,(jmp_speed)
	ld de,jmpSpeedLUT_0
	or a
	sbc hl,de
	pop de
	jp m,NORM_notNearTop

	push de
	ld hl,(bytesToSkip)
	ld de,(maxBytesToSkip)
	or a
	sbc hl,de
	pop de
	jr nz,NORM_noVerticalScroll
	ld a,(dispBlkFrmY)
	or a
	jr nz,NORM_noVerticalScroll
NORM_notNearTop:
	ex de,hl
	add hl,bc
	ld (Vcharacter_posY),hl
NORM_noVerticalScroll:
	; Jump Tile?
	ld bc,(Vcharacter_posY)
	ld hl,(currentDrawLoc)
	add hl,bc
	push hl
	ld de,beg_sprite+(320*9)+1 ; bottom left hand corner
	add hl,de
	ld a,(hl)
	cp 07Eh ; yellow
	jr c,NORM_jmpTile_jumping
	ld de,10
	add hl,de
	ld a,(hl)
	cp 07Eh ; yellow
	jr c,NORM_jmpTile_jumping
	ld de,10
	add hl,de
	ld a,(hl)
	cp 07Eh
	jr c,NORM_jmpTile_jumping
	ld de,(320*10)-20 ; bottom left hand corner
	add hl,de
	ld a,(hl)
	cp 07Eh ; yellow
	jr c,NORM_jmpTile_jumping
	ld de,10
	add hl,de
	ld a,(hl)
	cp 07Eh ; yellow
	jr c,NORM_jmpTile_jumping
	ld de,10
	add hl,de
	ld a,(hl)
	cp 07Eh
	jr nc,NORM_noJmpTile_jumping
NORM_jmpTile_jumping:
	cp 7Ah
	jr c,NORM_noJmpTile_jumping
	
	ld a,($F50012)
	bit 5, a
	jr z,NORM_noJmpTile_jumping
	bit FLG_jmpUsed, (iy+asm_flag2)
	jr nz,NORM_noJmpTile_jumping

	res FLG_jmpAgain, (iy+asm_flag1)
	set FLG_jumping, (iy+asm_flag1)
	set FLG_jmpUsed, (iy+asm_flag2)
	ld hl,jmpSpeedLUT
	ld (jmp_speed),hl
	ld hl,0
	ld (mn),hl
	set FLG_topReached, (iy+asm_flag1)
	pop hl
	jp dispCharacter
NORM_noJmpTile_jumping:
	;	Collisions (if jumping)
	pop hl
	ld de,beg_sprite+3
	add hl,de
	ld a,(hl) ; top left hand corner
	cp BGCl
	jp nz,die
	ld de,(13*320)+17 ; right side
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,NORM_noAmbiguity
	push hl
	dec hl
	dec hl
	dec hl
	dec hl
	ld b,a
	ld a,(hl)
	cp BGCl
	pop hl
	ld a,b
	jp z,die
NORM_noAmbiguity:
	ld de,(6*320)-6 ; bottom right hand corner
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,++_
	cp 07Ah
	jr c,+_
	cp 07Eh
	jr c,++_
	cp 0FAh
	jr nc,++_
_:
	ex de,hl
	or a
	sbc hl,hl
	add hl,de
	dec de
	dec de
	dec de
	dec de
	ld a,(de)
	cp BGCl
	jp z,die
	ld ix,-(beg_sprite+(19*320)+14)
	jr NORM_touchedRight
_:
	ld de,2*320+4 ; bottom right hand corner
	add hl,de
	ld ix,-(beg_sprite+(21*320)+18)
NORM_touchedRight:
	ld a,(hl)
	cp BGCl
	jp z,NORM_endJumping
	cp 80h
	jp nc,die
	cp 07Ah
	jr c,NORM_touchedRight_adjustPosition_suit
	cp 07Eh
	jr nz,NORM_endJumping
NORM_touchedRight_adjustPosition_suit:
	res FLG_jumping, (iy+asm_flag1)
	set FLG_botReached, (iy+asm_flag1) ; synch with other buffer
	ld de,-320
NORM_touchedRight_adjustPosition:
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,NORM_touchedRight_adjustPosition
	lea de,ix+0
	add hl,de
	ld de,(currentDrawLoc)
	or a
	sbc hl,de
	ld (Vcharacter_posY),hl
	ld hl,_jump_tiles
	ld (spr_state),hl
NORM_endJumping:
	; Jump Tile?
	ld bc,(Vcharacter_posY)
	ld hl,(currentDrawLoc)
	add hl,bc
	push hl
	ld de,beg_sprite+(320*20)+20 ; bottom right hand corner
	add hl,de
	ld a,(hl)
	cp 0FAh ; yellow
	jr nc,NORM_jmpTile
	ld de,-320*2
	add hl,de
	ld a,(hl)
	cp 0FAh
	jr nc,NORM_jmpTile
	ld de,-320*3-10
	add hl,de
	ld a,(hl)
	cp 0FAh
	jr nc,NORM_jmpTile
	ld de,-320*1-5
	add hl,de
	ld a,(hl)
	cp 0FAh
	jr nc,NORM_jmpTile
	ld de,-320*2-5
	add hl,de
	ld a,(hl)
	cp 0FAh
	jr c,NORM_noJmpTile
NORM_jmpTile:
	res FLG_jmpUsed, (iy+asm_flag2)
	set FLG_jumping, (iy+asm_flag1)
	ld hl,jmpSpeedLUT-15
	ld (jmp_speed),hl
	set FLG_topReached, (iy+asm_flag1)
	ld hl,0
	ld (mn),hl
	pop hl
	jp dispCharacter
NORM_noJmpTile:
	;	Collisions
	pop hl
	ld de,beg_sprite+(320*2)+20 ; top right hand corner
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,die
	ld de,320*8 ; right side (center of the square)
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,die
	ld de,320*8-4 ; right side (center of the square)
	add hl,de
	ld a,(hl)
	cp 80h
	jp nc,die
	cp 7Ah
	jr nc,+_
	ex de,hl
	or a
	sbc hl,hl
	add hl,de
	dec de
	dec de
	dec de
	dec de
	ld a,(de)
	cp BGCl
	jp z,die
_:
	ld de,4*320-7 ; bottom side
	add hl,de
	ld a,(hl)
	cp 80h
	jp nc,die
	ld de,-6 ; bottom left hand corner
	add hl,de
	ld a,(hl)
	cp 80h
	jp nc,die
	dec hl
	dec hl
	dec hl
	ld a,(hl)
	cp BGCl
	jr nz,NORM_notFalling
	bit FLG_jumping, (iy+asm_flag1)
	jp nz,dispCharacter ; if we're already falling...
	set FLG_jumping, (iy+asm_flag1)
	ld hl,jmpSpeedLUT_0+3
	ld (jmp_speed),hl
	jp dispCharacter
NORM_notFalling:
	cp 07Ah
	jr c,NORM_touchedLeft_adjustPosition_suit
	cp 07Eh
	jp nz,dispCharacter
NORM_touchedLeft_adjustPosition_suit:
	bit FLG_jumping, (iy+asm_flag1)
	jp z,dispCharacter
	res FLG_jumping, (iy+asm_flag1)
	set FLG_botReached, (iy+asm_flag1) ; synch with other buffer
	ld de,-320
NORM_touchedLeft_adjustPosition:
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,NORM_touchedLeft_adjustPosition
	ld de,-(beg_sprite+20+(320*11)+(9*320)-11-9)
	add hl,de
	ld de,(currentDrawLoc)
	or a
	sbc hl,de
	ld (Vcharacter_posY),hl
	ld hl,jmpSpeedLUT
	ld (jmp_speed),hl
	ld hl,_jump_tiles
	ld (spr_state),hl
	jp dispCharacter


RVRS_gravity:
	bit FLG_jumping, (iy+asm_flag1)
	jp z,RVRS_endJumping
	ld hl,(spr_state)
	dec hl
	dec hl
	dec hl
	ld de,_jump_tiles-3
	ex de,hl
	or a
	sbc hl,de
	jr nz,RVRS_notLastState
	ld de,_jump_tiles+(10*3)
RVRS_notLastState:
	ld (spr_state),de

	bit FLG_topReached, (iy+asm_flag1)
	jr nz,RVRS_topBotReached
	bit FLG_botReached, (iy+asm_flag1)
	jr z,RVRS_botNotReached
RVRS_topBotReached:
	ld hl,(Vcharacter_posY)
	ld de,(mn) ; value to add (end of scroll, Down or Top reached)
	add hl,de
	ld (Vcharacter_posY),hl
	jp RVRS_noVerticalScroll
RVRS_botNotReached:

	ld hl,(jmp_speed)
	dec hl
	dec hl
	dec hl
	ld (jmp_speed),hl
	ld bc,(hl)
	ld de,(Vcharacter_posY)
	ld hl,320*40
	or a
	sbc hl,de
	jr c,RVRS_checkScrollDwn
	push de
	ld hl,(jmp_speed)
	ld de,jmpSpeedLUT_0
	sbc hl,de
	pop de
	jp p,RVRS_notNearTop
	ld a,(bytesToSkip+1)
	or a
	jr nz,RVRS_noVerticalScroll
	ld a,(dispBlkFrmY)
	or a
	jr nz,RVRS_noVerticalScroll
	jr RVRS_notNearTop
RVRS_checkScrollDwn:
	bit FLG_jumping, (iy+asm_flag1)
	jr z,RVRS_notNearTop
	ld hl,320*140
	or a
	sbc hl,de
	jr nc,RVRS_notNearTop
	push de
	ld hl,(jmp_speed)
	ld de,jmpSpeedLUT_0
	or a
	sbc hl,de
	pop de
	jp m,RVRS_notNearTop
	push de
	ld hl,(bytesToSkip)
	ld de,(maxBytesToSkip)
	or a
	sbc hl,de
	pop de
	jr nz,RVRS_noVerticalScroll
	ld a,(dispBlkFrmY)
	or a
	jr nz,RVRS_noVerticalScroll
RVRS_notNearTop:
	ld hl,(Vcharacter_posY)
	add hl,bc
	ld (Vcharacter_posY),hl
RVRS_noVerticalScroll:
	; Jump Tile?
	ld bc,(Vcharacter_posY)
	ld hl,(currentDrawLoc)
	add hl,bc
	push hl
	ld de,beg_sprite+(320*2)+1 ; bottom left hand corner
	add hl,de
	ld a,(hl)
	cp 07Eh ; yellow
	jr c,RVRS_jmpTile_jumping
	ld de,10
	add hl,de
	ld a,(hl)
	cp 07Eh ; yellow
	jr c,RVRS_jmpTile_jumping
	ld de,10
	add hl,de
	ld a,(hl)
	cp 07Eh
	jr c,RVRS_jmpTile_jumping
	ld de,(320*10)-20 ; bottom left hand corner
	add hl,de
	ld a,(hl)
	cp 07Eh ; yellow
	jr c,RVRS_jmpTile_jumping
	ld de,10
	add hl,de
	ld a,(hl)
	cp 07Eh ; yellow
	jr c,RVRS_jmpTile_jumping
	ld de,10
	add hl,de
	ld a,(hl)
	cp 07Eh
	jr nc,RVRS_noJmpTile_jumping
RVRS_jmpTile_jumping:
	cp 7Ah
	jr c,RVRS_noJmpTile_jumping
	bit FLG_jmpAgain, (iy+asm_flag1)
	jr z,RVRS_noJmpTile_jumping

	res FLG_jmpAgain, (iy+asm_flag1)
	set FLG_jumping, (iy+asm_flag1)
	ld hl,jmpSpeedLUT_rvrs
	ld (jmp_speed),hl
	ld hl,0
	ld (mn),hl
	set FLG_botReached, (iy+asm_flag1)
	pop hl
	jp dispCharacter
RVRS_noJmpTile_jumping:
	;	Collisions (if jumping)
	pop hl
	ld de,beg_sprite+(320*20)
	add hl,de
	ld a,(hl) ; top left hand corner
	cp BGCl
	jp nz,die
	ld de,-(14*320)+18 ; bottom right hand corner
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,RVRS_noAmbiguity
	push hl
	dec hl
	dec hl
	dec hl
	dec hl
	ld b,a
	ld a,(hl)
	cp BGCl
	pop hl
	ld a,b
	jp z,die
RVRS_noAmbiguity:
	ld de,-(7*320) ; bottom right hand corner
	add hl,de
	ld a,(hl)
	cp 80h
	jp nc,die
	cp BGCl
	jp z,RVRS_endJumping
	cp 07Ah
	jr c,RVRS_touchedRight_adjustPosition_suit
	cp 07Eh
	jr nz,RVRS_endJumping
RVRS_touchedRight_adjustPosition_suit:
	res FLG_jumping, (iy+asm_flag1)
	set FLG_topReached, (iy+asm_flag1) ; synch with other buffer
	ld de,320
RVRS_touchedRight_adjustPosition:
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,RVRS_touchedRight_adjustPosition
	ld de,-(beg_sprite+18)
	add hl,de
	ld de,(currentDrawLoc)
	or a
	sbc hl,de
	ld (Vcharacter_posY),hl
	ld hl,_jump_tiles
	ld (spr_state),hl
RVRS_endJumping:
	; Jump Tile?
	ld bc,(Vcharacter_posY)
	ld hl,(currentDrawLoc)
	add hl,bc
	push hl
	ld de,beg_sprite+(320*0)+15 ; bottom right hand corner
	add hl,de
	ld a,(hl)
	cp 0FAh ; yellow
	jr nc,RVRS_jmpTile
	ld de,(320*2)-5
	add hl,de
	ld a,(hl)
	cp 0FAh
	jr c,RVRS_noJmpTile
RVRS_jmpTile:
	set FLG_jumping, (iy+asm_flag1)
	ld hl,jmpSpeedLUT_rvrs+15
	ld (jmp_speed),hl
	ld hl,0
	ld (mn),hl
	set FLG_botReached, (iy+asm_flag1)
	pop hl
	jp dispCharacter
RVRS_noJmpTile:
	;	Collisions
	pop hl
	ld de,beg_sprite+(19*320)+20 ; top right hand corner
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,die
	ld de,-320*10 ; right side (center of the square)
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,die
	ld de,-12*320-11 ; bottom side
	add hl,de
	ld a,(hl)
	cp 80h
	jp nc,die
	ld de,-6 ; bottom left hand corner
	add hl,de
	ld a,(hl)
	cp 80h
	jp nc,die
	dec hl
	dec hl
	dec hl
	ld a,(hl)
	cp BGCl
	jr nz,RVRS_notFalling
	bit FLG_jumping, (iy+asm_flag1)
	jp nz,dispCharacter ; if we're already falling...
	set FLG_jumping, (iy+asm_flag1)
	ld hl,jmpSpeedLUT_0
	ld (jmp_speed),hl
	jp dispCharacter
RVRS_notFalling:
	cp 07Ah
	jr c,RVRS_touchedLeft_adjustPosition_suit
	cp 07Eh
	jp nz,dispCharacter
RVRS_touchedLeft_adjustPosition_suit:
	bit FLG_jumping, (iy+asm_flag1)
	jp z,dispCharacter
	res FLG_jumping, (iy+asm_flag1)
	set FLG_topReached, (iy+asm_flag1) ; synch with other buffer
	ld de,320
RVRS_touchedLeft_adjustPosition:
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,RVRS_touchedLeft_adjustPosition
	ld de,-beg_sprite
	add hl,de
	ld de,(currentDrawLoc)
	or a
	sbc hl,de
	ld (Vcharacter_posY),hl
	ld hl,jmpSpeedLUT_0
	ld (jmp_speed),hl
	ld hl,_jump_tiles
	ld (spr_state),hl
	jp dispCharacter


scrollUp:
	ld hl,(jmp_speed)
	ld de,(hl)
	ld hl,0
	or a
	sbc hl,de
	ld bc,-320
	ld e,-1
	inc e
	add hl,bc
	jr c,$-2
	ld a,(dispBlkFrmY)
	sub a,e
	ld d,a
	jr nz,SCRUP_noVerif
	; checking if the top has been reached
	ld hl,(bytesToSkip)
	ld bc,0
	or a
	sbc hl,bc
	jr nz,SCRUP_noVerif
	ld (mn),hl
	set FLG_topReached, (iy+asm_flag1)
	jp SCRUP_noOverflowY
SCRUP_noVerif:
	jp nc,SCRUP_noOverflowY

	ld hl,(bytesToSkip)
	ld bc,0
	or a
	sbc hl,bc
	adc hl,bc
	jp nz,SCRUP_noMapOverflowY

	;sub a,d
	dec hl
	ld l,d
	; hl*320
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	push hl
	pop de
	add hl,hl
	add hl,hl
	add hl,de
	ld (mn),hl ; we'll update Vcharacter_posY later with this value (+ old Vcharacter_posY)

	ld a,(dispBlkFrmY)
	ld (prv_spdDiv320),a
	ld b,a
	xor a
	sub a,b
	ld hl,$FFFFFF
	ld l,a
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	push hl
	pop de
	add hl,hl
	add hl,hl
	add hl,de
	call SPHP_adjustTailElsAfterScroll
	ld de,(prev_speed)
	ld (prev_speed),hl
	add hl,de
	ld de,0
	or a
	sbc hl,de
	jp z,onlyScrollRight
	push hl
	ld de,MAP_sizeY*TIL_sizeY*320-1
	add hl,de
	push hl
	pop bc
	ld hl,(currentDrawLoc)
	add hl,bc
	ld de,SCROLL_spd
	add hl,de
	push hl
	ld hl,(currentDrawLoc)
	ld de,MAP_sizeY*TIL_sizeY*320-1
	add hl,de
	ex de,hl
	pop hl
	lddr

	pop hl
	ld de,320
	ld c,0
	inc c
	add hl,de
	jr nc,$-2
	ld ix,(fstBlock)
	ld de,-WIN_sizeX+1
	add ix,de
	ld a,(dispBlkFrmX)
	ld h,320-(21*14)-8
	sub a,h
	jr c,SCRUP_TOP_IXSet
	inc ix
SCRUP_TOP_IXSet:
	ld a,c
	exx
	ld c,a
	ld b,0
	exx
	ld hl,(currentDrawLoc)
	ld a,WIN_sizeX
	call SCRUP_NEW_dispTilesVertScroll

	xor a
	ld (dispBlkFrmY),a
	set FLG_topReached, (iy+asm_flag1) ; synch with other buffer
	jp afterScroll

SCRUP_noMapOverflowY:
	ld b,TIL_sizeY
	add a,b
	ld hl,(fstBlock)
	ld bc,(MAP_sizeX)
	or a
	sbc hl,bc
	ld (fstBlock),hl
	ld hl,(bytesToSkip)
	sbc hl,bc
	ld (bytesToSkip),hl
SCRUP_noOverflowY:
	ld (dispBlkFrmY),a
	ld a,e

	ld hl,(jmp_speed)
	ld hl,(hl)
	call SPHP_adjustTailElsAfterScroll
	ld de,(prev_speed)
	ld (prev_speed),hl
	add hl,de
	ld de,0
	or a
	sbc hl,de
	jp z,onlyScrollRight
	push af
	push hl
	ex de,hl
	ld hl,0
	or a
	sbc hl,de
	ex de,hl
	ld hl,MAP_sizeY*TIL_sizeY*320
	or a
	sbc hl,de
	push hl
	pop bc
	ld hl,(currentDrawLoc)
	ld de,MAP_sizeY*TIL_sizeY*320
	add hl,de
	dec hl
	pop de
	push hl
	add hl,de
	ld de,SCROLL_spd
	add hl,de
	pop de
	lddr
	
	; disp tiles scrolled
	pop af
	ld ix,(fstBlock)
	ld bc,-WIN_sizeX+1
	add ix,bc
	ld c,a
	ld a,(dispBlkFrmX)
	ld h,320-(21*14)-8
	sub a,h
	jr c,SCRUP_IXSet
	inc ix
SCRUP_IXSet:
	ld a,c
	ld de,(prv_spdDiv320)
	ld (prv_spdDiv320),a
	add a,e
	jp z,afterScroll
	ld e,a
	ld a,(dispBlkFrmY)
	ld c,a
	ld a,TIL_sizeX
	sub a,c
	cp e
	ld a,e
	jr c,SCRUP_afterDispTileNoOverflow
	exx
	ld c,a
	ld a,(dispBlkFrmY)
	ld b,a
	exx
	ld hl,(currentDrawLoc)
	ld a,WIN_sizeX
	call SCRUP_NEW_dispTilesVertScroll
	jp afterScroll
SCRUP_afterDispTileNoOverflow:
	push ix
	push af
	
	ld a,(dispBlkFrmY)
	ld c,a
	ld a,TIL_sizeX
	sub a,c
	push af
	exx
	ld c,a
	ld a,(dispBlkFrmY)
	ld b,a
	exx
	ld hl,(currentDrawLoc)
	ld a,WIN_sizeX
	call SCRUP_NEW_dispTilesVertScroll

	pop hl
	pop af
	pop ix
	sub a,h
	exx
	ld c,a
	ld b,0 ; dispBlkFrmY
	exx
	or a
	jp z,afterScroll
	ld l,h
	ld h,0
	; hl*320
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	push hl
	pop de
	add hl,hl
	add hl,hl
	add hl,de
	ld de,(currentDrawLoc)
	add hl,de
	ld de,(MAP_sizeX)
	add ix,de
	ld a,WIN_sizeX
	call SCRUP_NEW_dispTilesVertScroll
	jp afterScroll

SCRUP_scrollUpSyncOtherBuf:
	res FLG_topReached, (iy+asm_flag1)
	ld a,(prv_spdDiv320)
	or a
	jp z,onlyScrollRight
	
	; fixes a bug (try to scroll up when falling)
	ld hl,(Vcharacter_posY)
	ld de,320*110
	sbc hl,de
	jp nc,SCRDW_scrollDownSyncOtherBuf
	
	ld hl,(prev_speed)
	ld de,MAP_sizeY*TIL_sizeY*320-1
	add hl,de
	push hl
	pop bc
	ld hl,(currentDrawLoc)
	add hl,bc
	ld de,SCROLL_spd
	add hl,de
	push hl
	ld hl,(currentDrawLoc)
	ld de,MAP_sizeY*TIL_sizeY*320-1
	add hl,de
	ex de,hl
	pop hl
	lddr

	ld ix,(fstBlock)
	ld de,-WIN_sizeX+1
	add ix,de
	ld a,(dispBlkFrmX)
	ld c,320-(21*14)-8
	sub a,c
	jr c,SCRUP_SYNC_IXSet
	inc ix
SCRUP_SYNC_IXSet:
	ld a,(prv_spdDiv320)
	exx
	ld c,a
	ld b,0
	exx
	ld hl,(currentDrawLoc)
	ld a,WIN_sizeX
	call SCRUP_NEW_dispTilesVertScroll

	ld hl,0
	ld (prev_speed),hl
	ld (prv_spdDiv320),hl
	res FLG_topReached, (iy+asm_flag1)
	jp afterScroll

SCRUP_NEW_dispTilesVertScroll:
	; Paramètres :
	;	A = ?
	;	Shadow B = ?
	;	Shadow C = ?
	ex af,af' 
	exx
	ld a,c
	exx
	or a	; on check que shadow c!=0
	ret z
	ex af,af'

	push ix
	push af
	push hl
	push hl
	ld b,a
	ld a,(ix)
	ld e,a
	sla a
	add a,e
	ld de,0
	ld e,a
	ld hl,_GD_tiles
	add hl,de
	ld hl,(hl)
	exx
	ld a,b ; dispBlkFrmY
	exx
	ld e,a
	ld d,TIL_sizeX
	mlt de
	add hl,de
	pop de
	exx
	ld a,c
	exx
	ex af,af'
	ld iy,0
	ld ix,TIL_sizeX
	ld a,b
	dec a
	jr nz,SCRUP_notLastTile
	push hl
	ld hl,(currentDrawLoc)
	ld bc,320
	add hl,bc
	sbc hl,de
	jr nc,SCRUP_onlyOneTileDisp
	add hl,bc
	jr nc,$-1
SCRUP_onlyOneTileDisp:
	push hl
	pop ix
	lea bc,ix+0
	ld a,TIL_sizeX
	sub a,c
	jr nc,SCRUP_notTooFar
	xor a
	ld ix,TIL_sizeX
SCRUP_notTooFar:
	ld c,a
	push bc
	pop iy
	pop hl
	or a
	sbc hl,bc
	jr SCRUP_NEW_dispTilesVertScrollY-1
SCRUP_notLastTile:
	cp WIN_sizeX-1
	jr nz,SCRUP_NEW_dispTilesVertScrollY-1
	ld a,(dispBlkFrmX)
	ld b,a
	ld a,320-(14*TIL_sizeX+8)
	sub a,b
	jr z,SCRUP_fstTileEqu0
	jr nc,SCRUP_oneTileDisplayed
SCRUP_fstTileEqu0:
	ld b,TIL_sizeX
	add a,b
SCRUP_oneTileDisplayed:
	ld bc,0
	ld c,a
	push bc
	pop ix
	ld a,TIL_sizeX
	sub a,c
	ld c,a
	push bc
	pop iy
	ex af,af'
SCRUP_NEW_dispTilesVertScrollY:
	push de
	lea bc,iy+0
	add hl,bc
	lea bc,ix+0
	ldir
	pop de
	push hl
	ld hl,320
	add hl,de
	ex de,hl
	pop hl
	dec a
	jr nz,SCRUP_NEW_dispTilesVertScrollY

	pop hl
	lea de,ix+0
	add hl,de
	pop af
	pop ix
	inc ix
	dec a
	jp nz,SCRUP_NEW_dispTilesVertScroll
	ld iy,flags
	ret


scrollDown:
	ld hl,(jmp_speed)
	ld hl,(hl)
	ld bc,-320
	ld e,-1
	inc e
	add hl,bc
	jr c,$-2
	ld a,(dispBlkFrmY)
	add a,e
	ld d,a
	cp TIL_sizeY
	jr nz,SCRDW_noVerif
	; checking if the floor has been reached
	ld hl,(bytesToSkip)
	ld bc,(maxBytesToSkip)
	or a
	sbc hl,bc
	jp nz,SCRDW_noVerif
	ld (mn),hl
	set FLG_botReached, (iy+asm_flag1)
	jp SCRDW_noOverflowY
SCRDW_noVerif:
	jp c,SCRDW_noOverflowY

	ld hl,(bytesToSkip)
	ld bc,(MAP_sizeX)
	add hl,bc
	ld bc,(maxBytesToSkip)
	sbc hl,bc
	jp nz,SCRDW_noMapOverflowY
	ld (bytesToSkip),bc

	ld hl,(fstBlock)
	ld bc,(MAP_sizeX)
	add hl,bc
	ld (fstBlock),hl

	ld hl,0
	ld d,TIL_sizeX
	sub a,d
	ld l,a
	; hl*320
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ex de,hl
	sbc hl,hl
	add hl,de
	add hl,hl
	add hl,hl
	add hl,de
	ld (mn),hl ; it'll update Vcharacter_posY later with this value (+ old Vcharacter_posY)

	ld a,(prv_spdDiv320)
	push af
	ld a,(dispBlkFrmY)
	ld b,a
	ld a,TIL_sizeY
	sub a,b
	ld (prv_spdDiv320),a
	push af
	ld hl,0
	ld l,a
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ex de,hl
	sbc hl,hl
	add hl,de
	add hl,hl
	add hl,hl
	add hl,de
	call SPHP_adjustTailElsAfterScroll
	ld de,(prev_speed)
	ld (prev_speed),hl
	add hl,de
	push hl
	ex de,hl
	ld hl,320*(MAP_sizeY*TIL_sizeY)
	or a
	sbc hl,de
	push hl
	pop bc
	pop hl
	ld de,(currentDrawLoc)
	push de
	add hl,de
	ld de,SCROLL_spd
	add hl,de
	pop de
	dec bc
	ldir

	ld ix,(fstBlock)
	ld de,-WIN_sizeX+1
	add ix,de
	ld de,(MAP_sizeX)
	; HL*9
	or a
	sbc hl,hl
	add hl,de
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,de
	ex de,hl
	add ix,de
	ld a,(dispBlkFrmX)
	ld h,320-(21*14)-8
	sub a,h
	jr c,SCRDW_TOP_IXSet
	inc ix
SCRDW_TOP_IXSet:
	pop af
	pop de
	add a,d
	exx
	ld c,a
	ld b,TIL_sizeY-1
	exx
	ld hl,(currentDrawLoc)
	ld de,320*(TIL_sizeY*MAP_sizeY-1)
	add hl,de
	ld a,WIN_sizeX
	call SCRDW_NEW_dispTilesVertScroll

	xor a
	ld (dispBlkFrmY),a
	set FLG_botReached, (iy+asm_flag1) ; synch with other buffer
	jp afterScroll

SCRDW_noMapOverflowY:
	ld b,TIL_sizeY
	sub a,b
	ld hl,(fstBlock)
	ld bc,(MAP_sizeX)
	add hl,bc
	ld (fstBlock),hl
	ld hl,(bytesToSkip)
	add hl,bc
	ld (bytesToSkip),hl
SCRDW_noOverflowY:
	ld (dispBlkFrmY),a
	ld a,e

	ld hl,(jmp_speed)
	ld hl,(hl)
	call SPHP_adjustTailElsAfterScroll
	ld de,(prev_speed)
	ld (prev_speed),hl
	add hl,de
	ld de,0
	or a
	sbc hl,de
	jp z,onlyScrollRight
	push af
	push hl
	ex de,hl
	ld hl,320*(MAP_sizeY*TIL_sizeY)
	or a
	sbc hl,de
	push hl
	pop bc
	pop hl
	ld de,(currentDrawLoc)
	push de
	add hl,de
	ld de,SCROLL_spd
	add hl,de
	pop de
	dec bc
	ldir
	
	; disp tiles scrolled
	pop af
	ld ix,(fstBlock)
	ld bc,-WIN_sizeX+1
	add ix,bc
	ld hl,(MAP_sizeX)
	; HL*10
	add hl,hl
	ex de,hl
	sbc hl,hl
	add hl,de
	add hl,hl
	add hl,hl
	add hl,de
	ex de,hl
	add ix,de
	ld c,a
	ld a,(dispBlkFrmX)
	ld h,320-(21*14)-8
	sub a,h
	jr c,SCRDW_IXSet
	inc ix
SCRDW_IXSet:
	ld a,c
	ld de,(prv_spdDiv320)
	ld (prv_spdDiv320),a
	add a,e
	jp z,afterScroll
	ld e,a
	ld a,(dispBlkFrmY)
	cp e
	ld a,e
	jr c,SCRDW_afterDispTileNoOverflow
	exx
	ld c,a
	ld a,(dispBlkFrmY)
	dec a
	ld b,a
	exx
	ld hl,(currentDrawLoc)
	ld de,320*(TIL_sizeY*MAP_sizeY-1)
	add hl,de
	ld a,WIN_sizeX
	call SCRDW_NEW_dispTilesVertScroll
	jp afterScroll
SCRDW_afterDispTileNoOverflow:
	push ix
	push af
	
	ld a,(dispBlkFrmY)
	push af
	exx
	ld c,a
	dec a
	ld b,a
	exx
	ld hl,(currentDrawLoc)
	ld de,320*(TIL_sizeY*MAP_sizeY-1)
	add hl,de
	ld a,WIN_sizeX
	call SCRDW_NEW_dispTilesVertScroll

	pop hl
	pop af
	pop ix
	sub a,h
	exx
	ld c,a
	ld b,TIL_sizeY-1 ; dispBlkFrmY
	exx
	or a
	jp z,afterScroll
	ld l,h
	ld h,0
	; hl*320
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ex de,hl
	sbc hl,hl
	add hl,de
	add hl,hl
	add hl,hl
	add hl,de
	ex de,hl
	ld hl,(currentDrawLoc)
	sbc hl,de
	ld de,320*(TIL_sizeY*MAP_sizeY-1)
	add hl,de

	lea de,ix+0
	ex de,hl
	ld bc,(MAP_sizeX) ; IX=IX-(MAP_sizeX)
	sbc hl,bc
	ex de,hl
	ld ix,0
	add ix,de

	ld a,WIN_sizeX
	call SCRDW_NEW_dispTilesVertScroll
	jp afterScroll

SCRDW_scrollDownSyncOtherBuf:
	res FLG_botReached, (iy+asm_flag1)
	ld a,(prv_spdDiv320)
	or a
	jp z,onlyScrollRight

	; fixes a bug (try to scroll up when falling)
	ld hl,(Vcharacter_posY)
	ld de,320*70
	sbc hl,de
	jp c,SCRUP_scrollUpSyncOtherBuf
	
	ld de,(prev_speed)
	push de
	ld hl,320*(MAP_sizeY*TIL_sizeY)
	sbc hl,de
	push hl
	pop bc
	pop de
	ld hl,(currentDrawLoc)
	push hl
	add hl,de
	ld de,SCROLL_spd
	add hl,de
	pop de
	dec bc
	ldir

	ld ix,(fstBlock)
	ld de,-WIN_sizeX+1
	add ix,de
	ld hl,(MAP_sizeX)
	; HL*10
	add hl,hl
	ex de,hl
	sbc hl,hl
	add hl,de
	add hl,hl
	add hl,hl
	add hl,de
	ex de,hl
	add ix,de
	ld a,(dispBlkFrmX)
	ld h,320-(21*14)-8
	sub a,h
	jr c,SCRDW_SYNC_IXSet
	inc ix
SCRDW_SYNC_IXSet:
	ld a,(prv_spdDiv320)
	ld e,a
	ld a,(dispBlkFrmY)
	or a
	jr z,SCRDW_SYNC_oneTileToDisp
	cp e
	jr c,SCRDW_SYNC_twoTilesToDisp
SCRDW_SYNC_oneTileToDisp:
	ld a,e
	exx
	ld c,a
	ld a,(dispBlkFrmY)
	or a
	jr nz,groundNotTouched
	; on en profite pour monter d'une colonne si on synchronise car on a tout scrollé (car par défaut c'est plus bas si y'a eu aucun scroll)
	exx
	lea de,ix+0
	ex de,hl
	ld bc,(MAP_sizeX) ; IX=IX-(MAP_sizeX)
	sbc hl,bc
	ex de,hl
	ld ix,0
	add ix,de
	exx
	ld a,TIL_sizeY
groundNotTouched:
	dec a
	ld b,a
	exx
	ld hl,(currentDrawLoc)
	ld de,320*(TIL_sizeY*MAP_sizeY-1)
	add hl,de
	ld a,WIN_sizeX
	call SCRDW_NEW_dispTilesVertScroll

	ld hl,0
	ld (prev_speed),hl
	ld (prv_spdDiv320),hl
	jp afterScroll
SCRDW_SYNC_twoTilesToDisp:
	push ix
	ld a,(dispBlkFrmY)
	exx
	ld c,a
	dec a
	ld b,a
	exx
	ld hl,(currentDrawLoc)
	ld de,320*(TIL_sizeY*MAP_sizeY-1)
	add hl,de
	ld a,WIN_sizeX
	call SCRDW_NEW_dispTilesVertScroll
	pop ix

	ld hl,(dispBlkFrmY)
	; hl*320
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ex de,hl
	sbc hl,hl
	add hl,de
	add hl,hl
	add hl,hl
	add hl,de
	ex de,hl
	ld hl,(currentDrawLoc)
	sbc hl,de
	ld de,320*(TIL_sizeY*MAP_sizeY-1)
	add hl,de

	ld a,(dispBlkFrmY)
	ld e,a
	ld a,(prv_spdDiv320)
	sub a,e
	exx
	ld c,a
	ld b,TIL_sizeY-1
	exx

	lea de,ix+0
	ex de,hl
	ld bc,(MAP_sizeX) ; IX=IX-(MAP_sizeX)
	sbc hl,bc
	ex de,hl
	ld ix,0
	add ix,de

	ld a,WIN_sizeX
	call SCRDW_NEW_dispTilesVertScroll

	ld hl,0
	ld (prev_speed),hl
	ld (prv_spdDiv320),hl
	jp afterScroll

SCRDW_NEW_dispTilesVertScroll:
	; Paramètres :
	;	A = ?
	;	Shadow B = ?
	;	Shadow C = ?
	ex af,af'
	exx
	ld a,c
	exx
	or a
	ret z
	ex af,af'
	
	push ix
	push af
	push hl
	push hl
	ld b,a
	ld a,(ix)
	ld e,a
	sla a
	add a,e
	ld de,0
	ld e,a
	ld hl,_GD_tiles
	add hl,de
	ld hl,(hl)
	exx
	ld a,b ; dispBlkFrmY
	exx
	ld e,a
	ld d,TIL_sizeX
	mlt de
	add hl,de
	pop de
	exx
	ld a,c
	exx
	ex af,af'
	ld iy,0
	ld ix,TIL_sizeX
	ld a,b
	dec a
	jr nz,SCRDW_notLastTile

	push hl
	push de
	ld hl,(currentDrawLoc)
	ld bc,320*(TIL_sizeY*MAP_sizeY)
	add hl,bc
	ex de,hl
	sbc hl,de
	ld bc,320
	add hl,bc
	jr nc,$-1
SCRDW_onlyOneTileDisp:
	ex de,hl
	ld hl,320
	or a
	sbc hl,de
	push hl
	pop ix
	lea bc,ix+0
	ld a,TIL_sizeX
	sub a,c
	jr nc,SCRDW_notTooFar
	xor a
	ld ix,TIL_sizeX
SCRDW_notTooFar:
	ld c,a
	push bc
	pop iy
	pop de
	pop hl
	or a
	sbc hl,bc
	jr SCRDW_NEW_dispTilesVertScrollY-1
SCRDW_notLastTile:
	cp WIN_sizeX-1
	jr nz,SCRDW_NEW_dispTilesVertScrollY-1
	ld a,(dispBlkFrmX)
	ld b,a
	ld a,320-(14*TIL_sizeX+8)
	sub a,b
	jr z,SCRDW_fstTileEqu0pxl
	jr nc,SCRDW_oneTileDisplayed
SCRDW_fstTileEqu0pxl:
	ld b,TIL_sizeX
	add a,b
SCRDW_oneTileDisplayed:
	ld bc,0
	ld c,a
	push bc
	pop ix
	ld a,TIL_sizeX
	sub a,c
	ld c,a
	push bc
	pop iy
	ex af,af'
SCRDW_NEW_dispTilesVertScrollY:
	push de
	lea bc,iy+0
	add hl,bc
	lea bc,ix+0
	ldir
	pop de
	push hl
	ld hl,-320
	add hl,de
	ex de,hl
	pop hl
	ld bc,-TIL_sizeX*2
	add hl,bc
	dec a
	jr nz,SCRDW_NEW_dispTilesVertScrollY

	pop hl
	lea de,ix+0
	add hl,de
	pop af
	pop ix
	inc ix
	dec a
	jp nz,SCRDW_NEW_dispTilesVertScroll
	ld iy,flags
	ret

SPHP_adjustTailElsAfterScroll:
	; INPUT :
	;	HL
	bit FLG_spaceshipOn, (iy+asm_flag1)
	ret z
	push hl
	push de
	push bc
	push ix
	push af
	ex de,hl
	or a
	sbc hl,hl
	sbc hl,de
	ex de,hl
	ld hl,tailEffectBufPos+1
	ld b,TAIL_nb+2
addHL:
	ld ix,(hl)
	add ix,de
	ld (hl),ix
	inc hl
	inc hl
	inc hl
	inc hl
	djnz addHL
	pop af
	pop ix
	pop bc
	pop de
	pop hl
	ret


spaceshipOn:
	bit FLG_spaceshipOn, (iy+asm_flag1)
	ret nz
	
	;	Erasing previous square
	ld hl,(currentSprBuf)
	ld de,behindSprite1
	or a
	sbc hl,de
	jr nz,ON_SPHP_swap
	ld de,behindSprite2
ON_SPHP_swap:
	
	ld bc,(prev_posY)
	ld hl,(mpLcdBase)
	add hl,bc
	ex de,hl
	ld a,30
ON_SPHP_eraseSpriteY:
	ld bc,30
	ldir
	ex de,hl
	ld bc,320-30
	add hl,bc
	ex de,hl
	dec a
	jr nz,ON_SPHP_eraseSpriteY
	
	set FLG_alrdyErased, (iy+asm_flag1)
	ld hl,_spaceship_tiles
	ld (spr_state),hl
	ld hl,spcshpSpeedLUT
	ld (jmp_speed),hl
	ld hl,22
	ld (spriteSize),hl
	set FLG_spaceshipOn, (iy+asm_flag1)
	res FLG_jmpAgain, (iy+asm_flag1)
	ret
	
spaceshipOff:
	bit FLG_spaceshipOn, (iy+asm_flag1)
	jp z,afterChangingOfVehicle
	
	ld hl,(Vcharacter_posY)
	ld de,(currentDrawLoc)
	add hl,de
	ld de,beg_sprite+(SPR_sizeX*320)+10
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,OFF_adjustmtNotNeeded
	ld de,-320
OFF_adjustPosition:
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,OFF_adjustPosition
	ld de,-(beg_sprite+(SPR_sizeX*320)+10)
	add hl,de
	ld de,(currentDrawLoc)
	or a
	sbc hl,de
	ld (Vcharacter_posY),hl
OFF_adjustmtNotNeeded:
	
	;	Erasing previous square
	ld hl,(currentSprBuf)
	ld de,behindSprite1
	or a
	sbc hl,de
	jr nz,OFF_SPHP_swap
	ld de,behindSprite2
OFF_SPHP_swap:
	
	ld bc,(prev_posY)
	ld hl,(mpLcdBase)
	add hl,bc
	ex de,hl
	ld a,22
OFF_SPHP_eraseSpriteY:
	ld bc,22
	ldir
	ex de,hl
	ld bc,320-22
	add hl,bc
	ex de,hl
	dec a
	jr nz,OFF_SPHP_eraseSpriteY
	
	set FLG_alrdyErased, (iy+asm_flag1)
	ld hl,_jump_tiles
	ld (spr_state),hl
	ld hl,jmpSpeedLUT_0-3
	ld (jmp_speed),hl
	ld hl,30
	ld (spriteSize),hl
	res FLG_spaceshipOn, (iy+asm_flag1)
	jp afterChangingOfVehicle

SPCSHP_mode:
	ld de,(jmp_speed)
	bit FLG_jumping, (iy+asm_flag1)
	jp nz,SPCSHP_jmping
	ld hl,spcshpSpeedLUT_end
	or a
	sbc hl,de
	jr z,SPCSHP_suit
	inc de
	inc de
	inc de
	ld (jmp_speed),de
SPCSHP_suit:

	ex de,hl
	ld bc,(hl)
	ld hl,(Vcharacter_posY)
	add hl,bc
	ld (Vcharacter_posY),hl
	ld de,(currentDrawLoc)
	add hl,de

	; collisions
	ld de,(SPHP_sizeY/3)*320+SPHP_sizeX-3
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,+_
	ld de,-10
	add hl,de
	ld a,(hl)
	cp BGCl
	jp z,die	
_:
	ld de,(SPHP_sizeY/3)*320
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,+_
	ld de,-10
	add hl,de
	ld a,(hl)
	cp BGCl
	jp z,die	
_:

	ld de,-((SPHP_sizeX/3*2)*320)-SPHP_sizeX+7
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,+_
	jp nc,die
	ld bc,-4
	jr SPCSHP_touchedUp_adjustPosition
_:
	ld de,SPHP_sizeX-8
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,+_
	jp nc,die
	ld bc,-(SPHP_sizeX-4)
	jr SPCSHP_touchedUp_adjustPosition
_:
	ld de,((SPHP_sizeY-2)*320)-(SPHP_sizeX-8)
	add hl,de
	ld a,(hl)
	cp BGCl
	jr z,+_
	jp nc,die
	ld bc,-((SPHP_sizeY-3)*320)-4
	jr SPCSHP_touchedDown_adjustPosition
_:
	ld de,SPHP_sizeX-8
	add hl,de
	ld a,(hl)
	cp BGCl
	jp z,dispCharacter
	jp nc,die
	ld bc,-((SPHP_sizeY-3)*320)-(SPHP_sizeX-4)
	jr SPCSHP_touchedDown_adjustPosition


SPCSHP_touchedUp_adjustPosition:
	ld de,320
SPCSHP_touchedUp_adjustPosition_loop:
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,SPCSHP_touchedUp_adjustPosition_loop
	ld de,(currentDrawLoc)
	sbc hl,de
	add hl,bc
	ld (Vcharacter_posY),hl
	ld hl,spcshpSpeedLUT-3
	ld (jmp_speed),hl
	ld hl,0
	ld (mn),hl
	set FLG_topReached, (iy+asm_flag1)
	jp dispCharacter
	
SPCSHP_touchedDown_adjustPosition:
	; If positive speed
	push hl
	ld hl,(jmp_speed)
	ld de,spcshpSpeedLUT
	or a
	sbc hl,de
	pop hl
	jp c,dispCharacter
	ld de,-320
SPCSHP_touchedDown_adjustPosition_loop:
	add hl,de
	ld a,(hl)
	cp BGCl
	jp nz,SPCSHP_touchedDown_adjustPosition_loop
	ld de,(currentDrawLoc)
	sbc hl,de
	add hl,bc
	ld (Vcharacter_posY),hl
	ld hl,spcshpSpeedLUT-3
	ld (jmp_speed),hl
	ld hl,_spaceship_tiles
	ld (spr_state),hl
	ld hl,0
	ld (mn),hl
	set FLG_botReached, (iy+asm_flag1)
	jp dispCharacter
	
	
SPCSHP_jmping:
	ld hl,spcshpSpeedLUT_beg
	or a
	sbc hl,de
	jp nc,SPCSHP_suit
	dec de
	dec de
	dec de
	ld (jmp_speed),de
	ld hl,spcshpSpeedLUT
	or a
	sbc hl,de
	jp nc,SPCSHP_suit
	dec de
	dec de
	dec de
	ld (jmp_speed),de
	jp SPCSHP_suit

SPCSHP_up:
	set FLG_jumping, (iy+asm_flag1)
	ld de,(spr_state)
	ld hl,_spaceship_tiles_end-6
	or a
	sbc hl,de
	jr z,SPCSHP_up_lastState
	ex de,hl
	inc hl
	inc hl
	inc hl
	ld (spr_state),hl
SPCSHP_up_lastState:
	jp rsuit

SPCSHP_down:
	res FLG_jumping, (iy+asm_flag1)
	ld de,(spr_state)
	ld hl,_spaceship_tiles_beg+6
	or a
	sbc hl,de
	jr z,SPCSHP_down_lastState
	ex de,hl
	dec hl
	dec hl
	dec hl
	ld (spr_state),hl
SPCSHP_down_lastState:
	jp rsuit


game_pause:
	bit FLG_pauseLeft, (iy+asm_flag2)
	jp nz,rsuit
	ld hl,(mpLcdBase)
	push hl
	ld hl,saveSScreen+2001
	ld (hl),%01010101
	ld de,saveSScreen+1+2001
	ld bc,240*320/4
	ldir

	ld hl,saveSScreen+(320*TIL_sizeY*MAP_sizeY/4)+2001
	ld (hl),%11111111
	ld de,saveSScreen+(320*TIL_sizeY*MAP_sizeY/4)+1+2001
	ld bc,320/4
	ldir
	ld (hl),%10101010
	ld bc,(320*240-((320*TIL_sizeY*MAP_sizeY)+(320*1)))/4
	ldir

	ld a,lcdBpp2
	ld (mpLcdCtrl),a
	ld hl,saveSScreen+2001
	ld (mpLcdBase),hl

	ld hl,0
	ld (mpLcdPalette),hl
	ld hl,(mpLcdPalette+(BGCl*2))
	ld (mpLcdPalette+2),hl
	ld hl,(mpLcdPalette+((BGCl-1)*2))
	ld (mpLcdPalette+4),hl
	ld hl,$FFFF
	ld (mpLcdPalette+6),hl
	

	ld a,$30
	call _DelayTenTimesAms
	call DEBUG

	ld de,mpLcdPalette
	ld hl,_TILES_pal
	ld bc,16*2
	ldir
	pop hl
	ld (mpLcdBase),hl
	ld a,lcdBpp8
	ld (mpLcdCtrl),a
	set FLG_pauseLeft, (iy+asm_flag2)
	jp rsuit

_PAUSE_pal:
	.dw 00000h,0D002h,12288,0FFFFh

dieHard:
	; die because the character is out of the VRam bounds
	pop hl
	jr crashed
die:
	; If jumping tile
	cp $FA
	jp nc,dispCharacter
	cp $7A
	jr c,crashed
	cp $7F
	jp c,dispCharacter
crashed:
	; New High Score ?
	ld hl,(begLvlToPlay)
	ld de,0
	xor a
	sbc hl,de
	call z,addHighScore ; if we play by the beginning

	;	Erasing elements of the tail (one by one)
	ld hl,-(TAIL_nb*SCROLL_spd)/2
	ld (scrapMem),hl
	ld b,TAIL_nb+1
ERA_eraseTail:
	push bc
	ld hl,(currentTailEl)
	ld a,(hl)
	inc hl
	ld hl,(hl)
	ld de,(scrapMem)
	add hl,de
	ld b,a
ERA_erasingElTail:
	ld e,b
	ld b,a
	push hl
ERA_erasingElTailX:
	ld (hl),BGCl
	inc hl
	djnz ERA_erasingElTailX
	pop hl
	ld bc,320
	add hl,bc
	ld b,e
	djnz ERA_erasingElTail
	ld hl,(scrapMem)
	ld bc,SCROLL_spd/2
	add hl,bc
	ld (scrapMem),hl
	ld hl,(currentTailEl)
	inc hl
	inc hl
	inc hl
	inc hl
	ld de,tailEffectBufPos+(TAIL_nb*4)+4
	ex de,hl
	sbc hl,de
	ex de,hl
	jr nz,ERA_bufNotLastEl
	ld hl,tailEffectBufPos
ERA_bufNotLastEl:
	ld (currentTailEl),hl
	pop bc
	djnz ERA_eraseTail

	ld a,r
	sra a
	sra a
	sra a
	ld (colorCircle),a
	ld hl,(mpLcdBase)
	ld de,(prev_posY)
	add hl,de
	ld de,(SPR_sizeY/2*320)+(SPR_sizeX/2)
	add hl,de
	ld a,2
DIE_anim:
	call Fcircle
	push af
	call _Delay10ms
	pop af
	inc a
	cp 22
	jr nz,DIE_anim

	ld a,BGCl
	ld (colorCircle),a
	ld a,2
DIE_animErase:
	push af
	push hl
	call Fcircle
	call _Delay10ms
	pop hl
	pop af
	inc a
	cp 19
	jr nz,DIE_animErase

	ld a,$30
	call _DelayTenTimesAms
	DI
	jp playLevel_noextract

DEBUG:
	push af
	push bc
	push de
	push hl
	ld a,$10
	call _DelayTenTimesAms
	DI
DEBUG_LOOP:
	ld hl,$F50000
	ld (hl),2
	xor a
scan_wait2:
	cp (hl)
	jr nz,scan_wait2
	ld hl,($F50012)
	ld de,($F50015)
	add hl,de
	ld de,($F50018)
	add hl,de
	ld de,($F5001B)
	add hl,de
	ld a,($F5001E)
	ld e,a
	add hl,de
	ld de,0
	or a
	sbc hl,de
	jr z,DEBUG_LOOP
	pop hl
	pop de
	pop bc
	pop af
	ret

;DEBUG2:
;	push hl
;	push af
;	ld hl,(mpLcdPalette+(BGCl*2))
;	ld a,r
;	ld h,a
;	ld (mpLcdPalette+(BGCl*2)),hl
;	pop af
;	pop hl
;	ret

	.dl -320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10,-320*10
jmpSpeedLUT:
	.dl -320*10,-320*9,-320*8,-320*6,-320*5,-320*4,-320*4,-320*3,-320*3,-320*2,-320*1,320*0
jmpSpeedLUT_0:
	.dl 320*0,320*1,320*2,320*3,320*3,320*4,320*4,320*5,320*6,320*8,320*9
jmpSpeedLUT_rvrs:
	.dl 320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10,320*10

spcshpSpeedLUT_beg:
	.dl -320*6
	.dl -320*5,-320*5,-320*5,-320*5,-320*5,-320*4,-320*4,-320*4,-320*4,-320*4,-320*4,-320*3,-320*3,-320*3,-320*3,-320*2,-320*2,-320*2,-320*2,-320*1,-320*1,-320*1-320*1,-320*1,-320*0
spcshpSpeedLUT:
	.dl 320*0,320*0,320*1,320*1,320*1,320*1,320*1,320*2,320*2,320*2,320*2,320*3,320*3,320*3,320*3,320*4,320*4,320*4,320*4,320*4,320*5,320*5,320*5,320*5,320*5
spcshpSpeedLUT_end:
	.dl 320*6

tailEffectBufPos:
	.fill TAIL_nb*6,0