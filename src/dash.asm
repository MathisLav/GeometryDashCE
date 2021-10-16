.nolist
	#define db .db
	#define dw .dw
	#define dl .dl
	#include "include/ti84pce.inc"
	#macro bcall(x)
		call x
	#endmacro
	; constants
	#DEFINE TIL_sizeY 	21
	#DEFINE TIL_sizeX 	21
	#DEFINE MAP_sizeY 	10 ; error : I should have used only WIN_sizeY
	#DEFINE WIN_sizeY 	10
	#DEFINE WIN_sizeX 	16
	#DEFINE SPR_sizeX 	30
	#DEFINE SPR_sizeY 	30
	#DEFINE SPHP_sizeX 	22
	#DEFINE SPHP_sizeY 	22
	#DEFINE SPR_posX	80
	#DEFINE	SCROLL_spd	08
	#DEFINE BGCl		$7F
	#DEFINE BGCl_menu	$07
	#DEFINE TAIL_nb		08
	#DEFINE TAIL_fstClr	$FC
	#DEFINE beg_sprite	5*320+5
	#DEFINE tilesExtracted		pixelshadow2-6000
	#DEFINE tilesExtractedMenu	plotSScreen
	#DEFINE sizeGDFont	28*28*26
	; values of getkeyLetter
	#DEFINE CLEAR	-1
	#DEFINE DEL		-2
	#DEFINE ENTER	-3
	#DEFINE UP		-4
	#DEFINE DOWN	-5
	; saferam play level
	;#DEFINE TRS_paletteSRC	(pixelshadow2-3900)
	#DEFINE fstBlock 		pixelshadow		; adresse du block en haut à gauche de la fenêtre (visuelle)
	#DEFINE dispBlkFrmX		pixelshadow+3	; nombre de pixel qui ont déjà été affiché (à partir d'om commencer l'affichage du tile après le scroll)
	#DEFINE	Vcharacter_posY	pixelshadow+6	; position du sprite sur l'écran
	#DEFINE	prev_posY		pixelshadow+9	; position du sprite sur l'écran sur la précédente frame (sert à effacer le sprite)
	#DEFINE	jmp_speed		pixelshadow+12	; vitesse du saut
	#DEFINE spr_state		pixelshadow+15	; sprite we are displaying [0-9]
	#DEFINE prev_sprState	pixelshadow+18	; previous sprite we were displaying [0-9]
	#DEFINE currentTailEl	pixelshadow+21	; sert pour les étincelles qui suivent le sprite
	#DEFINE MAP_sizeX		pixelshadow+24	; taille X de la map qui est en train d'être jouée
	#DEFINE dispBlkFrmY		pixelshadow+27	; analogue à dispBlkFrmX pour l'axe Y
	#DEFINE prev_speed		pixelshadow+30	; ancienne vitesse du saut
	#DEFINE prv_spdDiv320	pixelshadow+33	; ancienne vitesse du saut/320
	#DEFINE bytesToSkip		pixelshadow+36	; offset qui pointe vers le block en haut à gauchde de la fenetre (pour le scroll en place)
	#DEFINE maxBytesToSkip	pixelshadow+39	; nombre de block qu'on peut sauter au maximum (pour le vertical scrolling, définit la valeur max de bytestoskip)
	#DEFINE currentSprBuf	pixelshadow+42	; buffer en cours sur cette frame (voir behindSprite1/20)
	#DEFINE addrGravity		pixelshadow+45	; adresse qui pointe vers l'adresse du prochain changement de gravité
	#DEFINE nbChngGrvtyRmng	pixelshadow+48	; combien il reste de changement de gravité sur ce niveau
	#DEFINE addrSpaceship	pixelshadow+51	; adresse qui pointe vers l'adresse du prochain changement de véhicule
	#DEFINE nbChngSphpRmng	pixelshadow+54	; combien il reste de changement de véhicule sur ce niveau
	#DEFINE beginningMap	pixelshadow+57	; adresse du point en haut à gauche de la map
	#DEFINE spriteSize		pixelshadow+60	; taille de l'actuel sprite (en fonction du véhicule, 30 ou 22)
	#DEFINE begLvlToPlay	pixelshadow+63	; en mode édition, permet de jouer à un level à partir de la position du curseur
	; saferam edit level
	#DEFINE LV_MAP_sizeX	pixelshadow+100	; version edition de MAP_sizeX
	#DEFINE LV_bytesToSkip	pixelshadow+103	; """
	#DEFINE LV_maxBytesToSkip pixelshadow+106; """
	#DEFINE LV_beginningMap	pixelshadow+109	; """
	#DEFINE LV_fstBlock		pixelshadow+112	; """
	#DEFINE posCursorScrnX	pixelshadow+115
	#DEFINE posCursorScrnY	pixelshadow+116
	#DEFINE currentBlock	pixelshadow+118
	#DEFINE currentBlock2	pixelshadow+119	; si FLG_twoPartTile est set, currentBlock2 est le numéro de la 2ème partie du tile
	#DEFINE addrMAP_sizeY	pixelshadow+121
	#DEFINE addrMAP_sizeX	pixelshadow+124
	#DEFINE LV_addrGravity	pixelshadow+127
	#DEFINE LV_addrSpaceship pixelshadow+130
	#DEFINE timerArrow		pixelshadow+133
	#DEFINE playMem			pixelshadow+136
	#DEFINE FLG_twoPartTile	0 ; asm_flag1 (1=current tile is cut in two parts of 21*21 pxl each)
	#DEFINE FLG_insDel10	1 ; asm_flag1 (1=inserting or deleting 10 columns in edit mode)
	#DEFINE FLG_blockInsDel	2 ; asm_flag1 (1=prevents program from inserting or deleting another line/column)
	#DEFINE FLG_blockArrows	3 ; asm_flag1 (1=block arrows)
	#DEFINE FLG_blockMode	4 ; asm_flag1 (1=block Mode press after game play)
	#DEFINE FLG_toogleCtxt	5 ; asm_flag1 (1=toogleSpacechip, 0=toogleGravity)
	; saferam misc
	#DEFINE mn				pixelshadow+197 ; used as scrapMem
	#DEFINE behindSprite1	pixelshadow+200 ; 900 bytes (30*30)
	#DEFINE behindSprite2	pixelshadow+1100 ; 900 bytes (30*30)
	#DEFINE nmbOfLevels		cmdShadow		; used in the main menu
	#DEFINE createDiffic	cmdShadow+3
	#DEFINE addrLvlEdited	cmdShadow+4	; adresse après le nom du niveau qui est en train d'être édité
	#DEFINE createNameBuf	cmdShadow+7	; (9+9+2 OCTETS) buffer de l'entrée utilisateur du nom du niveau qu'il faut créer
	#DEFINE namesBuf		cmdShadow+27	; used in the main menu
	#DEFINE	GD_MENU_MAX_LEVEL	16
	; FLAGS
	;	asm_flag1
	#DEFINE	FLG_jumping		0 ; flag (sprite is jumping or spaceship is flying)
	#DEFINE FLG_jmpAgain	1 ; flag (if user pressed the 2nd key while jumping)
	#DEFINE FLG_rvrsGravity	2 ; flag (1=gravity reversed)
	#DEFINE FLG_spaceshipOn	3 ; flag (1=spaceship)
	#DEFINE FLG_topReached	4 ; flag (used to synchronize the buffers)
	#DEFINE FLG_botReached	5 ; flag (used to synchronize the buffers)
	#DEFINE FLG_chgOfDir	6 ; flag (when user changes the direction of the spaceship)
	#DEFINE FLG_alrdyErased	7 ; flag ()
	;	asm_flag2
	#DEFINE FLG_pauseLeft	0 ; flag ()
	#DEFINE FLG_noLevel		1 ; MENU flag (if there's no level on the calc)
	#DEFINE FLG_jmpUsed		2 ; flag (prevent sprite from jumping on a yellow tile if user did not re-pressed 2nde)
	#DEFINE FLG_sphpAvlble	3 ; flag (COMPATIBILITÉ AVEC ANCIENNE VERSION (sans vaisseau spatial))
	#DEFINE FLG_2dePartName	4 ; MENU flag (used when choosing the name of the level that is being created)
	#DEFINE FLG_inEditorMode 5; flag (1=editing a level)
	;#DEFINE FLG_colorTRS	6 ; flag ()
.list
	.org userMem-2
	.db tExtTok,tAsm84CeCmp

	jp	_prgm_init
	db	$01
_icon:
	db	$10, $10
	db	$AC, $A5, $E5, $E4, $A3, $A3, $A3, $A3, $A3, $83, $A3, $A3, $A3, $C4, $CD, $AC
	db	$C5, $42, $83, $A3, $09, $09, $09, $21, $A4, $E5, $62, $09, $09, $09, $83, $CD
	db	$E5, $A3, $A3, $4A, $09, $09, $02, $A4, $E5, $E5, $E5, $62, $09, $0A, $0A, $C4
	db	$E4, $A3, $4A, $0A, $0A, $0A, $A4, $E6, $85, $84, $E5, $E5, $62, $0A, $0A, $A3
	db	$A3, $0A, $0A, $0A, $0A, $A4, $E6, $E5, $4D, $4D, $C5, $C4, $E4, $62, $0A, $A3
	db	$A3, $0A, $0B, $2A, $A4, $E6, $A5, $C5, $C5, $C5, $84, $2C, $A4, $E4, $62, $A3
	db	$A3, $0B, $0A, $A4, $E6, $A5, $35, $6C, $E5, $84, $36, $4D, $C4, $E5, $E4, $82
	db	$A3, $0A, $0A, $63, $E5, $E5, $84, $C5, $84, $36, $4D, $C4, $E4, $E4, $C2, $82
	db	$A3, $0B, $0B, $0B, $62, $E4, $E5, $84, $16, $4D, $C4, $E4, $E4, $C2, $29, $A3
	db	$A3, $13, $54, $13, $0A, $62, $E4, $84, $4C, $C4, $E4, $E4, $C3, $2A, $0B, $A3
	db	$A3, $33, $2A, $34, $13, $0B, $62, $E4, $E4, $E4, $E4, $C3, $2A, $13, $13, $A3
	db	$AB, $4B, $00, $4B, $13, $13, $0B, $62, $E3, $E4, $C3, $2A, $13, $13, $13, $A3
	db	$C3, $2A, $09, $2A, $34, $13, $13, $0B, $62, $A2, $2A, $13, $13, $14, $13, $A3
	db	$C3, $34, $34, $34, $55, $34, $14, $14, $13, $2B, $14, $14, $14, $14, $13, $C3
	db	$CB, $8B, $34, $15, $34, $14, $14, $14, $14, $14, $14, $14, $14, $14, $8C, $CC
	db	$AC, $CB, $C3, $AB, $AB, $AB, $AB, $AB, $AB, $AB, $AB, $AB, $AB, $C3, $CC, $AC
_description:
	db	"Geometry Dash pour CE", 0
_prgm_init:

	; AppVars stuffs
	ld hl,AppVarGraphics
	call _mov9ToOp1
	call _chkFindSym
	jr nc,AppVarGraphicsFound
	call _clrlcdfull
	call _homeUp
	ld hl,ERR_needGrphc
	call _PutS
	call _getkey
	ret
AppVarGraphicsFound:
	call _chkInRam
	call z,_ARC_UNARC
	ld hl,AppVarMenu
	call _mov9ToOp1
	call _chkFindSym
	jr nc,AppVarMenuFound
	call _clrlcdfull
	call _homeUp
	ld hl,ERR_needMenu
	call _PutS
	call _getkey
	ret
AppVarMenuFound:
	call _chkInRam
	call z,_ARC_UNARC

	ld hl,AppvarStoreScore
	call _mov9ToOp1
	call _chkFindSym
	jr nc,appvarExists
	ld hl,6 ; in this appvar : high score of the levels
	call _createAppvar
	inc de
	inc de
	ex de,hl
	ld de,0
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld (hl),de
	push hl
	call _op4toop1
	pop de
appvarExists:
	call _chkInRam
	call z,_ARC_UNARC

	call _RunIndicOff
	ld a,(mpLcdImsc)
	or a,4 ; bitLcdLnBuim
	ld (mpLcdImsc),a
	ld a,lcdBpp8
	ld (mpLcdCtrl),a

	ld hl,namesBuf
	ld (current_level),hl

menuGame:
	DI
	; looking for level appvars in VAT
	ld hl,(progPtr)
	ld ix,namesBuf ; address[3] addrAfterName[3] addrNameAppVarInVAT → 9 bytes
	ld iy,0
loopNextAppV:
	ex de,hl
	ld hl,(OPBase)
	or a
	sbc hl,de
	jp nc,lastElement
	ex de,hl

	ld a,(hl)
	ld de,-6
	add hl,de
	cp AppVarObj
	jr z,AppVarFound
	ld a,(hl)
	inc a
	ld de,0
	ld e,a
	or a
	sbc hl,de
	jr loopNextAppV
AppVarFound:
	dec hl
	ld a,(hl)
	inc hl
	cp 'L'
	jr nz,notForGD
	push hl
	inc hl
	ld a,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld e,(hl)
	ld (scrapMem),de
	ld (scrapMem+2),a
	ld de,(scrapMem)
	cp $D0
	jr nc,appVarInRAM
	ex de,hl
	ld de,9
	add hl,de
	ld a,(hl)
	inc a
	ld e,a
	add hl,de
	ex de,hl
appVarInRAM:
	inc de
	inc de
	; looking for "DataLevel"
	ld hl,DataLevel
	ld b,(hl)
	inc hl
stringToFind_loop:
	ld a,(de)
	cp (hl)
	jr nz,notForGD_pop
	inc hl
	inc de
	djnz stringToFind_loop
	ld	a, iyl
	inc	a
	cp	a, GD_MENU_MAX_LEVEL+1
	jr	nc, notForGD_pop
	inc	iy
	ex	de, hl
	ld	(ix), hl
	ld	bc, 0
	ld	c, (hl)
	add	hl, bc
	inc	hl
	ld	c, (hl)
	add	hl, bc
	inc	hl
	ld	(ix+3), hl
	pop	hl
	ld	(ix+6), hl
	lea	ix, ix+9
	push hl ; pour reset le stack si on est sorti de la boucle stringToFind_loop prématurément (y'a un pop d'écart)
notForGD_pop:
	pop hl
notForGD:
	ld a,(hl)
	inc a
	ld de,0
	ld e,a
	or a
	sbc hl,de
	jp loopNextAppV
lastElement:
	ld (nmbOfLevels),iy
	ld iy,flags
	ld a,(nmbOfLevels)
	res FLG_noLevel, (iy+asm_flag2)
	or a
	jr nz,levelFound
	set FLG_noLevel, (iy+asm_flag2)
levelFound:

	; extracting data
	ld hl,AppVarMenu
	call _mov9ToOp1
	call _chkFindSym
	ld hl,18
	add hl,de
	ld bc,(hl)
	inc hl
	inc hl
	inc hl
	ld de,tilesExtractedMenu
	call extractRLE

	; clear screen
	ld hl,vRam
	ld (hl),BGCl_menu
	ld de,vRam+1
	ld bc,320*240*2
	ldir
	ld de,mpLcdPalette
	ld hl,_menu_pal
	ld bc,512
	ldir

	; draw floor
	ld hl,vRam+(320*TIL_sizeY*MAP_sizeY)
	ld (hl),0
	ld de,vRam+(320*TIL_sizeY*MAP_sizeY)+1
	ld bc,320*1
	ldir
	ld (hl),06
	ld bc,320*240-((320*TIL_sizeY*MAP_sizeY)+(320*1))
	ldir
	ld hl,vRam+(320*TIL_sizeY*MAP_sizeY)+(320*240)
	ld (hl),0
	ld de,vRam+(320*TIL_sizeY*MAP_sizeY)+(320*240)+1
	ld bc,320*1
	ldir
	ld (hl),06
	ld bc,320*240-((320*TIL_sizeY*MAP_sizeY)+(320*1))
	ldir

	ld hl,vRam+(320*100)+2
	ld ix,tilesExtractedMenu+sizeGDFont ; _menu_tile_0
	ld de,320
	call drawSprite

	ld hl,vRam+(320*100)+290
	ld ix,tilesExtractedMenu+sizeGDFont ; _menu_tile_0
	ld de,320
	call drawSpriteMirror

	ld hl,vRam+(320*199)
	ld ix,tilesExtractedMenu+sizeGDFont+5050
	ld de,320
	call drawSprite

	ld hl,vRam+(320*199)+278
	ld ix,tilesExtractedMenu+sizeGDFont+5050
	ld de,320
	call drawSpriteMirror

	ld hl,vRam+60
	ld ix,tilesExtractedMenu+sizeGDFont+5050+1724
	ld de,320
	call drawSprite

	bit FLG_noLevel, (iy+asm_flag2)
	jp nz,LOOP_noLevel

	ld a,9
	ld (colorCircle),a
	ld a,13
	ld hl,vRam+(320*170)+77
	ld b,22
	ld de,8
drawBarBord:
	call Fcircle
	add hl,de
	djnz drawBarBord

menuGameNoClr:
	ld a,140 ; 098C6h
	ld (colorCircle),a
	ld a,10
	ld hl,vRam+(320*170)+81
	ld b,21
	ld de,8
drawBar:
	call Fcircle
	add hl,de
	djnz drawBar

	; draw main rectangle
	ld hl,pixelshadow
	ld (hl),8
	ld de,pixelshadow+1
	ld bc,80*250
	ldir

	ld ix,(current_level)
	pea ix+3
	ld ix,(ix)
	ld hl,pixelshadow+(250*27)+40
	ld de,25*8
	push ix
	call centerText
	ld a,(ix)
	pop ix
	or a
	jr z,oneName
	ld de,-250*15
	add hl,de
	call putGDFont
	ld hl,pixelshadow+(250*45)+40
	ld de,25*8
	push ix
	call centerText
	pop ix
oneName:
	call putGDFont
	
	pop hl
	ld hl,(hl)
	ld a,(hl)
	ld de,30*30+2
	call _Mult16By8
	ld ix,tilesExtractedMenu+sizeGDFont+1442
	ex de,hl
	add ix,de
	ld hl,pixelshadow+(250*23)+5
	ld de,250
	call drawSprite

	ld de,pixelshadow
	ld hl,vRam+(55*320)+35
	ld b,80
ANIM_dispRectInfoY:
	push bc
	ld b,250
ANIM_dispRectInfoX:
	ld a,(de)
	ld (hl),a
	ld a,10
DSP_pause:
	ld ix,(0)
	dec a
	jr nz,DSP_pause
	inc hl
	inc de
	djnz ANIM_dispRectInfoX
	ld bc,320-250
	add hl,bc
	pop bc
	djnz ANIM_dispRectInfoY

	ld hl,(current_level)
	inc hl
	inc hl
	inc hl
	ld hl,(hl)
	inc hl
	ld de,(hl)
	push de
	push hl
	call findHighScoreInAppVar
	ld hl,(ix)
	pop ix
	pop de
	jp nz,neverPlayed

	push de
	call _setXXXXop2
	ld a,21
	call _setxxop1
	call _FPmult
	pop de
	ld hl,$010000
	sbc hl,de
	jr nc,oldVerWhithoutSpeed
	inc ix
oldVerWhithoutSpeed:
	inc ix
	inc ix
	inc ix
	ld hl,(ix)
	call _setHLUTo0
	call _setXXXXop2
	call _FPDiv
	call _convop1
	ld b,e
	xor a
	cp b
	jr z,neverPlayed

	ld a,$2E
	ld (colorCircle),a
	ld hl,vRam+(320*170)+81
	ld de,8
drawBarFill:
	ld a,10
	call Fcircle
	exx
	ld hl,$F50000
	ld (hl),2
	xor a
FILL_scanWait:
	cp (hl)
	jr nz,FILL_scanWait
	ld a,(kbdG7)
	bit kbitLeft, a
	jp nz,prev_level
	bit kbitRight, a
	jp nz,next_level
	; quit
	ld a,(kbdG1)
	bit kbitMode, a
	jp nz,quit
	bit 7, a
	jp nz,quit
	ld a,(kbdG6)
	bit kbitClear, a
	jp nz,quit
	; play
	bit kbitEnter, a
	jp nz,fillingPlayLevel
	ld a,2
	call _DelayTenTimesAms
	exx
	add hl,de
	djnz drawBarFill
neverPlayed:


menuGameLoop:
	ld hl,$F50000
	ld (hl),2
	xor a
MENU_scanWait:
	cp (hl)
	jr nz,MENU_scanWait
	ld a,(kbdG7)
	bit kbitLeft, a
	jp nz,prev_level
	bit kbitRight, a
	jp nz,next_level
	; quit/play
	ld a,(kbdG1)
	bit kbitMode, a
	jp nz,quit
	bit kbitDel, a
	jp nz,quit
	bit kbit2nd, a
	jp nz,fillingPlayLevel
	ld a,(kbdG6)
	bit kbitClear, a
	jp nz,quit
	bit kbitEnter, a
	jp nz,fillingPlayLevel
	; create/edit
	ld a,(kbdG2)
	bit kbitAlpha, a
	jp nz,processEditLevel
	ld a,(kbdG6)
	bit kbitAdd, a
	jr z,menuGameLoop

	; choose the name of the level
	ld hl,vRam+(320*100)
	ld (hl),BGCl_menu
	push hl
	pop de
	inc de
	ld bc,320*85
	ldir

	ld hl,createNameBuf
	ld (hl),0
	push hl
	pop de
	inc de
	ld bc,20
	ldir
	res FLG_2dePartName, (iy+asm_flag2)
	xor a
	ld (createDiffic),a
	
CREATE_loopName:
	ld hl,pixelshadow
	ld (hl),8
	ld de,pixelshadow+1
	ld bc,80*250
	ldir

	ld ix,createNameBuf
	ld hl,pixelshadow+(250*27)+40
	ld de,25*8
	push ix
	call centerText
	pop ix
	bit FLG_2dePartName, (iy+asm_flag2)
	jr z,CREATE_oneName
	ld de,-250*15
	add hl,de
	call putGDFont
	ld hl,pixelshadow+(250*45)+40
	ld de,25*8
	push ix
	call centerText
	pop ix
	ld a,(ix)
CREATE_oneName:
	call putGDFont

	; difficulty
	ld a,(createDiffic)
	ld de,30*30+2
	call _Mult16By8
	ld ix,tilesExtractedMenu+sizeGDFont+1442
	ex de,hl
	add ix,de
	ld hl,pixelshadow+(250*23)+5
	ld de,250
	call drawSprite

	ld hl,pixelshadow
	ld de,vRam+(55*320)+35
	ld a,80
CREATE_copyRect:
	ld bc,250
	ldir
	push hl
	ld hl,70
	add hl,de
	ex de,hl
	pop hl
	dec a
	jr nz,CREATE_copyRect
	
	ld a,$10
	call _DelayTenTimesAms
	DI
	call getkeyLetter
	ld hl,createNameBuf
	ld de,0
	ld a,CLEAR
	cp c
	jp z,quit
	ld a,UP
	cp c
	jr z,incDifficMod3
	ld a,DOWN
	cp c
	jr z,decDifficMod3
	ld a,DEL
	cp c
	jr nz,CREATE_insertLetter ; if we didn't press DEL

	xor a
	cp (hl)
	jp z,CREATE_loopName
	bit FLG_2dePartName, (iy+asm_flag2)
	jr z,CREATE_del1Let
	ld e,(hl)
	add hl,de
	inc hl
	cp (hl)
	jr nz,CREATE_del1Let
	; if the second name is empty
	res FLG_2dePartName, (iy+asm_flag2)

CREATE_del1Let:
	ld e,(hl)
	dec (hl)
	add hl,de
	ld (hl),0
	jp CREATE_loopName

CREATE_insertLetter:
	ld a,ENTER
	cp c
	jr nz,CREATE_noEnter ; ENTER
	bit FLG_2dePartName, (iy+asm_flag2)
	jr nz,processCreateLevel
	ld a,(hl)
	or a
	jp z,CREATE_loopName
	set FLG_2dePartName, (iy+asm_flag2)
	jp CREATE_loopName

CREATE_noEnter:
	bit FLG_2dePartName, (iy+asm_flag2)
	jr z,CREATE_1stPartName

	ld e,(hl)
	add hl,de
	inc hl
	ld a,(hl)
	cp 8
	jp z,CREATE_loopName
	jr CREATE_endIfElse
CREATE_1stPartName:
	ld a,(hl)
	cp 8
	jp z,CREATE_loopName
CREATE_endIfElse:
	inc (hl)
	ld e,(hl)
	add hl,de
	ld (hl),c

	jp CREATE_loopName

incDifficMod3:
	ld a,(createDiffic)
	inc a
	and %00000011
	ld (createDiffic),a
	jp CREATE_loopName
decDifficMod3:
	ld a,(createDiffic)
	dec a
	cp -1
	jr nz,decNoPb
	ld a,3
decNoPb:
	ld (createDiffic),a
	jp CREATE_loopName


processCreateLevel:
	; on a le nom dans createNameBuf
	; on a la difficulté dans createDiffic
	ld hl,createNameBuf
	ld bc,0
	ld b,(hl)
	ex de,hl
	ld hl,OP1
	ld (hl),appVarObj
	inc hl
	ld (hl),'L'
	inc hl
	inc de
CREATE_formatName:
	ld a,(de)
	cp ' '
	jr z,CREATE_format_space
	add a,40h
	ld (hl),a
	inc hl
CREATE_format_space:
	inc de
	djnz CREATE_formatName
	ld (hl),b ; b=0
	push hl
	call _chkfindsym
	pop hl
	jr c,CREATE_createAppVar

	dec hl
	ld a,'0'
	ld b,10
CREATE_loopTryNames:
	ld (hl),a
	push hl
	call _chkfindsym
	pop hl
	jr c,CREATE_createAppVar
	inc a
	djnz CREATE_loopTryNames
	; error : name already taken
	jp _ErrSyntax

CREATE_createAppVar:
	; AppVar size : 12+sizeName+11+sizeDATA (we consider sizeDATA=40*10) => AppVar size = 423+sizeName
	call _pushrealo1
	call _pushrealo1
	ld hl,createNameBuf
	ld de,0
	ld e,(hl)
	add hl,de
	inc hl
	ld a,(hl)
	add a,e
	add a,2
	ld e,a
	push de
	ld hl,423
	add hl,de
	call _createAppVar
	inc de
	inc de
	ld hl,DataLevel+1
	ld bc,12 ; FFEphariusFFGD
	ldir
	pop bc
	ld hl,createNameBuf ; name
	ldir
	ld a,(createDiffic)
	ld (de),a ; difficulty
	inc de
	call createHashID
	ex de,hl
	ld (hl),de ; id
	inc hl
	inc hl
	inc hl
	ld (hl),15 ; speed
	inc hl
	ld de,40 ; map size X
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld (hl),0 ; map size Y
	inc hl
	ld (hl),0 ; empty tile
	push hl
	pop de
	inc de
	ld bc,399
	ldir
	ex de,hl
	ld (hl),0 ; gravity
	inc hl
	ld (hl),0 ; spaceship
	call _poprealo1
	jr processEditLevel_afterPushOp1

processEditLevel:
	ld hl,(current_level)
	ld de,6
	add hl,de
	ld hl,(hl)
	ld b,(hl)
	dec hl
	ex de,hl
	ld hl,OP1
	ld (hl),appVarObj
	inc hl
EDIT_copyNameOp1:
	ld a,(de)
	ld (hl),a
	inc hl
	dec de
	djnz EDIT_copyNameOp1
	ld (hl),b ; b=0
	call _pushrealo1
processEditLevel_afterPushOp1:
	; op1 must contain the name of the appvar
	call _pushrealo1
	call _chkfindsym
	call _chkInRam
	call nz,_ARC_UNARC ; in RAM
	call _poprealo1
	call _chkFindSym
	ex de,hl
	ld de,14
	add hl,de
	ld e,(hl)
	add hl,de
	inc hl
	ld e,(hl)
	add hl,de
	ld de,6
	add hl,de
	ld (addrLvlEdited),hl ; addr of the data of the level being edited
	; recup addr apres name
	call editLevel
	xor a
	ld (mpLcdRange+11),a
	ld hl,$EF7802
	ld (mpLcdRange+8),hl
	ld hl,vRam
	ld (mpLcdBase),hl
	jp menugame

createHashID:
	; Routine that create an almost "unique" hash from the name of the AppVar
	; INPUT :
	;	- OP4 : AppVar name
	; OUTPUT :
	;	- HL : Hash ID
	push de
	ld b,7
	ld hl,0
	ld ix,OP4+2
HASH_loop:
	push bc
	ex de,hl
	ld a,10
	call _Mult16By8
	ld bc,0
	ld c,(ix)
	add hl,bc
	pop bc
	inc ix
	djnz HASH_loop
	pop de
	ld a,$FF ; we don't want HLU to be=0 (it means "old version of GD that doesn't have spaceship")
	call _setHLUToA
	ret

fillingPlayLevel:
	ld hl,0
	ld (begLvlToPlay),hl
	res FLG_inEditorMode, (iy+asm_flag2)
	call playLevel
	xor a
	ld (mpLcdRange+11),a
	ld hl,$EF7802
	ld (mpLcdRange+8),hl
	ld hl,vRam
	ld (mpLcdBase),hl
	jp menuGame


addHighScore:
	bit FLG_inEditorMode, (iy+asm_flag2)
	ret nz
	push af
	ld hl,AppvarStoreScore
	call _mov9ToOp1
	call _pushrealo1
	call _ARC_UNARC
	ld hl,(current_level)
	inc hl
	inc hl
	inc hl
	ld hl,(hl)
	inc hl
	ld de,(hl)
	push hl
	call findHighScoreInAppVar
	call nz,newHighScore
	pop hl
	pop af
	or a
	jr nz,HS_endOfLevel
	push hl
	ld bc,(ix)
	ld hl,(fstBlock)
	ld de,(bytesToSkip)
	or a
	sbc hl,de
	pop de
	inc de
	inc de
	inc de
	inc de
	inc de
	sbc hl,de
	push hl
	sbc hl,bc
	pop hl
	jr c,endNewHighScore
	ld (ix),hl
	; affichage HIGH SCORE
	ld hl,(mpLcdBase)
	ld de,(90*320)+((320-194)/2)
	add hl,de
	ld de,320
	ld ix,tilesExtracted+(21*21*56)+(900*11)+(22*22*13)
	call drawSprite
endNewHighScore:
	call _poprealo1
	call _ARC_UNARC
	ret

HS_endOfLevel:
	ld hl,(MAP_sizeX)
	ld (ix),hl
	jr endNewHighScore

newHighScore:
	ld bc,-6
	add ix,bc
	ld (ix),de
	inc ix
	inc ix
	inc ix
	push ix
	lea de,ix+3
	ld hl,6
	call _insertMem
	ex de,hl
	ld de,0
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld (hl),de
	call _CpyTo1FPST
	call _chkFindSym
	ex de,hl
	ld de,(hl)
	ld ix,6
	add ix,de
	ld (hl),ix
	pop ix
	ret

prev_level:
	ld hl,(current_level)
	ld de,-9
	add hl,de
	ex de,hl
	ld hl,namesBuf
	or a
	sbc hl,de
	jr c,PREV_noProblem
	jr z,PREV_noProblem
	ld bc,(nmbOfLevels)
	dec c
	ld b,9
	mlt bc
	ld hl,namesBuf
	add hl,bc
	ex de,hl
PREV_noProblem:
	ld (current_level),de
	jp menuGameNoClr

next_level:
	ld hl,(current_level)
	ld de,9
	add hl,de
	ex de,hl
	ld bc,(nmbOfLevels)
	ld b,9
	mlt bc
	ld hl,namesBuf
	add hl,bc
	sbc hl,de
	jr z,NEXT_lastLevel
	jr nc,NEXT_noProblem
NEXT_lastLevel:
	ld de,namesBuf
NEXT_noProblem:
	ld (current_level),de
	jp menuGameNoClr

	pop hl ; reset stack
	pop hl
speedQuit:
	pop hl
	xor a
	ld (mpLcdRange+11),a
	ld hl,$EF7802
	ld (mpLcdRange+8),hl
	ld hl,vRam
	ld (mpLcdBase),hl
	bit FLG_inEditorMode, (iy+asm_flag2)
	jr z,quit
	pop hl ; reset stack from level editor

quit:
	; removing the timer
	ld a,(mpTmrCtrl)
	and %11010111
	ld (mpTmrCtrl),a
	; ---------------------------------
	EI
	call _clrlcdall
	ld a,lcdBpp16
	ld (mpLcdCtrl),a
	ld hl,vRam
	ld (mpLcdBase),hl
	call _clrTxtShd
	call _homeUp
	call _drawStatusBar
	set graphDraw, (iy+graphFlags)
	ret

LOOP_noLevel:
	; draw main rectangle
	ld hl,pixelshadow
	ld (hl),8
	ld de,pixelshadow+1
	ld bc,80*250
	ldir

	ld hl,pixelshadow+(250*27)+60
	ld ix,TXT_Empty
	call putGDFont

	ld de,pixelshadow
	ld hl,vRam+(80*320)+35
	ld b,80
NOL_ANIM_dispRectInfoY:
	push bc
	ld b,250
NOL_ANIM_dispRectInfoX:
	ld a,(de)
	ld (hl),a
	ld a,10
NOL_DSP_pause:
	ld ix,(0)
	dec a
	jr nz,NOL_DSP_pause
	inc hl
	inc de
	djnz NOL_ANIM_dispRectInfoX
	ld bc,320-250
	add hl,bc
	pop bc
	djnz NOL_ANIM_dispRectInfoY

	call DEBUG
	jr quit
TXT_Empty:
	.db 5,5,13,16,20,25

findHighScoreInAppVar:
	; INPUT :
	;	DE = ID of level
	; OUTPUT :
	;	NZ : High score not found
	;	IX : addr of high-score (if flag Z set)
	push de
	ld hl,AppvarStoreScore
	call _mov9ToOp1
	call _chkFindSym
	call _chkInRam
	jr z,stoScoreInRAM
	ld hl,18
	add hl,de
	ex de,hl
stoScoreInRAM:
	ex de,hl
	ld de,(hl)
	call _setDEUto0
	; dividing by 6
	push hl
	ex de,hl
	ld bc,-1
	ld de,-6
	inc bc
	add hl,de
	jr c,$-2
	pop ix

	inc ix
	inc ix
	pop de
loop_findHighScoreInAppVar:
	ld hl,(ix)
	inc ix
	inc ix
	inc ix
	inc ix
	inc ix
	inc ix
	or a
	sbc hl,de
	jr z,foundHighScore
	dec c
	jr nz,loop_findHighScoreInAppVar
	dec b
	jr nc,loop_findHighScoreInAppVar
	or 1 ; flag Z reset
	ret
foundHighScore:
	dec ix
	dec ix
	dec ix
	ret


currentDrawLoc:
	.dl vram+(lcdWidth*lcdHeight)
AppvarStoreScore:
	.db appVarObj,"GeomDash"
AppVarGraphics:
	.db appvarObj,"GDGrphc",0
AppVarMenu:
	.db appvarObj,"GDMenu",0
DataLevel:
	.db 12,$FF,"Epharius",$FF,"GD"
current_level:
	.dl 0
ERR_needGrphc:
	.db "Need Appvar GDGrphc",0
ERR_needMenu:
	.db "Need Appvar GDMenu",0


;	MEMO: À faire à chaque conversion :
;		- copier couleurs JUMP à la fin de la palette
;		- enlever pixels parasites sur pics
;		- MILIEU :
;			dw 0FFFFh
;			dw 06A32h
;			dw 07BE9h
;			dw 0FFEDh
;			dw 12288
;			dw 0D002h
;		- FIN :
;			dw 0FFFFh
;			dw 06A32h
;			dw 07BE9h
;			dw 0F2C8h
;			dw 0D002h
;		- remplacer les particules par des pixels transparents aux trampolines
;		- Lancer le compresseur

#include "src/graphic.asm"
#include "src/game.asm"
#include "src/end.asm"
#include "src/creator.asm"
#include "src/data.asm"

.echo "Size : ", $ - start
.end
