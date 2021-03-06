!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_TEB_GARDEN_FIELD (DTCO, IG, I, UG, U, USS, &
                                       TGD, TGDO, TGDPE, TGDP, TG, TOP, TVG, &
                                      HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)
!     #################################################################################
!
!!****  *PREP_HOR_TEB_GARDEN_FIELD* - reads, interpolates and prepares an ISBA field
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!      P. Le Moigne 03/2007, Ajout initialisation par ascllv
!!      B. Decharme  01/2009, Optional Arpege deep soil temperature initialization
!!      B. Decharme  03/2014, external init with FA files
!!                            new vertical interpol
!!------------------------------------------------------------------
!
!
!
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_TEB_GARDEN_n, ONLY : TEB_GARDEN_t
USE MODD_TEB_GARDEN_OPTION_n, ONLY : TEB_GARDEN_OPTIONS_t
USE MODD_TEB_GARDEN_PGD_EVOL_n, ONLY : TEB_GARDEN_PGD_EVOL_t
USE MODD_TEB_GARDEN_PGD_n, ONLY : TEB_GARDEN_PGD_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE MODD_PREP,            ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS,       &
                                 XLAT_OUT, XLON_OUT, XX_OUT, XY_OUT,       &
                                 LINTERP, CMASK

USE MODD_PREP_TEB_GARDEN, ONLY : XGRID_SOIL, NGRID_LEVEL,                  &
                                 XWSNOW_GD, XRSNOW_GD, XTSNOW_GD, XLWCSNOW_GD, &
                                 XAGESNOW_GD, XASNOW_GD, LSNOW_IDEAL_GD

USE MODD_ISBA_PAR,        ONLY : XWGMIN
USE MODD_DATA_COVER_PAR,  ONLY : NVEGTYPE
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
USE MODI_READ_PREP_TEB_GARDEN_CONF
USE MODI_READ_PREP_GARDEN_SNOW
USE MODI_PREP_TEB_GARDEN_ASCLLV
USE MODI_PREP_TEB_GARDEN_GRIB
USE MODI_PREP_TEB_GARDEN_UNIF
USE MODI_PREP_TEB_GARDEN_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_VEGTYPE_GRID_TO_PATCH_GRID
USE MODI_PREP_HOR_SNOW_FIELDS
USE MODI_GET_LUOUT
USE MODI_PREP_TEB_GARDEN_EXTERN
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
TYPE(TEB_GARDEN_t), INTENT(INOUT) :: TGD
TYPE(TEB_GARDEN_OPTIONS_t), INTENT(INOUT) :: TGDO
TYPE(TEB_GARDEN_PGD_EVOL_t), INTENT(INOUT) :: TGDPE
TYPE(TEB_GARDEN_PGD_t), INTENT(INOUT) :: TGDP
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
INTEGER,            INTENT(IN)  :: KPATCH
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILE     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
 CHARACTER(LEN=6)              :: YFILETYPE_SNOW ! type of input file
 CHARACTER(LEN=28)             :: YFILE_SNOW     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE_SNOW ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD_SNOW     ! name of file 
REAL, POINTER,     DIMENSION(:,:,:) :: ZFIELDIN  ! field to interpolate horizontally
REAL, POINTER,     DIMENSION(:,:)   :: ZFIELD ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZFIELDOUTP ! field interpolated   horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZFIELDOUTV !
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZVEGTYPE_PATCH ! vegtype for each patch
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZW        ! work array (x, fine   soil grid)
REAL, ALLOCATABLE, DIMENSION(:)     :: ZSUM
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZF        ! work array (x, output soil grid)
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZDG       ! out T grid (x, output soil grid)
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZPATCH    ! work array for patches
REAL, ALLOCATABLE, DIMENSION(:)     :: ZSG1SNOW, ZSG2SNOW, ZHISTSNOW
INTEGER                             :: ILUOUT    ! output listing logical unit
!
LOGICAL                             :: GUNIF     ! flag for prescribed uniform field
LOGICAL                             :: GUNIF_SNOW! flag for prescribed uniform field
INTEGER                             :: JVEGTYPE, JPATCH  ! loop on vegtypes
INTEGER                             :: JLAYER    ! loop on layers
INTEGER                             :: JI, INP, INL, INI
INTEGER                             :: IWORK     ! Work integer
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_GARDEN_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_TEB_GARDEN_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                               HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
 CMASK = 'TOWN  '
!
INI=SIZE(TG%XLAT)
!-------------------------------------------------------------------------------------
!
!*      2.     Snow variables case?
!
IF (HSURF=='SN_VEG ') THEN
  CALL READ_PREP_GARDEN_SNOW(HPROGRAM,TGD%CUR%TSNOW%SCHEME,TGD%CUR%TSNOW%NLAYER,YFILE_SNOW,&
        YFILETYPE_SNOW,YFILEPGD_SNOW,YFILEPGDTYPE_SNOW,GUNIF_SNOW)
  IF(.NOT.GUNIF_SNOW.AND.LEN_TRIM(YFILE_SNOW)==0.AND.LEN_TRIM(YFILETYPE_SNOW)==0)THEN
    !IF(LEN_TRIM(YFILE)/=0.AND.LEN_TRIM(YFILETYPE)/=0)THEN
    IF (YFILETYPE=='GRIB') THEN
      YFILE_SNOW    =YFILE
      YFILETYPE_SNOW=YFILETYPE
      YFILEPGD_SNOW    =YFILEPGD
      YFILEPGDTYPE_SNOW=YFILEPGDTYPE       
    ELSE          
      GUNIF_SNOW=.TRUE.
      IF(ALL(XWSNOW_GD==XUNDEF))XWSNOW_GD=0.0 
    ENDIF 
  ENDIF    
  ALLOCATE(ZSG1SNOW(SIZE(XWSNOW_GD)))
  ALLOCATE(ZSG2SNOW(SIZE(XWSNOW_GD)))
  ALLOCATE(ZHISTSNOW(SIZE(XWSNOW_GD)))
  ALLOCATE(ZPATCH(SIZE(TGDP%XVEGTYPE,1),1))
  ALLOCATE(ZVEGTYPE_PATCH (SIZE(TGDP%XVEGTYPE,1),SIZE(TGDP%XVEGTYPE,2),1))
  !
  ZPATCH=1.
  ZVEGTYPE_PATCH(:,:,1) = TGDP%XVEGTYPE(:,:)
  CALL PREP_HOR_SNOW_FIELDS(DTCO, &
                            IG, U, &
                            HPROGRAM,HSURF,                 &
                            YFILE,YFILETYPE,                &
                            YFILEPGD, YFILEPGDTYPE,         &
                            ILUOUT,GUNIF_SNOW,1,KPATCH,     &
                            INI,TGD%CUR%TSNOW, TOP%TTIME,   &
                            XWSNOW_GD, XRSNOW_GD, XTSNOW_GD,&
                            XLWCSNOW_GD, XASNOW_GD,         &
                            LSNOW_IDEAL_GD, ZSG1SNOW,       &
                            ZSG2SNOW, ZHISTSNOW, XAGESNOW_GD,  &
                            TGDP%XVEGTYPE,ZVEGTYPE_PATCH, ZPATCH )
  DEALLOCATE(ZSG1SNOW)
  DEALLOCATE(ZSG2SNOW)
  DEALLOCATE(ZHISTSNOW)  
  DEALLOCATE(ZPATCH)
  DEALLOCATE(ZVEGTYPE_PATCH)
  IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_GARDEN_FIELD',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------------
!
!*      3.     Reading of input  configuration (Grid and interpolation type)
!
IF (GUNIF) THEN
  CALL PREP_TEB_GARDEN_UNIF(ILUOUT,HSURF,ZFIELDIN)
ELSE IF (YFILETYPE=='ASCLLV') THEN
  CALL PREP_TEB_GARDEN_ASCLLV(DTCO, UG, U, USS, &
                              HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
ELSE IF (YFILETYPE=='GRIB  ') THEN
  CALL PREP_TEB_GARDEN_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '.OR.YFILETYPE=='FA    ') THEN
   CALL PREP_TEB_GARDEN_EXTERN(DTCO, I, U, &
                               HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,KPATCH,ZFIELDIN)
ELSE IF (YFILETYPE=='BUFFER') THEN
   CALL PREP_TEB_GARDEN_BUFFER(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
ELSE
   CALL ABOR1_SFX('PREP_HOR_TEB_GARDEN_FIELD: data file type not supported : '//YFILETYPE)
END IF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Horizontal interpolation
!
INL = SIZE(ZFIELDIN,2)
INP = SIZE(ZFIELDIN,3)
!
ALLOCATE(ZFIELDOUTP(INI,INL,INP))
ALLOCATE(ZFIELD(SIZE(ZFIELDIN,1),INL))
!
DO JPATCH = 1, SIZE(ZFIELDIN,3)
  ZFIELD=ZFIELDIN(:,:,JPATCH)
  IF (INP==NVEGTYPE) LINTERP = (TGDP%XVEGTYPE(:,JPATCH) > 0.)
  CALL HOR_INTERPOL(DTCO, U, &
                    ILUOUT,ZFIELD,ZFIELDOUTP(:,:,JPATCH))
  LINTERP = .TRUE.
END DO
!
DEALLOCATE(ZFIELD)
!
ALLOCATE(ZFIELDOUTV(INI,INL,NVEGTYPE))
!
 CALL PUT_ON_ALL_VEGTYPES(INI,INL,INP,NVEGTYPE,ZFIELDOUTP,ZFIELDOUTV)
!
DEALLOCATE(ZFIELDOUTP)
!
!-------------------------------------------------------------------------------------
!
!*      6.     Transformation from vegtype grid to averaged grid
!
ALLOCATE(ZW (INI,SIZE(ZFIELDOUTV,2)))
ALLOCATE(ZSUM (SIZE(ZFIELDOUTV,1)))
ZW = 0.
!
DO JLAYER=1,SIZE(ZW,2)
  ZSUM(:) = SUM(TGDP%XVEGTYPE(:,:),2,ZFIELDOUTV(:,JLAYER,:)/=XUNDEF)
  DO JVEGTYPE=1,NVEGTYPE
    WHERE (ZFIELDOUTV(:,JLAYER,JVEGTYPE)/=XUNDEF) 
      ZW(:,JLAYER) = ZW(:,JLAYER) + TGDP%XVEGTYPE(:,JVEGTYPE) * ZFIELDOUTV(:,JLAYER,JVEGTYPE) / ZSUM(:)
    END WHERE
  END DO
  DO JI=1,SIZE(ZW,1)
    IF (ALL(ZFIELDOUTV(JI,JLAYER,:)==XUNDEF)) ZW(JI,JLAYER) = XUNDEF
  ENDDO
END DO
!
!-------------------------------------------------------------------------------------
!
!*      7.     Return to historical variable
!
!
SELECT CASE (HSURF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WG     ') 
  ALLOCATE(ZF (SIZE(ZFIELDOUTV,1),TGDO%NGROUND_LAYER))
  !
  !* interpolates on output levels
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,TGDP%XDG,ZF)
  !
  !* retrieves soil water content from soil relative humidity
  ALLOCATE(TGD%CUR%XWG(SIZE(ZFIELDOUTV,1),TGDO%NGROUND_LAYER))
  TGD%CUR%XWG(:,:) = TGDP%XWWILT + ZF(:,:) * (TGDP%XWFC-TGDP%XWWILT)
  TGD%CUR%XWG(:,:) = MAX(MIN(TGD%CUR%XWG(:,:),TGDP%XWSAT),XWGMIN)
  !
  WHERE(ZF(:,:)==XUNDEF)TGD%CUR%XWG(:,:)=XUNDEF
  !
  DEALLOCATE(ZF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WGI    ')
  ALLOCATE(ZF (SIZE(ZFIELDOUTV,1),TGDO%NGROUND_LAYER))
  !
  !* interpolates on output levels
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,TGDP%XDG,ZF)
  !
  !* retrieves soil ice content from soil relative humidity
  ALLOCATE(TGD%CUR%XWGI(SIZE(ZFIELDOUTV,1),TGDO%NGROUND_LAYER))
  TGD%CUR%XWGI(:,:) = ZF(:,:) * TGDP%XWSAT
  TGD%CUR%XWGI(:,:) = MAX(MIN(TGD%CUR%XWGI(:,:),TGDP%XWSAT),0.)
  !
  WHERE(ZF(:,:)==XUNDEF)TGD%CUR%XWGI(:,:)=XUNDEF
  !
  DEALLOCATE(ZF)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('TG     ') 
  IWORK=TGDO%NGROUND_LAYER
  ALLOCATE(TGD%CUR%XTG(SIZE(ZFIELDOUTV,1),IWORK))
  ALLOCATE(ZDG(SIZE(TGDP%XDG,1),IWORK))
  IF (TVG%CISBA=='2-L'.OR.TVG%CISBA=='3-L') THEN
    ZDG(:,1) = 0.01
    ZDG(:,2) = 0.40   ! deep temperature for force-restore taken at 20cm
    IF(TVG%CISBA=='3-L') ZDG(:,3) = 5.00   ! climatological temperature, usually not used
  ELSE
    !* diffusion method, the soil grid is the same as for humidity
    ZDG(:,:) = TGDP%XDG(:,:)
  END IF
  CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW,ZDG,TGD%CUR%XTG)
  DEALLOCATE(ZDG)
  !
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('WR     ') 
  ALLOCATE(TGD%CUR%XWR(SIZE(ZFIELDOUTV,1)))
  TGD%CUR%XWR(:) = ZW(:,1)
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
 CASE('LAI    ') 
  !* LAI is updated only if present and pertinent (evolutive LAI) in input file

   WHERE (ZW(:,1)/=XUNDEF) TGDPE%CUR%XLAI(:) = ZW(:,1)
  !
END SELECT
!
DEALLOCATE(ZW)
!-------------------------------------------------------------------------------------
!
!*      8.     Deallocations
!
DEALLOCATE(ZFIELDIN )
DEALLOCATE(ZFIELDOUTV)
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_GARDEN_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
 CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
SUBROUTINE INIT_FROM_REF_GRID(PGRID1,PT1,PD2,PT2)
!
USE MODI_INTERP_GRID_NAT
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1    ! variable profile
REAL, DIMENSION(:),   INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2    ! variable profile
!
INTEGER                                  :: JI, JL  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PT1,2)) :: ZD1 ! input grid
!
INTEGER :: ILAYER1, ILAYER2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
!
IF (SIZE(PT1,2)==3) THEN
!
!* 1. case with only 3 input levels (typically coming from 'UNIF')
!     -----------------------------
!
  IF (TVG%CISBA=='2-L' .OR. TVG%CISBA=='3-L') THEN
    !* Possible LTEMP_ARP case
    IF(SIZE(PT2,2)>3)THEN
       ILAYER1=3
       ILAYER2=SIZE(PT2,2)
    ELSE
       ILAYER1=SIZE(PT2,2)
       ILAYER2=0
    ENDIF
    !* historical 2L or 3L ISBA version
    PT2(:,1:ILAYER1) = PT1(:,1:ILAYER1) 
    !* Possible LTEMP_ARP case
    IF(ILAYER2>0)THEN
       DO JL=ILAYER1+1,ILAYER2
         PT2(:,JL) = PT2(:,ILAYER1)
       ENDDO
    ENDIF
!    
  ELSEIF(TVG%CISBA=='DIF')THEN
       !surface layer (generally 0.01m imposed)
       PT2(:,1) = PT1(:,1) 
       !deep layers
       DO JL=2,TGDO%NGROUND_LAYER
          PT2(:,JL) = PT1(:,3)
       END DO
       !if root layers
       DO JI=1,SIZE(PT1,1)
          DO JL=2,TGDO%NGROUND_LAYER
             IF(TGDP%XROOTFRAC(JI,JL)<=1.0)THEN 
                PT2(JI,JL) = PT1(JI,2)
                EXIT
             ENDIF
          END DO
       END DO 
  END IF    
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ELSE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!* 2. case with fine grid as input (general case)
!     ----------------------------
!
  DO JL=1,SIZE(PT1,2)
    ZD1(:,JL) = PGRID1(JL)
  END DO
!
  CALL INTERP_GRID_NAT(ZD1,PT1(:,:),PD2,PT2(:,:))
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ENDIF
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_TEB_GARDEN_FIELD
