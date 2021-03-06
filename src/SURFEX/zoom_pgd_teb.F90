!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE ZOOM_PGD_TEB (B, DGCT, DGMT, T, TGD, TGDPE, TGR, TGRPE, &
                               BOP, BDD, DTB, DTCO, DTT, UG, U, TGDO, TGDP, TG, &
                               TOP, TVG, &
                               HPROGRAM,HINIFILE,HINIFILETYPE,OECOCLIMAP,OGARDEN)
!     ###########################################################

!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_DIAG_CUMUL_TEB_n, ONLY : DIAG_CUMUL_TEB_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_GARDEN_n, ONLY : TEB_GARDEN_t
USE MODD_TEB_GARDEN_PGD_EVOL_n, ONLY : TEB_GARDEN_PGD_EVOL_t
USE MODD_TEB_GREENROOF_n, ONLY : TEB_GREENROOF_t
USE MODD_TEB_GREENROOF_PGD_EVOL_n, ONLY : TEB_GREENROOF_PGD_EVOL_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_TEB_GARDEN_OPTION_n, ONLY : TEB_GARDEN_OPTIONS_t
USE MODD_TEB_GARDEN_PGD_n, ONLY : TEB_GARDEN_PGD_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE MODD_DATA_COVER_PAR,  ONLY : JPCOVER
!
USE MODD_PREP,            ONLY : CINGRID_TYPE, CINTERP_TYPE, LINTERP
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_OPEN_AUX_IO_SURF
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_PREP_GRID_EXTERN
USE MODI_PREP_OUTPUT_GRID
USE MODI_READ_SURF
USE MODI_READ_PGD_TEB_PAR_n
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_CLEAN_PREP_OUTPUT_GRID
USE MODI_GOTO_WRAPPER_TEB_PATCH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(DIAG_CUMUL_TEB_t), INTENT(INOUT) :: DGCT
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DGMT
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_GARDEN_PGD_EVOL_t), INTENT(INOUT) :: TGDPE
TYPE(TEB_GARDEN_t), INTENT(INOUT) :: TGD
TYPE(TEB_GREENROOF_t), INTENT(INOUT) :: TGR
TYPE(TEB_GREENROOF_PGD_EVOL_t), INTENT(INOUT) :: TGRPE
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_GARDEN_OPTIONS_t), INTENT(INOUT) :: TGDO
TYPE(TEB_GARDEN_PGD_t), INTENT(INOUT) :: TGDP
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! file to read
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! file type
LOGICAL,              INTENT(IN)  :: OECOCLIMAP  ! flag to use ecoclimap
LOGICAL,              INTENT(IN)  :: OGARDEN     ! flag to use garden
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IRESP   ! error return code
INTEGER :: ILUOUT  ! output listing logical unit
INTEGER :: INI     ! total 1D dimension (input grid)
INTEGER :: JLAYER  ! loop counter
INTEGER :: ILU     ! total 1D dimension (output grid, TOWN points only)
INTEGER :: JPATCH  ! TEB patch
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER           :: IVERSION
INTEGER           :: IBUGFIX
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_TEB',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
TOP%LECOCLIMAP = OECOCLIMAP
TOP%LGARDEN = OGARDEN
!
IF (.NOT. OECOCLIMAP) THEN
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) 'Ecoclimap is not used'
  WRITE(ILUOUT,*) 'Routine zoom_pgd_teb.f90 must be updated'
  WRITE(ILUOUT,*) 'to interpolate all TEB physiographic fields'
  CALL ABOR1_SFX('ZOOM_PGD_TEB: ECOCLIMAP NOT USED, ROUTINE MUST BE UPDATED')
END IF
!
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
 CALL OPEN_AUX_IO_SURF(&
                       HINIFILE,HINIFILETYPE,'FULL  ')
!
 CALL GOTO_WRAPPER_TEB_PATCH(B, DGCT, DGMT, T, TGD, TGDPE, TGR, TGRPE, 1)
!-------------------------------------------------------------------------------
!
!*    2.      Number of points and packing of general fields
!             ----------------------------------------------
!
!
 CALL GET_SURF_SIZE_n(DTCO, U, &
                      'TOWN  ',ILU)
!
ALLOCATE(TOP%LCOVER     (JPCOVER))
ALLOCATE(TOP%XZS        (ILU))
ALLOCATE(TG%XLAT       (ILU))
ALLOCATE(TG%XLON       (ILU))
ALLOCATE(TG%XMESH_SIZE (ILU))
!
 CALL PACK_PGD(DTCO, U, &
               HPROGRAM, 'TOWN  ',                      &
                TG%CGRID,  TG%XGRID_PAR,                     &
                TOP%LCOVER, TOP%XCOVER, TOP%XZS,                   &
                TG%XLAT, TG%XLON, TG%XMESH_SIZE                 )  
!
TG%NDIM = ILU
!
!
 CALL READ_SURF(&
                HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(&
                HPROGRAM,'BUG',IBUGFIX,IRESP)
!------------------------------------------------------------------------------
!
!*      3.     Reading of grid
!              ---------------
!
 CALL PREP_GRID_EXTERN(&
                       HINIFILETYPE,ILUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
 CALL PREP_OUTPUT_GRID(UG, U, &
                       ILUOUT,TG%CGRID,TG%XGRID_PAR,TG%XLAT,TG%XLON)
!
!
!------------------------------------------------------------------------------
!
!*      4.     Reading & interpolation of fields
!              ---------------------------------
!
!
IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<=2) THEN
  TOP%NTEB_PATCH=1
ELSE
  CALL READ_SURF(&
                HPROGRAM,'TEB_PATCH',TOP%NTEB_PATCH,IRESP)
END IF

!
 CALL READ_SURF(&
                HPROGRAM,'ROOF_LAYER',TOP%NROOF_LAYER,IRESP)
 CALL READ_SURF(&
                HPROGRAM,'ROAD_LAYER',TOP%NROAD_LAYER,IRESP)
 CALL READ_SURF(&
                HPROGRAM,'WALL_LAYER',TOP%NWALL_LAYER,IRESP)
!
IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=2)) THEN
  TOP%CBLD_ATYPE='ARI'
  TOP%CBEM = 'DEF'
ELSE
  CALL READ_SURF(&
                HPROGRAM,'BLD_ATYPE' ,TOP%CBLD_ATYPE,IRESP)
  CALL READ_SURF(&
                HPROGRAM,'BEM'       ,TOP%CBEM      ,IRESP)
END IF
!
IF (TOP%CBEM/='DEF') THEN
  CALL READ_SURF(&
                HPROGRAM,'FLOOR_LAYER',BOP%NFLOOR_LAYER,IRESP)
END IF
!
DO JPATCH=1,TOP%NTEB_PATCH
  CALL GOTO_WRAPPER_TEB_PATCH(B, DGCT, DGMT, T, TGD, TGDPE, TGR, TGRPE, JPATCH)
  CALL READ_PGD_TEB_PAR_n(DTCO, U, &
                          BDD, DTB, DTT, TG, TOP, &
                          HPROGRAM,INI,'A')
!
!------------------------------------------------------------------------------
!
!*      5.     Gardens
!              -------
!
  IF (TOP%LGARDEN) CALL ZOOM_PGD_TEB_GARDEN
END DO
!
 CALL CLOSE_AUX_IO_SURF(HINIFILE,HINIFILETYPE)
!
 CALL CLEAN_PREP_OUTPUT_GRID
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_TEB',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
 CONTAINS
!
SUBROUTINE ZOOM_PGD_TEB_GARDEN
!
USE MODI_HOR_INTERPOL
!
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), POINTER     :: ZIN     ! field  on all surface points
!
REAL, DIMENSION(INI)              :: ZFIELD  ! field read
REAL, DIMENSION(ILU,1)            :: ZOUT    ! final field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_TEB:ZOOM_PGD_TEB_GARDEN',0,ZHOOK_HANDLE)
!
LINTERP(:) = .TRUE.
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  CALL READ_SURF(&
                HPROGRAM,'GD_LAYER',TGDO%NGROUND_LAYER,IRESP)
  CALL READ_SURF(&
                HPROGRAM,'GD_ISBA',TVG%CISBA,IRESP)
  CALL READ_SURF(&
                HPROGRAM,'GD_PHOTO',TVG%CPHOTO,IRESP)
  CALL READ_SURF(&
                HPROGRAM,'GD_PEDOTF',TVG%CPEDOTF,IRESP)
  TVG%NNBIOMASS=1
  IF (TVG%CPHOTO=='NIT') TVG%NNBIOMASS=3  
ELSE
  CALL READ_SURF(&
                HPROGRAM,'TWN_LAYER',TGDO%NGROUND_LAYER,IRESP)
  CALL READ_SURF(&
                HPROGRAM,'TWN_ISBA',TVG%CISBA,IRESP)
  CALL READ_SURF(&
                HPROGRAM,'TWN_PHOTO',TVG%CPHOTO,IRESP)
  CALL READ_SURF(&
                HPROGRAM,'TWN_PEDOTF',TVG%CPEDOTF,IRESP)
  CALL READ_SURF(&
                HPROGRAM,'TWN_NBIOMASS',TVG%NNBIOMASS,IRESP)
ENDIF
!
!* sand
!
ALLOCATE(ZIN(INI,TGDO%NGROUND_LAYER))
YRECFM='TWN_SAND'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SAND'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,ZFIELD,IRESP,HDIR='A')
DO JLAYER=1,TGDO%NGROUND_LAYER
  ZIN(:,JLAYER) = ZFIELD(:)
END DO
ALLOCATE(TGDP%XSAND(ILU,TGDO%NGROUND_LAYER))
 CALL HOR_INTERPOL(DTCO, U, &
                   ILUOUT,ZIN,TGDP%XSAND)
DEALLOCATE(ZIN)
!
!* clay
!
ALLOCATE(ZIN(INI,TGDO%NGROUND_LAYER))
YRECFM='TWN_CLAY'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_CLAY'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,ZFIELD,IRESP,HDIR='A')
DO JLAYER=1,TGDO%NGROUND_LAYER
  ZIN(:,JLAYER) = ZFIELD(:)
END DO
ALLOCATE(TGDP%XCLAY(ILU,TGDO%NGROUND_LAYER))
 CALL HOR_INTERPOL(DTCO, U, &
                   ILUOUT,ZIN,TGDP%XCLAY)
DEALLOCATE(ZIN)
!
!* runoff & drainage
!
ALLOCATE(ZIN(INI,1))
YRECFM='TWN_RUNOFFB'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_RUNOFFB'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,ZFIELD,IRESP,HDIR='A')
ZIN(:,1) = ZFIELD(:)
ALLOCATE(TGDP%XRUNOFFB(ILU))
 CALL HOR_INTERPOL(DTCO, U, &
                   ILUOUT,ZIN,ZOUT)
TGDP%XRUNOFFB(:) = ZOUT(:,1)
!
IF (IVERSION<=3) THEN
  TGDP%XWDRAIN = 0.
ELSE
 YRECFM='TWN_WDRAIN'
 IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_WDRAIN'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,ZFIELD,IRESP,HDIR='A')
 ZIN(:,1) = ZFIELD(:)
 ALLOCATE(TGDP%XWDRAIN(ILU))
 CALL HOR_INTERPOL(DTCO, U, &
                   ILUOUT,ZIN,ZOUT)
 TGDP%XWDRAIN(:) = ZOUT(:,1)
ENDIF
!
DEALLOCATE(ZIN)
!
!* other garden parameters
!
 CALL READ_SURF(&
                HPROGRAM,'PAR_GARDEN',TGDO%LPAR_GARDEN,IRESP)
!
!!
IF (TGDO%LPAR_GARDEN) THEN
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) 'Specific garden fields are prescribed'
  WRITE(ILUOUT,*) 'Routine zoom_pgd_teb.f90 must be updated'
  WRITE(ILUOUT,*) 'to interpolate all TEB physiographic garden fields'
  CALL ABOR1_SFX('ZOOM_PGD_TEB: GARDEN fields used, ROUTINE MUST BE UPDATED')
END IF
!
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_TEB:ZOOM_PGD_TEB_GARDEN',1,ZHOOK_HANDLE)
!
END SUBROUTINE ZOOM_PGD_TEB_GARDEN
!_______________________________________________________________________________
!
END SUBROUTINE ZOOM_PGD_TEB
