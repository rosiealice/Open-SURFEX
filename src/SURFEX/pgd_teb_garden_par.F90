!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_GARDEN_PAR (DTCO, UG, U, USS, TG, GDM, &
                                     HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_GARDEN_PAR* monitor for averaging and interpolations of cover fractions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    A. Lemonsu       Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2009
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
!
USE MODD_DATA_COVER_PAR,    ONLY : NVEGTYPE
USE MODD_SURF_PAR,          ONLY : XUNDEF
!
USE MODD_PGDWORK,           ONLY : CATYPE
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_FIELD
USE MODI_ABOR1_SFX
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
LOGICAL               :: GNO_PAR_GARDEN ! true no fraction is prescribed
INTEGER               :: JTIME     ! loop counter on time
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                                :: NTIME_GD
INTEGER, PARAMETER                     :: NGROUND_MAX  = 20
INTEGER, PARAMETER                     :: NVEGTYPE_MAX = 19
INTEGER, PARAMETER                     :: NTIME_MAX    = 12
!
! type of vegetation
!
 CHARACTER(LEN=4)                       :: CTYP_GARDEN_HVEG ! type of high vegetation
 CHARACTER(LEN=4)                       :: CTYP_GARDEN_LVEG ! type of low  vegetation
 CHARACTER(LEN=4)                       :: CTYP_GARDEN_NVEG ! type of bare soil
!
! uniform value
!
REAL                                   :: XUNIF_FRAC_HVEG  ! fractions of high vegetation
REAL                                   :: XUNIF_FRAC_LVEG  ! fractions of low  vegetation
REAL                                   :: XUNIF_FRAC_NVEG  ! fractions of bare soil
REAL,DIMENSION(NTIME_MAX)              :: XUNIF_LAI_HVEG   ! LAI       of high vegetation
REAL,DIMENSION(NTIME_MAX)              :: XUNIF_LAI_LVEG   ! LAI       of low  vegetation
REAL                                   :: XUNIF_H_HVEG     ! height of trees
!
! name of files containing data
!
 CHARACTER(LEN=28)                      :: CFNAM_FRAC_HVEG  ! fractions of high vegetation
 CHARACTER(LEN=28)                      :: CFNAM_FRAC_LVEG  ! fractions of low  vegetation
 CHARACTER(LEN=28)                      :: CFNAM_FRAC_NVEG  ! fractions of bare soil
 CHARACTER(LEN=28),DIMENSION(NTIME_MAX) :: CFNAM_LAI_HVEG   ! LAI       of high vegetation
 CHARACTER(LEN=28),DIMENSION(NTIME_MAX) :: CFNAM_LAI_LVEG   ! LAI       of low  vegetation
 CHARACTER(LEN=28)                      :: CFNAM_H_HVEG     ! height of trees
!
! type of files containing data
!
 CHARACTER(LEN=28)                      :: CFTYP_FRAC_HVEG  ! fractions of high vegetation
 CHARACTER(LEN=28)                      :: CFTYP_FRAC_LVEG  ! fractions of low  vegetation
 CHARACTER(LEN=28)                      :: CFTYP_FRAC_NVEG  ! fractions of bare soil
 CHARACTER(LEN=28),DIMENSION(NTIME_MAX) :: CFTYP_LAI_HVEG   ! LAI       of high vegetation
 CHARACTER(LEN=28),DIMENSION(NTIME_MAX) :: CFTYP_LAI_LVEG   ! LAI       of low  vegetation
 CHARACTER(LEN=28)                      :: CFTYP_H_HVEG     ! height of trees
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_TEB_GARDEN/   NTIME_GD,                                          &
                                CTYP_GARDEN_HVEG, CTYP_GARDEN_LVEG,                &
                                CTYP_GARDEN_NVEG,                                  &
                                XUNIF_FRAC_HVEG, XUNIF_FRAC_LVEG, XUNIF_FRAC_NVEG, &
                                XUNIF_LAI_HVEG , XUNIF_LAI_LVEG ,                  &
                                XUNIF_H_HVEG   ,                                   &
                                CFNAM_FRAC_HVEG, CFNAM_FRAC_LVEG, CFNAM_FRAC_NVEG, &
                                CFNAM_LAI_HVEG , CFNAM_LAI_LVEG ,                  &
                                CFNAM_H_HVEG   ,                                   &
                                CFTYP_FRAC_HVEG, CFTYP_FRAC_LVEG, CFTYP_FRAC_NVEG, &
                                CFTYP_LAI_HVEG , CFTYP_LAI_LVEG ,                  &
                                CFTYP_H_HVEG  

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GARDEN_PAR',0,ZHOOK_HANDLE)

NTIME_GD = 12
!
 CTYP_GARDEN_HVEG   = 'TEBD'           ! Temperate broadleaf trees (forest)
 CTYP_GARDEN_LVEG   = 'PARK'           ! Grassland
 CTYP_GARDEN_NVEG   = 'NO  '           ! No vegetation
!
XUNIF_FRAC_HVEG    = XUNDEF
XUNIF_FRAC_LVEG    = XUNDEF
XUNIF_FRAC_NVEG    = XUNDEF
XUNIF_LAI_HVEG     = XUNDEF
XUNIF_LAI_LVEG     = XUNDEF
XUNIF_H_HVEG       = XUNDEF
!
 CFNAM_FRAC_HVEG    = '                            '
 CFNAM_FRAC_LVEG    = '                            '
 CFNAM_FRAC_NVEG    = '                            '
 CFNAM_LAI_HVEG     = '                            '
 CFNAM_LAI_LVEG     = '                            '
 CFNAM_H_HVEG       = '                            '
!
 CFTYP_FRAC_HVEG    = '      '
 CFTYP_FRAC_LVEG    = '      '
 CFTYP_FRAC_NVEG    = '      '
 CFTYP_LAI_HVEG     = '      '
 CFTYP_LAI_LVEG     = '      '
 CFTYP_H_HVEG       = '      '
!
!-------------------------------------------------------------------------------
GDM%DTGD%NTIME = 12
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_TEB_GARDEN',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_TEB_GARDEN)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
IF (NTIME_GD==1) THEN
  XUNIF_LAI_HVEG(2:) = XUNIF_LAI_HVEG(1)
  XUNIF_LAI_LVEG(2:) = XUNIF_LAI_LVEG(1)
ELSE IF (NTIME_GD/=12) THEN
  CALL ABOR1_SFX( 'Namelist NAM_DATA_TEB_GARDEN: NTIME_GD must be equal to 1 or 12')
END IF
!-------------------------------------------------------------------------------
!
!*    3.      Coherence check
!             ---------------
!
GDM%TGDO%LPAR_GARDEN =  (XUNIF_FRAC_HVEG /= XUNDEF .OR. LEN_TRIM(CFNAM_FRAC_HVEG) >0 )&
         .AND. (XUNIF_FRAC_LVEG /= XUNDEF .OR. LEN_TRIM(CFNAM_FRAC_LVEG) >0 )&
         .AND. (XUNIF_FRAC_NVEG /= XUNDEF .OR. LEN_TRIM(CFNAM_FRAC_NVEG) >0 )

GNO_PAR_GARDEN = (XUNIF_FRAC_HVEG == XUNDEF .AND. LEN_TRIM(CFNAM_FRAC_HVEG)==0)&
           .AND. (XUNIF_FRAC_LVEG == XUNDEF .AND. LEN_TRIM(CFNAM_FRAC_LVEG)==0)&
           .AND. (XUNIF_FRAC_NVEG == XUNDEF .AND. LEN_TRIM(CFNAM_FRAC_NVEG)==0)

IF ( .NOT. GDM%TGDO%LPAR_GARDEN .AND. .NOT. GNO_PAR_GARDEN ) THEN
  WRITE(ILUOUT,*) ' Error for fraction of high, low and no vegetation fractions in gardens '
  WRITE(ILUOUT,*) ' You need to specify the three of them ... or none. '
  CALL ABOR1_SFX( 'Namelist NAM_DATA_TEB_GARDEN: you need to specify all of  HVEG, LVEG, NVEG fractions or NONE of them')
END IF
!
IF (GNO_PAR_GARDEN) THEN
  IF (LHOOK) CALL DR_HOOK('PGD_TEB_GARDEN_PAR',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
GDM%DTGD%NTIME = NTIME_GD
!
ALLOCATE(GDM%DTGD%XDATA_FRAC_HVEG   (TG%NDIM        ))
ALLOCATE(GDM%DTGD%XDATA_FRAC_LVEG   (TG%NDIM        ))
ALLOCATE(GDM%DTGD%XDATA_FRAC_NVEG   (TG%NDIM        ))
ALLOCATE(GDM%DTGD%XDATA_LAI_HVEG    (TG%NDIM,GDM%DTGD%NTIME))
ALLOCATE(GDM%DTGD%XDATA_LAI_LVEG    (TG%NDIM,GDM%DTGD%NTIME))
ALLOCATE(GDM%DTGD%XDATA_H_HVEG      (TG%NDIM        ))
!
GDM%TGDP%CTYPE_HVEG = CTYP_GARDEN_HVEG
GDM%TGDP%CTYPE_LVEG = CTYP_GARDEN_LVEG
GDM%TGDP%CTYPE_NVEG = CTYP_GARDEN_NVEG
!
!-------------------------------------------------------------------------------
!
!*    3.      Uniform fields are prescribed
!             -----------------------------
!
 CATYPE = 'ARI'
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'FRAC_HVEG: fraction of high vegetation','TWN',CFNAM_FRAC_HVEG,   &
                 CFTYP_FRAC_HVEG,XUNIF_FRAC_HVEG,GDM%DTGD%XDATA_FRAC_HVEG(:))  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'FRAC_LVEG: fraction of low vegetation' ,'TWN',CFNAM_FRAC_LVEG,   &
                 CFTYP_FRAC_LVEG,XUNIF_FRAC_LVEG,GDM%DTGD%XDATA_FRAC_LVEG(:))  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'FRAC_NVEG: fraction of bare soil'      ,'TWN',CFNAM_FRAC_NVEG,   &
                 CFTYP_FRAC_NVEG,XUNIF_FRAC_NVEG,GDM%DTGD%XDATA_FRAC_NVEG(:))  
!
!
DO JTIME=1,GDM%DTGD%NTIME
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'LAI_HVEG: LAI of high vegetation','TWN',CFNAM_LAI_HVEG(JTIME),  &
                  CFTYP_LAI_HVEG(JTIME),XUNIF_LAI_HVEG(JTIME),GDM%DTGD%XDATA_LAI_HVEG(:,JTIME))  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'LAI_LVEG: LAI of low  vegetation','TWN',CFNAM_LAI_LVEG(JTIME),  &
                  CFTYP_LAI_LVEG(JTIME),XUNIF_LAI_LVEG(JTIME),GDM%DTGD%XDATA_LAI_LVEG(:,JTIME))  
!
!
ENDDO
!
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'H_HVEG: height of trees','TWN',CFNAM_H_HVEG,                     &
                 CFTYP_H_HVEG,XUNIF_H_HVEG,GDM%DTGD%XDATA_H_HVEG(:))  
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GARDEN_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB_GARDEN_PAR
