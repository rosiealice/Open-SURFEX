!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HOR_EXTRAPOL_SURF(KLUOUT,HCOORTYPE,PLAT_IN,PLON_IN,PFIELD_IN, &
                                         PLAT,PLON,PFIELD,OINTERP)  
!     ###################################################################
!
!!**** *HOR_EXTRAPOL_SURF* extrapolate a surface field
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!       For each point to interpolate, the nearest valid point value is set.
!!
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
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/12/98
!!     V. Masson    01/2004 extrapolation in latitude and longitude
!!     M. Jidane    11/2013 add OpenMP directives
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,               INTENT(IN)     :: KLUOUT   ! output listing logical unit
 CHARACTER(LEN=4),      INTENT(IN)     :: HCOORTYPE! type of coordinate
REAL,   DIMENSION(:),  INTENT(IN)     :: PLAT_IN  ! input lat. of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PLON_IN  ! input lon. of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PFIELD_IN! input field on grid mesh
REAL,   DIMENSION(:),  INTENT(IN)     :: PLAT     ! latitude of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PLON     ! longitude of each grid mesh.
REAL,   DIMENSION(:),  INTENT(INOUT)  :: PFIELD   ! field on grid mesh
LOGICAL,DIMENSION(:),  INTENT(IN)     :: OINTERP  ! .true. where physical value is needed
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER  :: INO     ! output array size
INTEGER  :: INO_IN  ! input  array size
!
REAL     :: ZLAT  ! latitude of point to define
REAL     :: ZLON  ! longitude of point to define
REAL     :: ZDIST ! current distance to valid point (in lat/lon grid)
REAL     :: ZFIELD! current found field value
REAL     :: ZNDIST! smallest distance to valid point
REAL     :: ZCOSLA! cosine of latitude
!
INTEGER  :: JI    ! loop index on points
INTEGER  :: JISC  ! loop index on valid points
REAL     :: ZLONSC! longitude of valid point
LOGICAL  :: GLALO ! flag true is second coordinate is a longitude or pseudo-lon.
                  !      false if metric coordinates
!
REAL(KIND=JPRB) :: ZRAD ! conversion degrees to radians
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF',0,ZHOOK_HANDLE)
!
INO = SIZE(PFIELD,1)
!
WHERE (.NOT. OINTERP(:)) PFIELD(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
INO_IN = SIZE(PFIELD_IN)
!
GLALO = HCOORTYPE=='LALO'
!
!-------------------------------------------------------------------------------
!
!*    3.     No data point
!            -------------
!
IF (COUNT(PFIELD_IN(:)/=XUNDEF)==0 .AND. LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF',1,ZHOOK_HANDLE)
IF (COUNT(PFIELD_IN(:)/=XUNDEF)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*      4.   Loop on points to define
!            ------------------------
!
ZRAD=XPI/180.0_JPRB
!
!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,JISC,ZLAT,ZLON,ZFIELD,ZCOSLA,ZLONSC,ZDIST,ZNDIST,ZHOOK_HANDLE_OMP)
DO JI=1,INO
  IF (PFIELD(JI)/=XUNDEF) CYCLE
  IF (.NOT. OINTERP(JI))  CYCLE
!
!*      4.1  initialisation
!            --------------
!
  IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF OMP',0,ZHOOK_HANDLE_OMP)
  ZNDIST=1.E20
  ZLAT=PLAT(JI)
  ZLON=PLON(JI)
  ZFIELD=PFIELD(JI)
  ZCOSLA=COS(ZLAT*ZRAD)
!
!*      4.2  extrapolation with nearest valid point
!            --------------------------------------
!
  DO JISC=1,INO_IN
    IF (PFIELD_IN(JISC)/=XUNDEF) THEN
      ZLONSC = PLON_IN(JISC)
      IF (GLALO) THEN
        IF (ZLONSC-ZLON> 180.) ZLONSC = ZLONSC - 360.
        IF (ZLONSC-ZLON<-180.) ZLONSC = ZLONSC + 360.
        ZDIST= (PLAT_IN(JISC)-ZLAT) ** 2 + ((ZLONSC-ZLON)*ZCOSLA) ** 2
      ELSE
        ZDIST= (PLAT_IN(JISC)-ZLAT) ** 2 + (ZLONSC-ZLON) ** 2
      END IF
      IF (ZDIST<=ZNDIST) THEN
        ZFIELD=PFIELD_IN(JISC)
        ZNDIST=ZDIST
      END IF
    END IF
  END DO
  PFIELD(JI) = ZFIELD

  IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF OMP',1,ZHOOK_HANDLE_OMP)
END DO
!$OMP END PARALLEL DO
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE HOR_EXTRAPOL_SURF
