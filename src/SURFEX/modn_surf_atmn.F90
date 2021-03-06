!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      MODULE MODN_SURF_ATM_n
!     ######################
!
!!****  *MODN_SURF_ATM_n* - declaration of namelist NAM_SURF_ATMn
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_SURF_ATMn
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      P. Tulet    flag namelist  for emission      02/2004  
!!      B. Decharme Cumulative diag for all Tile     08/2009
!!      B. Decharme Key to allow (or not) writting diag   10/2009
!!      S.Senesi    Additional write selection mechanism   12/2009
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!

!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=28), SAVE  :: CCHEM_SURF_FILE
LOGICAL, SAVE  :: LCH_SURF_EMIS
LOGICAL  :: LFRAC
REAL     :: XDIAG_TSTEP
INTEGER  :: N2M
LOGICAL  :: LT2MMW
LOGICAL  :: L2M_MIN_ZS
LOGICAL  :: LSURF_BUDGET
LOGICAL  :: LRAD_BUDGET
LOGICAL  :: LSURF_BUDGETC
LOGICAL  :: LRESET_BUDGETC
LOGICAL  :: LCOEF
LOGICAL  :: LSURF_VARS
LOGICAL  :: LDIAG_GRID
LOGICAL  :: LPROVAR_TO_DIAG
LOGICAL  :: LSELECT
 CHARACTER(LEN=12), DIMENSION(500)    :: CSELECT
!
NAMELIST/NAM_CH_CONTROLn/CCHEM_SURF_FILE
NAMELIST/NAM_CH_SURFn/LCH_SURF_EMIS
NAMELIST/NAM_DIAG_SURF_ATMn/LFRAC, LDIAG_GRID, LT2MMW
NAMELIST/NAM_DIAG_SURFn/N2M, L2M_MIN_ZS, LSURF_BUDGET, LRAD_BUDGET, LSURF_BUDGETC,  &
                        LRESET_BUDGETC, LCOEF, LSURF_VARS
!                        
NAMELIST/NAM_WRITE_DIAG_SURFn/LPROVAR_TO_DIAG,LSELECT,CSELECT
!
 CONTAINS
!
SUBROUTINE INIT_NAM_CH_CONTROLn (CHU)
!
  USE MODD_CH_SURF_n, ONLY : CH_SURF_t
!
  IMPLICIT NONE

!
  TYPE(CH_SURF_t), INTENT(INOUT) :: CHU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_CH_CONTROLN',0,ZHOOK_HANDLE)
  CCHEM_SURF_FILE = CHU%CCHEM_SURF_FILE
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_CH_CONTROLN',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_CH_CONTROLn

SUBROUTINE UPDATE_NAM_CH_CONTROLn (CHU)
!
  USE MODD_CH_SURF_n, ONLY : CH_SURF_t
!
  IMPLICIT NONE

!
  TYPE(CH_SURF_t), INTENT(INOUT) :: CHU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_CH_CONTROLN',0,ZHOOK_HANDLE)
  CHU%CCHEM_SURF_FILE = CCHEM_SURF_FILE
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_CH_CONTROLN',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_CH_CONTROLn

SUBROUTINE INIT_NAM_CH_SURFn (CHU)
!
  USE MODD_CH_SURF_n, ONLY : CH_SURF_t
!
  IMPLICIT NONE

!
  TYPE(CH_SURF_t), INTENT(INOUT) :: CHU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_CH_SURFN',0,ZHOOK_HANDLE)
  LCH_SURF_EMIS = CHU%LCH_SURF_EMIS
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_CH_SURFN',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_CH_SURFn

SUBROUTINE UPDATE_NAM_CH_SURFn (CHU)
!
  USE MODD_CH_SURF_n, ONLY : CH_SURF_t
!
  IMPLICIT NONE

!
  TYPE(CH_SURF_t), INTENT(INOUT) :: CHU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_CH_SURFN',0,ZHOOK_HANDLE)
  CHU%LCH_SURF_EMIS = LCH_SURF_EMIS
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_CH_SURFN',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_CH_SURFn

SUBROUTINE INIT_NAM_DIAG_SURF_ATMn (DGU)
!
  USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
  IMPLICIT NONE

!
  TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_DIAG_SURF_ATMN',0,ZHOOK_HANDLE)
  LFRAC = DGU%LFRAC
  LT2MMW = DGU%LT2MMW
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_DIAG_SURF_ATMN',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_DIAG_SURF_ATMn

SUBROUTINE UPDATE_NAM_DIAG_SURF_ATMn (DGU)
!
  USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
  IMPLICIT NONE

!
  TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_DIAG_SURF_ATMN',0,ZHOOK_HANDLE)
  DGU%LFRAC = LFRAC
  DGU%LT2MMW = LT2MMW
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_DIAG_SURF_ATMN',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_DIAG_SURF_ATMn

SUBROUTINE INIT_NAM_DIAG_SURFn (DGU)
!
  USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
  IMPLICIT NONE

!
  TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_DIAG_SURFN',0,ZHOOK_HANDLE)
  XDIAG_TSTEP = DGU%XDIAG_TSTEP
  N2M = DGU%N2M
  L2M_MIN_ZS = DGU%L2M_MIN_ZS
  LSURF_BUDGET = DGU%LSURF_BUDGET
  LRAD_BUDGET = DGU%LRAD_BUDGET
  LSURF_BUDGETC = DGU%LSURF_BUDGETC
  LRESET_BUDGETC = DGU%LRESET_BUDGETC
  LCOEF = DGU%LCOEF
  LSURF_VARS = DGU%LSURF_VARS
  LDIAG_GRID = DGU%LDIAG_GRID
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_DIAG_SURFN',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_DIAG_SURFn

SUBROUTINE UPDATE_NAM_DIAG_SURFn (DGU)
!
  USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
  IMPLICIT NONE

!
  TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_DIAG_SURFN',0,ZHOOK_HANDLE)
  DGU%XDIAG_TSTEP = XDIAG_TSTEP
  DGU%N2M = N2M
  DGU%L2M_MIN_ZS = L2M_MIN_ZS
  DGU%LSURF_BUDGET = LSURF_BUDGET
  DGU%LRAD_BUDGET = LRAD_BUDGET
  DGU%LSURF_BUDGETC = LSURF_BUDGETC
  DGU%LRESET_BUDGETC = LRESET_BUDGETC 
  DGU%LCOEF = LCOEF
  DGU%LSURF_VARS = LSURF_VARS
  DGU%LDIAG_GRID = LDIAG_GRID
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_DIAG_SURFN',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_DIAG_SURFn


SUBROUTINE INIT_NAM_WRITE_DIAG_SURFn (DGU)
!
  USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
  IMPLICIT NONE


!
  TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_WRITE_DIAG_SURFN',0,ZHOOK_HANDLE)
  LPROVAR_TO_DIAG = DGU%LPROVAR_TO_DIAG
  LSELECT = DGU%LSELECT
  CSELECT(:) = '            '
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:INIT_NAM_WRITE_DIAG_SURFN',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_WRITE_DIAG_SURFn

SUBROUTINE UPDATE_NAM_WRITE_DIAG_SURFn (DGU)
!
  USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
  IMPLICIT NONE
!
  TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
  INTEGER :: ICOUNT
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_WRITE_DIAG_SURFN',0,ZHOOK_HANDLE)
  DGU%LPROVAR_TO_DIAG = LPROVAR_TO_DIAG
!
  DGU%LSELECT = LSELECT
  IF (LSELECT) THEN
    ICOUNT = COUNT(CSELECT /= '            ')
    IF(.NOT.ASSOCIATED(DGU%CSELECT))THEN
      ALLOCATE(DGU%CSELECT(ICOUNT))
      DGU%CSELECT(:) = '            '
    ENDIF
    DGU%CSELECT = CSELECT(1:ICOUNT)
  ENDIF
IF (LHOOK) CALL DR_HOOK('MODN_SURF_ATM_N:UPDATE_NAM_WRITE_DIAG_SURFN',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_WRITE_DIAG_SURFn

END MODULE MODN_SURF_ATM_n
