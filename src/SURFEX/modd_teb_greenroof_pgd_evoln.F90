!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODD_TEB_GREENROOF_PGD_EVOL_n
!##################
!
!!****  *MODD_TEB_GREENROOF - declaration of ISBA scheme packed surface parameters for urban green roofs
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       09/2009
!!      C. de Munck     06/2011 
!!      V. Masson       06/2013  splits module in 4
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE TEB_GREENROOF_PGD_EVOL_1P_t
! - Vegetation: Ags Prognostic (YPHOTO = ('LAI', 'LST', or 'NIT') or prescribed (YPHOTO='NON', 'AGS' or 'LST')
!
  REAL, POINTER, DIMENSION(:)   :: XLAI              ! Leaf Area Index                         (m2/m2)
  REAL, POINTER, DIMENSION(:)   :: XVEG              ! vegetation cover fraction               (-)
  REAL, POINTER, DIMENSION(:)   :: XALBNIR           ! near-infra-red albedo                   (-)
  REAL, POINTER, DIMENSION(:)   :: XALBVIS           ! visible albedo                          (-)
  REAL, POINTER, DIMENSION(:)   :: XALBUV            ! UV albedo                               (-)
  REAL, POINTER, DIMENSION(:)   :: XEMIS             ! surface emissivity                      (-)
  REAL, POINTER, DIMENSION(:)   :: XZ0               ! surface roughness length                (m)
!
END TYPE TEB_GREENROOF_PGD_EVOL_1P_t

TYPE TEB_GREENROOF_PGD_EVOL_t
  !
  TYPE(TEB_GREENROOF_PGD_EVOL_1P_t), POINTER :: ALP(:) => NULL()
  TYPE(TEB_GREENROOF_PGD_EVOL_1P_t), POINTER :: CUR => NULL()
  !
END TYPE TEB_GREENROOF_PGD_EVOL_t



 CONTAINS

!


!

SUBROUTINE TEB_GREENROOF_PGD_EVOL_GOTO_PATCH(YTEB_GREENROOF_PGD_EVOL,KTO_PATCH)
TYPE(TEB_GREENROOF_PGD_EVOL_t), INTENT(INOUT) :: YTEB_GREENROOF_PGD_EVOL
INTEGER, INTENT(IN) :: KTO_PATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Current patch is set to patch KTO_PATCH
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_PGD_EVOL_N:TEB_GREENROOF_PGD_EVOL_GOTO_PATCH',0,ZHOOK_HANDLE)

YTEB_GREENROOF_PGD_EVOL%CUR => YTEB_GREENROOF_PGD_EVOL%ALP(KTO_PATCH)

IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_PGD_EVOL_N:TEB_GREENROOF_PGD_EVOL_GOTO_PATCH',1,ZHOOK_HANDLE)
!
END SUBROUTINE TEB_GREENROOF_PGD_EVOL_GOTO_PATCH

SUBROUTINE TEB_GREENROOF_PGD_EVOL_INIT(YTEB_GREENROOF_PGD_EVOL,KPATCH)
TYPE(TEB_GREENROOF_PGD_EVOL_t), INTENT(INOUT) :: YTEB_GREENROOF_PGD_EVOL
INTEGER, INTENT(IN) :: KPATCH
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_EVOL_INIT",0,ZHOOK_HANDLE)
 ALLOCATE(YTEB_GREENROOF_PGD_EVOL%ALP(KPATCH))
YTEB_GREENROOF_PGD_EVOL%CUR => YTEB_GREENROOF_PGD_EVOL%ALP(1)
DO JP=1,KPATCH
NULLIFY(YTEB_GREENROOF_PGD_EVOL%ALP(JP)%XALBNIR)
NULLIFY(YTEB_GREENROOF_PGD_EVOL%ALP(JP)%XALBVIS)
NULLIFY(YTEB_GREENROOF_PGD_EVOL%ALP(JP)%XALBUV)
NULLIFY(YTEB_GREENROOF_PGD_EVOL%ALP(JP)%XEMIS)
NULLIFY(YTEB_GREENROOF_PGD_EVOL%ALP(JP)%XZ0)
NULLIFY(YTEB_GREENROOF_PGD_EVOL%ALP(JP)%XVEG)
NULLIFY(YTEB_GREENROOF_PGD_EVOL%ALP(JP)%XLAI)
ENDDO 
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_PGD_EVOL_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_PGD_EVOL_INIT


END MODULE MODD_TEB_GREENROOF_PGD_EVOL_n
