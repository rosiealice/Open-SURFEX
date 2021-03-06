!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################################
SUBROUTINE SSO_BELJAARS04 (USS, &
                           KI,KLVL,PZ,PSSO_STDEV,PU,PFORC_U,PDFORC_UDU)
!     ###############################################################################
!
!!****  *SSO_BELJAARS04_n * - prepares forcing for canopy air model
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
!!      Original    07/2006
!!---------------------------------------------------------------
!
!
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
INTEGER,                  INTENT(IN)    :: KI        ! number of points
INTEGER,                  INTENT(IN)    :: KLVL      ! number of levels in canopy
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PZ        ! heights of canopy levels              (m)
REAL, DIMENSION(KI),      INTENT(IN)    :: PSSO_STDEV! Subgrid scale orography standard dev. (m)

REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PU        ! wind for each canopy layer            (m/s)
!
REAL, DIMENSION(KI,KLVL), INTENT(INOUT)   :: PFORC_U   ! tendency of wind due to canopy drag   (m/s2)
REAL, DIMENSION(KI,KLVL), INTENT(INOUT)   :: PDFORC_UDU! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
!
!*      0.2    declarations of local variables
!
!
!* BERJLAARS et al 2004  constants
!
REAL, PARAMETER :: C_ALPHA   = 12.
REAL, PARAMETER :: C_BETA    = 1.
REAL, PARAMETER :: C_CMD     = 0.005
REAL, PARAMETER :: C_COR     = 0.6
REAL, PARAMETER :: C_IH      = 0.00102    !   (m-1)
REAL, PARAMETER :: C_KFLT    = 0.00035    !   (m-1)
REAL, PARAMETER :: C_K1      = 0.003      !   (m-1)
REAL, PARAMETER :: C_N1      = -1.9
REAL, PARAMETER :: C_N2      = -2.8
REAL            :: C_AVAR                 ! = C_K1**(C_N1-C_N2) / (C_IH * C_KFLT**C_N1)  
!                                         ! (unit: m^{1+C_N2}  =  m^-1.8)
!
INTEGER                  :: JL            ! loop counter on canopy heights
REAL, DIMENSION(KI,KLVL) :: ZSSO_DRAG     ! drag due to subgrid-scale orogaphy
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SSO_BELJAARS04',0,ZHOOK_HANDLE)
!
!*      2.     Computations of wind tendency due to orographic drag
!              ----------------------------------------------------
!
 C_AVAR    = C_K1**(C_N1-C_N2) / (C_IH * C_KFLT**C_N1)   ! (unit: m^{1+C_N2}  =  m^-1.8)
!
!
!*      2.1    Drag coefficient in  drag force by subscale orography 
!              -----------------------------------------------------
!
! unit : m-1    (m^-1.8 . m ^2 . m^-1.2) 
!
ZSSO_DRAG = 0.
DO JL=1,KLVL
  ZSSO_DRAG (:,JL) = USS%XCOEFBE * C_ALPHA * C_BETA * C_COR * C_CMD * 2.109 * EXP( -(PZ(:,JL)/1500.)**1.5) &
    * C_AVAR * PSSO_STDEV(:)**2 * PZ(:,JL)**(-1.2)
END DO
!
!
!*      2.2    Impact on each SBL layer
!              ------------------------
!
! Ext = - Cdrag(z)  * u- * u-       (unit :  m s-2)   subscale orgraphy drag
!
PFORC_U(:,:)    = PFORC_U(:,:)    -       ZSSO_DRAG (:,:) * PU(:,:)**2
!
PDFORC_UDU(:,:) = PDFORC_UDU(:,:) -  2. * ZSSO_DRAG (:,:) * PU(:,:)
!
IF (LHOOK) CALL DR_HOOK('SSO_BELJAARS04',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE SSO_BELJAARS04
