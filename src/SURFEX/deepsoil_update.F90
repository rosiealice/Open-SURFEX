!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE DEEPSOIL_UPDATE (I, &
                                KMONTH)
!   ###############################################################
!!****  *DEEPSOIL_UPDATE*
!!
!!    PURPOSE
!!    -------
!
!     performs the time evolution of DEEPSOIL
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Le Moigne          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2008
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_DEEPSOIL, ONLY : XTDEEP_CLI, XGAMMAT_CLI
!
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
TYPE(ISBA_t), INTENT(INOUT) :: I
!
INTEGER,              INTENT(IN)    :: KMONTH   ! current month
!
!*      0.2    declarations of local variables
!
INTEGER                             :: IP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEEPSOIL_UPDATE',0,ZHOOK_HANDLE)
DO IP=1,SIZE(I%XTDEEP)
   !
   I%XTDEEP (IP) = XTDEEP_CLI (KMONTH)
   !
   I%XGAMMAT(IP) = 1. / XGAMMAT_CLI(KMONTH)
   !
ENDDO
IF (LHOOK) CALL DR_HOOK('DEEPSOIL_UPDATE',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE DEEPSOIL_UPDATE
