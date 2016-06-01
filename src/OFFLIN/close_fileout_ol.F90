!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CLOSE_FILEOUT_OL
!     #######################################################
!!****  *CLOSE_FILEOUT_OL* - closes netcdf files writen by surface
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2010 
!-------------------------------------------------------------------------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_OL_FILEID, ONLY: XID_SURF, XID_NATURE, XID_WATER, XID_SEA, XID_TOWN
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
INCLUDE "netcdf.inc"

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('CLOSE_FILEOUT_OL',0,ZHOOK_HANDLE)
!
IF (NRANK==NPIO) THEN
!$OMP SINGLE
  IF (ASSOCIATED(XID_SURF)) CALL CLOSE_EACH_FILE(XID_SURF)
  IF (ASSOCIATED(XID_NATURE)) CALL CLOSE_EACH_FILE(XID_NATURE)
  IF (ASSOCIATED(XID_SEA)) CALL CLOSE_EACH_FILE(XID_SEA)
  IF (ASSOCIATED(XID_WATER)) CALL CLOSE_EACH_FILE(XID_WATER)
  IF (ASSOCIATED(XID_TOWN)) CALL CLOSE_EACH_FILE(XID_TOWN)
!$OMP END SINGLE
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CLOSE_FILEOUT_OL',1,ZHOOK_HANDLE)
 CONTAINS

!-----------------------------------------------

SUBROUTINE CLOSE_EACH_FILE(NVAR)

IMPLICIT NONE

INTEGER,DIMENSION(:),INTENT(IN)::NVAR
INTEGER::JRET,JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('CLOSE_EACH_FILE',0,ZHOOK_HANDLE)
JRET=NF_CLOSE(NVAR(1))
DO JJ=2,SIZE(NVAR)
  IF (NVAR(JJ).NE.NVAR(JJ-1)) THEN 
     JRET=NF_CLOSE(NVAR(JJ))
   ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('CLOSE_EACH_FILE',1,ZHOOK_HANDLE)

END SUBROUTINE CLOSE_EACH_FILE

!-----------------------------------------------

END SUBROUTINE CLOSE_FILEOUT_OL
