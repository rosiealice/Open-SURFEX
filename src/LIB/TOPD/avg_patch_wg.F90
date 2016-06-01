!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
!     ##########################
      SUBROUTINE AVG_PATCH_WG (I, &
                               KI,PWG,PWGI,PDG)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!      from  AVERAGE_DIAG_MISC_ISBA_n
!!     ONLY for 3L cases!!
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
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
!!       ELYAZIDI/HEYMES/RISTOR * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original  02/2011 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURF_PAR,  ONLY : XUNDEF, NUNDEF
USE YOMHOOK   ,     ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,     ONLY : JPRB
!
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments

!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 INTEGER, INTENT(IN)               :: KI
 REAL, DIMENSION(:,:), INTENT(OUT) :: PWG
 REAL, DIMENSION(:,:), INTENT(OUT) :: PWGI
 REAL, DIMENSION(:,:), INTENT(OUT) :: PDG
!      
!*      0.2    declarations of local variables
 INTEGER                         :: JJ, JLAYER, JPATCH ! loop indexes
 INTEGER                         :: IDEPTH 
 INTEGER                         :: INI, INP
 REAL                            :: ZWORK 
REAL, DIMENSION(SIZE(I%XPATCH,1)) :: ZSUMPATCH
 !
 REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('AVG_PATCH_WG',0,ZHOOK_HANDLE)
!
INI=SIZE(I%XPATCH,1)
INP=SIZE(I%XPATCH,2)

ZSUMPATCH(:) = 0.0
DO JPATCH=1,INP
   DO JJ=1,INI
      ZSUMPATCH(JJ) = ZSUMPATCH(JJ) + I%XPATCH(JJ,JPATCH)
   END DO
END DO

PWG(:,:) =0.0
PWGI(:,:)=0.0
PDG(:,:) =0.0
!
! 
IF (INP/=1)THEN
  DO JPATCH=1,INP
     DO JJ=1,INI     
        IF(ZSUMPATCH(JJ) > 0.)THEN
!
          ZWORK=MAX(0.0,I%XDG(JJ,3,JPATCH)-I%XDG(JJ,2,JPATCH))
          PWG(JJ,1)  = PWG(JJ,1)  + I%XPATCH(JJ,JPATCH) * I%XWG(JJ,1,JPATCH)  * I%XDG (JJ,1,JPATCH) 
          PWG(JJ,2)  = PWG(JJ,2)  + I%XPATCH(JJ,JPATCH) * I%XWG(JJ,2,JPATCH)  * I%XDG (JJ,2,JPATCH) 
          PWG(JJ,3)  = PWG(JJ,3)  + I%XPATCH(JJ,JPATCH) * I%XWG(JJ,3,JPATCH)  * ZWORK
          PWGI(JJ,1) = PWGI(JJ,1) + I%XPATCH(JJ,JPATCH) * I%XWGI(JJ,1,JPATCH) * I%XDG (JJ,1,JPATCH) 
          PWGI(JJ,2) = PWGI(JJ,2) + I%XPATCH(JJ,JPATCH) * I%XWGI(JJ,2,JPATCH) * I%XDG (JJ,2,JPATCH) 
          PWGI(JJ,3) = PWGI(JJ,3) + I%XPATCH(JJ,JPATCH) * I%XWGI(JJ,3,JPATCH) * ZWORK
          ! 
          PDG(JJ,1) = PDG(JJ,1) + I%XPATCH(JJ,JPATCH) * I%XDG(JJ,1,JPATCH)
          PDG(JJ,2) = PDG(JJ,2) + I%XPATCH(JJ,JPATCH) * I%XDG (JJ,2,JPATCH)
          PDG(JJ,3) = PDG(JJ,3) + I%XPATCH(JJ,JPATCH) * I%XDG (JJ,3,JPATCH)
          !
!          
       ENDIF
     ENDDO
  ENDDO     
!     
 WHERE (PDG(:,1)>0.0)
    PWG(:,1)  = PWG(:,1)  / PDG(:,1)
    PWGI(:,1)  = PWGI(:,1)  / PDG(:,1)
 ENDWHERE
 WHERE (PDG(:,2)>0.0)
    PWG(:,2)  = PWG(:,2)  / PDG(:,2)
    PWGI(:,2)  = PWGI(:,2)  / PDG(:,2)
 ENDWHERE
 WHERE (PDG(:,3)-PDG(:,2)>0.0)
    PWG(:,3)  = PWG(:,3)  / (PDG(:,3)-PDG(:,2))
    PWGI(:,3)  = PWGI(:,3)  / (PDG(:,3)-PDG(:,2))
 ENDWHERE
ELSE
    PWG(:,:)  = I%XWG(:,:,1)
    PWGI(:,:) = I%XWGI(:,:,1)
    PDG (:,:) = I%XDG (:,:,1)

ENDIF 
!

IF (LHOOK) CALL DR_HOOK('AVG_PATCH_WG',1,ZHOOK_HANDLE)

END SUBROUTINE AVG_PATCH_WG


