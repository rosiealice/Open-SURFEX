!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
!     ##########################
      SUBROUTINE DISPATCH_WG (I, &
                              KI,PWG,PWGI,PDG)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!      from  AVERAGE_DIAG_MISC_ISBA_n
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
USE MODD_ISBA_PAR,      ONLY : XWGMIN
USE MODD_COUPLING_TOPD, ONLY :  XATOP
!
USE YOMHOOK   ,     ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,     ONLY : JPRB
!
!
IMPLICIT NONE
!
!*      0.1    declarations of argumentsXPATCH

!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 INTEGER, INTENT(IN)               :: KI
 REAL, DIMENSION(:,:), INTENT(IN) :: PWG
 REAL, DIMENSION(:,:), INTENT(IN) :: PWGI
 REAL, DIMENSION(:,:), INTENT(IN) :: PDG
!      
!*      0.2    declarations of local variables
 INTEGER                         :: JJ, JLAYER, JPATCH ! loop indexes
 INTEGER                         :: IDEPTH 
 INTEGER                         :: INI, INP
 REAL                            :: ZWORK,ZTMP, ZWORK2
 REAL, DIMENSION(SIZE(I%XPATCH,1)) :: ZSUMPATCH
 REAL, DIMENSION(SIZE(I%XPATCH,1),SIZE(I%XPATCH,2)) :: ZFRAC_PATCH2
 REAL, DIMENSION(SIZE(I%XPATCH,1),SIZE(I%XPATCH,2)) :: ZFRAC_PATCH3
 REAL, DIMENSION(SIZE(PWG,1),SIZE(PWG,2)) :: ZWG_CTL
 !
 REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DISPATCH_WG',0,ZHOOK_HANDLE)
!
INI=SIZE(I%XPATCH,1)
INP=SIZE(I%XPATCH,2)
!
 !DO JPATCH=1,INP
 !
 ! write(*,*) 'In dispatch XPATCH (1)',JPATCH,XPATCH(1,JPATCH),XWG(1,2,JPATCH)
 !ENDDO
!write(*,*) 'In dispatch wg ,KI,INI,INP ',KI,INI,INP
IF (INP/=1)THEN
 DO JPATCH=1,INP
   DO JJ=1,INI
    IF ((I%XPATCH(JJ,JPATCH)/=XUNDEF).AND.(I%XPATCH(JJ,JPATCH)/=0.)&
        .AND.(XATOP(JJ)==1.)) THEN
     WHERE (I%XWG(JJ,:,JPATCH)/=XUNDEF)
      I%XWG(JJ,:,JPATCH) = PWG(JJ,:) 
      I%XWGI(JJ,:,JPATCH)= PWGI(JJ,:) 
      I%XDG(JJ,:,JPATCH) = PDG (JJ,:)
     ENDWHERE
    ENDIF
   ENDDO
 ENDDO

ELSE
 I%XWG(:,:,1) = PWG(:,:) 
 I%XWGI(:,:,1)= PWGI(:,:) 
 I%XDG(:,:,1) = PDG (:,:)
ENDIF
!
WHERE (I%XWG(:,:,:)<XWGMIN) 
  I%XWG(:,:,:)=XWGMIN
ENDWHERE
 !     
IF (LHOOK) CALL DR_HOOK('DISPATCH_WG',1,ZHOOK_HANDLE)

END SUBROUTINE DISPATCH_WG


