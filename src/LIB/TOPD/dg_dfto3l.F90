!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
!     ##########################
      SUBROUTINE DG_DFTO3L (I, &
                            KI,PDG)
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
 REAL, DIMENSION(:,:), INTENT(OUT) :: PDG
!      
!*      0.2    declarations of local variables
 INTEGER                         :: JJ, JLAYER, JPATCH ! loop indexes
 INTEGER                         :: IDEPTH 
 INTEGER                         :: INI, INP
 REAL                            :: ZWORK 
 !
 REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DG_DFTO3L',0,ZHOOK_HANDLE)
INI=SIZE(I%XPATCH,1)
INP=SIZE(I%XPATCH,2)
!
PDG(:,:)=0.0
!
    DO JPATCH=1,INP
!  
      IF (I%NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
      DO JLAYER = 1,I%NGROUND_LAYER
        DO JJ=1,INI
          IDEPTH=I%NWG_LAYER(JJ,JPATCH)
          IF(JLAYER<=IDEPTH.AND.IDEPTH/=NUNDEF.AND.I%XPATCH(JJ,JPATCH)/=XUNDEF)THEN
            !
            PDG      (JJ,1) = PDG      (JJ,1) + I%XDG(JJ,1,JPATCH) * I%XPATCH(JJ,JPATCH) 
            ! ISBA-FR-DG2 comparable soil wetness index, liquid water and ice contents
            ZWORK=MIN(I%XDZG(JJ,JLAYER,JPATCH),MAX(0.0,I%XDG2(JJ,JPATCH)-I%XDG(JJ,JLAYER,JPATCH)+I%XDZG(JJ,JLAYER,JPATCH)))
            PDG      (JJ,2) = PDG      (JJ,2) + ZWORK * I%XPATCH(JJ,JPATCH) 
            !
            ! ISBA-FR-DG3 comparable soil wetness index, liquid water and ice contents
            ZWORK=MIN(I%XDZG(JJ,JLAYER,JPATCH),MAX(0.0,I%XDG(JJ,JLAYER,JPATCH)-I%XDG2(JJ,JPATCH)))
            PDG      (JJ,3) = PDG      (JJ,3) + ZWORK * I%XPATCH(JJ,JPATCH) 
            !
          ENDIF
        ENDDO
      ENDDO
!
    ENDDO
    ! 
     PDG (:,3) =  PDG (:,2) + PDG (:,3)
     WHERE (PDG(:,:)==0.0)
             PDG(:,:)=XUNDEF
     ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('DG_DFTO3L',1,ZHOOK_HANDLE)

END SUBROUTINE DG_DFTO3L


