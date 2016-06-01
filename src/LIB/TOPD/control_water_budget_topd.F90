!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #####################
      SUBROUTINE CONTROL_WATER_BUDGET_TOPD (I, U, &
                                            PWGM,PWG,PDG,PMESH_SIZE,&
                                           PAVG_MESH_SIZE,PWSAT)
!     #####################
!
!!****  *CONTROL_WATER_BUDGET_TOPD*  
!!
!!    PURPOSE
!!    -------
!     To control water budget after topodyn_lat lateral distribution
!     
!         
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
!!    
!!    
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Vincendon *  Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   : Out of COUPL_TOPD in february 2014
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,         ONLY : XUNDEF, NUNDEF
USE MODD_COUPLING_TOPD,    ONLY : XTOTBV_IN_MESH
USE MODD_ISBA_PAR,         ONLY : XWGMIN
USE MODI_AVG_PATCH_WG
!
USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PWGM
REAL, DIMENSION(:,:), INTENT(INOUT) :: PWG
REAL, DIMENSION(:,:), INTENT(IN)    :: PDG
REAL, DIMENSION(:),   INTENT(IN)    :: PMESH_SIZE
REAL,                 INTENT(IN)    :: PAVG_MESH_SIZE
REAL, DIMENSION(:),   INTENT(IN)    :: PWSAT
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PWG,1),3)     :: ZWG_3L, ZWGI_3L, ZDG_3L          
REAL                               :: ZSTOCK_WGM, ZSTOCK_WG
REAL                               :: ZAVG_DGALL, ZCONTROL_WATER_BUDGET_TOPD
REAL                               :: ZTMP, ZTMP2
INTEGER                            :: JMESH, JPATCH, JJ
REAL,    DIMENSION(SIZE(I%XPATCH,1)) :: ZSUMPATCH
REAL,    DIMENSION(SIZE(I%XPATCH,1)) :: ZWG_CORR, ZAVG_WGM, ZAVG_WG, ZAVG_DG
REAL,    DIMENSION(SIZE(I%XPATCH,1)) :: ZTOTBV_IN_MESH
LOGICAL, DIMENSION(SIZE(I%XPATCH,1)) :: LMODIF
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CONTROL_WATER_BUDGET_TOPD',0,ZHOOK_HANDLE)
!
IF(I%NPATCH/=1) THEN
 ZSUMPATCH(:) = 0.0
 DO JPATCH=1,I%NPATCH
   DO JJ=1,SIZE(I%XPATCH,1)
      ZSUMPATCH(JJ) = ZSUMPATCH(JJ) + I%XPATCH(JJ,JPATCH)
   ENDDO
 ENDDO
ZAVG_WGM(:)  = 0.
ZAVG_WG(:)  = 0.
ZAVG_DG(:)  = 0.
!
  DO JPATCH=1,I%NPATCH
     DO JJ=1,SIZE(I%XPATCH,1)     
        IF(ZSUMPATCH(JJ) > 0..AND.PWGM(JJ,JPATCH)/=XUNDEF.AND.PWG(JJ,JPATCH)/=XUNDEF.AND.PDG (JJ,JPATCH)/=XUNDEF)THEN
!
          ZAVG_WGM(JJ)  = ZAVG_WGM(JJ)  + I%XPATCH(JJ,JPATCH) * PWGM(JJ,JPATCH)  * PDG (JJ,JPATCH) 
          ZAVG_WG(JJ)  = ZAVG_WG(JJ)  + I%XPATCH(JJ,JPATCH) * PWG(JJ,JPATCH)  * PDG (JJ,JPATCH) 
          ZAVG_DG(JJ) = ZAVG_DG(JJ) + I%XPATCH(JJ,JPATCH) * PDG (JJ,JPATCH)
!          
       ENDIF
     ENDDO
  ENDDO     
!     
 WHERE (ZAVG_DG(:)>0.0.AND.ZSUMPATCH(:)>0.)
    ZAVG_WGM(:) = ZAVG_WGM(:) / ZAVG_DG(:)
    ZAVG_WG(:)  = ZAVG_WG(:)  / ZAVG_DG(:)
 ENDWHERE
!
ELSE
ZAVG_WGM(:)= PWGM(:,1)
ZAVG_WG(:) = PWG(:,1) 
ZAVG_DG(:) = PDG(:,1)
ZSUMPATCH(:) = 1.0
ENDIF
!
!
ZSTOCK_WGM = SUM(ZAVG_WGM(:)*ZAVG_DG(:)*PMESH_SIZE(:),&
            MASK=(ZAVG_WGM(:)/=XUNDEF.AND.&
                  ZAVG_DG(:)/=XUNDEF.AND.&
                  PMESH_SIZE(:)/=XUNDEF.AND.&
                  ZSUMPATCH(:)>0.))    ! water stocked in the ground (m3)
!
ZSTOCK_WG  = SUM(ZAVG_WG(:)*ZAVG_DG(:)*PMESH_SIZE(:),&
            MASK=(ZAVG_WG(:)/=XUNDEF.AND.&
                  ZAVG_DG(:)/=XUNDEF.AND.&
                  PMESH_SIZE(:)/=XUNDEF.AND.&
                  ZSUMPATCH(:)>0.))    ! water stocked in the ground (m3)
!
IF ( COUNT(ZAVG_DG(:)/=XUNDEF.AND.ZSUMPATCH(:)>0.)/=0. )&
ZAVG_DGALL = SUM(ZAVG_DG(:),MASK=(ZAVG_DG(:)/=XUNDEF.AND.ZSUMPATCH(:)>0.))&
          / COUNT(ZAVG_DG(:)/=XUNDEF.AND.ZSUMPATCH(:)>0.)

IF (ZAVG_DGALL/=0.) THEN
  ZCONTROL_WATER_BUDGET_TOPD = ( ZSTOCK_WG - ZSTOCK_WGM )/ ZAVG_DGALL / PAVG_MESH_SIZE
!
 IF (ZCONTROL_WATER_BUDGET_TOPD==0.0) GOTO 66
 !
 ZTMP  = COUNT( ZAVG_WG(:)/=ZAVG_WGM(:).AND.ZAVG_WG(:)/=XUNDEF.AND.ZAVG_WGM(:)/=XUNDEF.AND.ZSUMPATCH(:)>0. )
!
 LMODIF(:)=.FALSE.

 CALL PACK_SAME_RANK(U%NR_NATURE,XTOTBV_IN_MESH,ZTOTBV_IN_MESH)
 IF (ZTMP/=0.) THEN
   WHERE (ZTOTBV_IN_MESH(:)/=0.0.AND.ZAVG_WGM(:)/=XUNDEF.AND.ZAVG_WG(:)/=XUNDEF.AND.&
       ZAVG_WG(:)/=ZAVG_WGM(:) .AND. ZAVG_WG(:)>XWGMIN+(ZCONTROL_WATER_BUDGET_TOPD/ZTMP).AND.&
       ZAVG_WG(:)<=PWSAT(:)+(ZCONTROL_WATER_BUDGET_TOPD/ZTMP).AND.ZSUMPATCH(:)>0.)
       LMODIF(:)=.TRUE.
   ENDWHERE
!
 WHERE (LMODIF)
   ZAVG_WG(:) = MIN(MAX(ZAVG_WG(:) - (ZCONTROL_WATER_BUDGET_TOPD/ZTMP),XWGMIN),PWSAT(:))
 ENDWHERE
! 
 ENDIF
ENDIF

DO JPATCH=1,I%NPATCH
 WHERE ((PWG(:,JPATCH)/=XUNDEF).AND.(I%XPATCH(:,JPATCH)>0.)&
         .AND.(I%XPATCH(:,JPATCH)/=XUNDEF).AND.(ZTOTBV_IN_MESH(:)/=0.0))
  PWG(:,JPATCH)=MIN(MAX(ZAVG_WG(:),XWGMIN),PWSAT(:))
 ENDWHERE
ENDDO


ZSTOCK_WG  = SUM(ZAVG_WG(:)*ZAVG_DG(:)*PMESH_SIZE(:),&
            MASK=(ZAVG_WG(:)/=XUNDEF.AND.&
                  ZAVG_DG(:)/=XUNDEF.AND.&
                  PMESH_SIZE(:)/=XUNDEF.AND.&
                  ZSUMPATCH(:)>0.))    ! water stocked in the ground (m3)


 IF (ZAVG_DGALL/=0) THEN
  ZCONTROL_WATER_BUDGET_TOPD = ( ZSTOCK_WG - ZSTOCK_WGM )/ ZAVG_DGALL / PAVG_MESH_SIZE
 ENDIF

66 CONTINUE
!
IF (LHOOK) CALL DR_HOOK('CONTROL_WATER_BUDGET_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE CONTROL_WATER_BUDGET_TOPD

