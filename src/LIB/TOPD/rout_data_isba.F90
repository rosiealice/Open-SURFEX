!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #####################
      SUBROUTINE ROUT_DATA_ISBA (DGEI, DGMI, IG, I, UG, U, &
                                 HPROGRAM,KI,KSTEP)
!     #####################
!
!!****  *ROUT_DATA_ISBA*  
!!
!!    PURPOSE
!!    -------
!
!    Routes runoff and drainage of ISBA with coupling with Topmodel
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
!!      B. Vincendon    *  Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   15/06/2007
!!                 03/2014 (B. Vincendon) call of initialisation for budget on watersheds
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_GET_LUOUT
USE MODI_UNPACK_SAME_RANK
USE MODI_DIAG_ISBA_TO_ROUT
USE MODI_ISBA_TO_TOPD
USE MODI_INIT_BUDGET_COUPL_ROUT
USE MODI_ROUTING
!
USE MODD_TOPODYN,        ONLY : NNCAT, NMESHT, NNMC
USE MODD_COUPLING_TOPD,  ONLY : NMASKT, XRUNOFF_TOP, XATOP, NNPIX,&
                                  XAVG_RUNOFFCM, XAVG_DRAINCM, LBUDGET_TOPD
!
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DGEI
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DGMI
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! program calling surf. schemes
INTEGER, INTENT(IN)          :: KI     ! Grid dimensions
INTEGER, INTENT(IN)          :: KSTEP  ! current time step 
!
!*      0.2    declarations of local variables
!
INTEGER                       :: JJ,JI  ! loop control 
INTEGER                       :: ILUOUT      ! unit number of listing file
 CHARACTER(LEN=30)             :: YVAR
REAL, DIMENSION(KI)           :: ZRUNOFFC_FULL  ! Cumulated runoff from isba on the full domain (kg/m2)
REAL, DIMENSION(KI)           :: ZRUNOFFC_FULLM ! Cumulated runoff from isba on the full domain (kg/m2) at t-dt
REAL, DIMENSION(KI)           :: ZRUNOFF_ISBA   ! Runoff from Isba (kg/m2)
REAL, DIMENSION(KI)           :: ZDRAINC_FULL   ! Cumulated drainage from Isba on the full domain (kg/m2)
REAL, DIMENSION(KI)           :: ZDRAINC_FULLM  ! Cumulated drainage from Isba on the full domain (kg/m2) at t-dt
REAL, DIMENSION(KI)           :: ZDRAIN_ISBA    ! Drainage from Isba (m3/s)
REAL, DIMENSION(NNCAT,NMESHT) :: ZRUNOFF_TOPD   ! Runoff on the Topodyn grid (m3/s)
REAL, DIMENSION(NNCAT,NMESHT) :: ZDRAIN_TOPD    ! Drainage from Isba on Topodyn grid (m3/s)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ROUT_DATA_ISBA',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
ZRUNOFFC_FULL (:) = 0.
ZRUNOFFC_FULLM(:) = 0.
ZRUNOFF_ISBA  (:) = 0.
ZRUNOFF_TOPD(:,:) = 0.
ZDRAINC_FULL  (:) = 0.
ZDRAINC_FULLM (:) = 0.
ZDRAIN_ISBA   (:) = 0.
ZDRAIN_TOPD (:,:) = 0.
IF (KSTEP==1 .AND. LBUDGET_TOPD) CALL INIT_BUDGET_COUPL_ROUT(DGEI, DGMI, IG, I, U, &
                                                             KI)
!
!    Runoff on TOPODYN grid
!   ---------------------------------------
!
 CALL UNPACK_SAME_RANK(U%NR_NATURE,DGEI%XAVG_RUNOFFC,ZRUNOFFC_FULL)
 CALL UNPACK_SAME_RANK(U%NR_NATURE,XAVG_RUNOFFCM,ZRUNOFFC_FULLM)
!
 CALL DIAG_ISBA_TO_ROUT(UG, &
                        ZRUNOFFC_FULL,ZRUNOFFC_FULLM,ZRUNOFF_ISBA)
!
XAVG_RUNOFFCM(:) = DGEI%XAVG_RUNOFFC(:)
ZRUNOFF_TOPD(:,:) = 0.0
!
 CALL ISBA_TO_TOPD(ZRUNOFF_ISBA,ZRUNOFF_TOPD)
!
DO JJ=1,NNCAT
  DO JI=1,NNMC(JJ)
    ZRUNOFF_TOPD(JJ,JI) = ZRUNOFF_TOPD(JJ,JI) / NNPIX(NMASKT(JJ,JI))
  ENDDO
ENDDO
!
!    Drainage treatment
!    ----------------------------------------
!
 CALL UNPACK_SAME_RANK(U%NR_NATURE,DGEI%XAVG_DRAINC*XATOP,ZDRAINC_FULL)
 CALL UNPACK_SAME_RANK(U%NR_NATURE,XAVG_DRAINCM*XATOP,ZDRAINC_FULLM)
!
 CALL DIAG_ISBA_TO_ROUT(UG, &
                        ZDRAINC_FULL,ZDRAINC_FULLM,ZDRAIN_ISBA)
!
XAVG_DRAINCM(:)  = DGEI%XAVG_DRAINC(:)
ZDRAIN_TOPD(:,:) = 0.0
!
 CALL ISBA_TO_TOPD(ZDRAIN_ISBA,ZDRAIN_TOPD)
!
DO JJ=1,NNCAT
  DO JI=1,NNMC(JJ)
    ZDRAIN_TOPD(JJ,JI) = ZDRAIN_TOPD(JJ,JI) / NNPIX(NMASKT(JJ,JI))
  ENDDO
ENDDO
!*     Routing (runoff + drainage)
!     ----------------------------------------
!
 CALL ROUTING(ZRUNOFF_TOPD,ZDRAIN_TOPD,KSTEP)
!
IF (LHOOK) CALL DR_HOOK('ROUT_DATA_ISBA',1,ZHOOK_HANDLE)
!
END SUBROUTINE ROUT_DATA_ISBA
