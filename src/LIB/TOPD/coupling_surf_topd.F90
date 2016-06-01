!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!###################################################################
SUBROUTINE COUPLING_SURF_TOPD (DGEI, DGMI, IG, I, UG, U, &
                                HPROGRAM,KI)
!###################################################################
!
!!****  *COUPLING_SURF_TOPD*  
!!
!!    PURPOSE
!!    -------
!!   
!!    Driver for the coupling between SURFEX and TOPODYN
!!      
!!    REFERENCE
!!    ---------
!!    *COUPLING_SURF_TRIP from B. Decharme
!!      
!!    AUTHOR
!!    ------
!!      B. Vincendon    
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/06/11 
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
USE MODI_COUPL_TOPD
USE MODI_ROUT_DATA_ISBA
USE MODI_BUDGET_COUPL_ROUT
USE MODI_WRITE_DISCHARGE_FILE
USE MODI_WRITE_BUDGET_COUPL_ROUT
USE MODI_PREP_RESTART_COUPL_TOPD
!
USE MODD_TOPODYN,       ONLY : XQTOT, NNB_TOPD_STEP, XQB_RUN, XQB_DR
USE MODD_COUPLING_TOPD, ONLY : LCOUPL_TOPD, LBUDGET_TOPD, NNB_TOPD, LTOPD_STEP, NTOPD_STEP, &
                                 NYEAR,NMONTH,NDAY,NH,NM
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
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DGEI
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DGMI
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6), INTENT(IN)         :: HPROGRAM ! program calling surf. schemes
INTEGER,          INTENT(IN)         :: KI       ! Surfex grid dimension
                                                 ! in a forcing iteration
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=3)              :: YSTEP    ! time stepsurf_tmp/off
INTEGER                       :: ILUOUT   ! unit of output listing file
INTEGER                       :: JJ       ! loop control
!
REAL, DIMENSION(KI)           :: ZDG_FULL
REAL, DIMENSION(KI)           :: ZWG2_FULL,ZWG3_FULL,ZDG2_FULL,ZDG3_FULL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_TOPD',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF ( .NOT.LCOUPL_TOPD ) THEN
  IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_TOPD',1,ZHOOK_HANDLE)
  RETURN
ENDIF
  !
IF ( LTOPD_STEP ) THEN
  !
  ! * 1. Calling coupling or routing
  !
  IF (NTOPD_STEP<10) THEN
    WRITE(YSTEP,'(I1)') NTOPD_STEP
  ELSEIF (NTOPD_STEP < 100) THEN
    WRITE(YSTEP,'(I2)') NTOPD_STEP
  ELSE
    WRITE(YSTEP,'(I3)') NTOPD_STEP
  ENDIF
  !
  write(ILUOUT,*) 'pas de temps coupl ',YSTEP
  !
  IF (I%CRUNOFF=='TOPD') THEN
    CALL COUPL_TOPD(DGEI, DGMI, IG, I, UG, U, &
                    HPROGRAM,YSTEP,KI,NTOPD_STEP)
  ELSE
    CALL ROUT_DATA_ISBA(DGEI, DGMI, IG, I, UG, U, &
                        HPROGRAM,KI,NTOPD_STEP)
  ENDIF
  !
  IF (LBUDGET_TOPD) CALL BUDGET_COUPL_ROUT(DGEI, DGMI, I, U, &
                                           KI,NTOPD_STEP)
  !
ENDIF! (LCOUPL_TOPD.AND......
!
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_TOPD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE COUPLING_SURF_TOPD
