!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_TEB_VEG_n (DGU, U, &
                                           DTGD, TGDO, TGDP, TVG, &
                                          HPROGRAM)
!     ###############################################
!
!!****  *WRITE_PGD_TEB_VEG_n* - writes ISBA fields describing urban gardens
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
!!      A. Lemonsu & C. de Munck   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2011 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_DATA_TEB_GARDEN_n, ONLY : DATA_TEB_GARDEN_t
USE MODD_TEB_GARDEN_OPTION_n, ONLY : TEB_GARDEN_OPTIONS_t
USE MODD_TEB_GARDEN_PGD_n, ONLY : TEB_GARDEN_PGD_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(DATA_TEB_GARDEN_t), INTENT(INOUT) :: DTGD
TYPE(TEB_GARDEN_OPTIONS_t), INTENT(INOUT) :: TGDO
TYPE(TEB_GARDEN_PGD_t), INTENT(INOUT) :: TGDP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=4 ) :: YLVL
!
INTEGER :: JJ, JLAYER
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_VEG_N',0,ZHOOK_HANDLE)
!
!* soil scheme option
!
YRECFM='GD_ISBA'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TVG%CISBA,IRESP,HCOMMENT=YCOMMENT)
!
!* Reference grid for DIF
!
IF(TVG%CISBA=='DIF') THEN
  DO JLAYER=1,TGDO%NGROUND_LAYER
     WRITE(YLVL,'(I4)') JLAYER     
     YRECFM='GD_SGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='Depth of TEB Garden soilgrid layer '//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDO%XSOILGRID(JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO 
ENDIF
!
!* number of soil layers
!
YRECFM='GD_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDO%NGROUND_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of time data for vegetation characteristics (VEG, LAI, EMIS, Z0) 
!
YRECFM='GD_NTIME'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DTGD%NTIME,IRESP,HCOMMENT=YCOMMENT)
!
! * clay fraction
!
YRECFM='GD_CLAY'
YCOMMENT='X_Y_GD_CLAY'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDP%XCLAY(:,1),IRESP,HCOMMENT=YCOMMENT)
!        
! * sand fraction
!
YRECFM='GD_SAND'
YCOMMENT='X_Y_GD_SAND'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDP%XSAND(:,1),IRESP,HCOMMENT=YCOMMENT)
!        
! * orographic runoff coefficient
!
YRECFM='GD_RUNOFFB'
YCOMMENT='X_Y_GD_RUNOFFB'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDP%XRUNOFFB,IRESP,HCOMMENT=YCOMMENT)
!        
! * subgrid drainage coefficient
!
YRECFM='GD_WDRAIN'
YCOMMENT='X_Y_GD_WDRAIN'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDP%XWDRAIN,IRESP,HCOMMENT=YCOMMENT)
!
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_VEG_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_VEG_n
