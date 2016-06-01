!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_IO_SURF_OL_n (DTCO, DGU, U, &
                                    HPROGRAM,HMASK,HSCHEME,HACTION)
!     ######################
!
!!****  *INIT_IO_SURF_OL* Keep in memory the netcdf ID of the output files
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      modified 05/04 by P. LeMoigne *Meteo France*
!!      modified 06/10 by S. Faroux *Meteo France*
!!=================================================================
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_IO_SURF_OL
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODN_IO_OFFLINE,    ONLY : XTSTEP_OUTPUT
!
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_GET_DIM_FULL_n
USE MODI_GET_SIZE_FULL_n
USE MODI_GET_TYPE_DIM_n
USE MODI_INIT_IO_SURF_MASK_n
USE MODI_WRITE_SURF
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM 
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK    
 CHARACTER(LEN=6),  INTENT(IN)  :: HSCHEME 
 CHARACTER(LEN=5),  INTENT(IN)  :: HACTION 
!
REAL              :: ZDEN
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: ILU,IRET, IL, IFULL
INTEGER           :: ILUOUT
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!------------------------------------------------------------------------------ 
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_OL_N',0,ZHOOK_HANDLE)
!
LMASK = .TRUE.
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!$OMP BARRIER
!
IF (HACTION=='READ') THEN
  CALL READ_SURF(&
                 'OFFLIN','DIM_FULL',IFULL,IRET)
ELSE 
  CALL GET_DIM_FULL_n(U, &
                      IFULL)
ENDIF
!
! size by MPI task. NINDEX is supposed to be initialized at this step.  
 CALL GET_SIZE_FULL_n(U, &
                      'OFFLIN',IFULL,ILU)
!
IL = ILU
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     HMASK,IL)
 CALL INIT_IO_SURF_MASK_n(DTCO, U, &
                          HMASK, IL, ILUOUT, ILU, NMASK)
!
IF (HACTION=='READ' .AND. LHOOK) CALL DR_HOOK('INIT_IO_SURF_OL_N',1,ZHOOK_HANDLE)
IF (HACTION=='READ') RETURN
!
IF (NRANK==NPIO) THEN
  !  
  YCOMMENT=''
  !
  IF (XTSTEP_OUTPUT == FLOOR(XTSTEP_OUTPUT/86400.)*86400) THEN 
    ZDEN = 86400.
  ELSEIF (XTSTEP_OUTPUT == FLOOR(XTSTEP_OUTPUT/3600.)*3600) THEN
    ZDEN = 3600.
  ELSEIF (XTSTEP_OUTPUT == FLOOR(XTSTEP_OUTPUT/60.)*60) THEN
    ZDEN = 60.
  ELSE
    ZDEN = 1.
  ENDIF
  !
  IF (.NOT.LTIME_WRITTEN(1)) THEN 
    XTYPE=1
    CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,'time',XTSTEP_OUTPUT/ZDEN*XSTARTW,IRESP,HCOMMENT=YCOMMENT)
    LTIME_WRITTEN(1)=.TRUE.
  ENDIF
  !
  IF (HSCHEME.NE.'NONE  ') THEN
    !
    IF (HMASK=='NATURE' .AND. .NOT.LTIME_WRITTEN(2)) THEN
      XTYPE=2
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,'time',XTSTEP_OUTPUT/ZDEN*XSTARTW,IRESP,HCOMMENT=YCOMMENT)
      LTIME_WRITTEN(2)=.TRUE.
    ENDIF
    !
    IF (HMASK=='SEA   ' .AND. .NOT.LTIME_WRITTEN(3)) THEN
      XTYPE=3
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,'time',XTSTEP_OUTPUT/ZDEN*XSTARTW,IRESP,HCOMMENT=YCOMMENT)
      LTIME_WRITTEN(3)=.TRUE.
    ENDIF
    !
    IF (HMASK=='WATER ' .AND.  .NOT.LTIME_WRITTEN(4)) THEN  
      XTYPE=4
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,'time',XTSTEP_OUTPUT/ZDEN*XSTARTW,IRESP,HCOMMENT=YCOMMENT)
      LTIME_WRITTEN(4)=.TRUE.
    ENDIF
    !
    IF (HMASK=='TOWN  ' .AND. .NOT.LTIME_WRITTEN(5)) THEN     
      XTYPE=5
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,'time',XTSTEP_OUTPUT/ZDEN*XSTARTW,IRESP,HCOMMENT=YCOMMENT)
      LTIME_WRITTEN(5)=.TRUE.
    ENDIF
    !
  ENDIF
  !
ENDIF
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_OL_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
END SUBROUTINE INIT_IO_SURF_OL_n
