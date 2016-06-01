!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_OUTPUT_OL_n (YSC)
!     ######################
!
!!****  *INIT_OUTPUT_OL* Keep in memory the netcdf ID of the output files
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
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_OL_FILEID, ONLY : XVAR_TO_FILEOUT,         &
                           XID, XOUT,               &
                           XVAR_SURF, XID_SURF,     &
                           XVAR_NATURE, XID_NATURE, &
                           XVAR_SEA, XID_SEA,       &
                           XVAR_WATER, XID_WATER,   &
                           XVAR_TOWN, XID_TOWN  
USE MODD_IO_SURF_OL
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODI_INIT_OUTFN_FLAKE_n
USE MODI_INIT_OUTFN_ISBA_n
USE MODI_INIT_OUTFN_SEA_n
USE MODI_INIT_OUTFN_SURF_ATM_n
USE MODI_INIT_OUTFN_TEB_n
USE MODI_INIT_OUTFN_WATER_n
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
!
INTEGER           :: IRET
INTEGER           :: ILUOUT
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!------------------------------------------------------------------------------ 
IF (LHOOK) CALL DR_HOOK('INIT_OUTPUT_OL_N',0,ZHOOK_HANDLE)
!
ILUOUT = 0
!
IF (NRANK==NPIO) THEN
  !
  IF (.NOT. LDEFINED_SURF_ATM) THEN         
    CALL INIT_OUTFN_SURF_ATM_n(YSC%CHE, YSC%CHN, YSC%CHU, YSC%DGU, YSC%UG, YSC%U, YSC%SV, &
                               "NC    ",ILUOUT)
    CALL ALLOCATE_FILL_VAR(XVAR_SURF, XID_SURF)
    LDEFINED_SURF_ATM=.TRUE.
  ENDIF
  !
  IF (.NOT. LDEFINED_NATURE .AND. YSC%U%NDIM_NATURE>0) THEN
    IF (YSC%U%CNATURE=='ISBA  '.OR.YSC%U%CNATURE=='TSZ0  ') THEN 
      CALL INIT_OUTFN_ISBA_n(YSC%IM%CHI, YSC%IM%DGEI, YSC%IM%DGI, YSC%IM%DGMI, &
                             YSC%DGU, YSC%IM%GB, YSC%IM%ICP, YSC%IM%I, YSC%UG, YSC%U, &
                             "NC    ",ILUOUT)
      CALL ALLOCATE_FILL_VAR(XVAR_NATURE, XID_NATURE)
    ENDIF
    LDEFINED_NATURE=.TRUE.
  ENDIF
  !
  IF (.NOT. LDEFINED_SEA .AND. YSC%U%NDIM_SEA>0) THEN
    IF (YSC%U%CSEA=='SEAFLX') THEN 
      CALL INIT_OUTFN_SEA_n(YSC%SM%CHS, YSC%SM%DGO, YSC%SM%DGS, YSC%SM%DGSI, &
                            YSC%DGU, YSC%SM%O, YSC%SM%S, YSC%SM%SSB, YSC%UG, YSC%U, &
                            "NC    ",ILUOUT)
      CALL ALLOCATE_FILL_VAR(XVAR_SEA, XID_SEA)
    ENDIF
    LDEFINED_SEA=.TRUE.
  ENDIF
  !
  IF (.NOT. LDEFINED_WATER .AND. YSC%U%NDIM_WATER>0) THEN
    IF (YSC%U%CWATER=='WATFLX') CALL INIT_OUTFN_WATER_n(YSC%WM%CHW, YSC%DGU, YSC%WM%DGW, &
                                                   YSC%UG, YSC%U, YSC%WM%W, YSC%WM%WSB, &
                                                   "NC    ",ILUOUT)
    IF (YSC%U%CWATER=='FLAKE ') CALL INIT_OUTFN_FLAKE_n(YSC%WM%CHW, YSC%FM%DGF, YSC%DGU, &
                                                    YSC%FM%F, YSC%FM%FSB, YSC%UG, YSC%U, &
                                                   "NC    ",ILUOUT)
    IF (YSC%U%CWATER=='WATFLX' .OR. YSC%U%CWATER=='FLAKE') CALL ALLOCATE_FILL_VAR(XVAR_WATER, XID_WATER)
    LDEFINED_WATER=.TRUE.
  ENDIF
  !
  IF (.NOT. LDEFINED_TOWN .AND. YSC%U%NDIM_TOWN>0) THEN
    IF (YSC%U%CTOWN=='TEB   ') THEN 
      CALL INIT_OUTFN_TEB_n(YSC%TM%BOP, YSC%TM%CHT, YSC%TM%DGMTO, YSC%DGU, YSC%TM%DGT, YSC%TM%DGUT, &
                            YSC%UG, YSC%U, YSC%TM%TCP, YSC%GDM%TGD, YSC%GDM%TGDO, YSC%GRM%TGR, &
                            YSC%GRM%TGRO, YSC%TM%T, YSC%TM%TOP, YSC%GDM%TVG, &
                            "NC    ",ILUOUT)
      CALL ALLOCATE_FILL_VAR(XVAR_TOWN, XID_TOWN)
    ENDIF
    LDEFINED_TOWN=.TRUE.
  ENDIF
  !
ENDIF
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_OUTPUT_OL_N',1,ZHOOK_HANDLE)
 CONTAINS 
!------------------------------------------------------------------------------
SUBROUTINE ALLOCATE_FILL_VAR(HVAR, NVAR)
  
 CHARACTER(LEN=20),DIMENSION(:), POINTER :: HVAR
INTEGER*4, DIMENSION(:), POINTER :: NVAR
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ALLOCATE_FILL_VAR',0,ZHOOK_HANDLE)
ALLOCATE(HVAR(XOUT))
ALLOCATE(NVAR(XOUT))
HVAR(:)=XVAR_TO_FILEOUT(1:XOUT)
NVAR(:)=XID(1:XOUT)
IF (LHOOK) CALL DR_HOOK('ALLOCATE_FILL_VAR',1,ZHOOK_HANDLE)

END SUBROUTINE ALLOCATE_FILL_VAR
!------------------------------------------------------------------------------
!
END SUBROUTINE INIT_OUTPUT_OL_n
