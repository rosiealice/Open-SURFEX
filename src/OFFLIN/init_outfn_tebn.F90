!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE INIT_OUTFN_TEB_n (BOP, CHT, DGMTO, DGU, DGT, DGUT, UG, U, TCP, TGD, TGDO, TGR, TGRO, T, TOP, TVG, &
                                    HPROGRAM,KLUOUT)
!     ###############################
!
!
!!****  *INIT_OUTFN_TEB_n* -  create output files and defines variables
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
!!      P. LeMoigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04-04  P. LeMoigne
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_DIAG_MISC_TEB_OPTION_n, ONLY : DIAG_MISC_TEB_OPTIONS_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_DIAG_TEB_n, ONLY : DIAG_TEB_t
USE MODD_DIAG_UTCI_TEB_n, ONLY : DIAG_UTCI_TEB_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_TEB_CANOPY_n, ONLY : TEB_CANOPY_t
USE MODD_TEB_GARDEN_n, ONLY : TEB_GARDEN_t
USE MODD_TEB_GARDEN_OPTION_n, ONLY : TEB_GARDEN_OPTIONS_t
USE MODD_TEB_GREENROOF_n, ONLY : TEB_GREENROOF_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE MODD_OL_FILEID,       ONLY : XVAR_TO_FILEOUT, XID, XOUT
!

USE MODD_UTCI
!
!
USE MODD_DATA_COVER_PAR,  ONLY: NVEGTYPE
!
!
!
USE MODN_IO_OFFLINE,      ONLY : XTSTEP_OUTPUT
!
USE MODI_GET_DIM_FULL_n
USE MODI_OL_DEFINE_DIM
USE MODI_GET_DATE_OL
USE MODI_CREATE_FILE
USE MODI_DEF_VAR_NETCDF
USE MODI_OL_WRITE_COORD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
include 'netcdf.inc'

!
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT
TYPE(DIAG_MISC_TEB_OPTIONS_t), INTENT(INOUT) :: DGMTO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(DIAG_TEB_t), INTENT(INOUT) :: DGT
TYPE(DIAG_UTCI_TEB_t), INTENT(INOUT) :: DGUT
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_CANOPY_t), INTENT(INOUT) :: TCP
TYPE(TEB_GARDEN_t), INTENT(INOUT) :: TGD
TYPE(TEB_GARDEN_OPTIONS_t), INTENT(INOUT) :: TGDO
TYPE(TEB_GREENROOF_t), INTENT(INOUT) :: TGR
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
INTEGER,           INTENT(IN) :: KLUOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100), DIMENSION(:), POINTER :: YNAME_DIM
 CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YNAME_DIM1 
 CHARACTER(LEN=100), DIMENSION(1) :: YATT_TITLE, YATT 
 CHARACTER(LEN=40), DIMENSION(1)  :: YDATE
 CHARACTER(LEN=13), DIMENSION(1)  :: YUNIT1, YUNIT2
 CHARACTER(LEN=100)               :: YCOMMENT   
 CHARACTER(LEN=50)                :: YFILE 
 CHARACTER(LEN=12)                :: YRECFM
 CHARACTER(LEN=3)                 :: YPAS, YLVL 
 CHARACTER(LEN=3)                 :: YPATCH 
 CHARACTER(LEN=2)                 :: YNUM
!
INTEGER, DIMENSION(:), POINTER   :: IDIMS, IDDIM
INTEGER, DIMENSION(:), ALLOCATABLE :: IDDIM1, IDIMS1
INTEGER                          :: INI, JNBIOMASS
INTEGER                          :: IDIM1, INDIMS
INTEGER                          :: IFILE_ID, IDIMID, JSV, JSW
INTEGER                          :: JLAYER, JPATCH, JSTRESS, JRET
!
REAL,DIMENSION(:), POINTER       :: ZX, ZY
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

! 1. Compute output lenght dimension
!-----------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_TEB_N',0,ZHOOK_HANDLE)
 CALL GET_DIM_FULL_n(U, &
                     INI)
 CALL OL_DEFINE_DIM(UG, U, &
                    HPROGRAM, KLUOUT, INI, IDIM1, YUNIT1, YUNIT2, &
                   ZX, ZY, IDIMS, IDDIM, YNAME_DIM)
 CALL GET_DATE_OL(TOP%TTIME,XTSTEP_OUTPUT,YDATE(1))
!
INDIMS = SIZE(IDDIM)
!
ALLOCATE(IDIMS1(INDIMS+1))
IDIMS1(INDIMS+1) = IDIMS(INDIMS)
IDIMS1(1:INDIMS-1) = IDIMS(1:INDIMS-1)
IDIMS1(INDIMS) = 1
ALLOCATE(YNAME_DIM1(INDIMS+1))
YNAME_DIM1(INDIMS+1) = YNAME_DIM(INDIMS)
YNAME_DIM1(1:INDIMS-1) = YNAME_DIM(1:INDIMS-1)
YNAME_DIM1(INDIMS) = "Snow_patches"
ALLOCATE(IDDIM1(INDIMS+1))
!
! 4. Create output file for fluxes values
!----------------------------------------------------------

IF (ALLOCATED(XVAR_TO_FILEOUT)) DEALLOCATE(XVAR_TO_FILEOUT)
IF (ALLOCATED(XID)) DEALLOCATE(XID)
ALLOCATE(XID(0))
XOUT=0
!
YATT_TITLE(1)='units'
!
YFILE='TEB_DIAGNOSTICS.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID)
!
IF (DGMTO%LSURF_MISC_BUDGET) THEN
  YATT='-'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'D_RD','Road fraction',IDDIM,YATT_TITLE,YATT)   
  DO JPATCH=1,TOP%NTEB_PATCH

    YPATCH = '   '
    IF (TOP%NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',JPATCH,'_'
    YATT='(m)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'Z0_TOWN'),'Town_Rougness_Length',IDDIM,YATT_TITLE,YATT)
    YATT='(W/m2)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'XQF_BLD'),'Domestic_heating',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'XQF_TOWN'),'Anthropogenic heat',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'XDQS_TOWN'),'Storage',IDDIM,YATT_TITLE,YATT)
    YATT='(kg/m2/s)'  
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFF_TW'),'aggregated runoff for town',IDDIM,YATT_TITLE,YATT)
    YATT='(W/m2)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_RD'),'net radiation at road',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_RD'),'road sensible heat flux',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_RD'),'road latent heat flux',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_RD'),'net road conduction flux',IDDIM,YATT_TITLE,YATT)
    YATT='(kg/m2/s)'  
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFF_RD'),'road surface runoff',IDDIM,YATT_TITLE,YATT)
    YATT='(W/m2)'
    IF (TOP%CWALL_OPT=='UNIF') THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_WL'),'net radiation for wall',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_WL'),'wall sensible heat flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_WL'),'net wall conduction flux',IDDIM,YATT_TITLE,YATT)
    ELSE
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_WLA'),'net radiation for wall A',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_WLA'),'wall A sensible heat flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_WLA'),'net wall A conduction flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_WLB'),'net radiation for wall B',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_WLB'),'wall B sensible heat flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_WLB'),'net wall B conduction flux',IDDIM,YATT_TITLE,YATT)
    ENDIF
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_RF'),'net radiation for roof',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_RF'),'roof sensible heat flux',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_RF'),'roof latent heat flux',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_RF'),'net roof conduction flux',IDDIM,YATT_TITLE,YATT)
    YATT='(kg/m2/s)'  
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFF_RF'),'roof surface runoff',IDDIM,YATT_TITLE,YATT)
    YATT='(W/m2)'
    IF (TOP%LGARDEN) THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_GD'),'net radiation for GARDEN areas',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_GD'),'GARDEN area sensible heat flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_GD'),'GARDEN area latent heat flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_GD'),'net GARDEN area conduction flux',IDDIM,YATT_TITLE,YATT)
      YATT='(kg/m2/s)'  
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFF_GD'),'garden surface runoff',IDDIM,YATT_TITLE,YATT)
    ENDIF
    YATT='(W/m2)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_BLT'),'net radiation for built surfaces',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_BLT'),'built surface sensible heat flux',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_BLT'),'built surface latent heat flux',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_BLT'),'built surface conduction flux',IDDIM,YATT_TITLE,YATT)

    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_RF'),'Sdown absorbed by roofs',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_SN_RF'),'Sdown absorbed by snow on roofs',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_RF'),'Ldown absorbed by roofs',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_SN_RF'),'Ldown absorbed by snow on roofs',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_RD'),'Sdown absorbed by roads',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_SN_RD'),'Sdown absorbed by snow on roads',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_RD'),'Ldown absorbed by roads',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_SN_RD'),'Ldown absorbed by snow on roads',IDDIM,YATT_TITLE,YATT)
    IF (TOP%CWALL_OPT=='UNIF') THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_WL'),'Sdown absorbed by wall',IDDIM,YATT_TITLE,YATT) 
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_WL'),'Ldown absorbed by wall',IDDIM,YATT_TITLE,YATT)    
    ELSE
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_WLA'),'Sdown absorbed by wall A',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_WLA'),'Ldown absorbed by wall A',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_WLB'),'Sdown absorbed by wall B',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_WLB'),'Ldown absorbed by wall B',IDDIM,YATT_TITLE,YATT)
    ENDIF
    IF (TOP%LGARDEN) THEN
      YATT='(W/m2)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_GD'),'Sdown absorbed by GARDEN areas',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_GD'),'Ldown absorbed by GARDEN areas',IDDIM,YATT_TITLE,YATT)
    ENDIF

    YATT='(W/m2)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'REF_SW_GO'),'Total solar rad reflected by ground',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWE_GO'),'LW emitted by ground',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'REF_SW_FA'),'Total solar rad reflected by facade',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWE_FA'),'LW emitted by facade',IDDIM,YATT_TITLE,YATT)

    IF (TOP%CBEM=='BEM') THEN
      YATT='(K)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'CL_CURT'),'Current Cooling system temperature set point',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'HT_CURT'),'Current Heating system temperature set point',IDDIM,YATT_TITLE,YATT)
      YATT='(W m-2(floor))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'QIN_CUR'),'Current Building internal heat loads',IDDIM,YATT_TITLE,YATT)
      
      YATT='(W m-2(bld))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'XFLX_BLD'),'Heat flux from bld',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_BLD_CL'),'sensible cooling demand',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'T_BLD_CL'),'Total cooling demand',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_BLD_HT'),'sensible heating demand',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_BLD_CL'),'latent cooling demand',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_BLD_HT'),'latent heating demand',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_WASTE'),'sensible waste heat from HVAC',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_WASTE'),'latent waste heat from HVAC',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'HVAC_CL'),'cooling energy consumption',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'HVAC_HT'),'heating energy consumption',IDDIM,YATT_TITLE,YATT)
      YATT='(W m-2(bld))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'CAP_SYS'),'Actual capacity of the cooling system',IDDIM,YATT_TITLE,YATT)
      YATT='(kg s-1 m-2(bld))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'M_SYS'),'ctual HVAC mass flow rate',IDDIM,YATT_TITLE,YATT)
      YATT='(-)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'COP'),'Actual COP of the cooling system',IDDIM,YATT_TITLE,YATT)
      YATT='(kg kg-1)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'Q_SYS'),'Supply air specific humidity',IDDIM,YATT_TITLE,YATT)
      YATT='(K)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'T_SYS'),'Supply air temperature',IDDIM,YATT_TITLE,YATT)
      YATT='(W m-2(bld))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TR_SW_WIN'),'Solar radiation transmitted through windows',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'FAN_POWER'),'HVAC fan power',IDDIM,YATT_TITLE,YATT)
      YATT='(K)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'T_RAD_IND'),'Indoor mean radiant temperature',IDDIM,YATT_TITLE,YATT)
      YATT='(-)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'HU_BLD'),'Indoor relative humidity',IDDIM,YATT_TITLE,YATT)
      YATT='(W/m2)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_WIN'),'Sdown absorbed by windows',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_WIN'),'Ldown absorbed by windows',IDDIM,YATT_TITLE,YATT)
    ENDIF

    IF (TOP%LGREENROOF) THEN
      YATT='(W/m2)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_GR'),'net radiation for GREENROOFs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_GR'),'sensible heat flux for GREENROOFs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_GR'),'latent heat flux for GREENROOFs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_GR'),'net conduction flux for GREENROOFs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_GR'),'Sdown absorbed by GREENROOFs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_GR'),'Ldown absorbed by GREENROOFs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'G_GR_ROOF'),'heat flux between GREENROOF and ROOF',IDDIM,YATT_TITLE,YATT)
      YATT='(kg/m2/s)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFF_FR'),'GREENROOF soil surface runoff',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'DRAIN_GR'),'GREENROOF total vertical drainage',IDDIM,YATT_TITLE,YATT)
      YATT='(W/m2)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_SR'),'structural roof net radiation',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_SR'),'structural roof sensible heat flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LE_SR'),'structural roof latent heat flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GFLUX_SR'),'structural roof net conduction flux',IDDIM,YATT_TITLE,YATT)
      YATT='(kg/m2/s)'  
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFF_SR'),'structural roof surface runoff',IDDIM,YATT_TITLE,YATT)
    ENDIF

    IF (TOP%LSOLAR_PANEL) THEN
      YATT='(W/m2(panel))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'SWA_SP'),'Shortwave absorbed by solar panels on roofs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'LWA_SP'),'Longwave  absorbed by solar panels on roofs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RN_SP'),'Net radiation of solar panels on roofs',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'H_SP'),'Sensible Heat flux  from solar panels on roofs',IDDIM,YATT_TITLE,YATT)
      YATT='(W/m2(photovoltaic panel))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'PHOT_SP'),'Photovolatic production',IDDIM,YATT_TITLE,YATT)
      YATT='(W/m2(thermal panel))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'THER_SP'),'Hot water production',IDDIM,YATT_TITLE,YATT)
      YATT='(W/m2(panel))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'PROD_SP'),'Production by solar panels on roofs',IDDIM,YATT_TITLE,YATT)
      YATT='(W/m2(bld))'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'PHOT_BLD'),'Photovolatic production',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'THER_BLD'),'Hot water production',IDDIM,YATT_TITLE,YATT)
    END IF

  ENDDO

ENDIF
!
IF (DGT%N2M>0) THEN
  YATT='-'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RI_TEB'   ,'Averaged_Richardson_Number'                ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGT%LSURF_BUDGET) THEN
  YATT='(W/m2)'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RN_TEB'   ,'Averaged_Net_Radiation'                    ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_TEB'    ,'Averaged_Sensible_Heat_Flux'               ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_TEB'   ,'Averaged_Latent_Heat_Flux  '               ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLUX_TEB','Averaged_Ground_Heat_Flux  '               ,IDDIM,YATT_TITLE,YATT)
  IF (DGT%LRAD_BUDGET) THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWD_TEB'  ,'Averaged_Downward_SW       '               ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWU_TEB'  ,'Averaged_Upward_SW         '               ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWD_TEB'  ,'Averaged_Downward_LW       '               ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWU_TEB'  ,'Averaged_Upward_LW         '               ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMU_TEB'  ,'Averaged_Zonal_Wind_Stress '               ,IDDIM,YATT_TITLE,YATT)
    DO JSW=1, SIZE(DGT%XSWBD,2)
      YNUM=ACHAR(48+JSW)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWD_TEB_'//YNUM,'X_Y_SWD_TEB_'//YNUM,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWU_TEB_'//YNUM,'X_Y_SWD_TEB_'//YNUM,IDDIM,YATT_TITLE,YATT)
    ENDDO
  ENDIF
  YATT='(kg/ms2)'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMU_TEB'  ,'Averaged_Zonal_Wind_Stress '               ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMV_TEB'  ,'Averaged_Merid_Wind_Stress '               ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (CHT%SVT%NBEQ>0 .AND. CHT%CCH_DRY_DEP=="WES89 ") THEN
  !
  YATT="(m/s)"
  !
  DO JSV = 1,SIZE(CHT%CCH_NAMES,1)
    !
    YRECFM = 'DV_TWN_'//TRIM(CHT%CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A7,I3.3)')'DV_TWN_',JSV
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,YCOMMENT,IDDIM,YATT_TITLE,YATT)      
    !
  ENDDO
  !
END IF
!
IF (DGT%LCOEF)  THEN
  YATT='W/s2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CD_TEB'   ,'Drag_Coefficient_For_Momentum   '   ,IDDIM,YATT_TITLE,YATT)
  YATT='W/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CH_TEB'   ,'Drag_Coefficient_For_Heat       '   ,IDDIM,YATT_TITLE,YATT)
  YATT='W/s/K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CE_TEB'   ,'Drag_Coefficient_For_Evaporation'   ,IDDIM,YATT_TITLE,YATT)
  YATT='m'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0_TEB'   ,'Roughness_Length_For_Momentum'   ,IDDIM,YATT_TITLE,YATT)
  YATT='m'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0H_TEB'  ,'Roughness_Length_For_Heat'   ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGT%LSURF_VARS) THEN
  YATT='kg/kg'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QS_TEB'   ,'Surface_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGT%N2M>0) THEN
  YATT='K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2M_TEB' ,'2m_Temperature         '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2MMIN_TEB' ,'Minimum 2m_Temperature         '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2MMAX_TEB' ,'Maximum 2m_Temperature         '   ,IDDIM,YATT_TITLE,YATT)
  YATT='kg/kg'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Q2M_TEB' ,'2m_Specific_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
  YATT='(-)'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2M_TEB','2m_Relative_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2MMIN_TEB','Minimum 2m_Relative_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2MMAX_TEB','Maximum 2m_Relative_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
  YATT='m/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ZON10M_TEB','10m_Zonal_wind       '   ,IDDIM,YATT_TITLE,YATT)
  YATT='m/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MER10M_TEB','10m_Meridian_Wind     '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'W10M_TEB','10m_Wind Strength    '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'W10MMAX_TEB','Maximum 10m_Wind Strength    '   ,IDDIM,YATT_TITLE,YATT)
  YATT='kg/m2/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SFCO2_TEB','CO2 Flux              '   ,IDDIM,YATT_TITLE,YATT)  
ENDIF
!
IF (DGUT%LUTCI .AND. DGT%N2M >0) THEN
!RJ: extended ascii should be avoided in I/O
  YATT='°C'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'UTCI_IN','UTCI for person indoor'   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'UTCI_OUTSUN','UTCI for person at sun'   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'UTCI_OUTSHAD','UTCI for person at shade'   ,IDDIM,YATT_TITLE,YATT)
  YATT='K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TRAD_SUN','Mean radiant temperature seen by person at sun'   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TRAD_SHAD','Mean radiant temperature seen by person in shade'   ,IDDIM,YATT_TITLE,YATT)

  YATT='s'
  DO JSTRESS=1,NUTCI_STRESS
    YRECFM='UTCIC_IN_'//CUTCI_STRESS_NAMES(JSTRESS)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,                                                         &
      'Cumulated time spent in'//CUTCI_STRESS_NAMES(JSTRESS)//' stress range for person indoor' ,&
      IDDIM,YATT_TITLE,YATT)
  END DO
  DO JSTRESS=1,NUTCI_STRESS
    YRECFM='UTCIC_SU_'//CUTCI_STRESS_NAMES(JSTRESS)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,                                                         &
      'Cumulated time spent in'//CUTCI_STRESS_NAMES(JSTRESS)//' stress range for person at sun' ,&
      IDDIM,YATT_TITLE,YATT)
  END DO
  DO JSTRESS=1,NUTCI_STRESS
    YRECFM='UTCIC_SH_'//CUTCI_STRESS_NAMES(JSTRESS)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,                                                          &
      'Cumulated time spent in'//CUTCI_STRESS_NAMES(JSTRESS)//' stress range for person in shade',&
      IDDIM,YATT_TITLE,YATT)
  END DO
END IF

 CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
!
IF (DGT%LPGD .AND. ASSOCIATED(T%CUR%XBLD)) THEN

  YFILE='TEB_PGD.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
  JRET=NF_REDEF(IFILE_ID)
  !
  YATT='(-)'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'BLD','building fraction',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WALL_O_HOR','Wall surface over plan area surface',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  YATT='(m)'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'BLD_HEIGHT','Building Height',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0_TOWN','Town roughness length',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  YATT='(-)'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'XROAD_DIR','Road direction',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GARDEN_FRAC','Garden fraction',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GREENROOF_FRAC','Greenroof fraction',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PANEL_FRAC','Solar panel fraction',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALB_ROOF','Roof Albedo',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EMIS_ROOF','Roof Emissivity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  DO JLAYER=1,TOP%NROOF_LAYER
    WRITE(YRECFM,FMT='(A,I1.1)') 'HC_ROOF',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Roof Heat Capacity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    WRITE(YRECFM,FMT='(A,I1.1)') 'TC_ROOF',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Roof thermal conductivity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    WRITE(YRECFM,FMT='(A,I1.1)') 'D_ROOF',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Roof layer thickness',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  ENDDO
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ROUGH_ROOF','Roof Roughness',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RESIDENTIAL','Residential use fraction',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALB_WALL','Wall Albedo',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EMIS_WALL','Wall Emissivity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  DO JLAYER=1,TOP%NWALL_LAYER
    WRITE(YRECFM,FMT='(A,I1.1)') 'HC_WALL',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Wall Heat Capacity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    WRITE(YRECFM,FMT='(A,I1.1)') 'TC_WALL',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Wall thermal conductivity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    WRITE(YRECFM,FMT='(A,I1.1)') 'D_WALL',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Wall layer thickness',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  ENDDO
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ROUGH_WALL','Wall Roughness',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALB_ROAD','Road Albedo',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EMIS_ROAD','Road Emissivity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  DO JLAYER=1,TOP%NROAD_LAYER
    WRITE(YRECFM,FMT='(A,I1.1)') 'HC_ROAD',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Road Heat Capacity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    WRITE(YRECFM,FMT='(A,I1.1)') 'TC_ROAD',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Road thermal conductivity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    WRITE(YRECFM,FMT='(A,I1.1)') 'D_ROAD',JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'Road layer thickness',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  ENDDO
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_TRAFFIC','Traffic Heat Flux',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_TRAFFIC','Traffic Latent Flux',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_INDUSTRY','INDUSTRY Heat Flux',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_INDUSTRY','INDUSTRY Latent Flux',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  !
  IF (TOP%CBEM=='BEM') THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'N_FLOOR','Number of floors',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    DO JLAYER=1,BOP%NFLOOR_LAYER
      WRITE(YRECFM,FMT='(A,I1.1)') 'HC_FLOOR',JLAYER
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'FLOOR Heat Capacity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
      WRITE(YRECFM,FMT='(A,I1.1)') 'TC_FLOOR',JLAYER
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'FLOOR thermal conductivity',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
      WRITE(YRECFM,FMT='(A,I1.1)') 'D_FLOOR',JLAYER
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'FLOOR layer thickness',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    ENDDO          
  ENDIF
  !
  IF (TOP%LGARDEN) THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_LAI','leaf area index',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_VEG','',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_Z0VEG','',IDDIM,YATT_TITLE,YATT)
    YATT='(m)'
    DO JLAYER=1,TGDO%NGROUND_LAYER
      WRITE(YRECFM,FMT='(A5,I1)') 'GD_DG',JLAYER
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,'',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    ENDDO
    IF (TVG%CHORT=='SGH') THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_ICE','soil ice depth for runoff',IDDIM,YATT_TITLE,YATT)
    ENDIF
    YATT='(-)'
    DO JLAYER=1,NVEGTYPE
      WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_VEGTY_P'//YLVL,'fraction of each vegetation type',IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
    ENDDO
    YATT='(s.m-1)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_RSMIN','minimum stomatal resistance',IDDIM,YATT_TITLE,YATT)
    YATT='(-)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_GAMMA','coefficient for RSMIN calculation',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_CV','vegetation thermal inertia coefficient',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_RGL','maximum solar radiation usable in photosynthesis',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_EMIS_ISBA','surface emissivity',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_WRMAX_CF','coefficient for maximum water interception',IDDIM,YATT_TITLE,YATT)
    IF (DGMTO%LSURF_DIAG_ALBEDO) THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_ALBNIR_S','soil near-infra-red albedo',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_ALBVIS_S','soil visible albedo',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_ALBUV_S','soil UV albedo',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_ALBNIR_T','total near-infra-red albedo',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_ALBVIS_T','total visible albedo',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GD_ALBUV_T','total UV albedo',IDDIM,YATT_TITLE,YATT)
    ENDIF
  ENDIF
  !
  CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
  !
ENDIF
!
YFILE='TEB_DIAG_CUMUL.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID)
!
IF (DGMTO%LSURF_MISC_BUDGET) THEN
  DO JPATCH=1,TOP%NTEB_PATCH

    YPATCH = '   '
    IF (TOP%NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',JPATCH,'_'
    IF  (TOP%CBEM=='BEM') THEN
      YATT='J/m2'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'HVACC_CL'),'cumulated cooling energy consumption',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'HVACC_HT'),'cumulated heating energy consumption',IDDIM,YATT_TITLE,YATT)
    END IF
    YATT='kg/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFFC_TW'),'cumulated aggregated runoff for town',IDDIM,YATT_TITLE,YATT)
    YATT='kg/m2(road)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFFC_RD'),'cumulated road surface runoff',IDDIM,YATT_TITLE,YATT)
    YATT='kg/m2(roof)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFFC_RF'),'cumulated roof surface runoff',IDDIM,YATT_TITLE,YATT)
    YATT='kg/m2(road)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'IRRIGC_RD'),'cumulated road irrigation',IDDIM,YATT_TITLE,YATT)

    IF (TOP%LGARDEN) THEN
      YATT='kg/m2(garden)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFFC_GD'),'cumulated garden surface runoff',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'DRAINC_GD'),'cumulated garden surface drainage',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'IRRIGC_GD'),'cumulated garden irrigation',IDDIM,YATT_TITLE,YATT)
    END IF

    IF (TOP%LGREENROOF) THEN
      YATT='kg/m2(greenroof)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFFC_GR'),'cumulated greenroof surface runoff',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'DRAINC_GR'),'cumulated greenroof surface drainage',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'IRRIGC_GR'),'cumulated greenroof irrigation',IDDIM,YATT_TITLE,YATT)
      YATT='kg/m2(roof)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RUNOFFC_SR'),'cumulated structural roof surface runoff',IDDIM,YATT_TITLE,YATT)
    END IF

    IF (TOP%LSOLAR_PANEL) THEN
      YATT='J/m2(bld)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'PHOTC_BLD'),'Cumulated Photovolatic production',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'THERC_BLD'),'Cumulated Hot Water production',IDDIM,YATT_TITLE,YATT)
    END IF

  END DO
END IF
!
 CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
!
! 5. Create output file for prognostic and semi-pronostic variable in teb
!------------------------------------------------------------------------

YFILE='TEB_PROGNOSTIC.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS1,YNAME_DIM1,IFILE_ID,IDDIM1)
JRET=NF_REDEF(IFILE_ID)
IDDIM(1:INDIMS-1) = IDDIM1(1:INDIMS-1)
IDDIM(INDIMS) = IDDIM1(INDIMS+1)
!
! 5.1 Temperatures
YATT='K'
!
DO JPATCH=1,TOP%NTEB_PATCH

  YPATCH = '   '
  IF (TOP%NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',JPATCH,'_'

  ! Roof temperatures
  DO JLAYER=1,TOP%NROOF_LAYER
    WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TROOF'//YLVL),'Roof_Temperature_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
  END DO
  ! Road temperatures
  DO JLAYER=1,TOP%NROAD_LAYER
    WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TROAD'//YLVL),'Road_Temperature_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
  END DO
  ! Wall temperatures
  DO JLAYER=1,TOP%NWALL_LAYER
    WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    IF (TOP%CWALL_OPT=='UNIF') THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TWALL'//YLVL),'Wall_Temperature_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
    ELSE
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TWALLA'//YLVL),'Wall_Temperature_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TWALLB'//YLVL),'Wall_Temperature_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
    ENDIF  
  END DO
  ! Internal building temperature
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TI_BLD'),'Internal_Building_Temperature',IDDIM,YATT_TITLE,YATT)
  !
  ! Deep road temperature
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TI_ROAD'),'Deep_Road_Temperature',IDDIM,YATT_TITLE,YATT)
  !
  ! 5.2 Water contents
  !
  YATT = 'kg/m2'
  ! Roof water content
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'WS_ROOF'),'Roof_Water_Content_Layer' ,IDDIM,YATT_TITLE,YATT)
  !
  ! Road water content
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'WS_ROAD'),'Road_Water_Content_Layer' ,IDDIM,YATT_TITLE,YATT)
  !
  ! 5.3 semi pronostic variables
  !
  ! temperature of canyon air
  YATT = 'K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'T_CANYON'),'Canyon_Air_Temperature',IDDIM,YATT_TITLE,YATT)
  !
  ! humidity of canyon air
  YATT = 'kg/kg'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'Q_CANYON'),'Canyon_Air_Humidity',IDDIM,YATT_TITLE,YATT)
  !
  !* Thermal solar panels present day production
  !
  YATT = 'J/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'THER_PDAY'),'Thermal Solar Panels present day production',IDDIM,YATT_TITLE,YATT)
  !
  !* BEM semi-prognostic variables
  !
  IF (TOP%CBEM=='BEM') THEN
    YATT = 'kg/kg'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'QI_BLD'),'QI_BLD',IDDIM,YATT_TITLE,YATT)
    YATT='(K)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'T_WIN2'),'T_WIN2',IDDIM,YATT_TITLE,YATT)
    DO JLAYER=1,BOP%NFLOOR_LAYER
      WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TFLOO'//YLVL),'Floor_Temperature_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TMASS'//YLVL),'Mass_Temperature_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
    END DO
  ENDIF
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'WSN_RF1'), 'Rf_Snow_Water_Eq_layer_1', IDDIM1, YATT_TITLE, (/'Kg/m2'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RSN_RF1'), 'Rf_snow_density_layer_1', IDDIM1, YATT_TITLE, (/'Kg/m3'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TSN_RF1'), 'Rf_snow_temperature_1', IDDIM1, YATT_TITLE, (/'K'/))
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'ASNOW_RF'), 'Rf_snow_albedo_1', IDDIM1, YATT_TITLE, (/'-'/))
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'WSN_RD1'), 'Rd_snow_Water_Eq_layer_1', IDDIM1, YATT_TITLE, (/'Kg/m2'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'RSN_RD1'), 'Rd_snow_density_layer_1', IDDIM1, YATT_TITLE, (/'Kg/m3'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'TSN_RD1'), 'Rd_snow_temperature_1', IDDIM1, YATT_TITLE, (/'K'/))
  !  
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'ASNOW_RD'), 'Rd_snow_albedo_1', IDDIM, YATT_TITLE, (/'-'/))
  !
  IF (TOP%LGARDEN) THEN
    !
    DO JLAYER=1,TGDO%NGROUND_LAYER
      WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GD_TG'//YLVL),'GD_TG',IDDIM,YATT_TITLE,(/'K'/))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GD_WG'//YLVL),'GD_WG',IDDIM,YATT_TITLE,(/'m3/m3'/))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GD_WGI'//YLVL),'GD_WG',IDDIM,YATT_TITLE,(/'m3/m3'/))
    ENDDO
    !
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GD_WR'), 'GD_WR', IDDIM, YATT_TITLE, (/'(kg/m2)'/))
    !
    IF (TVG%CPHOTO/='NON' .AND. TVG%CPHOTO/='AGS' .AND. TVG%CPHOTO/='AST') THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GD_LAI'), 'GD_LAI', IDDIM, YATT_TITLE, (/'(m2/m2)'/))
    ENDIF
    !
    IF (TVG%CPHOTO=='NIT') THEN
      DO JNBIOMASS=1,TVG%NNBIOMASS
        WRITE(YPAS,'(I3)') JNBIOMASS ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GD_BIOMA'), 'GD_BIOMA', IDDIM, YATT_TITLE, (/'(kg/m2)'/))
      ENDDO
      DO JNBIOMASS=2,TVG%NNBIOMASS
        WRITE(YPAS,'(I3)') JNBIOMASS ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GD_RESPI'), 'GD_RESPI', IDDIM, YATT_TITLE, (/'(kg/m2/s)'/))
      ENDDO  
    ENDIF
    !    
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GD_RES'), 'GD_RES', IDDIM, YATT_TITLE, (/'(kg/m2)'/))
    !
    DO JLAYER=1,TGD%CUR%TSNOW%NLAYER
      WRITE(YPAS,'(I3)') JLAYER; YLVL = ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WSN_GD'//YLVL, 'Gd_Snow_Water_Eq_layer_'//YLVL, IDDIM1, YATT_TITLE, (/'Kg/m2'/))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'RSN_GD'//YLVL, 'Gd_snow_density_layer_'//YLVL ,         IDDIM1, YATT_TITLE, (/'Kg/m3'/))
      IF (TGD%CUR%TSNOW%SCHEME=='3-L' .OR. TGD%CUR%TSNOW%SCHEME=='CRO') THEN   
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'HSN_GD'//YLVL,  'Gd_snow_heat_layer'//YLVL,              IDDIM1, YATT_TITLE, (/'J/m2'/))
      ELSE
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TSN_GD'//YLVL,  'Gd_snow_temp_layer'//YLVL,              IDDIM1, YATT_TITLE, (/'K'/))
      ENDIF
      IF (TGD%CUR%TSNOW%SCHEME=='CRO') THEN   
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SG1_GD'//YLVL, 'Gd_snow_grain_par1_layer_'//YLVL, IDDIM1, YATT_TITLE, (/'-'/))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SG2_GD'//YLVL, 'Gd_snow_grain_par2_layer_'//YLVL, IDDIM1, YATT_TITLE, (/'-'/))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SHI_GD'//YLVL,  'Gd_snow_hist_par_layer_'//YLVL, IDDIM1, YATT_TITLE, (/'-'/))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SAG_GD'//YLVL,   'Gd_snow_age_par_layer_'//YLVL, IDDIM1, YATT_TITLE,&
             (/'days since snowfall'/))
      ENDIF
    ENDDO
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ASNOW_GD', 'Gd_snow_albedo', IDDIM1, YATT_TITLE, (/'-'/))    
    !
  ENDIF      
  !
  IF (TOP%LGREENROOF) THEN
    !
    DO JLAYER=1,TGRO%NLAYER_GR
      WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GR_TG'//YLVL),'GR_TG',IDDIM,YATT_TITLE,(/'K'/))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GR_WG'//YLVL),'GR_WG',IDDIM,YATT_TITLE,(/'m3/m3'/))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GR_WGI'//YLVL),'GR_WG',IDDIM,YATT_TITLE,(/'m3/m3'/))
    ENDDO
    !
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GR_WR'), 'GR_WR', IDDIM, YATT_TITLE, (/'(kg/m2)'/))
    !
    IF (TVG%CPHOTO/='NON' .AND. TVG%CPHOTO/='AGS' .AND. TVG%CPHOTO/='AST') THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GR_LAI'), 'GR_LAI', IDDIM, YATT_TITLE, (/'(m2/m2)'/))
    ENDIF
    !
    IF (TVG%CPHOTO=='NIT') THEN
      DO JNBIOMASS=1,TVG%NNBIOMASS
        WRITE(YPAS,'(I3)') JNBIOMASS ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GR_BIOMA'), 'GR_BIOMA', IDDIM, YATT_TITLE, (/'(kg/m2)'/))
      ENDDO
      DO JNBIOMASS=2,TVG%NNBIOMASS
        WRITE(YPAS,'(I3)') JNBIOMASS ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GR_RESPI'), 'GR_RESPI', IDDIM, YATT_TITLE, (/'(kg/m2/s)'/))
      ENDDO  
    ENDIF
    !    
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,ADJUSTL(YPATCH//'GR_RES'), 'GR_RES', IDDIM, YATT_TITLE, (/'(kg/m2)'/))
    !
    DO JLAYER=1,TGR%CUR%TSNOW%NLAYER
      WRITE(YPAS,'(I3)') JLAYER; YLVL = ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WSN_GR'//YLVL, 'Gr_Snow_Water_Eq_layer_'//YLVL, IDDIM1, YATT_TITLE, (/'Kg/m2'/))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'RSN_GR'//YLVL, 'Gr_snow_density_layer_'//YLVL ,         IDDIM1, YATT_TITLE, (/'Kg/m3'/))
      IF (TGR%CUR%TSNOW%SCHEME=='3-L' .OR. TGR%CUR%TSNOW%SCHEME=='CRO') THEN   
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'HSN_GR'//YLVL,  'Gr_snow_heat_layer'//YLVL,              IDDIM1, YATT_TITLE, (/'J/m2'/))
      ELSE
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TSN_GR'//YLVL,  'Gr_snow_temp_layer'//YLVL,              IDDIM1, YATT_TITLE, (/'K'/))
      ENDIF
      IF (TGR%CUR%TSNOW%SCHEME=='CRO') THEN   
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SG1_GR'//YLVL, 'Gr_snow_grain_par1_layer_'//YLVL, IDDIM1, YATT_TITLE, (/'-'/))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SG2_GR'//YLVL, 'Gr_snow_grain_par_layer_'//YLVL, IDDIM1, YATT_TITLE, (/'-'/))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SHI_GR'//YLVL,  'Gr_snow_hist_par_layer_'//YLVL, IDDIM1, YATT_TITLE, (/'-'/))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SAG_GR'//YLVL,   'Gr_snow_age_param_layer_'//YLVL       , IDDIM1, YATT_TITLE,&
             (/'days since snowfall'/))
      ENDIF
    ENDDO
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ASNOW_GR', 'Gr_snow_albedo', IDDIM1, YATT_TITLE, (/'-'/))    
    !    
  ENDIF
  !
ENDDO
!
 CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
! 6. Create output file for prognostic variables in teb canopy
!------------------------------------------------------------------------
!
YFILE='TEB_CANOPY.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID)
!
! 6.1 Heights
YATT = 'm'
!
DO JLAYER=1,TCP%NLVL
  WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TEB_CAN_Z'//YLVL,'Height_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
END DO
!
! 6.2 Wind
YATT = 'm/s'
DO JLAYER=1,TCP%NLVL
  WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TEB_CAN_U'//YLVL,'Wind_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
END DO
!
! 6.3 Temperature
YATT = 'K'
DO JLAYER=1,TCP%NLVL
  WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TEB_CAN_T'//YLVL,'Temperature_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
END DO
!
! 6.4 Temperature
YATT = 'kg/m3'
DO JLAYER=1,TCP%NLVL
  WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TEB_CAN_Q'//YLVL,'Humidity_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
END DO
!
! 6.5 Turbulence
YATT = 'm2/s2'
DO JLAYER=1,TCP%NLVL
  WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TEB_CAN_E'//YLVL,'TKE_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
END DO
!
 CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_TEB_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_OUTFN_TEB_n
