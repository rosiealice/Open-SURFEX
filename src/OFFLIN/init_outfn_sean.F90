!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE INIT_OUTFN_SEA_n (CHS, DGO, DGS, DGSI, DGU, O, S, SSB, UG, U, &
                                    HPROGRAM,KLUOUT)
!     ###############################
!
!!****  *INIT_OUTFN_SEA_n* -  create output files and defines variables
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
!!      modified    06-13  B. Decharme  : Delete LPROVAR_TO_DIAG here
!!                                        Add diag (Evap, Subl), GELATO
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_CH_SEAFLUX_n, ONLY : CH_SEAFLUX_t
USE MODD_DIAG_OCEAN_n, ONLY : DIAG_OCEAN_t
USE MODD_DIAG_SEAFLUX_n, ONLY : DIAG_SEAFLUX_t
USE MODD_DIAG_SEAICE_n, ONLY : DIAG_SEAICE_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_OCEAN_n, ONLY : OCEAN_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SEAFLUX_SBL_n, ONLY : SEAFLUX_SBL_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SFX_OASIS,      ONLY : LCPL_SEAICE
USE MODD_OL_FILEID,      ONLY : XVAR_TO_FILEOUT, XID, XOUT
USE MODD_OCEAN_GRID,    ONLY: NOCKMIN, NOCKMAX
!
!
USE MODN_IO_OFFLINE,    ONLY: XTSTEP_OUTPUT
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
TYPE(CH_SEAFLUX_t), INTENT(INOUT) :: CHS
TYPE(DIAG_OCEAN_t), INTENT(INOUT) :: DGO
TYPE(DIAG_SEAFLUX_t), INTENT(INOUT) :: DGS
TYPE(DIAG_SEAICE_t), INTENT(INOUT) :: DGSI
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(OCEAN_t), INTENT(INOUT) :: O
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SEAFLUX_SBL_t), INTENT(INOUT) :: SSB
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
INTEGER, INTENT(IN)           :: KLUOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100), DIMENSION(:), POINTER :: YNAME_DIM
 CHARACTER(LEN=100), DIMENSION(1) :: YATT_TITLE, YATT
 CHARACTER(LEN=40),DIMENSION(1)   :: YDATE
 CHARACTER(LEN=13),DIMENSION(1)   :: YUNIT1, YUNIT2
 CHARACTER(LEN=100)               :: YCOMMENT 
 CHARACTER(LEN=50)                :: YFILE
 CHARACTER(LEN=12)                :: YRECFM 
 CHARACTER(LEN=3)                 :: YPAS, YLVL
!
INTEGER, DIMENSION(:), POINTER   :: IDIMS, IDDIM, IDDIM1
INTEGER                          :: INI
INTEGER                          :: JLAYER
INTEGER                          :: IDIM1
INTEGER                          :: IFILE_ID, JSV, JRET
LOGICAL                          :: GMISC
!
REAL,DIMENSION(:), POINTER       :: ZX, ZY
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

! 1. Compute output lenght dimension
!-----------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_SEA_N',0,ZHOOK_HANDLE)
 CALL GET_DIM_FULL_n(U, &
                     INI)
 CALL OL_DEFINE_DIM(UG, U, &
                    HPROGRAM, KLUOUT, INI, IDIM1, YUNIT1, YUNIT2, &
                   ZX, ZY, IDIMS, IDDIM, YNAME_DIM)
 CALL GET_DATE_OL(S%TTIME,XTSTEP_OUTPUT,YDATE(1))
!
! 4. Create output file for fluxes values
!----------------------------------------------------------
!
IF (ALLOCATED(XVAR_TO_FILEOUT)) DEALLOCATE(XVAR_TO_FILEOUT)
IF (ALLOCATED(XID)) DEALLOCATE(XID)
ALLOCATE(XID(0))
XOUT=0
!
YATT_TITLE(1)='units'
!
YFILE='SEAFLUX_DIAGNOSTICS.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID)
YATT='dimensionless'
!
GMISC=(DGS%N2M>=1.OR.DGS%LSURF_BUDGET.OR.DGS%LSURF_BUDGETC)
IF (GMISC.AND.S%LHANDLE_SIC) THEN
  YATT='K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TS_SEA'   ,'Surface_temperature_over_sea',IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSRAD_SEA','Surface_radiative_temperature_over_sea',IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGS%LCOEF) THEN
  YATT='W/s2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CD_SEA'   ,'Drag_Coefficient_For_Momentum   ',IDDIM,YATT_TITLE,YATT)
  YATT='W/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CH_SEA'   ,'Drag_Coefficient_For_Heat       ',IDDIM,YATT_TITLE,YATT)
  YATT='W/s/K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CE_SEA'   ,'Drag_Coefficient_For_Evaporation',IDDIM,YATT_TITLE,YATT)
  YATT='m'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0_SEA'   ,'Roughness_Length_For_Momentum   ',IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0H_SEA'  ,'Roughness_Length_For_Heat       ',IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGS%LSURF_VARS) THEN
   YATT='kg/kg'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QS_SEA'   ,'Surface_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGS%N2M>0) THEN
   YATT='K'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2M_SEA' ,'2m_Temperature         '   ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2MMIN_SEA' ,'Minimum_2m_Temperature   '   ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2MMAX_SEA' ,'Maximum_2m_Temperature   '   ,IDDIM,YATT_TITLE,YATT)
   YATT='kg/kg'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Q2M_SEA' ,'2m_Specific_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
   YATT='(-)'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2M_SEA','2m_Relative_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2MMIN_SEA','Minimum_2m_Relative_Humidity   ' ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2MMAX_SEA','Maximum_2m_Relative_Humidity   ' ,IDDIM,YATT_TITLE,YATT)
   YATT='m/s'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ZON10M_SEA','10m_Zonal_wind       '   ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MER10M_SEA','10m_Meridian_Wind     '   ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'W10M_SEA','10m_Wind     '   ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'W10MMAX_SEA','Maximum_10m_Wind  '   ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGS%N2M>0) THEN
   YATT='-'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RI_SEA'   ,'Averaged_Richardson_Number'                ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGS%LSURF_BUDGET) THEN
   YATT='W/m2'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RN_SEA'   ,'Averaged_Net_Radiation'                    ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_SEA'    ,'Averaged_Sensible_Heat_Flux'               ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_SEA'   ,'Averaged_Total_Latent_Heat_Flux  '         ,IDDIM,YATT_TITLE,YATT)
   IF(.NOT.DGSI%LDIAG_SEAICE) THEN   
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEI_SEA'  ,'Averaged_SublimationLatent_Heat_Flux  '    ,IDDIM,YATT_TITLE,YATT)
   ENDIF
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLUX_SEA','Averaged_Ground_Heat_Flux  '               ,IDDIM,YATT_TITLE,YATT)
   IF(DGS%LRAD_BUDGET)THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWD_SEA'  ,'Averaged_Downward_SW       '             ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWU_SEA'  ,'Averaged_Upward_SW         '             ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWD_SEA'  ,'Averaged_Downward_LW       '             ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWU_SEA'  ,'Averaged_Upward_LW         '             ,IDDIM,YATT_TITLE,YATT)
   ENDIF
   YATT='kg/ms2'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMU_SEA'  ,'Averaged_Zonal_Wind_Stress '               ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMV_SEA'  ,'Averaged_Merid_Wind_Stress '               ,IDDIM,YATT_TITLE,YATT)
   YATT='kg/m2/s'  
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAP_SEA'  ,'Averaged_Total_Evaporation'    ,IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SUBL_SEA'  ,'Averaged_Sublimation_of_seaice',IDDIM,YATT_TITLE,YATT)   
ENDIF
!
IF (DGS%LSURF_BUDGET.OR.DGS%LSURF_BUDGETC)  THEN
   YATT='-'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TALB_SEA'  , 'Sea_total_albedo         '               ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGO%LDIAG_OCEAN) THEN
     ! Mean cmo temperature
     YATT='K'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TOML','Mean cmo Tempe ',IDDIM,YATT_TITLE,YATT)
     ! Mean cmo salinity
     YATT='psu'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SOML','Mean cmo Salin ',IDDIM,YATT_TITLE,YATT)
     ! Mean cmo U-current
     YATT='m/s'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'UOML','Mean cmo U-cur ',IDDIM,YATT_TITLE,YATT)
     ! Mean cmo V-current
     YATT='m/s'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'VOML','Mean cmo V-cur ',IDDIM,YATT_TITLE,YATT)
     ! Mean cmo density
     YATT='Kg/m3'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DOML','Mean cmo Densi ',IDDIM,YATT_TITLE,YATT)

ENDIF
!
IF (CHS%SVS%NBEQ>0 .AND. CHS%CCH_DRY_DEP=="WES89 ") THEN
  !
  YATT="(m/s)"
  !
  DO JSV = 1,SIZE(CHS%CCH_NAMES,1)
    !
    YRECFM = 'DV_SEA_'//TRIM(CHS%CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A7,I3.3)')'DV_SEA_',JSV    
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,YCOMMENT,IDDIM,YATT_TITLE,YATT)      
    !
  ENDDO
  !
END IF
!
IF (DGSI%LDIAG_SEAICE) THEN
!
   IF (TRIM(S%CSEAICE_SCHEME) == 'GELATO') THEN 
      YATT='m'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SIT','Sea Ice Thickness ',IDDIM,YATT_TITLE,YATT)
      YATT='-'   
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SIC','Sea Ice Cover ',IDDIM,YATT_TITLE,YATT)
      YATT='m'   
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SND','Sea Ice Snow Depth ',IDDIM,YATT_TITLE,YATT)
      YATT='K'   
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MLT','Sea Mixed Layer temp for Gelato ',IDDIM,YATT_TITLE,YATT)
   ENDIF
   IF(LCPL_SEAICE.OR.S%LHANDLE_SIC)THEN   
      YATT='K'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSICE','Sea Ice Temperature ',IDDIM,YATT_TITLE,YATT)   
      YATT='-'   
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IALB','Sea Ice Albedo ',IDDIM,YATT_TITLE,YATT)
   ENDIF
   IF (DGS%N2M>0) THEN
      YATT='-'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RI_SEAICE','Sea Ice Richardson_Number',IDDIM,YATT_TITLE,YATT)
   ENDIF
   YATT='W/m2'
   IF (DGS%LSURF_BUDGET) THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RN_SEAICE','Sea Ice Net_Radiation',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_SEAICE','Sea Ice Sensible_Heat_Flux',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_SEAICE'  ,'Averaged_SublimationLatent_Heat_Flux  '    ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLX_SEAICE','Sea Ice Ground_Heat_Flux ',IDDIM,YATT_TITLE,YATT)
      IF(DGS%LRAD_BUDGET)THEN
         CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWU_SEAICE','Sea Ice Upward_LW',IDDIM,YATT_TITLE,YATT)
         CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWU_SEAICE','Sea Ice Upward_SW',IDDIM,YATT_TITLE,YATT)
      ENDIF
   ENDIF
   YATT='kg/ms2'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMU_SEAICE','Sea Ice Zonal_Wind_Stress',IDDIM,YATT_TITLE,YATT)
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMV_SEAICE','Sea Ice Meridian_Wind_Stress',IDDIM,YATT_TITLE,YATT)
   IF (DGS%LCOEF) THEN
      YATT='W/s2'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CD_SEAICE','Sea Ice Drag_Coefficient_For_Momentum',IDDIM,YATT_TITLE,YATT)
      YATT='W/s'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CH_SEAICE','Sea Ice Drag_Coefficient_For_Heat',IDDIM,YATT_TITLE,YATT)
      YATT='m'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0_SEAICE','Sea Ice Roughness_Length_For_Momentum',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0H_SEAICE','Sea Ice Roughness_Length_For_Heat',IDDIM,YATT_TITLE,YATT)
      YATT='kg/kg'
   ENDIF
   IF (DGS%LSURF_VARS) THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QS_SEAICE','Sea Ice Surface_Humidity',IDDIM,YATT_TITLE,YATT)
      YATT='K'
   ENDIF
   IF (DGS%N2M>0) THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2M_SEAICE','Sea Ice 2m_Temperature',IDDIM,YATT_TITLE,YATT)
      YATT='kg/kg'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Q2M_SEAICE','Sea Ice 2m_Specific_Humidity ',IDDIM,YATT_TITLE,YATT)
      YATT='-'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2M_SEAICE','Sea Ice 2m_Relative_Humidity',IDDIM,YATT_TITLE,YATT)
      YATT='m/s'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ZON10M_SEAICE','Sea Ice 10m_Zonal_wind ',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MER10M_SEAICE','Sea Ice 10m_Meridian_Wind',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'W10M_SEAICE','Sea Ice 10m_Wind',IDDIM,YATT_TITLE,YATT)
   ENDIF
ENDIF
!
 CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
IF (DGS%LSURF_BUDGETC) THEN
  !        
  YFILE='SEAFLUX_DIAG_CUMUL.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
  JRET=NF_REDEF(IFILE_ID)
  !
  YATT='J/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RNC_SEA'  ,'Cumulated_Averaged_Net_Radiation'        ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HC_SEA'   ,'Cumulated_Averaged_Sensible_Heat_Flux'   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEC_SEA'  ,'Cumulated_Averaged_Total_Latent_Heat_Flux',IDDIM,YATT_TITLE,YATT)
  IF (.NOT.DGSI%LDIAG_SEAICE) THEN 
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEIC_SEA' ,'Cumulated_Averaged_Sublimation_Latent_Heat_Flux',IDDIM,YATT_TITLE,YATT)
  ENDIF
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLUXC_SEA','Cumulated_Averaged_Ground_Heat_Flux'    ,IDDIM,YATT_TITLE,YATT)
  IF(DGS%LRAD_BUDGET)THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWDC_SEA'  ,'Cumulated_Averaged_Downward_SW  '    ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWUC_SEA'  ,'Cumulated_Averaged_Upward_SW    '    ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWDC_SEA'  ,'Cumulated_Averaged_Downward_LW  '    ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWUC_SEA'  ,'Cumulated_Averaged_Upward_LW     '   ,IDDIM,YATT_TITLE,YATT)
  ENDIF
  YATT='kg/ms'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMUC_SEA'  ,'Cumulated_Averaged_Zonal_Wind_Stress '  ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMVC_SEA'  ,'Cumulated_Averaged_Merid_Wind_Stress '  ,IDDIM,YATT_TITLE,YATT)
  YATT='kg/m2'  
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAPC_SEA'  ,'Cumulated_Averaged_Total_Evaporation'    ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SUBLC_SEA'  ,'Cumulated_Averaged_Sublimation_of_seaice',IDDIM,YATT_TITLE,YATT)  
  !   
  IF (DGSI%LDIAG_SEAICE) THEN 
     YATT='J/m2'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RNC_SEAICE','Cumulated_Seaice_Net_Radiation',IDDIM,YATT_TITLE,YATT)
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HC_SEAICE','Cumulated_Seaice_Sensible_Heat_Flux',IDDIM,YATT_TITLE,YATT)
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEC_SEAICE' ,'Cumulated_Averaged_Sublimation_Latent_Heat_Flux',IDDIM,YATT_TITLE,YATT)
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLXC_SEAICE','Cumulated_Seaice_Ground_Heat_Flux',IDDIM,YATT_TITLE,YATT)
     IF(DGS%LRAD_BUDGET)THEN
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWUC_SEAICE','Cumulated_Seaice_Upward_SW',IDDIM,YATT_TITLE,YATT)
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWUC_SEAICE','Cumulated_Seaice_Upward_LW',IDDIM,YATT_TITLE,YATT)
     ENDIF
     YATT='kg/ms'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMUC_SEAICE','Cumulated_Seaice_Zonal_Wind_Stress',IDDIM,YATT_TITLE,YATT)
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMVC_SEAICE','Cumulated_Seaice_Merid_Wind_Stress',IDDIM,YATT_TITLE,YATT)
  ENDIF
  !
  CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
  !
ENDIF
!
YFILE='SEAFLUX_PROGNOSTIC.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID)
!
YATT='K'
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SST'   ,'Sea_Surface_Temperature'                    ,IDDIM,YATT_TITLE,YATT)
!
YATT='m'
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0SEA' ,'Roughness_Length'                           ,IDDIM,YATT_TITLE,YATT)
!
IF(O%LMERCATOR)THEN
!
  DO JLAYER=NOCKMIN+1,NOCKMAX
    IF (JLAYER<10) THEN
      WRITE(YPAS,'(I1.1,1X)') JLAYER
    ELSE
      WRITE(YPAS,'(I2.2,1X)') JLAYER
    ENDIF
     YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
     ! Ocean temperature
     YATT='K'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TEMP_OC'//YLVL,'Ocean Tempe '//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Ocean salinity
     YATT='psu'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SALT_OC'//YLVL,'Ocean Salinity '//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Ocean U-current
     YATT='m/s'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'UCUR_OC'//YLVL,'Ocean U-cur '//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Ocean V-current
     YATT='m/s'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'VCUR_OC'//YLVL,'Ocean V-cur '//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Ocean TKE
     YATT='J'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TKE_OC'//YLVL,'Ocean TKE '//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Ocean mixing coeff
     YATT='m2/s'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'KMEL_OC'//YLVL,'Ocean KMEL '//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Relaxation temperature
     YATT='K/s'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T_OC_REL'//YLVL,'Ocean T rel'//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Relaxation salinity
     YATT='psu/s'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'S_OC_REL'//YLVL,'Ocean S rel'//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Relaxation U
     YATT='m/s2'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'U_OC_REL'//YLVL,'Ocean U rel'//YLVL ,IDDIM,YATT_TITLE,YATT)
     ! Relaxation V
     YATT='m/s2'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'V_OC_REL'//YLVL,'Ocean V rel'//YLVL ,IDDIM,YATT_TITLE,YATT)
  END DO  
!
ENDIF
!
IF (S%LSBL) THEN
  ! 6.1 Heights
  YATT = 'm'
  DO JLAYER=1,SSB%NLVL
    WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SEA_SBL_Z'//YLVL,'Height_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
  END DO
  ! 6.2 Wind
  YATT = 'm/s'
  DO JLAYER=1,SSB%NLVL
    WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SEA_SBL_U'//YLVL,'Wind_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
  END DO
  ! 6.3 Temperature
  YATT = 'K'
  DO JLAYER=1,SSB%NLVL
    WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SEA_SBL_T'//YLVL,'Temperature_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
  END DO
  ! 6.4 Temperature
  YATT = 'kg/m3'
  DO JLAYER=1,SSB%NLVL
    WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SEA_SBL_Q'//YLVL,'Humidity_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
  END DO
  ! 6.5 Turbulence
  YATT = 'm2/s2'
  DO JLAYER=1,SSB%NLVL
    WRITE(YPAS,'(I2.2,1X)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SEA_SBL_E'//YLVL,'TKE_of_canopy_Layer_'//YLVL ,IDDIM,YATT_TITLE,YATT)
  END DO
ENDIF
!
 CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_SEA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_OUTFN_SEA_n
