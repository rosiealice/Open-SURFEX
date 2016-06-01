!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE INIT_OUTFN_FLAKE_n (CHW, DGF, DGU, F, FSB, UG, U, &
                                      HPROGRAM,KLUOUT)
!     ###############################
!
!!****  *INIT_OUTFN_FLAKE_n* -  create output files and defines variables
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
!!      Modified 06-10 by S. Faroux
!!      Modified 04-13 by P. Le Moigne
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
!
!
USE MODD_CH_WATFLUX_n, ONLY : CH_WATFLUX_t
USE MODD_DIAG_FLAKE_n, ONLY : DIAG_FLAKE_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_FLAKE_SBL_n, ONLY : FLAKE_SBL_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_OL_FILEID,    ONLY : XVAR_TO_FILEOUT, XID, XOUT
!
USE MODN_IO_OFFLINE,   ONLY : XTSTEP_OUTPUT
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
TYPE(CH_WATFLUX_t), INTENT(INOUT) :: CHW
TYPE(DIAG_FLAKE_t), INTENT(INOUT) :: DGF
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(FLAKE_SBL_t), INTENT(INOUT) :: FSB
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
INTEGER,           INTENT(IN) :: KLUOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100), DIMENSION(:), POINTER :: YNAME_DIM
!
 CHARACTER(LEN=40),DIMENSION(1)   :: YDATE
 CHARACTER(LEN=13),DIMENSION(1)   :: YUNIT1, YUNIT2
 CHARACTER(LEN=100)               :: YCOMMENT 
 CHARACTER(LEN=50)                :: YFILE
 CHARACTER(LEN=100), DIMENSION(1) :: YATT_TITLE, YATT
 CHARACTER(LEN=12)                :: YRECFM 
 CHARACTER(LEN=2)                 :: YLVLV
!
INTEGER, DIMENSION(:), POINTER   :: IDIMS, IDDIM
INTEGER                          :: INI
INTEGER                          :: IDIM1
INTEGER                          :: IFILE_ID, JSV, JRET
INTEGER                          :: JLAYER
!
REAL,DIMENSION(:), POINTER       :: ZX, ZY
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

! 1. Compute output lenght dimension
!-----------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_FLAKE_N',0,ZHOOK_HANDLE)
 CALL GET_DIM_FULL_n(U, &
                     INI)
!
 CALL OL_DEFINE_DIM(UG, U, &
                    HPROGRAM, KLUOUT, INI, IDIM1, YUNIT1, YUNIT2, &
                   ZX, ZY, IDIMS, IDDIM, YNAME_DIM)
 CALL GET_DATE_OL(F%TTIME,XTSTEP_OUTPUT,YDATE(1))
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
YFILE='FLAKE_DIAGNOSTICS.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID)
YATT='dimensionless'
!
IF (DGF%LCOEF) THEN
  YATT='W/s2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CD_WAT'   ,'Drag_Coefficient_For_Momentum   '   ,IDDIM,YATT_TITLE,YATT)
  YATT='W/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CH_WAT'   ,'Drag_Coefficient_For_Heat       '   ,IDDIM,YATT_TITLE,YATT)
  YATT='W/s/K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CE_WAT'   ,'Drag_Coefficient_For_Evaporation'   ,IDDIM,YATT_TITLE,YATT)
  YATT='m'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0_WAT'   ,'Roughness_Length_For_Momentum'   ,IDDIM,YATT_TITLE,YATT)
  YATT='m'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0H_WAT'  ,'Roughness_Length_For_Heat'   ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGF%LSURF_VARS) THEN
  YATT='kg/kg'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QS_WAT'   ,'Surface_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (DGF%N2M>0) THEN
  YATT='K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2M_WAT' ,'2m_Temperature           '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2MMIN_WAT' ,'Minimum_2m_Temperature'   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2MMAX_WAT' ,'Maximum_2m_Temperature'   ,IDDIM,YATT_TITLE,YATT)
  YATT='kg/kg'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Q2M_WAT' ,'2m_Specific_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
  YATT='(-)'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2M_WAT','2m_Relative_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2MMIN_WAT','Minimum_2m_Relative_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2MMAX_WAT','Maximum_2m_Relative_Humidity   '   ,IDDIM,YATT_TITLE,YATT)
  YATT='m/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ZON10M_WAT','10m_Zonal_wind       '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MER10M_WAT','10m_Meridian_Wind    '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'W10M_WAT','10m_Wind_Strength     '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'W10MMAX_WAT','Maximum_10m_Wind_Strength     '   ,IDDIM,YATT_TITLE,YATT)  
ENDIF
!
IF (DGF%N2M>0) THEN
  YATT='-'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RI_WAT'      ,'Averaged_Richardson_Number'                ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF(DGF%LSURF_BUDGET) THEN
  YATT='W/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RN_WAT'      ,'Averaged_Net_Radiation'                    ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_WAT'       ,'Averaged_Sensible_Heat_Flux'               ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_WAT'      ,'Averaged_Latent_Heat_Flux  '               ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEI_WAT'     ,'Averaged_Sublimation_Latent_Heat_Flux  '   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLUX_WAT'   ,'Averaged_Ground_Heat_Flux  '               ,IDDIM,YATT_TITLE,YATT)
  IF(DGF%LRAD_BUDGET)THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWD_WAT'     ,'Averaged_Downward_SW       '               ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWU_WAT'     ,'Averaged_Upward_SW         '               ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWD_WAT'     ,'Averaged_Downward_LW       '               ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWU_WAT'     ,'Averaged_Upward_LW         '               ,IDDIM,YATT_TITLE,YATT)
  ENDIF
  YATT='kg/ms2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMU_WAT'     ,'Averaged_Zonal_Wind_Stress '               ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMV_WAT'     ,'Averaged_Merid_Wind_Stress '               ,IDDIM,YATT_TITLE,YATT)
  YATT = 'kg/m2s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAP_WAT'   ,'Averaged_Total_Evaporation'                ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SUBL_WAT'   ,'Averaged_Sublimation_of_ice/snow'          ,IDDIM,YATT_TITLE,YATT)  
ENDIF
!
IF (DGF%LSURF_BUDGET.OR.DGF%LSURF_BUDGETC)  THEN
   YATT='-'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TALB_WAT'  , 'Flake_total_albedo         '               ,IDDIM,YATT_TITLE,YATT)
   YATT='kg/m2'
   CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WSN_WAT'   , 'Flake_snow_water_equivalent'               ,IDDIM,YATT_TITLE,YATT)
ENDIF
!
IF (CHW%SVW%NBEQ>0 .AND. CHW%CCH_DRY_DEP=="WES89 ") THEN
  !
  YATT="(m/s)"
  !
  DO JSV = 1,SIZE(CHW%CCH_NAMES,1)
    !
    YRECFM = 'DV_WAT_'//TRIM(CHW%CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A7,I3.3)')'DV_WAT_',JSV    
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,YCOMMENT,IDDIM,YATT_TITLE,YATT)      
    !
  ENDDO
  !
END IF
!
 CALL OL_WRITE_COORD(DGU, &
                    YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
IF (DGF%LSURF_BUDGETC) THEN
  !        
  YFILE='FLAKE_DIAG_CUMUL.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
  !
  YATT='J/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RNC_WAT'  ,'Cumulated_Averaged_Net_Radiation'        ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HC_WAT'   ,'Cumulated_Averaged_Sensible_Heat_Flux'   ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEC_WAT'  ,'Cumulated_Averaged_Total_Latent_Heat_Flux',IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEIC_WAT' ,'Cumulated_Averaged_Sublimation_Latent_Heat_Flux',IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLUXC_WAT','Cumulated_Averaged_Ground_Heat_Flux'    ,IDDIM,YATT_TITLE,YATT)
  IF(DGF%LRAD_BUDGET)THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWDC_WAT'  ,'Cumulated_Averaged_Downward_SW  '    ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWUC_WAT'  ,'Cumulated_Averaged_Upward_SW    '    ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWDC_WAT'  ,'Cumulated_Averaged_Downward_LW  '    ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWUC_WAT'  ,'Cumulated_Averaged_Upward_LW     '   ,IDDIM,YATT_TITLE,YATT)
  ENDIF
  YATT='kg/ms'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMUC_WAT'  ,'Cumulated_Averaged_Zonal_Wind_Stress '  ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMVC_WAT'  ,'Cumulated_Averaged_Merid_Wind_Stress '  ,IDDIM,YATT_TITLE,YATT)
  YATT='kg/m2'  
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAPC_WAT'  ,'Cumulated_Averaged_Total_Evaporation'      ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SUBLC_WAT'  ,'Cumulated_Averaged_Sublimation_of_ice/snow',IDDIM,YATT_TITLE,YATT)  
  !     
  CALL OL_WRITE_COORD(DGU, &
                    YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
  !
ENDIF
!
YFILE='FLAKE_PROGNOSTIC.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID)
YATT='dimensionless'
!
YATT='K'
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TS_WATER'   , 'Averaged_Water_S_Temperature'                 ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T_MNW'      , 'Averaged_Water_Temperature  '                 ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T_WML'      , 'Mixed_layer_wat_temperature '                 ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T_BOT'      , 'Bottom_Water_Temperature    '                 ,IDDIM,YATT_TITLE,YATT)
YATT='m' 
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_ML'       , 'Mixed_Layer_Depth           '                 ,IDDIM,YATT_TITLE,YATT)
YATT=' '
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CT'         , 'Termocline_Shape_Factor        '              ,IDDIM,YATT_TITLE,YATT)
YATT='K'
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T_SNOW'     , 'Temperature at the air-snow interface'        ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T_ICE'      , 'Ice_surface_Temperature  '                    ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T_B1'       , 'Temperature of the upper layer of sediments ' ,IDDIM,YATT_TITLE,YATT)
YATT='m'
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_SNOW'     , 'Snow thickness'              ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_ICE'      , 'Ice thickness'               ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_B1'       , 'Thickness of the upper layer of sediments'     ,IDDIM,YATT_TITLE,YATT)
YATT='m'
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_SNOW'     , 'Snow thickness'              ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_ICE'      , 'Ice thickness'               ,IDDIM,YATT_TITLE,YATT)
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_B1'       , 'Thickness of the upper layer of sediments'     ,IDDIM,YATT_TITLE,YATT)
YATT='m'
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0WATER'    ,'Roughness length            '       ,IDDIM,YATT_TITLE,YATT)
YATT='m/s'
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'USTAR_WATER','friction velocity           '       ,IDDIM,YATT_TITLE,YATT)
!
!
IF (F%LSBL) THEN
  DO JLAYER=1,FSB%NLVL
    WRITE(YLVLV,'(I2.2)') JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WAT_SBL_Z'//YLVLV,'Canopy height',   IDDIM,YATT_TITLE,(/'m'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WAT_SBL_U'//YLVLV,'Canopy wind',     IDDIM,YATT_TITLE,(/'m/s'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WAT_SBL_T'//YLVLV,'Canopy temp',     IDDIM,YATT_TITLE,(/'K'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WAT_SBL_Q'//YLVLV,'Canopy humidity', IDDIM,YATT_TITLE,(/'kg/m3'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WAT_SBL_E'//YLVLV,'Canopy TKE',      IDDIM,YATT_TITLE,(/'m2/s2'/))
  END DO
ENDIF
!
 CALL OL_WRITE_COORD(DGU, &
                    YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_FLAKE_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_OUTFN_FLAKE_n
