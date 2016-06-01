!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE INIT_OUTFN_ISBA_n (CHI, DGEI, DGI, DGMI, DGU, GB, ICP, I, UG, U, &
                                     HPROGRAM,KLUOUT)
!     ###############################
!
!
!!****  *INIT_OUTFN_ISBA_n* -  create output files and defines variables
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
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07-03
!!      modified    11-03, by P. Le Moigne   *Meteo France*
!!      modified    05-04, by P. Le Moigne : surf_atm diagnostics moved at the
!!                                           right place
!!      modified    10-04, by P. Le Moigne : add new diagnostics
!!      modified    10-04, by P. Le Moigne : add Halstead coefficient
!!      modified     2008, by B. Decharme  : limit the number of diag
!!                                           Add floodplains diag
!!      modified    04-09, by A.L. Gibelin : Add respiration diagnostics
!!      modified    05-09, by A.L. Gibelin : Add carbon spinup
!!      modified    07-09, by A.L. Gibelin : Add carbon prognostic variables
!!      modified    12-11, by B. Decharme  : correct some bug
!!      modified    09-12, by B. Decharme  : delete LPROVAR_TO_DIAG for prognostic variables
!!                                           delete NWG_LAYER
!!                                           Erroneous description in diag comments
!!      modified    06-13, by B. Decharme  : good dimension for Tg, Wg, et Wgi
!!                                           bug : TSN_VEG if Snowlayer = 1 ; 
!!                                           bug : TSRAD_P and not TTSRAD_P
!!                                           add diag (Qsb, Subl) and Snow noted SN
!!      modified    10-14, by P. Samuelsson: Added MEB output
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
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_ISBA_n, ONLY : DIAG_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
USE MODD_ISBA_CANOPY_n, ONLY : ISBA_CANOPY_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,         ONLY : NUNDEF
USE MODD_DATA_COVER_PAR,   ONLY : NVEGTYPE
USE MODD_OL_FILEID,        ONLY : XVAR_TO_FILEOUT, XID, XOUT
USE MODD_ASSIM ,           ONLY : LASSIM, CASSIM
USE MODD_AGRI  ,           ONLY : LAGRIP
!
!
USE MODN_IO_OFFLINE,       ONLY : XTSTEP_OUTPUT
!
USE MODI_GET_DIM_FULL_n
USE MODI_GET_ISBA_CONF_n
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
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DGEI
TYPE(DIAG_ISBA_t), INTENT(INOUT) :: DGI
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DGMI
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(ISBA_CANOPY_t), INTENT(INOUT) :: ICP
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
INTEGER,          INTENT(IN) :: KLUOUT
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
 CHARACTER(LEN=3)                 :: YISBA
 CHARACTER(LEN=2)                 :: YLVLV
! 
REAL,DIMENSION(:), POINTER       :: ZX, ZY
!
INTEGER, DIMENSION(:), POINTER   :: IDIMS, IDDIM
INTEGER, DIMENSION(:), ALLOCATABLE :: JDIM 
INTEGER                          :: INI, INPATCH, INLVLD, INLVLS, INBIOMASS, &
                                    INLITTER, INLITTLEVS, INSOILCARB
INTEGER                          :: JLAYER, JVEG, JNBIOMASS, JNLITTER, JNLITTLEVS, JNSOILCARB
INTEGER                          :: IDIM1, INDIMS
INTEGER                          :: IFILE_ID, IDIMID, JSV
INTEGER                          :: IL, JRET
INTEGER                          :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

! 1. Compute output lenght dimension
!-----------------------------------

IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_ISBA_N',0,ZHOOK_HANDLE)
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
!
 CALL GET_DIM_FULL_n(U, &
                     INI)
 CALL GET_ISBA_CONF_n(I, &
                      YISBA, INPATCH, INLVLD, INLVLS, INBIOMASS, &
                       INLITTER, INLITTLEVS, INSOILCARB)  
!
 CALL OL_DEFINE_DIM(UG, U, &
                    HPROGRAM, KLUOUT, INI, IDIM1, YUNIT1, YUNIT2, &
                   ZX, ZY, IDIMS, IDDIM, YNAME_DIM, KNPATCH=INPATCH)
 CALL GET_DATE_OL(I%TTIME,XTSTEP_OUTPUT,YDATE(1))
!
INDIMS = SIZE(IDDIM)
ALLOCATE(JDIM(INDIMS-1))
!
! 4. Create output file for prognostic variables
!----------------------------------------------------------
!
IF (ALLOCATED(XVAR_TO_FILEOUT)) DEALLOCATE(XVAR_TO_FILEOUT)
IF (ALLOCATED(XID)) DEALLOCATE(XID)
ALLOCATE(XID(0))
XOUT=0
!
!
YFILE='ISBA_PROGNOSTIC.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID) 
!
IF (IDIM1.NE.0) THEN
  JDIM(1)=IDDIM(1)
  JDIM(2)=IDDIM(2)
  JDIM(3)=IDDIM(4)
ELSE
  JDIM(1)=IDDIM(1)
  JDIM(2)=IDDIM(3)
ENDIF
!
IF(I%LTEMP_ARP)THEN
  IL=I%NTEMPLAYER_ARP
ELSEIF(I%CISBA=='DIF')THEN
  IL=INLVLD
ELSE
  IL=2
ENDIF
!
YATT_TITLE(1)='units'
! 
DO JLAYER=1,IL
  WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TG'//YLVL , 'Soil_temp_layer_'//YLVL  , IDDIM, YATT_TITLE, (/'Kelvin'/))
ENDDO
!
IL=INLVLD
!
DO JLAYER=1,IL
  WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WG'//YLVL , 'Soil_liquid_layer_'//YLVL, IDDIM, YATT_TITLE, (/'m3/m3'/))
ENDDO
!  
IF(I%CISBA/='DIF')THEN
   IL=2
ENDIF
DO JLAYER=1,IL
  WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WGI'//YLVL, 'Soil_ice_layer_'//YLVL, IDDIM, YATT_TITLE, (/'m3/m3'/))
ENDDO  
!
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WR'  , 'Interception_reservoir', IDDIM, YATT_TITLE, (/'mm'/))
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'RESA', 'Aerodynamic_resistance', IDDIM, YATT_TITLE, (/'s/m'/))
!
IF (ISIZE_LMEB_PATCH>0) THEN
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WRV'  , 'MEB: water intercepted on canopy vegetation leaves', &
                      IDDIM, YATT_TITLE, (/'mm'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WRVN' , 'MEB: snow intercepted on canopy vegetation leaves', &
                      IDDIM, YATT_TITLE, (/'mm'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TV'   , 'MEB: canopy vegetation temperature', &
                      IDDIM, YATT_TITLE, (/'K'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TC'   , 'MEB: vegetation canopy air temperature', &
                      IDDIM, YATT_TITLE, (/'K'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'QC'   , 'MEB: vegetation canopy specifc humidity', &
                      IDDIM, YATT_TITLE, (/'kg/kg'/))
ENDIF
!
IF(I%LGLACIER)THEN
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ICE_STO',   'Glacier_reservoir',        IDDIM, YATT_TITLE, (/'Kg/m2'/))
ENDIF
!
DO JLAYER=1,INLVLS
  WRITE(YPAS,'(I3)') JLAYER
  YLVL = ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WSN_VEG'//YLVL, 'Snow_Water_Equivalent_layer_'//YLVL, IDDIM, YATT_TITLE, (/'Kg/m2'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'RSN_VEG'//YLVL, 'Snow_density_layer_'//YLVL ,         IDDIM, YATT_TITLE, (/'Kg/m3'/))
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN   
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'HSN_VEG'//YLVL,  'Snow_heat_layer'//YLVL,              IDDIM, YATT_TITLE, (/'J/m2'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SAG_VEG'//YLVL,'Snow_age_param_layer_'//YLVL, IDDIM, YATT_TITLE,(/'days_since_snowfall'/))
  ELSEIF(I%TSNOW%SCHEME=='1-L') THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TSN_VEG'//YLVL,  'Snow_temp_layer'//YLVL,              IDDIM, YATT_TITLE, (/'K'/))
  ENDIF
  IF (I%TSNOW%SCHEME=='CRO') THEN   
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SG1_VEG'//YLVL,'Snow_grain_parameter1_layer_'//YLVL, IDDIM, YATT_TITLE, (/'-'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SG2_VEG'//YLVL,'Snow_grain_parameter2_layer_'//YLVL, IDDIM, YATT_TITLE, (/'-'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SHI_VEG'//YLVL,'Snow_historical_param_layer_'//YLVL, IDDIM, YATT_TITLE, (/'-'/))
  ENDIF
ENDDO
!
 CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ASN_VEG', 'Snow_albedo', IDDIM, YATT_TITLE, (/'-'/))
!
IF (I%CPHOTO /= 'NON') THEN
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'AN'   , 'Net CO2 Assimilation'      , IDDIM, YATT_TITLE, (/'kgCO2/kgair m/s'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ANFM' , 'Leaf CO2 Assimilation'     , IDDIM, YATT_TITLE, (/'kgCO2/kgair m/s'/))
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ANDAY', 'Daily Net CO2 Assimilation', IDDIM, YATT_TITLE, (/'kgCO2/m2/day'/))
ENDIF
!
IF (I%CPHOTO == 'NIT' .OR. I%CPHOTO == 'NCB') THEN
  DO JNBIOMASS=1,INBIOMASS
    WRITE(YPAS,'(I3)') JNBIOMASS
    YLVL = ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'BIOMA'//YLVL, 'Plant biomass'//YLVL, IDDIM, YATT_TITLE, (/'kgDM/m2'/))
  END DO
ENDIF
!
IF (I%CRESPSL=='CNT') THEN
  DO JNLITTER=1,INLITTER
    DO JNLITTLEVS=1,INLITTLEVS
      WRITE(YPAS,'(I1,A1,I1)') JNLITTER,'_',JNLITTLEVS
      YLVL = ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'LITTER'//YLVL, 'Litter pool'//YLVL, IDDIM, YATT_TITLE, (/'gC/m2'/))
    END DO
  END DO  
  DO JNSOILCARB=1,INSOILCARB
    WRITE(YPAS,'(I3)') JNSOILCARB
    YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SOILCARB'//YLVL, 'Soil carbon pool'//YLVL, IDDIM, YATT_TITLE, (/'gC/m2'/))
  END DO
  DO JNLITTLEVS=1,INLITTLEVS
    WRITE(YPAS,'(I3)') JNLITTLEVS
    YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'LIGNIN_STR'//YLVL, 'Ratio Lignin/Carbon in structural litter'//YLVL, &
      IDDIM, YATT_TITLE, (/'gC/m2'/))
  END DO
ENDIF
!
IF (I%LCANOPY) THEN
  DO JLAYER=1,ICP%NLVL
    WRITE(YLVLV,'(i2.2)') JLAYER
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ISBA_CAN_Z'//YLVLV, 'Canopy height'  , JDIM, YATT_TITLE, (/'m'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ISBA_CAN_U'//YLVLV, 'Canopy wind'    , JDIM, YATT_TITLE, (/'m/s'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ISBA_CAN_T'//YLVLV, 'Canopy temp'    , JDIM, YATT_TITLE, (/'K'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ISBA_CAN_Q'//YLVLV, 'Canopy humidity', JDIM, YATT_TITLE, (/'kg/m3'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ISBA_CAN_E'//YLVLV, 'Canopy TKE'     , JDIM, YATT_TITLE, (/'m2/s2'/))
  END DO
ENDIF
!
 CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
!
! 4. Create output file for fluxes values
!----------------------------------------------------------
!
YFILE='ISBA_DIAGNOSTICS.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
JRET=NF_REDEF(IFILE_ID) 
YATT = 'dimensionless'
!
IF (IDIM1.NE.0) THEN
  JDIM(1)=IDDIM(1)
  JDIM(2)=IDDIM(2)
  JDIM(3)=IDDIM(4)
ELSE
  JDIM(1)=IDDIM(1)
  JDIM(2)=IDDIM(3)
ENDIF
!
IF (DGI%LCOEF) THEN
  YATT = 'W/s2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'CD_ISBA' , 'Drag_Coefficient_For_Momentum   ', JDIM, YATT_TITLE, YATT)
  YATT = 'W/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'CH_ISBA' , 'Drag_Coefficient_For_Heat       ', JDIM, YATT_TITLE, YATT)
  YATT = 'W/s/K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'CE_ISBA' , 'Drag_Coefficient_For_Evaporation', JDIM, YATT_TITLE, YATT)
  YATT = 'm'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'Z0_ISBA' , 'Roughness_Length_For_Momentum'   , JDIM, YATT_TITLE, YATT)
  YATT = 'm'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'Z0H_ISBA', 'Roughness_Length_For_Heat'       , JDIM, YATT_TITLE, YATT)
ENDIF
!
IF (DGI%LSURF_VARS) THEN
  YATT = 'kg/kg'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QS_ISBA' , 'Surface_Humidity   '             , JDIM, YATT_TITLE, YATT)
ENDIF
!
IF (DGI%N2M>0) THEN
  !
  YATT = 'dimensionless'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RI_ISBA' , 'Averaged_Richardson_Number'      , JDIM, YATT_TITLE, YATT) 
  YATT = 'K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'T2M_ISBA'    , '2m_Temperature         '      , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'T2MMIN_ISBA' , 'Minimum_2m_Temperature '      , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'T2MMAX_ISBA' , 'Maximum_2m_Temperature '      , JDIM, YATT_TITLE, YATT)
  YATT = 'kg/kg'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'Q2M_ISBA'    , '2m_Specific_Humidity   '      , JDIM, YATT_TITLE, YATT)
  YATT = '(-)'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'HU2M_ISBA'   , '2m_Relative_Humidity   '      , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'HU2MMIN_ISBA', 'Minimum_2m_Relative_Humidity ', JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'HU2MMAX_ISBA', 'Maximum_2m_Relative_Humidity ', JDIM, YATT_TITLE, YATT)
  YATT = 'm/s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'ZON10M_ISBA' , '10m_Zonal_wind       '        , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'MER10M_ISBA' , '10m_Meridian_Wind     '       , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'W10M_ISBA'   , '10m_Wind     '                , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'W10MMAX_ISBA', 'Maximum_10m_Wind     '        , JDIM, YATT_TITLE, YATT)
  !
  IF(DGI%LPATCH_BUDGET) THEN
    YATT='K'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2M_P'   ,'2m_Temperature'        ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2MMIN_P','Minimum_2m_Temperature',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'T2MMAX_P','Maximum_2m_Temperature',IDDIM,YATT_TITLE,YATT)
    YATT='kg/kg'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Q2M_P'   ,'2m_Specific_Humidity'  ,IDDIM,YATT_TITLE,YATT)
    YATT='(-)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HU2M_P'  ,'2m_Relative_Humidity'  ,IDDIM,YATT_TITLE,YATT)
    YATT='m/s'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ZON10M_P','10m_Zonal_wind'        ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MER10M_P','10m_Meridian_Wind'     ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'W10M_P'  ,'10m_Wind'              ,IDDIM,YATT_TITLE,YATT)
  ENDIF
  !
ENDIF
!
IF (DGI%LSURF_BUDGET)  THEN
  !
  YATT = 'W/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'RN_ISBA'     , 'Averaged_Net_Radiation'                , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'H_ISBA'      , 'Averaged_Sensible_Heat_Flux'           , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'LE_ISBA'     , 'Averaged_Total_Latent_Heat_Flux  '     , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'LEI_ISBA'    , 'Averaged_Sublimation_Latent_Heat_Flux ', JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'GFLUX_ISBA'  , 'Averaged_Ground_Heat_Flux  '           , JDIM, YATT_TITLE, YATT)
  !
  IF(DGI%LRAD_BUDGET)THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SWD_ISBA'  , 'Averaged_Downward_SW       '           , JDIM, YATT_TITLE, YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SWU_ISBA'  , 'Averaged_Upward_SW         '           , JDIM, YATT_TITLE, YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'LWD_ISBA'  , 'Averaged_Downward_LW       '           , JDIM, YATT_TITLE, YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'LWU_ISBA'  , 'Averaged_Upward_LW         '           , JDIM, YATT_TITLE, YATT)
  ENDIF
  !
  YATT = 'Pa'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'FMU_ISBA'    , 'Averaged_Zonal_Wind_Stress '           , JDIM, YATT_TITLE, YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'FMV_ISBA'    , 'Averaged_Merid_Wind_Stress '           , JDIM, YATT_TITLE, YATT)
  !
  IF (DGI%LPATCH_BUDGET) THEN
    !
    YATT = 'W/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RN_P'        ,'Net_Radiation'                               ,IDDIM,YATT_TITLE,YATT)    
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_P'         ,'Sensible_Heat_Flux'                          ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_P'        ,'Total_Latent_Heat_Flux'                      ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEI_P'       ,'Sublimatiob_Latent_Heat_Flux'                ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLUX_P'     ,'Ground_Heat_Flux'                            ,IDDIM,YATT_TITLE,YATT)
    !
    IF(DGI%LRAD_BUDGET) THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWD_P'    ,'Downward_SW       '                           ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWU_P'    ,'Upward_SW         '                           ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWD_P'    ,'Downward_LW       '                           ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWU_P'    ,'Upward_LW         '                           ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    !
    YATT = 'Pa'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMU_P'      ,'Zonal_Wind_Stress '                           ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMV_P'      ,'Merid_Wind_Stress '                           ,IDDIM,YATT_TITLE,YATT)
    !
  ENDIF
  !
ENDIF
!
!
IF (DGI%LPATCH_BUDGET.AND.LAGRIP .AND. (I%CPHOTO=='NIT' .OR. I%CPHOTO=='LAI' .OR. I%CPHOTO=='LST' .OR. I%CPHOTO=='NCB')) THEN
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'IRRISEUIL'   , 'Irrigation_Threshold'                 , IDDIM, YATT_TITLE, YATT)
ENDIF
!
IF (DGEI%LSURF_EVAP_BUDGET) THEN
  !
  YATT = 'W/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEG_ISBA'    ,'Averaged_Ground_Evaporation_Heat_Flux'               ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEGI_ISBA'   ,'Averaged_Soil_Ice_Sublimation'                       ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEV_ISBA'    ,'Averaged_Vegetation_Evaporation_Heat_Flux'           ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LES_ISBA'    ,'Averaged_Snow_Sublimation_Heat_Flux'                 ,JDIM,YATT_TITLE,YATT)
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LESL_ISBA'   ,'Averaged_Snow_Evaporation_Heat_Flux'              ,JDIM,YATT_TITLE,YATT)
  ENDIF
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LER_ISBA'    ,'Averaged_Canopy_Direct_Evaporation_Heat_Flux'        ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LETR_ISBA'   ,'Averaged_Vegetation_Transpiration_Heat_Flux'         ,JDIM,YATT_TITLE,YATT)
  YATT = 'kg/m2s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAP_ISBA'   ,'Averaged_Evapotranspiration'                         ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SUBL_ISBA'   ,'Averaged_Sublimation_of_ice/snow'                    ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DRAIN_ISBA'  ,'Averaged_Soil_Drainage_Flux'                         ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RUNOFF_ISBA' ,'Averaged_Supersaturation_Runoff'                     ,JDIM,YATT_TITLE,YATT)
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNDRIF_ISBA'   ,'Averaged_blowing_snow_sublimation'              ,JDIM,YATT_TITLE,YATT)
  ENDIF
  IF(I%CRUNOFF=='SGH'.AND.I%CISBA=='DIF')THEN  
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QSB_ISBA'  ,'Averaged_lateral_subsurface_flow'                    ,JDIM,YATT_TITLE,YATT)
  ENDIF
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HORTON_ISBA' ,'Averaged_Horton_Surface_Runoff'                      ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DRIVEG_ISBA'  ,'Averaged_Dripping_from_the_vegetation_reservoir'   ,JDIM,YATT_TITLE,YATT)
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RRVEG_ISBA'   ,'Averaged_Precipitation_Intercepted_by_Vegetation'  ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNOMLT_ISBA'  ,'Averaged_Snow_melt_flux'                           ,JDIM,YATT_TITLE,YATT)
  IF(LAGRIP) CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IRRIG_ISBA'   ,'Averaged_irrigation_rate'              ,JDIM,YATT_TITLE,YATT)
  !
  IF (ISIZE_LMEB_PATCH>0) THEN
    YATT = 'W/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEVCV_ISBA'  ,'MEB: total evapotranspiration from vegetation canopy overstory' ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LESC_ISBA'   ,'MEB: total snow sublimation from vegetation canopy overstory'   ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LETRGV_ISBA' ,'MEB: transpiration from understory vegetation'                  ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LETRCV_ISBA' ,'MEB: transpiration from overstory canopy vegetation'            ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LERGV_ISBA'  ,'MEB: interception evaporation from understory vegetation'       ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LERCV_ISBA'  ,'MEB: interception evaporation from overstory canopy vegetation' ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_V_C_ISBA' ,'MEB: latent heat flux from vegetation canopy overstory'         ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_G_C_ISBA' ,'MEB: latent heat flux from understory'                          ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_C_A_ISBA' ,'MEB: latent heat flux from canopy air space to the atmosphere'  ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_N_C_ISBA' ,'MEB: latent heat flux from the snow on the ground'              ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWNET_V_ISBA' ,'MEB: net vegetation canopy shortwave radiation'                ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWNET_G_ISBA' ,'MEB: net ground shortwave radiation'                           ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWNET_N_ISBA' ,'MEB: net snow shortwave radiation'                             ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWNET_NS_ISBA' ,'MEB: net snow shortwave radiation for surface layer'          ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWNET_V_ISBA' ,'MEB: net vegetation canopy longwave radiation'                 ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWNET_G_ISBA' ,'MEB: net ground longwave radiation'                            ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWNET_N_ISBA' ,'MEB: net snow longwave radiation'                              ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_V_C_ISBA' ,'MEB: sensible heat flux from vegetation canopy overstory'        ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_G_C_ISBA' ,'MEB: sensible heat flux from understory'                         ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_C_A_ISBA' ,'MEB: sensible heat flux from canopy air space to the atmosphere' ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_N_C_ISBA' ,'MEB: sensible heat flux from the snow on the ground'             ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWDOWN_GN_ISBA' ,'MEB: SW reaching the snowpack/ground understory'             ,IDDIM,  &
                        YATT_TITLE,YATT)      
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWDOWN_GN_ISBA' ,'MEB: LW reaching the snowpack/ground understory'             ,IDDIM,  &
                        YATT_TITLE,YATT)      
    YATT = 'kg/m2s'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAP_N_C_ISBA','MEB: Total evap from snow on the ground to canopy air space'   ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAP_G_C_ISBA','MEB: Total evap from ground to canopy air space'               ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SR_GN_ISBA','MEB: total snow reaching the ground snow'                         ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MELTCV_ISBA','MEB: snow melt rate from the overstory snow reservoir'           ,IDDIM,  &
                        YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FRZCV_ISBA','MEB: snow refreeze rate from the overstory snow reservoir'        ,IDDIM,  &
                        YATT_TITLE,YATT)
  ENDIF
  !
  IF(I%LFLOOD) THEN
    YATT = 'kg/m2s'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IFLOOD_ISBA'  ,'Averaged_Floodplains_infiltration'                    ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PFLOOD_ISBA'  ,'Averaged_Precipitation_intercepted_by_the floodplains',JDIM,YATT_TITLE,YATT)
    YATT = 'W/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEF_ISBA'     ,'Averaged_Floodplains_evaporation_Heat_Flux'           ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEIF_ISBA'    ,'Averaged_Floodplains_Frozen_evaporation_Heat_Flux'    ,JDIM,YATT_TITLE,YATT)
  ENDIF        
  IF(I%CPHOTO/='NON')THEN
    YATT = 'kgCO2/m2/s'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GPP_ISBA'     ,'Averaged_gross_primary_production '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'R_AUTO_ISBA'  ,'Averaged_autotrophic_respiration  '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'R_ECO_ISBA'   ,'Averaged_ecosystem_respiration    '  ,JDIM,YATT_TITLE,YATT)
  ENDIF
  IF(DGEI%LWATER_BUDGET)THEN 
    YATT = 'kg/m2s'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RAINF_ISBA'   ,'Averaged_input_rainfall_rate             '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNOWF_ISBA'   ,'Averaged_input_snowfall_rate             '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWG_ISBA'     ,'Averaged_change_in_liquid_soil_moisture  '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWGI_ISBA'    ,'Averaged_change_in_solid_soil_moisture   '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWR_ISBA'     ,'Averaged_change_in_canopy_water          '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DSWE_ISBA'    ,'Averaged_change_in_snow_water_equivalent '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WATBUD_ISBA'  ,'Averaged_isba_water_budget_as_residue    '  ,JDIM,YATT_TITLE,YATT)
  ENDIF
  !
  IF(DGI%LPATCH_BUDGET) THEN      
    YATT = 'W/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEG_P'    ,'Ground_Evaporation_Heat_Flux'                        ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEGI_P'   ,'Soil_Ice_Sublimation'                                ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEV_P'    ,'Vegetation_Evaporation_Heat_Flux'                    ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LES_P'    ,'Snow_Sublimation_Heat_Flux'                          ,IDDIM,YATT_TITLE,YATT)
    IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LESL_P'   ,'Snow_Evaporation_Heat_Flux'                       ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LER_P'    ,'Canopy_Direct_Evaporation_Heat_Flux'                 ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LETR_P'   ,'Vegetation_Transpiration_Heat_Flux'                  ,IDDIM,YATT_TITLE,YATT)
    YATT = 'kg/m2s'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAP_P'   ,'Evapotranspiration'                                  ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SUBL_P'   ,'Sublimation_of_ice/snow'                             ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DRAIN_P'  ,'Soil_Drainage_Flux'                                  ,IDDIM,YATT_TITLE,YATT)
    IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNDRIF_P'   ,'blowing_snow_sublimation'                       ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    IF(I%CRUNOFF=='SGH'.AND.I%CISBA=='DIF')THEN  
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QSB_P'  ,'lateral_subsurface_flow'                             ,IDDIM,YATT_TITLE,YATT)
    ENDIF    
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RUNOFF_P' ,'Supersaturation_Runoff'                              ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HORTON_P' ,'Horton_Surface_Runoff'                               ,IDDIM,YATT_TITLE,YATT)  
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DRIVEG_P' ,'Dripping_from_the_vegetation_reservoir'              ,IDDIM,YATT_TITLE,YATT)
    !
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RRVEG_P'  ,'Precipitation_Intercepted_by_Vegetation'             ,IDDIM,YATT_TITLE,YATT)    
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNOMLT_P' ,'Snow_melt_flux'                                      ,IDDIM,YATT_TITLE,YATT)
    IF(LAGRIP) CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IRRIG_P'  ,'Irrigation_rate'                          ,IDDIM,YATT_TITLE,YATT)
    !
    IF (ISIZE_LMEB_PATCH>0) THEN
      YATT = 'W/m2'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEVCV_P'  ,'MEB: total evapotranspiration from vegetation canopy overstory' ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LESC_P'   ,'MEB: total snow sublimation from vegetation canopy overstory'   ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LETRGV_P' ,'MEB: transpiration from understory vegetation'                  ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LETRCV_P' ,'MEB: transpiration from overstory canopy vegetation'            ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LERGV_P'  ,'MEB: interception evaporation from understory vegetation'       ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LERCV_P'  ,'MEB: interception evaporation from overstory canopy vegetation' ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_V_C_P' ,'MEB: latent heat flux from vegetation canopy overstory'         ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_G_C_P' ,'MEB: latent heat flux from understory'                          ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_C_A_P' ,'MEB: latent heat flux from canopy air space to the atmosphere'  ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LE_N_C_P' ,'MEB: latent heat flux from the snow on the ground'              ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWNET_V_P' ,'MEB: net vegetation canopy shortwave radiation'                ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWNET_G_P' ,'MEB: net ground shortwave radiation'                           ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWNET_N_P' ,'MEB: net snow shortwave radiation'                             ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWNET_NS_P' ,'MEB: net snow shortwave radiation for surface layer'          ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWNET_V_P' ,'MEB: net vegetation canopy longwave radiation'                 ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWNET_G_P' ,'MEB: net ground longwave radiation'                            ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWNET_N_P' ,'MEB: net snow longwave radiation'                              ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_V_C_P' ,'MEB: sensible heat flux from vegetation canopy overstory'        ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_G_C_P' ,'MEB: sensible heat flux from understory'                         ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_C_A_P' ,'MEB: sensible heat flux from canopy air space to the atmosphere' ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_N_C_P' ,'MEB: sensible heat flux from the snow on the ground'             ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWDOWN_GN_P' ,'MEB: SW reaching the snowpack/ground understory'             ,IDDIM,  &
                          YATT_TITLE,YATT)      
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWDOWN_GN_P' ,'MEB: LW reaching the snowpack/ground understory'             ,IDDIM,  &
                          YATT_TITLE,YATT)      
      YATT = 'kg/m2s'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAP_N_C_P','MEB: Total evap from snow on the ground to canopy air space'   ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAP_G_C_P','MEB: Total evap from ground to canopy air space'               ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SR_GN_P','MEB: total snow reaching the ground snow'                         ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MELTCV_P','MEB: snow melt rate from the overstory snow reservoir'           ,IDDIM,  &
                          YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FRZCV_P','MEB: snow refreeze rate from the overstory snow reservoir'        ,IDDIM,  &
                          YATT_TITLE,YATT)
    ENDIF
    !
    IF(I%LFLOOD)THEN
      YATT = 'kg/m2s'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IFLOOD_P' ,'Floodplains_infiltration'                          ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PFLOOD_P' ,'Precipitation_intercepted_by_the_floodplains'      ,IDDIM,YATT_TITLE,YATT)
      YATT = 'W/m2'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEF_P'    ,'Floodplains_evaporation_Heat_Flux'                 ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEIF_P'   ,'Floodplains_Frozen_evaporation_Heat_Flux'          ,IDDIM,YATT_TITLE,YATT)
    ENDIF 
    IF(I%CPHOTO/='NON')THEN
      YATT = 'kgCO2/m2/s'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GPP_P'     ,'gross_primary_production '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'R_AUTO_P'  ,'autotrophic_respiration  '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'R_ECO_P'   ,'ecosystem_respiration    '  ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    IF(DGEI%LWATER_BUDGET)THEN 
      YATT = 'kg/m2s'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWG_P'     ,'change_in_liquid_soil_moisture  '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWGI_P'    ,'change_in_solid_soil_moisture   '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWR_P'     ,'change_in_water_on_canopy       '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DSWE_P'    ,'change_in_snow_water_equivalent '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WATBUD_P'  ,'isba_water_budget_as_residue    '  ,IDDIM,YATT_TITLE,YATT)
    ENDIF    
    !
  ENDIF
  !
ENDIF
!
IF (DGMI%LSURF_MISC_BUDGET) THEN
  !
  IL=INLVLD
  !
  DO JLAYER=1,IL
     WRITE(YPAS,'(I3)') JLAYER
     YLVL = ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SWI'//YLVL(:LEN_TRIM(YLVL))//'_ISBA',  &
        'Soil_Wetness_Index'//YLVL       , JDIM, YATT_TITLE, (/'-'/))  
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TSWI'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', &
        'Total_SWI_(liquid+solid)'//YLVL , JDIM, YATT_TITLE, (/'-'/))  
     IF(DGI%LPATCH_BUDGET)THEN      
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SWI'//YLVL,  'Soil_Wetness_Index'//YLVL      , IDDIM, YATT_TITLE, (/'-'/))
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TSWI'//YLVL, 'Total_SWI_(liquid+solid)'//YLVL, IDDIM, YATT_TITLE, (/'-'/))
     ENDIF
  ENDDO
  !  
  YATT = '-'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWI_T_ISBA'     ,'SWI_over_entire_soil '                       ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSWI_T_ISBA'    ,'Total_SWI_over_entire_soil '                  ,JDIM,YATT_TITLE,YATT)
  IF(I%CISBA=='DIF'.AND.DGMI%LSURF_MISC_DIF)THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSWI_D2_ISBA'   ,'Total_SWI_over_comparable_FR-DG2_reservoir',JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSWI_D3_ISBA'   ,'Total_SWI_over_comparable_FR-DG3_reservoir',JDIM,YATT_TITLE,YATT)
  ENDIF  
  !
  YATT = 'kg/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WGTOT_T_ISBA'  , 'Total_soil_water_reservoir_(liquid+solid)'  ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WGI_T_ISBA'    , 'Total_soil_ice_reservoir'                   ,JDIM,YATT_TITLE,YATT)
  YATT = 'm3/m3'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WGTOT_ISBA'  , 'Total_volumetric_soil_water_content_(liquid+solid)'  ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WGI_ISBA'    , 'Total_volumetric_soil_ice_content'                   ,JDIM,YATT_TITLE,YATT)  
  IF(I%CISBA=='DIF'.AND.DGMI%LSURF_MISC_DIF)THEN
    YATT = 'm3/m3'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WG_D2_ISBA'  ,'soil_liquid_water_over_comparable_FR-DG2_reservoir',JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WGI_D2_ISBA' ,'soil_ice_over_comparable_FR-DG2_reservoir         ',JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WG_D3_ISBA'  ,'soil_liquid_water_comparable_FR-DG3_reservoir     ',JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WGI_D3_ISBA' ,'soil_ice_over_comparable_FR-DG3_reservoir         ',JDIM,YATT_TITLE,YATT)
  ENDIF  
  IF(I%CISBA=='DIF')THEN
    YATT = 'm'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALT_ISBA'    ,'permafrost_active_layer_thickness'                          ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FLT_ISBA'    ,'non-permafrost_frozen_layer_thickness'                      ,JDIM,YATT_TITLE,YATT)
  ENDIF
  !
  IF(I%LGW)THEN
    YATT = '-'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FWTD_ISBA'   ,'grid-cell_fraction_of_water_table_to_rise'              ,JDIM,YATT_TITLE,YATT)
    YATT = 'm'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WTD_ISBA'    ,'water_table_depth_from_RRM_model_or_observation'        ,JDIM,YATT_TITLE,YATT)
  ENDIF  
  !
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
    YATT = 'K'          
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TS_ISBA'    ,'Surface_Temperature_(isba+snow3l)    ' ,JDIM, YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSRAD_ISBA' ,'Surface_Radiative_Temperature_(isba+snow3l)    ' ,JDIM, YATT_TITLE,YATT)
    IF (DGI%LPATCH_BUDGET) THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TS_P' ,'Surface_Temperature_(isba+snow3l)'   ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSRAD_P' ,'total_radiative_surface_Temperature_(isba+snow3l)',IDDIM,YATT_TITLE,YATT)
    ENDIF
  ENDIF
  !
  DO JLAYER=1,INLVLS
    WRITE(YPAS,'(I3)') JLAYER
    YLVL = ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN  
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SNOWTEMP'//YLVL,  'Snow_Temp_layer'//YLVL         , IDDIM, YATT_TITLE, (/'K'/))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SNOWLIQ'//YLVL,   'Snow_liquid_water_layer_'//YLVL, IDDIM, YATT_TITLE, (/'m'/))
    ENDIF
  ENDDO
  ! 
  IF(I%CRAIN=='SGH ')THEN
    YATT = '-'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'MUF_ISBA'    , 'Fraction_of_rainfall_reaching_the ground_(SGH)',JDIM,YATT_TITLE,YATT)  
  ENDIF  
  !
  YATT = '-'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PSNG_ISBA'      , 'Snow_frac_over_ground      '               ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PSNV_ISBA'      , 'Snow_frac_over_veg         '               ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PSN_ISBA '      , 'Snow_fraction              '               ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TALB_ISBA'      , 'Surface total albedo       '               ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HV_ISBA'        , 'Halstead_coefficient       '               ,JDIM,YATT_TITLE,YATT)
  IF(I%CPHOTO/='NON')THEN
    YATT = 'kg/kg'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LAI_ISBA'     ,'leaf_area_index             '               ,JDIM,YATT_TITLE,YATT) 
  ENDIF 
  YATT = 'kg/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WSN_T_ISBA'  , 'Total_snow_reservoir '                        ,JDIM,YATT_TITLE,YATT)
  YATT = 'm'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DSN_T_ISBA'  , 'Total_snow_depth '                            ,JDIM,YATT_TITLE,YATT)
  YATT = 'K'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSN_T_ISBA'  , 'Total_snow_temperature '                      ,JDIM,YATT_TITLE,YATT)  
  !
  IF(I%CRUNOFF=='SGH '.OR.I%CRUNOFF=='DT92')THEN
    YATT = '-'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FSAT_ISBA'      ,'Soil_saturated_grid-cell_fraction' ,JDIM,YATT_TITLE,YATT)
  ENDIF   
  !
  IF(I%LFLOOD)THEN
    YATT = '-'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FFG_ISBA'     ,'flood_frac_over_ground      '            ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FFV_ISBA'     ,'flood_frac_over_veg         '            ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FF_ISBA '     ,'flood_fraction              '            ,JDIM,YATT_TITLE,YATT)
    YATT = '-'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FFLOOD_ISBA'  ,'Potential_floodplain_grid-cell_fraction' ,JDIM,YATT_TITLE,YATT)
    YATT (1)='kg/m2/s'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PIFLOOD_ISBA' ,'Potential_floodplain_infiltration',JDIM,YATT_TITLE,YATT)     
  ENDIF
  ! 
  IF(DGI%LPATCH_BUDGET)THEN
    ! 
    YATT (1)='-'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PSNG_P' ,'snow_fraction_per_patch_over_ground'    ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PSNV_P' ,'snow_fraction_per_patch_over_vegetation',IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PSN_P'  ,'total_snow_fraction_per_patch'          ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TALB_P' ,'total_albedo_per_patch'                 ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HV_P'   ,'Halstead_coefficient_per_patch'         ,IDDIM,YATT_TITLE,YATT)
    YATT      (1)='kg/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WSN_T_P','Total_snow_reservoir_per_patch '        ,IDDIM,YATT_TITLE,YATT)
    YATT      (1)='m'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DSN_T_P','Total_snow_depth_per_patch '            ,IDDIM,YATT_TITLE,YATT)
    YATT      (1)='K'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TSN_T_P','Total_snow_temperature_per_patch '      ,IDDIM,YATT_TITLE,YATT)
   ! 
    IF(I%CRUNOFF=='SGH '.OR.I%CRUNOFF=='DT92')THEN
      YATT(1)='-'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FSAT_P','Soil_saturated_fraction_per_patch',IDDIM,YATT_TITLE,YATT)
    ENDIF
    !
    IF(I%CISBA=='DIF')THEN
      YATT = 'm'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALT_P' ,'permafrost_active_layer_thickness_per_patch    ',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FLT_P' ,'non-permafrost_frozen_layer_thickness_per_patch',IDDIM,YATT_TITLE,YATT)
    ENDIF
    !
    IF(I%LFLOOD)THEN
      YATT(1)='-'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FFG_P','flood_frac_per_patch_over_ground',IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FFV_P','flood_frac_per_patch_over_veg'   ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FF_P' ,'total_flood_fraction_per_patch'  ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    ! 
    IF (I%LTR_ML) THEN
      YATT (1)='(-)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FAPAR'    ,'Fapar of vegetation',IDDIM,YATT_TITLE,YATT)
      YATT (1)='(-)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FAPIR'    ,'Fapir of vegetation',IDDIM,YATT_TITLE,YATT)
      YATT (1)='(-)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DFAPARC'    ,'Fapar of vegetation (daily cumul)',IDDIM,YATT_TITLE,YATT)
      YATT (1)='(-)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DFAPIRC'    ,'Fapir of vegetation (daily cumul)',IDDIM,YATT_TITLE,YATT)      
      YATT (1)='(-)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FAPAR_BS' ,'Fapar of bare soil',IDDIM,YATT_TITLE,YATT)
      YATT (1)='(-)'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FAPIR_BS' ,'Fapir of bare soil',IDDIM,YATT_TITLE,YATT)      
      YATT (1)='m2/m2'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DLAI_EFFC'  ,'Effective LAI (daily cumul)',IDDIM,YATT_TITLE,YATT)
    ENDIF
    !
  ENDIF
  !  
ENDIF
!
IF (CHI%SVI%NBEQ>0 .AND. CHI%CCH_DRY_DEP=="WES89 ") THEN
  !
  YATT="(m/s)"
  !
  DO JSV = 1,SIZE(CHI%CCH_NAMES,1)
    !
    YRECFM = 'DV_NAT_'//TRIM(CHI%CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A7,I3.3)')'DV_NAT_',JSV
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,YCOMMENT,IDDIM,YATT_TITLE,YATT)      
    !
  ENDDO
  !
END IF
!
IF (CHI%SVI%NBEQ>0 .AND. CHI%LCH_BIO_FLUX) THEN
  !
  IF (ASSOCIATED(GB%XFISO)) THEN
    YRECFM='FISO'
    WRITE(YCOMMENT,'(A21)')'FISO (molecules/m2/s)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,YCOMMENT,JDIM,YATT_TITLE,YATT)  
  END IF
  !
  IF (ASSOCIATED(GB%XFISO)) THEN
    YRECFM='FMONO'
    WRITE(YCOMMENT,'(A22)')'FMONO (molecules/m2/s)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,YCOMMENT,JDIM,YATT_TITLE,YATT)  
  END IF
  !
ENDIF
!
IF (CHI%LCH_NO_FLUX) THEN
  !
  IF (ASSOCIATED(GB%XNOFLUX)) THEN
    YRECFM='NOFLUX'
    WRITE(YCOMMENT,'(A21)')'NOFLUX (molecules/m2/s)'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,YRECFM,YCOMMENT,JDIM,YATT_TITLE,YATT)     
  END IF
  !
END IF
!
IF(DGU%LPROVAR_TO_DIAG)THEN
  !
  IF(I%LTEMP_ARP)THEN
    IL=I%NTEMPLAYER_ARP
  ELSEIF(I%CISBA=='DIF')THEN
     IL=INLVLD    
  ELSE
    IL=2
  ENDIF
  !
  YATT = 'K'
  DO JLAYER=1,IL
     WRITE(YPAS,'(I3)') JLAYER
     YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'TG'//YLVL(:LEN_TRIM(YLVL))//'_ISBA','Soil_temp_layer_'//YLVL,JDIM,YATT_TITLE,YATT)
  ENDDO
  !
  IL=INLVLD
  !
  YATT = 'm3/m3'
  DO JLAYER=1,IL
     WRITE(YPAS,'(I3)') JLAYER
     YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WG'//YLVL(:LEN_TRIM(YLVL))//'_ISBA','Soil_liquid_layer_'//YLVL,JDIM,YATT_TITLE,YATT)
  ENDDO
  YATT = 'kg/m2'
  DO JLAYER=1,IL
     WRITE(YPAS,'(I3)') JLAYER
     YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SOILM'//YLVL(:LEN_TRIM(YLVL))//'_ISBA','Soil_moisture_(liquid)_layer_'//YLVL, &
                                                                                                  JDIM,YATT_TITLE,YATT)
  ENDDO  
  !  
  IF(I%CISBA/='DIF')THEN
    IL=2
  ENDIF
  !
  YATT = 'm3/m3'
  DO JLAYER=1,IL
    WRITE(YPAS,'(I3)') JLAYER
    YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WGI'//YLVL(:LEN_TRIM(YLVL))//'_ISBA','Soil_ice_layer_'//YLVL,JDIM,YATT_TITLE,YATT)
  ENDDO
  YATT = 'kg/m2'
  DO JLAYER=1,IL
     WRITE(YPAS,'(I3)') JLAYER
     YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SOILI'//YLVL(:LEN_TRIM(YLVL))//'_ISBA','Soil_ice_mass_layer_'//YLVL,JDIM,YATT_TITLE,YATT)
  ENDDO  
  !
  YATT = 'kg/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WR_ISBA','Interception_reservoir',JDIM,YATT_TITLE,YATT) 
  !  
  IF(I%LGLACIER)THEN
     YATT = 'kg/m2'
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ICE_STO_ISBA','Glacier_reservoir',JDIM,YATT_TITLE,YATT)
  ENDIF
  !
  YATT='-'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ASN_ISBA','Snow_Albedo',JDIM,YATT_TITLE,YATT)
  !
  IF(I%TSNOW%SCHEME=='3-L'  .OR. I%TSNOW%SCHEME=='CRO')THEN
    DO JLAYER=1,INLVLS   
       WRITE(YPAS,'(I3)') JLAYER
       YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
       YATT = 'kg/m2'
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'WSN_'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', &
                             'Snow_Water_Equivalent_layer_'//YLVL, JDIM, YATT_TITLE, YATT)  
       YATT = 'm'                  
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'DSN_'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', &
                             'Snow_Depth_layer_'//YLVL           , JDIM, YATT_TITLE, YATT)  
        YATT = 'K'                        
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'TSN_'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', &
                             'Snow_Temperature_layer_'//YLVL     , JDIM, YATT_TITLE, YATT)
        YATT = 'day_since_snowfall'                        
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'AGSN_'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', &
                             'Snow_age_layer_'//YLVL     , JDIM, YATT_TITLE, YATT)                             
    ENDDO 
  ENDIF
  !
  IF(I%CPHOTO=='NIT'.OR.I%CPHOTO=='NCB')THEN
    YATT = 'kgDM/m2'
    DO JNBIOMASS=1,INBIOMASS
       WRITE(YPAS,'(I3)') JNBIOMASS
       YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'BIOM'//YLVL(:LEN_TRIM(YLVL))//'_ISBA','Biomass_reservoir_'//YLVL,JDIM,YATT_TITLE,YATT)
    ENDDO
  ENDIF
  !
  IF(I%CRESPSL=='CNT')THEN
    YATT = 'gC/m2'
    DO JNLITTER=1,INLITTER
      DO JNLITTLEVS=1,INLITTLEVS
        WRITE(YPAS,'(I1,A1,I1)') JNLITTER,'_',JNLITTLEVS
        YLVL = ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'LIT'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', 'Litter_pool'//YLVL,JDIM,YATT_TITLE,YATT)
      END DO
    END DO  
    DO JNSOILCARB=1,INSOILCARB
      WRITE(YPAS,'(I3)') JNSOILCARB
      YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'SCARB'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', 'Soil_carbon_pool'//YLVL,JDIM,YATT_TITLE,YATT)
    END DO
    YATT = '-'
    DO JNLITTLEVS=1,INLITTLEVS
      WRITE(YPAS,'(I3)') JNLITTLEVS
      YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID, 'LIGSTR'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', 'Ratio_Lignin/Carbon_in_structural_litter'//YLVL, &
                          JDIM,YATT_TITLE,YATT)
    END DO          
  ENDIF
ENDIF    
!
 CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
!
IF (DGEI%LSURF_BUDGETC) THEN
  !
  YFILE='ISBA_DIAG_CUMUL.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
  JRET=NF_REDEF(IFILE_ID)
  YATT      (1)='dimensionless'
  !
  IF(DGI%LPATCH_BUDGET)THEN      
    YATT='J/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RNC_P'    ,'Cumulated_Net_Radiation'                            ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HC_P'     ,'Cumulated_Sensible_Heat_Flux'                       ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEC_P'    ,'Cumulated_Total_Latent_Heat_Flux'                   ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEIC_P'   ,'Cumulated_Sublimation_Latent_Heat_Flux'             ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLUXC_P' ,'Cumulated_Ground_Heat_Flux'                         ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEGC_P'   ,'Cumulated_Ground_Evaporation_Heat_Flux'             ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEGIC_P'  ,'Cumulated_Soil_Ice_Sublimation'                     ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEVC_P'   ,'Cumulated_Vegetation_Evaporation_Heat_Flux'         ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LESC_P'   ,'Cumulated_Snow_Sublimation_Heat_Flux'               ,IDDIM,YATT_TITLE,YATT)
    IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LESLC_P'  ,'Cumulated_Snow_Evaporation_Heat_Flux'            ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LERC_P'   ,'Cumulated_Canopy_Direct_Evaporation_Heat_Flux'      ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LETRC_P'  ,'Cumulated_Vegetation_Transpiration_Heat_Flux'       ,IDDIM,YATT_TITLE,YATT)
    IF(DGI%LRAD_BUDGET)THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWDC_P' ,'Cumulated_Downward_SW       '                       ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWUC_P' ,'Cumulated_Upward_SW         '                       ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWDC_P' ,'Cumulated_Downward_LW       '                       ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWUC_P' ,'Cumulated_Upward_LW         '                       ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    !
    YATT='Pa.s'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMUC_P'   ,'Cumulated_Zonal_Wind_Stress '                       ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMVC_P'   ,'Cumulated_Merid_Wind_Stress '                       ,IDDIM,YATT_TITLE,YATT)  
    YATT='kg/m2'  
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAPC_P'  ,'Cumulated_Evapotranspiration'                       ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SUBLC_P'  ,'Cumulated_Sublimation_of_ice/snow'                  ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DRAINC_P' ,'Cumulated_Soil_Drainage_Flux'                       ,IDDIM,YATT_TITLE,YATT)
    IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
       CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNDRIFC_P'  ,'Cumulated_blowing_snow_sublimation'            ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    IF(I%CRUNOFF=='SGH'.AND.I%CISBA=='DIF')THEN  
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QSBC_P' ,'Cumulated_lateral_subsurface_flow'                  ,IDDIM,YATT_TITLE,YATT)
    ENDIF    
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RUNOFFC_P','Cumulated_Supersaturation_Runoff'                   ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HORTONC_P','Cumulated_Horton_Runoff'                            ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DRIVEGC_P','Cumulated_Dripping_from_the_vegetation_reservoir'   ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNOMLTC_P','Cumulated_Snow_melt_flux'                           ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RRVEGC_P' ,'Cumulated_Precipitation_Intercepted_by_Vegetation'  ,IDDIM,YATT_TITLE,YATT)
    IF(LAGRIP) CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IRRIGC_P' ,'Cumulated_irrigation_rate'               ,IDDIM,YATT_TITLE,YATT)
    !
    IF(I%LGLACIER) THEN
      YATT='kg/m2'  
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ICE_FC_P' ,'Cumulated_Glacier_ice_flux'                         ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    IF(I%LFLOOD) THEN
      YATT='kg/m2'  
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IFLOODC_P','Cumulated_Floodplains_infiltration'                    ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PFLOODC_P','Cumulated_Precipitation_intercepted_by_the_floodplains',IDDIM,YATT_TITLE,YATT)
      YATT='J/m2'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEFC_P'   ,'Cumulated_Floodplains_evaporation_Heat_Flux'        ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEIFC_P'  ,'Cumulated_Floodplains_Frozen_evaporation_Heat_Flux' ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    IF(I%CPHOTO/='NON')THEN
      YATT = 'kgCO2/m2'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GPPC_P'     ,'Cumulated_gross_primary_production '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RC_AUTO_P'   ,'Cumulated_autotrophic_respiration  '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RC_ECO_P'    ,'Cumulated_ecosystem_respiration    '  ,IDDIM,YATT_TITLE,YATT)
    ENDIF
    IF(DGEI%LWATER_BUDGET)THEN 
      YATT = 'kg/m2'
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWGC_P'     ,'Cumulated_change_in_liquid_soil_moisture  '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWGIC_P'    ,'Cumulated_change_in_solid_soil_moisture   '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWRC_P'     ,'Cumulated_change_in_canopy_water          '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DSWEC_P'    ,'Cumulated_change_in_snow_water_equivalent '  ,IDDIM,YATT_TITLE,YATT)
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WATBUDC_P'  ,'Cumulated_isba_water_budget_as_residue    '  ,IDDIM,YATT_TITLE,YATT)
    ENDIF
  ENDIF
  !  
  YATT='J/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEGC_ISBA'   ,'Averaged_Cumulated_Ground_Evaporation_Heat_Flux'     ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEGIC_ISBA'  ,'Averaged_Cumulated_Soil_Ice_Sublimation'             ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEVC_ISBA'   ,'Averaged_Cumulated_Vegetation_Evaporation_Heat_Flux' ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LESC_ISBA'   ,'Averaged_Cumulated_Snow_Sublimation_Heat_Flux'       ,JDIM,YATT_TITLE,YATT)
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LESLC_ISBA'  ,'Averaged_Cumulated_Snow_Evaporation_Heat_Flux'    ,JDIM,YATT_TITLE,YATT)
  ENDIF
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LERC_ISBA'   ,'Averaged_Cumulated_Canopy_Direct_Evaporation_Heat_Flux',JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LETRC_ISBA'  ,'Averaged_Cumulated_Vegetation_Transpiration_Heat_Flux',JDIM,YATT_TITLE,YATT)
  YATT='kg/m2'  
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EVAPC_ISBA'  ,'Averaged_Cumulated_Evapotranspiration'               ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SUBLC_ISBA'  ,'Averaged_Cumulated_Sublimation_of_ice/snow'          ,JDIM,YATT_TITLE,YATT)
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNDRIFC_ISBA'  ,'Averaged_Cumulated_blowing_snow_sublimation'    ,JDIM,YATT_TITLE,YATT)
  ENDIF
  IF(I%CRUNOFF=='SGH'.AND.I%CISBA=='DIF')THEN  
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'QSBC_ISBA' ,'Averaged_Cumulated_lateral_subsurface_flow'          ,JDIM,YATT_TITLE,YATT)
  ENDIF
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DRAINC_ISBA' ,'Averaged_Cumulated_Soil_Drainage_Flux'               ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RUNOFFC_ISBA','Averaged_Cumulated_Supersaturation_Runoff'           ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HORTONC_ISBA','Averaged_Cumulated_Horton_Surface_Runoff'            ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DRIVEGC_ISBA','Averaged_Dripping_from_the_vegetation_reservoir'     ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNOMLTC_ISBA','Averaged_Cumulated_Snow_melt_flux'                   ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RRVEGC_ISBA' ,'Averaged_Cumulated_Precipitation_Intercepted_by_Vegetation',&
         JDIM,YATT_TITLE,YATT)     
  IF(LAGRIP) CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IRRIGC_ISBA' ,'Averaged_Cumulated_irrigation_rate'       ,JDIM,YATT_TITLE,YATT)     
  !
  IF(I%LGLACIER)THEN
  YATT='kg/m2'  
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ICE_FC_ISBA' ,'Averaged_Cumulated_Glacier_ice_flux'                      ,JDIM,YATT_TITLE,YATT)
  ENDIF
  IF(I%LFLOOD)THEN
  YATT='kg/m2'  
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IFLOODC_ISBA','Averaged_Cumulated_Floodplains_infiltration'              ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PFLOODC_ISBA','Averaged_Cumulated_Precip_intercepted_by_the_floodplains' ,JDIM,YATT_TITLE,YATT)
  YATT='J/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEFC_ISBA'   ,'Averaged_Cumulated_Flood_evaporation_Heat_Flux'           ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEIFC_ISBA'  ,'Averaged_Cumulated_Flood_Frozen_evaporation_Heat_Flux'    ,JDIM,YATT_TITLE,YATT)
  ENDIF 
  IF(I%CPHOTO/='NON')THEN
    YATT = 'kgCO2/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GPPC_ISBA'     ,'Averaged_Cumulated_gross_primary_production '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RC_AUTO_ISBA'  ,'Averaged_Cumulated_autotrophic_respiration  '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RC_ECO_ISBA'   ,'Averaged_Cumulated_ecosystem_respiration    '  ,JDIM,YATT_TITLE,YATT)
  ENDIF
  IF(DGEI%LWATER_BUDGET)THEN 
    YATT = 'kg/m2'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RAINFC_ISBA'   ,'Averaged_Cumulated_input_rainfall_rate             '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SNOWFC_ISBA'   ,'Averaged_Cumulated_input_snowfall_rate             '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWGC_ISBA'     ,'Averaged_Cumulated_change_in_liquid_soil_moisture  '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWGIC_ISBA'    ,'Averaged_Cumulated_change_in_solid_soil_moisture   '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DWRC_ISBA'     ,'Averaged_Cumulated_change_in_canopy_water          '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DSWEC_ISBA'    ,'Averaged_Cumulated_change_in_snow_water_equivalent '  ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WATBUDC_ISBA'  ,'Averaged_Cumulated_isba_water_budget_as_residue    '  ,JDIM,YATT_TITLE,YATT)
  ENDIF
  !
  YATT='J/m2'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RNC_ISBA'    ,'Averaged_Cumulated_Net_Radiation'                         ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'HC_ISBA'     ,'Averaged_Cumulated_Sensible_Heat_Flux'                    ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEC_ISBA'    ,'Averaged_Cumulated_Total_Latent_Heat_Flux'                ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LEIC_ISBA'   ,'Averaged_Cumulated_Sublimation_Latent_Heat_Flux'          ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GFLUXC_ISBA' ,'Averaged_Cumulated_Ground_Heat_Flux'                      ,JDIM,YATT_TITLE,YATT)
  IF(DGI%LRAD_BUDGET)THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWDC_ISBA' ,'Averaged_Cumulated_Downward_SW       '                    ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'SWUC_ISBA' ,'Averaged_Cumulated_Upward_SW         '                    ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWDC_ISBA' ,'Averaged_Cumulated_Downward_LW       '                    ,JDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LWUC_ISBA' ,'Averaged_Cumulated_Upward_LW         '                    ,JDIM,YATT_TITLE,YATT)
  ENDIF
  YATT='Pa.s'
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMUC_ISBA'   ,'Averaged_Cumulated_Zonal_Wind_Stress '                ,JDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FMVC_ISBA'   ,'Averaged_Cumulated_Merid_Wind_Stress '                ,JDIM,YATT_TITLE,YATT)
  !  
  CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
  !
ENDIF


! 6. Create file for vegetation parameter values
!----------------------------------------------------------

IF(LASSIM) THEN
  IF(CASSIM=='PLUS ') THEN
    YFILE='ISBA_VEG_EVOLUTION_P.OUT.nc'
    CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
    JRET=NF_REDEF(IFILE_ID)
    YATT='dimensionless'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LAIp'   ,'Output_LAI_ISBA' ,IDDIM,YATT_TITLE,YATT)
    CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
  ELSEIF(CASSIM=='AVERA') THEN
    YFILE='ISBA_VEG_EVOLUTION_A.OUT.nc'
    CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
    JRET=NF_REDEF(IFILE_ID)
    YATT ='dimensionless'     
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LAIa'   ,'Output_LAI_ISBA' ,IDDIM,YATT_TITLE,YATT)
    CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
  ELSEIF(CASSIM=='2DVAR') THEN
    YFILE='ISBA_VEG_EVOLUTION.OUT.nc'
    CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)
    JRET=NF_REDEF(IFILE_ID)
    YATT='dimensionless'
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LAI'   ,'Output_LAI_ISBA' ,IDDIM,YATT_TITLE,YATT)
    CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
  ENDIF
ELSEIF(DGI%LPGD)THEN
  YFILE='ISBA_VEG_EVOLUTION.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIM)  
  JRET=NF_REDEF(IFILE_ID)
  YATT ='dimensionless'
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'VEG'         ,'Output_vegetation_fraction'         ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LAI'         ,'Output_LAI_per_patch'               ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0VEG'       ,'Roughness_Length_Vegetation'        ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'PATCH'       ,'Fraction_Of_Patch'                  ,IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0REL'       ,'orography_roughness_length',IDDIM(1:1),YATT_TITLE,(/'m'/))
  !
  IF (ISIZE_LMEB_PATCH>0) THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'VEGGV'         ,'MEB: Output_understory_vegetation_fraction'         ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'LAIGV'         ,'MEB: Output_understory_LAI_per_patch'               ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'Z0VEGGV'       ,'MEB: Understory_Roughness_Length_Vegetation'        ,IDDIM,YATT_TITLE,YATT)
  ENDIF
  !
  DO JLAYER=1,INLVLD
    WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DG'//YLVL   ,'soil_depth_layer_'//YLVL ,IDDIM(1:INDIMS-1),YATT_TITLE,(/'m'/))
    IF(INPATCH>1)THEN
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DG'//YLVL(:LEN_TRIM(YLVL))//'_ISBA', &
                          'averaged_soil_depth_layer_'//YLVL,IDDIM(1:1),YATT_TITLE,(/'m'/))
    ENDIF
  ENDDO
  !
  DO JLAYER=1,INLVLD
    WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WSAT'//YLVL ,'soil_porosity_layer_'//YLVL,IDDIM(1:1),YATT_TITLE,(/'m3/m3'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WFC'//YLVL  ,'field_capacity_layer_'//YLVL,IDDIM(1:1),YATT_TITLE,(/'m3/m3'/)) 
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WWILT'//YLVL,'wilting_point_layer_'//YLVL,IDDIM(1:1),YATT_TITLE,(/'m3/m3'/))
  ENDDO
  !
  IF(I%CISBA=='DIF')THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DROOT_DIF' ,'Root_depth_in_ISBA-DIF'                   ,IDDIM(1:INDIMS-1),YATT_TITLE,(/'m'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DG2_DIF'   ,'DG2_depth_in_ISBA-DIF'                    ,IDDIM(1:INDIMS-1),YATT_TITLE,(/'m'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RUNOFFD'   ,'Runoff_depth_in_ISBA-DIF'                 ,IDDIM(1:INDIMS-1),YATT_TITLE,(/'m'/))
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DTOT_DIF'  ,'Total_soil_depth_for_moisture_in_ISBA-DIF',IDDIM(1:INDIMS-1),YATT_TITLE,(/'m'/))
    IF(INPATCH>1)THEN
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DG2_DIF_ISBA','averaged_DG2_depth_in_ISBA-DIF'                    ,IDDIM(1:1),YATT_TITLE,(/'m'/))
     CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DTOTDF_ISBA' ,'averaged_Total_soil_depth_for_moisture_in_ISBA-DIF',IDDIM(1:1),YATT_TITLE,(/'m'/))
    ENDIF
    DO JLAYER=1,INLVLD
      WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ROOTFRAC'//YLVL,'root_fraction_layer_'//YLVL ,IDDIM(1:INDIMS-1),YATT_TITLE,(/'-'/))           
    ENDDO
    IF (ISIZE_LMEB_PATCH>0) THEN
      DO JLAYER=1,INLVLD
        WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
        CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ROOTFRACGV'//YLVL,'MEB: understory_root_fraction_layer_'//YLVL ,&
                            IDDIM(1:INDIMS-1),YATT_TITLE,(/'-'/))           
      ENDDO
    ENDIF
    IF(I%LSOC)THEN
      DO JLAYER=1,INLVLD
         WRITE(YPAS,'(I3)') JLAYER ; YLVL=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
         CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'FRACSOC'//YLVL,'SOC_fraction_layer_'//YLVL,IDDIM(1:2),YATT_TITLE,(/'-'/))
      ENDDO
    ENDIF
  ENDIF
  !
  IF(I%CHORT=='SGH')THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'DICE','soil_ice_depth_for_runoff',IDDIM(1:INDIMS-1),YATT_TITLE,(/'m'/))
  ENDIF   
  !    
  DO JVEG=1,NVEGTYPE
     WRITE(YPAS,'(i2)') JVEG 
     YLVLV=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
     CALL DEF_VAR_NETCDF(DGU,&
             IFILE_ID,'VEGTYPE'//YLVLV,'fraction_of_vegtype_in_the_grid_cell',IDDIM(1:1),YATT_TITLE,(/'-'/))
  ENDDO
  !    
  IF(INPATCH>1.AND.NVEGTYPE/=INPATCH)THEN
    DO JVEG=1,NVEGTYPE
      WRITE(YPAS,'(i2)') JVEG 
      YLVLV=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
      CALL DEF_VAR_NETCDF(DGU,&
               IFILE_ID,'VEGTY_P'//YLVLV,'fraction_of_vegtype_in_each_patch'//YLVLV,IDDIM(1:INDIMS-1),YATT_TITLE,(/'-'/))
      ENDDO
  ENDIF
  !
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'EMIS_ISBA'   ,'Emissivity_Of_Vegetation'           ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RSMIN'       ,'Minimal_Stomatal_Resistance'        ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GAMMA'       ,'Coefficient_Computation_Rsmin'      ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'CV'          ,'Vegetal_Thermal_Inertia'            ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RGL'         ,'Max_Solar_Radiation_Photosynthesis' ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WRMAX_CF'    ,'Coefficient_Max_Water_Interception' ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALBNIR_SOIL' ,'Output_ALBNIR_SOIL'                 ,IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALBVIS_SOIL' ,'Output_ALBVIS_SOIL'                 ,IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALBUV_SOIL'  ,'soil_UV_albedo'                     ,IDDIM(1:INDIMS-1),YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALBNIR_ISBA' ,'total_near-infra-red albedo'        ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALBVIS_ISBA' ,'total_visible_albedo'               ,IDDIM,YATT_TITLE,YATT)
  CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ALBUV_ISBA'  ,'total_UV_albedo'                    ,IDDIM,YATT_TITLE,YATT)
  !
  IF (ISIZE_LMEB_PATCH>0) THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RSMINGV'       ,'MEB: Understory_Minimal_Stomatal_Resistance'        ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'GAMMAGV'       ,'MEB: Understory_Coefficient_Computation_Rsmin'      ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'RGLGV'         ,'MEB: Understory_Max_Solar_Radiation_Photosynthesis' ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WRMAX_CFGV'    ,'MEB: Understory_Coefficient_Max_Water_Interception' ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'ZF_TALLVEG'    ,'MEB: identification_variable_for tall_vegetation'   ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'H_VEG'         ,'MEB: height_of_vegetation'                          ,IDDIM,YATT_TITLE,YATT)
  ENDIF
  !  
  IF (LAGRIP .AND. (I%CPHOTO=='NIT' .OR. I%CPHOTO=='LAI' .OR. I%CPHOTO=='LST' .OR. I%CPHOTO=='NCB') ) THEN
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'WATSUP' ,'Water_Supply_Irrigation' ,IDDIM,YATT_TITLE,YATT)
    CALL DEF_VAR_NETCDF(DGU, &
                      IFILE_ID,'IRRIG'  ,'Fraction_Of_Irrigated_Vegetation' ,IDDIM,YATT_TITLE,YATT)
    !CALL DEF_VAR_NETCDF(IFILE_ID,'SEED'  ,'Seeding_Date' ,IDDIM,YATT_TITLE,YATT)
    !CALL DEF_VAR_NETCDF(IFILE_ID,'REAP'  ,'Reaping_Date' ,IDDIM,YATT_TITLE,YATT)
  END IF
  !
  CALL OL_WRITE_COORD(DGU, &
                     YFILE,IFILE_ID,IDDIM,YATT_TITLE,YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY)
  !
ENDIF
!
DEALLOCATE(JDIM)
!
IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_ISBA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_OUTFN_ISBA_n
