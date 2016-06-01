!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_WRITESURF_ISBA_n 
CONTAINS
!     #########
      SUBROUTINE WRITESURF_ISBA_n (DGU, U, &
                                    CHI, DST, I, &
                                   HPROGRAM,OLAND_USE)
!     #####################################
!
!!****  *WRITESURF_ISBA_n* - writes ISBA prognostic fields
!!                        
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      P. LeMoigne 12/2004 : correct dimensionning if more than 10 layers in
!!                            the soil (diffusion version)
!!      B. Decharme  2008    : Floodplains
!!      B. Decharme  01/2009 : Optional Arpege deep soil temperature write
!!      A.L. Gibelin   03/09 : modifications for CENTURY model 
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays 
!!      A.L. Gibelin 06/2009 : Soil carbon variables for CNT option
!!      B. Decharme  07/2011 : land_use semi-prognostic variables
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
!!      B. Decharme  09/2012 : write some key for prep_read_external
!!      B. Decharme  04/2013 : Only 2 temperature layer in ISBA-FR
!!      P. Samuelsson 10/2014: MEB
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
!
USE MODD_ASSIM, ONLY : LASSIM, CASSIM, CASSIM_ISBA, NIE, NENS, &
                       XADDTIMECORR, LENS_GEN, NVAR
!
USE MODD_DST_SURF
!
USE MODI_WRITE_SURF
USE MODI_WRITESURF_GR_SNOW
!
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
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
LOGICAL,           INTENT(IN)  :: OLAND_USE !
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=4 ) :: YLVL
 CHARACTER(LEN=3 ) :: YVAR
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=25) :: YFORM          ! Writing format
!
INTEGER :: JJ, JLAYER, JP, JNBIOMASS, JNLITTER, JNSOILCARB, JNLITTLEVS  ! loop counter on levels
INTEGER :: IWORK   ! Work integer
INTEGER :: JSV
INTEGER :: ISIZE_LMEB_PATCH
INTEGER :: JVAR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*       2.     Prognostic fields:
!               -----------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_ISBA_N',0,ZHOOK_HANDLE)
!* soil temperatures
!
IF(I%LTEMP_ARP)THEN
  IWORK=I%NTEMPLAYER_ARP
ELSEIF(I%CISBA=='DIF')THEN
  IWORK=I%NGROUND_LAYER
ELSE
  IWORK=2 !Only 2 temperature layer in ISBA-FR
ENDIF
!
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YFORM='(A6,I1.1,A4)'
  IF (JLAYER >= 10)  YFORM='(A6,I2.2,A4)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_TG',JLAYER,' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XTG(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!* soil liquid water contents
!
DO JLAYER=1,I%NGROUND_LAYER
   WRITE(YLVL,'(I4)') JLAYER     
   YRECFM='WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
   YFORM='(A6,I1.1,A8)'
   IF (JLAYER >= 10)  YFORM='(A6,I2.2,A8)'
   WRITE(YCOMMENT,FMT=YFORM) 'X_Y_WG',JLAYER,' (m3/m3)'
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWG(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!* soil ice water contents
!
IF(I%CISBA=='DIF')THEN
  IWORK=I%NGROUND_LAYER
ELSE
  IWORK=2 !Only 2 soil ice layer in ISBA-FR
ENDIF
!
DO JLAYER=1,IWORK
   WRITE(YLVL,'(I4)') JLAYER     
   YRECFM='WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
   YFORM='(A7,I1.1,A8)'
   IF (JLAYER >= 10)  YFORM='(A7,I2.2,A8)'
   WRITE(YCOMMENT,YFORM) 'X_Y_WGI',JLAYER,' (m3/m3)'
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWGI(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)  
END DO
!
!* water intercepted on leaves
!
YRECFM='WR'
YCOMMENT='X_Y_WR (kg/m2)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWR(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* Glacier ice storage
!
YRECFM = 'GLACIER'
YCOMMENT='LGLACIER key for external prep'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%LGLACIER,IRESP,HCOMMENT=YCOMMENT)
!
IF(I%LGLACIER)THEN
  YRECFM='ICE_STO'
  YCOMMENT='X_Y_ICE_STO (kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XICE_STO(:,:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!* Leaf Area Index
!
IF (I%CPHOTO/='NON' .AND. I%CPHOTO/='AGS' .AND. I%CPHOTO/='AST') THEN
  !
  YRECFM='LAI'
  !
  YCOMMENT='X_Y_LAI (m2/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XLAI(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
IF ( TRIM(CASSIM_ISBA)=="ENKF" .AND. (LASSIM .OR. NIE/=0) ) THEN
  DO JVAR = 1,NVAR
    IF ( XADDTIMECORR(JVAR)>0. ) THEN
      WRITE(YVAR,'(I3)') JVAR
      YCOMMENT = 'Red_Noise_Enkf'
      YRECFM='RED_NOISE'//ADJUSTL(YVAR(:LEN_TRIM(YVAR)))
      CALL WRITE_SURF(DGU, U, &
                      HPROGRAM,YRECFM,I%XRED_NOISE(:,:,JVAR),IRESP,HCOMMENT=YCOMMENT)
    ENDIF
  ENDDO
ENDIF
!
!* snow mantel
!
 CALL WRITESURF_GR_SNOW(DGU, U, &
                        HPROGRAM,'VEG','     ',I%TSNOW)
!
!
!* key and/or field usefull to make an external prep
!
IF(I%CISBA=='DIF')THEN
!
  YRECFM = 'SOC'
  YCOMMENT='SOC key for external prep'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%LSOC,IRESP,HCOMMENT=YCOMMENT)
!
ELSE
!
  YRECFM = 'TEMPARP'
  YCOMMENT='LTEMP_ARP key for external prep'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%LTEMP_ARP,IRESP,HCOMMENT=YCOMMENT)
!
  IF(I%LTEMP_ARP)THEN
    YRECFM = 'NTEMPLARP'
    YCOMMENT='NTEMPLAYER_ARP for external prep'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NTEMPLAYER_ARP,IRESP,HCOMMENT=YCOMMENT)
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.  MEB Prognostic or Semi-prognostic variables
!            -------------------------------------------
!
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
!
IF (ISIZE_LMEB_PATCH>0) THEN
!
!* water intercepted on canopy vegetation leaves
!
  YRECFM='WRL'
  YCOMMENT='X_Y_WRL (kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,I%XWRL(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* ice on litter
!
  YRECFM='WRLI'
  YCOMMENT='X_Y_WRLI (kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                HPROGRAM,YRECFM,I%XWRLI(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* snow intercepted on canopy vegetation leaves
!
  YRECFM='WRVN'
  YCOMMENT='X_Y_WRVN (kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWRVN(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* canopy vegetation temperature
!
  YRECFM='TV'
  YCOMMENT='X_Y_TV (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XTV(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* litter temperature
!
  YRECFM='TL'
  YCOMMENT='X_Y_TL (K)'
  CALL WRITE_SURF(DGU, U, &
                HPROGRAM,YRECFM,I%XTL(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* vegetation canopy air temperature
!
  YRECFM='TC'
  YCOMMENT='X_Y_TC (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XTC(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* vegetation canopy air specific humidity
!
  YRECFM='QC'
  YCOMMENT='X_Y_QC (kg/kg)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XQC(:,:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
!
!* Fraction for each patch
!
YRECFM='PATCH'
YCOMMENT='fraction for each patch (-)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XPATCH(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* patch averaged radiative temperature (K)
!
YRECFM='TSRAD_NAT'
YCOMMENT='X_TSRAD_NAT (K)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XTSRAD_NAT(:),IRESP,HCOMMENT=YCOMMENT)
!
!* aerodynamical resistance
!
YRECFM='RESA'
YCOMMENT='X_Y_RESA (s/m)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XRESA(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* Land use variables
!
IF(OLAND_USE)THEN
!
  DO JLAYER=1,I%NGROUND_LAYER
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='OLD_DG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YFORM='(A6,I1.1,A8)'
    IF (JLAYER >= 10)  YFORM='(A6,I2.2,A8)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_OLD_DG',JLAYER,' (m)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XDG(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
!
ENDIF
!
!* ISBA-AGS variables
!
IF (I%CPHOTO/='NON') THEN
  YRECFM='AN'
  YCOMMENT='X_Y_AN (kgCO2/kgair m/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XAN(:,:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='ANDAY'
  YCOMMENT='X_Y_ANDAY (kgCO2/m2/day)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XANDAY(:,:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='ANFM'
  YCOMMENT='X_Y_ANFM (kgCO2/kgair m/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XANFM(:,:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='LE_AGS'
  YCOMMENT='X_Y_LE_AGS (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XLE(:,:),IRESP,HCOMMENT=YCOMMENT)
END IF
!
!
IF (I%CPHOTO=='NIT' .OR. I%CPHOTO=='NCB') THEN
  !
  DO JNBIOMASS=1,I%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM='BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YFORM='(A11,I1.1,A10)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_BIOMASS',JNBIOMASS,' (kgDM/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XBIOMASS(:,JNBIOMASS,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !
  DO JNBIOMASS=2,I%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM='RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YFORM='(A16,I1.1,A10)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_RESP_BIOMASS',JNBIOMASS,' (kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XRESP_BIOMASS(:,JNBIOMASS,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
END IF
!
!* Soil carbon
!
YRECFM = 'RESPSL'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%CRESPSL,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NLITTER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NNLITTER,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NLITTLEVS'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NNLITTLEVS,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NSOILCARB'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NNSOILCARB,IRESP,HCOMMENT=YCOMMENT)
!
IF(I%LSPINUPCARBS.OR.I%LSPINUPCARBW)THEN
  YRECFM='NBYEARSOLD'
  YCOMMENT='yrs'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NNBYEARSOLD,IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
IF (I%CRESPSL=='CNT') THEN
  !
  DO JNLITTER=1,I%NNLITTER
    DO JNLITTLEVS=1,I%NNLITTLEVS
      WRITE(YLVL,'(I1,A1,I1)') JNLITTER,'_',JNLITTLEVS
      YRECFM='LITTER'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      YFORM='(A10,I1.1,A1,I1.1,A8)'
      WRITE(YCOMMENT,FMT=YFORM) 'X_Y_LITTER',JNLITTER,' ',JNLITTLEVS,' (gC/m2)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XLITTER(:,JNLITTER,JNLITTLEVS,:),IRESP,HCOMMENT=YCOMMENT)
    END DO
  END DO

  DO JNSOILCARB=1,I%NNSOILCARB
    WRITE(YLVL,'(I4)') JNSOILCARB
    YRECFM='SOILCARB'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YFORM='(A8,I1.1,A8)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_SOILCARB',JNSOILCARB,' (gC/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XSOILCARB(:,JNSOILCARB,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
!
  DO JNLITTLEVS=1,I%NNLITTLEVS
    WRITE(YLVL,'(I4)') JNLITTLEVS
    YRECFM='LIGNIN_STR'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YFORM='(A12,I1.1,A8)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_LIGNIN_STRUC',JNLITTLEVS,' (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XLIGNIN_STRUC(:,JNLITTLEVS,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
!
ENDIF
!
!
IF (CHI%SVI%NDSTEQ > 0)THEN
  DO JSV = 1,NDSTMDE ! for all dust modes
    WRITE(YRECFM,'(A8,I3.3)')'FLX_DSTM',JSV
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DST%XSFDSTM(:,JSV,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5.  Time
!            ----
!
YRECFM='DTCUR'
YCOMMENT='s'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%TTIME,IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_ISBA_n
END MODULE

