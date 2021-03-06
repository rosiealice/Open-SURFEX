!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_VEG_PGD_GARDEN_n (CHI, DTCO, DST, I, SLT, U, &
                                  HPROGRAM, KLUOUT, KI, KGROUND_LAYER, KMONTH,        &
                        PVEGTYPE, PTDEEP, PGAMMAT, HPHOTO, HINIT, OTR_ML, HRUNOFF,  &
                        KNBIOMASS, PCO2, PRHOA, PABC, PPOI,  &
                        PGMES, PGC, PDMAX, PANMAX, PFZERO, PEPSO, PGAMM, PQDGAMM,   &
                        PQDGMES, PT1GMES, PT2GMES, PAMAX, PQDAMAX, PT1AMAX, PT2AMAX,&
                        PAH, PBH,                   &
                        KSV, HSV, YSV, HCH_NAMES, HAER_NAMES, HDSTNAMES, HSLTNAMES, &
                        HCHEM_SURF_FILE,                      &
                        PCLAY, PSAND, HPEDOTF,                                      &
                        PCONDSAT, PMPOTSAT, PBCOEF, PWWILT, PWFC, PWSAT,            &
                        PTAUICE, PCGSAT, PC1SAT, PC2REF, PC3, PC4B, PACOEF, PPCOEF, &
                        PC4REF, PPCPS, PPLVTT, PPLSTT,                              &
                        HSCOND, HISBA, PHCAPSOIL, PCONDDRY, PCONDSLD, HCPSURF,      &
                        PDG, PDROOT, PDG2, PROOTFRAC, PRUNOFFD, PDZG, PDZDIF,       &
                        PSOILWGHT, KWG_LAYER, KLAYER_HORT, KLAYER_DUN, PD_ICE,      &
                        PKSAT_ICE, PALBNIR_DRY, PALBVIS_DRY, PALBUV_DRY,            &
                        PALBNIR_WET, PALBVIS_WET, PALBUV_WET, PBSLAI_NITRO,         &
                        PCE_NITRO, PCNA_NITRO, PCF_NITRO                            )  
!#############################################################
!
!!****  *INIT_VEG_PGD_GARDEN_n* - routine to initialize ISBA
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
!     B. decharme 04/2013 : dummy for water table / surface coupling
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_INIT_VEG_PGD_n
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
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER, INTENT(IN)  :: KLUOUT
!
INTEGER, INTENT(IN)  :: KI
INTEGER, INTENT(IN)  :: KGROUND_LAYER
INTEGER, INTENT(IN)  :: KMONTH
!
REAL, DIMENSION(:,:), POINTER :: PVEGTYPE
!
REAL, DIMENSION(:), POINTER :: PTDEEP
REAL, DIMENSION(:), POINTER :: PGAMMAT
!
!
 CHARACTER(LEN=3), INTENT(IN) :: HPHOTO
 CHARACTER(LEN=3), INTENT(IN) :: HINIT
LOGICAL, INTENT(IN) :: OTR_ML
 CHARACTER(LEN=4), INTENT(IN) :: HRUNOFF
INTEGER, INTENT(IN) :: KNBIOMASS
REAL, DIMENSION(:), INTENT(IN) :: PCO2
REAL, DIMENSION(:), INTENT(IN) :: PRHOA
REAL, DIMENSION(:), POINTER :: PABC
REAL, DIMENSION(:), POINTER :: PPOI
REAL, DIMENSION(:), POINTER :: PGMES
REAL, DIMENSION(:), POINTER :: PGC
REAL, DIMENSION(:), POINTER :: PDMAX
REAL, DIMENSION(:), POINTER :: PANMAX
REAL, DIMENSION(:), POINTER :: PFZERO
REAL, DIMENSION(:), POINTER :: PEPSO
REAL, DIMENSION(:), POINTER :: PGAMM
REAL, DIMENSION(:), POINTER :: PQDGAMM
REAL, DIMENSION(:), POINTER :: PQDGMES
REAL, DIMENSION(:), POINTER :: PT1GMES
REAL, DIMENSION(:), POINTER :: PT2GMES
REAL, DIMENSION(:), POINTER :: PAMAX
REAL, DIMENSION(:), POINTER :: PQDAMAX
REAL, DIMENSION(:), POINTER :: PT1AMAX
REAL, DIMENSION(:), POINTER :: PT2AMAX
REAL, DIMENSION(:), POINTER :: PAH
REAL, DIMENSION(:), POINTER :: PBH
!
INTEGER,                          INTENT(IN) :: KSV      ! number of scalars
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN) :: HSV      ! name of all scalar variables
TYPE(SV_t), INTENT(INOUT) :: YSV
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HCH_NAMES
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: HAER_NAMES     
 CHARACTER(LEN=6), DIMENSION(:), POINTER, OPTIONAL :: HDSTNAMES
 CHARACTER(LEN=6), DIMENSION(:), POINTER, OPTIONAL :: HSLTNAMES
!
 CHARACTER(LEN=28), INTENT(OUT) :: HCHEM_SURF_FILE
!
REAL, DIMENSION(:,:), INTENT(IN) :: PCLAY
REAL, DIMENSION(:,:), INTENT(IN) :: PSAND
 CHARACTER(LEN=4), INTENT(IN) :: HPEDOTF
REAL, DIMENSION(:,:), POINTER :: PCONDSAT
REAL, DIMENSION(:,:), POINTER :: PMPOTSAT
REAL, DIMENSION(:,:), POINTER :: PBCOEF
REAL, DIMENSION(:,:), POINTER :: PWWILT
REAL, DIMENSION(:,:), POINTER :: PWFC
REAL, DIMENSION(:,:), POINTER :: PWSAT
REAL, DIMENSION(:), POINTER :: PTAUICE
REAL, DIMENSION(:), POINTER :: PCGSAT
REAL, DIMENSION(:), POINTER :: PC1SAT
REAL, DIMENSION(:), POINTER :: PC2REF
REAL, DIMENSION(:,:), POINTER :: PC3
REAL, DIMENSION(:), POINTER :: PC4B
REAL, DIMENSION(:), POINTER :: PACOEF
REAL, DIMENSION(:), POINTER :: PPCOEF
REAL, DIMENSION(:), POINTER :: PC4REF
!
REAL, DIMENSION(:), POINTER :: PPCPS
REAL, DIMENSION(:), POINTER :: PPLVTT
REAL, DIMENSION(:), POINTER :: PPLSTT
!
 CHARACTER(LEN=4), INTENT(IN) :: HSCOND
 CHARACTER(LEN=3), INTENT(IN) :: HISBA
REAL, DIMENSION(:,:), POINTER :: PHCAPSOIL
REAL, DIMENSION(:,:), POINTER :: PCONDDRY
REAL, DIMENSION(:,:), POINTER :: PCONDSLD
 CHARACTER(LEN=3), INTENT(IN) :: HCPSURF
!
REAL, DIMENSION(:,:), INTENT(IN) :: PDG
REAL, DIMENSION(:), INTENT(IN) :: PDROOT
REAL, DIMENSION(:), INTENT(IN) :: PDG2
REAL, DIMENSION(:,:), INTENT(IN) :: PROOTFRAC
REAL, DIMENSION(:), POINTER :: PRUNOFFD
REAL, DIMENSION(:,:), POINTER :: PDZG
REAL, DIMENSION(:,:), POINTER :: PDZDIF
REAL, DIMENSION(:,:), POINTER :: PSOILWGHT
INTEGER, DIMENSION(:), INTENT(IN) :: KWG_LAYER
INTEGER, INTENT(OUT) :: KLAYER_HORT
INTEGER, INTENT(OUT) :: KLAYER_DUN
!
REAL, DIMENSION(:), INTENT(INOUT) :: PD_ICE
REAL, DIMENSION(:), POINTER :: PKSAT_ICE
!
REAL, DIMENSION(:), POINTER :: PALBNIR_DRY
REAL, DIMENSION(:), POINTER :: PALBVIS_DRY
REAL, DIMENSION(:), POINTER :: PALBUV_DRY
REAL, DIMENSION(:), POINTER :: PALBNIR_WET
REAL, DIMENSION(:), POINTER :: PALBVIS_WET
REAL, DIMENSION(:), POINTER :: PALBUV_WET
!
REAL, DIMENSION(:), POINTER :: PBSLAI_NITRO
REAL, DIMENSION(:), INTENT(IN) :: PCE_NITRO
REAL, DIMENSION(:), INTENT(IN) :: PCNA_NITRO
REAL, DIMENSION(:), INTENT(IN) :: PCF_NITRO
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:,:), POINTER :: ZPATCH
REAL, DIMENSION(:,:,:), POINTER :: ZVEGTYPE_PATCH
INTEGER, DIMENSION(:), POINTER :: ISIZE_NATURE_P
INTEGER, DIMENSION(:,:), POINTER :: IR_NATURE_P
REAL, DIMENSION(0) :: ZTDEEP_CLI, ZGAMMAT_CLI, ZTHRESHOLD
INTEGER, DIMENSION(:,:), POINTER :: IIRRINUM
LOGICAL, DIMENSION(:,:), POINTER :: GIRRIDAY
LOGICAL, DIMENSION(:,:), POINTER :: GIRRIGATE
REAL, DIMENSION(:,:), POINTER :: ZTHRESHOLDSPT
REAL, DIMENSION(:,:,:), POINTER :: ZINCREASE
REAL, DIMENSION(:,:,:), POINTER :: ZTURNOVER
REAL, DIMENSION(:,:,:), POINTER :: ZSFDST
REAL, DIMENSION(:,:,:), POINTER :: ZSFDSTM
REAL, DIMENSION(:,:,:), POINTER :: ZSFSLT
REAL, DIMENSION(0) :: ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM
REAL, DIMENSION(0) :: ZHO2IP, ZHO2IM, ZHO2JP, ZHO2JM
REAL, DIMENSION(0,0) :: ZZ0
REAL, DIMENSION(:,:), POINTER :: ZZ0EFFIP
REAL, DIMENSION(:,:), POINTER :: ZZ0EFFIM
REAL, DIMENSION(:,:), POINTER :: ZZ0EFFJP
REAL, DIMENSION(:,:), POINTER :: ZZ0EFFJM
REAL, DIMENSION(:), POINTER :: ZZ0REL
!
REAL, DIMENSION(:,:), POINTER :: ZC1SAT
REAL, DIMENSION(:,:), POINTER :: ZC2REF
REAL, DIMENSION(:,:), POINTER :: ZC4REF
REAL, DIMENSION(:,:,:), POINTER :: ZC3
!
REAL, DIMENSION(:,:), POINTER :: ZPCPS
REAL, DIMENSION(:,:), POINTER :: ZPLVTT
REAL, DIMENSION(:,:), POINTER :: ZPLSTT
!
REAL, DIMENSION(:,:), POINTER :: ZKSAT_ICE
REAL, DIMENSION(:,:,:), POINTER :: ZDZG
REAL, DIMENSION(:,:,:), POINTER :: ZDZDIF
REAL, DIMENSION(:,:,:), POINTER :: ZSOILWGHT
REAL, DIMENSION(:,:,:), POINTER :: ZCONDSAT
REAL, DIMENSION(:,:), POINTER :: ZRUNOFFD
REAL, DIMENSION(:,:), POINTER :: ZBSLAI_NITRO
REAL, DIMENSION(:,:), POINTER :: ZANMAX
REAL, DIMENSION(:,:), POINTER :: ZFZERO
REAL, DIMENSION(:,:), POINTER :: ZEPSO
REAL, DIMENSION(:,:), POINTER :: ZGAMM
REAL, DIMENSION(:,:), POINTER :: ZQDGAMM
REAL, DIMENSION(:,:), POINTER :: ZQDGMES
REAL, DIMENSION(:,:), POINTER :: ZT1GMES
REAL, DIMENSION(:,:), POINTER :: ZT2GMES
REAL, DIMENSION(:,:), POINTER :: ZAMAX
REAL, DIMENSION(:,:), POINTER :: ZQDAMAX
REAL, DIMENSION(:,:), POINTER :: ZT1AMAX
REAL, DIMENSION(:,:), POINTER :: ZT2AMAX
REAL, DIMENSION(:,:), POINTER :: ZAH
REAL, DIMENSION(:,:), POINTER :: ZBH
REAL, DIMENSION(:,:), POINTER :: ZTAU_WOOD
REAL, DIMENSION(:,:), POINTER :: ZKANISO
REAL, DIMENSION(:,:), POINTER :: ZWD0
!
REAL, DIMENSION(:), POINTER   :: ZFWTD ! grid-cell fraction of water table to rise
REAL, DIMENSION(:), POINTER   :: ZWTD  ! water table depth from Obs, TRIP or MODCOU
!
REAL, DIMENSION(SIZE(PDG,1),SIZE(PDG,2),1) :: ZDG
REAL, DIMENSION(SIZE(PDROOT),1) :: ZDROOT
REAL, DIMENSION(SIZE(PDG2),1) :: ZDG2
REAL, DIMENSION(SIZE(PD_ICE),1) :: ZD_ICE
REAL, DIMENSION(SIZE(PROOTFRAC,1),SIZE(PROOTFRAC,2),1) :: ZROOTFRAC
REAL, DIMENSION(SIZE(PCE_NITRO),1) :: ZCE_NITRO
REAL, DIMENSION(SIZE(PCNA_NITRO),1) :: ZCNA_NITRO
REAL, DIMENSION(SIZE(PCF_NITRO),1) :: ZCF_NITRO
REAL, DIMENSION(SIZE(PGMES),1) :: ZGMES
REAL, DIMENSION(SIZE(PGC),1) :: ZGC
REAL, DIMENSION(SIZE(PDMAX),1) :: ZDMAX
INTEGER, DIMENSION(SIZE(KWG_LAYER),1) :: IWG_LAYER
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_PGD_GARDEN_n',0,ZHOOK_HANDLE)
!
!*       2.4    Fraction of each tile
!               ---------------------
!
NULLIFY(ZPATCH)
NULLIFY(ZVEGTYPE_PATCH)
NULLIFY(ISIZE_NATURE_P)
NULLIFY(IR_NATURE_P)
NULLIFY(IIRRINUM)
NULLIFY(GIRRIDAY)
NULLIFY(GIRRIGATE)
NULLIFY(ZTHRESHOLDSPT)
NULLIFY(ZINCREASE)
NULLIFY(ZTURNOVER)
NULLIFY(ZSFDST)
NULLIFY(ZSFDSTM)
NULLIFY(ZSFSLT)
NULLIFY(ZZ0EFFIP)
NULLIFY(ZZ0EFFIM)
NULLIFY(ZZ0EFFJP)
NULLIFY(ZZ0EFFJM)
NULLIFY(ZZ0REL)
!
NULLIFY(ZC1SAT)
NULLIFY(ZC2REF)
NULLIFY(ZC4REF)
NULLIFY(ZC3)
!
NULLIFY(ZPCPS)
NULLIFY(ZPLVTT)
NULLIFY(ZPLSTT)
!
NULLIFY(ZKSAT_ICE)
NULLIFY(ZDZG)
NULLIFY(ZDZDIF)
NULLIFY(ZSOILWGHT)
NULLIFY(ZCONDSAT)
NULLIFY(ZRUNOFFD)
NULLIFY(ZBSLAI_NITRO)
NULLIFY(ZANMAX)
NULLIFY(ZFZERO)
NULLIFY(ZEPSO)
NULLIFY(ZGAMM)
NULLIFY(ZQDGAMM)
NULLIFY(ZQDGMES)
NULLIFY(ZT1GMES)
NULLIFY(ZT2GMES)
NULLIFY(ZAMAX)
NULLIFY(ZT1AMAX)
NULLIFY(ZT2AMAX)
NULLIFY(ZAH)
NULLIFY(ZBH)
NULLIFY(ZTAU_WOOD)
NULLIFY(ZFWTD)
NULLIFY(ZWTD)
NULLIFY(ZWD0)
NULLIFY(ZKANISO)
!
ZDG(:,:,1) = PDG(:,:)
ZROOTFRAC(:,:,1) = PROOTFRAC(:,:)
ZDROOT(:,1) = PDROOT(:)
ZDG2(:,1) = PDG2(:)
ZD_ICE(:,1) = PD_ICE(:)
IWG_LAYER(:,1) = KWG_LAYER(:)
ZCE_NITRO(:,1) = PCE_NITRO(:)
ZCNA_NITRO(:,1) = PCNA_NITRO(:)
ZCF_NITRO(:,1) = PCF_NITRO(:)
ZGMES(:,1) = PGMES(:)
ZGC(:,1) = PGC(:)
ZDMAX(:,1) = PDMAX(:)
!
 CALL INIT_VEG_PGD_n(CHI, DTCO, DST, I, SLT, U, &
                     HPROGRAM, 'TOWN  ',KLUOUT, KI, 1, KGROUND_LAYER, KMONTH,   &
                  PVEGTYPE, ZPATCH, ZVEGTYPE_PATCH, ISIZE_NATURE_P, IR_NATURE_P,    &
                  0.0, &
                  .FALSE., .FALSE., ZTDEEP_CLI, ZGAMMAT_CLI, PTDEEP, PGAMMAT,   &
                  .FALSE., ZTHRESHOLD, IIRRINUM, GIRRIDAY, GIRRIGATE, ZTHRESHOLDSPT, &
                  HPHOTO, HINIT, OTR_ML, KNBIOMASS, PCO2, PRHOA, PABC, PPOI,  &
                  ZGMES, ZGC, ZDMAX, ZANMAX, ZFZERO, ZEPSO, ZGAMM, ZQDGAMM,   &
                  ZQDGMES, ZT1GMES, ZT2GMES, ZAMAX, ZQDAMAX, ZT1AMAX, ZT2AMAX,&
                  ZAH, ZBH, ZTAU_WOOD, ZINCREASE, ZTURNOVER,                  &
                  KSV, HSV, YSV, HCH_NAMES, HAER_NAMES, HDSTNAMES, HSLTNAMES, &
                  HCHEM_SURF_FILE,                      &
                  ZSFDST, ZSFDSTM, ZSFSLT,                                    &
                  ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM, ZHO2IP, ZHO2IM, ZHO2JP,     &
                  ZHO2JM, ZZ0, ZZ0EFFIP, ZZ0EFFIM, ZZ0EFFJP, ZZ0EFFJM, ZZ0REL,&
                  PCLAY, PSAND, HPEDOTF,                                      &
                  ZCONDSAT, PMPOTSAT, PBCOEF, PWWILT, PWFC, PWSAT, ZWD0,      &
                  ZKANISO, HRUNOFF,                                           &
                  PTAUICE, PCGSAT, ZC1SAT, ZC2REF, ZC3, PC4B, PACOEF, PPCOEF, &
                  ZC4REF, ZPCPS, ZPLVTT, ZPLSTT,                              &
                  HSCOND, HISBA, PHCAPSOIL, PCONDDRY, PCONDSLD, HCPSURF,      &
                  ZDG, ZDROOT, ZDG2, ZROOTFRAC, ZRUNOFFD, ZDZG, ZDZDIF,       &
                  ZSOILWGHT, IWG_LAYER, KLAYER_HORT, KLAYER_DUN, ZD_ICE,      &
                  ZKSAT_ICE, PALBNIR_DRY, PALBVIS_DRY, PALBUV_DRY,            &
                  PALBNIR_WET, PALBVIS_WET, PALBUV_WET, ZBSLAI_NITRO,         &
                  ZCE_NITRO, ZCNA_NITRO, ZCF_NITRO, ZFWTD, ZWTD               )
!
ALLOCATE(PPCPS(SIZE(ZPCPS,1)))
IF (SIZE(ZPCPS)>0) &
PPCPS(:) = ZPCPS(:,1)

ALLOCATE(PPLVTT(SIZE(ZPLVTT,1)))
IF (SIZE(ZPLVTT)>0) &
PPLVTT(:) = ZPLVTT(:,1)

ALLOCATE(PPLSTT(SIZE(ZPLSTT,1)))
IF (SIZE(ZPLSTT)>0) &
PPLSTT(:) = ZPLSTT(:,1)

IF (SIZE(ZD_ICE)>0) &
PD_ICE(:) = ZD_ICE(:,1)

ALLOCATE(PKSAT_ICE(SIZE(ZKSAT_ICE,1)))
IF (SIZE(ZKSAT_ICE)>0) &
PKSAT_ICE(:) = ZKSAT_ICE(:,1)

ALLOCATE(PDZG(SIZE(ZDZG,1),SIZE(ZDZG,2)))
IF (SIZE(ZDZG)>0) &
PDZG(:,:) = ZDZG(:,:,1)

ALLOCATE(PDZDIF(SIZE(ZDZDIF,1),SIZE(ZDZDIF,2)))
IF (SIZE(ZDZDIF)>0) &
PDZDIF(:,:) = ZDZDIF(:,:,1)

ALLOCATE(PSOILWGHT(SIZE(ZSOILWGHT,1),SIZE(ZSOILWGHT,2)))
IF (SIZE(ZSOILWGHT)>0) &
PSOILWGHT(:,:) = ZSOILWGHT(:,:,1)

ALLOCATE(PCONDSAT(SIZE(ZCONDSAT,1),SIZE(ZCONDSAT,2)))
IF (SIZE(ZCONDSAT)>0) &
PCONDSAT(:,:) = ZCONDSAT(:,:,1)

ALLOCATE(PC1SAT(SIZE(ZC1SAT,1)))
IF (SIZE(ZC1SAT)>0) &
PC1SAT(:) = ZC1SAT(:,1)

ALLOCATE(PC2REF(SIZE(ZC2REF,1)))
IF (SIZE(ZC2REF)>0) &
PC2REF(:) = ZC2REF(:,1)

ALLOCATE(PC4REF(SIZE(ZC4REF,1)))
IF (SIZE(ZC4REF)>0) &
PC4REF(:) = ZC4REF(:,1)

ALLOCATE(PC3(SIZE(ZC3,1),SIZE(ZC3,2)))
IF (SIZE(ZC3)>0) &
PC3(:,:) = ZC3(:,:,1)

ALLOCATE(PRUNOFFD(SIZE(ZRUNOFFD,1)))
IF (SIZE(ZRUNOFFD)>0) &
PRUNOFFD(:) = ZRUNOFFD(:,1) 

ALLOCATE(PBSLAI_NITRO(SIZE(ZBSLAI_NITRO,1)))
IF (SIZE(ZBSLAI_NITRO)>0) &
PBSLAI_NITRO(:) =  ZBSLAI_NITRO(:,1)

ALLOCATE(PANMAX(SIZE(ZANMAX,1)))
IF (SIZE(PANMAX)>0) &
PANMAX(:) = ZANMAX(:,1)

ALLOCATE(PFZERO(SIZE(ZFZERO,1)))
IF (SIZE(ZFZERO)>0) &
PFZERO(:) = ZFZERO(:,1)

ALLOCATE(PEPSO(SIZE(ZEPSO,1)))
IF (SIZE(ZEPSO)>0) &
PEPSO(:) = ZEPSO(:,1)

ALLOCATE(PGAMM(SIZE(ZGAMM,1)))
IF (SIZE(ZGAMM)>0) &
PGAMM(:) = ZGAMM(:,1)

ALLOCATE(PQDGAMM(SIZE(ZQDGAMM,1)))
IF (SIZE(ZQDGAMM)>0) &
PQDGAMM(:) = ZQDGAMM(:,1)

ALLOCATE(PQDGMES(SIZE(ZQDGMES,1)))
IF (SIZE(ZQDGMES)>0) &
PQDGMES(:) = ZQDGMES(:,1)

ALLOCATE(PT1GMES(SIZE(ZT1GMES,1)))
IF (SIZE(ZT1GMES)>0) &
PT1GMES(:) = ZT1GMES(:,1)

ALLOCATE(PT2GMES(SIZE(ZT2GMES,1)))
IF (SIZE(ZT2GMES)>0) &
PT2GMES(:) = ZT2GMES(:,1)

ALLOCATE(PAMAX(SIZE(ZAMAX,1)))
IF (SIZE(ZAMAX)>0) &
PAMAX(:) = ZAMAX(:,1)

ALLOCATE(PQDAMAX(SIZE(ZQDAMAX,1)))
IF (SIZE(ZQDAMAX)>0) &
PQDAMAX(:) = ZQDAMAX(:,1)

ALLOCATE(PT1AMAX(SIZE(ZT1AMAX,1)))
IF (SIZE(ZT1AMAX)>0) &
PT1AMAX(:) = ZT1AMAX(:,1)

ALLOCATE(PT2AMAX(SIZE(ZT2AMAX,1)))
IF (SIZE(ZT2AMAX)>0) &
PT2AMAX(:) = ZT2AMAX(:,1)

ALLOCATE(PAH(SIZE(ZAH,1)))
IF (SIZE(ZAH)>0) &
PAH(:) = ZAH(:,1)

ALLOCATE(PBH(SIZE(ZBH,1)))
IF (SIZE(ZBH)>0) &
PBH(:) = ZBH(:,1)
!
IF (LHOOK) CALL DR_HOOK('INIT_VEG_PGD_GARDEN_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_VEG_PGD_GARDEN_n
