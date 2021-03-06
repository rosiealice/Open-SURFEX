!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SURFEX_ALLOC(YDSURFEX)
!
USE MODD_TEB_PAR, ONLY : NTEB_PATCH_MAX
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_AGRI_n, ONLY : AGRI_INIT
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_INIT
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_INIT
USE MODD_CH_EMIS_FIELD_n, ONLY : CH_EMIS_FIELD_INIT
USE MODD_CH_FLAKE_n, ONLY : CH_FLAKE_INIT
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_INIT
USE MODD_CH_SEAFLUX_n, ONLY : CH_SEAFLUX_INIT
USE MODD_CH_SNAP_n, ONLY : CH_EMIS_SNAP_INIT
USE MODD_CH_SURF_n, ONLY : CH_SURF_INIT
USE MODD_CH_TEB_n, ONLY : CH_TEB_INIT
USE MODD_CH_WATFLUX_n, ONLY : CH_WATFLUX_INIT
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_INIT
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_INIT
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_INIT
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_INIT
USE MODD_DATA_TEB_GARDEN_n, ONLY : DATA_TEB_GARDEN_INIT
USE MODD_DATA_TEB_GREENROOF_n, ONLY : DATA_TEB_GREENROOF_INIT
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_INIT
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_INIT
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_INIT
USE MODD_DIAG_FLAKE_n, ONLY : DIAG_FLAKE_INIT
USE MODD_DIAG_IDEAL_n, ONLY : DIAG_IDEAL_INIT
USE MODD_DIAG_ISBA_n, ONLY : DIAG_ISBA_INIT
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_INIT
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_INIT
USE MODD_DIAG_MISC_TEB_OPTION_n, ONLY : DIAG_MISC_TEB_OPTIONS_INIT
USE MODD_DIAG_OCEAN_n, ONLY : DIAG_OCEAN_INIT
USE MODD_DIAG_SEAFLUX_n, ONLY : DIAG_SEAFLUX_INIT
USE MODD_DIAG_SEAICE_n, ONLY : DIAG_SEAICE_INIT
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_INIT
USE MODD_DIAG_TEB_GARDEN_n, ONLY : DIAG_TEB_GARDEN_INIT
USE MODD_DIAG_TEB_GREENROOF_n, ONLY : DIAG_TEB_GREENROOF_INIT
USE MODD_DIAG_TEB_n, ONLY : DIAG_TEB_INIT
USE MODD_DIAG_UTCI_TEB_n, ONLY : DIAG_UTCI_TEB_INIT
USE MODD_DIAG_WATFLUX_n, ONLY : DIAG_WATFLUX_INIT
USE MODD_DST_n, ONLY : DST_INIT
USE MODD_DUMMY_SURF_FIELDS_n, ONLY : DUMMY_SURF_FIELDS_INIT
USE MODD_EMIS_GR_FIELD_n, ONLY : EMIS_GR_FIELD_INIT
USE MODD_FLAKE_GRID_n, ONLY : FLAKE_GRID_INIT
USE MODD_FLAKE_n, ONLY : FLAKE_INIT
USE MODD_FLAKE_SBL_n, ONLY : FLAKE_SBL_INIT
USE MODD_GR_BIOG_GARDEN_n, ONLY : GR_BIOG_GARDEN_INIT
USE MODD_GR_BIOG_GREENROOF_n, ONLY : GR_BIOG_GREENROOF_INIT
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_INIT
USE MODD_IDEAL_n, ONLY : IDEAL_INIT
USE MODD_ISBA_CANOPY_n, ONLY : ISBA_CANOPY_INIT
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_INIT
USE MODD_ISBA_n, ONLY : ISBA_INIT
USE MODD_OCEAN_n, ONLY : OCEAN_INIT
USE MODD_OCEAN_REL_n, ONLY : OCEAN_REL_INIT
USE MODD_PACK_CH_ISBA, ONLY : PACK_CH_ISBA_INIT
USE MODD_PACK_DIAG_ISBA, ONLY : PACK_DIAG_ISBA_INIT
USE MODD_PACK_ISBA, ONLY : PACK_ISBA_INIT
USE MODD_SEAFLUX_GRID_n, ONLY : SEAFLUX_GRID_INIT
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_INIT
USE MODD_SEAFLUX_SBL_n, ONLY : SEAFLUX_SBL_INIT
USE MODD_SLT_n, ONLY : SLT_INIT
USE MODD_SSO_CANOPY_n, ONLY : SSO_CANOPY_INIT
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_INIT
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_INIT
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_INIT
USE MODD_SV_n, ONLY : SV_INIT
USE MODD_TEB_CANOPY_n, ONLY : TEB_CANOPY_INIT
USE MODD_TEB_GARDEN_OPTION_n, ONLY : TEB_GARDEN_OPTIONS_INIT
USE MODD_TEB_GARDEN_PGD_n, ONLY : TEB_GARDEN_PGD_INIT
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_INIT
USE MODD_TEB_GREENROOF_PGD_n, ONLY : TEB_GREENROOF_PGD_INIT
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_INIT
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_INIT
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_INIT
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_INIT
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_INIT
USE MODD_WATFLUX_GRID_n, ONLY : WATFLUX_GRID_INIT
USE MODD_WATFLUX_n, ONLY : WATFLUX_INIT
USE MODD_WATFLUX_SBL_n, ONLY : WATFLUX_SBL_INIT
!
USE MODD_BEM_n, ONLY : BEM_INIT
USE MODD_DIAG_CUMUL_TEB_n, ONLY : DIAG_CUMUL_TEB_INIT
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_INIT
USE MODD_TEB_GARDEN_n, ONLY : TEB_GARDEN_INIT
USE MODD_TEB_GARDEN_PGD_EVOL_n, ONLY : TEB_GARDEN_PGD_EVOL_INIT
USE MODD_TEB_GREENROOF_n, ONLY : TEB_GREENROOF_INIT
USE MODD_TEB_GREENROOF_PGD_EVOL_n, ONLY : TEB_GREENROOF_PGD_EVOL_INIT
USE MODD_TEB_n, ONLY : TEB_INIT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE (SURFEX_t), INTENT (INOUT) :: YDSURFEX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("SURFEX_ALLOC",0,ZHOOK_HANDLE)
!
  CALL CH_FLAKE_INIT(YDSURFEX%FM%CHF)
  CALL DIAG_FLAKE_INIT(YDSURFEX%FM%DGF)
  CALL DIAG_MISC_FLAKE_INIT(YDSURFEX%FM%DGMF)
  CALL FLAKE_GRID_INIT(YDSURFEX%FM%FG)
  CALL FLAKE_INIT(YDSURFEX%FM%F)
  CALL FLAKE_SBL_INIT(YDSURFEX%FM%FSB)
  !  
  CALL CH_WATFLUX_INIT(YDSURFEX%WM%CHW)
  CALL DIAG_WATFLUX_INIT(YDSURFEX%WM%DGW)
  CALL WATFLUX_GRID_INIT(YDSURFEX%WM%WG)
  CALL WATFLUX_INIT(YDSURFEX%WM%W)
  CALL WATFLUX_SBL_INIT(YDSURFEX%WM%WSB)
  !
  CALL CH_SEAFLUX_INIT(YDSURFEX%SM%CHS)
  CALL DATA_SEAFLUX_INIT(YDSURFEX%SM%DTS)
  CALL DIAG_OCEAN_INIT(YDSURFEX%SM%DGO)
  CALL DIAG_SEAFLUX_INIT(YDSURFEX%SM%DGS)
  CALL DIAG_SEAICE_INIT(YDSURFEX%SM%DGSI)
  CALL OCEAN_INIT(YDSURFEX%SM%O)
  CALL OCEAN_REL_INIT(YDSURFEX%SM%OR)
  CALL SEAFLUX_GRID_INIT(YDSURFEX%SM%SG)
  CALL SEAFLUX_INIT(YDSURFEX%SM%S)
  CALL SEAFLUX_SBL_INIT(YDSURFEX%SM%SSB)
  !
  CALL AGRI_INIT(YDSURFEX%IM%AG)
  CALL CH_ISBA_INIT(YDSURFEX%IM%CHI)
  CALL DATA_ISBA_INIT(YDSURFEX%IM%DTI)
  CALL DIAG_EVAP_ISBA_INIT(YDSURFEX%IM%DGEI)
  CALL DIAG_ISBA_INIT(YDSURFEX%IM%DGI)
  CALL DIAG_MISC_ISBA_INIT(YDSURFEX%IM%DGMI)
  CALL GR_BIOG_INIT(YDSURFEX%IM%GB)
  CALL ISBA_CANOPY_INIT(YDSURFEX%IM%ICP)
  CALL ISBA_GRID_INIT(YDSURFEX%IM%IG)
  CALL ISBA_INIT(YDSURFEX%IM%I)
  CALL PACK_CH_ISBA_INIT(YDSURFEX%IM%PKCI)
  CALL PACK_DIAG_ISBA_INIT(YDSURFEX%IM%PKDI)
  CALL PACK_ISBA_INIT(YDSURFEX%IM%PKI)
  !
  CALL TEB_VEG_OPTIONS_INIT(YDSURFEX%GDM%TVG)    
  CALL DATA_TEB_GARDEN_INIT(YDSURFEX%GDM%DTGD)
  CALL DIAG_TEB_GARDEN_INIT(YDSURFEX%GDM%DGTGD)
  CALL GR_BIOG_GARDEN_INIT(YDSURFEX%GDM%GBGD)
  CALL TEB_GARDEN_OPTIONS_INIT(YDSURFEX%GDM%TGDO)
  CALL TEB_GARDEN_PGD_INIT(YDSURFEX%GDM%TGDP)
  CALL TEB_GARDEN_INIT(YDSURFEX%GDM%TGD,NTEB_PATCH_MAX)
  CALL TEB_GARDEN_PGD_EVOL_INIT(YDSURFEX%GDM%TGDPE,NTEB_PATCH_MAX) 
  CALL TEB_IRRIG_INIT(YDSURFEX%GDM%TIR)    
  !
  CALL DATA_TEB_GREENROOF_INIT(YDSURFEX%GRM%DTGR)
  CALL DIAG_TEB_GREENROOF_INIT(YDSURFEX%GRM%DGTGR)
  CALL GR_BIOG_GREENROOF_INIT(YDSURFEX%GRM%GBGR)  
  CALL TEB_GREENROOF_OPTIONS_INIT(YDSURFEX%GRM%TGRO)
  CALL TEB_GREENROOF_PGD_INIT(YDSURFEX%GRM%TGRP)
  CALL TEB_GREENROOF_INIT(YDSURFEX%GRM%TGR,NTEB_PATCH_MAX)
  CALL TEB_GREENROOF_PGD_EVOL_INIT(YDSURFEX%GRM%TGRPE,NTEB_PATCH_MAX)
  !
  CALL CH_TEB_INIT(YDSURFEX%TM%CHT)
  CALL DATA_TEB_INIT(YDSURFEX%TM%DTT)
  CALL DIAG_MISC_TEB_OPTIONS_INIT(YDSURFEX%TM%DGMTO)
  CALL DIAG_TEB_INIT(YDSURFEX%TM%DGT)
  CALL DIAG_UTCI_TEB_INIT(YDSURFEX%TM%DGUT)
  CALL TEB_CANOPY_INIT(YDSURFEX%TM%TCP)
  CALL TEB_GRID_INIT(YDSURFEX%TM%TG)
  CALL TEB_OPTIONS_INIT(YDSURFEX%TM%TOP)
  CALL TEB_PANEL_INIT(YDSURFEX%TM%TPN)
  CALL DIAG_CUMUL_TEB_INIT(YDSURFEX%TM%DGCT,NTEB_PATCH_MAX)
  CALL DIAG_MISC_TEB_INIT(YDSURFEX%TM%DGMT,NTEB_PATCH_MAX)
  CALL TEB_INIT(YDSURFEX%TM%T,NTEB_PATCH_MAX)  
  !
  CALL BLD_DESC_INIT(YDSURFEX%TM%BDD)
  CALL BEM_OPTIONS_INIT(YDSURFEX%TM%BOP)
  CALL DATA_BEM_INIT(YDSURFEX%TM%DTB)  
  CALL BEM_INIT(YDSURFEX%TM%B,NTEB_PATCH_MAX)  
  !
  CALL DATA_COVER_INIT(YDSURFEX%DTCO)
  CALL DATA_TSZ0_INIT(YDSURFEX%DTZ)
  CALL DUMMY_SURF_FIELDS_INIT(YDSURFEX%DUU)
  !
  CALL SURF_ATM_GRID_INIT(YDSURFEX%UG)
  CALL SURF_ATM_INIT(YDSURFEX%U)
  CALL DIAG_SURF_ATM_INIT(YDSURFEX%DGU)  
  CALL SURF_ATM_SSO_INIT(YDSURFEX%USS)
  CALL SSO_CANOPY_INIT(YDSURFEX%SSCP)
  !
  CALL DIAG_IDEAL_INIT(YDSURFEX%DGL)
  CALL IDEAL_INIT(YDSURFEX%L)
  !
  CALL SV_INIT(YDSURFEX%SV)
  CALL CH_SURF_INIT(YDSURFEX%CHU)  
  CALL CH_EMIS_FIELD_INIT(YDSURFEX%CHE)
  CALL CH_EMIS_SNAP_INIT(YDSURFEX%CHN)
  CALL EMIS_GR_FIELD_INIT(YDSURFEX%EGF)  
  CALL DST_INIT(YDSURFEX%DST)
  CALL SLT_INIT(YDSURFEX%SLT)
  !
IF (LHOOK) CALL DR_HOOK("SURFEX_ALLOC",1,ZHOOK_HANDLE)
!
END SUBROUTINE SURFEX_ALLOC
