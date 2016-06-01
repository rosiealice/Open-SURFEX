!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_PGD_ISBA_n (DTCO, DGU, U, CHI, DGMI, I, &
                                        HPROGRAM)
!     #########################################
!
!!****  *WRITE_DIAG_PGD_ISBA_n* - writes the ISBA physiographic diagnostic fields
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
!!      Original    01/2004 
!!      Modified    10/2004 by P. Le Moigne: add XZ0REL, XVEGTYPE_PATCH
!!      Modified    11/2005 by P. Le Moigne: limit length of VEGTYPE_PATCH field names
!!      Modified    11/2013 by B. Decharme : XPATCH now in writesurf_isban.F90
!!      Modified    10/2014 by P. Samuelsson: MEB variables
!!      Modified    06/2014 by B. Decharme : add XVEGTYPE
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
USE MODD_AGRI,       ONLY : LAGRIP
!
!
USE MODD_IO_SURF_FA, ONLY : LFANOCOMPACT, LPREP
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DGMI
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,3)) :: ZWORK ! Work array
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,2)) :: ZDG   ! Work array
REAL, DIMENSION(SIZE(I%XDG,1)            ) :: ZDG2
REAL, DIMENSION(SIZE(I%XDG,1)            ) :: ZDTOT
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=2)  :: YLVLV, YPAS
 CHARACTER(LEN=4)  :: YLVL
!
INTEGER         :: JJ, JL, JP, ILAYER
INTEGER           :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',0,ZHOOK_HANDLE)
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'NATURE','ISBA  ','WRITE')
!
!-------------------------------------------------------------------------------
!
!* Leaf Area Index
!
IF (I%CPHOTO=='NON' .OR. I%CPHOTO=='AGS' .OR. I%CPHOTO=='AST') THEN
  !
  YRECFM='LAI'
  YCOMMENT='leaf area index (-)'
  !
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XLAI(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (ISIZE_LMEB_PATCH>0) THEN
    !
    YRECFM='LAIGV'
    YCOMMENT='MEB: understory leaf area index (-)'
    !
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XLAIGV(:,:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Vegetation fraction
!
YRECFM='VEG'
YCOMMENT='vegetation fraction (-)'
!
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XVEG(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* Surface roughness length (without snow)
!
YRECFM='Z0VEG'
YCOMMENT='surface roughness length (without snow) (m)'
!
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XZ0(:,:),IRESP,HCOMMENT=YCOMMENT)
!
IF (ISIZE_LMEB_PATCH>0) THEN
  !
  YRECFM='GNDLITTER'
  YCOMMENT='MEB: ground litter fraction (-)'
  !
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XGNDLITTER(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='Z0LITTER'
  YCOMMENT='MEB: ground litter roughness length (without snow) (m)'
  !
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XZ0LITTER(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Soil depth for each patch
!
DO JL=1,SIZE(I%XDG,2)
  IF (JL<10) THEN
    WRITE(YRECFM,FMT='(A2,I1)') 'DG',JL
  ELSE
    WRITE(YRECFM,FMT='(A2,I2)') 'DG',JL          
  ENDIF
  YCOMMENT='soil depth'//' (M)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XDG(:,JL,:),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!* Averaged Soil depth
!
IF(I%NPATCH>1)THEN
!        
  ZDG(:,:)=0.0
  DO JP=1,I%NPATCH
     DO JL=1,SIZE(I%XDG,2)
        DO JJ=1,SIZE(I%XDG,1) 
           ZDG(JJ,JL)=ZDG(JJ,JL)+I%XPATCH(JJ,JP)*I%XDG(JJ,JL,JP)
        ENDDO
     ENDDO
  ENDDO
!
  DO JL=1,SIZE(I%XDG,2)
    WRITE(YLVL,'(I4)')JL
    YRECFM='DG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='averaged soil depth layer '//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))//' (m)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZDG(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
!        
ENDIF
!
!-------------------------------------------------------------------------------
!
IF(I%CISBA=='DIF')THEN
!
  ZDG2 (:)=0.0
  ZDTOT(:)=0.0
  ZWORK(:,:)=XUNDEF
  DO JP=1,SIZE(I%XDG,3)
     DO JJ=1,SIZE(I%XDG,1)
        ZDG2(JJ)=ZDG2(JJ)+I%XPATCH(JJ,JP)*I%XDG2(JJ,JP)
        JL=I%NWG_LAYER(JJ,JP)
        IF(JL/=NUNDEF)THEN
          ZWORK(JJ,JP)=I%XDG(JJ,JL,JP)
          ZDTOT(JJ)=ZDTOT(JJ)+I%XPATCH(JJ,JP)*I%XDG(JJ,JL,JP)
        ENDIF
     ENDDO
  ENDDO
!
!* Root depth
!
  YRECFM='DROOT_DIF'
  YCOMMENT='Root depth in ISBA-DIF'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XDROOT(:,:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='DG2_DIF'
  YCOMMENT='DG2 depth in ISBA-DIF'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XDG2(:,:),IRESP,HCOMMENT=YCOMMENT)
!  
  IF(I%NPATCH>1)THEN
    YRECFM='DG2_DIF_ISBA'
    YCOMMENT='Averaged DG2 depth in ISBA-DIF'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZDG2(:),IRESP,HCOMMENT=YCOMMENT)          
  ENDIF  
!
!* Runoff depth
!
  YRECFM='RUNOFFD'
  YCOMMENT='Runoff deph in ISBA-DIF'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XRUNOFFD(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!* Total soil depth for mositure
!
  YRECFM='DTOT_DIF'
  YCOMMENT='Total soil depth for moisture in ISBA-DIF'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:,:),IRESP,HCOMMENT=YCOMMENT)
!
  IF(I%NPATCH>1)THEN
    YRECFM='DTOTDF_ISBA'
    YCOMMENT='Averaged Total soil depth for moisture in ISBA-DIF'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZDTOT(:),IRESP,HCOMMENT=YCOMMENT)          
  ENDIF
!
!* Root fraction for each patch
!
  DO JL=1,SIZE(I%XROOTFRAC,2)
     IF (JL<10) THEN
       WRITE(YRECFM,FMT='(A8,I1)') 'ROOTFRAC',JL
     ELSE
       WRITE(YRECFM,FMT='(A8,I2)') 'ROOTFRAC',JL          
     ENDIF  
     YCOMMENT='root fraction by layer (-)'
     ZWORK(:,:)=XUNDEF
     DO JJ=1,SIZE(I%XDG,1)
        WHERE(JL<=I%NWG_LAYER(JJ,:).AND.I%NWG_LAYER(JJ,:)/=NUNDEF)
              ZWORK(JJ,:)=I%XROOTFRAC(JJ,JL,:)
        ENDWHERE
     ENDDO
     CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  IF (ISIZE_LMEB_PATCH>0) THEN
    DO JL=1,SIZE(I%XROOTFRACGV,2)
       IF (JL<10) THEN
         WRITE(YRECFM,FMT='(A10,I1)') 'ROOTFRACGV',JL
       ELSE
         WRITE(YRECFM,FMT='(A10,I2)') 'ROOTFRACGV',JL          
       ENDIF  
       YCOMMENT='MEB: understory root fraction by layer (-)'
       ZWORK(:,:)=XUNDEF
       DO JJ=1,SIZE(I%XDG,1)
          WHERE(JL<=I%NWG_LAYER(JJ,:).AND.I%NWG_LAYER(JJ,:)/=NUNDEF)
                ZWORK(JJ,:)=I%XROOTFRACGV(JJ,JL,:)
          ENDWHERE
       ENDDO
       CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:,:),IRESP,HCOMMENT=YCOMMENT)
    END DO
  ENDIF
!
!* SOC fraction for each layer
!
  IF(I%LSOC)THEN
    DO JL=1,SIZE(I%XDG,2)
     IF (JL<10) THEN
       WRITE(YRECFM,FMT='(A7,I1)') 'FRACSOC',JL
     ELSE
       WRITE(YRECFM,FMT='(A7,I2)') 'FRACSOC',JL          
     ENDIF  
     YCOMMENT='SOC fraction by layer (-)'
     CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XFRACSOC(:,JL),IRESP,HCOMMENT=YCOMMENT)
    END DO
  ENDIF
!
ENDIF        
!
!-------------------------------------------------------------------------------
!
DO JL=1,SIZE(I%XDG,2)
   IF (JL<10) THEN
     WRITE(YRECFM,FMT='(A4,I1)') 'WSAT',JL
   ELSE
     WRITE(YRECFM,FMT='(A4,I2)') 'WSAT',JL          
   ENDIF  
  YCOMMENT='soil porosity by layer (m3/m3)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWSAT(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO
!
DO JL=1,SIZE(I%XDG,2)
   IF (JL<10) THEN
     WRITE(YRECFM,FMT='(A3,I1)') 'WFC',JL
   ELSE
     WRITE(YRECFM,FMT='(A3,I2)') 'WFC',JL          
   ENDIF  
  YCOMMENT='field capacity by layer (m3/m3)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWFC(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO
!
DO JL=1,SIZE(I%XDG,2)
   IF (JL<10) THEN
     WRITE(YRECFM,FMT='(A5,I1)') 'WWILT',JL
   ELSE
     WRITE(YRECFM,FMT='(A5,I2)') 'WWILT',JL          
   ENDIF  
  YCOMMENT='wilting point by layer (m3/m3)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWWILT(:,JL),IRESP,HCOMMENT=YCOMMENT)
ENDDO     
!
!-------------------------------------------------------------------------------
! For Earth System Model
IF(LFANOCOMPACT.AND..NOT.LPREP)THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!
YRECFM='Z0REL'
YCOMMENT='orography roughness length (M)'
!
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XZ0REL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* Runoff soil ice depth for each patch
!
IF(I%CHORT=='SGH'.AND.I%CISBA/='DIF')THEN
  YRECFM='DICE'
  YCOMMENT='soil ice depth for runoff (m)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XD_ICE(:,:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!-------------------------------------------------------------------------------
!
!* Fraction of each vegetation type in the grid cell
!
DO JL=1,SIZE(I%XVEGTYPE_PATCH,2)
  WRITE(YPAS,'(I2)') JL 
  YLVLV=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
  WRITE(YRECFM,FMT='(A9)') 'VEGTYPE'//YLVLV
  YCOMMENT='fraction of each vegetation type in the grid cell'//' (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XVEGTYPE(:,JL),IRESP,HCOMMENT=YCOMMENT)
END DO
!-------------------------------------------------------------------------------
!
!* Fraction of each vegetation type for each patch
!
IF(I%NPATCH>1.AND.SIZE(I%XVEGTYPE_PATCH,2)/=SIZE(I%XVEGTYPE_PATCH,3))THEN
!
  DO JL=1,SIZE(I%XVEGTYPE_PATCH,2)
    WRITE(YPAS,'(I2)') JL 
    YLVLV=ADJUSTL(YPAS(:LEN_TRIM(YPAS)))
    WRITE(YRECFM,FMT='(A9)') 'VEGTY_P'//YLVLV
    YCOMMENT='fraction of each vegetation type in each patch'//' (-)'
    CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,I%XVEGTYPE_PATCH(:,JL,:),IRESP,HCOMMENT=YCOMMENT)
  END DO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!* other surface parameters
!
YRECFM='RSMIN'
YCOMMENT='minimum stomatal resistance (sm-1)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XRSMIN(:,:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='GAMMA'
YCOMMENT='coefficient for RSMIN calculation (-)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XGAMMA(:,:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='CV'
YCOMMENT='vegetation thermal inertia coefficient (-)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XCV(:,:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='RGL'
YCOMMENT='maximum solar radiation usable in photosynthesis (-)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XRGL(:,:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='EMIS_ISBA'
YCOMMENT='surface emissivity (-)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XEMIS(:,:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='WRMAX_CF'
YCOMMENT='coefficient for maximum water interception (-)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWRMAX_CF(:,:),IRESP,HCOMMENT=YCOMMENT)
!
IF (ISIZE_LMEB_PATCH>0) THEN
  !
  YRECFM='RSMINGV'
  YCOMMENT='MEB: understory minimum stomatal resistance (sm-1)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XRSMINGV(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='GAMMAGV'
  YCOMMENT='MEB: understory coefficient for RSMIN calculation (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XGAMMAGV(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='RGLGV'
  YCOMMENT='MEB: understory maximum solar radiation usable in photosynthesis (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XRGLGV(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WRMAX_CFGV'
  YCOMMENT='MEB: understory coefficient for maximum water interception (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWRMAX_CFGV(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='H_VEG'
  YCOMMENT='MEB: height of vegetation (m)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XH_VEG(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (DGMI%LSURF_DIAG_ALBEDO) THEN
!
!* Soil albedos
!
!
   YRECFM='ALBNIR_S'
   YCOMMENT='soil near-infra-red albedo (-)'
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XALBNIR_SOIL(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
   YRECFM='ALBVIS_S'
   YCOMMENT='soil visible albedo (-)'
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XALBVIS_SOIL(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
   YRECFM='ALBUV_S'
   YCOMMENT='soil UV albedo (-)'
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XALBUV_SOIL(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* albedos
!
   YRECFM='ALBNIR_ISBA'
   YCOMMENT='total near-infra-red albedo (-)'
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XALBNIR(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
   YRECFM='ALBVIS_ISBA'
   YCOMMENT='total visible albedo (-)'
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XALBVIS(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
   YRECFM='ALBUV_ISBA'
   YCOMMENT='total UV albedo (-)'
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XALBUV(:,:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!-------------------------------------------------------------------------------
!
!* chemical soil resistances
!
IF (CHI%CCH_DRY_DEP=='WES89' .AND. CHI%SVI%NBEQ>0) THEN
  YRECFM='SOILRC_SO2'
  YCOMMENT='bare soil resistance for SO2 (?)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,CHI%XSOILRC_SO2(:,:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SOILRC_O3'
  YCOMMENT='bare soil resistance for O3 (?)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,CHI%XSOILRC_O3(:,:),IRESP,HCOMMENT=YCOMMENT)
END IF
!
!-------------------------------------------------------------------------------
!
IF (LAGRIP .AND. (I%CPHOTO=='LAI' .OR. I%CPHOTO=='LST' .OR. I%CPHOTO=='NIT' .OR. I%CPHOTO=='NCB') ) THEN
!
!* seeding and reaping
!
!
  YRECFM='TSEED'
  YCOMMENT='date of seeding (-)'
!
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%TSEED(:,:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='TREAP'
  YCOMMENT='date of reaping (-)'
!
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%TREAP(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* irrigated fraction
!
  YRECFM='IRRIG'
  YCOMMENT='flag for irrigation (irrigation if >0.) (-)'
!
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XIRRIG(:,:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!* water supply for irrigation
!
  YRECFM='WATSUP'
  YCOMMENT='water supply during irrigation process (mm)'
!
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWATSUP(:,:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!-------------------------------------------------------------------------------
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_PGD_ISBA_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_PGD_ISBA_n
