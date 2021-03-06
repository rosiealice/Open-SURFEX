!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_EXTERN (DTCO, I, U, &
                             HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,PFIELD,OKEY)
!     #################################################################################
!
!!****  *PREP_ISBA_EXTERN* - initializes ISBA fields from operational GRIB
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme  04/2014, external init with FA files
!!------------------------------------------------------------------
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_ISBA,      ONLY : XGRID_SOIL, XWR_DEF
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODE_READ_EXTERN
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_INTERP_GRID_NAT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,            INTENT(IN)   :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER   :: PFIELD    ! field to interpolate horizontally (on final soil grid)
LOGICAL, OPTIONAL,  INTENT(INOUT):: OKEY
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IRESP          ! reading return code
INTEGER           :: INI            ! total 1D dimension
INTEGER           :: IPATCH         ! number of patch
LOGICAL           :: GGLACIER
 CHARACTER(LEN=3)  :: YPHOTO
!
REAL, DIMENSION(:,:,:), POINTER     :: ZFIELD         ! field read on initial MNH vertical soil grid, all patches
REAL, DIMENSION(:,:),   POINTER     :: ZFIELD1        ! field read on initial MNH vertical soil grid, one patch
REAL, DIMENSION(:,:,:), POINTER     :: ZD             ! layer thicknesses
REAL, DIMENSION(:,:),   POINTER     :: ZD1            ! layer thicknesses, one patch
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZOUT           !
REAL, DIMENSION(:), ALLOCATABLE     :: ZMASK
INTEGER                             :: JPATCH, JL       ! loop counter for patch
INTEGER :: IVERSION
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_EXTERN',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL OPEN_AUX_IO_SURF(&
                       HFILEPGD,HFILEPGDTYPE,'FULL  ')
!
 CALL PREP_GRID_EXTERN(&
                       HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
YRECFM='VERSION'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
!
ALLOCATE(ZMASK(INI))
IF (IVERSION>=7) THEN 
  YRECFM='FRAC_NATURE'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZMASK,IRESP,HDIR='A')
ELSE
  ZMASK(:) = 1.
ENDIF
!
!---------------------------------------------------------------------------------------
!
!*      3.     Transformation into physical quantity to be interpolated
!              --------------------------------------------------------
!
SELECT CASE(HSURF)
!
!*     3.      Orography
!              ---------
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(INI,1,1))
    PFIELD(:,:,:) = XUNDEF
    YRECFM='ZS'
    CALL READ_SURF(&
                   HFILEPGDTYPE,YRECFM,PFIELD(:,1,1),IRESP,HDIR='A')
    CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!--------------------------------------------------------------------------
!
!
!*      3.1    Profile of temperature, water or ice in the soil
!
  CASE('TG    ','WG    ','WGI   ')
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!* reading of the profile and its depth definition
     CALL READ_EXTERN_ISBA(U, &
                           DTCO, I, &
                           HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                           KLUOUT,INI,HSURF,HSURF,ZFIELD,ZD,OKEY)
! 
     ALLOCATE(ZFIELD1(SIZE(ZFIELD,1),SIZE(ZFIELD,2)))
     ALLOCATE(ZD1(SIZE(ZFIELD,1),SIZE(ZFIELD,2)))
     ALLOCATE(ZOUT(SIZE(ZFIELD,1),SIZE(XGRID_SOIL)))
     ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_SOIL),SIZE(ZFIELD,3)))
     PFIELD(:,:,:) = XUNDEF
!
     DO JPATCH=1,SIZE(ZFIELD,3)
        ZFIELD1(:,:)=ZFIELD(:,:,JPATCH)
        ZD1    (:,:)=ZD    (:,:,JPATCH)
        CALL INTERP_GRID_NAT(ZD1,ZFIELD1,XGRID_SOIL,ZOUT)
        PFIELD(:,:,JPATCH)=ZOUT(:,:)
     END DO
     !
     DO JPATCH=1,SIZE(PFIELD,3)
       DO JL=1,SIZE(PFIELD,2)
         WHERE (ZMASK(:)==0.) PFIELD(:,JL,JPATCH) = XUNDEF
       ENDDO
     ENDDO
     !
     DEALLOCATE(ZFIELD)
     DEALLOCATE(ZOUT)
     DEALLOCATE(ZFIELD1)
     DEALLOCATE(ZD)
!
!--------------------------------------------------------------------------
!
!*      3.4    Water content intercepted on leaves, LAI
!
  CASE('WR     ')
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)     
     !* number of tiles
     CALL OPEN_AUX_IO_SURF(&
                       HFILEPGD,HFILEPGDTYPE,'NATURE')
     YRECFM='PATCH_NUMBER'
     CALL READ_SURF(&
                   HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     ALLOCATE(PFIELD(INI,1,IPATCH))
     PFIELD(:,:,:) = XUNDEF
     YRECFM = 'WR'
     CALL OPEN_AUX_IO_SURF(&
                       HFILE,HFILETYPE,'NATURE')
     CALL READ_SURF(&
                   HFILETYPE,YRECFM,PFIELD(:,1,:),IRESP,HDIR='A')
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
     DO JPATCH=1,SIZE(PFIELD,3)
       WHERE (ZMASK(:)==0.) PFIELD(:,1,JPATCH) = XUNDEF
     ENDDO     
!
  CASE('LAI    ')
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     !* number of tiles
     CALL OPEN_AUX_IO_SURF(&
                       HFILEPGD,HFILEPGDTYPE,'NATURE')
     YRECFM='PATCH_NUMBER'
     CALL READ_SURF(&
                   HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
     YRECFM='PHOTO'
     CALL READ_SURF(&
                   HFILEPGDTYPE,YRECFM,YPHOTO,IRESP)     
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     ALLOCATE(PFIELD(INI,1,IPATCH))
     PFIELD(:,:,:) = XUNDEF     
     IF (YPHOTO=='LAI' .OR. YPHOTO=='LST' .OR. YPHOTO=='NIT' .OR. YPHOTO=='NCB') THEN
       CALL OPEN_AUX_IO_SURF(&
                       HFILE,HFILETYPE,'NATURE')
       YRECFM = 'LAI'
       CALL READ_SURF(&
                   HFILETYPE,YRECFM,PFIELD(:,1,:),IRESP,HDIR='A')
       CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
       DO JPATCH=1,SIZE(PFIELD,3)
         WHERE (ZMASK(:)==0.) PFIELD(:,1,JPATCH) = XUNDEF
       ENDDO       
     ENDIF
!
  CASE('ICE_STO')
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)     
      !* number of tiles
     CALL OPEN_AUX_IO_SURF(&
                       HFILEPGD,HFILEPGDTYPE,'NATURE')
     YRECFM='PATCH_NUMBER'
     CALL READ_SURF(&
                   HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     CALL OPEN_AUX_IO_SURF(&
                       HFILE,HFILETYPE,'NATURE')
     YRECFM='GLACIER'
     CALL READ_SURF(&
                   HFILETYPE,YRECFM,GGLACIER,IRESP)
     ALLOCATE(PFIELD(INI,1,IPATCH))
     PFIELD(:,:,:) = 0.0     
     IF(GGLACIER)THEN
       YRECFM = 'ICE_STO'
       CALL READ_SURF(&
                   HFILETYPE,YRECFM,PFIELD(:,1,:),IRESP,HDIR='A')
     ENDIF
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
     DO JPATCH=1,SIZE(PFIELD,3)
       WHERE (ZMASK(:)==0.) PFIELD(:,1,JPATCH) = XUNDEF
     ENDDO     
!
  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_EXTERN: '//TRIM(HSURF)//" initialization not implemented !")
!
END SELECT
!
DEALLOCATE(ZMASK)
!
!---------------------------------------------------------------------------
!
!*      6.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_EXTERN
