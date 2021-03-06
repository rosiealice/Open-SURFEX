!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_CC_EXTERN (&
                                HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,PFIELD,OPREP_AGS)
!     #################################################################################
!
!!****  *PREP_ISBA_CC_EXTERN* - initializes ISBA-CC fields from external isba field
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
USE MODE_READ_EXTERN
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=8),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,             INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER   :: PFIELD    ! field to interpolate horizontally (on final soil grid)
LOGICAL,            INTENT(INOUT):: OPREP_AGS
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IRESP          ! reading return code
INTEGER           :: INI            ! total 1D dimension
INTEGER           :: IPATCH         ! number of patch
 CHARACTER(LEN=3)  :: YPHOTO
 CHARACTER(LEN=3)  :: YRESPSL
 CHARACTER(LEN=4)  :: YLVL
!
INTEGER           :: JNBIOMASS             ! loop counter
INTEGER           :: JNLITTER              ! loop counter
INTEGER           :: JNLITTLEVS            ! loop counter
INTEGER           :: JNSOILCARB            ! loop counter
INTEGER           :: IVERSION     ! surface version
INTEGER           :: IWORK,INBIOMASS,INLITTER, &
                     INLITTLEVS,INSOILCARB
!
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
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_CC_EXTERN',0,ZHOOK_HANDLE)
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
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
!
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!---------------------------------------------------------------------------------------
!
!*      3.     Transformation into physical quantity to be interpolated
!              --------------------------------------------------------
!
 CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'NATURE')
YRECFM='PHOTO'
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,YPHOTO,IRESP)  
YRECFM='PATCH_NUMBER'
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
IF(IVERSION<8.OR.(YPHOTO/='NIT'.AND.YPHOTO/='NCB'))THEN
  OPREP_AGS = .FALSE.
  IF (LHOOK) CALL DR_HOOK('PREP_ISBA_CC_EXTERN',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!       
SELECT CASE(HSURF)
!
  CASE('BIOMASS')
     CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'NATURE')
     YRECFM='NBIOMASS'
     CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,INBIOMASS,IRESP)
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     IF (YPHOTO=='NIT' .OR. YPHOTO=='NCB') THEN
       ALLOCATE(PFIELD(INI,INBIOMASS,IPATCH))
       PFIELD(:,:,:) = XUNDEF
       CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'NATURE')
       DO JNBIOMASS=1,INBIOMASS
          WRITE(YLVL,'(I1)') JNBIOMASS
          YRECFM='BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
          CALL READ_SURF(&
               HFILETYPE,YRECFM,PFIELD(:,JNBIOMASS,:),IRESP,HDIR='A')
       ENDDO
       CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
     ELSE
       OPREP_AGS = .FALSE.
     ENDIF
!
  CASE('LITTER')
     CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'NATURE')
     YRECFM='RESPSL'
     CALL READ_SURF(&
               HFILETYPE,YRECFM,YRESPSL,IRESP)
     IF(YRESPSL=='CNT')THEN
       YRECFM='NLITTER'
       CALL READ_SURF(&
               HFILETYPE,YRECFM,INLITTER,IRESP)
       YRECFM='NLITTLEVS'
       CALL READ_SURF(&
               HFILETYPE,YRECFM,INLITTLEVS,IRESP)
       ALLOCATE(PFIELD(INI,INLITTER*INLITTLEVS,IPATCH))
       PFIELD(:,:,:) = XUNDEF
       IWORK=0
       DO JNLITTER=1,INLITTER
          DO JNLITTLEVS=1,INLITTLEVS
             IWORK=IWORK+1
             WRITE(YLVL,'(I1,A1,I1)') JNLITTER,'_',JNLITTLEVS
             YRECFM='LITTER'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
             CALL READ_SURF(&
               HFILETYPE,YRECFM,PFIELD(:,IWORK,:),IRESP,HDIR='A')
          ENDDO
       ENDDO
     ELSE
       OPREP_AGS = .FALSE.
     ENDIF    
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
  CASE('SOILCARB')
     CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'NATURE')
     YRECFM='RESPSL'
     CALL READ_SURF(&
               HFILETYPE,YRECFM,YRESPSL,IRESP)
     IF(YRESPSL=='CNT')THEN
       YRECFM='NSOILCARB'
       CALL READ_SURF(&
               HFILETYPE,YRECFM,INSOILCARB,IRESP)
       ALLOCATE(PFIELD(INI,INSOILCARB,IPATCH))
       PFIELD(:,:,:) = XUNDEF
       DO JNSOILCARB=1,INSOILCARB
          WRITE(YLVL,'(I4)') JNSOILCARB
          YRECFM='SOILCARB'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
          CALL READ_SURF(&
               HFILETYPE,YRECFM,PFIELD(:,JNSOILCARB,:),IRESP,HDIR='A')
       ENDDO
     ELSE
       OPREP_AGS = .FALSE.
     ENDIF    
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!     
  CASE('LIGNIN')
     CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'NATURE')
     YRECFM='RESPSL'
     CALL READ_SURF(&
               HFILETYPE,YRECFM,YRESPSL,IRESP)
     IF(YRESPSL=='CNT')THEN
       YRECFM='NLITTLEVS'
       CALL READ_SURF(&
               HFILETYPE,YRECFM,INLITTLEVS,IRESP)
       ALLOCATE(PFIELD(INI,INLITTLEVS,IPATCH))
       PFIELD(:,:,:) = XUNDEF
       DO JNLITTLEVS=1,INLITTLEVS
          WRITE(YLVL,'(I4)') JNLITTLEVS
          YRECFM='LIGNIN_STR'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
          CALL READ_SURF(&
               HFILETYPE,YRECFM,PFIELD(:,JNLITTLEVS,:),IRESP,HDIR='A')
       ENDDO
     ELSE
       OPREP_AGS = .FALSE.
     ENDIF    
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
END SELECT
!
!
!---------------------------------------------------------------------------
!
!*      6.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_CC_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_CC_EXTERN
