!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########
      PROGRAM PGD
!     ###########
!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!   
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    F. Mereyde                  Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     21/07/95
!!    Modification 26/07/95       Treatment of orography and subgrid-scale
!!                                orography roughness length (V. Masson)
!!    Modification 22/05/96       Variable CSTORAGE_TYPE (V. Masson)
!!    Modification 25/05/96       Modification of splines, correction on z0rel
!!                                and set limits for some surface varaibles
!!    Modification 12/06/96       Treatment of a rare case for ZPGDZ0EFF (Masson)
!!    Modification 22/11/96       removes the filtering. It will have to be 
!!                                performed in ADVANCED_PREP_PGD (Masson)
!!    Modification 15/03/99       **** MAJOR MODIFICATION **** (Masson)
!!                                PGD fields are now defined from the cover
!!                                type fractions in the grid meshes
!!                                User can still include its own data, and
!!                                even additional (dummy) fields
!!    Modificatio 06/00           patch approach, for vegetation related variable (Solmon/Masson)
!                                  averaging is performed on subclass(=patch) of nature
!!                08/03/01        add chemical emission treatment (D.Gazen)
!!    Modification 15/10/01       allow namelists in different orders (I.Mallet)
!!    Modification    07/11       new routine write_pgd_surf_atmn.F90 for writing PGD field (B.Decharme)
!!                                flag_update now in write_pgd_surf_atmn.F90 (B.Decharme)
!!
!!
!!                   ################################
!!    13/10/03       EXTERNALIZED VERSION (V. Masson)
!!                   ################################
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_OMP, ONLY : NWORK, NWORK2, XWORK, XWORK2, XWORK3, &
                            NWORK_FULL, NWORK2_FULL, XWORK_FULL, XWORK2_FULL
!
USE MODD_IO_SURF_ASC
USE MODD_IO_SURF_FA
USE MODD_IO_SURF_LFI
USE MODD_IO_SURF_NC
USE MODD_SURF_CONF
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!      
USE MODI_GET_LONLAT_n
!
USE MODI_IO_BUFF_CLEAN
USE MODI_PGD_OROG_FILTER
USE MODI_PGD_SURF_ATM
USE MODI_PGD_GRID_SURF_ATM
USE MODI_SPLIT_GRID
USE MODI_WRITE_HEADER_FA
USE MODI_WRITE_HEADER_MNH
USE MODI_WRITE_PGD_SURF_ATM_n
USE MODI_INIT_OUTPUT_NC_n
!
USE MODE_POS_SURF
!
USE MODN_IO_OFFLINE
USE MODN_WRITE_SURF_ATM
!
USE MODD_IO_SURF_FA, ONLY : LFANOCOMPACT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
!
USE MODD_OFF_SURFEX_n
!
IMPLICIT NONE
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER            :: ILUOUT
INTEGER            :: ILUNAM
LOGICAL            :: GFOUND
!
 CHARACTER(LEN=28)  :: YLUOUT    ='LISTING_PGD'   ! name of the listing
!
INTEGER            :: INW, JNW
INTEGER            :: IRET      
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD',0,ZHOOK_HANDLE)
!
 CALL SURFEX_ALLOC_LIST(1)
 CSOFTWARE='PGD    '
 CALL GOTO_MODEL(1)
!
!*    1.      Set default names and parallelized I/O
!             --------------------------------------
!
 CALL GET_LUOUT('ASCII ',ILUOUT)
 CLUOUT_LFI =  ADJUSTL(ADJUSTR(YLUOUT)//'.txt')
OPEN(UNIT=ILUOUT,FILE=ADJUSTL(ADJUSTR(YLUOUT)//'.txt'),FORM='FORMATTED',ACTION='WRITE')
!
!     1.3     output file name read in namelist
!             ---------------------------------
 CALL OPEN_NAMELIST('ASCII ',ILUNAM,CNAMELIST)
 CALL POSNAM(ILUNAM,'NAM_IO_OFFLINE',GFOUND)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_IO_OFFLINE)
 CALL POSNAM(ILUNAM,'NAM_WRITE_SURF_ATM',GFOUND)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_WRITE_SURF_ATM)
 CALL CLOSE_NAMELIST('ASCII ',ILUNAM)
!
 CFILEOUT     = ADJUSTL(ADJUSTR(CPGDFILE)//'.txt')      ! output of PGD program
 CFILEOUT_FA  = ADJUSTL(ADJUSTR(CPGDFILE)//'.fa')
 CFILEOUT_LFI = CPGDFILE
 CFILEOUT_NC  = ADJUSTL(ADJUSTR(CPGDFILE)//'.nc')
!
!*    2.      Preparation of surface physiographic fields
!             -------------------------------------------
!
 CALL PGD_GRID_SURF_ATM(YSURF_CUR%UG, YSURF_CUR%U,&
                CSURF_FILETYPE,'                            ','      ',.FALSE.)
!
 CALL SPLIT_GRID(YSURF_CUR%UG, YSURF_CUR%U,&
                 'OFFLIN')
!
 CALL PGD_SURF_ATM(YSURF_CUR,&
                   CSURF_FILETYPE,'                            ','      ',.FALSE.)
!
 CALL PGD_OROG_FILTER(YSURF_CUR%U,&
                      CSURF_FILETYPE)
!
!*    3.      writing of surface physiographic fields
!             ---------------------------------------
!
!* building of the header for the opening of the file in case of Arpege file
IF (CSURF_FILETYPE=='FA    ') THEN
  LFANOCOMPACT = .TRUE.
  CALL WRITE_HEADER_FA(YSURF_CUR%UG, &
                       CSURF_FILETYPE,'PGD') 
END IF
!
LDEF = .TRUE.
!
IF (CSURF_FILETYPE=="NC    ") THEN
  CALL INIT_OUTPUT_NC_n (YSURF_CUR%TM%BDD, YSURF_CUR%CHE, YSURF_CUR%CHN, YSURF_CUR%CHU, &
                         YSURF_CUR%SM%DTS, YSURF_CUR%TM%DTT, YSURF_CUR%DTZ, YSURF_CUR%IM%I, &
                         YSURF_CUR%UG, YSURF_CUR%U, YSURF_CUR%DGU)
ENDIF
!
INW = 1
IF (CSURF_FILETYPE=="NC    ") INW = 2
!
DO JNW = 1,INW
  !
  IF (LWRITE_COORD) CALL GET_LONLAT_n(YSURF_CUR, &
                                      CSURF_FILETYPE)
  !
  !* writing of the fields
 CALL IO_BUFF_CLEAN
  
  ! FLAG_UPDATE now in WRITE_PGD_SURF_ATM_n
  CALL WRITE_PGD_SURF_ATM_n(YSURF_CUR, &
                            CSURF_FILETYPE)
  !
  LDEF = .FALSE.
  CALL IO_BUFF_CLEAN  
  !
ENDDO
!
!* closes the file
IF (CSURF_FILETYPE=='FA    ') THEN
#ifdef SFX_FA
  CALL FAIRME(IRET,NUNIT_FA,'UNKNOWN')
#endif
END IF
!
!* add informations in the file
IF (CSURF_FILETYPE=='LFI   ' .AND. LMNH_COMPATIBLE) CALL WRITE_HEADER_MNH
!
!*    3.     Close parallelized I/O
!            ----------------------
!
WRITE(ILUOUT,*) ' '
WRITE(ILUOUT,*) '    ----------------------'
WRITE(ILUOUT,*) '    | PGD ENDS CORRECTLY |'
WRITE(ILUOUT,*) '    ----------------------'
!
WRITE(*,*) ' '
WRITE(*,*) '    ----------------------'
WRITE(*,*) '    | PGD ENDS CORRECTLY |'
WRITE(*,*) '    ----------------------'
      !
 CLOSE(ILUOUT)
 CALL SURFEX_DEALLO_LIST
!
IF (ASSOCIATED(NWORK)) DEALLOCATE(NWORK)
IF (ASSOCIATED(XWORK)) DEALLOCATE(XWORK)
IF (ASSOCIATED(NWORK2)) DEALLOCATE(NWORK2)
IF (ASSOCIATED(XWORK2)) DEALLOCATE(XWORK2)
IF (ASSOCIATED(XWORK3)) DEALLOCATE(XWORK3)
IF (ASSOCIATED(NWORK_FULL)) DEALLOCATE(NWORK_FULL)
IF (ASSOCIATED(XWORK_FULL)) DEALLOCATE(XWORK_FULL)
IF (ASSOCIATED(NWORK2_FULL)) DEALLOCATE(NWORK2_FULL)
IF (ASSOCIATED(XWORK2_FULL)) DEALLOCATE(XWORK2_FULL)
!
IF (LHOOK) CALL DR_HOOK('PGD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END PROGRAM PGD
