!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_READ_SURF_OL
!
!!    PURPOSE
!!    -------
!
!       The purpose of READ_SURF_OL is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!     
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      F. Habets      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
INTERFACE READ_SURF0_OL
      SUBROUTINE READ_SURFX0_OL(HREC,PFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL,               INTENT(OUT) :: PFIELD   ! the real scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFX0_OL
      SUBROUTINE READ_SURFN0_OL(HREC,KFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT) :: KFIELD   ! the integer scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFN0_OL
      SUBROUTINE READ_SURFC0_OL(HREC,HFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be read
 CHARACTER(LEN=40),   INTENT(OUT) :: HFIELD   ! the integer scalar to be read
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFC0_OL
      SUBROUTINE READ_SURFL0_OL(HREC,OFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL,                  INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFL0_OL
END INTERFACE
INTERFACE READ_SURFN_OL
      SUBROUTINE READ_SURFX1_OL(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),   INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFX1_OL
      SUBROUTINE READ_SURFX2_OL(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:),     INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFX2_OL
      SUBROUTINE READ_SURFX3_OL(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFX3_OL
      SUBROUTINE READ_SURFN1_OL(HREC,KFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:), INTENT(OUT) :: KFIELD   ! the integer scalar to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFN1_OL
      SUBROUTINE READ_SURFL1_OL(HREC,OFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL, DIMENSION(:),   INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFL1_OL
END INTERFACE
INTERFACE READ_SURFT_OL
      SUBROUTINE READ_SURFT0_OL(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                  INTENT(OUT) :: KYEAR    ! year
INTEGER,                  INTENT(OUT) :: KMONTH   ! month
INTEGER,                  INTENT(OUT) :: KDAY     ! day
REAL,                     INTENT(OUT) :: PTIME    ! time
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFT0_OL
END INTERFACE
!
END MODULE MODE_READ_SURF_OL
!
!     #############################################################
      SUBROUTINE READ_SURFX0_OL(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READX0* - routine to read a real scalar
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL,               INTENT(OUT) :: PFIELD   ! the real scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
REAL*4 :: ZFIELD
 CHARACTER(LEN=100) :: YFILE          ! filename
INTEGER            :: IVAR_ID,IFILE_ID,JRET,IVAL,ITYPE,INDIMS
INTEGER,DIMENSION(4) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFX0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
! 0. find filename
! -----------------
 CALL OL_FIND_FILE_READ(HREC,IFILE_ID)
!
IF (IFILE_ID.NE.0) THEN
  !       
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
  IRET(1)=NF_INQ_VARTYPE (IFILE_ID,IVAR_ID,ITYPE)
  IRET(1)=NF_INQ_VARNDIMS(IFILE_ID,IVAR_ID,INDIMS)
  !  
  ! 2. Get variable
  !----------------------------
  IF (ITYPE==NF_DOUBLE) THEN
    IRET(2)=NF_GET_VAR_DOUBLE(IFILE_ID,IVAR_ID,PFIELD)
  ELSEIF (ITYPE==NF_FLOAT) THEN
    IRET(2)=NF_GET_VAR_REAL(IFILE_ID,IVAR_ID,ZFIELD)
    PFIELD = ZFIELD
  ENDIF
  !  
ENDIF
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF ((PFIELD==XUNDEF).OR.(IFILE_ID==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    PFIELD=XUNDEF
    KRESP=1
  ENDIF
ENDDO
!     
IF (KRESP /=0) CALL ERROR_READ_SURF_OL(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFX0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX0_OL
!
!     #############################################################
      SUBROUTINE READ_SURFX1_OL(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX1* - routine to fill a real 1D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_READ
!
USE MODD_SURFEX_OMP, ONLY : XWORKD, NWORKB
!
USE MODD_IO_SURF_OL, ONLY: LMASK,NMASK,XSTART,XCOUNT,XSTRIDE,LPARTR
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),   INTENT(IN)  :: HDIR     ! type of field :
                                            ! 'H' : field with
                                            !       horizontal spatial dim.
                                            ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YFILE,YOUT          ! Filename
INTEGER :: IVAR_ID,IFILE_ID,JRET,JDIM,INDIMS, ITYPE
INTEGER,DIMENSION(2) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(2) :: IRET
!
INTEGER,DIMENSION(:),ALLOCATABLE :: ISTART,ICOUNT,ISTRIDE
REAL*4, DIMENSION(:), ALLOCATABLE :: ZTAB_1D4
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFX1_OL',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
NWORKB=0
!$OMP END SINGLE
!
HCOMMENT = " "
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !  
  ! 0. find filename
  ! -----------------
  CALL OL_FIND_FILE_READ(HREC,IFILE_ID)
  ! 
  IF (IFILE_ID.NE.0) THEN
    !  
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
    IRET(1)=NF_INQ_VARTYPE (IFILE_ID,IVAR_ID,ITYPE)
    IRET(1)=NF_INQ_VARNDIMS(IFILE_ID,IVAR_ID,INDIMS)
    IRET(1)=NF_INQ_VARDIMID(IFILE_ID,IVAR_ID,IDIMIDS(1:INDIMS))
    IDIMLEN(:) = 1.
    DO JDIM=1,INDIMS
      JRET=NF_INQ_DIMLEN(IFILE_ID,IDIMIDS(JDIM),IDIMLEN(JDIM))
    ENDDO
    ALLOCATE(XWORKD(IDIMLEN(1)*IDIMLEN(2)))
    !
    ! 2. Get variable
    !----------------------------
    IF  (LPARTR) THEN
      ! write partially a time-matrix. 
      ! Have to find which of the dimension is the time dimension
      ALLOCATE(ISTART(INDIMS))
      ALLOCATE(ICOUNT(INDIMS))
      ALLOCATE(ISTRIDE(INDIMS))
      DO  JDIM=1,INDIMS
        IRET=NF_INQ_DIMNAME(IFILE_ID,IDIMIDS(JDIM),YOUT)
        IF ((INDEX(YOUT,'time') > 0).OR.(INDEX(YOUT,'TIME') >0) &
          .OR.(INDEX(YOUT,'Time')>0.)) THEN  
          ISTART(JDIM)=XSTART
          ICOUNT(JDIM)=XCOUNT
          ISTRIDE(JDIM)=XSTRIDE
        ELSE
          ISTART(JDIM)=1
          ICOUNT(JDIM)=IDIMLEN(JDIM)
          ISTRIDE(JDIM)=1
        ENDIF
      ENDDO

      IF (ITYPE==NF_DOUBLE) THEN
        IRET(1)=NF_GET_VARS_DOUBLE(IFILE_ID,IVAR_ID,ISTART,ICOUNT,ISTRIDE,XWORKD)
      ELSEIF (ITYPE==NF_FLOAT) THEN
        ALLOCATE(ZTAB_1D4(IDIMLEN(1)*IDIMLEN(2)))
        IRET(1)=NF_GET_VARS_REAL(IFILE_ID,IVAR_ID,ISTART,ICOUNT,ISTRIDE,ZTAB_1D4)
        XWORKD(:) = ZTAB_1D4(:)
        DEALLOCATE(ZTAB_1D4)
      ENDIF

      DEALLOCATE(ISTART)
      DEALLOCATE(ICOUNT)
      DEALLOCATE(ISTRIDE)

    ELSE
      IF (ITYPE==NF_DOUBLE) THEN
        IRET(1)=NF_GET_VAR_DOUBLE(IFILE_ID,IVAR_ID,XWORKD)
      ELSEIF (ITYPE==NF_FLOAT) THEN
        ALLOCATE(ZTAB_1D4(IDIMLEN(1)*IDIMLEN(2)))
        IRET(1)=NF_GET_VAR_REAL(IFILE_ID,IVAR_ID,ZTAB_1D4)
        XWORKD(:) = ZTAB_1D4(:)
        DEALLOCATE(ZTAB_1D4)
      ENDIF            
    ENDIF
    !
  ENDIF
  !
  ! 3. Check for errors
  !--------------------
  DO JRET=1,1
    IF ((IFILE_ID==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      XWORKD = XUNDEF
      NWORKB=1
    ELSE
      IF (MINVAL(XWORKD)==XUNDEF) THEN 
        NWORKB = 1
        XWORKD = XUNDEF
     ENDIF
    ENDIF
  ENDDO
  !
!$OMP END SINGLE
  !
  IF (NWORKB /=0) CALL ERROR_READ_SURF_OL(HREC,NWORKB)
  !
ELSE
!$OMP SINGLE
  ALLOCATE(XWORKD(0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (LMASK) THEN
  CALL READ_AND_SEND_MPI(XWORKD,PFIELD,NMASK)
ELSE 
  CALL READ_AND_SEND_MPI(XWORKD,PFIELD)
END IF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(XWORKD)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFX1_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX1_OL
!
!     #############################################################
      SUBROUTINE READ_SURFX2_OL(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX2* - routine to fill a real 2D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY: NRANK, NPIO, XTIME_NPIO_READ
!
USE MODD_SURFEX_OMP, ONLY : XWORKD2, NWORKB
!
USE MODD_IO_SURF_OL, ONLY: LMASK,NMASK,XSTART,XCOUNT,XSTRIDE,LPARTR
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:),     INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
                                                  ! 'H' : field with
                                                  !       horizontal spatial dim.
                                                  ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YFILE,YOUT          ! filename
INTEGER            :: IVAR_ID,IFILE_ID,JRET,JDIM,INDIMS,ITYPE
INTEGER,DIMENSION(3) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(2) :: IRET
INTEGER, DIMENSION(:), ALLOCATABLE :: ISTART,ISTRIDE,ICOUNT
REAL*4, DIMENSION(:,:), ALLOCATABLE :: ZTAB_2D4
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFX2_OL',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
NWORKB=0
!$OMP END SINGLE
!
HCOMMENT = " "
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !  
  ! 0. find filename
  ! -----------------
  CALL OL_FIND_FILE_READ(HREC,IFILE_ID)
  ! 
  IF (IFILE_ID.NE.0) THEN
    !   
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
    IRET(1)=NF_INQ_VARTYPE (IFILE_ID,IVAR_ID,ITYPE)
    IRET(1)=NF_INQ_VARNDIMS(IFILE_ID,IVAR_ID,INDIMS)
    IRET(1)=NF_INQ_VARDIMID(IFILE_ID,IVAR_ID,IDIMIDS(1:INDIMS))
    IDIMLEN(:) = 1.
    DO JDIM=1,INDIMS
      JRET=NF_INQ_DIMLEN(IFILE_ID,IDIMIDS(JDIM),IDIMLEN(JDIM))
    ENDDO
    ! 
    ! 2. Get variable
    !----------------------------
    IF (LPARTR) THEN
      ! write partially a time-matrix. 
      ! Have to find which of the dimension is the time dimension
      ALLOCATE(ISTART(INDIMS))
      ALLOCATE(ICOUNT(INDIMS))
      ICOUNT(:) = 1.
      ALLOCATE(ISTRIDE(INDIMS))
      DO JDIM=1,INDIMS
        IRET=NF_INQ_DIMNAME(IFILE_ID,IDIMIDS(JDIM),YOUT)
        IF ((INDEX(YOUT,'time') > 0).OR.(INDEX(YOUT,'TIME') >0) &
          .OR.(INDEX(YOUT,'Time')>0.)) THEN  
          ISTART(JDIM)=XSTART
          ICOUNT(JDIM)=XCOUNT
          ISTRIDE(JDIM)=XSTRIDE
        ELSE
          ISTART(JDIM)=1
          ICOUNT(JDIM)=IDIMLEN(JDIM)
          ISTRIDE(JDIM)=1
        ENDIF
      ENDDO

      ALLOCATE(XWORKD2(PRODUCT(ICOUNT(1:INDIMS-1)),ICOUNT(INDIMS)))
      IF (ITYPE==NF_DOUBLE) THEN
        IRET(2)=NF_GET_VARS_DOUBLE(IFILE_ID,IVAR_ID,ISTART,ICOUNT,ISTRIDE,XWORKD2)
      ELSEIF (ITYPE==NF_FLOAT) THEN
        ALLOCATE(ZTAB_2D4(PRODUCT(ICOUNT(1:INDIMS-1)),ICOUNT(INDIMS)))
        IRET(2)=NF_GET_VARS_REAL(IFILE_ID,IVAR_ID,ISTART,ICOUNT,ISTRIDE,ZTAB_2D4)
        XWORKD2(:,:) = ZTAB_2D4(:,:)
        DEALLOCATE(ZTAB_2D4)
      ENDIF
      DEALLOCATE(ISTART)
      DEALLOCATE(ICOUNT)
      DEALLOCATE(ISTRIDE)

    ELSE
      ALLOCATE(XWORKD2(PRODUCT(IDIMLEN(1:INDIMS-1)),IDIMLEN(INDIMS)))
      IF (ITYPE==NF_DOUBLE) THEN
        IRET(2)=NF_GET_VAR_DOUBLE(IFILE_ID,IVAR_ID,XWORKD2)
      ELSEIF (ITYPE==NF_FLOAT) THEN
        ALLOCATE(ZTAB_2D4(PRODUCT(IDIMLEN(1:INDIMS-1)),IDIMLEN(INDIMS)))
        IRET(2)=NF_GET_VAR_REAL(IFILE_ID,IVAR_ID,ZTAB_2D4)
        XWORKD2(:,:) = ZTAB_2D4(:,:)
        DEALLOCATE(ZTAB_2D4)
      ENDIF      
    ENDIF

  ENDIF

  ! 3. Check for errors
  !--------------------
  DO JRET=1,2
    IF ((IFILE_ID==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      XWORKD2 = XUNDEF
      NWORKB=1
    ELSE
      IF (MINVAL(XWORKD2)==XUNDEF) THEN 
        NWORKB=1
        XWORKD2 = XUNDEF
      ENDIF
    ENDIF
  ENDDO
  !
!$OMP END SINGLE
  !
  IF (NWORKB /=0) CALL ERROR_READ_SURF_OL(HREC,NWORKB)
  !
ELSE
!$OMP SINGLE
  ALLOCATE(XWORKD2(0,0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (LMASK) THEN
  CALL READ_AND_SEND_MPI(XWORKD2,PFIELD,NMASK)
ELSE 
  CALL READ_AND_SEND_MPI(XWORKD2,PFIELD)
END IF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(XWORKD2) 
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFX2_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX2_OL
!
!     #############################################################
      SUBROUTINE READ_SURFX3_OL(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX3* - routine to fill a real 2D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY: NRANK, NPIO, XTIME_NPIO_READ
!
USE MODD_SURFEX_OMP, ONLY: XWORKD3, NWORKB
!
USE MODD_IO_SURF_OL, ONLY: LMASK,NMASK,XSTART,XCOUNT,XSTRIDE,LPARTR
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
                                                  ! 'H' : field with
                                                  !       horizontal spatial dim.
                                                  ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YFILE,YOUT          ! filename
INTEGER :: IVAR_ID,IFILE_ID,JRET,JDIM,INDIMS,ITYPE
INTEGER,DIMENSION(3) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(2) :: IRET
INTEGER, DIMENSION(:), ALLOCATABLE :: ISTART,ISTRIDE,ICOUNT
REAL*4, DIMENSION(:,:,:), ALLOCATABLE :: ZTAB_3D4
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFX3_OL',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
NWORKB=0
!$OMP END SINGLE
!
HCOMMENT = " "
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !  
  ! 0. find filename
  ! -----------------
  CALL OL_FIND_FILE_READ(HREC,IFILE_ID)
  ! 
  IF (IFILE_ID.NE.0) THEN
    !      
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
    IRET(1)=NF_INQ_VARTYPE (IFILE_ID,IVAR_ID,ITYPE)
    IRET(1)=NF_INQ_VARNDIMS(IFILE_ID,IVAR_ID,INDIMS)
    IRET(1)=NF_INQ_VARDIMID(IFILE_ID,IVAR_ID,IDIMIDS(1:INDIMS))
    DO JDIM=1,INDIMS
      JRET=NF_INQ_DIMLEN(IFILE_ID,IDIMIDS(JDIM),IDIMLEN(JDIM))
    ENDDO
    ! 
    ! 2. Get variable
    !----------------------------
    IF (LPARTR) THEN
      ! write partially a time-matrix. 
      ! Have to find which of the dimension is the time dimension
      ALLOCATE(ISTART(INDIMS))
      ALLOCATE(ICOUNT(INDIMS))
      ALLOCATE(ISTRIDE(INDIMS))
      DO  JDIM=1,INDIMS
        IRET=NF_INQ_DIMNAME(IFILE_ID,IDIMIDS(JDIM),YOUT)
        IF ((INDEX(YOUT,'time') > 0).OR.(INDEX(YOUT,'TIME') >0) &
            .OR.(INDEX(YOUT,'Time')>0.)) THEN  
          ISTART(JDIM)=XSTART
          ICOUNT(JDIM)=XCOUNT
          ISTRIDE(JDIM)=XSTRIDE
        ELSE
          ISTART(JDIM)=1
          ICOUNT(JDIM)=IDIMLEN(JDIM)
          ISTRIDE(JDIM)=1
        ENDIF
      ENDDO

      ALLOCATE(XWORKD3(ICOUNT(1),ICOUNT(2),ICOUNT(3)))

      IF (ITYPE==NF_DOUBLE) THEN
        IRET(2)=NF_GET_VARS_DOUBLE(IFILE_ID,IVAR_ID,ISTART,ICOUNT,ISTRIDE,XWORKD3)
      ELSEIF (ITYPE==NF_FLOAT) THEN
        ALLOCATE(ZTAB_3D4(ICOUNT(1),ICOUNT(2),ICOUNT(3)))
        IRET(2)=NF_GET_VARS_REAL(IFILE_ID,IVAR_ID,ISTART,ICOUNT,ISTRIDE,ZTAB_3D4)
        XWORKD3(:,:,:) = ZTAB_3D4(:,:,:)
        DEALLOCATE(ZTAB_3D4)
      ENDIF
      DEALLOCATE(ISTART)
      DEALLOCATE(ICOUNT)
      DEALLOCATE(ISTRIDE)
      !
    ELSE
      ALLOCATE(XWORKD3(IDIMLEN(1),IDIMLEN(2),IDIMLEN(3)))
      IF (ITYPE==NF_DOUBLE) THEN
        IRET(2)=NF_GET_VAR_DOUBLE(IFILE_ID,IVAR_ID,XWORKD3)
      ELSEIF (ITYPE==NF_FLOAT) THEN
        ALLOCATE(ZTAB_3D4(ICOUNT(1),ICOUNT(2),ICOUNT(3)))
        IRET(2)=NF_GET_VAR_REAL(IFILE_ID,IVAR_ID,ZTAB_3D4)
        XWORKD3(:,:,:) = ZTAB_3D4(:,:,:)
        DEALLOCATE(ZTAB_3D4)
      ENDIF      
    ENDIF
    !
  ENDIF
  !
  ! 3. Check for errors
  !--------------------
  DO JRET=1,2
    IF ((IFILE_ID==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      XWORKD3 = XUNDEF
      NWORKB = 1
    ELSE
      IF (MINVAL(XWORKD3)==XUNDEF) THEN 
        NWORKB = 1
        XWORKD3 = XUNDEF
      ENDIF
    ENDIF
  ENDDO
  !
!$OMP END SINGLE
  !
  IF (NWORKB /=0) CALL ERROR_READ_SURF_OL(HREC,NWORKB)
  !
ELSE
!$OMP SINGLE
  ALLOCATE(XWORKD3(0,0,0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (LMASK) THEN
  CALL READ_AND_SEND_MPI(XWORKD3,PFIELD,NMASK)
ELSE 
  CALL READ_AND_SEND_MPI(XWORKD3,PFIELD)
END IF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(XWORKD3)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFX3_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX3_OL
!
!     #############################################################
      SUBROUTINE READ_SURFN0_OL(HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
USE MODD_SURF_PAR,   ONLY: NUNDEF
!
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT) :: KFIELD   ! the integer scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100):: YFILE          ! filename
INTEGER :: IVAR_ID,IFILE_ID,JRET,JDIM,INDIMS
INTEGER,DIMENSION(4) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFN0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
! 0. find filename
! -----------------
 CALL OL_FIND_FILE_READ(HREC,IFILE_ID)
!
IF (IFILE_ID.NE.0) THEN
  !        
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
  !  
  ! 2. Get variable
  !----------------------------
  IRET(2)=NF_GET_VAR_INT(IFILE_ID,IVAR_ID,KFIELD)
  !  
ENDIF
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF ((KFIELD==NUNDEF).OR.(IFILE_ID==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    KFIELD=NUNDEF
    KRESP=1
  ENDIF
ENDDO
!
IF (KRESP /=0)  CALL ERROR_READ_SURF_OL(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFN0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN0_OL
!
!     #############################################################
      SUBROUTINE READ_SURFN1_OL(HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!RJ: missing interface, assumed shape on callee
USE MODI_READ_SURFX1_OL
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:), INTENT(OUT) :: KFIELD   ! the integer scalar to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
                                                ! 'H' : field with
                                                !       horizontal spatial dim.
                                                ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(KFIELD)) :: ZFIELD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFN1_OL',0,ZHOOK_HANDLE)
!
 CALL READ_SURFX1_OL(HREC,ZFIELD,KRESP,HCOMMENT,HDIR)
KFIELD = NINT(ZFIELD)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFN1_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN1_OL
!
!     #############################################################
      SUBROUTINE READ_SURFC0_OL(HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READC0* - routine to read a STRING
!
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be read
 CHARACTER(LEN=40),   INTENT(OUT) :: HFIELD   ! the integer scalar to be read
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100):: YFILE          ! filename
 CHARACTER(LEN=100):: YFIELD   
INTEGER :: IVAR_ID,IFILE_ID,JRET,JDIM,INDIMS
INTEGER,DIMENSION(4) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFC0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
! 0. find filename
! -----------------
 CALL OL_FIND_FILE_READ(HREC,IFILE_ID)
!
IF (IFILE_ID.NE.0) THEN
  !       
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
  !  
  ! 2. Get variable
  !----------------------------
  IRET(2)=NF_GET_VAR_TEXT(IFILE_ID,IVAR_ID,YFIELD)
  HFIELD=YFIELD(:LEN_TRIM(YFIELD))
  !  
ENDIF

! 3. Check for errors
!--------------------
DO JRET=1,2
  IF ((IFILE_ID==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    KRESP=1
  ENDIF
ENDDO  
!
IF (KRESP /=0) CALL ERROR_READ_SURF_OL(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFC0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFC0_OL
!
!     #############################################################
      SUBROUTINE READ_SURFL0_OL(HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READL0* - routine to read a logical
!    
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL,                  INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=1)   :: YFIELD   ! work array read in the file
 CHARACTER(LEN=100) :: YFILE    ! Filename
INTEGER :: IVAR_ID,IFILE_ID, JRET
INTEGER,DIMENSION(2) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFL0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
! 0. find filename
! -----------------
 CALL OL_FIND_FILE_READ(HREC,IFILE_ID)
!
IF (IFILE_ID.NE.0) THEN
  !       
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
  !  
  ! 2. Get variable
  !----------------------------
  IRET(2)=NF_GET_VAR_TEXT(IFILE_ID,IVAR_ID,YFIELD)
  !  
  IF (YFIELD =='T') OFIELD=.TRUE.
  IF (YFIELD =='F') OFIELD=.FALSE.
  !
ENDIF
!
! 3. Check for errors
!--------------------
IF ((IFILE_ID==0).OR.IRET(1).NE.NF_NOERR) THEN 
  KRESP=1
ENDIF
!
IF (KRESP /=0)  CALL ERROR_READ_SURF_OL(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFL0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL0_OL
!
!     #############################################################
      SUBROUTINE READ_SURFL1_OL(HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READL1* - routine to read a logical array
!    
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ
!
USE MODD_SURFEX_OMP, ONLY : LWORKD, NWORKB
!
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL, DIMENSION(:),   INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
                                                  ! 'H' : field with
                                                  !       horizontal spatial dim.
                                                  ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YFILE          ! Filename
 CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE :: YTAB_1D  ! work array read in the file
!
INTEGER :: IVAR_ID,IFILE_ID,JRET,JDIM,INDIMS
INTEGER :: INFOMPI
INTEGER,DIMENSION(1) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(2) :: IRET
INTEGER, DIMENSION(:),    POINTER     :: IMASK    ! 1D mask to read only interesting
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFL1_OL',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
NWORKB=0
!$OMP END SINGLE
!
HCOMMENT = " "
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
!$OMP SINGLE
ALLOCATE(LWORKD(SIZE(OFIELD)))
!$OMP END SINGLE
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !
  ! 0. find filename
  ! -----------------
  CALL OL_FIND_FILE_READ(HREC,IFILE_ID)
  ! 
  IF (IFILE_ID.NE.0) THEN
    !   
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
    IRET(1)=NF_INQ_VARNDIMS(IFILE_ID,IVAR_ID,INDIMS)
    IRET(1)=NF_INQ_VARDIMID(IFILE_ID,IVAR_ID,IDIMIDS)
    DO JDIM=1,INDIMS
      JRET=NF_INQ_DIMLEN(IFILE_ID,IDIMIDS(JDIM),IDIMLEN(JDIM))
    ENDDO
    ALLOCATE(YTAB_1D(IDIMLEN(1)))
    !  
    ! 2. Get variable
    !----------------------------
    IRET(1)=NF_GET_VAR_TEXT(IFILE_ID,IVAR_ID,YTAB_1D)
    !
    DO JRET=1,IDIMLEN(1)
      IF (YTAB_1D(JRET) =='T') LWORKD(JRET)=.TRUE.
      IF (YTAB_1D(JRET) =='F') LWORKD(JRET)=.FALSE.
    ENDDO
    !
  ENDIF
  !
  ! 3. Check for errors
  !--------------------
  DO JRET=1,1
    IF ((IFILE_ID==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      NWORKB=1
    ENDIF
  ENDDO
  !
  DEALLOCATE(YTAB_1D)
  !
!$OMP END SINGLE
  !  
  IF (NWORKB /=0) CALL ERROR_READ_SURF_OL(HREC,NWORKB)
  !
ENDIF
!
KRESP = NWORKB
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
!$OMP SINGLE  
  CALL MPI_BCAST(LWORKD,SIZE(LWORKD),MPI_LOGICAL,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
  XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
ENDIF
!
OFIELD = LWORKD
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFL1_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL1_OL
!
!
!     #############################################################
      SUBROUTINE READ_SURFT0_OL(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a NETCDF  date_time scalar
!
USE MODI_OL_FIND_FILE_READ
USE MODI_ERROR_READ_SURF_OL
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INCLUDE "netcdf.inc"
!
!*      0.1   Declarations of arguments
!
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                  INTENT(OUT) :: KYEAR    ! year
INTEGER,                  INTENT(OUT) :: KMONTH   ! month
INTEGER,                  INTENT(OUT) :: KDAY     ! day
REAL,                     INTENT(OUT) :: PTIME    ! time
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment

!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=18)  :: YRECFM    ! Name of the article to be written
 CHARACTER(LEN=100) :: YFILE          ! Filename
INTEGER :: IVAR_ID,IFILE_ID,JRET,JDIM,INDIMS,JWRK
INTEGER, DIMENSION(1) :: IDIMIDS,IDIMLEN
INTEGER, DIMENSION(4) :: IRET
INTEGER, DIMENSION(3) :: ITDATE  ! work array read in the file
INTEGER, DIMENSION(:), POINTER :: IMASK    ! 1D mask to read only interesting
REAL:: ZTIME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFT0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
DO JWRK=1,2
  IF (JWRK == 1) THEN 
    YRECFM=TRIM(HREC)//'-TDATE'
  ELSE
    YRECFM=TRIM(HREC)//'-TIME'
  ENDIF
! 0. find filename
! -----------------
  CALL OL_FIND_FILE_READ(YRECFM,IFILE_ID)
  !
  IF (IFILE_ID.NE.0) THEN
    !   
    ! 1. Find id of the variable
    !----------------------------
    JRET=NF_INQ_VARID   (IFILE_ID,YRECFM,IVAR_ID)
    !
    ! 2. Get variable
    !----------------------------
    IF (JWRK == 1) THEN 
      IRET(JWRK)=NF_GET_VAR_INT(IFILE_ID,IVAR_ID,ITDATE)
      KYEAR  = ITDATE(1)
      KMONTH = ITDATE(2)
      KDAY   = ITDATE(3)
    ELSE
      IRET(JWRK)=NF_GET_VAR_DOUBLE(IFILE_ID,IVAR_ID,PTIME)
    ENDIF
  ENDIF
ENDDO
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF ((IFILE_ID==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    KRESP=1
  ENDIF
ENDDO
IF (KRESP /=0) CALL ERROR_READ_SURF_OL(YRECFM,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_OL:READ_SURFT0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT0_OL
!
