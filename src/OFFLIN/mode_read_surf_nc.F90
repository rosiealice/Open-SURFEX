!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_READ_SURF_NC
!
!!    PURPOSE
!!    -------
!
!       The purpose of READ_SURF_NC is
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
INTERFACE READ_SURF0_NC
      SUBROUTINE READ_SURFX0_NC(HREC,PFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL,               INTENT(OUT) :: PFIELD   ! the real scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFX0_NC
      SUBROUTINE READ_SURFN0_NC(HREC,KFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT) :: KFIELD   ! the integer scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFN0_NC
      SUBROUTINE READ_SURFC0_NC(HREC,HFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=*),   INTENT(IN)  :: HREC     ! name of the article to be read
 CHARACTER(LEN=40),   INTENT(OUT) :: HFIELD   ! the integer scalar to be read
INTEGER,             INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFC0_NC
      SUBROUTINE READ_SURFL0_NC(HREC,OFIELD,KRESP,HCOMMENT)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL,                  INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFL0_NC
END INTERFACE
INTERFACE READ_SURFN_NC
      SUBROUTINE READ_SURFX1_NC(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),   INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFX1_NC
      SUBROUTINE READ_SURFX2_NC(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:),     INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFX2_NC
      SUBROUTINE READ_SURFN1_NC(HREC,KFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:), INTENT(OUT) :: KFIELD   ! the integer scalar to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFN1_NC
      SUBROUTINE READ_SURFN2_NC(HREC,KFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KFIELD   ! the integer scalar to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFN2_NC
      SUBROUTINE READ_SURFL1_NC(HREC,OFIELD,KRESP,HCOMMENT,HDIR)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL, DIMENSION(:),   INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),         INTENT(IN)  :: HDIR     ! type of field :
END SUBROUTINE READ_SURFL1_NC
END INTERFACE
INTERFACE READ_SURFT_NC
      SUBROUTINE READ_SURFT0_NC(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                  INTENT(OUT) :: KYEAR    ! year
INTEGER,                  INTENT(OUT) :: KMONTH   ! month
INTEGER,                  INTENT(OUT) :: KDAY     ! day
REAL,                     INTENT(OUT) :: PTIME    ! time
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFT0_NC
      SUBROUTINE READ_SURFT1_NC(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:),    INTENT(OUT) :: KYEAR    ! year
INTEGER, DIMENSION(:),    INTENT(OUT) :: KMONTH   ! month
INTEGER, DIMENSION(:),    INTENT(OUT) :: KDAY     ! day
REAL, DIMENSION(:),       INTENT(OUT) :: PTIME    ! time
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFT1_NC
      SUBROUTINE READ_SURFT2_NC(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
 CHARACTER(LEN=*),        INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:,:),  INTENT(OUT) :: KYEAR    ! year
INTEGER, DIMENSION(:,:),  INTENT(OUT) :: KMONTH   ! month
INTEGER, DIMENSION(:,:),  INTENT(OUT) :: KDAY     ! day
REAL, DIMENSION(:,:),     INTENT(OUT) :: PTIME    ! time
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment
END SUBROUTINE READ_SURFT2_NC

END INTERFACE
!
END MODULE MODE_READ_SURF_NC
!
!     #############################################################
      SUBROUTINE READ_SURFX0_NC(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READX0* - routine to read a real scalar
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODI_ERROR_READ_SURF_NC
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
INTEGER            :: IVAR_ID,JRET,IVAL,ITYPE,INDIMS
INTEGER,DIMENSION(4) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFX0_NC',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
IF (NID_NC.NE.0) THEN
  !       
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
  IRET(1)=NF_INQ_VARTYPE (NID_NC,IVAR_ID,ITYPE)
  IRET(1)=NF_INQ_VARNDIMS(NID_NC,IVAR_ID,INDIMS)
  !  
  ! 2. Get variable
  !----------------------------
  IF (ITYPE==NF_DOUBLE) THEN
    IRET(2)=NF_GET_VAR_DOUBLE(NID_NC,IVAR_ID,PFIELD)
  ELSEIF (ITYPE==NF_FLOAT) THEN
    IRET(2)=NF_GET_VAR_REAL(NID_NC,IVAR_ID,ZFIELD)
    PFIELD = ZFIELD
  ENDIF
  !
  IRET(3) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",HCOMMENT)
  !
ENDIF
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF ((PFIELD==XUNDEF).OR.(NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    PFIELD=XUNDEF
    KRESP=1
  ENDIF
ENDDO
!     
IF (KRESP /=0) CALL ERROR_READ_SURF_NC(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFX0_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX0_NC
!
!     #############################################################
      SUBROUTINE READ_SURFX1_NC(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX1* - routine to fill a real 1D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_READ, NPROC, NCOMM, &
                                 XTIME_NPIO_READ, XTIME_COMM_READ
!
USE MODD_SURFEX_OMP, ONLY : XWORKD, NWORKB, CWORK0
!
USE MODD_IO_SURF_NC, ONLY: LMASK,NMASK,NID_NC
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE MODI_ERROR_READ_SURF_NC
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
 CHARACTER(LEN=100)    :: YNAME
INTEGER :: IL1, IVAR_ID,JRET,JDIM,INDIMS, ITYPE, INFOMPI
INTEGER,DIMENSION(4) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(4) :: IRET
!
REAL*4, DIMENSION(:), ALLOCATABLE :: ZTAB_1D4
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFX1_NC',0,ZHOOK_HANDLE)
!
IL1 = SIZE(PFIELD)
!
!$OMP SINGLE
NWORKB=0
 CWORK0 = " "
!$OMP END SINGLE
!
HCOMMENT = " "
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (HDIR=='-') THEN
!$OMP SINGLE
  ALLOCATE(XWORKD(IL1))
!$OMP END SINGLE
ENDIF
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !  
  IF (NID_NC.NE.0) THEN
    !  
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
    IRET(2)=NF_INQ_VARTYPE (NID_NC,IVAR_ID,ITYPE)
    !
    IF (HDIR=='A') THEN
      !
      ALLOCATE(XWORKD(IL1))
      !
    ELSEIF (HDIR/='-') THEN
      !
      IRET(3)=NF_INQ_VARNDIMS(NID_NC,IVAR_ID,INDIMS)
      IRET(4)=NF_INQ_VARDIMID(NID_NC,IVAR_ID,IDIMIDS(1:INDIMS))
      IDIMLEN(:) = 1.
      DO JDIM=1,INDIMS
        JRET=NF_INQ_DIMLEN(NID_NC,IDIMIDS(JDIM),IDIMLEN(JDIM))
      ENDDO
      IRET(4)=NF_INQ_DIMNAME(NID_NC,IDIMIDS(1),YNAME)
      !
      IF (TRIM(YNAME).NE.'Number_of_points') THEN
        ALLOCATE(XWORKD(IDIMLEN(1)*IDIMLEN(2)))
      ELSE
        ALLOCATE(XWORKD(IDIMLEN(1)))
      ENDIF
      !
    ENDIF
    !
    ! 2. Get variable
    !----------------------------
    IF (ITYPE==NF_DOUBLE) THEN
      IRET(1)=NF_GET_VAR_DOUBLE(NID_NC,IVAR_ID,XWORKD)
    ELSEIF (ITYPE==NF_FLOAT) THEN
      ALLOCATE(ZTAB_1D4(SIZE(XWORKD)))
      IRET(2)=NF_GET_VAR_REAL(NID_NC,IVAR_ID,ZTAB_1D4)
      XWORKD(:) = ZTAB_1D4(:)
      DEALLOCATE(ZTAB_1D4)
    ENDIF
    !
    IRET(3) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",CWORK0)   
    !
  ENDIF
  !
  ! 3. Check for errors
  !--------------------
  DO JRET=1,1
    IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      XWORKD = XUNDEF
      NWORKB=1
    ENDIF
  ENDDO
  !
!$OMP END SINGLE
  !
  IF (NWORKB /=0) CALL ERROR_READ_SURF_NC(HREC,NWORKB)
  !
ELSEIF (HDIR/='-') THEN
!$OMP SINGLE
  ALLOCATE(XWORKD(0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (HDIR=='A') THEN  ! no distribution on other tasks
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    PFIELD(:) = XWORKD(1:SIZE(PFIELD))
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN ! distribution of the total field on other tasks
!$OMP SINGLE
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(XWORKD,SIZE(XWORKD)*KIND(XWORKD)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)   
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
!$OMP END SINGLE
  PFIELD(:) = XWORKD(1:SIZE(PFIELD))
ELSE
  IF (LMASK) THEN
    CALL READ_AND_SEND_MPI(XWORKD,PFIELD,NMASK)
  ELSE 
    CALL READ_AND_SEND_MPI(XWORKD,PFIELD)
  END IF
ENDIF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(XWORKD)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFX1_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX1_NC
!
!     #############################################################
      SUBROUTINE READ_SURFX2_NC(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX2* - routine to fill a real 2D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY: NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ
!
USE MODD_SURFEX_OMP, ONLY : XWORKD2, NWORKB, CWORK0
!
USE MODD_IO_SURF_NC, ONLY: LMASK,NMASK,NID_NC
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE MODI_ERROR_READ_SURF_NC
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
 CHARACTER(LEN=100)    :: YNAME 
INTEGER            :: IL1, IL2
INTEGER            :: IVAR_ID,JRET,JDIM,INDIMS,ITYPE, INFOMPI
INTEGER,DIMENSION(4) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(4) :: IRET
REAL*4, DIMENSION(:,:), ALLOCATABLE :: ZTAB_2D4
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFX2_NC',0,ZHOOK_HANDLE)
!
IL1 = SIZE(PFIELD,1)
IL2 = SIZE(PFIELD,2)
!
!$OMP SINGLE
NWORKB=0
 CWORK0 = " "
!$OMP END SINGLE
!
HCOMMENT = " "
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (HDIR=='-') THEN
!$OMP SINGLE
  ALLOCATE(XWORKD2(IL1,IL2))
!$OMP END SINGLE
ENDIF
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !  
  IF (NID_NC.NE.0) THEN
    !
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
    IRET(2)=NF_INQ_VARTYPE (NID_NC,IVAR_ID,ITYPE)
    !
    IF (HDIR=='A') THEN
      !
      ALLOCATE(XWORKD2(IL1,IL2))
      !
    ELSEIF (HDIR/='-') THEN
      !
      IRET(3)=NF_INQ_VARNDIMS(NID_NC,IVAR_ID,INDIMS)
      IRET(4)=NF_INQ_VARDIMID(NID_NC,IVAR_ID,IDIMIDS(1:INDIMS))
      IDIMLEN(:) = 1.
      DO JDIM=1,INDIMS
        JRET=NF_INQ_DIMLEN(NID_NC,IDIMIDS(JDIM),IDIMLEN(JDIM))
      ENDDO
      !
      IRET(4)=NF_INQ_DIMNAME(NID_NC,IDIMIDS(1),YNAME)    
      ! 
      IF (TRIM(YNAME).NE.'Number_of_points') THEN
        ALLOCATE(XWORKD2(IDIMLEN(1)*IDIMLEN(2),IDIMLEN(3)))
      ELSE
        ALLOCATE(XWORKD2(IDIMLEN(1),IDIMLEN(2)))
      ENDIF
      !
    ENDIF
    !
    ! 2. Get variable
    !----------------------------
    !
    IF (ITYPE==NF_DOUBLE) THEN
      IRET(2)=NF_GET_VAR_DOUBLE(NID_NC,IVAR_ID,XWORKD2)
    ELSEIF (ITYPE==NF_FLOAT) THEN
      ALLOCATE(ZTAB_2D4(SIZE(XWORKD2,1),SIZE(XWORKD2,2)))
      IRET(2)=NF_GET_VAR_REAL(NID_NC,IVAR_ID,ZTAB_2D4)
      XWORKD2(:,:) = ZTAB_2D4(:,:)
      DEALLOCATE(ZTAB_2D4)
    ENDIF      
    !
    IRET(3) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",CWORK0)  
    !
  ENDIF

  ! 3. Check for errors
  !--------------------
  DO JRET=1,2
    IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      XWORKD2 = XUNDEF
      NWORKB=1
    ENDIF
  ENDDO
  !
!$OMP END SINGLE
  !
  IF (NWORKB /=0) CALL ERROR_READ_SURF_NC(HREC,NWORKB)
  !
ELSEIF (HDIR/='-') THEN
!$OMP SINGLE
  ALLOCATE(XWORKD2(1,SIZE(PFIELD,2)))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (HDIR=='A') THEN  ! no distribution on other tasks
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    PFIELD(:,:) = XWORKD2(:,1:SIZE(PFIELD,2))
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN ! distribution of the total field on other tasks
!$OMP SINGLE
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(XWORKD2,SIZE(XWORKD2)*KIND(XWORKD2)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)   
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
!$OMP END SINGLE
  PFIELD(:,:) = XWORKD2(:,1:SIZE(PFIELD,2))
ELSE
  IF (LMASK) THEN
    CALL READ_AND_SEND_MPI(XWORKD2(:,1:SIZE(PFIELD,2)),PFIELD,NMASK)
  ELSE 
    CALL READ_AND_SEND_MPI(XWORKD2(:,1:SIZE(PFIELD,2)),PFIELD)
  END IF
ENDIF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(XWORKD2)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFX2_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFX2_NC
!
!     #############################################################
      SUBROUTINE READ_SURFN0_NC(HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
USE MODD_SURF_PAR,   ONLY: NUNDEF
!
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODI_ERROR_READ_SURF_NC
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
INTEGER :: IVAR_ID,JRET,JDIM,INDIMS
INTEGER,DIMENSION(4) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFN0_NC',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
IF (NID_NC.NE.0) THEN
  !        
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
  !  
  ! 2. Get variable
  !----------------------------
  IRET(2)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KFIELD)
  !
  IRET(3)=NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",HCOMMENT)
  !
ENDIF
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF ((KFIELD==NUNDEF).OR.(NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    KFIELD=NUNDEF
    KRESP=1
  ENDIF
ENDDO
!
IF (KRESP /=0)  CALL ERROR_READ_SURF_NC(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFN0_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN0_NC
!
!     #############################################################
      SUBROUTINE READ_SURFN1_NC(HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_READ, NPROC, NCOMM, &
                                 XTIME_NPIO_READ, XTIME_COMM_READ
!
USE MODD_SURFEX_OMP, ONLY : NWORKD, NWORKB, CWORK0
!
USE MODD_IO_SURF_NC, ONLY: LMASK,NMASK,NID_NC
!
USE MODD_SURF_PAR,   ONLY: NUNDEF
!
USE MODI_ERROR_READ_SURF_NC
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
 CHARACTER(LEN=100) :: YFILE,YOUT          ! Filename
 CHARACTER(LEN=100)    :: YNAME
INTEGER :: IL1, IVAR_ID,JRET,JDIM,INDIMS, ITYPE, INFOMPI
INTEGER,DIMENSION(4) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(4) :: IRET
!
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFX1_NC',0,ZHOOK_HANDLE)
!
IL1 = SIZE(KFIELD)
!
!$OMP SINGLE
NWORKB=0
 CWORK0 = " "
!$OMP END SINGLE
!
HCOMMENT = " "
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (HDIR=='-') THEN
!$OMP SINGLE
  ALLOCATE(NWORKD(IL1))
!$OMP END SINGLE
ENDIF
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !  
  IF (NID_NC.NE.0) THEN
    !  
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
    IRET(2)=NF_INQ_VARTYPE (NID_NC,IVAR_ID,ITYPE)
    IRET(3)=NF_INQ_VARNDIMS(NID_NC,IVAR_ID,INDIMS)
    IRET(4)=NF_INQ_VARDIMID(NID_NC,IVAR_ID,IDIMIDS(1:INDIMS))
    IDIMLEN(:) = 1.
    DO JDIM=1,INDIMS
      JRET=NF_INQ_DIMLEN(NID_NC,IDIMIDS(JDIM),IDIMLEN(JDIM))
    ENDDO
    !
    IRET(4)=NF_INQ_DIMNAME(NID_NC,IDIMIDS(1),YNAME)
    !
    IF (TRIM(YNAME).NE.'Number_of_points') THEN
      ALLOCATE(NWORKD(IDIMLEN(1)*IDIMLEN(2)))
    ELSE
      ALLOCATE(NWORKD(IDIMLEN(1)))
    ENDIF
    !
    ! 2. Get variable
    !----------------------------
    IF (ITYPE==NF_INT) THEN
      IRET(1)=NF_GET_VAR_INT(NID_NC,IVAR_ID,NWORKD)
    ENDIF
    !  
    IRET(2) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",CWORK0)
    !
  ENDIF
  !
  ! 3. Check for errors
  !--------------------
  DO JRET=1,1
    IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      NWORKD = NUNDEF
      NWORKB=1
    ENDIF
  ENDDO
  !
!$OMP END SINGLE
  !
  IF (NWORKB /=0) CALL ERROR_READ_SURF_NC(HREC,NWORKB)
  !
ELSEIF (HDIR/='-') THEN
!$OMP SINGLE
  ALLOCATE(NWORKD(0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (HDIR=='A') THEN  ! no distribution on other tasks
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    KFIELD(:) = NWORKD(1:SIZE(KFIELD))
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN ! distribution of the total field on other tasks
!$OMP SINGLE
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(NWORKD,SIZE(NWORKD)*KIND(NWORKD)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)   
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
!$OMP END SINGLE
  KFIELD(:) = NWORKD(1:SIZE(KFIELD))
ELSE
  IF (LMASK) THEN
    CALL READ_AND_SEND_MPI(NWORKD,KFIELD,NMASK)
  ELSE 
    CALL READ_AND_SEND_MPI(NWORKD,KFIELD)
  END IF
ENDIF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(NWORKD)
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFN1_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN1_NC
!
!     #############################################################
      SUBROUTINE READ_SURFN2_NC(HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!
USE MODD_SURFEX_MPI, ONLY: NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ
!
USE MODD_SURFEX_OMP, ONLY : NWORKD2, NWORKB, CWORK0
!
USE MODD_IO_SURF_NC, ONLY: LMASK,NMASK,NID_NC
!
USE MODD_SURF_PAR,   ONLY: NUNDEF
!
USE MODI_ERROR_READ_SURF_NC
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
 CHARACTER(LEN=*),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KFIELD   ! the integer scalar to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
 CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
                                                ! 'H' : field with
                                                !       horizontal spatial dim.
                                                ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YFILE,YOUT          ! filename
 CHARACTER(LEN=100)    :: YNAME 
INTEGER            ::  IL1, IL2, IVAR_ID,JRET,JDIM,INDIMS,ITYPE, INFOMPI
INTEGER,DIMENSION(4) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(4) :: IRET
REAL*4, DIMENSION(:,:), ALLOCATABLE :: ZTAB_2D4
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFN2_NC',0,ZHOOK_HANDLE)
!
IL1 = SIZE(KFIELD,1)
IL2 = SIZE(KFIELD,2)
!
!$OMP SINGLE
NWORKB=0
 CWORK0 = " "
!$OMP END SINGLE
!
HCOMMENT = " "
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
IF (HDIR=='-') THEN
!$OMP SINGLE
  ALLOCATE(NWORKD2(IL1,IL2))
!$OMP END SINGLE
ENDIF
!
IF (NRANK==NPIO) THEN
  !
!$OMP SINGLE
  !  
  IF (NID_NC.NE.0) THEN
    !   
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
    IRET(2)=NF_INQ_VARTYPE (NID_NC,IVAR_ID,ITYPE)
    IRET(3)=NF_INQ_VARNDIMS(NID_NC,IVAR_ID,INDIMS)
    IRET(4)=NF_INQ_VARDIMID(NID_NC,IVAR_ID,IDIMIDS(1:INDIMS))
    IDIMLEN(:) = 1.
    DO JDIM=1,INDIMS
      JRET=NF_INQ_DIMLEN(NID_NC,IDIMIDS(JDIM),IDIMLEN(JDIM))
    ENDDO
    !
    IRET(4)=NF_INQ_DIMNAME(NID_NC,IDIMIDS(1),YNAME)    
    ! 
    ! 2. Get variable
    !----------------------------
    IF (TRIM(YNAME).NE.'Number_of_points') THEN
      ALLOCATE(NWORKD2(IDIMLEN(1)*IDIMLEN(2),IDIMLEN(3)))
    ELSE
      ALLOCATE(NWORKD2(IDIMLEN(1),IDIMLEN(2)))
    ENDIF
    !
    IF (ITYPE==NF_INT) THEN
      IRET(2)=NF_GET_VAR_INT(NID_NC,IVAR_ID,NWORKD2)
    ENDIF      
    !
    IRET(3) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",CWORK0)
  ENDIF

  ! 3. Check for errors
  !--------------------
  DO JRET=1,2
    IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      NWORKD2 = NUNDEF
      NWORKB=1
    ENDIF
  ENDDO
  !
!$OMP END SINGLE
  !
  IF (NWORKB /=0) CALL ERROR_READ_SURF_NC(HREC,NWORKB)
  !
ELSEIF (HDIR/='-') THEN
!$OMP SINGLE
  ALLOCATE(NWORKD2(0,0))
!$OMP END SINGLE
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (HDIR=='A') THEN  ! no distribution on other tasks
  IF ( NRANK==NPIO ) THEN
#ifdef SFX_MPI
    XTIME0 = MPI_WTIME()
#endif
    KFIELD(:,:) = NWORKD2(:,:)
#ifdef SFX_MPI
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
#endif
  ENDIF
ELSEIF (HDIR=='-') THEN ! distribution of the total field on other tasks
!$OMP SINGLE
#ifdef SFX_MPI
  IF (NPROC>1) THEN
    XTIME0 = MPI_WTIME()
    CALL MPI_BCAST(NWORKD2,SIZE(NWORKD2)*KIND(NWORKD2)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)   
    XTIME_COMM_READ = XTIME_COMM_READ + (MPI_WTIME() - XTIME0)
  ENDIF
#endif
!$OMP END SINGLE
  KFIELD(:,:) = NWORKD2(:,:)
ELSE
  IF (LMASK) THEN
    CALL READ_AND_SEND_MPI(NWORKD2,KFIELD,NMASK)
  ELSE 
    CALL READ_AND_SEND_MPI(NWORKD2,KFIELD)
  END IF
ENDIF
!
!$OMP BARRIER
!
!$OMP SINGLE
DEALLOCATE(NWORKD2) 
!$OMP END SINGLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFN2_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFN2_NC
!
!     #############################################################
      SUBROUTINE READ_SURFC0_NC(HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READC0* - routine to read a STRING
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODI_ERROR_READ_SURF_NC
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
 CHARACTER(LEN=40):: YFIELD   
INTEGER :: IVAR_ID,JRET,JDIM,INDIMS
INTEGER,DIMENSION(4) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFC0_NC',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
IF (NID_NC.NE.0) THEN
  !       
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
  !  
  ! 2. Get variable
  !----------------------------
  IRET(2)=NF_GET_VAR_TEXT(NID_NC,IVAR_ID,YFIELD)
  HFIELD=YFIELD(:LEN_TRIM(YFIELD))
  !
  IRET(3) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",HCOMMENT)
  !
ENDIF

! 3. Check for errors
!--------------------
DO JRET=1,2
  IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    KRESP=1
  ENDIF
ENDDO  
!
IF (KRESP /=0) CALL ERROR_READ_SURF_NC(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFC0_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFC0_NC
!
!     #############################################################
      SUBROUTINE READ_SURFL0_NC(HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READL0* - routine to read a logical
!    
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODI_ERROR_READ_SURF_NC
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
 CHARACTER(LEN=1)  :: YFIELD   ! work array read in the file
 CHARACTER(LEN=100) :: YFILE    ! Filename
INTEGER :: IVAR_ID,JRET
INTEGER,DIMENSION(3) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFL0_NC',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
IF (NID_NC.NE.0) THEN
  !       
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
  !  
  ! 2. Get variable
  !----------------------------
  IRET(2)=NF_GET_VAR_TEXT(NID_NC,IVAR_ID,YFIELD)
  !
  IRET(3) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",HCOMMENT)
  !  
ENDIF
!  
IF (YFIELD =="T") OFIELD=.TRUE.
IF (YFIELD =="F") OFIELD=.FALSE.
!
! 3. Check for errors
!--------------------
IF ((NID_NC==0).OR.IRET(1).NE.NF_NOERR) THEN 
  KRESP=1
ENDIF
!
IF (KRESP /=0)  CALL ERROR_READ_SURF_NC(HREC,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFL0_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL0_NC
!
!     #############################################################
      SUBROUTINE READ_SURFL1_NC(HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READL1* - routine to read a logical array
!    
USE MODD_SURFEX_MPI, ONLY : NRANK, NPROC, NCOMM, NPIO, XTIME_NPIO_READ, XTIME_COMM_READ
!
USE MODD_SURFEX_OMP, ONLY : LWORKD, NWORKB, CWORK0
!
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODI_ERROR_READ_SURF_NC
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
INTEGER :: IVAR_ID,JRET,JDIM,INDIMS
INTEGER :: INFOMPI
INTEGER,DIMENSION(1) :: IDIMIDS,IDIMLEN
INTEGER,DIMENSION(2) :: IRET
INTEGER, DIMENSION(:),    POINTER     :: IMASK    ! 1D mask to read only interesting
DOUBLE PRECISION   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFL1_NC',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
NWORKB=0
 CWORK0 = " "
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
  IF (NID_NC.NE.0) THEN
    !   
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (NID_NC,HREC,IVAR_ID)
    IRET(1)=NF_INQ_VARNDIMS(NID_NC,IVAR_ID,INDIMS)
    IRET(1)=NF_INQ_VARDIMID(NID_NC,IVAR_ID,IDIMIDS)
    DO JDIM=1,INDIMS
      JRET=NF_INQ_DIMLEN(NID_NC,IDIMIDS(JDIM),IDIMLEN(JDIM))
    ENDDO
    ALLOCATE(YTAB_1D(IDIMLEN(1)))
    !  
    ! 2. Get variable
    !----------------------------
    IRET(1)=NF_GET_VAR_TEXT(NID_NC,IVAR_ID,YTAB_1D)
    !
    DO JRET=1,SIZE(LWORKD)
      IF (YTAB_1D(JRET) =="T") LWORKD(JRET)=.TRUE.
      IF (YTAB_1D(JRET) =="F") LWORKD(JRET)=.FALSE.
    ENDDO
    !
    IRET(2) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",CWORK0)
    !
  ENDIF
  !
  ! 3. Check for errors
  !--------------------
  DO JRET=1,1
    IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
      NWORKB=1
    ENDIF
  ENDDO
  !
  DEALLOCATE(YTAB_1D)
  !
!$OMP END SINGLE
  !  
  IF (NWORKB /=0) CALL ERROR_READ_SURF_NC(HREC,NWORKB)
  !
ENDIF
!
KRESP = NWORKB
HCOMMENT = CWORK0
!
#ifdef SFX_MPI
XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
!
IF (NPROC>1 .AND. HDIR/='A') THEN
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
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFL1_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFL1_NC
!
!
!     #############################################################
      SUBROUTINE READ_SURFT0_NC(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a NETCDF  date_time scalar
!
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODI_ERROR_READ_SURF_NC
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
INTEGER :: IVAR_ID,JRET,JDIM,INDIMS,JWRK
INTEGER, DIMENSION(1) :: IDIMIDS,IDIMLEN
INTEGER, DIMENSION(5) :: IRET
INTEGER, DIMENSION(:), POINTER :: IMASK    ! 1D mask to read only interesting
REAL:: ZTIME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFT0_NC',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
DO JWRK=1,4
  !
  IF (JWRK == 1) THEN 
    YRECFM=TRIM(HREC)//'-YEAR'
  ELSEIF (JWRK == 2) THEN
    YRECFM = TRIM(HREC)//'-MONTH'
  ELSEIF (JWRK == 3) THEN
    YRECFM = TRIM(HREC)//'-DAY'
  ELSEIF (JWRK == 4) THEN    
    YRECFM=TRIM(HREC)//'-TIME'
  ENDIF
! 0. find filename
  !
  IF (NID_NC.NE.0) THEN
    !   
    ! 1. Find id of the variable
    !----------------------------
    JRET=NF_INQ_VARID   (NID_NC,YRECFM,IVAR_ID)
    !
    ! 2. Get variable
    !----------------------------
    IF (JWRK == 1) THEN 
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KYEAR)
    ELSEIF (JWRK==2) THEN
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KMONTH)
    ELSEIF (JWRK==3) THEN
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KDAY)
    ELSEIF (JWRK==4) THEN      
      IRET(JWRK)=NF_GET_VAR_DOUBLE(NID_NC,IVAR_ID,PTIME)
    ENDIF
    !
    IRET(5) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",HCOMMENT)
    !
  ENDIF
ENDDO
!
! 3. Check for errors
!--------------------
DO JRET=1,4
  IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    KRESP=1
  ENDIF
ENDDO
IF (KRESP /=0) CALL ERROR_READ_SURF_NC(YRECFM,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFT0_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT0_NC
!
!     #############################################################
      SUBROUTINE READ_SURFT1_NC(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a NETCDF  date_time scalar
!
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODI_ERROR_READ_SURF_NC
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
INTEGER, DIMENSION(:),    INTENT(OUT) :: KYEAR    ! year
INTEGER, DIMENSION(:),    INTENT(OUT) :: KMONTH   ! month
INTEGER, DIMENSION(:),    INTENT(OUT) :: KDAY     ! day
REAL, DIMENSION(:),       INTENT(OUT) :: PTIME    ! time
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment

!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=18)  :: YRECFM    ! Name of the article to be written
 CHARACTER(LEN=100) :: YFILE          ! Filename
INTEGER :: IVAR_ID,JRET,JDIM,INDIMS,JWRK
INTEGER, DIMENSION(1) :: IDIMIDS,IDIMLEN
INTEGER, DIMENSION(5) :: IRET
INTEGER, DIMENSION(:), POINTER :: IMASK    ! 1D mask to read only interesting
REAL:: ZTIME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFT1_NC',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
DO JWRK=1,4
  !
  IF (JWRK == 1) THEN 
    YRECFM=TRIM(HREC)//'-YEAR'
  ELSEIF (JWRK == 2) THEN
    YRECFM = TRIM(HREC)//'-MONTH'
  ELSEIF (JWRK == 3) THEN
    YRECFM = TRIM(HREC)//'-DAY'
  ELSEIF (JWRK == 4) THEN    
    YRECFM=TRIM(HREC)//'-TIME'
  ENDIF
! 0. find filename
  !
  IF (NID_NC.NE.0) THEN
    !   
    ! 1. Find id of the variable
    !----------------------------
    JRET=NF_INQ_VARID   (NID_NC,YRECFM,IVAR_ID)
    !
    ! 2. Get variable
    !----------------------------
    IF (JWRK == 1) THEN 
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KYEAR)
    ELSEIF (JWRK==2) THEN
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KMONTH)
    ELSEIF (JWRK==3) THEN
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KDAY)
    ELSEIF (JWRK==4) THEN      
      IRET(JWRK)=NF_GET_VAR_DOUBLE(NID_NC,IVAR_ID,PTIME)
    ENDIF
    !
    IRET(5) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",HCOMMENT)
    !
  ENDIF
ENDDO
!
! 3. Check for errors
!--------------------
DO JRET=1,4
  IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    KRESP=1
  ENDIF
ENDDO
IF (KRESP /=0) CALL ERROR_READ_SURF_NC(YRECFM,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFT1_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT1_NC
!
!     #############################################################
      SUBROUTINE READ_SURFT2_NC(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a NETCDF  date_time scalar
!
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODI_ERROR_READ_SURF_NC
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
INTEGER, DIMENSION(:,:),  INTENT(OUT) :: KYEAR    ! year
INTEGER, DIMENSION(:,:),  INTENT(OUT) :: KMONTH   ! month
INTEGER, DIMENSION(:,:),  INTENT(OUT) :: KDAY     ! day
REAL, DIMENSION(:,:),     INTENT(OUT) :: PTIME    ! time
INTEGER,                  INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(OUT) :: HCOMMENT ! comment

!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=18)  :: YRECFM    ! Name of the article to be written
 CHARACTER(LEN=100) :: YFILE          ! Filename
INTEGER :: IVAR_ID,JRET,JDIM,INDIMS,JWRK
INTEGER, DIMENSION(1) :: IDIMIDS,IDIMLEN
INTEGER, DIMENSION(5) :: IRET
INTEGER, DIMENSION(:), POINTER :: IMASK    ! 1D mask to read only interesting
REAL:: ZTIME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFT2_NC',0,ZHOOK_HANDLE)
!
KRESP=0
HCOMMENT = " "
!
DO JWRK=1,4
  !
  IF (JWRK == 1) THEN 
    YRECFM=TRIM(HREC)//'-YEAR'
  ELSEIF (JWRK == 2) THEN
    YRECFM = TRIM(HREC)//'-MONTH'
  ELSEIF (JWRK == 3) THEN
    YRECFM = TRIM(HREC)//'-DAY'
  ELSEIF (JWRK == 4) THEN    
    YRECFM=TRIM(HREC)//'-TIME'
  ENDIF
! 0. find filename
  !
  IF (NID_NC.NE.0) THEN
    !   
    ! 1. Find id of the variable
    !----------------------------
    JRET=NF_INQ_VARID   (NID_NC,YRECFM,IVAR_ID)
    !
    ! 2. Get variable
    !----------------------------
    IF (JWRK == 1) THEN 
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KYEAR)
    ELSEIF (JWRK==2) THEN
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KMONTH)
    ELSEIF (JWRK==3) THEN
      IRET(JWRK)=NF_GET_VAR_INT(NID_NC,IVAR_ID,KDAY)
    ELSEIF (JWRK==4) THEN      
      IRET(JWRK)=NF_GET_VAR_DOUBLE(NID_NC,IVAR_ID,PTIME)
    ENDIF
    !
    IRET(5) = NF_GET_ATT_TEXT(NID_NC,IVAR_ID,"comment",HCOMMENT)
    !
  ENDIF
ENDDO
!
! 3. Check for errors
!--------------------
DO JRET=1,4
  IF ((NID_NC==0).OR.IRET(JRET).NE.NF_NOERR) THEN 
    KRESP=1
  ENDIF
ENDDO
IF (KRESP /=0) CALL ERROR_READ_SURF_NC(YRECFM,KRESP)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_SURF_NC:READ_SURFT2_NC',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SURFT2_NC
!
