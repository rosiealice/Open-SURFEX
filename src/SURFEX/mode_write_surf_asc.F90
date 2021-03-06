!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_WRITE_SURF_ASC
!
INTERFACE WRITE_SURF0_ASC
        MODULE PROCEDURE WRITE_SURFX0_ASC
        MODULE PROCEDURE WRITE_SURFN0_ASC
        MODULE PROCEDURE WRITE_SURFL0_ASC
        MODULE PROCEDURE WRITE_SURFC0_ASC
END INTERFACE
INTERFACE WRITE_SURFN_ASC
        MODULE PROCEDURE WRITE_SURFX1_ASC
        MODULE PROCEDURE WRITE_SURFN1_ASC
        MODULE PROCEDURE WRITE_SURFL1_ASC
        MODULE PROCEDURE WRITE_SURFX2_ASC
END INTERFACE
INTERFACE WRITE_SURFT_ASC
        MODULE PROCEDURE WRITE_SURFT0_ASC
        MODULE PROCEDURE WRITE_SURFT1_ASC
        MODULE PROCEDURE WRITE_SURFT2_ASC
END INTERFACE
!
 CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_SURFX0_ASC (&
                                   HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a real scalar
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
REAL,               INTENT(IN) :: PFIELD   ! the real scalar to be read
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX0_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX0_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
WRITE(NUNIT,FMT=*,ERR=100) PFIELD
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX0_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFN0_ASC (&
                                   HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write an integer
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NMASK, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN) :: KFIELD   ! the integer to be read
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFN0_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFN0_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
WRITE(NUNIT,FMT=*,ERR=100) KFIELD
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFN0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFN0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN0_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFL0_ASC (&
                                   HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a logical
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),  INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL,            INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN) :: HCOMMENT ! comment string
!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFL0_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFL0_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
WRITE(NUNIT,FMT=*,ERR=100) OFIELD
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFL0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFL0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL0_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFC0_ASC (&
                                   HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a character
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC      ! name of the article to be read
 CHARACTER(LEN=40),  INTENT(IN)  :: HFIELD    ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP     ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT  ! comment string
!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFC0_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFC0_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//HREC
WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
WRITE(NUNIT,FMT='(A40)',ERR=100) HFIELD
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFC0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFC0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFC0_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFX1_ASC (&
                                   HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 1D array for the externalised surface 
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE, WLOG_MPI
!
USE MODD_SURFEX_OMP, ONLY : LWORK0, NWORKB, NBLOCK
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NMASK, NFULL, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
USE MODI_GATHER_AND_WRITE_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),   INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:),  INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),    INTENT(IN) :: HDIR     ! type of field :
                                            ! 'H' : field with
                                            !       horizontal spatial dim.
                                            ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
INTEGER :: ISIZE, J
REAL   :: XTIME0
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD))) :: ZWORK   ! work array read in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX1_ASC',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
!
NWORKB=0
! 
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
!
!$OMP END SINGLE
!
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX1_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
IF (HDIR=='-') THEN
  ISIZE = SIZE(PFIELD)
  ZWORK(1:ISIZE) = PFIELD
ELSE
  ISIZE = SIZE(ZWORK)
  CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
  !
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) '&'//CMASK//' '//HREC
  WRITE(NUNIT,FMT='(A50)',IOSTAT=NWORKB) HCOMMENT(1:50)
  WRITE(NUNIT,FMT='(50D20.8)',IOSTAT=NWORKB) ZWORK(1:ISIZE)
  !
!$OMP END SINGLE
  !  
  IF (NWORKB/=0) CALL ERROR_WRITE_SURF_ASC(HREC,NWORKB)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
KRESP = NWORKB
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX1_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX1_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2_ASC (&
                                   HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to fill a write 2D array for the externalised surface 
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE, WLOG_MPI
!
USE MODD_SURFEX_OMP, ONLY : LWORK0, NWORKB
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NMASK, NFULL, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
USE MODI_GATHER_AND_WRITE_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),        INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:),     INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),       INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),         INTENT(IN) :: HDIR     ! type of field :
                                                 ! 'H' : field with
                                                 !       horizontal spatial dim.
                                                 ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
! 
INTEGER :: ISIZE
REAL   :: XTIME0
REAL, DIMENSION(MAX(NFULL,SIZE(PFIELD,1)),SIZE(PFIELD,2)) :: ZWORK   ! work array read in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX2_ASC',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
!
NWORKB=0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
!
!$OMP END SINGLE
!
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX2_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
IF (HDIR=='-') THEN
  ISIZE = SIZE(PFIELD,1)
  ZWORK(1:ISIZE,:) = PFIELD(:,:)
ELSE
  ISIZE = SIZE(ZWORK,1)
  CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK,NMASK)
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
  !
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) '&'//CMASK//' '//HREC
  WRITE(NUNIT,FMT='(A50)',IOSTAT=NWORKB) HCOMMENT(1:50)
  WRITE(NUNIT,FMT='(50D20.8)',IOSTAT=NWORKB) ZWORK(1:ISIZE,:)
  !
!$OMP END SINGLE
  !  
  IF (NWORKB/=0) CALL ERROR_WRITE_SURF_ASC(HREC,NWORKB)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
KRESP = NWORKB
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFX2_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX2_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFN1_ASC (&
                                   HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to write an integer array
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_SURFEX_OMP, ONLY : LWORK0, NWORKB
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, NMASK, NFULL, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
USE MODI_GATHER_AND_WRITE_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),      INTENT(IN) :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:),  INTENT(IN) :: KFIELD   ! the integer to be read
INTEGER,                INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
INTEGER :: ISIZE
INTEGER, DIMENSION(MAX(NFULL,SIZE(KFIELD))) :: IWORK  ! work array read in the file
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFN1_ASC',0,ZHOOK_HANDLE)
!
!$OMP SINGLE
!
NWORKB = 0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
!
!$OMP END SINGLE
!
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFN1_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
IF (HDIR=='-' .OR. HREC=='-') THEN
  ISIZE = SIZE(KFIELD)
  IWORK(1:ISIZE) = KFIELD
ELSE
  ISIZE = SIZE(IWORK)
  CALL GATHER_AND_WRITE_MPI(KFIELD,IWORK,NMASK)
ENDIF
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
  !   
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) '&'//CMASK//' '//HREC
  WRITE(NUNIT,FMT='(A50)',IOSTAT=NWORKB) HCOMMENT(1:50)
  WRITE(NUNIT,FMT='(100I8)',IOSTAT=NWORKB) IWORK(1:ISIZE)
  !
!$OMP END SINGLE
  !
  IF (NWORKB/=0) CALL ERROR_WRITE_SURF_ASC(HREC,NWORKB)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
KRESP = NWORKB
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFN1_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN1_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFL1_ASC (&
                                   HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  * - routine to write a logical array
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_SURFEX_OMP, ONLY : LWORK0, NWORKB
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),      INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL, DIMENSION(:),  INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN) :: HCOMMENT ! comment string
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFL1_ASC',0,ZHOOK_HANDLE)
!
!$OMP SINGLE
!
NWORKB = 0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
!
!$OMP END SINGLE
!
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFL1_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
  !  
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) '&'//CMASK//' '//HREC
  WRITE(NUNIT,FMT='(A50)',IOSTAT=NWORKB) HCOMMENT(1:50)
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) OFIELD
  !
!$OMP END SINGLE
  !
  IF (NWORKB/=0) CALL ERROR_WRITE_SURF_ASC(HREC,NWORKB)
  !  
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
KRESP = NWORKB
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFL1_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL1_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFT0_ASC (&
                                   HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
!
!
!
USE MODD_SURFEX_OMP, ONLY : LWORK0
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN)  :: KYEAR    ! year
INTEGER,            INTENT(IN)  :: KMONTH   ! month
INTEGER,            INTENT(IN)  :: KDAY     ! day
REAL,               INTENT(IN)  :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
INTEGER, DIMENSION(3) :: ITDATE
REAL(KIND=JPRB)       :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT0_ASC',0,ZHOOK_HANDLE)
!
KRESP=0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT0_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
ITDATE(1) = KYEAR
ITDATE(2) = KMONTH
ITDATE(3) = KDAY
!
WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//TRIM(HREC)//'%TDATE'
WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
WRITE(NUNIT,FMT=*,ERR=100) ITDATE(:)
!
WRITE(NUNIT,FMT=*,ERR=100) '&'//CMASK//' '//TRIM(HREC)//'%TIME'
WRITE(NUNIT,FMT='(A50)',ERR=100) HCOMMENT(1:50)
WRITE(NUNIT,FMT=*,ERR=100) PTIME
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT0_ASC',1,ZHOOK_HANDLE)
RETURN
!
100 CONTINUE
 CALL ERROR_WRITE_SURF_ASC(HREC,KRESP)
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT0_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT0_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFT1_ASC (&
                                   HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_SURFEX_OMP, ONLY : LWORK0, NWORKB
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),     INTENT(IN) :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:), INTENT(IN) :: KYEAR    ! year
INTEGER, DIMENSION(:), INTENT(IN) :: KMONTH   ! month
INTEGER, DIMENSION(:), INTENT(IN) :: KDAY     ! day
REAL,    DIMENSION(:), INTENT(IN) :: PTIME    ! time
INTEGER,               INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),    INTENT(IN) :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
INTEGER, DIMENSION(3,SIZE(KYEAR)) :: ITDATE
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT1_ASC',0,ZHOOK_HANDLE)
!
!$OMP SINGLE
!
NWORKB = 0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
!
!$OMP END SINGLE
!
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT1_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
  !
  ITDATE(1,:) = KYEAR  (:)
  ITDATE(2,:) = KMONTH (:)
  ITDATE(3,:) = KDAY   (:)
  !
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) '&'//CMASK//' '//TRIM(HREC)//'%TDATE'
  WRITE(NUNIT,FMT='(A50)',IOSTAT=NWORKB) HCOMMENT(1:50)
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) ITDATE(:,:)
  !
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) '&'//CMASK//' '//TRIM(HREC)//'%TIME'
  WRITE(NUNIT,FMT='(A50)',IOSTAT=NWORKB) HCOMMENT(1:50)
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) PTIME
  !
!$OMP END SINGLE
  !   
  IF (NWORKB/=0) CALL ERROR_WRITE_SURF_ASC(HREC,NWORKB)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
KRESP = NWORKB
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT1_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT1_ASC
!
!     #############################################################
      SUBROUTINE WRITE_SURFT2_ASC (&
                                   HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  * - routine to write a date
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODD_SURFEX_OMP, ONLY : LWORK0, NWORKB
!
USE MODD_IO_SURF_ASC,        ONLY : NUNIT, CMASK
!
USE MODI_IO_BUFF
USE MODI_ERROR_WRITE_SURF_ASC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1   Declarations of arguments
!
!
!
 CHARACTER(LEN=12),       INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KYEAR    ! year
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KMONTH   ! month
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KDAY     ! day
REAL,    DIMENSION(:,:), INTENT(IN)  :: PTIME    ! time
INTEGER,                 INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),      INTENT(IN)  :: HCOMMENT ! comment string

!*      0.2   Declarations of local variables
!
INTEGER, DIMENSION(3,SIZE(KYEAR,1),SIZE(KYEAR,2)) :: ITDATE
REAL   :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT2_ASC',0,ZHOOK_HANDLE)
!
!$OMP SINGLE
NWORKB = 0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0) 
!$OMP END SINGLE
!
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT2_ASC',1,ZHOOK_HANDLE)
IF (LWORK0) RETURN
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
  !    
  ITDATE(1,:,:) = KYEAR  (:,:)
  ITDATE(2,:,:) = KMONTH (:,:)
  ITDATE(3,:,:) = KDAY   (:,:)
  !
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) '&'//CMASK//' '//TRIM(HREC)//'%TDATE'
  WRITE(NUNIT,FMT='(A50)',IOSTAT=NWORKB) HCOMMENT(1:50)
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) ITDATE(:,:,:)
  !
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) '&'//CMASK//' '//TRIM(HREC)//'%TIME'
  WRITE(NUNIT,FMT='(A50)',IOSTAT=NWORKB) HCOMMENT(1:50)
  WRITE(NUNIT,FMT=*,IOSTAT=NWORKB) PTIME
  !
!$OMP END SINGLE
  !
  IF (NWORKB/=0) CALL ERROR_WRITE_SURF_ASC(HREC,NWORKB)
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
KRESP = NWORKB
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_ASC:WRITE_SURFT2_ASC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT2_ASC
!
END MODULE MODE_WRITE_SURF_ASC
