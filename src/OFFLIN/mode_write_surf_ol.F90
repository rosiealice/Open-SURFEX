!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_WRITE_SURF_OL
!
INTERFACE WRITE_SURF0_OL
        MODULE PROCEDURE WRITE_SURFX0_OL
        MODULE PROCEDURE WRITE_SURFN0_OL
        MODULE PROCEDURE WRITE_SURFC0_OL
        MODULE PROCEDURE WRITE_SURFL0_OL
END INTERFACE
INTERFACE WRITE_SURF0_TIME_OL
        MODULE PROCEDURE WRITE_SURFX0_TIME_OL
END INTERFACE
INTERFACE WRITE_SURFX1N1_OL
        MODULE PROCEDURE WRITE_SURFX1_OL
        MODULE PROCEDURE WRITE_SURFN1_OL
END INTERFACE
INTERFACE WRITE_SURFL1X2_OL
        MODULE PROCEDURE WRITE_SURFL1_OL
        MODULE PROCEDURE WRITE_SURFX2_OL
END INTERFACE
INTERFACE WRITE_SURFT_OL
        MODULE PROCEDURE WRITE_SURFT0_OL
END INTERFACE
!
 CONTAINS
!
!     #############################################################
      SUBROUTINE WRITE_SURFX0_OL(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEX0* - routine to read a real scalar
!
USE MODI_OL_FIND_FILE_WRITE
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
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL,               INTENT(IN)  :: PFIELD   ! the real scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT
!
!*      0.2   Declarations of local variables
!
INTEGER              :: IFILE_ID,IVAR_ID,JRET
INTEGER,DIMENSION(2) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
!
! 0. find filename
! -----------------
 CALL OL_FIND_FILE_WRITE(HREC,IFILE_ID)
! 
IF (IFILE_ID /= 0) THEN        
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)  
  ! 2. Put variable
  !----------------------------
  IRET(2)=NF_PUT_VAR_DOUBLE (IFILE_ID,IVAR_ID,PFIELD)
ENDIF
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF (IFILE_ID==0 .OR. IRET(JRET).NE.NF_NOERR) KRESP=1
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFX0_OL
!
!     #############################################################
      SUBROUTINE WRITE_SURFN0_OL(HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEN0* - routine to read an integer
!
USE MODI_OL_FIND_FILE_WRITE
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
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN)  :: KFIELD   ! the integer scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT
!
!*      0.2   Declarations of local variables
!
INTEGER              :: IFILE_ID, IVAR_ID, JRET
INTEGER,DIMENSION(2) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFN0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
!
! 0. find filename
! -----------------
 CALL OL_FIND_FILE_WRITE(HREC,IFILE_ID)
!
IF (IFILE_ID /= 0) THEN    
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)   
  ! 2. Get variable
  !----------------------------
  IRET(2)=NF_PUT_VAR_INT(IFILE_ID,IVAR_ID,KFIELD)  
ENDIF
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF (IFILE_ID==0 .OR. IRET(JRET).NE.NF_NOERR) KRESP=1
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFN0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN0_OL
!
!     #############################################################
      SUBROUTINE WRITE_SURFC0_OL(HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEC0* - routine to read a STRING
!
USE MODI_OL_FIND_FILE_WRITE
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
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
 CHARACTER(LEN=40),  INTENT(IN)  :: HFIELD   ! the integer scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100)   :: YFIELD
INTEGER              :: IFILE_ID, IVAR_ID, JRET
INTEGER,DIMENSION(2) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFC0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
!
! 0. find filename
! -----------------
 CALL OL_FIND_FILE_WRITE(HREC,IFILE_ID)
!
IF (IFILE_ID /= 0) THEN 
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
  ! 2. Get variable
  !----------------------------
  YFIELD=HFIELD(:LEN_TRIM(HFIELD))
  IRET(2)=NF_PUT_VAR_TEXT(IFILE_ID,IVAR_ID,YFIELD)  
ENDIF
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF (IFILE_ID==0 .OR. IRET(JRET).NE.NF_NOERR) KRESP=1
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFC0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFC0_OL
!
!     #############################################################
      SUBROUTINE WRITE_SURFL0_OL(HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEL0* - routine to read a logical
!    
USE MODI_OL_FIND_FILE_WRITE
USE MODI_HANDLE_ERR
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
 CHARACTER(LEN=12),   INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL,             INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN) :: HCOMMENT
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=1)     :: YFIELD   ! work array read in the file
INTEGER              :: IFILE_ID, IVAR_ID, JRET
INTEGER,DIMENSION(2) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFL0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
!
! 0. find filename
! -----------------
 CALL OL_FIND_FILE_WRITE(HREC,IFILE_ID)
! 
IF (IFILE_ID /= 0) THEN        
  ! 1. Find id of the variable
  !----------------------------
  IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
  IF (OFIELD) THEN
    YFIELD ='T'
  ELSE
    YFIELD ='F'
  ENDIF    
  ! 2. Put variable
  !----------------------------
  IRET(2)=NF_PUT_VAR_TEXT(IFILE_ID,IVAR_ID,YFIELD)
  CALL HANDLE_ERR(IRET(1),HREC)
ENDIF
!
! 3. Check for errors
!--------------------
DO JRET=1,2
  IF (IFILE_ID==0 .OR. IRET(JRET).NE.NF_NOERR) KRESP=1
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFL0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFL0_OL
!
!     #############################################################
      SUBROUTINE WRITE_SURFX0_TIME_OL(PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITEX1* - routine to fill a real 1D array for the externalised surface 
!
USE MODD_IO_SURF_OL, ONLY: XSTARTW, XTYPE
USE MODD_OL_FILEID,  ONLY: XID_SURF, XID_NATURE, XID_SEA, XID_WATER, XID_TOWN 
!
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
INCLUDE "netcdf.inc"
!
!*      0.1   Declarations of arguments
!
REAL,                INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN) :: HCOMMENT
!
!*      0.2   Declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX0_TIME_OL',0,ZHOOK_HANDLE)
!
KRESP=0
!
IF (XTYPE==1) THEN
  CALL WRITE_TIME_DIM(XID_SURF)
ELSEIF (XTYPE==2) THEN
  CALL WRITE_TIME_DIM(XID_NATURE)
ELSEIF (XTYPE==3) THEN
  CALL WRITE_TIME_DIM(XID_SEA)
ELSEIF (XTYPE==4) THEN
  CALL WRITE_TIME_DIM(XID_WATER)
ELSEIF (XTYPE==5) THEN
  CALL WRITE_TIME_DIM(XID_TOWN)
ENDIF
!
KRESP = 0
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX0_TIME_OL',1,ZHOOK_HANDLE)
!
 CONTAINS
!
SUBROUTINE WRITE_TIME_DIM(PTAB)
!
INTEGER, DIMENSION(:), INTENT(IN) :: PTAB
!
INTEGER :: IVAR_ID
INTEGER :: JRET, JJ       !loop index
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX0_TIME_OL:WRITE_TIME_DIM',0,ZHOOK_HANDLE)
!
IF (PTAB(1).NE.0 .AND. XTYPE.NE.1) THEN
  !
  JRET = NF_INQ_VARID(PTAB(1),'time',IVAR_ID)
  IF (JRET.EQ.NF_NOERR) THEN
    JRET = NF_PUT_VARS_DOUBLE(PTAB(1),IVAR_ID,XSTARTW,1,1,PFIELD)
    CALL HANDLE_ERR(JRET,'time')
  ENDIF
  !
ENDIF
!  
DO JJ=2,SIZE(PTAB)
  !
  IF (PTAB(JJ).NE.PTAB(JJ-1) .AND. PTAB(JJ).NE.0) THEN
    JRET = NF_INQ_VARID(PTAB(JJ),'time',IVAR_ID)
    IF (JRET.EQ.NF_NOERR) THEN
      JRET = NF_PUT_VARS_DOUBLE(PTAB(JJ),IVAR_ID,XSTARTW,1,1,PFIELD)
      CALL HANDLE_ERR(JRET,'time')
    ENDIF
  ENDIF
  ! 
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX0_TIME_OL:WRITE_TIME_DIM',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_TIME_DIM
!
END SUBROUTINE WRITE_SURFX0_TIME_OL
!
!     #############################################################
      SUBROUTINE WRITE_SURFX1_OL (&
                                  HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEX1* - routine to fill a real 1D array for the externalised surface 
!  
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_WRITE, &
                            XTIME_COMM_WRITE, WLOG_MPI
USE MODD_SURFEX_OMP, ONLY : CWORK0, NWORK0, NWORKVAR, NWORKB, NWORKDIMS, &
                            NWORKIDS, NWORKLEN, LWORK0
!
USE MODD_IO_SURF_OL, ONLY: LMASK, NMASK, NMASK_IGN, XSTART, &
                           XSTRIDE, LPARTW, XSTARTW, XCOUNTW
!
USE MODI_IO_BUFF
USE MODI_OL_FIND_FILE_WRITE
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
!
!
 CHARACTER(LEN=12),   INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:),  INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),  INTENT(IN) :: HCOMMENT
 CHARACTER(LEN=1),    INTENT(IN) :: HDIR     ! type of field :
                                            ! 'H' : field with
                                            !       horizontal spatial dim.
                                            ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100)    :: YNAME
INTEGER               :: IFILE_ID, IVAR_ID, JDIM, INDIMS
INTEGER               :: JRET
INTEGER               :: INFOMPI
INTEGER, DIMENSION(4) :: IDIMIDS
INTEGER, DIMENSION(4) :: IDIMLEN
INTEGER,DIMENSION(5)  :: IRET
DOUBLE PRECISION   :: XTIME0
REAL, DIMENSION(:), ALLOCATABLE :: ZTAB1D
REAL, DIMENSION(:), ALLOCATABLE :: ZWORK_IGN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX1_OL',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
IRET(:) = 0
!
!$OMP SINGLE
!
NWORKDIMS = 0
NWORKLEN(:) = 0
 CWORK0 = ""
NWORKB=0
!
 CALL IO_BUFF(&
                HREC,'W',LWORK0)
!
!$OMP END SINGLE
!
IF (LWORK0 .AND. LHOOK) CALL DR_HOOK("WRITE_SURF_OL:WRITE_SURFX1_OL",1,ZHOOK_HANDLE)
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
  ! 0. find filename
  ! -----------------
  CALL OL_FIND_FILE_WRITE(HREC,NWORK0)
  !
  IF (NWORK0 /= 0) THEN
    !
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (NWORK0,HREC,NWORKVAR)
    IRET(2)=NF_INQ_VARNDIMS(NWORK0,NWORKVAR,NWORKDIMS)
    IRET(3)=NF_INQ_VARDIMID(NWORK0,NWORKVAR,NWORKIDS(1:NWORKDIMS))
    DO JDIM=1,2
      JRET=NF_INQ_DIMLEN(NWORK0,NWORKIDS(JDIM),NWORKLEN(JDIM))
    ENDDO
    !
    IRET(4)=NF_INQ_DIMNAME(NWORK0,NWORKIDS(1),CWORK0)
    !
    ! 3. Check for errors
    !--------------------
    DO JRET=1,4
      IF (NWORK0==0 .OR. IRET(JRET).NE.NF_NOERR) NWORKB=1
    ENDDO
    !
  ENDIF
  ! 
!$OMP END SINGLE
  !
  IVAR_ID = NWORKVAR
  INDIMS = NWORKDIMS
  IDIMIDS = NWORKIDS
  IDIMLEN = NWORKLEN
  YNAME = CWORK0
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ELSE
  IVAR_ID = 0
  INDIMS = 0
  IDIMIDS(:) = 0
  IDIMLEN(:) = 0
  YNAME = ""
ENDIF
!
KRESP = NWORKB
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
!$OMP SINGLE
  CALL MPI_BCAST(NWORK0,KIND(NWORK0)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
  XTIME_COMM_WRITE = XTIME_COMM_WRITE + (MPI_WTIME() - XTIME0)
#endif
ENDIF
!
IFILE_ID = NWORK0
!
IF (IFILE_ID/=0) THEN
  !
  IF (YNAME .EQ. 'Number_of_points') THEN
    CALL WRITE_DATAX1_OL(IDIMLEN(1),INDIMS)
  ELSE
   CALL WRITE_DATAX1_OL(IDIMLEN(1)*IDIMLEN(2),INDIMS)
  ENDIF
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX1_OL',1,ZHOOK_HANDLE)
!
 CONTAINS
!
SUBROUTINE WRITE_DATAX1_OL(KDIM,KNDIMS)
!
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_UNPACK_SAME_RANK
USE MODI_HANDLE_ERR
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KDIM
INTEGER, INTENT(IN) :: KNDIMS
!
REAL, DIMENSION(KDIM) :: ZTAB1D
REAL, DIMENSION(KDIM) :: ZWORK_IGN
!
INTEGER,   DIMENSION(KNDIMS) :: ISTART
INTEGER,   DIMENSION(KNDIMS) :: ICOUNT
INTEGER,   DIMENSION(KNDIMS) :: ISTRIDE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX1_OL:WRITE_DATAX1_OL',0,ZHOOK_HANDLE)
!
IF(.NOT.ALLOCATED(NMASK_IGN))THEN
  IF (LMASK) THEN
    CALL GATHER_AND_WRITE_MPI(PFIELD,ZTAB1D,NMASK)
  ELSE 
    CALL GATHER_AND_WRITE_MPI(PFIELD,ZTAB1D)
  ENDIF
ELSE
  !ign grid 
  IF (LMASK) THEN
    CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK_IGN(1:SIZE(NMASK_IGN)),NMASK)
  ELSE 
    CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK_IGN(1:SIZE(NMASK_IGN)))
  ENDIF
  CALL UNPACK_SAME_RANK(NMASK_IGN,ZWORK_IGN(1:SIZE(NMASK_IGN)),ZTAB1D)
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
  IF  (LPARTW) THEN
    ! write partially a time-matrix. 
    ! Have to find which of the dimension is the time dimension
    DO  JDIM=1,KNDIMS
      JRET=NF_INQ_DIMNAME(IFILE_ID,IDIMIDS(JDIM),YNAME)
      IF ((INDEX(YNAME,'time') > 0).OR.(INDEX(YNAME,'TIME') >0) &
            .OR.(INDEX(YNAME,'Time')>0.)) THEN
        ISTART(JDIM)=XSTARTW
        ICOUNT(JDIM)=XCOUNTW
        ISTRIDE(JDIM)=XSTRIDE
      ELSE
        ISTART(JDIM)=1
        ICOUNT(JDIM)=IDIMLEN(JDIM)
        ISTRIDE(JDIM)=1
      ENDIF
    ENDDO
    IRET(5)=NF_PUT_VARS_DOUBLE(IFILE_ID,IVAR_ID,ISTART,ICOUNT,ISTRIDE,ZTAB1D)
  ELSE
    IRET(5)=NF_PUT_VAR_DOUBLE(IFILE_ID,IVAR_ID,ZTAB1D)
  ENDIF
  !
  CALL HANDLE_ERR(IRET(5),HREC)
  !
!$OMP END SINGLE NOWAIT
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX1_OL:WRITE_DATAX1_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_DATAX1_OL
!
END SUBROUTINE WRITE_SURFX1_OL
!
!     #############################################################
      SUBROUTINE WRITE_SURFX2_OL(HREC,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEX2* - routine to fill a real 2D array for the externalised surface 
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, XTIME_NPIO_WRITE, &
                            XTIME_COMM_WRITE
!
USE MODD_SURFEX_OMP, ONLY : CWORK0, NWORK0, NWORKVAR, NWORKB, NWORKDIMS, &
                            NWORKIDS, NWORKLEN, NBLOCK
!
USE MODD_IO_SURF_OL, ONLY: LMASK, NMASK, NMASK_IGN, XSTART, XSTRIDE, &
                           LPARTW, XSTARTW, XCOUNTW
!
USE MODI_OL_FIND_FILE_WRITE
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
 CHARACTER(LEN=12),    INTENT(IN) :: HREC     ! name of the article to be read
REAL, DIMENSION(:,:), INTENT(IN) :: PFIELD   ! array containing the data field
INTEGER,              INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),   INTENT(IN) :: HCOMMENT
 CHARACTER(LEN=1),     INTENT(IN) :: HDIR     ! type of field :
                                             ! 'H' : field with
                                             !       horizontal spatial dim.
                                             ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100)    :: YNAME
INTEGER               :: IFILE_ID, IVAR_ID, JDIM, INDIMS
INTEGER               :: JRET
INTEGER               :: INFOMPI
INTEGER, DIMENSION(4) :: IDIMIDS
INTEGER, DIMENSION(4) :: IDIMLEN
INTEGER, DIMENSION(5) :: IRET
DOUBLE PRECISION      :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX2_OL',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
!$OMP SINGLE
NWORKDIMS = 0
NWORKLEN(:) = 0
 CWORK0 = ""
!
NWORKB=0
!$OMP END SINGLE
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
!$OMP SINGLE
  !
  ! 0. find filename
  ! -----------------
  CALL OL_FIND_FILE_WRITE(HREC,NWORK0)
  !
  IF ( NWORK0 /= 0 ) THEN
    !
    ! 1. Find id of the variable
    !----------------------------
    !
    IRET(1)=NF_INQ_VARID   (NWORK0,HREC,NWORKVAR)
    IRET(2)=NF_INQ_VARNDIMS(NWORK0,NWORKVAR,NWORKDIMS) 

    IRET(3)=NF_INQ_VARDIMID(NWORK0,NWORKVAR,NWORKIDS(1:NWORKDIMS))  
    DO JDIM=1,NWORKDIMS
      JRET=NF_INQ_DIMLEN(NWORK0,NWORKIDS(JDIM),NWORKLEN(JDIM))
      IF (JRET.NE.NF_NOERR) NWORKB=1
    ENDDO
    !
    IRET(4)=NF_INQ_DIMNAME(NWORK0,NWORKIDS(1),CWORK0)
    !  
  ENDIF
  !
  DO JRET=1,4
    IF (NWORK0==0 .OR. IRET(JRET).NE.NF_NOERR) NWORKB=1
  ENDDO
  !
!$OMP END SINGLE
  !
  IVAR_ID = NWORKVAR
  INDIMS = NWORKDIMS
  IDIMIDS = NWORKIDS
  IDIMLEN = NWORKLEN
  YNAME = CWORK0
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ELSE
  IVAR_ID = 0
  INDIMS = 0
  IDIMIDS(:) = 0
  IDIMLEN(:) = SIZE(PFIELD,2)
  YNAME = "" 
ENDIF
!
!
KRESP = NWORKB
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
!$OMP SINGLE
  CALL MPI_BCAST(NWORK0,KIND(NWORK0)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
  XTIME_COMM_WRITE = XTIME_COMM_WRITE + (MPI_WTIME() - XTIME0)  
#endif
ENDIF
!
IFILE_ID = NWORK0
!
IF (IFILE_ID/=0) THEN
  !
  IF (YNAME .EQ. 'Number_of_points') THEN
    CALL WRITE_DATAX2_OL(IDIMLEN(1),IDIMLEN(2),INDIMS)
  ELSE
    CALL WRITE_DATAX2_OL(IDIMLEN(1)*IDIMLEN(2),IDIMLEN(3),INDIMS)
  ENDIF
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX2_OL',1,ZHOOK_HANDLE)
!
 CONTAINS
!
SUBROUTINE WRITE_DATAX2_OL(KDIM1,KDIM2,KNDIMS)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_UNPACK_SAME_RANK
USE MODI_HANDLE_ERR
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KDIM1
INTEGER, INTENT(IN) :: KDIM2
INTEGER, INTENT(IN) :: KNDIMS
!
REAL, DIMENSION(KDIM1,KDIM2) :: ZTAB2D    ! work array read in the file
REAL, DIMENSION(KDIM1,SIZE(PFIELD,2)) :: ZWORK_IGN ! work array read in the file
INTEGER,         DIMENSION(KNDIMS) :: ISTART
INTEGER,         DIMENSION(KNDIMS) :: ISTRIDE
INTEGER,         DIMENSION(KNDIMS) :: ICOUNT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX2_OL:WRITE_DATAX2_OL',0,ZHOOK_HANDLE)
!
ZTAB2D(:,:) = XUNDEF
!
IF(.NOT.ALLOCATED(NMASK_IGN))THEN
  IF (LMASK) THEN
    CALL GATHER_AND_WRITE_MPI(PFIELD,ZTAB2D(:,1:SIZE(PFIELD,2)),NMASK)
  ELSE 
    CALL GATHER_AND_WRITE_MPI(PFIELD,ZTAB2D(:,1:SIZE(PFIELD,2)))
  ENDIF
ELSE
  !ign grid 
  IF (LMASK) THEN
    CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK_IGN(1:SIZE(NMASK_IGN),:),NMASK)
  ELSE 
    CALL GATHER_AND_WRITE_MPI(PFIELD,ZWORK_IGN(1:SIZE(NMASK_IGN),:))
  ENDIF
  CALL UNPACK_SAME_RANK(NMASK_IGN,ZWORK_IGN(1:SIZE(NMASK_IGN),:),ZTAB2D(:,1:SIZE(PFIELD,2)))
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
  ! 2. Put variable
  !----------------------------
  IF (LPARTW) THEN
    ! write partially a time-matrix. 
    ! Have to find which of the dimension is the time dimension
    DO  JDIM=1,KNDIMS
      JRET=NF_INQ_DIMNAME(IFILE_ID,IDIMIDS(JDIM),YNAME)
      IF ((INDEX(YNAME,'time') > 0).OR.(INDEX(YNAME,'TIME') >0) &
         .OR.(INDEX(YNAME,'Time')>0.)) THEN  
        ISTART(JDIM)=XSTARTW
        ICOUNT(JDIM)=XCOUNTW
        ISTRIDE(JDIM)=XSTRIDE
      ELSE
        ISTART(JDIM)=1
        ICOUNT(JDIM)=IDIMLEN(JDIM)
        ISTRIDE(JDIM)=1
      ENDIF
    ENDDO
    IRET(5)=NF_PUT_VARS_DOUBLE(IFILE_ID,IVAR_ID,ISTART,ICOUNT,ISTRIDE,ZTAB2D)
  ELSE
    IRET(5)=NF_PUT_VAR_DOUBLE(IFILE_ID,IVAR_ID,ZTAB2D)
  ENDIF
  !    
  CALL HANDLE_ERR(IRET(5),HREC)  
  !
!$OMP END SINGLE NOWAIT
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFX2_OL:WRITE_DATAX2_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_DATAX2_OL
!
END SUBROUTINE WRITE_SURFX2_OL

!     #############################################################
      SUBROUTINE WRITE_SURFN1_OL (&
                                  HREC,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEN0* - routine to read an integer
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!
 CHARACTER(LEN=12),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER, DIMENSION(:),  INTENT(IN)  :: KFIELD   ! the integer scalar to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),     INTENT(IN)  :: HCOMMENT
 CHARACTER(LEN=1),       INTENT(IN) :: HDIR     ! type of field :
                                               ! 'H' : field with
                                               !       horizontal spatial dim.
                                               ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(KFIELD)) :: ZFIELD 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFN1_OL',0,ZHOOK_HANDLE)
!
ZFIELD=FLOAT(KFIELD)
 CALL WRITE_SURFX1_OL(&
                      HREC,ZFIELD,KRESP,HCOMMENT,HDIR)
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFN1_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFN1_OL
!
!     #############################################################
      SUBROUTINE WRITE_SURFL1_OL(HREC,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *WRITEL1* - routine to read a logical array
!    
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, XTIME_NPIO_WRITE
!
USE MODI_OL_FIND_FILE_WRITE
USE MODI_HANDLE_ERR
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
 CHARACTER(LEN=*),      INTENT(IN) :: HREC     ! name of the article to be read
LOGICAL, DIMENSION(:), INTENT(IN) :: OFIELD   ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100),    INTENT(IN) :: HCOMMENT
 CHARACTER(LEN=1),      INTENT(IN) :: HDIR     ! type of field :
                                              ! 'H' : field with
                                              !       horizontal spatial dim.
                                              ! '-' : no horizontal dim.
!*      0.2   Declarations of local variables
!
INTEGER               :: IFILE_ID, IVAR_ID, JDIM, INDIMS
INTEGER               :: JRET
INTEGER, DIMENSION(4) :: IDIMIDS
INTEGER, DIMENSION(1) :: IDIMLEN
INTEGER, DIMENSION(4) :: IRET
DOUBLE PRECISION      :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFL1_OL',0,ZHOOK_HANDLE)
!
!$OMP BARRIER
!
KRESP=0
!
IF ( NRANK==NPIO ) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif

  !
!$OMP SINGLE
  !   
  ! 0. find filename
  ! -----------------
  CALL OL_FIND_FILE_WRITE(HREC,IFILE_ID)
  ! 
  IF (IFILE_ID /= 0) THEN
    !       
    ! 1. Find id of the variable
    !----------------------------
    IRET(1)=NF_INQ_VARID   (IFILE_ID,HREC,IVAR_ID)
    IRET(2)=NF_INQ_VARNDIMS(IFILE_ID,IVAR_ID,INDIMS)
    IRET(3)=NF_INQ_VARDIMID(IFILE_ID,IVAR_ID,IDIMIDS(1:INDIMS))
    DO JDIM=1,1
      JRET=NF_INQ_DIMLEN(IFILE_ID,IDIMIDS(JDIM),IDIMLEN(JDIM))
    ENDDO  
    !
    CALL WRITE_DATAL1_OL(IDIMLEN(1))
    !
  ENDIF
  !
  ! 3. Check for errors
  !--------------------
  DO JRET=1,4
    IF (IFILE_ID==0 .OR. IRET(JRET).NE.NF_NOERR) KRESP=1
  ENDDO
  !
!$OMP END SINGLE
  !
#ifdef SFX_MPI
  XTIME_NPIO_WRITE = XTIME_NPIO_WRITE + (MPI_WTIME() - XTIME0)
#endif
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFL1_OL',1,ZHOOK_HANDLE)
!
 CONTAINS
!
SUBROUTINE WRITE_DATAL1_OL(KDIM)
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=1), DIMENSION(KDIM) :: YTAB1D  ! work array read in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFL1_OL:WRITE_DATAL1_OL',0,ZHOOK_HANDLE)
!
DO JRET=1,KDIM
  IF (OFIELD(JRET)) THEN
    YTAB1D(JRET) ='T'
  ELSE
    YTAB1D(JRET) ='F'
  ENDIF
ENDDO  
!
! 2. Put variable
!-----------------
IRET(4)=NF_PUT_VAR_TEXT(IFILE_ID,IVAR_ID,YTAB1D)
!
 CALL HANDLE_ERR(IRET(4),HREC)
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFL1_OL:WRITE_DATAL1_OL',1,ZHOOK_HANDLE)
END SUBROUTINE WRITE_DATAL1_OL
!
END SUBROUTINE WRITE_SURFL1_OL
!
!
!     #############################################################
      SUBROUTINE WRITE_SURFT0_OL(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *WRITET0* - routine to read a NETCDF  date_time scalar
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_OL_FIND_FILE_WRITE
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
 CHARACTER(LEN=12),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN)  :: KYEAR    ! year
INTEGER,            INTENT(IN)  :: KMONTH   ! month
INTEGER,            INTENT(IN)  :: KDAY     ! day
REAL,               INTENT(IN)  :: PTIME    ! time
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=100), INTENT(IN)  :: HCOMMENT
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=12) :: YRECFM    ! Name of the article to be written
INTEGER :: IFILE_ID, IVAR_ID, JRET, JWRK
INTEGER :: JLEN
INTEGER,DIMENSION(3) :: ITDATE  ! work array read in the file
INTEGER,DIMENSION(4) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFT0_OL',0,ZHOOK_HANDLE)
!
KRESP=0
!
DO JWRK=1,2
  !
  IF (JWRK == 1) THEN 
    YRECFM=TRIM(HREC)//'-TDATE'
    JLEN=3
  ELSE
    YRECFM=TRIM(HREC)//'-TIME'
    JLEN=1
  ENDIF
  ! 0. find filename
  ! -----------------
  CALL OL_FIND_FILE_WRITE(YRECFM,IFILE_ID)
  !
  IF (IFILE_ID /= 0) THEN
    !    
    ! 1. Find id of the variable
    !----------------------------
    IRET(1+JWRK*2)=NF_INQ_VARID   (IFILE_ID,YRECFM,IVAR_ID)
    IF (JWRK == 1) THEN 
      ITDATE(1)=KYEAR
      ITDATE(2)=KMONTH
      ITDATE(3)=KDAY
      IRET(JWRK)=NF_PUT_VAR_INT(IFILE_ID,IVAR_ID,ITDATE)
    ELSE
      JLEN=1
      IRET(JWRK)=NF_PUT_VAR_DOUBLE(IFILE_ID,IVAR_ID,PTIME)
    ENDIF
  ENDIF
ENDDO
!
! 3. Check for errors
!--------------------
DO JRET=1,4
  IF (IFILE_ID==0.OR.IRET(JRET).NE.NF_NOERR) KRESP=1
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_SURF_OL:WRITE_SURFT0_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_SURFT0_OL

END MODULE MODE_WRITE_SURF_OL
