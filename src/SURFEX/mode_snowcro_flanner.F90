!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_SNOWCRO_FLANNER

!!****  SNOWCRO_FLANNER - read "drdt_bst_fit_60.nc" file, which containes parameters from Flanner and Zender, 2006
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
!!     C. Carmagnola
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2013
!
USE MODD_SURFEX_OMP, ONLY : NBLOCK
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NPROC, NCOMM
!
USE MODI_ABOR1_SFX

USE MODE_READ_CDF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
#ifdef AIX64
!$ USE OMP_LIB
#endif
!
IMPLICIT NONE
!
INCLUDE 'netcdf.inc'
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
#ifndef AIX64
!$ INCLUDE 'omp_lib.h'
#endif
!
 CONTAINS
!
!------------------------------------------------------------------
!
SUBROUTINE READ_FZ06(HFILE)
!
USE MODD_SNOW_METAMO, ONLY : NID_FILE, XDRDT0, XTAU, XKAPPA
!
IMPLICIT NONE
!
!*      1.    declarations of arguments
!
 CHARACTER(LEN=18),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=5),DIMENSION(3),PARAMETER :: HVARNAME=(/'drdt0','tau  ','kappa'/)
!
!*      2.    declarations of local variables
!
INTEGER :: INFOMPI
INTEGER :: IERROR !error status
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SNOWCRO_FLANNER',0,ZHOOK_HANDLE)
!
!*      3.     Reading of field
!
! Open netcdf file
!
IF (NRANK==NPIO) THEN
!$OMP SINGLE
  IERROR = NF_OPEN(HFILE,NF_NOWRITE,NID_FILE)
  CALL HANDLE_ERR_CDF(IERROR,"can't open file "//TRIM(HFILE))
!$OMP END SINGLE
ENDIF
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
!$OMP SINGLE
  CALL MPI_BCAST(NID_FILE,KIND(NID_FILE)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
#endif
ENDIF
!
 CALL READ_VAR_FZ06(NID_FILE,HVARNAME(1),XDRDT0)
 CALL READ_VAR_FZ06(NID_FILE,HVARNAME(2),XTAU)
 CALL READ_VAR_FZ06(NID_FILE,HVARNAME(3),XKAPPA)
!
IF (NRANK==NPIO) THEN
!$OMP SINGLE
  ! Close netcdf file
  IERROR=NF_CLOSE(NID_FILE)
!$OMP END SINGLE
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SNOWCRO_FLANNER',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_FZ06
!------------------------------------------------------------------
SUBROUTINE READ_VAR_FZ06(ID_FILE,HSURF,PVAR)
!
USE MODD_SNOW_METAMO, ONLY : NVARDIMS, NLENDIM1, NLENDIM2, &
                             NLENDIM3, NID_VAR
!
IMPLICIT NONE
!
INTEGER,INTENT(IN) :: ID_FILE
 CHARACTER(LEN=5),INTENT(IN) :: HSURF
REAL, DIMENSION(:,:,:), POINTER :: PVAR
!
INTEGER :: INFOMPI
INTEGER, DIMENSION(:), ALLOCATABLE :: IVARDIMSID
!
INTEGER :: IERROR !error status
!
IF (NRANK==NPIO) THEN
!$OMP SINGLE
  ! Look for variable ID
  IERROR = NF_INQ_VARID(ID_FILE,TRIM(HSURF),NID_VAR)
  CALL HANDLE_ERR_CDF(IERROR,"can't find variable "//TRIM(HSURF))
  !
  ! Number of dimensions
  IERROR = NF_INQ_VARNDIMS(ID_FILE,NID_VAR,NVARDIMS)
  IF ( IERROR/=NF_NOERR ) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions number")
  !
  ! Id of dimensions
  ALLOCATE(IVARDIMSID(NVARDIMS))
  IERROR = NF_INQ_VARDIMID(ID_FILE,NID_VAR,IVARDIMSID)
  IF ( IERROR/=NF_NOERR ) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions ids")
  !
  SELECT CASE (NVARDIMS)
    !
    CASE (3)
      IERROR = NF_INQ_DIMLEN(ID_FILE,IVARDIMSID(1),NLENDIM1)
      IF ( IERROR/=NF_NOERR ) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")
      IERROR = NF_INQ_DIMLEN(ID_FILE,IVARDIMSID(2),NLENDIM2)
      IF ( IERROR/=NF_NOERR ) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")
      IERROR = NF_INQ_DIMLEN(ID_FILE,IVARDIMSID(3),NLENDIM3)
      IF ( IERROR/=NF_NOERR ) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")
      !
    CASE DEFAULT
      CALL ABOR1_SFX('SNOWCRO_FLANNER: incorrect number of dimensions for variable '//TRIM(HSURF))
    !
  END SELECT
!$OMP END SINGLE
ENDIF
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
!$OMP SINGLE
  CALL MPI_BCAST(NLENDIM1,KIND(NLENDIM1)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
  CALL MPI_BCAST(NLENDIM2,KIND(NLENDIM2)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
  CALL MPI_BCAST(NLENDIM3,KIND(NLENDIM3)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
#endif
ENDIF
!
!$OMP SINGLE
ALLOCATE(PVAR(NLENDIM1,NLENDIM2,NLENDIM3))
!$OMP END SINGLE
!
IF (NRANK==NPIO) THEN
!$OMP SINGLE
  ! Read 3D variable
  IERROR = NF_GET_VAR_DOUBLE(ID_FILE,NID_VAR,PVAR)
  CALL HANDLE_ERR_CDF(IERROR,"can't read variable "//TRIM(HSURF))
!$OMP END SINGLE
ENDIF
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
!$OMP SINGLE
  CALL MPI_BCAST(PVAR,KIND(PVAR)*SIZE(PVAR)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)
!$OMP END SINGLE
#endif
ENDIF
!
END SUBROUTINE READ_VAR_FZ06
!------------------------------------------------------------------
END MODULE MODE_SNOWCRO_FLANNER
