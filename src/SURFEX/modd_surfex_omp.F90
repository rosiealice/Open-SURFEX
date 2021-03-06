!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_SURFEX_OMP
!     ######################
!
!!****  *MODD_SURFEX_OMP
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       26/06/12
!!      Modified    11/2013 by J.Escobar :add !$ to inhibit completly omp
!!                                 dependency
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef AIX64 
 USE OMP_LIB
#endif
!
IMPLICIT NONE
!
#ifndef AIX64
  INCLUDE 'omp_lib.h'
#endif
!
!RJ: this broke non openmp version before
!RJ: OMP_GET_THREAD_NUM() returns 0 for first omp thread
!RJ: OMP_GET_NUM_THREADS() returns 1 for omp thread count
#ifdef RJ_OFIX
INTEGER :: NBLOCKTOT = 1
INTEGER :: NBLOCK = 0
#else
INTEGER :: NBLOCKTOT = 1
INTEGER :: NBLOCK = 1
#endif
!$OMP THREADPRIVATE(NBLOCK)
INTEGER :: NINDX1SFX = 1
!$OMP THREADPRIVATE(NINDX1SFX)
INTEGER :: NINDX2SFX = 1
!$OMP THREADPRIVATE(NINDX2SFX)
INTEGER :: IDC = 0
!
 CHARACTER(LEN=100) :: CWORK0, CWORKB
LOGICAL :: LWORK0
LOGICAL, DIMENSION(:), POINTER :: LWORKD
INTEGER :: NWORK0, NWORKVAR, NWORKB, NWORKDIMS
INTEGER, DIMENSION(4) :: NWORKLEN
INTEGER, DIMENSION(4) :: NWORKIDS
INTEGER, DIMENSION(:), POINTER :: NWORK_FULL
INTEGER, DIMENSION(:), POINTER :: NWORKD
INTEGER, DIMENSION(:), POINTER :: NWORK
INTEGER, DIMENSION(:,:), POINTER :: NWORK2_FULL
INTEGER, DIMENSION(:,:), POINTER :: NWORKD2
INTEGER, DIMENSION(:,:), POINTER :: NWORK2
INTEGER, DIMENSION(:,:,:), POINTER :: NWORKD3
REAL :: XWORK0
REAL, DIMENSION(:), POINTER :: XWORK_FULL
REAL, DIMENSION(:), POINTER :: XWORKD
REAL, DIMENSION(:), POINTER :: XWORK
REAL, DIMENSION(:,:), POINTER :: XWORK2_FULL
REAL, DIMENSION(:,:), POINTER :: XWORKD2
REAL, DIMENSION(:,:), POINTER :: XWORK2
REAL, DIMENSION(:,:,:), POINTER :: XWORKD3
REAL, DIMENSION(:,:,:), POINTER :: XWORK3
!
 CONTAINS
!
!*********************************************************
!
SUBROUTINE INIT_DIM(KSIZE_OMP,KBLOCK,KKPROMA,KINDX1,KINDX2)
!
!RJ: work around to detect serial regions
!RJ INTEGER, DIMENSION(0:NBLOCKTOT-1), INTENT(IN) :: KSIZE_OMP
INTEGER, DIMENSION(0:), INTENT(IN) :: KSIZE_OMP
INTEGER, INTENT(IN) :: KBLOCK
INTEGER, INTENT(OUT) :: KKPROMA
INTEGER, INTENT(OUT) :: KINDX1
INTEGER, INTENT(OUT) :: KINDX2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODD_SURFEX_OMP:INIT_DIM',0,ZHOOK_HANDLE)
!
IF((KBLOCK<SIZE(KSIZE_OMP)).AND.(KBLOCK<NBLOCKTOT)) THEN
KKPROMA = KSIZE_OMP(KBLOCK)
KINDX2  = SUM(KSIZE_OMP(0:KBLOCK))
KINDX1   = KINDX2 - KKPROMA + 1
ELSE
write(0,*) "Warning[OMP]: dummy dim init for KBLOCK=",KBLOCK
KKPROMA=0
KINDX2=-666
KINDX1=-666
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODD_SURFEX_OMP:INIT_DIM',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_DIM
!
!*********************************************************
!
SUBROUTINE RESET_DIM(KNI,KKPROMA,KINDX1,KINDX2)
!
INTEGER, INTENT(IN) :: KNI
INTEGER, INTENT(OUT) :: KKPROMA
INTEGER, INTENT(OUT) :: KINDX1
INTEGER, INTENT(OUT) :: KINDX2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODD_SURFEX_OMP:RESET_DIM',0,ZHOOK_HANDLE)
!
KKPROMA = KNI
KINDX2 = KNI
KINDX1 = 1
!
IF (LHOOK) CALL DR_HOOK('MODD_SURFEX_OMP:RESET_DIM',1,ZHOOK_HANDLE)
!
END SUBROUTINE RESET_DIM
!
!*********************************************************
!
SUBROUTINE PLOG_OMP(HLOG,RLOG,KLOG,KLOG2,OLOG)
!
 CHARACTER(LEN=*), INTENT(IN) :: HLOG
REAL, INTENT(IN),OPTIONAL :: RLOG
INTEGER, INTENT(IN), OPTIONAL :: KLOG
INTEGER, INTENT(IN), OPTIONAL :: KLOG2
LOGICAL, INTENT(IN), OPTIONAL :: OLOG
!
INTEGER :: ME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODD_SURFEX_OMP:PLOG_OMP',0,ZHOOK_HANDLE)
!
!$ ME = OMP_GET_THREAD_NUM()
!
IF (PRESENT(OLOG)) THEN
  IF (PRESENT(RLOG)) THEN
    IF (PRESENT(KLOG)) THEN
      IF (PRESENT(KLOG2)) THEN
        PRINT*,ME, HLOG, KLOG, KLOG2, RLOG, OLOG
      ELSE
        PRINT*,ME, HLOG, KLOG, RLOG, OLOG
      ENDIF
    ELSE
      PRINT*,ME, HLOG, RLOG, OLOG
    ENDIF
  ELSEIF (PRESENT(KLOG)) THEN
    IF (PRESENT(KLOG2)) THEN
      PRINT*,ME, HLOG, KLOG, KLOG2, OLOG
    ELSE
      PRINT*,ME, HLOG, KLOG, OLOG
    ENDIF
  ELSE
    PRINT*,ME, HLOG, OLOG
  ENDIF
ELSEIF (PRESENT(RLOG)) THEN
  IF (PRESENT(KLOG)) THEN
    IF (PRESENT(KLOG2)) THEN
      PRINT*,ME, HLOG, KLOG, KLOG2, RLOG
    ELSE
      PRINT*,ME, HLOG, KLOG, RLOG
    ENDIF
  ELSE
    PRINT*,ME, HLOG, RLOG
  ENDIF
ELSEIF (PRESENT(KLOG)) THEN
  IF (PRESENT(KLOG2)) THEN
    PRINT*,ME, HLOG, KLOG, KLOG2
  ELSE
    PRINT*,ME, HLOG, KLOG
  ENDIF
ELSE
  PRINT*,ME, HLOG
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODD_SURFEX_OMP:PLOG_OMP',1,ZHOOK_HANDLE)
!
END SUBROUTINE PLOG_OMP
!
!*********************************************************
!
END MODULE MODD_SURFEX_OMP

