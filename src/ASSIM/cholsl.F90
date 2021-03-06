!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE CHOLSL(N,A,P,B,X)

 USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
 USE PARKIND1  ,ONLY : JPRB

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: N
 REAL, DIMENSION (N,N),  INTENT(IN) :: A
 REAL, DIMENSION (N),  INTENT(IN)   :: P,B
 REAL, DIMENSION (N), INTENT(INOUT) :: X
 INTEGER :: I
 REAL(KIND=JPRB) :: ZHOOK_HANDLE

 IF (LHOOK) CALL DR_HOOK('CHOLSL',0,ZHOOK_HANDLE)

 DO I=1,N
   X(I) = (B(I) - DOT_PRODUCT(A(I,1:I-1),X(1:I-1)))/P(I)
 ENDDO
 DO I=N,1,-1
   X(I) = (X(I) - DOT_PRODUCT(A(I+1:N,I),X(I+1:N)))/P(I)
 ENDDO

 IF (LHOOK) CALL DR_HOOK('CHOLSL',1,ZHOOK_HANDLE)

END SUBROUTINE CHOLSL
