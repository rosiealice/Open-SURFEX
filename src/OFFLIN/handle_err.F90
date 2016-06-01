!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
        SUBROUTINE HANDLE_ERR(IRET,HNAME)

!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
implicit none

INCLUDE "netcdf.inc"

integer,         intent(in)     :: iret
character(LEN=*),intent(in)     :: HNAME
REAL(KIND=JPRB) :: ZHOOK_HANDLE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - 

        IF (LHOOK) CALL DR_HOOK('HANDLE_ERR',0,ZHOOK_HANDLE)
        if (iret /= NF_NOERR) then
           write(*,*)'HANDLE_ERR: ',nf_strerror(iret)
           CALL ABOR1_SFX('HANFLE_ERR: ABORTING PROGRAM TO WRITE A NETCDF FILE: '//HNAME)
        endif
IF (LHOOK) CALL DR_HOOK('HANDLE_ERR',1,ZHOOK_HANDLE)

END SUBROUTINE HANDLE_ERR
