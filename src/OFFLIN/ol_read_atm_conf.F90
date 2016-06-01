!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE OL_READ_ATM_CONF (YSC, &
                             HSURF_FILETYPE, HFORCING_FILETYPE,  &
                              PDURATION,                          &
                              PTSTEP_FORC, KNI, KYEAR,KMONTH,     &
                              KDAY, PTIME, PLAT, PLON,            &
                              PZS, PZREF, PUREF                   )  
!
!==================================================================
!!****  *OL_READ_ATM_CONF* - Initialization routine
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified by P. Le Moigne (04/2005): cleaning and checking
!!      Modified by P. Le Moigne (04/2006): init_io_surf for nature
!!                  with GTMSK to read dimensions.
!==================================================================
!
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODI_OL_READ_ATM_CONF_NETCDF
USE MODI_OL_READ_ATM_CONF_ASCII
USE MODD_SURF_CONF,      ONLY : CPROGNAME
!==================================================================
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6), INTENT(IN)  :: HSURF_FILETYPE
 CHARACTER(LEN=6), INTENT(IN)  :: HFORCING_FILETYPE
INTEGER,          INTENT(OUT) :: KNI
INTEGER,          INTENT(OUT) :: KYEAR, KMONTH, KDAY
REAL,             INTENT(OUT) :: PDURATION,PTSTEP_FORC
REAL,             INTENT(OUT) :: PTIME
REAL, DIMENSION(:),  POINTER  :: PLAT, PLON
REAL, DIMENSION(:),  POINTER  :: PZS
REAL, DIMENSION(:),  POINTER  :: PZREF, PUREF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!==================================================================
!
IF (LHOOK) CALL DR_HOOK('OL_READ_ATM_CONF',0,ZHOOK_HANDLE)
 CPROGNAME = HSURF_FILETYPE
!
IF (HFORCING_FILETYPE == 'NETCDF') THEN
!
 CALL OL_READ_ATM_CONF_NETCDF(YSC, &
                              HSURF_FILETYPE,                     &
                                PDURATION,                          &
                                PTSTEP_FORC, KNI, KYEAR,KMONTH,     &
                                KDAY, PTIME, PLAT, PLON,            &
                                PZS, PZREF, PUREF                   )  
!
ELSE IF (HFORCING_FILETYPE == 'ASCII ' .OR. HFORCING_FILETYPE == 'BINARY') THEN
!
 CALL OL_READ_ATM_CONF_ASCII(YSC, &
                              HSURF_FILETYPE,HFORCING_FILETYPE,   &
                                PDURATION,                          &
                                PTSTEP_FORC, KNI, KYEAR,KMONTH,     &
                                KDAY, PTIME, PLAT, PLON,            &
                                PZS, PZREF, PUREF                   )  
!                              
ENDIF
IF (LHOOK) CALL DR_HOOK('OL_READ_ATM_CONF',1,ZHOOK_HANDLE)
!
!==================================================================
!
END SUBROUTINE OL_READ_ATM_CONF
