      SUBROUTINE MALO_SHC_VGEP ( MALO, AGRA, DEG_SPHE, MALO_OUTPUT, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_SHC_VGEP  just unscales scaled sphercial harmonics   *
! *   transform of the surface pressure field.                           *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 22-OCT-2014  MALO_SHC_VGEP  v1.1 (d)  L. Petrov  10-JAN-2026 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'malo.i'
      INCLUDE   'agra.i'
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( AGRA__TYPE ) :: AGRA
      CHARACTER  MALO_OUTPUT*(*)
      CHARACTER  STR*128
      REAL*8     NEW_COEF, OLD_COEF
      INTEGER*4  DEG_SPHE, IVRB, IUER
      INTEGER*4  J1, J2, J3, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      AGRA%L_DEG = MALO__VGEP_DEG
      AGRA%L_EPC = 1
      AGRA%L_COE = AGRA%L_EPC*((AGRA%L_DEG+1)*(AGRA%L_DEG+2))/2
      AGRA%MJD_BEG  = MALO%MJD_BEG
      AGRA%SEC_BEG  = MALO%TAI_BEG
      AGRA%MJD_END  = AGRA%MJD_BEG  
      AGRA%SEC_END  = MALO%TAI_BEG + MALO%TIM_STEP
      AGRA%INTERVAL = MALO%TIM_STEP
!
      ALLOCATE ( AGRA%STOKES(2,0:AGRA%L_DEG,0:AGRA%L_DEG,AGRA%L_EPC), STAT=IER )
      IF ( IER .NE. 0 )  THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 2*AGRA%L_DEG*AGRA%L_DEG*AGRA%L_EPC, STR )
           CALL ERR_LOG ( 6731, IUER, 'MALO_SHC_VGEP', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!    
      DO 410 J1=0,MALO__VGEP_DEG  ! order
         DO 420 J2=J1,MALO__VGEP_DEG ! degree
            IF ( J2 == 0 ) THEN
                 AGRA%STOKES(1:2,J2,J1,1) = 0.0D0
              ELSE 
                 DO 430 J3=1,2
                    OLD_COEF = 3.0D0*MALO%LOVE(J2,MALO__H)/(2*J2+1)/(MALO__DENS*MALO__GRAV)
                    NEW_COEF = 3.0D0/(4.0D0*PI__NUM)/(REA__WGS84*MALO__DENS*MALO__GRAV)* &
     &                         (1.0D0 + MALO%LOVE(J2,MALO__K))/(2*J2+1)
                    AGRA%STOKES(J3,J2,J1,1) = MALO%SPH(J3,J1,J2,1,1)*NEW_COEF/OLD_COEF
 430             CONTINUE 
!! if ( j2 == 1 ) write ( 6, * ) 'AGRA j2= ', int2(j2), ' j1= ', int2(j1), ' stok= ', sngl(AGRA%STOKES(1:2,J2,J1,1))
!!   write ( 6, * ) ' j2=', int2(j2), ' j1= ', int2(j1), ' stok= ', malo%sph(1:2,j1,j2,1,1)
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      OLD_COEF = 3.0D0*MALO%LOVE(1,MALO__H)/3/(MALO__DENS*MALO__GRAV)
      NEW_COEF = 3.0D0/(4.0D0*PI__NUM)/(REA__WGS84*MALO__DENS*MALO__GRAV)/3
!
      AGRA%STOKES_10    = MALO%SPH(1,0,1,1,1)*NEW_COEF/OLD_COEF
      AGRA%STOKES_11(1) = MALO%SPH(1,1,1,1,1)*NEW_COEF/OLD_COEF
      AGRA%STOKES_11(2) = MALO%SPH(2,1,1,1,1)*NEW_COEF/OLD_COEF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_SHC_VGEP   !#!  
