      SUBROUTINE SC_CONTR ( MODE_STR, HEB_IN, HEB_PRS, HEB_SC, &
     &                      HEB_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SC_CONTR
! *                                                                      *
! *  ### 06-FEB-2016    SC_CONTR   v1.0 (c)  L. Petrov  19-FEB-2016 ###  *
! *                                                                      *
! ************************************************************************
      USE ISO_C_BINDING
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      CHARACTER  MODE_STR*(*)
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_PRS, HEB_SC, HEB_OUT
      INTEGER*4  DIM4, IUER
      REAL*4     PRES
      INTEGER*4  IND_LON, IND_LAT, NC, J1, J2, J3, IER
!
      HEB_OUT = HEB_IN
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6831, IUER, 'SC_CONTR', 'Failed to allocate '// &
     &         'dynamic memory for array HEB_OUT%VAL' ) 
           CALL EXIT ( 1 )
      END IF
!
      HEB_OUT%VAL = HEB_IN%VAL
      HEB_OUT%HISTORY = HEB_PRS%HISTORY
      HEB_OUT%SOURCE = HEB_PRS%SOURCE
!
! --- The 4th dimension is 1 in a case of sampling correction is applied to the loading
! --- at a certain time epoch. 
! --- If sampling correction is applied to the regression model, then the 4th
! --- dimesion runs over regression model components
!
      DO 410 J1=1,HEB_OUT%DIMS(4)
         DO 420 J2=1,HEB_OUT%DIMS(2)
            DO 430 J3=1,HEB_OUT%DIMS(1)
               IND_LAT = J2
               IND_LON = J3
               IF ( MODE_STR == 'ocean' .OR. MODE_STR == 'ocean0' .OR. &
     &              MODE_STR == 'OCEAN' .OR. MODE_STR == 'OCEAN0'      ) THEN
                    IF ( HEB_PRS%VAL(IND_LON,IND_LAT,1,1) > HEB_PRS%FILL_VALUE/2.0 ) THEN
!
! ---------------------- Fill value? Means land
!
                         PRES = 0.0
                       ELSE 
!
! ---------------------- For the case of ocean HEB_PRS%VAL bears the sea height.
! ---------------------- We convert it to the surface pressure. 
! ---------------------- NB: sign is negative because we apply land-sea mask that has 1 
! ---------------------- for land and 0 for ocean. For the case of ocean loading the 
! ---------------------- land-sea mask should be applied as (1-LS).
!
                         PRES = -MALO__SW_DENS*MALO__GRAV*HEB_PRS%VAL(IND_LON,IND_LAT,J1,1)
                    END IF
                 ELSE 
!
! ----------------- Case of land
!
                    PRES = HEB_PRS%VAL(IND_LON,IND_LAT,J1,1)
               END IF
               IF ( MODE_STR == 'ocean0' .OR. MODE_STR == 'OCEAN0' ) THEN
                    HEB_OUT%VAL(IND_LON,IND_LAT,1:3,J1) = 0.0
               END IF
!
! ------------ Apply sampling correction
!
               HEB_OUT%VAL(IND_LON,IND_LAT,1:3,J1) = HEB_OUT%VAL(IND_LON,IND_LAT,1:3,J1) &
     &                     + PRES*HEB_SC%VAL(IND_LON,IND_LAT,1:3,1) 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SC_CONTR  !#!#
