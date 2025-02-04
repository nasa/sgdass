      SUBROUTINE VTD_OBS ( SOU_NAM, STA1_NAM, STA2_NAM, MJD, TAI, VTD, &
     &                     TAU_GR, TAU_PH, RATE_PH, ACCL_PH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_OBS 
! *                                                                      *
! *  ### 27-JAN-2004     VTD_OBS   v1.0 (c)  L. Petrov  27-JAN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      CHARACTER  SOU_NAM*8, STA1_NAM*8, STA2_NAM*8
      INTEGER*4  MJD, IUER
      REAL*8     TAI, TAU_GR, TAU_PH, RATE_PH, ACCL_PH
      TYPE     ( VTD__TYPE ) :: VTD
      REAL*8     AXOF_CRS(3,2)
      INTEGER*4  ISTA(2), ISOU, J1, J2, J3, IER
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_MOMENT ( MJD, TAI, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2211, IUER, 'VTD_OBS', 'Error in an attempt '// &
     &         'to compute station-independent time varying intermediate '// &
     &         'quantities' )
           RETURN 
      END IF
!
      ISTA(1) = 0
      ISTA(2) = 0
      DO 410 J1=1,VTD%L_STA
         IF ( VTD%STA(J1)%IVS_NAME .EQ. STA1_NAM ) THEN
              ISTA(1) = J1
         END IF
         IF ( VTD%STA(J1)%IVS_NAME .EQ. STA2_NAM ) THEN
              ISTA(2) = J1
         END IF
 410  CONTINUE 
!
      IF ( ISTA(1) .EQ. 0 ) THEN
           CALL ERR_LOG ( 2411, IUER, 'VTD_OBS', 'Station '//STA1_NAM// &
     &         'was not found in the list of stations' )
           RETURN 
      END IF
!
      IF ( ISTA(2) .EQ. 0 ) THEN
           CALL ERR_LOG ( 2412, IUER, 'VTD_OBS', 'Station '//STA2_NAM// &
     &         'was not found in the list of stations' )
           RETURN 
      END IF
!
      ISOU = 0
      DO 420 J2=1,VTD%L_SOU
         IF ( VTD%SOU(J2)%IVS_NAME .EQ. SOU_NAM ) THEN
              ISOU = J2
         END IF
 420  CONTINUE 
!
      IF ( ISOU .EQ. 0 ) THEN
           CALL ERR_LOG ( 2413, IUER, 'VTD_OBS', 'Source '//SOU_NAM// &
     &         'was not found in the list of sources' )
           RETURN 
      END IF
!
      DO 430 J3=1,2
         IF ( DABS ( VTD%STA(ISTA(J3))%AXIS_OFFSET ) .LT. 1.D-4 ) THEN
              CALL NOUT_R8 ( 3, AXOF_CRS(1,J3) )
           ELSE 
!
! ----------- Compute antenna axis offset
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_AXOF ( VTD, ISTA(J3), ISOU, AXOF_CRS(1,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2414, IUER, 'VTD_OBS', 'Error in an '// &
     &                 'attempt to compute antenna axis offset for station '// &
     &                  VTD%STA(J3)%IVS_NAME )
                    RETURN 
             END IF
           WRITE ( 6, * ) VTD%STA(ISTA(J3))%IVS_NAME, &
     &                    ' AXOF=', AXOF_CRS(1,J3), &
     &                    AXOF_CRS(2,J3), AXOF_CRS(3,J3), &
     &                    ' OFFS=',VTD%STA(ISTA(J3))%AXIS_OFFSET  ! %%%%%%%
         END IF
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_OBS
