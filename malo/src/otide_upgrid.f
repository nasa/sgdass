      SUBROUTINE OTIDE_UPGRID ( NLON, NLAT, NCMP, BWAV, EWAV, NWAV, LLON, &
     &                          LLAT, FILL_VALUE, HEB_LS, ARRIN_R4, &
     &                          ARROUT_R4, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine OTIDE_UPGRID
! *                                                                      *
! *  ### 28-MAY-2014  OTIDE_UPGRID  v1.2 (c)  L. Petrov  10-JUN-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_LS
      TYPE     ( HEB__TYPE ) :: HEBIN, HEB_MASK, HEBOUT
      REAL*4     FILL_VALUE
      INTEGER*4  NLON, NLAT, NCMP, BWAV, EWAV, NWAV, LLON, LLAT, &
     &           MSN, MSF, MST, IVRB, IUER
      REAL*4     EPS_LS, HEI_MAX     
      PARAMETER  ( EPS_LS  = 0.002 )
      PARAMETER  ( HEI_MAX = 30.0  )
      REAL*4, TARGET :: ARRIN_R4(NLON,NLAT,NCMP,EWAV), ARROUT_R4(LLON,LLAT,NCMP,EWAV)
      INTEGER*4  J1, J2, J3, J4, IWAV, IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      IF ( LLON == NLON .AND. LLAT == NLAT ) THEN
           ARROUT_R4 = ARRIN_R4
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      HEBIN%DIMS(1) = NLON
      HEBIN%DIMS(2) = NLAT
      HEBIN%DIMS(3) = 1
      HEBIN%DIMS(4) = 1
!
      HEBOUT%DIMS(1) = LLON
      HEBOUT%DIMS(2) = LLAT
      HEBOUT%DIMS(3) = 1
      HEBOUT%DIMS(4) = 1      
!
      CALL NOUT ( SIZEOF(HEB_MASK), HEB_MASK )
      HEB_MASK%VAL => NULL()
!
      IF ( FLOAT(LLON)/NLON .GE. 10 ) THEN
           MSF = 8  ! Case of GOT410c with 0.25x0.25 deg grid
           MSN = 2
        ELSE
           MSF = 32 ! Case of FES2014b witn 1/16 x 1/16 deg grid
           MSN = 3
      END IF      
      MST = 1
!
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, 210 ) NLON, NLAT, LLON, LLAT, MSN, MSF, MST
 210       FORMAT ( 'OTIDE_UPGRID: NLON/MLAT= ', I5, 1X, I5, ' LLON/LLAT= ', I5, 1X, I5 / &
     &              'OTIDE_UPGRID: MSN= ', I4, ' MSF= ', I4, ' MST= ', I4 )
      END IF
      IWAV = 0
      DO 410 J1=BWAV,EWAV
         IF ( IVRB .GE. 1 ) THEN
              WRITE ( 6, 220 ) J1, NWAV, CHAR(13)
 220          FORMAT ( '  Wave ', I3, ' ( ', I3, ' )  ',A$ )
              CALL FLUSH ( 6 )
         END IF
         IWAV = IWAV + 1
         DO 420 J2=1,NCMP
            HEBIN%VAL  => ARRIN_R4(1:NLON,1:NLAT,J2:J2,J1:J1)
            HEBOUT%VAL => ARROUT_R4(1:LLON,1:LLAT,J2:J2,IWAV:IWAV)
!
            IF ( IVRB .GE. 7 ) THEN
                 CALL PLOT_GRID_R4 ( 1, 7, 0, 1, NLON, NLAT, HEBIN%VAL, &
     &               'Original data ', 'm', 1.0, -1.0, '/tmp/boo', IUER )
            END IF
            CALL ERR_PASS ( IUER, IER )
            CALL MALO_UPGRID ( 1, HEBIN, HEB_LS, HEB_MASK, HEBOUT, MSN, MSF, MST, &
     &                         -30.0, 30.0, IER )
            IF ( IVRB .GE. 7 ) THEN
                 CALL PLOT_GRID_R4 ( 1, 7, 0, 1, LLON, LLAT, HEBOUT%VAL, &
     &               'Upgrided data ', 'm', -0.001, 0.001, '/tmp/boo', IUER )
            END IF
            IF ( IER .NE. 0 ) THEN
                 WRITE ( 6, * ) ' J2= ', J2, ' J1= ', J1
                 CALL ERR_LOG ( IUER, 6861, 'OTIDE_UPGRID', 'Error in an '// &
     &               'attempt to run MALO_UPGRID' )
                 RETURN 
            END IF
            DO 430 J3=1,LLAT
               DO 440 J4=1,LLON
                  IF ( HEB_LS%VAL(J4,J3,1,1) > 1.0 - EPS_LS ) THEN
                       HEBOUT%VAL(J4,J3,1,1) = FILL_VALUE
                  END IF
 440           CONTINUE 
 430        CONTINUE 
            IF ( IVRB .GE. 7 ) THEN
                 CALL PLOT_GRID_R4 ( 1, 7, 0, 1, LLON, LLAT, HEBOUT%VAL, &
     &               'Upgrided data ', 'm', -1.0, 1.0, '/tmp/boo', IUER )
            END IF
!
 420     CONTINUE 
 410  CONTINUE 
      HEBIN%VAL  => NULL()
      HEBOUT%VAL => NULL()
      IF ( IVRB .GE. 1 ) THEN
           IF ( BWAV == 1 .AND.  EWAV == NWAV ) THEN
                WRITE ( 6, '(A)' ) 'Finished ocean tide upgridding       '
              ELSE 
                WRITE ( 6, '(A,I2)' ) 'Finished ocean tide upgridding for wave ', BWAV
           END IF 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  OTIDE_UPGRID  !#!#
