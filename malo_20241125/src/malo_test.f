      SUBROUTINE MALO_TEST ( MODE, IPAR, MALO, MALO_AVR, NLON, NLAT, MJD, &
     &                       TAI, DSPL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine MALO_TEST
! *                                                                      *
! *  ### 17-OCT-2012   MALO_TEST   v1.0 (c)  L. Petrov  17-OCT-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INTEGER*4  MODE, IPAR, IUER
      TYPE     ( MALO__TYPE ) :: MALO, MALO_AVR
      INTEGER*4  NLON, NLAT, MJD
      REAL*8     TAI
      REAL*4     DSPL_ARR(NLON,NLAT,3)
      LOGICAL*1  FL_LAT_NP_POLE 
      CHARACTER  STR*32, FILOUT*128, TITLE*80
      INTEGER*4  J1, J2, J3, J4, J5, J6, IDEV, ISCL, IPRC, IND, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( DABS ( DLOG(NLAT-1.D0)/DLOG(2.0D0) - IDINT(DLOG(NLAT-1.D0)/DLOG(2.0D0)) ) < 1.0D-5 ) THEN
           FL_LAT_NP_POLE = .TRUE.
         ELSE 
           FL_LAT_NP_POLE = .FALSE.
      END IF
      FILOUT = '/tmp/foo'
      IF ( MODE == 1 ) THEN
           IDEV = 1
           IF ( IPAR == 1 ) ISCL = 26
           IF ( IPAR == 2 ) ISCL = 28
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_GRID_R4 ( IDEV, 7, ISCL, 1, MALO%NLON, MALO%NLAT, MALO%SPR(1,1,1), &
     &                        'bububu', 'Pa', 1.0, -1.0, FILOUT, IER )
         ELSE IF ( MODE == 2 ) THEN
           DO 410 J1=1,MALO%NTIM
              DO 420 J2=1,MALO%NLAT
                 DO 430 J3=1,MALO%NLON
                    IF ( MALO%SPR(J3,J2,J1) < 40000.0 .OR. MALO%SPR(J3,J2,J1) > 120000.0 ) THEN
                         WRITE ( 6, * ) 'Inds: ', INT2(J3), INT2(J2), INT2(J1), ' SPR: ', MALO%SPR(J3,J2,J1) 
                    END IF
 430             CONTINUE 
 420          CONTINUE 
 410       CONTINUE 
         ELSE IF ( MODE == 3 ) THEN
           DO 440 J4=1,MALO%NTIM
              DO 450 J5=1,MALO%NLAT
                 DO 460 J6=1,MALO%NLON
                    IF ( MALO%SPR(J6,J5,J4) < 40000.0 .OR. MALO%SPR(J6,J5,J4) > 120000.0 ) THEN
                         CONTINUE 
                       ELSE 
                         MALO%SPR(J6,J5,J4) = MALO%SPR(J6,J5,J4) - MALO_AVR%SPR(J6,J5,1) 
                    END IF
 460             CONTINUE 
 450          CONTINUE 
 440       CONTINUE 
!
           IDEV = 1
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_GRID_R4 ( IDEV, 7, 28, 1, MALO%NLON, MALO%NLAT, MALO%SPR(1,1,IPAR), &
     &                        'bebebe', 'Pa', 1.0, -1.0, FILOUT, IER )
         ELSE IF ( MODE == 5 ) THEN
           IDEV = 1
           STR = MJDSEC_TO_DATE ( MJD, TAI, IUER )
           IF ( IPAR == 1 ) THEN
                TITLE = 'Up displacement (mm) on '//STR(1:16)
                FILOUT = '/tmp/dspl_up_'//STR(1:13)//'_'//STR(15:16)
                ISCL = 30
                IPRC = 1
                IND  = 1
             ELSE IF ( IPAR == 2 ) THEN
                TITLE = 'East displacement (mm) on '//STR(1:16)
                FILOUT = '/tmp/dspl_east_'//STR(1:13)//'_'//STR(15:16)
                ISCL = 31
                IPRC = 1
                IND  = 2
             ELSE IF ( IPAR == 3 ) THEN
                TITLE = 'North displacement (mm) on '//STR(1:16)
                FILOUT = '/tmp/dspl_north_'//STR(1:13)//'_'//STR(15:16)
                ISCL = 31
                IPRC = 1
                IND  = 3
             ELSE IF ( IPAR == 11 ) THEN
                TITLE = 'Up displacement (mm) on '//STR(1:16)
                FILOUT = '/tmp/dspl_up_'//STR(1:13)//'_'//STR(15:16)
                ISCL = 30
                IPRC = 2
                IND  = 1
             ELSE IF ( IPAR == 12 ) THEN
                TITLE = 'East displacement (mm) on '//STR(1:16)
                FILOUT = '/tmp/dspl_east_'//STR(1:13)//'_'//STR(15:16)
                ISCL = 31
                IPRC = 2
                IND  = 2
             ELSE IF ( IPAR == 13 ) THEN
                TITLE = 'North displacement (mm) on '//STR(1:16)
                FILOUT = '/tmp/dspl_north_'//STR(1:13)//'_'//STR(15:16)
                ISCL = 31
                IPRC = 2
                IND  = 3
           END IF
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_GRID_R4 ( IDEV, 7, ISCL, IPRC, NLON, NLAT, &
     &                         DSPL_ARR(1,1,IND), TITLE, 'mm', 1.0, -1.0, FILOUT, IER )
         ELSE IF ( MODE == 6 .OR. MODE == 7 .OR. MODE == 11 ) THEN
           IF ( MODE == 11 ) THEN
                STR = MJDSEC_TO_DATE ( MJD, TAI, IUER )
                MALO%SPR_LONG_NAME = 'Surface pressure on '//STR(1:16)
                MALO%SPR_UNITS = 'Pa'
                FILOUT = '/tmp/spr_'//STR(1:13)//'_'//STR(15:16)
           END IF 
           IDEV = 1
           ISCL = 0
           IPRC = 1
           CALL ERR_PASS ( IUER, IER )
           IF ( FL_LAT_NP_POLE ) THEN
                CALL PLOT_GRID_R4 ( IDEV, 7, ISCL, IPRC, NLON, NLAT-1, &
     &                              DSPL_ARR(1,1,1), MALO%SPR_LONG_NAME, &
     &                              MALO%SPR_UNITS, 1.0, -1.0, FILOUT, IER )
              ELSE 
                CALL PLOT_GRID_R4 ( IDEV, 7, ISCL, IPRC, NLON, NLAT, &
     &                              DSPL_ARR(1,1,1), MALO%SPR_LONG_NAME, &
     &                              MALO%SPR_UNITS, 1.0, -1.0, FILOUT, IER )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_TEST  !#!  
