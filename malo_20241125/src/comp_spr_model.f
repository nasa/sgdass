      SUBROUTINE COMP_SPR_MODEL ( MODE, L_FIL, FIL_NC, MALO, MALO_AVR, &
     &                            N_EPC, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_SPR_MODEL
! *                                                                      *
! *  ### 19-OCT-2012  COMP_SPR_MODEL v1.0 (c) L. Petrov 19-OCT-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO, MALO_AVR
      INTEGER*4  MODE, L_FIL, N_EPC, IVRB, IUER
      CHARACTER  FIL_NC(L_FIL)*(*)
      REAL*8     TIM_BEG, TIM_END
      INTEGER*4, ALLOCATABLE :: NUM_POI(:,:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
!      WRITE ( 6, * ) ' fil_ch(1)= ', fil_nc(1)           ! %%%%%%%
!      WRITE ( 6, * ) ' fil_ch(l_fil)= ', fil_nc(l_fil)   ! %%%%%%%
!      write ( 6, * ) ' mode= ', mode, ' l_fil = ', l_fil ! %%%%%%%
!
      N_EPC = 0
      TIM_BEG =  1.D10
      TIM_END = -1.D10
      IF ( MODE == 1 ) THEN
           DO 410 J1=1,L_FIL
              IF ( IVRB .GE. 1 ) THEN
                   WRITE ( 6, 110 ) J1, L_FIL, CHAR(13)
 110               FORMAT ( '  Processing file ', I7, ' ( ', I7, ' ) ',A$ )
              END IF 
              CALL ERR_PASS ( IUER, IER ) 
              CALL SPR_READ_NC ( FIL_NC(J1), MALO, IVRB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5231, IUER, 'COMP_SPR_MODEL', 'Error in '// &
     &                 'an attempt to read nc-file '//FIL_NC(J1) )
                   RETURN 
              END IF   
!
              IF ( MALO%MJD_BEG*86400.0D0 + MALO%UTC_BEG < TIM_BEG ) THEN
                   MALO_AVR%MJD_BEG = MALO%MJD_BEG 
                   MALO_AVR%UTC_BEG = MALO%UTC_BEG 
                   TIM_BEG = MALO%MJD_BEG*86400.0D0 + MALO%UTC_BEG
              END IF
              IF ( MALO%MJD_END*86400.0D0 + MALO%UTC_END > TIM_END ) THEN
                   MALO_AVR%MJD_END = MALO%MJD_END 
                   MALO_AVR%UTC_END = MALO%UTC_END
                   TIM_END = MALO%MJD_END*86400.0D0 + MALO%UTC_END
              END IF
!
              N_EPC = N_EPC + MALO%NTIM
              IF ( J1 == 1 ) THEN
                   MALO_AVR = MALO
                   MALO_AVR%LAT     => NULL()
                   MALO_AVR%LON     => NULL()
                   MALO_AVR%MJD_ARR => NULL()
                   MALO_AVR%TAI_ARR => NULL()
                   MALO_AVR%SPR     => NULL()
!
                   MALO_AVR%NTIM = 1
                   ALLOCATE ( MALO_AVR%LAT(MALO_AVR%NLAT), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5232, IUER, 'COMP_SPR_MODEL', 'Error in '// &
     &                      'an attempt to allocate dynamic memory for array LAT' )
                        RETURN 
                   END IF
!
                   ALLOCATE ( MALO_AVR%LON(MALO_AVR%NLON), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5233, IUER, 'COMP_SPR_MODEL', 'Error in '// &
     &                      'an attempt to allocate dynamic memory for array LAT' )
                        RETURN 
                   END IF
!
                   ALLOCATE ( MALO_AVR%MJD_ARR(MALO_AVR%NTIM), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5234, IUER, 'COMP_SPR_MODEL', 'Error in '// &
     &                      'an attempt to allocate dynamic memory for array MJD_ARR' )
                        RETURN 
                   END IF
!
                   ALLOCATE ( MALO_AVR%TAI_ARR(MALO_AVR%NTIM), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5235, IUER, 'COMP_SPR_MODEL', 'Error in '// &
     &                      'an attempt to allocate dynamic memory for array TAI_ARR' )
                        RETURN 
                   END IF
!
                   ALLOCATE ( MALO_AVR%SPR(MALO_AVR%NLON,MALO_AVR%NLAT,MALO_AVR%NTIM), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5236, IUER, 'COMP_SPR_MODEL', 'Error in '// &
     &                      'an attempt to allocate dynamic memory for array SPR' )
                        RETURN 
                   END IF
!
                   ALLOCATE ( NUM_POI(MALO_AVR%NLON,MALO_AVR%NLAT), STAT=IER ) 
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5237, IUER, 'COMP_SPR_MODEL', 'Error in '// &
     &                      'an attempt to allocate dynamic memory for array NUM_POI' )
                        RETURN 
                   END IF
!
                   MALO_AVR%LON(1:MALO%NLON) = MALO%LON(1:MALO%NLON)
                   MALO_AVR%LAT(1:MALO%NLAT) = MALO%LAT(1:MALO%NLAT)
                   MALO_AVR%MJD_ARR(1) = MALO%MJD_ARR(1)
                   MALO_AVR%TAI_ARR(1) = MALO%TAI_ARR(1)
                   MALO_AVR%SPR = 0.0
                   NUM_POI = 0
              END IF
!
! ----------- Update the average
!
              DO 420 J2=1,MALO%NTIM
                  DO 430 J3=1,MALO%NLAT
                     DO 440 J4=1,MALO%NLON
                        IF ( MALO%SPR(J4,J3,J2) .NE. MALO%SPR_MISSING ) THEN
                             MALO_AVR%SPR(J4,J3,1) = MALO_AVR%SPR(J4,J3,1) + MALO%SPR(J4,J3,J2) 
                             NUM_POI(J4,J3) = NUM_POI(J4,J3) + 1
                        END IF
 440                 CONTINUE 
 430              CONTINUE 
 420          CONTINUE 
              CALL ERR_PASS  ( IUER, IER )
              CALL MALO_FREE ( MALO, IER )
 410       CONTINUE 
!
! -------- Normalize the average
!
           DO 450 J5=1,MALO_AVR%NLAT
              DO 460 J6=1,MALO_AVR%NLON
                 IF ( NUM_POI(J6,J5) > 0 ) THEN
                      MALO_AVR%SPR(J6,J5,1) = MALO_AVR%SPR(J6,J5,1)/NUM_POI(J6,J5)
                 END IF
 460          CONTINUE 
 450      CONTINUE 
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 120 ) L_FIL, N_EPC
 120       FORMAT  ( 'All ',I7, ' files ', I7, ' epochs have been processed' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_SPR_MODEL  !#!#
