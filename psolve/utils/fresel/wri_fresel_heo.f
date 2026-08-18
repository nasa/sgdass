      SUBROUTINE WRI_FRESEL_HEO ( FRESEL, IVRB, LUN_LOG, LUN_BATCH, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine WRI_FRESEL_HEO 
! *                                                                      *
! *  ### 04-NOV-2003  WRI_FRESEL_HEO v2.2 (c) L. Petrov 2025.12.04  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'fresel.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  IVRB, LUN_LOG, LUN_BATCH, IUER
      REAL*8     ARR1(M_HEO), ARR2(M_HEO), FRQ_MIN, FRQ_LAST
      CHARACTER  STR*32, STR1*32, LAB_PREV*10, LAB_ARR(M_HEO)*10
      LOGICAL*4  LEX
      INTEGER*4  N_BUF, LUN, IND_FRQ, IND_1, IND_2, N_PAR, SIZE_FIELD2, &
     &           SIZE_FRESEL_HEADER, J1, J2, J3, J4, J5, L_LAB, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( FRESEL%N_FRQ .EQ. 0 ) THEN
           CALL ERR_LOG ( 3381, IUER, 'WRI_FRESEL_HEO', 'No frequencies '// &
     &         'were picked up' )
           RETURN 
      END IF
!
      DO 410 J1=1,FRESEL%N_FRQ
         ARR1(J1) = FRESEL%DAT(J1)%FRQ
         ARR2(J1) = J1 + 1.D-7
 410  CONTINUE 
!
      CALL SORT8 ( FRESEL%N_FRQ, ARR1, ARR2 ) 
!
      INQUIRE ( FILE=FRESEL%FILOUT, EXIST=LEX )
      IF ( LEX ) CALL UNLINK ( FRESEL%FILOUT(1:I_LEN(FRESEL%FILOUT))//CHAR(0) )
!      
      FRQ_LAST  = FRQ_LOW
      FRQ_MIN = PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
      N_PAR = 0
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) '------------------------------------'// &
     &                    '------------------------------------'
      END IF
      WRITE ( LUN_LOG, '(A)' ) '------------------------------------'// &
     &                    '------------------------------------'
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FRESEL%FILOUT, 'NEW', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3391, IUER, 'WRI_FRESEL_HEO', 'Error in an '// &
     &         'to attempt open the output file '//FRESEL%FILOUT )
           RETURN 
      END IF
!
! --- Beware of a bug in HP-UX Fortran compiler!!
!
      SIZE_FIELD2 = SIZEOF(FRESEL%FIELD2) 
      SIZE_FRESEL_HEADER = ( LOC(FRESEL%FIELD2) - LOC(FRESEL%FIELD1) ) + &
     &                     SIZE_FIELD2
!
      FRESEL%N_PAR = -1
      CALL ERR_PASS  ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_FRESEL_HEADER, FRESEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3392, IUER, 'WRI_FRESEL_HEO', 'Error in an '// &
     &         'to attempt to write the fresel header in the output file '// &
     &          FRESEL%FILOUT )
           RETURN 
      END IF
!
      LAB_PREV = '????????'
      L_LAB = 0
      DO 420 J2=1,FRESEL%N_FRQ
         IND_FRQ = ARR2(J2)
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, 110 ) J2, FRESEL%DAT(IND_FRQ)%TYP, &
     &                             FRESEL%DAT(IND_FRQ)%IND, &
     &                             FRESEL%DAT(IND_FRQ)%PHS, &
     &                             FRESEL%DAT(IND_FRQ)%FRQ, &
     &                            (FRESEL%DAT(IND_FRQ)%FRQ -FRQ_LAST)/FRESEL%NF, &
     &                             FRESEL%DAT(IND_FRQ)%USE_PM,                 &
     &                             FRESEL%DAT(IND_FRQ)%USE_UT1,                &
     &                             FRESEL%DAT(IND_FRQ)%IND_MAIN
 110          FORMAT ( I4,') ', I2,' Ind=',I5,' PHS=',F12.10, &
     &                            ' FRQ=',1PD19.12,' Dif=',0PF8.2, 1X, L1, L1, &
     &                            1X,I4 )
         END IF
         WRITE ( LUN_LOG, 110 ) J2, FRESEL%DAT(IND_FRQ)%TYP, &
     &                             FRESEL%DAT(IND_FRQ)%IND, &
     &                             FRESEL%DAT(IND_FRQ)%PHS, &
     &                             FRESEL%DAT(IND_FRQ)%FRQ, &
     &                            (FRESEL%DAT(IND_FRQ)%FRQ -FRQ_LAST)/FRESEL%NF, &
     &                             FRESEL%DAT(IND_FRQ)%USE_PM,                 &
     &                             FRESEL%DAT(IND_FRQ)%USE_UT1,                &
     &                             FRESEL%DAT(IND_FRQ)%IND_MAIN
         CALL CLRCH ( STR )
         IF ( FRESEL%DAT(IND_FRQ)%TYP == -5 ) THEN
              STR = 'F F F F  T F F F'
            ELSE IF ( FRESEL%DAT(IND_FRQ)%TYP == -1 .OR. &
     &                FRESEL%DAT(IND_FRQ)%TYP == -2 .OR. & 
     &                FRESEL%DAT(IND_FRQ)%TYP == -3 .OR. & 
     &                FRESEL%DAT(IND_FRQ)%TYP == -4 .OR. &
     &                ( FRESEL%DAT(IND_FRQ)%TYP == -6 .AND. &
     &                  DABS(FRESEL%DAT(IND_FRQ)%FRQ) > OM__EAR/2.5D0 ) .OR. &
     &                ( FRESEL%DAT(IND_FRQ)%TYP == -7 .AND. &
     &                  DABS(FRESEL%DAT(IND_FRQ)%FRQ) > OM__EAR/2.5D0 ) ) THEN
!
              IF ( FRESEL%DAT(IND_FRQ)%USE_PM  .AND. &
     &             FRESEL%DAT(IND_FRQ)%USE_UT1       ) THEN
                   STR = 'T T F F  F F F F'
                 ELSE IF ( .NOT. FRESEL%DAT(IND_FRQ)%USE_PM  .AND. &
     &                           FRESEL%DAT(IND_FRQ)%USE_UT1       ) THEN
                   STR = 'F T F F  F F F F'
                 ELSE IF (       FRESEL%DAT(IND_FRQ)%USE_PM  .AND. &
     &                     .NOT. FRESEL%DAT(IND_FRQ)%USE_UT1       ) THEN
                   STR = 'T F F F  F F F F'
              END IF
            ELSE IF ( FRESEL%DAT(IND_FRQ)%TYP == -6 .AND. &
     &                DABS(FRESEL%DAT(IND_FRQ)%FRQ) < OM__EAR/2.5D0 ) THEN
              STR = 'F T F T  F F F F'
         END IF
!
         IF ( FRESEL%DAT(IND_FRQ)%TYP == -6 .AND.              &
     &        FRESEL%DAT(IND_FRQ)%FRQ > -OM__EAR/2.5D0  .AND.  &
     &        FRESEL%DAT(IND_FRQ)%FRQ < 0.0D0                  ) THEN
!
! ----------- Ignore negative frequencies for zonal tides
!
              CONTINUE 
            ELSE 
              IF ( ILEN(FRESEL%DAT(IND_FRQ)%LAB) == 0 ) THEN
                   WRITE ( UNIT=STR1(1:11), FMT='(1PD11.4)' ) FRESEL%DAT(IND_FRQ)%FRQ
                   CALL CHASHR ( STR1(1:11) )
                   STR1 = STR1(1:9)//STR1(11:11)
                   IF ( FRESEL%DAT(IND_FRQ)%TYP == -1 ) STR1(8:8) = 'P'
                   FRESEL%DAT(IND_FRQ)%LAB = STR1(1:10)
              END IF
!@              IF ( FRESEL%DAT(IND_FRQ)%LAB == LAB_PREV ) THEN
!@                   FRESEL%DAT(IND_FRQ)%LAB(8:8) = 'a' 
!@                   IF ( FRESEL%DAT(IND_FRQ)%LAB == LAB_PREV ) THEN
!@                        FRESEL%DAT(IND_FRQ)%LAB(8:8) =  &
!@     &                    CHAR ( ICHAR(FRESEL%DAT(IND_FRQ)%LAB(8:8))+1 )
!@                   END IF
!@              END IF
              IF ( L_LAB > 1 ) THEN
                   DO 430 J3=1,8
                      DO 440 J4=1,L_LAB
                         IF ( FRESEL%DAT(IND_FRQ)%LAB == LAB_ARR(J4) ) THEN
                              IF ( FRESEL%DAT(IND_FRQ)%LAB(8:8) .LE. 'Z' ) THEN
                                   FRESEL%DAT(IND_FRQ)%LAB(8:8) = 'a' 
                                 ELSE
                                   FRESEL%DAT(IND_FRQ)%LAB(8:8) = CHAR(ICHAR(LAB_ARR(J4)(8:8))+1)
                              END IF
                         END IF
 440                  CONTINUE 
 430               CONTINUE 
              END IF
              WRITE ( LUN_BATCH, 210 ) FRESEL%DAT(IND_FRQ)%LAB, &
     &                                 FRESEL%DAT(IND_FRQ)%PHS, &
     &                                 FRESEL%DAT(IND_FRQ)%FRQ,  &
     &                                 0.0D0, STR(1:16)
 210          FORMAT ( ' W ',A, 1X, F13.10,'D0', 1X, 1PD19.12, 1X, 1PD11.4, &
     &                  2X, A, ' \' )
         END IF
         IF ( J2 .EQ. 1 ) FRQ_LAST = FRESEL%DAT(IND_FRQ)%FRQ 
         FRQ_LAST = FRESEL%DAT(IND_FRQ)%FRQ
!
         IF ( FRESEL%DAT(IND_FRQ)%TYP .EQ. IND__PRC  ) THEN
              N_PAR = N_PAR + 2
         END IF
         IF ( FRESEL%DAT(IND_FRQ)%USE_PM ) THEN
              N_PAR = N_PAR + 2
         END IF
         IF ( FRESEL%DAT(IND_FRQ)%USE_UT1 ) THEN
              N_PAR = N_PAR + 2
         END IF
!
         CALL ERR_PASS     ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZEOF(FRESEL%DAT(IND_FRQ)), &
     &                      FRESEL%DAT(IND_FRQ), IER )
         IF ( IER .NE. 0 ) THEN
              WRITE( 6, * ) ' J2=',J2,' IER=',IER
              CALL ERR_LOG ( 3393, IUER, 'WRI_FRESEL_HEO', 'Error in an '// &
     &            'to attempt to write the fresel record in the output '// &
     &            'file '//FRESEL%FILOUT )
              RETURN 
         END IF
         LAB_PREV = FRESEL%DAT(IND_FRQ)%LAB
         CALL ADD_CLIST ( M_HEO, L_LAB, LAB_ARR, FRESEL%DAT(IND_FRQ)%LAB, IER )
 420  CONTINUE 
!
      WRITE ( LUN_BATCH, '(A)' ) '*'
      DO 450 J5=1,FRESEL%N_FRQ
         IF ( FRESEL%DAT(J5)%TYP == -4  .AND. &
     &        FRESEL%DAT(J5)%IND_MAIN .NE. 0 ) THEN
!
              IND_1 = J5
              IND_2 = FRESEL%DAT(J5)%IND_MAIN 
!
              WRITE ( LUN_BATCH, 220 ) FRESEL%DAT(IND_1)%LAB, &
     &                                 FRESEL%DAT(IND_1)%PMC, &
     &                                 FRESEL%DAT(IND_1)%PMS, &
     &                                 FRESEL%DAT(IND_2)%LAB, &
     &                                 FRESEL%DAT(IND_2)%PMC, &
     &                                 FRESEL%DAT(IND_2)%PMS
 220          FORMAT ( ' C ', 2X, 2( A, 2X, 1PD14.6, 1X, 1PD14.6, 2X), '\' )
         END IF
 450  CONTINUE 
      WRITE ( LUN_BATCH, '(A)' ) '*'
!
      FRESEL%N_PAR = N_PAR
      IER = 0
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) 'Number of frequencies: ',FRESEL%N_FRQ
           WRITE ( 6, * ) 'Number of parameters:  ',N_PAR
           IF ( FRESEL%N_FRQ .GT. 0 ) THEN
                WRITE  ( 6, 120 ) FRESEL%NUT_RMS*1.D12, FRESEL%NUT_MAX*1.D12
 120            FORMAT ( ' Nutation residuals: rms= ', F15.2, &
     &                   ' prad,  max= ', F15.2,' prad' )
           END IF
           IF ( FRESEL%N_TID .GT. 0 ) THEN
                WRITE  ( 6, 130 ) FRESEL%TID_RMS, FRESEL%TID_MAX
 130            FORMAT ( ' Tidal residuals: rms= ', F10.6, &
     &                   ' m^2/s^2,  max= ', F10.6,' m^2/s^2' )
           END IF
      END IF
!
      IF ( IVRB .GT. 0 ) THEN
           WRITE ( 6, * ) 'Number of frequencies: ',FRESEL%N_FRQ
           WRITE ( 6, * ) 'Number of parameters:  ',N_PAR
      END IF
!
      WRITE ( LUN_LOG, * ) 'Number of frequencies: ',FRESEL%N_FRQ
      WRITE ( LUN_LOG, * ) 'Number of parameters:  ',N_PAR
!    
      IF ( FRESEL%N_FRQ .GT. 0 ) THEN
           WRITE  ( LUN_LOG, 120 ) FRESEL%NUT_RMS*1.D12, FRESEL%NUT_MAX*1.D12
      END IF
      IF ( FRESEL%N_TID .GT. 0 ) THEN
           WRITE  ( LUN_LOG, 130 ) FRESEL%TID_RMS, FRESEL%TID_MAX
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRI_FRESEL_HEO  #!#
