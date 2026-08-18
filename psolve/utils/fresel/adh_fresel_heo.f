      SUBROUTINE ADH_FRESEL_HEO ( FRESEL, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ADH_FRESEL_HEO
! *                                                                      *
! *  ### 03-NOV-2003  ADH_FRESEL_HEO v2.0 (c) L. Petrov 23-JUN-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'fresel.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  IVRB, IUER
      REAL*8     PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) !
      CHARACTER  BUF(M_HEO)*80, NAM_ADH*20, STR*80
      REAL*8     PHS_ADH, FRQ_ADH, FRQ_MIN, FRQ_SEP_MIN
      LOGICAL*4  PM_FLAG_ADH, UT1_FLAG_ADH, FL_E3, FL_PM
      INTEGER*4  N_BUF, IB, IE, IC1, IC2, IOS, IS, IND_FRQ, J1, J2, J3, J4, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      FRESEL%N_ADH = 0
      IF ( FRESEL%ADH_FILE(1:4) .EQ. 'NONE' ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Read the ad hoc frequencies file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FRESEL%ADH_FILE, M_HEO, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3371, IUER, 'ADH_FRESEL_HEO', 'Error in '// &
     &         'attempt to read ad hock frequencies from the file: '// &
     &          FRESEL%ADH_FILE )
           RETURN
      END IF
!
      FRQ_MIN = PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
      DO 410 J1=1,N_BUF
         CALL INCH ( J1, STR )
!
         IF ( ILEN(BUF(J1)) .LE.  60  ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '#'  ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '*'  ) GOTO 410
!
         FRESEL%N_ADH = FRESEL%N_ADH + 1
         IF ( FRESEL%N_ADH .GT. M_HEO ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_HEO, STR )
              CALL ERR_LOG ( 3372, IUER, 'ADH_FRESEL_HEO', 'Error in '// &
     &            'parsing the ad hoc frequencies definition file '// &
     &             FRESEL%ADH_FILE(1:I_LEN(FRESEL%ADH_FILE))// &
     &            ' too many frequencies. More than M_HEO: '//STR )
              RETURN
         END IF
!
! ------ Extract the name
!
         CALL CLRCH ( NAM_ADH )
         NAM_ADH = BUF(J1)(2:17)
!
! ------ Extract the frequency
!
         READ ( UNIT=BUF(J1)(21:39), FMT='(F19.12)', IOSTAT=IOS ) FRQ_ADH
         IF ( IOS .NE. 0 ) THEN
              CALL ERR_LOG ( 3373, IUER, 'ADH_FRESEL_HEO', 'Error in '// &
     &            'parsing line '//STR(1:I_LEN(STR))//' of the ad hoc '// &
     &            'frequencies definition file '// &
     &             FRESEL%ADH_FILE(1:I_LEN(FRESEL%ADH_FILE))// &
     &            ' wrong format for the frequency' )
              RETURN
         END IF
!
! ------ Extract the phase
!
         READ ( UNIT=BUF(J1)(42:53), FMT='(F19.12)', IOSTAT=IOS ) PHS_ADH
         IF ( IOS .NE. 0 ) THEN
              write ( 6, * ) ' ios=',ios ! %%%
              CALL ERR_LOG ( 3374, IUER, 'ADH_FRESEL_HEO', 'Error in '// &
     &            'parsing line '//STR(1:I_LEN(STR))//' of the ad hoc '// &
     &            'frequencies definition file '// &
     &             FRESEL%ADH_FILE(1:I_LEN(FRESEL%ADH_FILE))// &
     &            ' wrong format for the phase' )
              RETURN
         END IF
!
! ------ Extract the polar motion flag
!
         READ ( UNIT=BUF(J1)(58:58), FMT='(L1)', IOSTAT=IOS ) PM_FLAG_ADH
         IF ( IOS .NE. 0 ) THEN
              CALL ERR_LOG ( 3375, IUER, 'ADH_FRESEL_HEO', 'Error in '// &
     &            'parsing line '//STR(1:I_LEN(STR))//' of the ad hoc '// &
     &            'frequencies definition file '// &
     &             FRESEL%ADH_FILE(1:I_LEN(FRESEL%ADH_FILE))// &
     &            ' wrong polar motion flag' )
              RETURN
         END IF
!
! ------ Extract the UT1 flag
!
         READ ( UNIT=BUF(J1)(62:62), FMT='(L1)', IOSTAT=IOS ) UT1_FLAG_ADH
         IF ( IOS .NE. 0 ) THEN
              CALL ERR_LOG ( 3376, IUER, 'ADH_FRESEL_HEO', 'Error in '// &
     &            'parsing line '//STR(1:I_LEN(STR))//' of the ad hoc '// &
     &            'frequencies definition file '// &
     &             FRESEL%ADH_FILE(1:I_LEN(FRESEL%ADH_FILE))// &
     &            ' wrong ut1 flag' )
              RETURN
         END IF
!
         IF ( FRESEL%N_FRQ .GT. 0 ) THEN
              FRQ_SEP_MIN = 1.D10
              DO 420 J2=1,FRESEL%N_FRQ
                 IF ( DABS( FRQ_ADH - FRESEL%DAT(J2)%FRQ ) .LT. FRESEL%NF ) THEN
                      IND_FRQ = J2
                      FRQ_SEP_MIN = DABS( FRQ_ADH - FRESEL%DAT(J2)%FRQ )
                 END IF
 420          CONTINUE
              IF ( FRQ_SEP_MIN .LT. FRESEL%NF ) THEN
                   FL_PM = .FALSE.
                   FL_E3 = .FALSE.
                   DO 430 J3=1,FRESEL%N_FRQ
                      IF ( DABS(FRESEL%DAT(J3)%FRQ - &
     &                          FRESEL%DAT(IND_FRQ)%FRQ ) < FRESEL%NF ) THEN
                           IF ( FRESEL%DAT(J3)%USE_UT1 ) THEN
                                FL_E3 = .TRUE.
                           END IF
                           IF ( FRESEL%DAT(J3)%USE_PM ) THEN
                                FL_PM = .TRUE.
                           END IF
                      END IF
 430               CONTINUE 
!
                   IF ( FRESEL%DAT(IND_FRQ)%TYP .NE. IND__PRC ) THEN
                        IF ( FRESEL%PM_EST_ADH   .AND.  PM_FLAG_ADH ) THEN
                             IF ( .NOT. FL_PM ) THEN
                                  FRESEL%DAT(IND_FRQ)%USE_PM  = .TRUE.
                             END IF
                        END IF
                        IF ( FRESEL%UT1_EST_ADH  .AND.  UT1_FLAG_ADH ) THEN
                             IF ( .NOT. FL_E3 ) THEN
                                  FRESEL%DAT(IND_FRQ)%USE_UT1 = .TRUE.
                             END IF
                        END IF
                   END IF
!
! ---------------- We ignore this frequency bean, since it is arelady in use
!
                   GOTO 410
              END IF
         END IF
!
         FRESEL%N_FRQ = FRESEL%N_FRQ + 1
         FRESEL%DAT(FRESEL%N_FRQ)%FRQ = FRQ_ADH 
!
         IF ( FRESEL%PM_EST_ADH   .AND.  PM_FLAG_ADH ) THEN
              FRESEL%DAT(FRESEL%N_FRQ)%USE_PM = .TRUE.
            ELSE
              FRESEL%DAT(FRESEL%N_FRQ)%USE_PM = .FALSE.
         END IF
!
         IF ( FRESEL%UT1_EST_ADH  .AND.  UT1_FLAG_ADH ) THEN
              FRESEL%DAT(FRESEL%N_FRQ)%USE_UT1 = .TRUE.
            ELSE
              FRESEL%DAT(FRESEL%N_FRQ)%USE_UT1 = .FALSE.
         END IF
!
         FRESEL%DAT(FRESEL%N_FRQ)%PHS = PHS_ADH
         FRESEL%DAT(FRESEL%N_FRQ)%NAM = NAM_ADH
         FRESEL%DAT(FRESEL%N_FRQ)%IND = FRESEL%N_ADH
         FRESEL%DAT(FRESEL%N_FRQ)%TYP = IND__ADH
         FRESEL%DAT(FRESEL%N_FRQ)%PMC = 1.0D0
         FRESEL%DAT(FRESEL%N_FRQ)%PMS = 0.0D0
         FRESEL%DAT(FRESEL%N_FRQ)%TID_AMPL = 0.0D0
         FRESEL%DAT(FRESEL%N_FRQ)%L_PWR = 0
 410  CONTINUE
!
      IF ( IVRB .GE. 3   .AND.   FRESEL%N_ADH .GT. 0 ) THEN
           DO 440 J4=1,FRESEL%N_FRQ
              IF ( FRESEL%DAT(J4)%TYP == IND__ADH  ) THEN
                   WRITE (6, 110 ) J4, FRESEL%DAT(J4)%TYP, FRESEL%DAT(J4)%IND, &
     &                                 FRESEL%DAT(J4)%PHS, FRESEL%DAT(J4)%FRQ, &
     &                                 FRESEL%DAT(J4)%NAM
 110               FORMAT ( I4,') ', I2,' Ind=',I4,' PHS=',F12.10, &
     &                            ' FRQ=',1PD19.12,'  ', A )
              END IF
 440       CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ADH_FRESEL_HEO  #!#
