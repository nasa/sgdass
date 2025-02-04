      SUBROUTINE MALO_LOAD_EOP ( MALO, EOP, NERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_LOAD_EOP
! *                                                                      *
! *  ### 05-MAR-2016  MALO_LOAD_EOP  v1.5 (c) L. Petrov 21-APR-2023  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'vtd.i'
      TYPE     ( MALO__TYPE     ) :: MALO
      TYPE     ( HEO__STRUC     ) :: HEO(M__HEO)
      TYPE     ( MALO__EOP_TYPE ) :: EOP
      TYPE     ( NERS__TYPE     ) :: NERS
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 128*1024 )
      PARAMETER  ( MIND = 64       )
      CHARACTER  BUF(MBUF)*256, STR*128, STR1*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4  IUER 
      REAL*8       EPS_ANG, EPS_RAT
      PARAMETER  ( EPS_ANG = 3.0D-13 ) 
      PARAMETER  ( EPS_RAT = 1.0D-30 ) 
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, IP, NBUF, LIND, IND(2,MIND), MJD, L_HEO, &
     &           L_HEO_SAVED, IER
      REAL*8     SIG_DEF(3,2), SIG_MIN(3,2), SIG_MAX(3,2)
      DATA       SIG_DEF  /          &
     &                      4.8D-10, &
     &                      4.8D-10, &
     &                      1.2D-10, &
     &                      2.0D-14, &
     &                      2.0D-14, &
     &                      5.0D-15  &
     &                    /
      DATA       SIG_MIN  /          &
     &                      4.8D-11, &
     &                      4.8D-11, &
     &                      1.2D-11, &
     &                      2.0D-15, &
     &                      2.0D-15, &
     &                      5.0D-16  &
     &                    /
      DATA       SIG_MAX  /          &
     &                      4.8D-8,  &
     &                      4.8D-8,  &
     &                      1.2D-8,  &
     &                      2.0D-12, &
     &                      2.0D-12, &
     &                      5.0D-13  &
     &                    /
      REAL*8     TAI, UTC, HEO_EPOCH_SEC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      MALO%LEAPSEC%FINAM_LEAPSEC = EOP%CONF%FIL_LEAPSEC
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_LOAD_LEAPSEC ( MALO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6631, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &         'reading reading and parsing leapsecond file '// &
     &          EOP%CONF%FIL_LEAPSEC )
           RETURN 
      END IF
!
      DO 410 J1=1,M__EOPS
         EOP%EOPS(J1)%NP = 0
!
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( EOP%CONF%FIL_EOP(J1), MBUF, BUF, NBUF, IER )
!!         WRITE ( 6, * ) 'J1= ', J1, ' URL_EOP = ', TRIM(EOP%CONF%FIL_EOP(J1))
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) 'J1= ', J1
              WRITE ( 6, * ) 'URL_EOP = ', TRIM(EOP%CONF%FIL_EOP(J1))
              CALL ERR_LOG ( 6632, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &            'reading EOP file '//EOP%CONF%FIL_EOP(J1) )
              RETURN 
         END IF
         DO 420 J2=1,NBUF
            IF ( BUF(J2)(1:1)  == '#' ) GOTO 420
            IF ( ILEN(BUF(J2)) ==  0  ) GOTO 420
            CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, IER )
            IF ( LIND .NE. 16 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J2, STR )
                 CALL CLRCH ( STR1 )
                 CALL INCH  ( LIND, STR1 )
                 CALL ERR_LOG ( 6633, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &               'parsing EOP file '// &
     &                EOP%CONF%FIL_EOP(J1)(1:I_LEN(EOP%CONF%FIL_EOP(J1)))// &
     &               ' -- there are '//STR1(1:I_LEN(STR1))//' words in line '// &
     &                STR(1:I_LEN(STR))//' while 16 were expected' )
                 RETURN 
            END IF
            IF ( BUF(J2)(IND(1,6):IND(2,6)) == '0' .AND. &
     &           BUF(J2)(IND(1,7):IND(2,7)) == '0' .AND. &
     &           BUF(J2)(IND(1,8):IND(2,8)) == '0' .AND. &
     &           BUF(J2)(IND(1,9):IND(2,9)) == '0'       ) THEN
                 GOTO 420
            END IF
            EOP%EOPS(J1)%NP = EOP%EOPS(J1)%NP + 1
 420     CONTINUE 
!
         IF ( ASSOCIATED ( EOP%EOPS(J1)%SER ) ) THEN
              DEALLOCATE ( EOP%EOPS(J1)%SER )
         END IF
         ALLOCATE ( EOP%EOPS(J1)%SER(EOP%EOPS(J1)%NP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6634, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &            'an attempt to allocate memory for array EOP%EOPS(J1)%SER' )
              RETURN 
         END IF
         IP = 0
         DO 430 J3=1,NBUF
            IF ( BUF(J3)(1:1)  == '#' ) GOTO 430
            IF ( ILEN(BUF(J3)) ==  0  ) GOTO 430
            CALL EXWORD ( BUF(J3), MIND, LIND, IND, REG, IER )
            IF ( LIND .NE. 16 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL CLRCH ( STR1 )
                 CALL INCH  ( LIND, STR1 )
                 CALL ERR_LOG ( 6635, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &              'parsing EOP file '// &
     &              EOP%CONF%FIL_EOP(J1)(1:I_LEN(EOP%CONF%FIL_EOP(J1)))// &
     &              ' -- there are '//STR1(1:I_LEN(STR1))//' words in line '// &
     &              STR(1:I_LEN(STR))//' while 16 were expected' )
                 RETURN 
            END IF
            IF ( BUF(J3)(IND(1,6):IND(2,6)) == '0' .AND. &
     &           BUF(J3)(IND(1,7):IND(2,7)) == '0' .AND. &
     &           BUF(J3)(IND(1,8):IND(2,8)) == '0' .AND. &
     &           BUF(J3)(IND(1,9):IND(2,9)) == '0'       ) THEN
                 GOTO 430
            END IF
            IP = IP + 1
            IF ( IP > EOP%EOPS(J1)%NP ) THEN
                 CALL ERR_LOG ( 6636, IUER, 'MALO_LOAD_EOP', 'Trap of '// &
     &               'internal control in parsing file '// &
     &                EOP%CONF%FIL_EOP(J1)(1:I_LEN(EOP%CONF%FIL_EOP(J1))) )
                 RETURN 
            END IF
!
            READ ( UNIT=BUF(J3)(IND(1,3):IND(2,3)), FMT='(F12.3)', IOSTAT=IER ) &
     &             EOP%EOPS(J1)%SER(IP)%TIM
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL ERR_LOG ( 6637, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &               'parsing the '//STR(1:I_LEN(STR))//'th line of EOP file '// &
     &                EOP%CONF%FIL_EOP(J1)(1:I_LEN(EOP%CONF%FIL_EOP(J1)))// &
     &               ' -- the third word is '//BUF(J3)(IND(1,3):IND(2,3))// &
     &               ' but a float number was expected' )
                 RETURN 
            END IF
!
            DO 440 J4=1,3
               READ ( UNIT=BUF(J3)(IND(1,3+J4):IND(2,3+J4)), FMT='(F15.8)', IOSTAT=IER ) &
     &                EOP%EOPS(J1)%SER(IP)%E(J4)
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J3, STR )
                    CALL CLRCH ( STR1 )
                    CALL INCH  ( 3+J4, STR1 )
                    CALL ERR_LOG ( 6638, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'th line of EOP file '// &
     &                   EOP%CONF%FIL_EOP(J1)(1:I_LEN(EOP%CONF%FIL_EOP(J1)))// &
     &                  ' -- the '//STR1(1:I_LEN(STR1))//'-th word is '// &
     &                  BUF(J3)(IND(1,3+J4):IND(2,3+J4))// &
     &                  ' but a float number was expected' )
                    RETURN 
               END IF
 440        CONTINUE 
!
            DO 450 J5=1,3
               READ ( UNIT=BUF(J3)(IND(1,6+J5):IND(2,6+J5)), FMT='(F15.8)', IOSTAT=IER ) &
     &                EOP%EOPS(J1)%SER(IP)%ER(J5)
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J3, STR )
                    CALL CLRCH ( STR1 )
                    CALL INCH  ( 6+J5, STR1 )
                    CALL ERR_LOG ( 6639, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'th line of EOP file '// &
     &                   EOP%CONF%FIL_EOP(J1)(1:I_LEN(EOP%CONF%FIL_EOP(J1)))// &
     &                  ' -- the '//STR1(1:I_LEN(STR1))//'-th word is '// &
     &                  BUF(J3)(IND(1,6+J5):IND(2,6+J5))// &
     &                  ' but a float number was expected' )
                    RETURN 
               END IF
 450        CONTINUE 
!
            DO 460 J6=1,3
               READ ( UNIT=BUF(J3)(IND(1,10+J6):IND(2,10+J6)), FMT='(F15.8)', IOSTAT=IER ) &
     &                EOP%EOPS(J1)%SER(IP)%DE(J6)
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J3, STR )
                    CALL CLRCH ( STR1 )
                    CALL INCH  ( 10+J6, STR1 )
                    CALL ERR_LOG ( 6640, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'th line of EOP file '// &
     &                   EOP%CONF%FIL_EOP(J1)(1:I_LEN(EOP%CONF%FIL_EOP(J1)))// &
     &                  ' -- the '//STR1(1:I_LEN(STR1))//'-th word is '// &
     &                  BUF(J3)(IND(1,10+J6):IND(2,10+J6))// &
     &                  ' but a float number was expected' )
                    RETURN 
               END IF
               IF ( EOP%EOPS(J1)%SER(IP)%DE(J6) < SIG_MIN(J6,1) .OR. &
     &              EOP%EOPS(J1)%SER(IP)%DE(J6) > SIG_MAX(J6,1)      ) THEN
                    EOP%EOPS(J1)%SER(IP)%DE(J6) = SIG_DEF(J6,1)
               END IF
 460        CONTINUE 
!
            DO 470 J7=1,3
               READ ( UNIT=BUF(J3)(IND(1,13+J7):IND(2,13+J7)), FMT='(F15.8)', IOSTAT=IER ) &
     &                EOP%EOPS(J1)%SER(IP)%DER(J7)
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J3, STR )
                    CALL CLRCH ( STR1 )
                    CALL INCH  ( 13+J7, STR1 )
                    CALL ERR_LOG ( 6641, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'th line of EOP file '// &
     &                   EOP%CONF%FIL_EOP(J1)(1:I_LEN(EOP%CONF%FIL_EOP(J1)))// &
     &                  ' -- the '//STR1(1:I_LEN(STR1))//'-th word is '// &
     &                  BUF(J3)(IND(1,13+J7):IND(2,13+J7))// &
     &                  ' but a float number was expected' )
                    RETURN 
               END IF
               IF ( EOP%EOPS(J1)%SER(IP)%DER(J7) < SIG_MIN(J7,2) .OR. &
     &              EOP%EOPS(J1)%SER(IP)%DER(J7) > SIG_MAX(J7,2)      ) THEN
                    EOP%EOPS(J1)%SER(IP)%DER(J7) = SIG_DEF(J7,2)
               END IF
 470        CONTINUE 
 430     CONTINUE 
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( EOP%CONF%FIL_AAM_SER, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6642, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &         'reading AAM file '//EOP%CONF%FIL_AAM_SER )
           RETURN 
      END IF
!
      EOP%AAM%NP = 0
      DO 480 J8=1,NBUF
         IF ( BUF(J8)(1:24) == '# Assimilation end date:' ) THEN
              CALL DATE_TO_TIME ( BUF(J8)(28:50), MJD, TAI, IER )
              NERS%FCS%TAI_LAST_EOPS_A_ASS =  (MJD - J2000__MJD)*86400.0D0 + TAI
         END IF
         IF ( BUF(J8)(1:1)  == '#' ) GOTO 480
         IF ( ILEN(BUF(J8)) ==  0  ) GOTO 480
         EOP%AAM%NP = EOP%AAM%NP + 1
 480  CONTINUE 
      IF ( ASSOCIATED ( EOP%AAM%TIM ) ) THEN
           DEALLOCATE ( EOP%AAM%TIM )
      END IF
      ALLOCATE ( EOP%AAM%TIM(EOP%AAM%NP) )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6643, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &         'an attempt to allocate memory for array EOP%AAM%TIM' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( EOP%AAM%VAL ) ) THEN
           DEALLOCATE ( EOP%AAM%VAL )
      END IF
      ALLOCATE ( EOP%AAM%VAL(EOP%AAM%NP,M__AAM) )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6644, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &         'an attempt to allocate memory for array EOP%AAM%VAL' )
           RETURN 
      END IF
!
      IP = 0
      DO 490 J9=1,NBUF
         IF ( BUF(J9)(1:1)  == '#' ) GOTO 490
         IF ( ILEN(BUF(J9)) ==  0  ) GOTO 490
!
         IP = IP + 1
         CALL EXWORD ( BUF(J9), MIND, LIND, IND, REG, IER )
         IF ( LIND .NE. 18 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J9, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( LIND, STR1 )
              CALL ERR_LOG ( 6645, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &            'parsing AAM file '// &
     &             EOP%CONF%FIL_AAM_SER(1:I_LEN(EOP%CONF%FIL_AAM_SER))// &
     &            ' -- there are '//STR1(1:I_LEN(STR1))//' words in line '// &
     &            STR(1:I_LEN(STR))//' while 18 were expected' )
              RETURN 
         END IF
         READ ( UNIT=BUF(J9)(IND(1,2):IND(2,2)), FMT='(I5)', IOSTAT=IER ) MJD
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL ERR_LOG ( 6646, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'th line of AAN file '// &
     &             EOP%CONF%FIL_AAM_SER(1:I_LEN(EOP%CONF%FIL_AAM_SER))// &
     &            ' -- the 2nd word is '//BUF(J9)(IND(1,2):IND(2,2))// &
     &            ' but an integer number was expected' )
              RETURN 
         END IF
         READ ( UNIT=BUF(J9)(IND(1,3):IND(2,3)), FMT='(F7.1)', IOSTAT=IER ) UTC
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL ERR_LOG ( 6647, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'th line of AAN file '// &
     &             EOP%CONF%FIL_AAM_SER(1:I_LEN(EOP%CONF%FIL_AAM_SER))// &
     &            ' -- the 3rd word is '//BUF(J9)(IND(1,2):IND(2,2))// &
     &            ' but an integer number was expected' )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL MALO_UTC_TO_TAI ( MALO, MJD, UTC, TAI, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL ERR_LOG ( 6648, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &            'an attempt to compute time on a given MJD and TAI' )
              RETURN 
         END IF
         EOP%AAM%TIM(IP) = (MJD - J2000__MJD)*86400.0D0 + TAI
!
         DO 4100 J10=1,M__AAM
            READ ( UNIT=BUF(J9)(IND(1,3+J10):IND(2,3+J10)), FMT='(F13.6)', IOSTAT=IER ) &
     &             EOP%AAM%VAL(IP,J10)
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL CLRCH ( STR1 )
                 CALL INCH  ( 3+J10, STR1 )
                 CALL ERR_LOG ( 6649, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &               'parsing the '//STR(1:I_LEN(STR))//'th line of AAM file '// &
     &                EOP%CONF%FIL_AAM_SER(1:I_LEN(EOP%CONF%FIL_AAM_SER))// &
     &               ' -- the '//STR1(1:I_LEN(STR1))//'-th word is '// &
     &                BUF(J9)(IND(1,3+J10):IND(2,3+J10))// &
     &               ' but a float number was expected' )
                 RETURN 
            END IF
 4100    CONTINUE 
 490  CONTINUE 
!
      DO 4110 J11=1,M__EOPS
         IF ( .NOT. ASSOCIATED ( EOP%EOPS(J11)%SER ) ) THEN
              CALL ERR_LOG ( 6650, IUER, 'MALO_LOAD_EOP', 'Series '// &
     &             EOPS__NAME(J11)//' was not loaded. Please check, '// &
     &            'weather the corresponding EOP file '// &
     &             EOP%CONF%FIL_EOP(J11)(1:I_LEN(EOP%CONF%FIL_EOP(J11)))// &
     &            ' exists' )
              RETURN 
         END IF
 4110 CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEO ( EOP%CONF%FIL_HEO, M__HEO, L_HEO, HEO, &
     &                NERS%FCS%HEO_MOD, HEO_EPOCH_SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6651, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &         'an attempt to read harmonic Earth orientaiton '// &
     &         'parameters file' )
           RETURN
      END IF
      NERS%FCS%TAI_HEO_EPOCH = HEO_EPOCH_SEC + 43200.0D0
      NERS%FCS%L_HEO  = 0
      NERS%FCS%L_HEOR = 0
      DO 4120 J12=1,L_HEO
         IF ( DABS(HEO(J12)%FREQ) < OM__EAR/5.0 ) GOTO 4120
         IF ( DABS(HEO(J12)%ROTANG(1,1)) > EPS_ANG .OR. &
     &        DABS(HEO(J12)%ROTANG(2,1)) > EPS_ANG .OR. &
     &        DABS(HEO(J12)%ROTANG(3,1)) > EPS_ANG .OR. &
     &        DABS(HEO(J12)%ROTANG(4,1)) > EPS_ANG      ) THEN
              NERS%FCS%L_HEO = NERS%FCS%L_HEO + 1
         END IF
         IF ( DABS(HEO(J12)%ROTANG(1,2)) > EPS_RAT .OR. &
     &        DABS(HEO(J12)%ROTANG(2,2)) > EPS_RAT .OR. &
     &        DABS(HEO(J12)%ROTANG(3,2)) > EPS_RAT .OR. &
     &        DABS(HEO(J12)%ROTANG(4,2)) > EPS_RAT      ) THEN
              NERS%FCS%L_HEOR = NERS%FCS%L_HEOR + 1
         END IF
 4120 CONTINUE 
!
      IF ( NERS%FCS%L_HEO > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEO_ARG(NERS%FCS%L_HEO,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6652, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &              'an attempt to allocate memory for array HEO_ARG' ) 
                RETURN
           END IF
           ALLOCATE ( NERS%FCS%HEO_AMP(NERS%FCS%L_HEO,2,2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6653, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &              'an attempt to allocate memory for array HEO_AMP' ) 
                RETURN
           END IF
!
           L_HEO_SAVED    = NERS%FCS%L_HEO 
           NERS%FCS%L_HEO = 0
           DO 4130 J13=1,L_HEO
              IF ( DABS(HEO(J13)%FREQ) < OM__EAR/5.0 ) GOTO 4130
              IF ( DABS(HEO(J13)%ROTANG(1,1)) > EPS_ANG .OR. &
     &             DABS(HEO(J13)%ROTANG(2,1)) > EPS_ANG .OR. &
     &             DABS(HEO(J13)%ROTANG(3,1)) > EPS_ANG .OR. &
     &             DABS(HEO(J13)%ROTANG(4,1)) > EPS_ANG      ) THEN
!
                   NERS%FCS%L_HEO= NERS%FCS%L_HEO + 1
                   IF ( NERS%FCS%L_HEO > L_HEO_SAVED ) THEN
                        CALL ERR_LOG ( 6654, IUER, 'MALO_LOAD_EOP', 'Trap of '// &
     &                      'internal control: NERS%FCS%L_HEO is in overflow' )
                        WRITE ( 6, * ) 'L_HEO_SAVED = ', L_HEO_SAVED, 'NERS%FCS%L_HEO = ', NERS%FCS%L_HEO 
                        RETURN
                   END IF
                   NERS%FCS%HEO_ARG(NERS%FCS%L_HEO,1)   = HEO(J13)%PHAS
                   NERS%FCS%HEO_ARG(NERS%FCS%L_HEO,2)   = HEO(J13)%FREQ
                   NERS%FCS%HEO_ARG(NERS%FCS%L_HEO,3)   = HEO(J13)%ACCL
                   NERS%FCS%HEO_AMP(NERS%FCS%L_HEO,1,1) = HEO(J13)%ROTANG(1,1)
                   NERS%FCS%HEO_AMP(NERS%FCS%L_HEO,2,1) = HEO(J13)%ROTANG(2,1)
                   NERS%FCS%HEO_AMP(NERS%FCS%L_HEO,1,2) = HEO(J13)%ROTANG(3,1)
                   NERS%FCS%HEO_AMP(NERS%FCS%L_HEO,2,2) = HEO(J13)%ROTANG(4,1)
              END IF
 4130     CONTINUE 
      END IF
!
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEOR_ARG(NERS%FCS%L_HEOR,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6655, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &              'an attempt to allocate memory for array HEO_ARG' ) 
                RETURN
           END IF
           ALLOCATE ( NERS%FCS%HEOR_AMP(NERS%FCS%L_HEOR,2,2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6656, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &              'an attempt to allocate memory for array HEO_AMP' ) 
                RETURN
           END IF
!
           NERS%FCS%L_HEOR = 0
           DO 4140 J14=1,L_HEO
              IF ( DABS(HEO(J14)%FREQ) < OM__EAR/5.0 ) GOTO 4140
              IF ( DABS(HEO(J14)%ROTANG(1,2)) > EPS_RAT .OR. &
     &             DABS(HEO(J14)%ROTANG(2,2)) > EPS_RAT .OR. &
     &             DABS(HEO(J14)%ROTANG(3,2)) > EPS_RAT .OR. &
     &             DABS(HEO(J14)%ROTANG(4,2)) > EPS_RAT      ) THEN
!
                   NERS%FCS%L_HEOR = NERS%FCS%L_HEOR + 1
                   NERS%FCS%HEOR_ARG(NERS%FCS%L_HEOR,1)   = HEO(J14)%PHAS
                   NERS%FCS%HEOR_ARG(NERS%FCS%L_HEOR,2)   = HEO(J14)%FREQ
                   NERS%FCS%HEOR_ARG(NERS%FCS%L_HEOR,3)   = HEO(J14)%ACCL
                   NERS%FCS%HEOR_AMP(NERS%FCS%L_HEOR,1,1) = HEO(J14)%ROTANG(1,2)
                   NERS%FCS%HEOR_AMP(NERS%FCS%L_HEOR,2,1) = HEO(J14)%ROTANG(2,2)
                   NERS%FCS%HEOR_AMP(NERS%FCS%L_HEOR,1,2) = HEO(J14)%ROTANG(3,2)
                   NERS%FCS%HEOR_AMP(NERS%FCS%L_HEOR,2,2) = HEO(J14)%ROTANG(4,2)
              END IF
 4140     CONTINUE 
      END IF
!
      NERS%FCS%NJ = MALO%LEAPSEC%L_LPS
      ALLOCATE ( NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6657, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_UTC_M_TAI' )
           RETURN 
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_UTC_M_TAI(NERS%FCS%NJ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6658, IUER, 'MALO_LOAD_EOP', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_UTC_M_TAI' )
           RETURN 
      END IF
!
      DO 4150 J15=1,NERS%FCS%NJ
         NERS%FCS%ARG_UTC_M_TAI(J15)  = (MALO%LEAPSEC%MJD_LPS(J15) - J2000__MJD)*86400.0D0 + &
     &                                   MALO%LEAPSEC%TAI_LPS(J15)
         NERS%FCS%BSPL_UTC_M_TAI(J15) =  MALO%LEAPSEC%UTC_M_TAI(J15)
 4150 CONTINUE 
!
      EOP%STATUS = MALO__ALLO
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_LOAD_EOP  !#!#
