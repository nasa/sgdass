      SUBROUTINE PIMA_GET_GAIN ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GET_GAIN parses FITS-IDI files and extracts          *
! *   information about antenna gains.                                   *
! *                                                                      *
! * ### 09-JAN-2006   PIMA_GET_GAIN   v3.0 (c) L. Petrov 24-NOV-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, NGAIN, STA_ID, IER, &
     &           KP(PIM__MFIL), NO_POL, NO_TABS, FRG_ID, IND_FRG, &
     &           IND_TAB(PIM__MFIL),     IND_ANT(PIM__MFIL),   &
     &           IND_TYP(PIM__MFIL,2),   IND_NTERM(PIM__MFIL,2), &
     &           IND_XTYP(PIM__MFIL,2),  IND_YTYP(PIM__MFIL,2),  &
     &           IND_XVAL(PIM__MFIL,2),  IND_YVAL(PIM__MFIL,2),  &
     &           IND_GAIN(PIM__MFIL,2),  IND_SENS(PIM__MFIL,2), &
     &           IND_FREQID(PIM__MFIL)
      CHARACTER  STR*128, GAIN_KEY*10
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      NGAIN = 0
!
      DO 410 J1=1,PIM%L_FIL
         CALL ERR_PASS   ( IUER, IER )
         CALL FFITS_OPEN ( PIM%FILE(J1)%NAME, PIM%FILE(J1)%FITS_DESC, &
     &                     'OLD', IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7231, IUER, 'PIMA_GET_GAIN', 'Error in an '// &
     &            'attempt to open FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
!
! ------ Collect indexes of keywords
!
         IND_TAB(J1) = 0
         IND_ANT(J1) = 0
         IND_FREQID(J1) = 0
         DO 420 J2=1,PIM%FILE(J1)%L_HDR
            DO 430 J3=1,PIM%FILE(J1)%L_KWD(J2)
               IF ( PIM%FILE(J1)%KEY(J3,J2)(1:8) == 'EXTNAME' ) THEN
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'GAIN_CURVE'" ) THEN
                         GAIN_KEY = 'GAIN_CURVE'
                         IND_TAB(J1) = J2
                    END IF
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:20) == "'AIPS GC '" ) THEN
                         GAIN_KEY = 'AIPS GC   '
                         IND_TAB(J1) = J2
                    END IF
               END IF
               IF ( IND_TAB(J1) == J2 ) THEN
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'ANTENNA_NO'" ) THEN
                         IND_ANT(J1) = J3
                    END IF
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'FREQID  '  " ) THEN
                         IND_FREQID(J1) = J3
                    END IF
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'FREQ ID '  " ) THEN
                         IND_FREQID(J1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:21) == "'TYPE_1  '" ) THEN
                         IND_TYP(J1,1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:25) == "'NTERM_1 '" ) THEN
                         IND_NTERM(J1,1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:21) == "'X_TYP_1 '" ) THEN
                         IND_XTYP(J1,1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'Y_TYP_1 '" ) THEN
                         IND_YTYP(J1,1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'X_VAL_1 '" ) THEN
                         IND_XVAL(J1,1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'Y_VAL_1 '" ) THEN
                         IND_YVAL(J1,1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'GAIN_1  '" ) THEN
                         IND_GAIN(J1,1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'SENS_1  '" ) THEN
                         IND_SENS(J1,1) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:21) == "'TYPE_2  '" ) THEN
                         IND_TYP(J1,2) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:25) == "'NTERM_2 '" ) THEN
                         IND_NTERM(J1,2) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:21) == "'X_TYP_2 '" ) THEN
                         IND_XTYP(J1,2) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'Y_TYP_2 '" ) THEN
                         IND_YTYP(J1,2) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'X_VAL_2 '" ) THEN
                         IND_XVAL(J1,2) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'Y_VAL_2 '" ) THEN
                         IND_YVAL(J1,2) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'GAIN_2  '" ) THEN
                         IND_GAIN(J1,2) = J3
                    END IF
!
                    IF ( PIM%FILE(J1)%KEY(J3,J2)(11:22) == "'SENS_2  '" ) THEN
                         IND_SENS(J1,2) = J3
                    END IF
               END IF
 430        CONTINUE
 420     CONTINUE
!
! ------ Checke whether all indexes have been collected
!
         IF ( IND_TAB(J1) == 0 ) THEN
              IF ( PIM%CONF%WARNING ) THEN
                   WRITE ( 6, '(A)' ) 'PIMA_GET_GAIN: no GAIN table was '// &
     &                                'found in uv-file '// &
     &                     PIM%FILE(J1)%NAME(1:I_LEN(PIM%FILE(J1)%NAME))
              END IF
           ELSE
              IF ( IND_ANT(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7232, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'ANTENNA_NO was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_FREQID(J1) == 0 ) THEN
                   CALL ERR_LOG ( 7233, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'FREQID was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_TYP(J1,1) == 0 ) THEN
                   CALL ERR_LOG ( 7234, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'TYPE_1 was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_NTERM(J1,1) == 0 ) THEN
                   CALL ERR_LOG ( 7235, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'PC_NTERM_1 was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_XTYP(J1,1) == 0 ) THEN
                   CALL ERR_LOG ( 7236, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'X_TYP_1 was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_YTYP(J1,1) == 0 ) THEN
                   CALL ERR_LOG ( 7237, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'Y_TYP_1 was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_XVAL(J1,1) == 0 ) THEN
                   CALL ERR_LOG ( 7238, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'X_VAL_1 was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_YVAL(J1,1) == 0 ) THEN
                   CALL ERR_LOG ( 7239, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'Y_VAL_1 was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_GAIN(J1,1) == 0 ) THEN
                   CALL ERR_LOG ( 7240, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'GAIN_1 was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
              IF ( IND_SENS(J1,1) == 0 ) THEN
                   CALL ERR_LOG ( 7241, IUER, 'PIMA_GET_GAIN', 'Keyword '// &
     &                 'SENS_1 was not found in the GAIN table '// &
     &                 'in reading file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
!
! ----------- Get KP(J1) -- the number of entries in the GAIN_CURVE table for
! ----------- the J1 -th file
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J1, GAIN_KEY, 'NAXIS2', &
     &                               KP(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7242, IUER, 'PIMA_GET_GAIN', &
     &                 'Failure to get the number of points of '// &
     &                 'gain data in FITS-IDI file '//PIM%FILE(J1)%NAME )
                   RETURN
              END IF
!
! ----------- Get the number of polarizations
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J1, GAIN_KEY, 'NO_POL', &
     &                               NO_POL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7243, IUER, 'PIMA_GET_GAIN', &
     &                 'Failure to get the number of polarications '// &
     &                 'for gain curve data in FITS-IDI file '// &
     &                  PIM%FILE(J1)%NAME  )
                   RETURN
              END IF
!
! ----------- Get the number of rows in the table for gain cureve coefficients
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J1, GAIN_KEY, 'NO_TABS', &
     &                               NO_TABS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7244, IUER, 'PIMA_GET_GAIN', &
     &                 'Failure to get the number of tables for '// &
     &                 'gain curve data in FITS-IDI file '// &
     &                  PIM%FILE(J1)%NAME  )
                   RETURN
              END IF
!
              NGAIN = NGAIN + KP(J1)
!
! ----------- Check whether statins IDs are valid
!
              DO 440 J4=1,KP(J1)
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J1)%FITS_DESC, IND_TAB(J1), J4,   &
     &                              PIM%FILE(J1)%KEY(IND_ANT(J1),IND_TAB(J1)), &
     &                              1, STA_ID, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 7245, IUER, 'PIMA_GET_GAIN', 'Error in '// &
     &                    'getting station id for the '//STR(1:I_LEN(STR))// &
     &                    '-th GAIN of the FITS-IDI file '// &
     &                     PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
!
                 IF ( STA_ID .LE. 0  .OR.  STA_ID > PIM%NSTA ) THEN
                      write ( 6, * ) ' j1=', j1,' j4=',j4, ' sta_id=',sta_id
                      write ( 6, * ) ' ind_and(j1) = ', ind_ant(j1)
                      CALL ERR_LOG ( 7246, IUER, 'PIMA_GET_GAIN', 'Error in '// &
     &                    'getting station id for the '//STR(1:I_LEN(STR))// &
     &                    '-th GAIN of the FITS-IDI file '// &
     &                     PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
!
                 IF ( PIM%REF_STA(STA_ID,J1) .LE. 0  .OR. &
     &                PIM%REF_STA(STA_ID,J1) > PIM%NSTA   ) THEN
!
                      write ( 6, * ) ' j1=', j1,' j4=',j4, ' sta_id=',sta_id, &
     &                           ' pim%ref_sta(sta_id,j1) = ', pim%ref_sta(sta_id,j1)
                      CALL ERR_LOG ( 7247, IUER, 'PIMA_GET_GAIN', 'Trap of '// &
     &                    'internal control in getting GAIN data from '// &
     &                    'FITS-IDI file '//PIM%FILE(J1)%NAME )
                      RETURN
                 END IF
 440          CONTINUE
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL FFITS_CLOSE ( PIM%FILE(J1)%FITS_DESC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7248, IUER, 'PIMA_GET_GAIN', 'Error in an '// &
     &            'attempt to close FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
 410  CONTINUE
      IF ( NGAIN == 0 ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                WRITE ( 6, 210 )
 210            FORMAT ( 'PIMA_GET_GAIN: no gain table was found' )
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Now collect gain curve information station by station
!
      DO 450 J5=1,PIM%NSTA
         DO 460 J6=1,PIM%NFRG
!
! --------- Allocate dynamic memory for station gain and initialize it
!
            PIM%STA(J5)%GAIN(J6)%AVAIL = .TRUE.
            PIM%STA(J5)%GAIN(J6)%NFRQ  = PIM%NFRQ
            PIM%STA(J5)%GAIN(J6)%NPOL  = NO_POL
            PIM%STA(J5)%GAIN(J6)%NTAB  = NO_TABS-1
!
            ALLOCATE ( PIM%STA(J5)%GAIN(J6)%TYP  (PIM%NFRQ,NO_POL) )
            ALLOCATE ( PIM%STA(J5)%GAIN(J6)%NTERM(PIM%NFRQ,NO_POL) )
            ALLOCATE ( PIM%STA(J5)%GAIN(J6)%X_TYP(PIM%NFRQ,NO_POL) )
            ALLOCATE ( PIM%STA(J5)%GAIN(J6)%Y_TYP(PIM%NFRQ,NO_POL) )
            ALLOCATE ( PIM%STA(J5)%GAIN(J6)%X_VAL(PIM%NFRQ,NO_POL) )
            ALLOCATE ( PIM%STA(J5)%GAIN(J6)%Y_VAL(0:PIM%STA(J5)%GAIN(J6)%NTAB,PIM%NFRQ,NO_POL) )
            ALLOCATE ( PIM%STA(J5)%GAIN(J6)%GAIN (0:PIM%STA(J5)%GAIN(J6)%NTAB,PIM%NFRQ,NO_POL) )
            ALLOCATE ( PIM%STA(J5)%GAIN(J6)%SENS (PIM%NFRQ,NO_POL) )
!
            PIM%STA(J5)%GAIN(J6)%TYP   = 0
            PIM%STA(J5)%GAIN(J6)%NTERM = 0
            PIM%STA(J5)%GAIN(J6)%X_TYP = 0
            PIM%STA(J5)%GAIN(J6)%Y_TYP = 0
            PIM%STA(J5)%GAIN(J6)%X_VAL = 0.0
            PIM%STA(J5)%GAIN(J6)%Y_VAL = 0.0
            PIM%STA(J5)%GAIN(J6)%GAIN  = 0.0
            PIM%STA(J5)%GAIN(J6)%SENS  = 0.0
!
! ----------- Now run through files
!
            DO 470 J7=1,PIM%L_FIL
               CALL ERR_PASS   ( IUER, IER )
               CALL FFITS_OPEN ( PIM%FILE(J7)%NAME, &
     &                           PIM%FILE(J7)%FITS_DESC, 'OLD', IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7249, IUER, 'PIMA_GET_GAIN', 'Error '// &
     &                  'in an attempt to open FITS UV-file '// &
     &                   PIM%FILE(J7)%NAME )
                    RETURN
               END IF
!
! ------------ Run through all entries of the J7- th file
!
               DO 480 J8=1,KP(J7)
!
! --------------- Get the frequency group id...
!
                  IF ( IND_TAB(J7) == 0  .OR.  IND_FREQID(J7) == 0 ) THEN
                       GOTO 480
                  END IF
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7), J8,   &
     &                               PIM%FILE(J7)%KEY(IND_FREQID(J7),IND_TAB(J7)), &
     &                               1, FRG_ID, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 7250, IUER, 'PIMA_GET_GAIN', &
     &                     'Failure to get the frequency group id for '// &
     &                     'gain curve data in FITS-IDI file '// &
     &                      PIM%FILE(J7)%NAME  )
                       RETURN
                  END IF
!
! --------------- ... and convert it to the index in the global frequency group table
!
                  IND_FRG = PIM%FILE(J7)%REF_FRG(1,FRG_ID)
                  IF ( IND_FRG .NE. J6 ) GOTO 480
!
! --------------- Get the station ID
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7), J8,   &
     &                               PIM%FILE(J7)%KEY(IND_ANT(J7),IND_TAB(J7)), &
     &                               1, STA_ID, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( J8, STR )
                       CALL ERR_LOG ( 7252, IUER, 'PIMA_GET_GAIN', 'Error '// &
     &                     'in getting station id for the '// &
     &                      STR(1:I_LEN(STR))//'-th GAIN of the FITS-IDI '// &
     &                     'file '//PIM%FILE(J7)%NAME )
                      RETURN
                  END IF
!
                  IF ( PIM%REF_STA(STA_ID,J7) .EQ. J5 ) THEN
!
! ------------------ Get the gain type for the first polarization
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),   &
     &                          J8, PIM%FILE(J7)%KEY(IND_TYP(J7,1),IND_TAB(J7)), &
     &                          PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%TYP(1,1), &
     &                          IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J8, STR )
                          CALL ERR_LOG ( 7253, IUER, 'PIMA_GET_GAIN', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of type from the '// &
     &                        'FITS-IDI file '//PIM%FILE(J7)%NAME )
                         RETURN
                     END IF
!
                     IF ( PIM%STA(J5)%GAIN(J6)%NPOL == 2 ) THEN
!
! ----------------------- ... and for the second (LL) polarization if applicable
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),   &
     &                            J8, PIM%FILE(J7)%KEY(IND_TYP(J7,2),IND_TAB(J7)), &
     &                            PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%TYP(1,2), &
     &                            IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J8, STR )
                               CALL ERR_LOG ( 7254, IUER, 'PIMA_GET_GAIN', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of type from the '// &
     &                             'FITS-IDI file '//PIM%FILE(J7)%NAME )
                               RETURN
                          END IF
                     END IF
!
! ------------------ Get the number of terms in the table for the first (RR)
! ------------------ polarization
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                          J8, PIM%FILE(J7)%KEY(IND_NTERM(J7,1),IND_TAB(J7)), &
     &                          PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%NTERM(1,1), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J8, STR )
                          CALL ERR_LOG ( 7255, IUER, 'PIMA_GET_GAIN', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of NTERM_1 from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J7)%NAME )
                         RETURN
                     END IF
!
                     IF ( PIM%STA(J5)%GAIN(J6)%NPOL == 2 ) THEN
!
! ----------------------- ... and for the second (LL) polarization, if applicable
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),  &
     &                            J8, PIM%FILE(J7)%KEY(IND_NTERM(J7,2),IND_TAB(J7)), &
     &                            PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%NTERM(1,2), &
     &                            IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J8, STR )
                               CALL ERR_LOG ( 7256, IUER, 'PIMA_GET_GAIN', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of NTERM_2 from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J7)%NAME )
                               RETURN
                          END IF
                     END IF
!
! ------------------ Get the table of arguments types for first (RR) polarization
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                          J8, PIM%FILE(J7)%KEY(IND_XTYP(J7,1),IND_TAB(J7)), &
     &                          PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%X_TYP(1,1), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J8, STR )
                          CALL ERR_LOG ( 7257, IUER, 'PIMA_GET_GAIN', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of X_TYP_1 from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J7)%NAME )
                         RETURN
                     END IF
                     IF ( PIM%STA(J5)%GAIN(J6)%NPOL == 2 ) THEN
!
! ----------------------- ... and argument types for the second (LL) polarization
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                               J8, PIM%FILE(J7)%KEY(IND_XTYP(J7,2),IND_TAB(J7)), &
     &                               PIM%FILE(J7)%NFRQ, &
     &                               PIM%STA(J5)%GAIN(J6)%X_TYP(1,2), IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J8, STR )
                               CALL ERR_LOG ( 7258, IUER, 'PIMA_GET_GAIN', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of X_TYP_2 from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J7)%NAME )
                              RETURN
                          END IF
                     END IF
!
! ------------------ Get the table of value types  for first (RR) polarization
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                          J8, PIM%FILE(J7)%KEY(IND_YTYP(J7,1),IND_TAB(J7)), &
     &                          PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%Y_TYP(1,1), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J8, STR )
                          CALL ERR_LOG ( 7259, IUER, 'PIMA_GET_GAIN', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of Y_TYP_1 from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J7)%NAME )
                         RETURN
                     END IF
                     IF ( PIM%STA(J5)%GAIN(J6)%NPOL == 2 ) THEN
!
! ----------------------- ... and value types for the second (LL) polarizaitons
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETI4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                               J8, PIM%FILE(J7)%KEY(IND_YTYP(J7,2),IND_TAB(J7)), &
     &                               PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%Y_TYP(1,2), IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J8, STR )
                               CALL ERR_LOG ( 7260, IUER, 'PIMA_GET_GAIN', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of Y_TYP_2 from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J7)%NAME )
                              RETURN
                          END IF
                     END IF
!
! ------------------ Get the table of arguments ofr hte first (RR) polarization
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                          J8, PIM%FILE(J7)%KEY(IND_XVAL(J7,1),IND_TAB(J7)), &
     &                          PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%X_VAL(1,1), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J8, STR )
                          CALL ERR_LOG ( 7261, IUER, 'PIMA_GET_GAIN', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of X_VAL_1 from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J7)%NAME )
                         RETURN
                     END IF
                     IF ( PIM%STA(J5)%GAIN(J6)%NPOL == 2 ) THEN
!
! ----------------------- ...amd the table of arguments for the second (LL) polarization
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETR4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                               J8, PIM%FILE(J7)%KEY(IND_XVAL(J7,2),IND_TAB(J7)), &
     &                               PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%X_VAL(1,2), IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J8, STR )
                               CALL ERR_LOG ( 7262, IUER, 'PIMA_GET_GAIN', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of X_VAL_2 from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J7)%NAME )
                              RETURN
                          END IF
                     END IF
!
! ------------------ Get the table of values for the first (RR) polarization
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                          J8, PIM%FILE(J7)%KEY(IND_YVAL(J7,1),IND_TAB(J7)), &
     &                          PIM%FILE%NFRQ*(PIM%STA(J5)%GAIN(J6)%NTAB+1),    &
     &                          PIM%STA(J5)%GAIN(J6)%Y_VAL(0,1,1), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J8, STR )
                          CALL ERR_LOG ( 7263, IUER, 'PIMA_GET_GAIN', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of Y_VAL_1 from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J7)%NAME )
                         RETURN
                     END IF
!
                     IF ( PIM%STA(J5)%GAIN(J6)%NPOL == 2 ) THEN
!
! ----------------------- ... and for the second (LL) polarization
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETR4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7), &
     &                               J8, PIM%FILE(J7)%KEY(IND_YVAL(J7,2),IND_TAB(J7)), &
     &                               PIM%FILE%NFRQ*(PIM%STA(J5)%GAIN(J6)%NTAB+1),     &
     &                               PIM%STA(J5)%GAIN(J6)%Y_VAL(0,1,2), IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J8, STR )
                               CALL ERR_LOG ( 7264, IUER, 'PIMA_GET_GAIN', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of Y_VAL_2 from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J7)%NAME )
                              RETURN
                          END IF
                     END IF
!
! ------------------ Get the table of gain coefficients for the first (RR) polarization
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                          J8, PIM%FILE(J7)%KEY(IND_GAIN(J7,1),IND_TAB(J7)), &
     &                          PIM%FILE%NFRQ*(PIM%STA(J5)%GAIN(J6)%NTAB+1),  &
     &                          PIM%STA(J5)%GAIN(J6)%GAIN(0,1,1), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J8, STR )
                          CALL ERR_LOG ( 7265, IUER, 'PIMA_GET_GAIN', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of GAIN_1 from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J7)%NAME )
                         RETURN
                     END IF
!
                     IF ( PIM%STA(J5)%GAIN(J6)%NPOL == 2 ) THEN
!
! ----------------------- ... and gain coefficients for the second (LL) polarization
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETR4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                               J8, PIM%FILE(J7)%KEY(IND_GAIN(J7,2),IND_TAB(J7)), &
     &                               PIM%FILE%NFRQ*(PIM%STA(J5)%GAIN(J6)%NTAB+1),  &
     &                               PIM%STA(J5)%GAIN(J6)%GAIN(0,1,2), IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J8, STR )
                               CALL ERR_LOG ( 7266, IUER, 'PIMA_GET_GAIN', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of GAIN_2 from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J7)%NAME )
                              RETURN
                          END IF
                     END IF
!
! ------------------ Get zenith sensitivity (DFPU) for the first (RR) polarization
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL FFITS_GETR4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                          J8, PIM%FILE(J7)%KEY(IND_SENS(J7,1),IND_TAB(J7)), &
     &                          PIM%FILE(J7)%NFRQ, PIM%STA(J5)%GAIN(J6)%SENS(1,1), IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J8, STR )
                          CALL ERR_LOG ( 7267, IUER, 'PIMA_GET_GAIN', &
     &                        'Error in getting '//STR(1:I_LEN(STR))// &
     &                        '-th point of SENS_1 from the FITS-IDI '// &
     &                        'file '//PIM%FILE(J7)%NAME )
                          RETURN
                     END IF
!
                     IF ( PIM%STA(J5)%GAIN(J6)%NPOL == 2 ) THEN
!
! ----------------------- ... and zenith sensitivity (DFPU) for the second (LL) polarization
!
                          CALL ERR_PASS ( IUER, IER )
                          CALL FFITS_GETR4 ( PIM%FILE(J7)%FITS_DESC, IND_TAB(J7),    &
     &                               J8, PIM%FILE(J7)%KEY(IND_SENS(J7,2),IND_TAB(J7)), &
     &                               PIM%FILE(J7)%NFRQ, &
     &                               PIM%STA(J5)%GAIN(J6)%SENS(1,2), IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL CLRCH ( STR )
                               CALL INCH  ( J8, STR )
                               CALL ERR_LOG ( 7268, IUER, 'PIMA_GET_GAIN', &
     &                             'Error in getting '//STR(1:I_LEN(STR))// &
     &                             '-th point of SENS_2 from the FITS-IDI '// &
     &                             'file '//PIM%FILE(J7)%NAME )
                               RETURN
                          END IF
                     END IF
                  END IF
 480           CONTINUE
!
               CALL ERR_PASS    ( IUER, IER )
               CALL FFITS_CLOSE ( PIM%FILE(J7)%FITS_DESC, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7269, IUER, 'PIMA_GET_GAIN', 'Error '// &
     &                  'in an attempt to close FITS UV-file '// &
     &                   PIM%FILE(J7)%NAME )
                    RETURN
               END IF
 470        CONTINUE
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                 WRITE ( 6, 220 ) PIM%STA(J5)%IVS_NAME, J6, &
                                  PIM%STA(J5)%GAIN(J6)%AVAIL
 220             FORMAT ( 'PIMA_GET_GAIN: Station: ', A, &
     &                    ' Frq_Grp: ',I1, ' Gain_avail: ', L1 )
                 CALL FLUSH ( 6 )
                 IF ( PIM%STA(J5)%GAIN(J6)%AVAIL ) THEN
                      WRITE ( 6, * ) 'PIMA_GET_GAIN: TYP:   ', PIM%STA(J5)%GAIN(J6)%TYP(1,1)
                      WRITE ( 6, * ) 'PIMA_GET_GAIN: NTERM: ', PIM%STA(J5)%GAIN(J6)%NTERM(1,1)
                      WRITE ( 6, * ) 'PIMA_GET_GAIN: NTAB:  ', PIM%STA(J5)%GAIN(J6)%NTAB
                      IF ( PIM%STA(J5)%GAIN(J6)%NTERM(1,1) .GE. 1 ) WRITE ( 6, * ) 'PIMA_GET_GAIN: GAIN:  ', PIM%STA(J5)%GAIN(J6)%GAIN(0:PIM%STA(J5)%GAIN(J6)%NTAB,1,1)
                      IF ( PIM%STA(J5)%GAIN(J6)%NTERM(1,1) .GE. 2 ) WRITE ( 6, * ) 'PIMA_GET_GAIN: GAIN:  ', PIM%STA(J5)%GAIN(J6)%GAIN(0:PIM%STA(J5)%GAIN(J6)%NTAB,2,1)
                      IF ( PIM%STA(J5)%GAIN(J6)%NTERM(1,1) .GE. 3 ) WRITE ( 6, * ) 'PIMA_GET_GAIN: GAIN:  ', PIM%STA(J5)%GAIN(J6)%GAIN(0:PIM%STA(J5)%GAIN(J6)%NTAB,3,1)
                      IF ( PIM%STA(J5)%GAIN(J6)%NTERM(1,1) .GE. 4 ) WRITE ( 6, * ) 'PIMA_GET_GAIN: GAIN:  ', PIM%STA(J5)%GAIN(J6)%GAIN(0:PIM%STA(J5)%GAIN(J6)%NTAB,4,1)
                      WRITE ( 6, * ) 'PIMA_GET_GAIN: SENS:  ', PIM%STA(J5)%GAIN(J6)%SENS(1,1)
                      WRITE ( 6, * ) 'PIMA_GET_GAIN: X_VAL: ', PIM%STA(J5)%GAIN(J6)%X_VAL(1,1)
                      WRITE ( 6, * ) 'PIMA_GET_GAIN: Y_VAL: ', PIM%STA(J5)%GAIN(J6)%Y_VAL(0:PIM%STA(J5)%GAIN(J6)%NTAB,1,1)
                END IF
            END IF
 460     CONTINUE
 450  CONTINUE
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           CALL FLUSH ( 6 )
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_GAIN  !#!#
