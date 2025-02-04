      SUBROUTINE GET_ECCDAT ( ECCDAT_FILE, DBNAME, L_STA, C_STA, STA_COO, &
     &           FJD_BEG, FJD_END, NO_HIS, MONU_NAME, MONU_TYPE, &
     &           ECC_ORIG, ECC_XYZ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_ECCDAT  reads eccentricity file, parses it and gets   *
! *   appropriate values of eccentricity vectors for a list of requested *
! *   stations. Eccentricity vector is a vector from the monument        *
! *   reference point to the point of antenna axis intersection.         *
! *                                                                      *
! *   GET_ECCDAT also returns an array of monument names and monument    *
! *   types.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * ECCDAT_FILE ( CHARACTER ) -- Filename of the eccentricity file.      *
! *                              Eccentricity file should be in          *
! *                              ECC-format (POST_OCT99).                *
! *      DBNAME ( CHARACTER ) -- Database name. Used for error messages  *
! *                              only.                                   *
! *       L_STA ( INTEGER*4 ) -- Number of stations whose eccentricity   *
! *                              vectors should be found in the          *
! *                              eccentricity file.                      *
! *       C_STA ( CHARACTER ) -- List of station names whose             *
! *                              eccentricity vectors should be found in *
! *                              the eccentricity file. Dimension: L_STA *
! *     STA_COO ( REAL*8    ) -- Arrays of station coordinates in crust  *
! *                              fixed reference frame.                  *
! *                              Dimension: (3,L_STA)                    *
! *     FJD_BEG ( REAL*8    ) -- Julian date of the first observation in *
! *                              the session.                            *
! *     FJD_END ( REAL*8    ) -- Julian date of the last observation in  *
! *                              the session.                            *
! *      NO_HIS ( LOGICAL*2 ) -- If .TRUE. then no informational         *
! *                              messages will be put in the screen in   *
! *                              curses mode.                            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   MONU_NAME ( CHARACTER ) -- Array of monument names. Monument name  *
! *                              is a 4-digit number of the monument.    *
! *                              Dimension: L_STA.                       *
! *   MONU_TYPE ( CHARACTER ) -- Array of eccentricity types: XY or NE   *
! *                              XY means the eccentricity vector is in  *
! *                                 the same crust-fixed coordinate      *
! *                                 system as station positions.         *
! *                              NE means the eccentricity vector is in  *
! *                                 the local topocentric frame: north,  *
! *                                 east, vertical component.            *
! *                              Dimension: L_STA.                       *
! *    ECC_ORIG ( REAL*8    ) -- Eccentricity vector (in meter) as it    *
! *                              was read from eccentricity file.        *
! *                              Dimension: (3,L_STA).                   *
! *     ECC_XYZ ( REAL*8    ) -- Eccentricity vector (in meter)          *
! *                              transformed to XYZ crust-fixed          *
! *                              reference system: the same as the       *
! *                              system  for station positions.          *
! *                              Dimension: (3,L_STA).                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  14-OCT-99   GET_ECCDAT   v1.1  (c)  L. Petrov 01-OCT-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'precm.i'
      LOGICAL*2  NO_HIS
      INTEGER*4  L_STA, IUER
      CHARACTER  ECCDAT_FILE*(*), DBNAME*(*), C_STA(L_STA)*8, &
     &           MONU_NAME(L_STA)*10, MONU_TYPE(L_STA)*2
      REAL*8     FJD_BEG, FJD_END, STA_COO(3,L_STA), ECC_ORIG(3,L_STA), &
     &           ECC_XYZ(3,L_STA)
!
      INTEGER*4  MBUF, MAX_TRIES, SLEEP_TRY
      PARAMETER  ( MBUF = 4096    ) ! max length of eccentricity file
      PARAMETER  ( MAX_TRIES = 16 ) ! maximal number of tries to read ecc file
      PARAMETER  ( SLEEP_TRY = 2  ) ! sleep interval between attempt to read
      CHARACTER  BUF(MBUF)*90
      CHARACTER  NAME*8, MONUMENT*4, START_DATE*16, STOP_DATE*16, FINAM*128, &
     &           ECC_COO(3)*10, ECC_TYP*3, STR*80, STR1*32, OUT*1024
      CHARACTER  JD_TO_DATE*23
      REAL*8     JD_START, JD_STOP, SEC
      LOGICAL*4  FL_STA_FOUND(MAX_ARC_STA)
      INTEGER*4  NBUF, MJD, IOS, J1, J2, J3, J4, J5, J6, J7, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Determine eccentricity data file (with path)
!
      IF ( ECCDAT_FILE(1:1) .EQ. '/' ) THEN
           FINAM = ECCDAT_FILE
         ELSE
           FINAM = PRE_SAV_DIR(:PRE_SV_LEN)//ECCDAT_FILE
      END IF
!
! --- Initialization
!
      DO 410 J1=1,L_STA
         FL_STA_FOUND(J1) = .FALSE.
         CALL CLRCH   (     MONU_NAME(J1) )
         CALL CLRCH   (     MONU_TYPE(J1) )
         CALL NOUT_R8 ( 3, ECC_ORIG(1,J1) )
 410  CONTINUE
!
! --- Reading eccentricity file into the text buffer BUF.
! --- Trailing line is sought. If the line is not found then new reading attempt
! --- is done. This trick is made in order to prevent error in reading when
! --- somebody writes the file at the same time as SOLVE reads it.
!
      DO 420 J2=1,MAX_TRIES
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7901, IUER, 'GET_ECCDAT', 'Error in reading '// &
     &            'eccentricity file' )
              RETURN
         END IF
         IF ( NBUF .LE. 2 ) THEN
#ifdef GNU
              CALL SLEEP ( SLEEP_TRY )
#else
              CALL FUNC_SLEEP ( SLEEP_TRY )
#endif
              GOTO 420
         END IF
!
! ------ Check the first line: The first line should cntain a header which
! ------ identifies the file
!
         IF ( BUF(1)(1:18) .NE. ECC__LABEL ) THEN
              CALL ERR_LOG ( 7902, IUER, 'GET_ECCDAT', 'File '// &
     &             FINAM(1:I_LEN(FINAM))//' is not an eccentricity file: '// &
     &            'a signature in the first line '//ECC__LABEL//' has '// &
     &            'not been found' )
              RETURN
         END IF
         IF ( BUF(NBUF)(1:18) .EQ. '# ECC-FORMAT V 1.0' ) GOTO 820
#ifdef GNU
         CALL SLEEP ( SLEEP_TRY )
#else
         CALL FUNC_SLEEP ( SLEEP_TRY )
#endif
 420  CONTINUE
 820  CONTINUE
!
! --- Check the last line: The first line should cntain a header which
! --- identifies the file
!
      IF ( BUF(NBUF)(1:18) .NE. '# ECC-FORMAT V 1.0' ) THEN
           CALL ERR_LOG ( 7903, IUER, 'GET_ECCDAT', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' is not an eccentricity file: '// &
     &         'a signature in the last line <# ECC-FORMAT V 1.0> has '// &
     &         'not been found' )
           RETURN
      END IF
!
      IF ( .NOT. NO_HIS ) THEN
           CALL CLRCH ( STR )
           STR = 'SITE     MONUMENT      OFF-1    OFF-2    OFF-3 TYPE    '// &
     &           'DEL-X    DEL-Y    DEL-Z'
           CALL ADDSTR_F ( STR(1:79) )
           CALL NL_MN()
      END IF
!
! --- Parse eccentricity file
!
      DO 430 J3=1,NBUF
         IF ( BUF(J3)(1:1) .EQ. '#' ) GOTO 430 ! comment
         IF ( BUF(J3)(1:1) .EQ. '$' ) GOTO 430 ! comment
!
! ------ Extract different fields
!
         NAME       = BUF(J3)(3:10)
         MONUMENT   = BUF(J3)(12:15)
         START_DATE = BUF(J3)(18:33)
         STOP_DATE  = BUF(J3)(36:51)
         ECC_COO(1) = BUF(J3)(54:63)
         ECC_COO(2) = BUF(J3)(65:74)
         ECC_COO(3) = BUF(J3)(76:85)
         ECC_TYP    = BUF(J3)(88:90)
!
! ------ Try to match the record to one of the stations from the list
!
         DO 440 J4=1,L_STA
            IF ( C_STA(J4) .EQ. NAME ) THEN
!
! -------------- Well. We matched the J4-th station with the J3-th line.
! -------------- Transform start and stop dates of eccentricity vector to
! -------------- Julian dates
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( START_DATE//':00', MJD, SEC, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH ( J3, STR )
                      CALL ERR_LOG ( 7903, IUER, 'GET_ECCDAT', 'Wrong '// &
     &                    'format of staring date at the '//STR(1:I_LEN(STR))// &
     &                    'line of the eccentricity file '//FINAM )
                      RETURN
                 END IF
                 JD_START = 2400000.5 + MJD + SEC/86400.0
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( STOP_DATE//':00', MJD, SEC, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH ( J3, STR )
                      CALL ERR_LOG ( 7903, IUER, 'GET_ECCDAT', 'Wrong '// &
     &                    'format of ending date at the '//STR(1:I_LEN(STR))// &
     &                    'line of the eccentricity file '//FINAM )
                      RETURN
                 END IF
                 JD_STOP = 2400000.5 + MJD + SEC/86400.0
!
! -------------- Check: are the dates valid.
!
                 IF ( JD_STOP .LE. JD_START ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH ( J3, STR )
                      CALL ERR_LOG ( 7904, IUER, 'GET_ECCDAT', 'Wrong '// &
     &                    'dates at the '//STR(1:I_LEN(STR))//'-th line of '// &
     &                    'the ending date preceeds the starting date. '// &
     &                    'Eccentricity file: '//FINAM )
                      RETURN
                 END IF
!
                 IF ( FJD_BEG .GT. JD_START  .AND.  FJD_END .LT. JD_STOP ) THEN
!
! ------------------- Interval of epochs of the session: [FJD_BEG, FJD_END]
! ------------------- lays within the interval of validity of eccentricity
! ------------------- vector: [JD_START, JD_STOP]
!
                      IF ( FL_STA_FOUND(J4) ) THEN
!
! ------------------------ Oh! But we already had a record for thsi interval.
!
                           STR  = JD_TO_DATE ( FJD_BEG, IER )
                           STR1 = JD_TO_DATE ( FJD_END, IER )
                           CALL ERR_LOG ( 7905, IUER, 'GET_ECCDAT', &
     &                         'Error in searching eccentricity for station '// &
     &                          NAME//' -- more than one valid record for '// &
     &                         'the range of experiment date/time: '// &
     &                         '[ '//STR(1:23)//', '//STR1(1:23)//' ]. '// &
     &                         'Eccentricity file: '//FINAM )
                           RETURN
                      END IF
!
! ------------------- Decode eccentricity
!
                      DO 450 J5=1,3
                         READ ( UNIT=ECC_COO(J5)(1:I_LEN(ECC_COO(J5))), FMT='(F10.5)', IOSTAT=IOS) &
     &                          ECC_ORIG(J5,J4)
                         IF ( IOS .NE. 0 ) THEN
                              CALL CLRCH ( STR )
                              CALL INCH  ( J3, STR )
                              CALL ERR_LOG ( 7906, IUER, 'GET_ECCDAT', &
     &                            'Error in decoding eccentricity in '// &
     &                            'processing the '//STR(1:I_LEN(STR))// &
     &                            '-th line of the file '//FINAM )
                              RETURN
                         END IF
 450                  CONTINUE
!
! ------------------- Put final reccentricity vector to ECC_XYZ.
! ------------------- ECC_XYZ means that data have been transormed to XYZ
! ------------------- system
!
                      IF ( ECC_TYP .EQ. 'XYZ' ) THEN
!
! ------------------------ No rotation. Simple copying
!
                           DO 460 J6=1,3
                              ECC_XYZ(J6,J4) = ECC_ORIG(J6,J4)
 460                       CONTINUE
                         ELSE IF ( ECC_TYP .EQ. 'NEU' ) THEN
!
! ------------------------ Rotation of the eccentricity vector from NEU tp XYZ
! ------------------------ system
!
                           CALL KROT ( STA_COO(1,J4),  STA_COO(2,J4), &
     &                                 STA_COO(3,J4),  ECC_ORIG(3,J4), &
     &                                 ECC_ORIG(2,J4), ECC_ORIG(1,J4), &
     &                                 ECC_XYZ(1,J4),  ECC_XYZ(2,J4), &
     &                                 ECC_XYZ(3,J4) )
                         ELSE
                           CALL CLRCH ( STR )
                           CALL INCH  ( J3, STR )
                           CALL ERR_LOG ( 7907, IUER, 'GET_ECCDAT', &
     &                         'Wrong eccentricity type found in the line '// &
     &                          STR(1:I_LEN(STR))//'-th line of the file '// &
     &                          FINAM(1:I_LEN(FINAM))//' -- "'//ECC_TYP//'" '// &
     &                         'NEU or XYZ was expected' )
                           RETURN
                      END IF
!
! ------------------- Copy monumet name and monument type
!
                      MONU_NAME(J4) = MONUMENT
                      MONU_TYPE(J4) = ECC_TYP(1:2)
!
                      IF ( .NOT. NO_HIS ) THEN
!
! ------------------------ Print eccentricity values at screen
!
                           WRITE ( STR, 110 ) NAME, MONU_NAME(J4), &
     &                             ECC_ORIG(1,J4), ECC_ORIG(2,J4), &
     &                             ECC_ORIG(3,J4), ECC_TYP, &
     &                             ECC_XYZ(1,J4), ECC_XYZ(2,J4), ECC_XYZ(3,J4)
 110                       FORMAT ( A, 1X, A, 3F9.4, 2X, A, 3F9.4 )
                           CALL ADDSTR_F ( STR(1:79) )
                           CALL NL_MN()
                      END IF
!
! ------------------- Set flag: eccentricity vector for the J4-th station has
! ------------------- been found
!
                      FL_STA_FOUND(J4) = .TRUE.
                 END IF
            END IF
 440     CONTINUE
 430  CONTINUE
!
! --- Check: whether eccentricity vector for all stations has been found
!
      CALL CLRCH ( OUT )
      DO 470 J7=1,L_STA
         IF ( .NOT. FL_STA_FOUND(J7) ) THEN
              IF ( ILEN(OUT) .EQ. 0 ) THEN
                   OUT(1:8) = C_STA(J7)
                ELSE
                   OUT(I_LEN(OUT)+2:) = C_STA(J7)
              ENDIF
         END IF
 470  CONTINUE
!
      IF ( ILEN(OUT) .GT. 0 ) THEN
           STR  = JD_TO_DATE ( FJD_BEG, IER )
           STR1 = JD_TO_DATE ( FJD_END, IER )
           CALL ERR_LOG ( 7907, IUER, 'GET_ECCDAT_OLD', 'Error in '// &
     &         'processing the database '//DBNAME//' : Eccentricity file '// &
     &          FINAM(1:I_LEN(FINAM))//' doesn''t have records for the '// &
     &         ' stations '//OUT(1:I_LEN(OUT))//' for the time interval '// &
     &         'of this session: ['//STR(1:23)//', '//STR1(1:23)//']. '// &
     &         'You have to update eccentricity file' )
           RETURN
      END IF
!
      IF ( .NOT. NO_HIS ) THEN
           CALL NL_MN()
           CALL ADDSTR_F ( "Completed getting eccentricity data." )
           CALL NL_MN()
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_ECCDAT  #!#
