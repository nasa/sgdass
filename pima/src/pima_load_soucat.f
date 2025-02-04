      SUBROUTINE PIMA_LOAD_SOUCAT ( PIM, L_CSO, SOU_CAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LOAD_SOUCAT
! *                                                                      *
! * ### 07-JAN-2006 PIMA_LOAD_SOUCAT v4.3 (c)  L. Petrov 15-DEC-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      INCLUDE   'pima_getpar.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      TYPE     ( PIM_SOU__TYPE ) :: SOU_CAT(PIM__MCSO)
      TYPE     ( SOURCE_CAT__TYPE ), ALLOCATABLE :: SOUCAT(:)
      INTEGER*4  L_CSO, IUER
      CHARACTER  STR*128, C_SOU(PIM__MCSO)*8, CH*1
      CHARACTER, ALLOCATABLE :: BUF(:)*512
      LOGICAL*4  FL_DOWN, FL_UP
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, IND_SOU, MODE, NB, ILN, &
     &           MM, L_SWAP, IER
      INTEGER*4  I_LEN, ILEN
!
      ALLOCATE ( SOUCAT(PIM__MCSO), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM__MCSO*SIZEOF(SOUCAT(1)), STR )
           CALL ERR_LOG ( 7191, IUER, 'PIMA_LOAD_SOUCAT', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_READ_SOU ( PIM%CONF%SOU_NAMES_FILE, PIM__MCSO, L_CSO, SOUCAT, &
     &                     C_SOU, PIM%CONF%DEBUG_LEVEL, MODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7192, IUER, 'PIMA_LOAD_SOUCAT', 'Error in '// &
     &         'attempt to parse the file with input source catalogue '// &
                PIM%CONF%SOU_NAMES_FILE )
           DEALLOCATE ( SOUCAT )
           RETURN
      END IF
!
      DO 420 J2=1,L_CSO
         SOU_CAT(J2)%IVS_NAME   = SOUCAT(J2)%NAME
         SOU_CAT(J2)%B1950_NAME = SOUCAT(J2)%B1950_NAME
         SOU_CAT(J2)%J2000_NAME = SOUCAT(J2)%ALT_NAME
         SOU_CAT(J2)%DB_NAME    = SOUCAT(J2)%DB_NAME
         SOU_CAT(J2)%ALPHA      = SOUCAT(J2)%ALP
         SOU_CAT(J2)%DELTA      = SOUCAT(J2)%DEL
         SOU_CAT(J2)%S_VEC(1)   = SOUCAT(J2)%S_VEC(1)
         SOU_CAT(J2)%S_VEC(2)   = SOUCAT(J2)%S_VEC(2)
         SOU_CAT(J2)%S_VEC(3)   = SOUCAT(J2)%S_VEC(3)
         SOU_CAT(J2)%NISO       = 0
         SOU_CAT(J2)%ISO_IND    = 0
         SOU_CAT(J2)%IND_LINE   = SOUCAT(J2)%IND_LINE
         IF ( SOUCAT(J2)%CALIB == -1 ) THEN
              SOU_CAT(J2)%IVS_NAME   = 'NOFRINGE'
              SOU_CAT(J2)%B1950_NAME = 'NOFRINGE'
              SOU_CAT(J2)%J2000_NAME = 'NOFRINGE'
         END IF
 420  CONTINUE
      DEALLOCATE ( SOUCAT )
!
      IF ( MODE == 4 .OR. MODE == 6 ) THEN
!
! -------- Search for in-beam pairs marked as "@" in the source name file or
! --------        for synomymos marked as "%" in the source name file
! -------- First allocate memory for the buffer with the file contents
!
           ALLOCATE ( BUF(PIM__MCSO), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( PIM__MCSO*SIZEOF(BUF(1)), STR )
                CALL ERR_LOG ( 7193, IUER, 'PIMA_LOAD_SOUCAT', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'dynamic memory' )
                RETURN
           END IF
!
! -------- Read the file
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT ( PIM%CONF%SOU_NAMES_FILE, PIM__MCSO, BUF, NB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7194, IUER, 'PIMA_LOAD_SOUCAT', 'Error in '// &
     &              'attempt to parse the file wiht input source catalogue '// &
                     PIM%CONF%SOU_NAMES_FILE )
                DEALLOCATE ( BUF )
                RETURN
           END IF
!
! -------- Check each source against the file
!
           L_SWAP = 0
           DO 440 J4=1,L_CSO
              DO 450 J5=1,NB
                 IF ( BUF(J5)(1:1) == '#' ) GOTO 450 ! bypass comments
                 IF ( BUF(J5)(32:32) == '@'  .OR.  BUF(J5)(32:32) == '%' ) THEN
                      IF ( SOU_CAT(J4)%IVS_NAME == BUF(J5)(1:8) ) THEN
!
! ------------------------ Well. Our J4 -th source is marked as having duplicates
!
                           FL_UP = .TRUE. 
                           FL_DOWN = .TRUE.
!
! ------------------------ Look Down and Up around this source. Lines
! ------------------------ with "@" in the 32th column indicate that such 
! ------------------------ lines form a cluster for the current
! ------------------------ source. Stop looking up or down after we found
! ------------------------ a line without "@" in the 32 th column
!
                           IF ( BUF(J5)(32:32) == '@' ) THEN
                                MM = PIM__MISO
                                CH = '@'
                              ELSE 
                                MM = PIM__MSYN
                                CH = '%'
                           END IF
!
                           DO 460 J6=1,MM
!
! --------------------------- ILN is an index that goes up and down wrt J5
!
                              IF ( MOD(J6,2) == 1 ) THEN
                                   IF ( .NOT. FL_DOWN ) GOTO 460
                                   ILN = J5 + (J6+1)/2
                                 ELSE 
                                   IF ( .NOT. FL_UP   ) GOTO 460
                                   ILN = J5 - J6/2
                              END IF
!
                              IF ( ILN < 1  ) GOTO 460
                              IF ( ILN > NB ) GOTO 460
                              IF ( BUF(ILN)(33:42) .NE. BUF(J5)(33:42) ) GOTO 460
                              IF ( BUF(ILN)(1:1) == '#' ) THEN
                                   IF ( MOD(J6,2) == 1 ) THEN
                                        FL_DOWN = .FALSE.
                                      ELSE 
                                        FL_UP = .FALSE.
                                   END IF
                                   GOTO 460
                              END IF
!
                              IF ( BUF(ILN)(32:32) == CH ) THEN
                                   IND_SOU = 0
                                   DO 470 J7=1,L_CSO
                                      IF ( SOU_CAT(J7)%IVS_NAME == BUF(ILN)(1:8)  .AND. &
     &                                     ABS(J5 - SOU_CAT(J7)%IND_LINE) < MM/2        ) THEN
                                           IND_SOU = J7
                                      END IF
 470                               CONTINUE 
                                   IF ( IND_SOU > 0 ) THEN
                                        IF ( BUF(J5)(32:32) == '@' ) THEN
                                             SOU_CAT(J4)%NISO= SOU_CAT(J4)%NISO + 1
                                             SOU_CAT(J4)%ISO_IND(SOU_CAT(J4)%NISO) = IND_SOU
                                             IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                                                  WRITE ( 6, 110 ) J5, SOU_CAT(J4)%IVS_NAME, &
     &                                                   SOU_CAT(J4)%DB_NAME, &
     &                                                   SOU_CAT(J4)%NISO, &
     &                                                   SOU_CAT(IND_SOU)%IVS_NAME, &
     &                                                   SOU_CAT(IND_SOU)%DB_NAME, BUF(ILN)(1:8), IND_SOU, &
     &                                                   SOU_CAT(IND_SOU)%IND_LINE
 110                                              FORMAT ( 'PIMA_LOAD_SOUCAT: ', I5, ' multiple: IVS: ',A, &
     &                                                     ' DB: ', A, ' MP: ', I1, ' -- ', A, 2X, A, 1X, &
     &                                                     ' || ', A, ' IND_SOU: ', I5, ' IND_LINE: ', I5 )
                                             END IF
                                           ELSE IF ( BUF(J5)(32:32) == '%' ) THEN
                                             SOU_CAT(J4)%NSYN = SOU_CAT(J4)%NSYN + 1
                                             SOU_CAT(J4)%SYN_IND(SOU_CAT(J4)%NSYN) = IND_SOU
                                        END IF
                                   END IF
                                 ELSE 
                                   IF ( MOD(J6,2) == 1 ) THEN
                                        FL_DOWN = .FALSE.
                                      ELSE 
                                        FL_UP = .FALSE.
                                   END IF
                              END IF
 460                       CONTINUE 
                      END IF
                   ELSE IF ( BUF(J5)(32:32) == '^' ) THEN
                      IF ( SOU_CAT(J4)%IVS_NAME == BUF(J5)(1:8) ) THEN
                           DO 480 J8=1,L_CSO
                              IF ( SOU_CAT(J8)%IVS_NAME == BUF(J5)(33:40) ) THEN
                                   SOU_CAT(J4)%IND_SWAP = J8
                                   L_SWAP = L_SWAP + 1
                              END IF
 480                       CONTINUE 
                      END IF
                 END IF
 450          CONTINUE 
 440       CONTINUE 
      END IF
      DEALLOCATE ( BUF )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
           DO 490 J9=1,L_CSO
              WRITE ( 6, 120 ) J9, SOU_CAT(J9)%IVS_NAME, SOU_CAT(J9)%B1950_NAME, &
     &                             SOU_CAT(J9)%J2000_NAME, SOU_CAT(J9)%DB_NAME, &
     &                             SOU_CAT(J9)%NISO, SOU_CAT(J9)%ISO_IND(1:4), &
     &                             SOU_CAT(J9)%IND_SWAP
 120          FORMAT ( 'Pima_load_soucat: ', I5, ' IVS: ', A, ' B1950: ', A, &
     &                  ' J2000: ', A, ' DB: ', A, &
     &                  ' Niso: ', I1, ' Iso_ind: ', 4(I5,1X), ' Ind_swap: ', I5 )
 490       CONTINUE 
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_LOAD_SOUCAT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_READ_SOU ( FILIN, MAX_SOU, L_SOU, SOUCAT, C_SOU, &
     &                           DEBUG_LEVEL, MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_READ_SOU  reads the catalogue in either getpar       *
! *   format or in astro_cat format and returns the array of record with *
! *   information foudn in the catalogue.                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FILIN ( CHARACTER ) -- File with the source catalogue.             *
! * MAX_SOU ( INTEGER*4 ) -- Maximal number of sources in the catalogue. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   L_SOU ( INTEGER*4 ) -- The number of sources in the catalogue.     *
! *  SOUCAT ( RECORD    ) -- Array of object which contains information  *
! *                          extractd from parsing the catalogue.        *
! *                          Dimension: MAX_SOU.                         *
! *   C_SOU ( CHARACTER ) -- Array of soure names. Dimension: MAX_SOU.   *
! * DEBUG_LEVEL  MODE ( INTEGER*4 ) -- debug level. 0 -- silent mode     *
! *    MODE ( INTEGER*4 ) -- format of the catalogue:                    *
! *                          1) getpar-sou                               *
! *                          2) astro_cat                                *
! *                          3) getpar-lso                               *
! *                          4) sou-tab file                             *
! *                          5) solve mod-sou                            *
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
! * ### 21-JAN-2004  PIMA_READ_SOU  v4.0 (c)  L. Petrov  18-DEC-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima_getpar.i'
      INTEGER*4  MAX_SOU, L_SOU, DEBUG_LEVEL, MODE, IUER
      CHARACTER  FILIN*(*), C_SOU(MAX_SOU)*(*)
      TYPE     ( SOURCE_CAT__TYPE ) :: SOUCAT(MAX_SOU)
      INTEGER*4    PIMA__MIGN
      PARAMETER  ( PIMA__MIGN = 32 )
      CHARACTER,  ALLOCATABLE   :: BUF(:)*256
      CHARACTER  STR_PIMAVAR_SOU_IGNORE*1024, STR_IGN(PIMA__MIGN)*128, STR*80
      INTEGER*4  J1, J2, J3, IB, IE, IOS, L_IGN, NBUF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, MULTI_INDEX
!
      L_IGN = 0
      CALL GETENVAR ( 'PIMAVAR_SOU_IGNORE', STR_PIMAVAR_SOU_IGNORE )
      IF ( ILEN(STR_PIMAVAR_SOU_IGNORE) > 0 ) THEN
!
! -------- Environment variable PIMAVAR_SOU_IGNORE defines the substrings to ignore.
! -------- This substrings are separated by & character
!
           IF ( STR_PIMAVAR_SOU_IGNORE(1:1) .NE. '&' ) STR_PIMAVAR_SOU_IGNORE = '&'//STR_PIMAVAR_SOU_IGNORE
!
! -------- Get the list of substrings to ingore
!
           DO 410 J1=1,PIMA__MIGN
              IB = MULTI_INDEX ( J1,   STR_PIMAVAR_SOU_IGNORE, '&' )
              IE = MULTI_INDEX ( J1+1, STR_PIMAVAR_SOU_IGNORE, '&' )
              IF ( IE < 1 .AND. ILEN(STR_PIMAVAR_SOU_IGNORE) > IB ) THEN
                   STR_IGN(J1) = STR_PIMAVAR_SOU_IGNORE(IB+1:ILEN(STR_PIMAVAR_SOU_IGNORE) )
                   IF ( DEBUG_LEVEL .GE. 2 ) THEN
                        WRITE ( 6, '(A)' ) 'PIMA_LOAD_SOUCAT pattern to ingore: '//TRIM(STR_IGN(J1))
                   END IF
                   L_IGN = J1
                   GOTO 810
                 ELSE IF ( IE > IB+1 ) THEN
                   STR_IGN(J1) = STR_PIMAVAR_SOU_IGNORE(IB+1:IE-1)
                   IF ( DEBUG_LEVEL .GE. 2 ) THEN
                        WRITE ( 6, '(A)' ) 'PIMA_LOAD_SOUCAT pattern to ingore: '//TRIM(STR_IGN(J1))
                   END IF
              END IF
 410       CONTINUE 
 810       CONTINUE 
      END IF
!
      ALLOCATE ( BUF(MAX_SOU), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( IOS, STR )
           CALL ERR_LOG ( 2831, IUER, 'PIMA_READ_SOU', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FILIN, MAX_SOU, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2832, IUER, 'PIMA_READ_SOU', 'Error in reading '// &
     &         'source catalogie '//FILIN )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( BUF(1)(1:62) == &
     &    '# GETPAR_SOU format version 1.0  of 2001.05.25' ) THEN
           MODE = 1
         ELSE IF ( BUF(1)(1:62) == &
     &    '# VLBI SOURCE POSITION CATALOGUE  Format version of 2004.08.20' ) THEN
           MODE = 2
         ELSE IF ( BUF(1)(1:32) == '# SOURCE-NAMES  v 2.0 2005.09.06' ) THEN
           MODE = 4
         ELSE IF ( BUF(1)(1:32) == '$$  SOU-MODFILE Format pre-2000 ' ) THEN
           MODE = 5
         ELSE IF ( BUF(1)(1:32) == '# SOURCE-NAMES  v 2.1 2022.07.24' ) THEN
           MODE = 6
         ELSE
           CALL ERR_LOG ( 2833, IUER, 'PIMA_READ_SOU', 'Unsupported format of '// &
     &         'the source catalogue '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- the first line is '//BUF(1) )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      L_SOU = 0
      DO 420 J2=1,NBUF ! Cycle over the catalogue file
         IF ( BUF(J2)(1:1)  .EQ. '#' ) GOTO 420
         IF ( BUF(J2)(1:1)  .EQ. '$' ) GOTO 420
         IF ( ILEN(BUF(J2)) .EQ.  0  ) GOTO 420
         IF ( L_IGN > 0 ) THEN
!
! ----------- Check whether we should ignore this line
!
              DO 430 J3=1,L_IGN
                 IF ( INDEX ( BUF(J2), TRIM(STR_IGN(J3)) ) > 0 ) THEN
                      IF ( DEBUG_LEVEL .GE. 2 ) THEN
                           WRITE ( 6, '(A)' ) 'PIMA_LOAD_SOUCAT ignored entry: '//TRIM(BUF(J2))
                      END IF
                      GOTO 420
                 END IF 
 430          CONTINUE 
         END IF
!
! ------ Get the entry from the input catalogue and compute a semi-major
! ------ axis of the error ellipse
!
         L_SOU = L_SOU + 1
         CALL ERR_PASS       ( IUER, IER )
         CALL PIMA_READ_SOU_LINE  ( MODE, BUF(J2), SOUCAT(L_SOU), IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 2834, IUER, 'PIMA_READ_SOU', 'Error in parsing '// &
     &            ' the '//STR(1:I_LEN(STR))//'-th line '// &
     &             BUF(J2)(1:I_LEN(BUF(J2)))//' of the input file '// &
     &             FILIN )
              RETURN
         END IF
         C_SOU(L_SOU) = SOUCAT(L_SOU)%NAME
         SOUCAT(L_SOU)%IND_LINE = J2
 420  CONTINUE
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PIMA_READ_SOU  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_READ_SOU_LINE ( MODE, STR, SOUCAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_READ_SOU_LINE  parses one line of the buffer with source  *
! *   catalogue and puts results in SOURCAT object.                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MODE ( INTEGER*4 ) -- Format of the catalogue:                    *
! *                          1) getpar-sou                               *
! *                          2) astro_cat                                *
! *                          3) getpar-lso                               *
! *     STR ( CHARACTER ) -- Line with contents of the catalogue record. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  SOUCAT ( RECORD    ) -- Array of object which contains information  *
! *                          extractd from parsing the catalogue.        *
! *                                                                      *
! * ### 03-DEC-2001  PIMA_READ_SOU_LINE   v3.1 (c) L. Petrov 28-JUL-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima_getpar.i'
      TYPE ( SOURCE_CAT__TYPE ) :: SOUCAT
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  MODE, IUER
      CHARACTER  STR*(*), SGN_CHR*1, ALP_STR*16, DEC_STR*16, STR_SAVE*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      REAL*8     SEC, ERR2, TETA
      INTEGER*4  IHR, IMN, IDG, SGN, LIND, IND(2,MIND), IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Initialization
!
      SOUCAT%CALIB = 0
      SOUCAT%NSES_TOTAL = 0
      SOUCAT%NSES_USED  = 0
      CALL CLRCH  ( SOUCAT%ALT_NAME )
      CALL CLRCH  ( SOUCAT%DAT_BEG  )
      CALL CLRCH  ( SOUCAT%DAT_END  )
      CALL CLRCH  ( SOUCAT%SESS     )
      SOUCAT%EPOCH_J2000_SEC = 0.0D0
      STR_SAVE = STR
!
      IF ( MODE .EQ. 1 ) THEN
           SOUCAT%NAME = STR(11:18)
           READ ( UNIT=STR(25:41), FMT='(I2,1X,I2,1X,F11.8)' ) IHR, IMN, SEC
           SOUCAT%ALP = ( DBLE(IHR) + DBLE(IMN)/60.D0 + SEC/3600.0D0 )/ &
     &                    12.0D0*PI__NUM
!
           READ ( UNIT=STR(46:55), FMT='(F10.4)' ) SOUCAT%ALP_ERR
           SOUCAT%ALP_ERR = SOUCAT%ALP_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(62:78), FMT='(A1,I2,1X,I2,1X,F10.7)' ) SGN_CHR, &
     &                                   IDG, IMN, SEC
           IF ( SGN_CHR .EQ. '-' ) THEN
                SGN = -1
              ELSE
                SGN =  1
           END IF
           SOUCAT%DEL = SGN*( DABS(DBLE(IDG)) + DBLE(IMN)/60.D0 + &
     &                        SEC/3600.0D0 )/180.0D0*PI__NUM
!
           READ ( UNIT=STR(83:92), FMT='(F10.4)' ) SOUCAT%DEL_ERR
           SOUCAT%DEL_ERR = SOUCAT%DEL_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(98:104),  FMT='(F7.3)' ) SOUCAT%CORR
           READ ( UNIT=STR(116:122), FMT='(I7)' ) SOUCAT%NOBS_USED
           READ ( UNIT=STR(133:139), FMT='(I7)' ) SOUCAT%NOBS_TOTAL
           READ ( UNIT=STR(151:155), FMT='(I5)' ) SOUCAT%NSES_USED
           READ ( UNIT=STR(166:170), FMT='(I4)' ) SOUCAT%NSES_TOTAL
           STR(182:191) = SOUCAT%DAT_BEG
           STR(203:212) = SOUCAT%DAT_END
         ELSE IF ( MODE .EQ. 2 ) THEN
           IF ( STR(1:1) .EQ. 'c'  .OR.  STR(1:1) .EQ. 'C' ) THEN
                SOUCAT%CALIB = 1
           END IF
           IF ( STR(1:1) .EQ. 'n'  .OR.  STR(1:1) .EQ. 'N' ) THEN
                SOUCAT%CALIB = 2
           END IF
           IF ( STR(1:1) .EQ. 'u'  .OR.  STR(1:1) .EQ. 'U' ) THEN
                SOUCAT%CALIB = 3
           END IF
           SOUCAT%NAME = STR(4:11)
           SOUCAT%ALT_NAME = STR(13:22)
           READ ( UNIT=STR(25:39), FMT='(I2,1X,I2,1X,F9.6)' ) IHR, IMN, SEC
           SOUCAT%ALP = ( DBLE(IHR) + DBLE(IMN)/60.D0 + SEC/3600.0D0 )/ &
     &                  12.0D0*PI__NUM
!
           READ ( UNIT=STR(58:63), FMT='(F7.4)' ) SOUCAT%ALP_ERR
           SOUCAT%ALP_ERR = SOUCAT%ALP_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(41:57), FMT='(A1,I2,1X,I2,1X,F8.5)' ) SGN_CHR, IDG, &
     &                                                           IMN, SEC
           IF ( SGN_CHR .EQ. '-' ) THEN
                SGN = -1
              ELSE
                SGN =  1
           END IF
           SOUCAT%DEL = SGN*( DABS(DBLE(IDG)) + DBLE(IMN)/60.D0 + &
     &                        SEC/3600.0D0 )/180.0D0*PI__NUM
!
           READ ( UNIT=STR(65:70), FMT='(F6.2)' ) SOUCAT%DEL_ERR
           SOUCAT%DEL_ERR = SOUCAT%DEL_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(73:78), FMT='(F6.2)' ) SOUCAT%CORR
           READ ( UNIT=STR(80:85), FMT='(I6)' ) SOUCAT%NOBS_USED
           SOUCAT%NSES_USED = 1
         ELSE IF ( MODE .EQ. 4 ) THEN
           IF ( STR(43:43) .EQ. 'c'  .OR.  STR(43:43) .EQ. 'C' ) THEN
                SOUCAT%CALIB = 1
           END IF
           IF ( STR(43:43) .EQ. 'n'  .OR.  STR(43:43) .EQ. 'N' ) THEN
                SOUCAT%CALIB = 2
           END IF
           IF ( STR(43:43) .EQ. 'u'  .OR.  STR(43:43) .EQ. 'U' ) THEN
                SOUCAT%CALIB = 3
           END IF
           IF ( STR(43:43) .EQ. 's'  .OR.  STR(43:43) .EQ. 'S' ) THEN
                SOUCAT%CALIB = -1
           END IF
           SOUCAT%NAME       = STR(1:8)
           SOUCAT%ALT_NAME   = STR(11:20)
           SOUCAT%B1950_NAME = STR(23:30)
!
! -------- Alternative source name may be in some cases as long 
! -------- as 16 characters long, but normally it is shorter.
! -------- Truncate it after the first blank
!
           SOUCAT%DB_NAME    = STR(33:48)
           IP = INDEX ( SOUCAT%DB_NAME, ' ' )
           IF ( IP > 0 ) THEN
                CALL CLRCH ( SOUCAT%DB_NAME(IP:) )
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL HR_TAT ( STR(46:58), SOUCAT%ALP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2841, IUER, 'PIMA_READ_SOU_LINE', 'Error in '// &
     &              'decoding right ascension '//STR(46:58)// &
     &              ' in line '//STR_SAVE )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GR_TAT ( STR(60:72), SOUCAT%DEL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2842, IUER, 'PIMA_READ_SOU_LINE', 'Error in '// &
     &              'decoding declination '//STR(60:72)//' in line '//STR_SAVE )
                RETURN
           END IF
           READ ( UNIT = STR(75:80), FMT='(F6.2)', IOSTAT=IER ) SOUCAT%SOU_ERR
           IF ( IER == 0 ) THEN
                SOUCAT%SOU_ERR = SOUCAT%SOU_ERR*MAS__TO__RAD
              ELSE 
                SOUCAT%SOU_ERR = 999.99*MAS__TO__RAD
           END IF
         ELSE IF ( MODE .EQ. 5 ) THEN
           SOUCAT%CALIB = 1
           IP = INDEX ( STR, '!' ) - 1
           IF ( IP .LE. 0 ) IP =I_LEN(STR)
           CALL EXWORD ( STR, MIND, LIND, IND, REG, -2 )
           IF ( LIND < 7 ) THEN
                CALL ERR_LOG ( 2843, IUER, 'PIMA_READ_SOU_LINE', 'Too few words '// &
     &              'in line '//STR )
                RETURN
           END IF
!
           SOUCAT%NAME = STR(IND(1,1):IND(2,1))
           ALP_STR = STR(IND(1,2):IND(2,2))//'_'// &
     &               STR(IND(1,3):IND(2,3))//'_'// &
     &               STR(IND(1,4):IND(2,4))
!
           DEC_STR = STR(IND(1,5):IND(2,5))//'_'// &
     &               STR(IND(1,6):IND(2,6))//'_'// &
     &               STR(IND(1,7):IND(2,7))
!
           CALL ERR_PASS ( IUER, IER )
           CALL HR_TAT   ( ALP_STR, SOUCAT%ALP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2844, IUER, 'PIMA_READ_SOU_LINE', 'Error in '// &
     &              'decoding right ascension '//ALP_STR(1:I_LEN(ALP_STR))// &
     &              ' in line '//STR_SAVE )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GR_TAT   ( DEC_STR, SOUCAT%DEL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2845, IUER, 'PIMA_READ_SOU_LINE', 'Error in '// &
     &              'decoding declination '//DEC_STR(1:I_LEN(ALP_STR))// &
     &              ' in line '//STR_SAVE )
                RETURN
           END IF
           IF ( LIND .GE. 8 ) THEN
                READ ( UNIT=STR(IND(1,8):IND(2,8)), FMT='(F10.4)' ) SOUCAT%SOU_ERR
                IF ( SOUCAT%SOU_ERR > 900.0 ) THEN
                     SOUCAT%CALIB = 4
                     SOUCAT%SOU_ERR = SOUCAT%SOU_ERR*MAS__TO__RAD
                END IF
           END IF
         ELSE IF ( MODE .EQ. 6 ) THEN
           IF ( STR(50:50) .EQ. 'c'  .OR.  STR(50:50) .EQ. 'C' ) THEN
                SOUCAT%CALIB = 1
           END IF
           IF ( STR(50:50) .EQ. 'n'  .OR.  STR(50:50) .EQ. 'N' ) THEN
                SOUCAT%CALIB = 2
           END IF
           IF ( STR(50:50) .EQ. 'u'  .OR.  STR(50:50) .EQ. 'U' ) THEN
                SOUCAT%CALIB = 3
           END IF
           SOUCAT%NAME       = STR(1:8)
           SOUCAT%ALT_NAME   = STR(11:20)
           SOUCAT%B1950_NAME = STR(23:30)
!
! -------- Alternative source name may be in some cases as long 
! -------- as 16 characters long, but normally it is shorter.
! -------- Truncate it after the first blank
!
           SOUCAT%DB_NAME    = STR(33:48)
           IP = INDEX ( SOUCAT%DB_NAME, ' ' )
           IF ( IP > 0 ) THEN
                CALL CLRCH ( SOUCAT%DB_NAME(IP:) )
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL HR_TAT ( STR(53:65), SOUCAT%ALP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2841, IUER, 'PIMA_READ_SOU_LINE', 'Error in '// &
     &              'decoding right ascension '//STR(53:65)// &
     &              ' in line '//STR_SAVE )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GR_TAT ( STR(67:79), SOUCAT%DEL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2842, IUER, 'PIMA_READ_SOU_LINE', 'Error in '// &
     &              'decoding declination '//STR(67:79)//' in line '//STR_SAVE )
                RETURN
           END IF
           READ ( UNIT = STR(82:87), FMT='(F6.2)', IOSTAT=IER ) SOUCAT%SOU_ERR
           IF ( IER == 0 ) THEN
                SOUCAT%SOU_ERR = SOUCAT%SOU_ERR*MAS__TO__RAD
              ELSE 
                SOUCAT%SOU_ERR = 999.99*MAS__TO__RAD
           END IF
      END IF
!
      IF ( MODE == 1  .OR. &
     &     MODE == 2  .OR. &
     &     MODE == 3       ) THEN
           CALL ERROR_ELLIPSE ( SOUCAT%ALP_ERR*DCOS(DABS(SOUCAT%DEL)), &
     &                          SOUCAT%DEL_ERR, SOUCAT%CORR, SOUCAT%SOU_ERR, &
     &                          ERR2, TETA )
      END IF
!
      SOUCAT%S_VEC(1) = DCOS(SOUCAT%DEL)*DCOS(SOUCAT%ALP)
      SOUCAT%S_VEC(2) = DCOS(SOUCAT%DEL)*DSIN(SOUCAT%ALP)
      SOUCAT%S_VEC(3) = DSIN(SOUCAT%DEL)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_READ_SOU_LINE
