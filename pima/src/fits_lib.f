      SUBROUTINE FFITS_OPEN ( FINAM, FPTR, STATUS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine opens FITS file with name FINAM and returns the file       *
! *   pointer FPTR.                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FINAM ( CHARACTER ) -- Name of the input FITS file.                 *
! * STATUS ( CHARACTER ) -- Supported values:                            *
! *                         'NEW', 'OLD', 'UNKNOWN', or 'UPDATE'         *
! *                i                                                     *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   FPTR ( INTEGER*8 ) -- File pointer.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! *  ### 07-OCT-2005   FFITS_OPEN  v2.1 (c)  L. Petrov  05-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FINAM*(*), STATUS*(*)
      INTEGER*8  FPTR
      INTEGER*4  IUER
      LOGICAL*4  LEX
      INTEGER*4  READWRITE, BLOCKSIZE, FT_STATUS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IUER = -1
      BLOCKSIZE = 0
      FT_STATUS = 0
      IF ( ILEN(FINAM) == 0 ) THEN
           CALL ERR_LOG ( 1811, IUER, 'FFITS_OPEN', 'File name is empty' )
           RETURN
      END IF
!
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( STATUS == 'NEW'  .OR.  STATUS == 'new' ) THEN
           IF ( LEX ) THEN
                CALL ERR_LOG ( 1812, IUER, 'FFITS_OPEN', 'File '// &
     &                         FINAM(1:I_LEN(FINAM))//' already exists' )
                RETURN
           END IF
           READWRITE = 1
         ELSE IF ( STATUS == 'OLD' .OR.  STATUS == 'old' ) THEN
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 1813, IUER, 'FFITS_OPEN', 'Cannot find file '// &
     &                         FINAM )
                RETURN
           END IF
           READWRITE = 0
         ELSE IF ( STATUS == 'UPDATE' .OR.  STATUS == 'update' ) THEN
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 1813, IUER, 'FFITS_OPEN', 'Cannot find file '// &
     &                         FINAM )
                RETURN
           END IF
           READWRITE = 1
         ELSE IF ( STATUS == 'UNKNOWN' .OR.  STATUS == 'unknown' ) THEN
           IF ( LEX ) THEN
                CALL UNLINK ( FINAM(1:I_LEN(FINAM))//CHAR(0) )
           END IF
           READWRITE = 1
         ELSE
           CALL ERR_LOG ( 1814, IUER, 'FFITS_OPEN', 'Unsupported parameter '// &
     &                    'status= '//STATUS )
           RETURN
      END IF
!
      IF ( STATUS == 'OLD'                   .OR. &
     &     STATUS == 'UPDATE'                .OR. &
     &     ( STATUS == 'UNKNOWN' .AND. LEX )      ) THEN
           CALL FFOPEN ( FPTR, FINAM(1:I_LEN(FINAM))//CHAR(0), &
     &                   %VAL(READWRITE), FT_STATUS )
         ELSE
           CALL FFINIT ( FPTR, FINAM(1:I_LEN(FINAM))//CHAR(0), FT_STATUS )
      END IF
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1815, IUER, 'FFITS_OPEN', FT_STATUS )
           CALL ERR_LOG ( 1815, IUER, 'FFITS_OPEN', 'Error in attempt '// &
     &         'to open fits-file '//FINAM )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_OPEN  !#!#
!
! ------------------------------------------------------------------------
!
!@      SUBROUTINE FFITS_OPEN ( FINAM, FPTR, IUER )
!@! ************************************************************************
!@! *                                                                      *
!@! *   Auxilliary routine opens FITS file.                                *
!@! *                                                                      *
!@! *  ### 07-OCT-2005   FFITS_OPEN  v1.0 (c)  L. Petrov  07-OCT-2005 ###  *
!@! *                                                                      *
!@! ************************************************************************
!@      IMPLICIT   NONE
!@      CHARACTER  FINAM*(*)
!@      INTEGER*8  FPTR
!@      INTEGER*4  IUER
!@      INTEGER*4  READWRITE, FT_STATUS
!@      INTEGER*4, EXTERNAL :: I_LEN, MALLOC
!@!
!@      READWRITE = 0
!@      FT_STATUS = 0
!@      CALL FFOPEN ( FPTR, FINAM(1:I_LEN(FINAM))//CHAR(0), READWRITE, FT_STATUS )
!@      IF ( FT_STATUS .NE. 0 ) THEN
!@           CALL FT_PRINTERROR ( 1811, IUER, 'FFITS_OPEN', FT_STATUS )
!@           RETURN
!@      END IF
!@      CALL ERR_LOG ( 0, IUER )
!@      RETURN
!@      END  SUBROUTINE  FFITS_OPEN  !#!#
!@!
!@! ------------------------------------------------------------------------
!@!
      SUBROUTINE FFITS_GET_KEYP ( FPTR, MHDR, MKEY, MAX_KEY, LHDR, LKEY, &
     &                            KEYS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FFITS_GET_KEYP
! *                                                                      *
! *   It allocates KEYS object. If MKEY < 0, then for each table,        *
! *   it allocates -MKEY rows.                                           *
! *                                                                      *
! * ### 07-OCT-2005  FFITS_GET_KEYP  v2.1 (c) L. Petrov  05-APR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  FPTR
      INTEGER*4  MHDR, MKEY, MAX_KEY, LHDR, LKEY(MHDR), IUER
      CHARACTER*80, POINTER :: KEYS(:,:)
      CHARACTER  STR*32, STR1*32
      INTEGER*4  FT_STATUS, NHDTYPE, NSPACE, J1, J2, J3, IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FT_STATUS = 0
      CALL FFTHDU ( %VAL(FPTR), LHDR, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1821, IUER, 'FFITS_GET_KEYP', FT_STATUS )
           RETURN
      END IF
      IF ( LHDR > MHDR ) THEN
           CALL CLRCH   ( STR )
           CALL CLRCH   ( STR1 )
           CALL INCH    ( LHDR, STR  )
           CALL INCH    ( MHDR, STR1 )
           CALL ERR_LOG ( 1822, IUER, 'FFITS_GET_KEYP', 'Too many header '// &
     &                    'units: '//STR(1:I_LEN(STR))//', but MHDR='//STR1 )
           RETURN
      END IF
!
      CALL FFGHDT ( %VAL(FPTR), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1823, IUER, 'FFITS_GET_KEYP', FT_STATUS )
           RETURN
      END IF
!
      MAX_KEY = -1
      DO 410 J1=1,LHDR
         CALL FFMAHD ( %VAL(FPTR), %VAL(J1), NHDTYPE, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL FT_PRINTERROR ( 1824, IUER, 'FFITS_GET_KEYP', FT_STATUS )
              RETURN
         END IF
!
         CALL FFGHSP ( %VAL(FPTR), LKEY(J1), NSPACE, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL FT_PRINTERROR ( 1825, IUER, 'FFITS_GET_KEYP', FT_STATUS )
              RETURN
         END IF
!
         IF ( LKEY(J1) > MKEY .AND. MKEY > 0 ) THEN
              CALL CLRCH   ( STR )
              CALL CLRCH   ( STR1 )
              CALL INCH    ( LKEY(J1), STR  )
              CALL INCH    ( MKEY, STR1 )
              CALL ERR_LOG ( 1826, IUER, 'FFITS_GET_KEYP', 'Too many header '// &
     &                      'keys: '//STR(1:I_LEN(STR))//' but MKEY='//STR1 )
              RETURN
         END IF
         IF ( LKEY(J1) > MAX_KEY ) MAX_KEY = LKEY(J1)
 410  CONTINUE
!
      IF ( MKEY > 0 ) THEN
           ALLOCATE ( KEYS(MAX_KEY,LHDR), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( LEN(KEYS(1,1))*MAX_KEY*LHDR, STR )
                CALL ERR_LOG ( 1827, IUER, 'FFITS_GET_KEYP', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                        'dynamic memory' )
                RETURN
           END IF
        ELSE IF ( MKEY < 0 ) THEN
           ALLOCATE ( KEYS(-MKEY,LHDR), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( -LEN(KEYS(1,1))*MKEY*LHDR, STR )
                CALL ERR_LOG ( 1828, IUER, 'FFITS_GET_KEYP', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                        'dynamic memory' )
                RETURN
          END IF
      END IF
!
      DO 420 J2=1,LHDR
         CALL FFMAHD ( %VAL(FPTR), %VAL(J2), NHDTYPE, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL FT_PRINTERROR ( 1829, IUER, 'FFITS_GET_KEYP', FT_STATUS )
              RETURN
         END IF
!
         DO 430 J3=1,LKEY(J2)
            CALL FFGREC ( %VAL(FPTR), %VAL(J3), %REF(KEYS(J3,J2)), FT_STATUS )
            IF ( FT_STATUS .NE. 0 ) THEN
                 CALL FT_PRINTERROR ( 1830, IUER, 'FFITS_GET_KEYP', FT_STATUS )
                 RETURN
            END IF
            IP = INDEX ( KEYS(J3,J2), CHAR(0) )
            IF ( IP > 0 ) CALL CLRCH ( KEYS(J3,J2)(IP:) )
 430     CONTINUE
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_GET_KEYP  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFITS_GET_KEYS ( FPTR, MHDR, MKEY, LHDR, LKEY, KEYS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FFITS_GET_KEYS
! *                                                                      *
! * ### 07-OCT-2005  FFITS_GET_KEYS  v1.1 (c) L. Petrov  05-APR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  FPTR
      INTEGER*4  MHDR, MKEY, LHDR, LKEY(MHDR), IUER
      CHARACTER  KEYS(MKEY,MHDR)*(*)
      CHARACTER  STR*32, STR1*32
      INTEGER*4  FT_STATUS, NHDTYPE, NSPACE, J1, J2, IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FT_STATUS = 0
      CALL FFTHDU ( %VAL(FPTR), LHDR, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1831, IUER, 'FFITS_GET_KEYS', FT_STATUS )
           RETURN
      END IF
      IF ( LHDR > MHDR ) THEN
           CALL CLRCH   ( STR )
           CALL CLRCH   ( STR1 )
           CALL INCH    ( LHDR, STR  )
           CALL INCH    ( MHDR, STR1 )
           CALL ERR_LOG ( 1832, IUER, 'FFITS_GET_KEYS', 'Too many header '// &
     &                   'units: '//STR(1:I_LEN(STR))//', but MHDR='//STR1 )
           RETURN
      END IF
!
      CALL FFGHDT ( %VAL(FPTR), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1833, IUER, 'FFITS_GET_KEYS', FT_STATUS )
           RETURN
      END IF
!
      DO 410 J1=1,LHDR
         CALL FFMAHD ( %VAL(FPTR), %VAL(J1), NHDTYPE, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL FT_PRINTERROR ( 1834, IUER, 'FFITS_GET_KEYS', FT_STATUS )
              RETURN
         END IF
!
         CALL FFGHSP ( %VAL(FPTR), LKEY(J1), NSPACE, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL FT_PRINTERROR ( 1835, IUER, 'FFITS_GET_KEYS', FT_STATUS )
              RETURN
         END IF
!
         IF ( LKEY(J1) > MKEY ) THEN
              CALL CLRCH   ( STR )
              CALL CLRCH   ( STR1 )
              CALL INCH    ( LKEY(J1), STR  )
              CALL INCH    ( MKEY, STR1 )
              CALL ERR_LOG ( 1836, IUER, 'FFITS_GET_KEYS', 'Too many header '// &
     &                      'keys: '//STR(1:I_LEN(STR))//' but MKEY='//STR1 )
              RETURN
         END IF
!
         DO 420 J2=1,LKEY(J1)
            CALL FFGREC ( %VAL(FPTR), %VAL(J2), %REF(KEYS(J2,J1)), FT_STATUS )
            IF ( FT_STATUS .NE. 0 ) THEN
                 CALL FT_PRINTERROR ( 1837, IUER, 'FFITS_GET_KEYS', FT_STATUS )
                 RETURN
            END IF
            IP = INDEX ( KEYS(J2,J1), CHAR(0) )
            IF ( IP > 0 ) CALL CLRCH ( KEYS(J2,J1)(IP:) )
 420     CONTINUE
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_GET_KEYS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFITS_GETI2 ( FPTR, L_TAB, L_ROW, KEY, NEL, ARR_I2, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFITS_GETI2  returns an array of NEL elements of          *
! *   an INTEGER*2 type array ARR_I2 from the column wiht name KEY,      *
! *   row L_ROW, binary or ascii table L_TAB from the contents of the    *
! *   file in fits format which has the associated pointer to FITSIO     *
! *   internal data structure FPTR.                                      *
! *                                                                      *
! * ___________________________ Input parameters: _______________________*
! *                                                                      *
! *   FPTR ( INTEGER*8 ) -- The unit where the file in fits format was   *
! *                         opened.                                      *
! *  L_TAB ( INTEGER*4 ) -- Index of the table.                          *
! *  L_ROW ( INTEGER*4 ) -- The raw of the table where the array will    *
! *                         be read.                                     *
! *    KEY ( CHARACTER ) -- Name of the key which identifies the column. *
! *    NEL ( INTEGER*4 ) -- The number of elements from the column to    *
! *                         be read. Should not exceed the maximal       *
! *                         number of elelments.                         *
! *                                                                      *
! * ___________________________ Output parameters: ______________________*
! *                                                                      *
! * ARR_I2 ( INTEGER*2 ) -- Output array.                                *
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
! *  ### 09-DEC-2005  FFITS_GETI2  v1.1 (c)  L. Petrov  05-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  FPTR
      INTEGER*4  L_TAB, L_ROW, NEL, IUER
      INTEGER*4  ARR_I2(*)
      CHARACTER  KEY*(*)
      CHARACTER  STR*5
      LOGICAL*4  ANYF
      INTEGER*8  I8_VAR1, I8_VAR2, I8_VAR3
      INTEGER*4  IND_COL, FT_STATUS, HDUTYPE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get the column index
!
      STR = KEY(6:8)
      CALL CHASHR ( STR )
      CALL CHIN   ( STR, IND_COL )
      IF ( IND_COL .LE. 0  .OR.  IND_COL > 99999 ) THEN
           CALL ERR_LOG ( 1841, IUER, 'FFITS_GETI2', 'Wrong key '// &
     &          KEY(1:I_LEN(KEY))//' -- cannot extract the column index' )
           RETURN
      END IF
!
! --- Set the CDU to the the requrest table index
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(L_TAB), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1842, IUER, 'FFITS_GETI2', FT_STATUS )
           RETURN
      END IF
!
! --- Get the array
!
      I8_VAR1   = L_ROW
      I8_VAR2   = 1
      I8_VAR3   = NEL
      FT_STATUS = 0
      CALL FFGCVI ( %VAL(FPTR), %VAL(IND_COL), %VAL(I8_VAR1), %VAL(I8_VAR2), &
     &              %VAL(I8_VAR3), %VAL(0), ARR_I2, ANYF, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1843, IUER, 'FFITS_GETI2', FT_STATUS )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_GETI2  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFITS_GETI4 ( FPTR, L_TAB, L_ROW, KEY, NEL, ARR_I4, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFITS_GETI4  returns an array of NEL elements of          *
! *   an INTEGER*4 type array ARR_I4 from the column with name KEY,      *
! *   row L_ROW, binary or ascii table L_TAB from the contents of the    *
! *   file in fits format which has the associated pointer to FITSIO     *
! *   internal data structure FPTR.                                      *
! *                                                                      *
! * ___________________________ Input parameters: _______________________*
! *                                                                      *
! *   FPTR ( INTEGER*8 ) -- The unit where the file in fits format was   *
! *                         opened.                                      *
! *  L_TAB ( INTEGER*4 ) -- Index of the table.                          *
! *  L_ROW ( INTEGER*4 ) -- The raw of the table where the array will    *
! *                         be read.                                     *
! *    KEY ( CHARACTER ) -- Name of the key which identifies the column. *
! *    NEL ( INTEGER*4 ) -- The number of elements from the column to    *
! *                         be read. Should not exceed the maximal       *
! *                         number of elelments.                         *
! *                                                                      *
! * ___________________________ Output parameters: ______________________*
! *                                                                      *
! * ARR_I4 ( INTEGER*4 ) -- Output array.                                *
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
! *  ### 09-DEC-2005  FFITS_GETI4  v1.2 (c)  L. Petrov  05-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  FPTR
      INTEGER*4  L_TAB, L_ROW, NEL, IUER
      INTEGER*4  ARR_I4(*)
      CHARACTER  KEY*(*)
      CHARACTER  STR*5
      LOGICAL*4  ANYF
      INTEGER*8  I8_VAR1, I8_VAR2, I8_VAR3
      INTEGER*4  IND_COL, FT_STATUS, HDUTYPE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get the column index
!
      STR = KEY(6:8)
      CALL CHASHR ( STR )
      CALL CHIN   ( STR, IND_COL )
      IF ( IND_COL .LE. 0  .OR.  IND_COL > 99999 ) THEN
           CALL ERR_LOG ( 1851, IUER, 'FFITS_GETI4', 'Wrong key '// &
     &          KEY(1:I_LEN(KEY))//' -- cannot extract the column index' )
           RETURN
      END IF
!
! --- Set the CDU to the the requrest table index
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(L_TAB), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1852, IUER, 'FFITS_GETI4', FT_STATUS )
           RETURN
      END IF
!
! --- Get the array
!
      I8_VAR1  =   L_ROW
      I8_VAR2  =   1
      I8_VAR3  =   NEL
      FT_STATUS = 0
#ifdef ADR_32BIT
      CALL FFGCVJ ( %VAL(FPTR), %VAL(IND_COL), %VAL(I8_VAR1), %VAL(I8_VAR2), &
     &              %VAL(I8_VAR3), %VAL(0), ARR_I4, ANYF, FT_STATUS )
#else
      CALL FFGCVK ( %VAL(FPTR), %VAL(IND_COL), %VAL(I8_VAR1), %VAL(I8_VAR2), &
     &              %VAL(I8_VAR3), %VAL(0), ARR_I4, ANYF, FT_STATUS )
#endif
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1853, IUER, 'FFITS_GETI4', FT_STATUS )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_GETI4  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFITS_GETR4 ( FPTR, L_TAB, L_ROW, KEY, NEL, ARR_R4, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFITS_GETR4  returns an array of NEL elements of          *
! *   an REAL*4 type array ARR_R4 from the column wiht name KEY,         *
! *   row L_ROW, binary or ascii table L_TAB from the contents of the    *
! *   file in fits format which has the associated pointer to FITSIO     *
! *   internal data structure FPTR.                                      *
! *                                                                      *
! * ___________________________ Input parameters: _______________________*
! *                                                                      *
! *   FPTR ( INTEGER*8 ) -- The unit where the file in fits format was   *
! *                         opened.                                      *
! *  L_TAB ( INTEGER*4 ) -- Index of the table.                          *
! *  L_ROW ( INTEGER*4 ) -- The raw of the table where the array will    *
! *                         be read.                                     *
! *    KEY ( CHARACTER ) -- Name of the key which identifies the column. *
! *    NEL ( INTEGER*4 ) -- The number of elements from the column to    *
! *                         be read. Should not exceed the maximal       *
! *                         number of elelments.                         *
! *                                                                      *
! * ___________________________ Output parameters: ______________________*
! *                                                                      *
! * ARR_R4 ( REAL*4    ) -- Output array.                                *
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
! *  ### 09-DEC-2005  FFITS_GETR4  v1.1 (c)  L. Petrov  05-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  FPTR
      INTEGER*4  L_TAB, L_ROW, NEL, IUER
      REAL*4     ARR_R4(*)
      CHARACTER  KEY*(*)
      CHARACTER  STR*5
      LOGICAL*4  ANYF
      INTEGER*8  I8_VAR1, I8_VAR2, I8_VAR3
      INTEGER*4  IND_COL, FT_STATUS, HDUTYPE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get the column index
!
      STR = KEY(6:8)
      CALL CHASHR ( STR )
      CALL CHIN   ( STR, IND_COL )
      IF ( IND_COL .LE. 0  .OR.  IND_COL > 99999 ) THEN
           CALL ERR_LOG ( 1861, IUER, 'FFITS_GETR4', 'Wrong key '// &
     &          KEY(1:I_LEN(KEY))//' -- cannot extract the column index' )
           RETURN
      END IF
!
! --- Set the CDU to the requested table index
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(L_TAB), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1862, IUER, 'FFITS_GETR4', FT_STATUS )
           RETURN
      END IF
!
! --- Get the array
!
      I8_VAR1   = L_ROW
      I8_VAR2   = 1
      I8_VAR3   = NEL
      FT_STATUS = 0
      CALL FFGCVE ( %VAL(FPTR), %VAL(IND_COL), %VAL(I8_VAR1), %VAL(I8_VAR2), &
     &              %VAL(I8_VAR3), %VAL(0.0E0), ARR_R4, ANYF, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1863, IUER, 'FFITS_GETR4', FT_STATUS )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_GETR4  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFITS_GETR8 ( FPTR, L_TAB, L_ROW, KEY, NEL, ARR_R8, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFITS_GETR8  returns an array of NEL elements of          *
! *   an REAL*8 type array ARR_R8 from the column wiht name KEY,         *
! *   row L_ROW, binary or ascii table L_TAB from the contents of the    *
! *   file in fits format which has the associated pointer to FITSIO     *
! *   internal data structure FPTR.                                      *
! *                                                                      *
! * ___________________________ Input parameters: _______________________*
! *                                                                      *
! *   FPTR ( INTEGER*8 ) -- The unit where the file in fits format was   *
! *                         opened.                                      *
! *  L_TAB ( INTEGER*4 ) -- Index of the table.                          *
! *  L_ROW ( INTEGER*4 ) -- The raw of the table where the array will    *
! *                         be read.                                     *
! *    KEY ( CHARACTER ) -- Name of the key which identifies the column. *
! *    NEL ( INTEGER*4 ) -- The number of elements from the column to    *
! *                         be read. Should not exceed the maximal       *
! *                         number of elelments.                         *
! *                                                                      *
! * ___________________________ Output parameters: ______________________*
! *                                                                      *
! * ARR_R8 ( REAL*8    ) -- Output array.                                *
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
! *  ### 09-DEC-2005  FFITS_GETR8  v1.1 (c)  L. Petrov  05-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  FPTR
      INTEGER*4  L_TAB, L_ROW, NEL, IUER
      REAL*8     ARR_R8(*)
      CHARACTER  KEY*(*)
      CHARACTER  STR*5
      LOGICAL*4  ANYF
      INTEGER*8  I8_VAR1, I8_VAR2, I8_VAR3
      INTEGER*4  IND_COL, FT_STATUS, HDUTYPE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get the column index
!
      STR = KEY(6:8)
      CALL CHASHR ( STR )
      CALL CHIN   ( STR, IND_COL )
      IF ( IND_COL .LE. 0  .OR.  IND_COL > 99999 ) THEN
           CALL ERR_LOG ( 1871, IUER, 'FFITS_GETR8', 'Wrong key '// &
     &          KEY(1:I_LEN(KEY))//' -- cannot extract the column index' )
           RETURN
      END IF
!
! --- Set the CDU to the the requrest table index
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(L_TAB), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1872, IUER, 'FFITS_GETR8', FT_STATUS )
           RETURN
      END IF
!
! --- Get the array
!
      FT_STATUS = 0
      I8_VAR1   = L_ROW
      I8_VAR2   = 1
      I8_VAR3   = NEL
      FT_STATUS = 0
      CALL FFGCVD ( %VAL(FPTR), %VAL(IND_COL), %VAL(I8_VAR1), %VAL(I8_VAR2), &
     &              %VAL(I8_VAR3), %VAL(0.0D0), ARR_R8, ANYF, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1873, IUER, 'FFITS_GETR8', FT_STATUS )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_GETR8  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFITS_GETCH ( FPTR, L_TAB, L_ROW, KEY, NEL, STR_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFITS_GETCH  returns a character string STR_OUT from the  *
! *   column with name KEY, row L_ROW, binary or ascii table L_TAB from  *
! *   file in fits format which has the associated pointer to FITSIO     *
! *   internal data structure FPTR.                                      *
! *                                                                      *
! * ___________________________ Input parameters: _______________________*
! *                                                                      *
! *   FPTR ( INTEGER*8 ) -- The unit where the file in fits format was   *
! *                         opened.                                      *
! *  L_TAB ( INTEGER*4 ) -- Index of the table.                          *
! *  L_ROW ( INTEGER*4 ) -- The row of the table where the array will    *
! *                         be read.                                     *
! *    KEY ( CHARACTER ) -- Name of the key which identifies the column. *
! *    NEL ( INTEGER*4 ) -- The number of elements from the column to    *
! *                         be read. Should not exceed the maximal       *
! *                         number of elelments.                         *
! *                                                                      *
! * ___________________________ Output parameters: ______________________*
! *                                                                      *
! * STR_OUT ( CHARACTER ) -- Output array of strings.                    *
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
! *  ### 09-DEC-2005  FFITS_GETCH  v1.2 (c)  L. Petrov  05-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  FPTR
      INTEGER*4  L_TAB, L_ROW, NEL, IUER
      CHARACTER  STR_OUT(*)*(*)
      CHARACTER  KEY*(*)
      CHARACTER  STR*5
      LOGICAL*4  ANYF
      INTEGER*8  I8_VAR1, I8_VAR2, I8_VAR3
      INTEGER*1, ALLOCATABLE :: BUF(:)
      ADDRESS__TYPE, ALLOCATABLE :: DESC(:)
      INTEGER*4  J1, J2, IND_COL, FT_STATUS, HDUTYPE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get the column index
!
      STR = KEY(6:8)
      CALL CHASHR ( STR )
      CALL CHIN   ( STR, IND_COL )
      IF ( IND_COL .LE. 0  .OR.  IND_COL > 99999 ) THEN
           CALL ERR_LOG ( 1881, IUER, 'FFITS_GETCH', 'Wrong key '// &
     &          KEY(1:I_LEN(KEY))//' -- cannot extract the column index' )
           RETURN
      END IF
!
! --- Set the CDU to the the requrest table index
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(L_TAB), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1882, IUER, 'FFITS_GETCH', FT_STATUS )
           RETURN
      END IF
!
! --- Get the array
!
      FT_STATUS = 0
      I8_VAR1   = L_ROW
      I8_VAR2   = 1
      I8_VAR3   = NEL
!
! --- The problem is that C returns strings with CHAR(0) attached to the end
! --- We have to allocate a buffer which will take extended string
!
      ALLOCATE ( BUF((LEN(STR_OUT)+1)*NEL) )
      ALLOCATE ( DESC(NEL) )
      DO 410 J1=1,NEL
         DESC(J1) = LOC(BUF) + (LEN(STR_OUT)+1)*(J1-1)
 410  CONTINUE
      CALL FFGCVS ( %VAL(FPTR), %VAL(IND_COL), %VAL(I8_VAR1), &
     &              %VAL(I8_VAR2), %VAL(I8_VAR3), %REF('A'//CHAR(0)), &
     &              DESC, ANYF, FT_STATUS )
!
! --- ... and then copy it back
!
      DO 420 J2=1,NEL
         CALL MEMCPY ( STR_OUT(J2), %VAL(DESC(J2)) ) ! NB hidden 3rd argument
 420  CONTINUE
      DEALLOCATE ( BUF  )
      DEALLOCATE ( DESC )
!
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1883, IUER, 'FFITS_GETCH', FT_STATUS )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_GETCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFITS_CLOSE ( FPTR, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine closes FITS file.                               *
! *                                                                      *
! *  ### 07-OCT-2005   FFITS_CLOSE  v1.1 (c)  L. Petrov  05-APR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  FPTR
      INTEGER*4  IUER
      INTEGER*4  FT_STATUS
!
      FT_STATUS = 0
      CALL FFCLOS ( %VAL(FPTR), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1891, IUER, 'FFITS_CLOSE', FT_STATUS )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFITS_CLOSE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FT_PRINTERROR ( ICODE, IUER, PROG_NAME, FT_STATUS )
! ************************************************************************
! *                                                                      *
! *   Ausilliary routine FT_PRINTERROR
! *                                                                      *
! * ### 07-OCT-2005  FT_PRINTERROR  v1.0 (c)  L. Petrov 07-OCT-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  ICODE, IUER, FT_STATUS
      CHARACTER  PROG_NAME*(*)
      CHARACTER  ERRMESSAGE*80
      INTEGER*4  J1
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      DO 410 J1=1,32
         IF ( J1 == 1 ) THEN
              CALL CLRCH  ( ERRMESSAGE )
              ERRMESSAGE(80:80) = CHAR(0)
              CALL FFGERR ( %VAL(FT_STATUS), ERRMESSAGE )
            ELSE
              CALL CLRCH  ( ERRMESSAGE )
              ERRMESSAGE(80:80) = CHAR(0)
              CALL FFGMSG ( ERRMESSAGE )
         END IF
         IF ( ILEN(ERRMESSAGE) > 0 ) THEN
              CALL ERR_LOG ( ICODE, IUER, PROG_NAME, 'CFITSIO_error: '// &
     &                       ERRMESSAGE )
            ELSE
              RETURN
         END IF
 410  CONTINUE
      RETURN
      END  SUBROUTINE  FT_PRINTERROR !#!
