      SUBROUTINE TPD_READ ( TPD, TPD_FLAG, VTD_FILE, DBNAME_CH, &
     &                      N_TPD_INIT, TPD_INIT_LIST, FL_TPD_DEBUG, &
     &                      FL_TPD_READ, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine TPD_READ
! *                                                                      *
! *  ### 07-NOV-2007   TPD_READ    v1.0 (c)  L. Petrov  07-NOV-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      TYPE     ( TPD_HEADER__TYPE ) :: HEADER
      TYPE     ( TPD__TYPE        ) :: TPD
      INTEGER*4  TPD_FLAG, N_TPD_INIT, IUER
      CHARACTER  TPD_INIT_LIST(N_TPD_INIT)*(*), VTD_FILE*(*), DBNAME_CH*(*)
      LOGICAL*4  FL_TPD_DEBUG, FL_TPD_READ
      LOGICAL*4  LEX
      CHARACTER  STR*128, FILE_NAME_SAVE*128
      INTEGER*4  NBT, IS, UNIX_DATE, LUN, IL, J1, IER
      INTEGER*8  SIZE_I8
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO 
!
! --- Check whether the input TPD file exists
!
      INQUIRE ( FILE=TPD%FILE_NAME, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
!
! -------- Does not exist? Try alternative initials
!
           IL = ILEN(TPD%FILE_NAME) 
           IF ( N_TPD_INIT > 0  .AND.  IL > 12 ) THEN
                FILE_NAME_SAVE = TPD%FILE_NAME 
!
! ------------- Cycle over alternative initials
!
                DO 410 J1=1,N_TPD_INIT
                   TPD%FILE_NAME = TPD%FILE_NAME(1:IL-12)//TPD_INIT_LIST(J1)// &
     &                             TPD%FILE_NAME(IL-9:IL)
                   INQUIRE ( FILE=TPD%FILE_NAME, EXIST=LEX ) 
                   IF ( LEX ) GOTO 810
 410            CONTINUE 
                TPD%FILE_NAME  = FILE_NAME_SAVE 
           END IF
           IF ( FL_TPD_DEBUG ) THEN
                 INQUIRE ( UNIT=23, OPENED=LEX )
                 IF ( .NOT. LEX ) CALL USE_SPOOL ( 'O' )
                 WRITE ( 23, '(A)' ) ' TPD_READ: Not found '// &
     &                 'input file '//TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))
           END IF
           FL_TPD_READ  = .FALSE.
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
 810  CONTINUE 
!
! --- Open the input TPD file
!
      CALL ERR_PASS  ( IUER,  IER )
      CALL BINF_OPEN ( TPD%FILE_NAME, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           IF ( FL_TPD_DEBUG ) THEN
                 INQUIRE ( UNIT=23, OPENED=LEX )
                 IF ( .NOT. LEX ) CALL USE_SPOOL ( 'O' )
                 WRITE ( 23, '(A)' ) 'TPD_READ: Failure '// &
     &                  'to open input file '//TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))
           END IF
           FL_TPD_READ  = .FALSE.
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, SIZEOF(HEADER), HEADER, NBT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7431, IUER, 'TPD_READ', 'Error in an attempt '// &
     &         'to read the header of the input TPD file '// &
     &          TPD%FILE_NAME )
           RETURN 
      END IF 
!
      IF ( HEADER%LABEL .NE. TPD__LABEL ) THEN
           IF ( FL_TPD_DEBUG ) THEN
                 INQUIRE ( UNIT=23, OPENED=LEX )
                 IF ( .NOT. LEX ) CALL USE_SPOOL ( 'O' )
                 WRITE ( 23, '(A)' ) 'TPD_READ: Wrong header '// &
     &          'in the input TPD file '//TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))
           END IF
           FL_TPD_READ  = .FALSE.
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IF ( ILEN(HEADER%VTD_FILE) > 0 ) THEN
           IF ( VTD_FILE .NE. HEADER%VTD_FILE ) THEN
                IF ( FL_TPD_DEBUG ) WRITE ( 23, '(A)' ) 'TPD_READ: Different '// &
     &              'name of the VTD_FILE specified in the input TPD file '// &
     &               TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))
                FL_TPD_READ  = .FALSE.
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
           IS = FILE_INFO ( HEADER%VTD_FILE(1:I_LEN(HEADER%VTD_FILE))//CHAR(0), &
     &                      UNIX_DATE, SIZE_I8 )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                IF ( FL_TPD_DEBUG ) THEN
                     INQUIRE ( UNIT=23, OPENED=LEX )
                     IF ( .NOT. LEX ) CALL USE_SPOOL ( 'O' )
                     WRITE ( 23, '(A)' ) 'TPD_READ: Error '// &
     &                  STR(1:I_LEN(STR))//' in getting information about the VTD file '// &
     &                  HEADER%VTD_FILE(1:I_LEN(HEADER%VTD_FILE))// &
     &                  ' specified in the input TPD file '// &
     &                  TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))
                END IF
                FL_TPD_READ  = .FALSE.
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
!
           IF ( UNIX_DATE .NE. HEADER%VTD_UNIX_DATE ) THEN
                IF ( FL_TPD_DEBUG ) THEN
                      INQUIRE ( UNIT=23, OPENED=LEX )
                      IF ( .NOT. LEX ) CALL USE_SPOOL ( 'O' )
                      WRITE ( 23, '(A)' ) 'TPD_READ: Different '// &
     &                   ' date in the VTD file '// &
     &                   HEADER%VTD_FILE(1:I_LEN(HEADER%VTD_FILE))// &
     &                   ' specified in the input TPD file '// &
     &                   TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))
                END IF
                FL_TPD_READ  = .FALSE.
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
!
           IF ( DBNAME_CH(1:10) .NE. HEADER%DB_NAME(1:10) ) THEN
                IF ( FL_TPD_DEBUG ) THEN
                     INQUIRE ( UNIT=23, OPENED=LEX )
                     IF ( .NOT. LEX ) CALL USE_SPOOL ( 'O' )
                     WRITE ( 23, '(A)' ) 'TPD_READ: Different '// &
     &                 ' database name '//HEADER%DB_NAME// &
     &                 ' specified in the input TPD file '// &
     &                 TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))//'  '// &
     &                 HEADER%DB_NAME
                END IF
                FL_TPD_READ  = .FALSE.
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
         ELSE 
           IF ( FL_TPD_DEBUG ) THEN
                INQUIRE ( UNIT=23, OPENED=LEX )
                IF ( .NOT. LEX ) CALL USE_SPOOL ( 'O' )
                WRITE ( 23, '(A)' ) ' TPD_READ: NO VTD file '// &
     &            'inside the input file '//TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))
           END IF
      END IF
!
      HEADER = TPD%HEADER 
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, TPD%HEADER%NSTA*SIZEOF(TPD%STA(1)), &
     &                    TPD%STA, NBT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7432, IUER, 'TPD_READ', 'Error in an attempt '// &
     &         'to read the array of station objects from the input TPD '// &
     &         'file '//TPD%FILE_NAME )
           RETURN 
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, TPD%HEADER%NSOU*SIZEOF(TPD%SOU(1)), &
     &                    TPD%SOU, NBT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7433, IUER, 'TPD_READ', 'Error in an attempt '// &
     &         'to red the array of source objects from the input TPD '// &
     &         'file '//TPD%FILE_NAME )
           RETURN 
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, TPD%HEADER%NOBS*SIZEOF(TPD%PARAM(1)), &
     &                    TPD%PARAM, NBT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7434, IUER, 'TPD_READ', 'Error in an attempt '// &
     &         'to read param records from the input TPD file '// &
     &          TPD%FILE_NAME )
           RETURN 
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, TPD%HEADER%NOBS*SIZEOF(TPD%DELAY(1)), &
     &                    TPD%DELAY, NBT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7435, IUER, 'TPD_READ', 'Error in an attempt '// &
     &         'to read delay records from the input TPD file '// &
     &          TPD%FILE_NAME )
           RETURN 
      END IF 
!
      IF ( TPD%HEADER%RATE_USE == SOLVE__YES ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_RECORD ( LUN, TPD%HEADER%NOBS*SIZEOF(TPD%RATE(1)), &
     &                         TPD%RATE, NBT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7436, IUER, 'TPD_READ', 'Error in an '// &
     &             'attempt to read rate records from the input TPD file '// &
     &              TPD%FILE_NAME )
                RETURN 
           END IF 
      END IF 
      FL_TPD_READ  = .TRUE.
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7437, IUER, 'TPD_READ', 'Error in an '// &
     &         'attempt to read the input TPD file '//TPD%FILE_NAME)
           RETURN 
      END IF 
      IF ( FL_TPD_DEBUG ) THEN
           INQUIRE ( UNIT=23, OPENED=LEX )
           IF ( .NOT. LEX ) CALL USE_SPOOL ( 'O' )
           WRITE ( 23, '(A)' ) ' TPD_READ: read file '//TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TPD_READ  !#!#
