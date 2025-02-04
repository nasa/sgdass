      FUNCTION CHECK_SOLVE_INITIALS ( ACTION, SOLVE_INITIALS, IUER )
! ************************************************************************
! *                                                                      *
! *   Function CHECK_SOLVE_INITIALS checks:                              *
! *   1) whether solve user initials SOLVE_INITIALS are defined in       *
! *      letok file.                                                     *
! *   2) whether sovle user initials SOLVE_INITIALS are already in use.  *
! *                                                                      *
! *   If Solve user initials are correct and not in use, then the        *
! *   routine execute the actiion.                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *         ACTION ( CHARACTER ) -- Action code: one of                  *
! *                                'R' -- set read lock after checking,  *
! *                                'W' -- set write lock after checking. *
! * SOLVE_INITIALS ( CHARACTER ) -- solve user initials (must be in      *
! *                                 upper case) to be checked.           *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <CHECK_SOLVE_INITIALS> ( LOGICAL*4 ) -- .TURE. if Solve user         *
! *                                         initials are correct and not *
! *                                         in use.                      *
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
! * ### 14-AUG-00 CHECK_SOLVE_INITIALS v2.0 (c) L. Petrov 07-SEP-00 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE    'precm.i'
      CHARACTER  SOLVE_INITIALS*(*)
      INTEGER*4  IUER, FDES, MBUF
      PARAMETER  ( MBUF = 256 )
      CHARACTER  ACTION*1, LETOK_FINAM*128, LOCK_FINAM*128, BUF(MBUF)*80
      LOGICAL*4  CHECK_SOLVE_INITIALS
      LOGICAL*4  LEX
      INTEGER*4  J1, NBUF, MAKE_SEMA, IER
      INTEGER*4  I_LEN
!
      CHECK_SOLVE_INITIALS = .TRUE.
      IF ( ACTION .NE. '$'  ) THEN
!
! -------- We do not need it any more.
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      CHECK_SOLVE_INITIALS = .FALSE.
!
! --- Some initials checks
!
      IF ( ACTION .NE. 'C'  .AND. ACTION .NE. 'R'  .AND.  ACTION .NE. 'W' ) THEN
           CALL ERR_LOG ( 5131, IUER, 'CHECK_SOLVE_INITIALS', 'Wrong action '// &
     &         'is specified: '//ACTION//'  C, R or W were expected' )
           RETURN
      END IF
!
      IF ( PRE_SV_LEN .LE. 0  .OR.  PRE_SV_LEN .GT. LEN(PRE_SAV_DIR) .OR. &
     &     PRE_SD_LEN .LE. 0  .OR.  PRE_SD_LEN .GT. LEN(PRE_SCR_DIR)     ) THEN
           CALL ERR_LOG ( 5132, IUER, 'CHECK_SOLVE_INITIALS', 'Common block '// &
     &         'precm has not been defined' )
           RETURN
      END IF
!
! --- Build filename which keeps solve user initials
!
      CALL CLRCH ( LETOK_FINAM )
      CALL CLRCH ( LOCK_FINAM  )
      LETOK_FINAM = PRE_SAV_DIR(1:PRE_SV_LEN)//'letok'
      LOCK_FINAM  = PRE_SCR_DIR(1:PRE_SD_LEN)//'LOCK'//SOLVE_INITIALS
!
! --- Check: whether this file exist
!
      INQUIRE ( FILE=LETOK_FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5133, IUER, 'CHECK_SOLVE_INITIALS', 'Letok file '// &
     &         'which keeps solve user initials: '// &
     &          LETOK_FINAM(1:I_LEN(LETOK_FINAM))//' was not found ' )
           RETURN
      END IF
!
! --- Read Letok file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( LETOK_FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5134, IUER, 'RESOLVE_DBNAME', 'Error in attempt '// &
     &         'to read letok file '//LETOK_FINAM )
           RETURN
      END IF
!
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:2) .EQ. SOLVE_INITIALS ) CHECK_SOLVE_INITIALS = .TRUE.
 410  CONTINUE
!
      IF ( CHECK_SOLVE_INITIALS ) THEN
!
! -------- Try to lock user initials; quit if already locked
!
           IF ( ACTION .EQ. 'R' ) THEN
                FDES = MAKE_SEMA ( LOCK_FINAM, 'R', 'N' )
              ELSE IF ( ACTION .EQ. 'W' ) THEN
                FDES = MAKE_SEMA ( LOCK_FINAM, 'W', 'N' )
           END IF
           IF ( FDES .EQ. -1 ) THEN
                CHECK_SOLVE_INITIALS = .FALSE.
                CALL ERR_LOG ( 5135, IUER, 'RESOLVE_DBNAME', 'Sorry, but '// &
     &              'Solve user initials '//SOLVE_INITIALS//' are already '// &
     &              'in use' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CHECK_SOLVE_INITIALS  #!#
