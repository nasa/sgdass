      SUBROUTINE READ_GETFILE ( DBI, MG_DBS, LG_DBS, CG_DBS, &
     &                               MN_DBS, LN_DBS, CN_DBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Procedure  READ_GETFILE  reads the get_list and noget_list from    *
! *   the files DBI.GET_FILE and DBI.NOGET_FILE . It puts contents of    *
! *   this files into the lists CG_DBS and CN_DBS respectively with      *
! *   removing comments.                                                 *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    DBI ( RECORD    ) -- Object with data structure for keeping       *
! *                         configuration of DB_IMPORT utility.          *
! * MG_DBS ( INTEGER*4 ) -- Maximal numbers of the database file names   *
! *                         for get_list.                                *
! * MN_DBS ( INTEGER*4 ) -- Maximal numbers of the database file names.  *
! *                         for noget_list.                              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * LG_DBS ( INTEGER*4 ) -- Numbers of elements in noget_list.           *
! * CG_DBS ( CHARACTER ) -- get_list. Dimension: LG_DBS.                 *
! * LN_DBS ( INTEGER*4 ) -- Numbers of elements in noget_list.           *
! * CN_DBS ( CHARACTER ) -- noget_list. Dimension: LN_DBS.               *
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
! *  ### 19-OCT-2000  READ_GETFILE  v1.0 (c) L. Petrov  19-OCT-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'db_import.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  MG_DBS, LG_DBS, MN_DBS, LN_DBS, IUER
      CHARACTER  CG_DBS(MG_DBS)*(*), CN_DBS(MN_DBS)*(*)
      INTEGER*4    MBUF
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  BUF(MBUF)*80, STR*32
      INTEGER*4  NBUF, J1, J2, IER
      INTEGER*4  I_LEN, ILEN
!
! --- REad file with GET_LIST
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( DBI%GET_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5791, IUER, 'READ_GETFILE', 'Error in attempt to '// &
     &         'read file with get_list: '//DBI%GET_FILE )
           RETURN
      END IF
!
! --- Parse get_list
!
      LG_DBS = 0
      DO 410 J1=1,NBUF
!
! ------ Bypass comment lines and empty lines
!
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410  ! Comment
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410  ! EMpty line
!
! ------ Add the J1-th element to the list
!
         LG_DBS = LG_DBS + 1
         IF ( LG_DBS .GT. MG_DBS ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MG_DBS, STR )
              CALL ERR_LOG ( 5792, IUER, 'READ_GETFILE', 'Too many lines'// &
     &            'in the get-list file '// &
     &             DBI%GET_FILE(1:I_LEN(DBI%GET_FILE))// &
     &            ' Parameter MG_DBS '//STR(1:I_LEN(STR))// &
     &            ' is too small' )
              RETURN
         END IF
         CALL CLRCH ( CG_DBS(LG_DBS) )
         CG_DBS(LG_DBS) = BUF(J1)
 410  CONTINUE
!
! --- Read noget list
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( DBI%NOGET_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5793, IUER, 'READ_GETFILE', 'Error in attempt to '// &
     &         'read file with noget_list: '//DBI%NOGET_FILE )
           RETURN
      END IF
!
! --- Parse noget-list
!
      LN_DBS = 0
      DO 420 J2=1,NBUF
!
! ------ Bypass comment lines and empty lines
!
         IF ( BUF(J2)(1:1)  .EQ. '#' ) GOTO 420  !  Comment
         IF ( ILEN(BUF(J2)) .EQ.  0  ) GOTO 420  !  empty line
!
! ------ Add the J2-th element to the list
!
         LN_DBS = LN_DBS + 1
         IF ( LN_DBS .GT. MN_DBS ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MN_DBS, STR )
              CALL ERR_LOG ( 5794, IUER, 'READ_GETFILE', 'Too many lines'// &
     &            'in the noget-list file '// &
     &             DBI%NOGET_FILE(1:I_LEN(DBI%NOGET_FILE))// &
     &            ' Parameter MN_DBS '//STR(1:I_LEN(STR))// &
     &            ' is too small' )
              RETURN
         END IF
         CALL CLRCH ( CN_DBS(LN_DBS) )
         CN_DBS(LN_DBS) = BUF(J2)
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_GETFILE  #!#
