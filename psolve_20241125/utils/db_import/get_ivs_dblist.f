      SUBROUTINE GET_IVS_DBLIST ( DBI, MT_DBS, LT_DBS, CT_DBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_IVS_DBLIST  returns the database file list for the    *
! *   specified period of dates from the IVS Data Center. It accesses    *
! *   the IVS Data Center by using wget through ftp/http protocol,       *
! *   parses the listing file and extracts database names.               *
! *   GET_IVS_DBLIST returns only the base file names and extension, but *
! *   strips the database filenames from the directory pathnames.        *
! *   If more than one version of the database file for the same         *
! *   experiment exists, then the latest version is taken. The date      *
! *   range is specified as [DBI.DATE_START, DBI.DATE_END] inclusive.    *
! *   IVS Data Center filename listing is retrieved by program wget      *
! *   by using ftp/http protocols.                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    DBI ( RECORD    ) -- Object with data structure for keeping       *
! *                         configuration of DB_IMPORT utility.          *
! * MT_DBS ( INTEGER*4 ) -- Maximal number of database names which       *
! *                         can be returned.                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * LT_DBS ( INTEGER*4 ) -- Numbers of database file names.              *
! * CT_DBS ( CHARACTER ) -- Database filename list. Dimension: LT_DBS.   *
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
! *  History:                                                            *
! *  pet  2001.01.03  Corrected the program in order to allow it to      *
! *                   survive a bug in the IVS Data Center software      *
! *                   discovered on 2001.01.03 -- a dummy file           *
! *                   placeholder appeared sometimes in the directories  *
! *                   where only database files should appear. This file *
! *                   is certainly not a database and GET_IVS_DBLIST     *
! *                   bypasses it without an attempt to interpret.       *
! *                                                                      *
! * ### 17-OCT-2000  GET_IVS_DBLIST  v1.1 (c) L. Petrov 03-JAN-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'db_import.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  MT_DBS, LT_DBS, IUER
      CHARACTER  CT_DBS(MT_DBS)*(*)
      INTEGER*4    M_BUF
      PARAMETER  ( M_BUF = 2048 )
      CHARACTER  YEAR_CH*4, STR*128, PID_STR*5, IVS_DIR_FIL*256, CDATE*10, &
     &           SUFFIX*2, URL_DIR*256, REMOTE_COMSTR*256, BUF(M_BUF)*256, &
     &           CT_DBS_LAST*20, MON(12)*3
      DATA       MON  / &
     &                  'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &                  'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' &
     &                /
      INTEGER*4  IYEAR_START, IYEAR_END, PID, NBUF, IS, IB, IE, IM, IP, &
     &           ISKIP, J1, J2, J3, STATB(12), IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SYSTEM, GETPID, LINDEX, FOR_STAT
!
! --- Check validity of date range
!
      CALL CHIN ( DBI%DATE_START(1:4), IYEAR_START )
      IF ( IYEAR_START .LT. MYEAR_START .OR. IYEAR_START .GT. MYEAR_END ) THEN
           CALL ERR_LOG ( 5731, IUER, 'GET_IVS_DBLIST', 'DATE_START: '// &
     &          DBI%DATE_START//' is out of range' )
           RETURN
      END IF
      CALL CHIN ( DBI%DATE_END(1:4), IYEAR_END )
      IF ( IYEAR_END .LT. MYEAR_START .OR. IYEAR_END .GT. MYEAR_END ) THEN
           CALL ERR_LOG ( 5732, IUER, 'GET_IVS_DBLIST', 'DATE_END: '// &
     &          DBI%DATE_END//' is out of range' )
           RETURN
      END IF
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (       PID_STR )
      CALL INCH    ( PID,  PID_STR )
      CALL CHASHR  (       PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Biild the name of the temporary file where IVS Data Center directory
! --- listings will be put
!
      CALL CLRCH  ( IVS_DIR_FIL )
      IVS_DIR_FIL = DBI%TMP_DIR(1:I_LEN(DBI%TMP_DIR))// &
     &             'db_import_'//PID_STR//'.dpr'
!
      LT_DBS = 0
!
! --- Database fiels in the IVS Data Center are gather in direcotories for each
! --- year of the experiement
! --- Cycle over the experiment years
!
      DO 410 J1=IYEAR_START,IYEAR_END
         CALL CLRCH  ( REMOTE_COMSTR )
         CALL CLRCH  ( URL_DIR       )
         CALL INCH   ( J1, YEAR_CH   )
!
! ------ Build the command line for launching wget
!
         URL_DIR = DBI%IVS_DB_URL(1:I_LEN(DBI%IVS_DB_URL))//YEAR_CH//'/'
         REMOTE_COMSTR = DBI%WGET_EXE(1:I_LEN(DBI%WGET_EXE))//' '// &
     &                   URL_DIR(1:I_LEN(URL_DIR))//' -q -O '// &
     &                   IVS_DIR_FIL
!
! ------ Execute wget. IVS Data Center directory listing is written in
! ------ IVS_DIR_FIL
!
         IS = SYSTEM ( REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))//CHAR(0) )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 5733, IUER, 'GET_IVS_DBLIST', 'Error in '// &
     &            'attempt to execute a command "'// &
     &             REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))// &
     &            '" for getting a database directory of the IVS data center' )
              RETURN
         END IF
!
! ------ Check: was the file of zero length?
!
         IS = FOR_STAT ( IVS_DIR_FIL, STATB )
         IF ( STATB(8) .EQ. 0 ) GOTO 410
!
! ------ Read listing of the IVS Data Center directory
!
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( IVS_DIR_FIL, M_BUF, BUF, NBUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5734, IUER, 'GET_IVS_DBLIST', 'Error in '// &
     &            'attempt to read a file with listing of a remote '// &
     &            'directory of IVS data center '//IVS_DIR_FIL )
              RETURN
         END IF
!
! ------ Parse buffer with listing.
!
         DO 420 J2=1,NBUF
            IF ( ILEN(BUF(J2)) .EQ. 0 ) GOTO 420 ! empty line
!
! --------- Filename is embraced in substrings "</a>" and ">"
!
            IP = INDEX ( BUF(J2), '</a>' )
            IF ( IP .GT. 1 ) THEN
                 IB = LINDEX ( BUF(J2)(1:IP-1), '>' )
                 IF ( IB .GT. 0 ) THEN
!
! ------------------- Well. This line contains database filename
!
                      IB = IB + 1
                      IE = IP - 1
!
! ------------------- We bypass the bug of IVS Data Center software discovered
! ------------------- on 2001.01.03 : it sometimdes puts dummy file
! ------------------- "plaseholder". This file violates IVS specifications.
! ------------------- This file is certainly not a database
!
                      IF ( BUF(J2)(IB:IE) .EQ. 'placeholder' ) GOTO 420
!
                      LT_DBS = LT_DBS + 1
                      IF ( LT_DBS .GT. MT_DBS ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( MT_DBS, STR )
                           CALL ERR_LOG ( 5735, IUER, 'GET_IVS_DBLIST', &
     &                         'too many databases in the IVS Data Center. '// &
     &                         'Parameter MT_DBS '//STR(1:I_LEN(STR))// &
     &                         ' is too small' )
                           RETURN
                      END IF
!
! ------------------- .. then get it! Replace "_" with space if necessary
!
                      CALL CLRCH ( CT_DBS(LT_DBS) )
                      CT_DBS(LT_DBS) = BUF(J2)(IB:IE)
                      IF ( CT_DBS(LT_DBS)(9:9) .EQ. '_' ) THEN
                           CT_DBS(LT_DBS)(9:9) = ' '
                      END IF
!
! ------------------- Now we parse database file name. We extracts database
! ------------------- suffix and experiment date. The experiment date is
! ------------------- decoded in Solve format
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL PARSE_DBNAME ( CT_DBS(LT_DBS)(1:9), CDATE, SUFFIX, &
     &                                    IER )
                      IF ( IER .NE. 0 ) THEN
                           WRITE  ( 6, * ) ' IVS_DIR_FIL = ', &
     &                                       IVS_DIR_FIL(1:I_LEN(IVS_DIR_FIL))
                           WRITE  ( 6, * ) ' Line ', J2 
                           CALL ERR_LOG ( 5736, IUER, 'GET_IVS_DBLIST', &
     &                         'Wrong database name was found in the IVS '// &
     &                         'Data Center: '//CT_DBS(LT_DBS) )
                           RETURN
                      END IF
                      IF ( CDATE .GE. DBI%DATE_START  .AND. &
     &                     CDATE .LE. DBI%DATE_END           ) THEN
!
! ------------------------ Database name fits data range. We teporarily
! ------------------------ transform database filename to another format:
! ------------------------ the first 7 symbols of the database name are
! ------------------------ reaplased with experiment date in Solve format
! ------------------------ for further sorting.
!
                           CT_DBS(LT_DBS) = CDATE//CT_DBS(LT_DBS)(8:17)
                         ELSE
!
! ------------------------ Database name doesn't fit datew range. Play back:
! ------------------------ this database filename will not be included in
! ------------------------ the list
!
                           LT_DBS = LT_DBS -1
                      END IF
!
                 END IF
            END IF
 420     CONTINUE
!
! ------ Remove file with listing of the directory.
!
         CALL UNLINK ( IVS_DIR_FIL(1:I_LEN(IVS_DIR_FIL))//CHAR(0) )
 410  CONTINUE
!
      IF ( LT_DBS .GT. 0 ) THEN
!
! -------- Sort files in chronological order
!
           CALL SORT_CH ( LT_DBS, CT_DBS )
!
! -------- Decode database filename back to the original format
!
           ISKIP = 0
           CALL CLRCH ( CT_DBS_LAST )
           DO 430 J3=1,LT_DBS
              IF ( CT_DBS(J3)(1:12) .EQ. CT_DBS_LAST(1:12) ) THEN
!
! ---------------- If the current database filename has the same date and
! ---------------- suffix as the previous database then the current database
! ---------------- filename replaces the previous one.
! ---------------- This situation may occur in the case when more than one
! ---------------- database version is in the IVS Data Center (it should not
! ---------------- occur, but...). In this case the latest version will be
! ---------------- taken since database filenames are sorted.
!
                   ISKIP = ISKIP + 1
              END IF
              CT_DBS_LAST = CT_DBS(J3)
              CALL CHIN  ( CT_DBS(J3)(6:7), IM ) ! month name
              CT_DBS(J3-ISKIP) = CT_DBS(J3)(3:4)//MON(IM)//CT_DBS(J3)(9:20)// &
     &                           '                    '
 430       CONTINUE
      END IF
      LT_DBS = LT_DBS - ISKIP
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      do 530 j3=1,lt_dbs                                ! %%%%
!         type *,' j3=',j3,' ct_dbs  >>',ct_dbs(j3),'<<' ! %%%%
! 530  continue                                          ! %%%%
!                        call pause ( 'get_ivs_dblist' ) ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_IVS_DBLIST #!#
