      FUNCTION   CHECK_IVSDIR ( DB_NAME, IVS_DB_URL, DIR_LIS, WGET_EXE, IUER )
! ************************************************************************
! *                                                                      *
! *   Function  CHECK_IVSDIR  determines whether database  DB_NAME  has  *
! *   been submitted to the IVS Data Center. CHECK_IVSDIR uses program   *
! *   WGET to read listing of data directories contents of the Data      *
! *   Center specified by its URL through Internet.                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    DB_NAME ( CHARACTER ) -- Database name. It may contain leading    *
! *                               dollar character, but may not.         *
! * IVS_DB_URL ( CHARACTER ) -- URL of the directory where               *
! *                             subdirectories like 1999, 2000, 2001 etc *
! *                             located. They in turn contain databases. *
! *    DIR_LIS ( CHARACTER ) -- Name of the temporary file where         *
! *                             directory listing will be put.           *
! *   WGET_EXE ( CHARACTER ) -- Command string for calling wget.         *
! *                             It may contain full path to wget or only *
! *                             "wget" provided the directory where wget *
! *                             is located is specified in $PATH         *
! *                             environment variable.                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <CHECK_IVSDIR> ( LOGICAL*4 ) -- result of the check:                 *
! *                                 .TRUE. -- Database has been submitted*
! *                                 to the IVS Data Center.              *
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
! *  ### 17-AUG-2000  CHECK_IVSDIR  v1.1 (c) L. Petrov  07-JAN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      LOGICAL*4  CHECK_IVSDIR
      INTEGER*4  IUER
      CHARACTER  DB_NAME*(*), IVS_DB_URL*(*), DIR_LIS*(*), WGET_EXE*(*)
      CHARACTER  REMOTE_COMSTR*256, URL_DIR*128, IVS_DIR_FIL*128, STR*5, &
     &           DBN*9, DB_YEAR*2, FILE_NAME*80
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 2048 )
      INTEGER*4  IYEAR, PID, IS, IB, IE, IP, ID, J1, NBUF, STAT_BLOCK(12), IER
      CHARACTER  BUF(MBUF)*160
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GETPID, LINDEX, FOR_STAT, SYSTEM
!
      CHECK_IVSDIR = .FALSE.
!
! --- Get rid of leading dollar sign in DB_NAME
!
      CALL CLRCH ( DBN )
      IF ( DB_NAME(1:1) .EQ. '$' ) THEN
           DBN = DB_NAME(2:)
         ELSE
           DBN = DB_NAME
      END IF
!
      CALL CLRCH ( REMOTE_COMSTR )
      CALL CLRCH ( URL_DIR       )
      CALL CLRCH ( IVS_DIR_FIL   )
!
! --- Set exact URL to directory where databases for tyhis year are strored
!
      DB_YEAR = DBN(1:2)
      CALL CHIN ( DB_YEAR, IYEAR )
      IF ( IYEAR .GE. 70 ) THEN
           URL_DIR = IVS_DB_URL(1:I_LEN(IVS_DB_URL))//'19'//DB_YEAR//'/'
         ELSE
           URL_DIR = IVS_DB_URL(1:I_LEN(IVS_DB_URL))//'20'//DB_YEAR(1:2)//'/'
      END IF
!
      PID = GETPID () ! Process ID
      CALL INCH    (  PID, STR(1:5) )
      CALL CHASHR  (       STR(1:5) )
      CALL BLANK_TO_ZERO ( STR(1:5) )
      IVS_DIR_FIL = DIR_LIS(1:I_LEN(DIR_LIS))//'opa_'//STR(1:5)//'.rdl'
!
! --- Build the command line
!
      REMOTE_COMSTR = WGET_EXE(1:I_LEN(WGET_EXE))//' '// &
     &                URL_DIR(1:I_LEN(URL_DIR))//' -q -O '//IVS_DIR_FIL
!      type *,' remote_comstr >>',remote_comstr(1:i_len(remote_comstr))//'<<' ! %
!
! --- Execute command which invlokes wget. Contents of directory listing will
! --- be put in temporary file IVS_DIR_FIL
!
      IS = SYSTEM ( REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5211, IUER, 'CHECK_IVSDIR', 'Error in attempt to '// &
     &         'execute a command "'//REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))// &
     &         '" for getting a database directory of the IVS data center' )
           RETURN
      END IF
!
! --- Open file with listing of remote directory and put in the buffer BUF
!
      IS = FOR_STAT ( IVS_DIR_FIL, STAT_BLOCK )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5212, IUER, 'CHECK_IVSDIR', 'Trap of internal '// &
     &         'control: error in STAT during getting information about '// &
     &         'the listing of a remote directory of IVS data center '// &
     &          IVS_DIR_FIL )
           RETURN
      END IF
!
      IF ( STAT_BLOCK(8) .EQ. 0 ) THEN
!
! -------- The file has zero length. It may occur if we process the first
! -------- session in this year.
!
           CALL CLRCH ( BUF(1) )
           NBUF = 1
         ELSE
!
! -------- REad the listing of the directory
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( IVS_DIR_FIL, MBUF, BUF, NBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5212, IUER, 'CHECK_IVSDIR', 'Error in attempt '// &
     &              'to read a file with listing of a remote directory of '// &
     &              'IVS data center '//IVS_DIR_FIL )
                RETURN
           END IF
      END IF
!
! --- Skan buffer BUF
!
      ID = ILEN(DBN)
      DO 410 J1=1,NBUF
         CALL CLRCH ( FILE_NAME )
         IF ( ILEN(BUF(J1)) .EQ. 0 ) GOTO 410
!
         IP = INDEX ( BUF(J1), '</a>' )
         IF ( IP .GT. 1 ) THEN
              IB = LINDEX ( BUF(J1)(1:IP-1), '>' )
              IF ( IB .GT. 0 ) THEN
                   IB = IB + 1
                   IE = IP - 1
!
! ---------------- Well, we extracted full file name
!
                   FILE_NAME = BUF(J1)(IB:IE)
!
! ---------------- ... now check it
!
                   IF ( FILE_NAME(1:ID) .EQ. DBN(1:ID) ) THEN
                        CHECK_IVSDIR = .TRUE.  ! Uraa-a-a!
                        GOTO 810               ! We have found what we sought
                   END IF
              END IF
        END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Remove temporary file with listing of the IVS database directory
!
      CALL UNLINK ( IVS_DIR_FIL(1:I_LEN(IVS_DIR_FIL))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CHECK_IVSDIR  #!#
