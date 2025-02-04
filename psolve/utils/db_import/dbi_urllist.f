      SUBROUTINE DBI_URLLIST ( DBI, L_DBS, C_DBS, C_URL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DBI_URLLIST  creates URL list on the basis of the         *
! *   database filename list and URL of the directory where the database *
! *   filename are located. Then the URL list is written in file         *
! *   DBI.URL_FILE .                                                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    DBI ( RECORD    ) -- Object with data structure for keeping       *
! *                         configuration of DB_IMPORT utility.          *
! *  L_DBS ( INTEGER*4 ) -- Numbers of database file names.              *
! *  C_DBS ( CHARACTER ) -- Database filename list. Dimension: L_DBS.    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * CI_URL ( CHARACTER ) -- URL filename list. Dimension: LI_DBS.        *
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
! *  ### 18-OCT-2000  DBI_URLLIST  v1.1 (c)  L. Petrov  27-APR-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'db_import.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  L_DBS, IUER
      CHARACTER  C_DBS(L_DBS)*(*), C_URL(L_DBS)*(*)
      CHARACTER  PID_STR*5, DBS_NAME*32
      INTEGER*4  IYEAR, PID, J1, IER
      INTEGER*4  I_LEN, GETPID
!
! --- Scan database filename list
!
      DO 410 J1=1,L_DBS
         CALL CHIN ( C_DBS(J1)(1:2), IYEAR )
         CALL CLRCH ( C_URL(J1) )
!
! ------ Set the full URL on the basis of the database name. The trick is
! ------ that there is an intermediary direcotry between DBI.IVS_DB_URL and
! ------ the base filename. This directory is a 4-character experiment year
!
         CALL CLRCH ( DBS_NAME )
         DBS_NAME = C_DBS(J1)
         IF ( DBS_NAME(9:9) .EQ. ' ' ) DBS_NAME(9:9) = '_'
         IF ( IYEAR .LE. 70 ) THEN
              C_URL(J1) = DBI%IVS_DB_URL(1:I_LEN(DBI%IVS_DB_URL))// &
     &                    '20'//C_DBS(J1)(1:2)//'/'//DBS_NAME
            ELSE
              C_URL(J1) = DBI%IVS_DB_URL(1:I_LEN(DBI%IVS_DB_URL))// &
     &                    '19'//C_DBS(J1)(1:2)//'/'//DBS_NAME
         END IF
 410  CONTINUE
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (       PID_STR )
      CALL INCH    ( PID,  PID_STR )
      CALL CHASHR  (       PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Buil the filename DBI.URL_FILE where the URL list is to be written
!
      DBI%URL_FILE = DBI%TMP_DIR(1:I_LEN(DBI%TMP_DIR))//'dbi_'//PID_STR//'.url'
!
! --- Remove the old URL file if it acidentally exists
!
      CALL UNLINK ( DBI%URL_FILE(1:I_LEN(DBI%URL_FILE))//CHAR(0) )
!
! --- Write URL list in file DBI.URL_FILE,
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT ( L_DBS, C_URL, DBI%URL_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5761, IUER, 'DBI_URLLIST', 'Error in attempt to '// &
     &         'write URL file '//DBI%URL_FILE )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DBI_URLLIST  #!#
