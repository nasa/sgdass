#include <mk5_preprocessor_directives.inc>
      SUBROUTINE ANC_READ ( FILIN, MBUF, NBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ANC_READ 
! *                                                                      *
! *  ### 07-MAY-2021    ANC_READ   v1.0 (c)  L. Petrov  07-MAY-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      INTEGER*4  MBUF, NBUF, IUER 
      CHARACTER  FILIN*128, BUF(MBUF)*(ANC__MSTR)
      CHARACTER  FILTMP*128, STR*128, COM*256, SHM_DIR*7
      PARAMETER  ( SHM_DIR = '/dev/shm' )
      LOGICAL*1  FL_BZIP2
      INTEGER*4  IP, IS, IL, PID, NTHR, IER
      INTEGER*8  DIR_DESC
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4, EXTERNAL :: CLOSEDIR, GETPID, SYSTEM, ILEN, I_LEN,     &
     &                       OMP_GET_THREAD_NUM
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
!
      IL = ILEN(FILIN)
      IF ( IL < 4 ) IL = 4
      IF ( FILIN(IL-3:IL) == '.bz2' ) THEN
         PID = GETPID()
         CALL INCH ( PID, FILTMP(1:8) )
         CALL CHASHR    ( FILTMP(1:8) )
         CALL BLANK_TO_ZERO ( FILTMP(1:8) )
         IF ( OMP_IN_PARALLEL() ) THEN
            FILTMP(9:9) = '_'
            CALL INCH ( OMP_GET_THREAD_NUM(), FILTMP(10:13) )
            CALL CHASHR    ( FILTMP(10:13) )
            CALL BLANK_TO_ZERO ( FILTMP(10:13) )
         END IF 
!          
         DIR_DESC = FUNC_OPENDIR ( TRIM(SHM_DIR)//CHAR(0) )
         IF ( DIR_DESC .EQ. 0 ) THEN
            FILTMP = '/dev/shm'//'/'//FILTMP
         ELSE
            IP = CLOSEDIR ( %VAL(DIR_DESC) )
            FILTMP = TRIM(SHM_DIR)//'/'//FILTMP
         END IF
!
! ------ Honor environment variable OMP_NUM_THREADS.
! ------ We limit the number of threads for lbzip2
!
         CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
         IF ( ILEN(STR) > 0 ) THEN
            CALL CHIN ( STR, NTHR )
            IF ( NTHR < 1 ) NTHR = 1
            CALL CLRCH ( STR ) 
            CALL INCH  ( NTHR, STR )
            STR = '-n '//STR
         ELSE
!
! ------- ... or do not use any limit when the variable is not set up
!
            CALL CLRCH ( STR )
         END IF
!
         FL_BZIP2 = .TRUE.
         IF ( OMP_IN_PARALLEL() ) THEN
            COM = 'lbzip2 -n 1 -dfc '//                                 &
     &            FILIN(1:I_LEN(FILIN))//' > '//FILTMP
         ELSE
            COM = 'lbzip2 '//STR(1:I_LEN(STR))//' -dsfc '//             &
     &            FILIN(1:I_LEN(FILIN))//' > '//FILTMP
         END IF
         IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
         IF ( IS .NE. 0 ) THEN
!
! --------- lbzip2 may fail because of "Cannot allocate memory". As a desperate
! --------- attempt we try once more with using bzip2 and with only one thread 
! --------- and a small block size
!
            CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
            COM = 'bzip2 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
            IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
         END IF
         IF ( IS .NE. 0 ) THEN
            WRITE ( 6, * ) 'System: IS = ', IS
            CALL CLRCH  ( STR )
            CALL GERROR ( STR )
            CALL ERR_LOG ( 6850, IUER, 'ANC_READ',                      &
     &              'Failure to uncompress the input heb-file '//       &
     &              FILIN(1:I_LEN(FILIN))//' using command '//          &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
            IF ( FL_BZIP2 ) THEN 
               CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
            END IF
            RETURN
         END IF
      ELSE
         FL_BZIP2 = .FALSE.
         FILTMP = FILIN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILTMP, MBUF, BUF, NBUF, IER )
      IF ( FL_BZIP2 ) THEN
         CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
      END IF
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6851, IUER, 'ANC_READ',                         &
     &           'Failure in reading antenna calibration file '//       &
     &            TRIM(FILTMP) )
         RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ANC_READ  !#!
