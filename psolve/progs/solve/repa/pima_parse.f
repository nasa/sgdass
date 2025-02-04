      SUBROUTINE PIMA_PARSE ( PIMA_CNT, FPL_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_PARSE 
! *                                                                      *
! *  ### 06-FEB-2010  PIMA_PARSE   v1.1 (c)  L. Petrov  03-SEP-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  PIMA_CNT*(*), FPL_DIR*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 2048 )
      PARAMETER  ( MIND =   64 )
      CHARACTER  BUF(MBUF)*256
      CHARACTER  REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      CHARACTER  SESS_CODE*128, EXPER_DIR*128, STR*128
      INTEGER*4  J1, J2, IP, IND(2,MIND), LIND, NBUF, IER
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: OPENDIR, CLOSEDIR 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIMA_CNT, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3711, IUER, 'PIMA_PARSE', 'Failure in an attempt '// &
     &         'to read pima control file '//PIMA_CNT )
           RETURN 
      END IF
!
      CALL CLRCH ( SESS_CODE )
      CALL CLRCH ( EXPER_DIR )
      DO 410 J1=1,NBUF
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( LIND .GE. 2 ) THEN
              IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SESS_CODE:' ) THEN
                   SESS_CODE = BUF(J1)(IND(1,2):IND(2,2)) 
                 ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EXPER_DIR:' ) THEN
                   EXPER_DIR = BUF(J1)(IND(1,2):IND(2,2)) 
              END IF
         END IF
 410  CONTINUE 
!
      IF ( ILEN(SESS_CODE) == 0 ) THEN
           CALL ERR_LOG ( 3712, IUER, 'PIMA_PARSE', 'Did not find keyword '// &
     &         'SESS_CODE in the control file '//PIMA_CNT )
           RETURN 
      END IF
      IF ( ILEN(EXPER_DIR) == 0 ) THEN
           CALL ERR_LOG ( 3713, IUER, 'PIMA_PARSE', 'Did not find keyword '// &
     &         'EXPER_DIR in the control file '//PIMA_CNT )
           RETURN 
      END IF
!
      FPL_DIR = EXPER_DIR(1:I_LEN(EXPER_DIR))//'/'// &
     &          SESS_CODE(1:I_LEN(SESS_CODE))//'_fpl'
      DIR_DESC = OPENDIR ( FPL_DIR(1:I_LEN(FPL_DIR))//CHAR(0) )
      IF ( DIR_DESC .EQ. 0 ) THEN
           CALL GERROR ( STR )
!           CALL ERR_LOG ( 3713, IUER, 'PIMA_PARSE', 'Directory with fringe '// &
!     &         'plots '//FPL_DIR(1:I_LEN(FPL_DIR))//' cannot be '// &
!     &         'openned for reading: '//STR(1:I_LEN(STR))//' Please '// &
!     &         'check pima control file '//PIMA_CNT )
!           RETURN 
           WRITE ( 6, * ) 'WARNING: Directory with fringe '// &
     &         'plots '//FPL_DIR(1:I_LEN(FPL_DIR))//' cannot be '// &
     &         'openned for reading: '
           WRITE ( 6, * ) STR(1:I_LEN(STR))//' Please '// &
     &                    'check pima control file '//PIMA_CNT
      END IF
      IP = CLOSEDIR ( %VAL(DIR_DESC) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PARSE  !#!  
