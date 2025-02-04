      SUBROUTINE DATE_TO_TIME ( DATE_CHR, MJD, SEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DATE_TO_TIME transforms a string with date and time       *
! *   from internal SOLVE character format to MJD and seconds.           *
! *   SOLVE internal format is                                           *
! *   yyyy.mm.dd-hh:mm:ss.ssssss  like                                   *
! *   1999.09.02-20:54:45.000000                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * DATE_CHR ( CHARACTER ) -- Date in internal SOLVE format. It should   *
! *                           be at  least 4 characters long.            *
! *                           missed fields are substituted from the     *
! *                           default string 2000.01.01_00:00:00.000000  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      MJD ( INTEGER*4 ) -- Modified Julian date: integer number of    *
! *                           days elapsed from 0 UT January 01, 2000    *
! *                           plus magical number 51544                  *
! *      SEC ( REAL*8    ) -- Time in seconds elapsed from midnight.     *
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
! *  ###  03-SEP-1999  DATE_TO_TIME  v1.4 (c) L. Petrov 18-SEP-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  DATE_CHR*(*)
      INTEGER*4  MJD, IUER
      REAL*8     SEC
      INTEGER*4  MJD0, YEAR4, IYY, ILD
      DATA       MJD0  / -678957 /,  & ! MJD at 01-JAN of the 0-th year (near the
!                                   ! mythical date of Jesus Christ birth)
     &           YEAR4 /    1461 /  ! number of days in 4-year cycle
      INTEGER*4  MON_TAB(12,4)
      DATA MON_TAB / &
     &     0,  31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335,  & !  1 - 12
     &   366, 397, 425, 456, 486, 517, 547, 578, 609, 639, 670, 700,  & ! 13 - 24
     &   731, 762, 790, 821, 851, 882, 912, 943, 974,1004,1035,1065,  & ! 25 - 36
     &  1096,1127,1155,1186,1216,1247,1277,1308,1339,1369,1400,1430   & ! 37 - 48
     &             /
!
      CHARACTER  DATE_USE*32, DATE_DEFAULT*32
      DATA       DATE_DEFAULT / '2000.01.01_00:00:00.000000000000' /
!
      CHARACTER  SEC_CHR*80
      INTEGER*4  IYEA, IMON, IDAY, IHOU, IMIN, IO, IP, IZ, DAY_EXTRA
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      ILD = ILEN(DATE_CHR)
      DAY_EXTRA = 0
!
! --- Isn't the date string too short?
!
      IF ( ILD .LT. 4 ) THEN
           CALL ERR_LOG ( 801, IUER, 'DATE_TO_TIME', 'Date string '// &
     &          DATE_CHR(1:I_LEN(DATE_CHR))//' is too short' )
           RETURN
      END IF
      IF ( ILEN(DATE_CHR) .LT. LEN(DATE_DEFAULT) ) THEN
           DATE_USE = DATE_CHR(1:ILD)//DATE_DEFAULT(ILD+1:)
         ELSE
           DATE_USE = DATE_CHR(1:LEN(DATE_DEFAULT))
      END IF
!
! --- We consider trailing Z acceptable in accordance to ISO 8601
!
      IP = LINDEX ( DATE_USE, 'Z' ) 
      IF ( IP > 0 ) CALL CLRCH ( DATE_USE(IP:) )
      IP = LINDEX ( DATE_USE, 'z' ) 
      IF ( IP > 0 ) CALL CLRCH ( DATE_USE(IP:) )
!
! --- Read year field
!
      CALL CHIN ( DATE_USE(1:4), IYEA )
      IF ( IYEA .GE. 70  .AND. IYEA .LE.   99 ) IYEA = IYEA + 1900
      IF ( IYEA .GE.  0  .AND. IYEA .LE.   69 ) IYEA = IYEA + 2000
      IF ( IYEA .LT.  0  .OR.  IYEA .GT. 2399 ) THEN
           CALL ERR_LOG ( 802, IUER, 'DATE_TO_TIME', 'Error in parsing '// &
     &         'YEAR field in the date '//DATE_USE(1:I_LEN(DATE_USE)) )
           RETURN
      END IF
!
! --- Read month field
!
      CALL CHIN ( DATE_USE(6:7), IMON )
      IF ( IMON .LE. 0  .OR. IMON .GE. 13 ) THEN
           CALL ERR_LOG ( 803, IUER, 'DATE_TO_TIME', 'Error in parsing '// &
     &         'MONTH field in the date '//DATE_USE(1:I_LEN(DATE_USE)) )
           RETURN
      END IF
!
! --- Read day field
!
      CALL CHIN ( DATE_USE(9:10), IDAY )
      IF ( IDAY .LE. 0  .OR. IDAY .GT. 31 ) THEN
           CALL ERR_LOG ( 804, IUER, 'DATE_TO_TIME', 'Error in parsing '// &
     &         'DAY field in the date '//DATE_USE(1:I_LEN(DATE_USE)) )
           RETURN
      END IF
!
! --- Read hour field
!
      CALL CHIN ( DATE_USE(12:13), IHOU )
      IF ( IHOU == 24 ) THEN
           IHOU = IHOU - 24
           DAY_EXTRA = 1
      END IF
      IF ( IHOU .LT. 0  .OR. IHOU .GE. 24 ) THEN
           CALL ERR_LOG ( 805, IUER, 'DATE_TO_TIME', 'Error in parsing '// &
     &         'HOUR field in the date '//DATE_USE(1:I_LEN(DATE_USE)) )
           RETURN
      END IF
!
! --- Read minute field
!
      CALL CHIN ( DATE_USE(15:16), IMIN )
      IF ( IMIN .LT. 0  .OR. IMIN .GE. 60 ) THEN
           CALL ERR_LOG ( 806, IUER, 'DATE_TO_TIME', 'Error in parsing '// &
     &         'MINUTE field in the date '//DATE_USE(1:I_LEN(DATE_USE)) )
           RETURN
      END IF
!
! --- Read second field
!
      CALL CLRCH ( SEC_CHR )
      SEC_CHR = DATE_USE(18:I_LEN(DATE_USE))
      IF ( INDEX ( SEC_CHR, '.' ) .EQ. 0 ) THEN
           SEC_CHR = SEC_CHR(1:I_LEN(SEC_CHR))//'.0'
      END IF
!
      READ ( SEC_CHR(1:I_LEN(SEC_CHR)), FMT='(F32.20)', IOSTAT=IO ) SEC
      IF ( IO .NE. 0 ) THEN
           CALL ERR_LOG ( 807, IUER, 'DATE_TO_TIME', 'Error in parsing '// &
     &         'SECOND field in the date '//DATE_USE(1:I_LEN(DATE_USE)) )
           RETURN
      END IF
      IF ( SEC .LT. -1.D-10  .OR. SEC .GE. 60.D0 + 1.D-10 ) THEN
           write ( 6, * ) ' sec = ', sec ! %%
           CALL ERR_LOG ( 808, IUER, 'DATE_TO_TIME', 'Error in parsing '// &
     &         'SECONDS field in the date '//DATE_USE(1:I_LEN(DATE_USE)) )
           RETURN
      END IF
!
! --- ... and at last some computations
!
      SEC = SEC + IMIN*60.0 + IHOU*3600.0
      IYY = IYEA -     4*(IYEA/4) + 1
      MJD = MJD0 + YEAR4*(IYEA/4) + MON_TAB(IMON,IYY) + IDAY + DAY_EXTRA
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DATE_TO_TIME  #!#
