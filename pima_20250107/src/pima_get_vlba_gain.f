      SUBROUTINE PIMA_GET_VLBA_GAIN ( PIM, GAIN_FILE, GAIN_BAND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GET_VLBA_GAIN imports gain table for VLBA stations    *
! *   form the so-called gain key-file.                                  *
! *                                                                      *
! * ## 10-APR-2011 PIMA_GET_VLBA_GAIN v2.1 (c) L. Petrov  28-AUG-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( PIM_GAIN__TYPE ) :: GAIN_SAVE
      CHARACTER  GAIN_FILE*(*), GAIN_BAND*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF  = 16384 )
      PARAMETER  ( MIND  =   128 )
      TYPE ( GAIN_KEY__STRU ) :: KEY(2*PIM__MSTA)
      CHARACTER    BUF(MBUF)*256, STR*128, STA_NAM*8, &
     &             DATE_BEG*19, DATE_END*19, BAND(2)*8, REG*8, GAIN_TYPE*4
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//',=/' )
!
      LOGICAL*1  FL_BAND(2)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, NPOLY, L_KEY, NUM_BAND, &
     &           IP, NBUF, NBAND, LIND, IND(2,MIND), MJD_BEG, MJD_END, &
     &           IND_KEY(2), NSG, NPOLY_MAX, IER
      REAL*8     FREQ, DPFU_R, DPFU_L, POLY(0:PIM__MGPL), TIM_BEG, TIM_END
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( GAIN_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6811, IUER, 'PIMA_GET_VLBA_GAIN', 'Failure '// &
     &         'in attempt to read VLBA gain file '//GAIN_FILE )
           RETURN
      END IF
!
      IF ( BUF(1)(1:32) == '!                VLBA GAINS FILE' ) THEN
           GAIN_TYPE = 'VLBA'
           NSG = 3
         ELSE IF ( BUF(1)(1:31) == '!                IVS GAINS FILE' ) THEN
           GAIN_TYPE = 'IVS'
           NSG = 2
         ELSE
           CALL CLRCH ( STR )
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 6812, IUER, 'PIMA_GET_VLBA_GAIN', 'Unrecoginzed '// &
     &         'format of VLBA gain file '//GAIN_FILE(1:I_LEN(GAIN_FILE))// &
     &         ' -- the first line is '//STR(1:I_LEN(STR))//' while '// &
     &         '!                VLBA GAINS FILE was expected' )
           RETURN
      END IF
!
      IP = INDEX ( GAIN_BAND, '/' )
      IF ( IP > 1 ) THEN
           BAND(1) = GAIN_BAND(1:IP-1)
           BAND(2) = GAIN_BAND(IP+1:)
           NBAND = 2
         ELSE
           BAND(1) = GAIN_BAND
           NBAND = 1
      END IF
!
      FL_BAND = .FALSE.
      L_KEY = 0
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1) == '!' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( LIND < 2 ) GOTO 410
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BAND' .AND. &
     &        ( BUF(J1)(IND(1,2):IND(2,2)) == "'"//BAND(1)(1:I_LEN(BAND(1)))//"'"  .OR. &
     &          BUF(J1)(IND(1,2):IND(2,2)) == "'"//BAND(2)(1:I_LEN(BAND(2)))//"'"       ) ) THEN
!
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == "'"//BAND(1)(1:I_LEN(BAND(1)))//"'"  ) THEN
                   FL_BAND(1) = .TRUE.
              END IF
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == "'"//BAND(2)(1:I_LEN(BAND(2)))//"'"  ) THEN
                   FL_BAND(2) = .TRUE.
              END IF
              IF ( LIND < 11 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6813, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Violation of gain-file format at line '//STR )
                   RETURN
              END IF
!
              DATE_BEG = BUF(J1)(IND(1,4):IND(2,4))//'.'// &
     &                   BUF(J1)(IND(1,5):IND(2,5))//'.'// &
     &                   BUF(J1)(IND(1,6):IND(2,6))//'_'// &
     &                   BUF(J1)(IND(1,7):IND(2,7))//':00.00.0'
!
              DATE_END = BUF(J1)(IND(1,8):IND(2,8))//'.'// &
     &                   BUF(J1)(IND(1,9):IND(2,9))//'.'// &
     &                   BUF(J1)(IND(1,10):IND(2,10))//'_'// &
     &                   BUF(J1)(IND(1,11):IND(2,11))//':00.00.0'
              IF ( DATE_END == '2100.01.01_00:00:00.0' ) THEN
                   DATE_END = '2049.12.31_23:59:59.9'
              END IF
!!              write ( 6, * ) ' date_beg= '//date_beg//'   '//date_end ! %%%
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TIM_BEG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6814, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Failure in parsing the time range date at '// &
     &                 'line '//STR(1:I_LEN(STR))//' -- '// &
     &                 BUF(J1)(IND(1,4):IND(2,7)) )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( DATE_END, MJD_END, TIM_END, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6814, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Failure in parsing the time range date at '// &
     &                 'line '//STR(1:I_LEN(STR))//' -- '// &
     &                 BUF(J1)(IND(1,8):IND(2,11)) )
                   RETURN
              END IF
!
              IF ( PIM%MJD_0 .GE. MJD_BEG  .AND.  &
     &             PIM%MJD_0 .LE. MJD_END         ) THEN
                   CONTINUE
                 ELSE
                   GOTO 410
              END IF
!
! ----------- Parse the freqency field
!
              CALL EXWORD ( BUF(J1+1), MIND, LIND, IND, REG, IER )
              IF ( LIND < 2 .OR. BUF(J1+1)(IND(1,1):IND(2,1)) .NE. 'FREQ' ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1+1, STR )
                   CALL ERR_LOG ( 6813, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Violation of gain-file format at line '//STR )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1+1)(IND(1,2):IND(2,2)), FMT='(F8.0)', IOSTAT=IER ) FREQ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1+1, STR )
                   CALL ERR_LOG ( 6814, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Failure in parsing frequency at line '// &
     &                 STR(1:I_LEN(STR))//' -- '// &
     &                 BUF(J1+1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
!
! ----------- Parse the GAIN field
!
              CALL EXWORD ( BUF(J1+NSG), MIND, LIND, IND, REG, IER )
              IF ( LIND < 8 .OR. &
     &             BUF(J1+NSG)(IND(1,2):IND(2,2)) .NE. 'GAIN'  .OR. &
     &             BUF(J1+NSG)(IND(1,3):IND(2,3)) .NE. 'ALTAZ' .OR. &
     &             BUF(J1+NSG)(IND(1,4):IND(2,4)) .NE. 'DPFU'      ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1+1, STR )
                   CALL ERR_LOG ( 6815, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Violation of gain-file format at line '//STR )
                   RETURN
              END IF
              CALL CLRCH ( STA_NAM )
              STA_NAM = BUF(J1+NSG)(IND(1,1):IND(2,1))
!
              READ ( UNIT=BUF(J1+NSG)(IND(1,5):IND(2,5)), FMT='(F8.4)', IOSTAT=IER ) DPFU_R
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1+NSG, STR )
                   CALL ERR_LOG ( 6814, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Failure in parsing DFPU at R-polarizaion at line '// &
     &                 STR(1:I_LEN(STR))//' -- '// &
     &                 BUF(J1+1)(IND(1,5):IND(2,5)) )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1+NSG)(IND(1,6):IND(2,6)), FMT='(F8.4)', IOSTAT=IER ) DPFU_L
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1+NSG, STR )
                   CALL ERR_LOG ( 6814, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Failure in parsing DFPU at R-polarizaion at line '// &
     &                 STR(1:I_LEN(STR))//' -- '// &
     &                 BUF(J1+1)(IND(1,6):IND(2,6)) )
                   RETURN
              END IF
              NPOLY = 0
              POLY = 0.0D0
              DO 420 J2=8,LIND
                 NPOLY = J2-8
                 READ ( BUF(J1+NSG)(IND(1,J2):IND(2,J2)), FMT='(F15.4)' ) &
     &                  POLY(NPOLY)
 420          CONTINUE
              L_KEY = L_KEY + 1
!!              write ( 6, * ) 'sta_nam: '//sta_nam, ' j1= ', j1 ! %%%
              KEY(L_KEY)%FREQ    = FREQ*1.D6
              KEY(L_KEY)%DPFU(1) = DPFU_R
              KEY(L_KEY)%DPFU(2) = DPFU_L
              KEY(L_KEY)%NPOL    = NPOLY
              KEY(L_KEY)%POLY    = POLY
              KEY(L_KEY)%STA_NAM = STA_NAM
         END IF
 410  CONTINUE
!
      IF ( .NOT. FL_BAND(1) ) THEN
           CALL ERR_LOG ( 6817, IUER, 'PIMA_GET_VLBA_GAIN', 'Did not '// &
     &         'find band '//BAND(1)(1:I_LEN(BAND(1)))//' band in '// &
     &         ' the VLBA gain file '//GAIN_FILE )
           RETURN
      END IF
!
      IF ( NBAND == 2 .AND. .NOT. FL_BAND(2) ) THEN
           CALL ERR_LOG ( 6818, IUER, 'PIMA_GET_VLBA_GAIN', 'Did not '// &
     &         'find band '//BAND(2)(1:I_LEN(BAND(2)))//' band in '// &
     &         ' the VLBA gain file '//GAIN_FILE )
           RETURN
      END IF
!
      WRITE ( 6, * ) ' L_KEY = ', L_KEY
!
      DO 430 J3=1,PIM%NSTA
         IND_KEY = 0
         NUM_BAND   = 0
!
! ------ Scan the key file and try to match the station name.
! ------ Count how many bands have been found (NUM_BAND)
!
         DO 440 J4=1,L_KEY
            IF ( ILEN(KEY(J4)%STA_NAM) == 2 ) THEN
!
! -------------- The station name is a two-letters characeter string
!
                 IF ( PIM%STA(J3)%ORIG_NAME(1:2) == KEY(J4)%STA_NAM(1:2) ) THEN
                      NUM_BAND = NUM_BAND + 1
                      IND_KEY(NUM_BAND) = J4
                      GOTO 440
                 END IF
               ELSE
!
! -------------- The station name is a eight-letters characeter string.
! -------------- First try IVS station name
!
!  write ( 6, * ) ' key=  >>'//KEY(J4)%STA_NAM//'<<  ' ! %%%%%%%%%%%%%%
!  write ( 6, * ) ' sta=  >>'//PIM%STA(J3)%IVS_NAME//'<<  ' ! %%%%%%%%%%%%%%
                 IF ( PIM%STA(J3)%IVS_NAME == KEY(J4)%STA_NAM ) THEN
                      NUM_BAND = NUM_BAND + 1
                      IND_KEY(NUM_BAND) = J4
                      GOTO 440
                 END IF
!
! -------------- If it does not help, try original station name from the
! -------------- FITS file
!
                 IF ( PIM%STA(J3)%ORIG_NAME == KEY(J4)%STA_NAM ) THEN
                      NUM_BAND = NUM_BAND + 1
                      IND_KEY(NUM_BAND) = J4
                      GOTO 440
                 END IF
            END IF
 440     CONTINUE
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              WRITE ( 6, * ) 'Sta: ', PIM%C_STA(J3), ' NUM_BAND= ', INT2(NUM_BAND), &
     &                       ' IND_KEY = ', INT2(IND_KEY(1)), INT2(IND_KEY(2)), &
     &                       ' ga= ', PIM%STA(J3)%GAIN%AVAIL
         END IF
         IF ( NUM_BAND .LE. 0 ) GOTO 430
         NPOLY_MAX = 0
         DO 450 J5=1,NUM_BAND
            NPOLY = KEY(IND_KEY(J5))%NPOL
            NPOLY_MAX = MAX ( NPOLY_MAX, NPOLY ) 
 450     CONTINUE 
!
         IF ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%AVAIL ) THEN
              IF ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NPOL < PIM%NPOL .OR. &
     &             PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NTAB < NPOLY_MAX ) THEN
                   CALL ERR_LOG ( 6819, IUER, 'PIMA_GET_VLBA_GAIN', &
     &                 'Trap of internal control: the number of '// &
     &                 'of polarizations or max gain degree in the old '// &
     &                 'gain table is not sufficient to accommodate the new '// &
     &                 'table ' )
                   RETURN
              END IF
         END IF
!
         IF ( .NOT. PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%AVAIL ) THEN
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%AVAIL = .TRUE.
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ  = PIM%NFRQ
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NPOL  = PIM%NPOL
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NTAB  = NPOLY_MAX
!
              ALLOCATE ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%TYP  (PIM%NFRQ,PIM%NPOL) )
              ALLOCATE ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(PIM%NFRQ,PIM%NPOL) )
              ALLOCATE ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP(PIM%NFRQ,PIM%NPOL) )
              ALLOCATE ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP(PIM%NFRQ,PIM%NPOL) )
              ALLOCATE ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL(PIM%NFRQ,PIM%NPOL) )
              ALLOCATE ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL(0:PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NTAB,PIM%NFRQ,PIM%NPOL) )
              ALLOCATE ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%GAIN (0:PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NTAB,PIM%NFRQ,PIM%NPOL) )
              ALLOCATE ( PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%SENS (PIM%NFRQ,PIM%NPOL) )
!
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%TYP   = 0
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NTERM = 0
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP = 0
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP = 0
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL = 0.0
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL = 0.0
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%GAIN  = 0.0
              PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%SENS  = 0.0
         END IF
!
         PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%TYP = 2
         DO 460 J6=1,NUM_BAND
            NPOLY = KEY(IND_KEY(J6))%NPOL
            DO 470 J7=1,PIM%NFRQ
               IF ( DABS( KEY(IND_KEY(J6))%FREQ - PIM%FRQ(J7,PIM%CONF%FRQ_GRP)%FREQ ) < &
     &              0.26*PIM%FRQ(J7,PIM%CONF%FRQ_GRP)%FREQ ) THEN
!
                    PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(J7,1) = NPOLY
                    PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J7,1) = KEY(IND_KEY(J6))%DPFU(1)
                    PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(0:NPOLY,J7,1) = KEY(IND_KEY(J6))%POLY(0:NPOLY)
                    IF ( PIM%NPOL > 1 ) THEN
                         PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(J7,2) = NPOLY
                         PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J7,2) = KEY(IND_KEY(J6))%DPFU(2)
                         PIM%STA(J3)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(0:NPOLY,J7,2) = KEY(IND_KEY(J6))%POLY(0:NPOLY)
                    END IF
               END IF
 470        CONTINUE
 460     CONTINUE
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_GET_VLBA_GAIN  !#!#

