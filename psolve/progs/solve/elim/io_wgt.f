      SUBROUTINE IO_WGT ( IOP, IDB2, DBOBJ, WEIGR_BAS, WEIPH_BAS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  IO_WGT  reads or writes baseline-dependent corrections    *
! *   to the weights NAMFIL  <--->  WEIxx_BAS                            *
! *                                                                      *
! *   NB: WEIxx_BAS contain weights of the observations. They have       *
! *   dimensions 1/sec                                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       IOP ( INTEGER*4 ) -- Switch of the mode of work:               *
! *                            IOP=1 -- read NAMFIL and put weights to   *
! *                                     WEIxx_BAS.                       *
! *                            IOP=2 -- write weights to NAMFIL from     *
! *                                     WEIxx_BAS.                       *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * WEIGR_BAS ( REAL*8    ) -- Array of baseline-dependent corrections   *
! *                            to weights when group delay observables   *
! *                            are in use. Array is ordered in according *
! *                            with list of baselines kept in the object *
! *                            DBOBJ.                                    *
! * WEIPH_BAS ( REAL*8    ) -- Array of baseline-dependent corrections   *
! *                            to weights when phase delay observables   *
! *                            are in use. Array is ordered in according *
! *                            with list of baselines kept in the object *
! *                            DBOBJ.                                    *
! *                                                                      *
! *  ###  28-JAN-1998   IO_WGT     v1.8  (c) L. Petrov  29-JUL-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'prfil.i'
      INCLUDE   'socom.i'
      INCLUDE   'obser.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*2  IDB2
      INTEGER*4  IOP, IUER
      CHARACTER  JBUF(MO_BAS)*160, STR*20, STR1*20, BAS*17, BASOPP*17
      REAL*8     WEIGR_BAS(MO_BAS), WEIPH_BAS(MO_BAS)
      REAL*8     WEI_FLOOR_LIM, SIG_MAX
      PARAMETER  ( WEI_FLOOR_LIM = 1.0D-13 ) ! 0.1 psec
      PARAMETER  ( SIG_MAX = 1.D5 ) ! ps
      REAL*8     WEI_ARR(4)
      INTEGER*2  IER2, IFL2
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, IC, K_BAS, IOS
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Openning NAMFILE
!
      CALL OPENNAMFIL()
!
! --- Reading INIT-card
!
      CALL GETCARD ( IDB2, 'INIT', INT2(1), JBUF, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(IER2), STR )
           CALL ERR_LOG ( 6481, IUER, 'IO_WGT', 'Error reading NAMFIL INIT '// &
     &         'card  IER='//STR )
           RETURN
      END IF
      IF ( SUPMET == SUPMET__META ) THEN
           CALL USE_GLBFIL_3 ( 'ORC' )
      END IF
!
      DO 410 J1=1,(INT4(NUMSTA)*(INT4(NUMSTA)-1))/2
         IF ( J1 .EQ. 1 ) THEN
              IFL2 = INT2(1)
            ELSE
              IFL2 = INT2(0)
         END IF
!
! ------ Reading REWT card to the arrays of cards
!
         CALL GETCARD ( IDB2, 'REWT', IFL2, JBUF(J1), IER2 )
         IF ( IER2 .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( INT4(IER2), STR1 )
              CALL ERR_LOG ( 6482, IUER, 'IO_WGT', 'Error of reading '// &
     &             STR(1:I_LEN(STR))//'-th NAMFIL REWT card  IER='//STR1 )
              RETURN
         END IF
 410  CONTINUE
!
      IF ( IOP .EQ. 1 ) THEN
!
! -------- Reading information from the card, parsing and writing to WEIxx_BAS
!
           DO 420 J2=1,(INT4(NUMSTA)*(INT4(NUMSTA)-1))/2
!
! ----------- Parsing the J2-th card
!
              IF ( JBUF(J2)(34:42) == '*********' ) JBUF(J2)(34:42) = '     0.00'
              IF ( JBUF(J2)(44:52) == '*********' ) JBUF(J2)(44:52) = '     0.00'
              IF ( JBUF(J2)(54:62) == '*********' ) JBUF(J2)(54:62) = '     0.00'
              IF ( JBUF(J2)(49:52) == ' NaN' ) JBUF(J2)(49:52) = '0.00'
              IF ( JBUF(J2)(59:62) == ' NaN' ) JBUF(J2)(59:62) = '0.00'
              READ ( JBUF(J2), '(5X, A8, 1X, A8, 4F10.2, 8X )', IOSTAT=IOS ) &
     &               BAS(1:8), BAS(10:17), WEI_ARR
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( IOS, STR1 )
                   CALL ERR_LOG ( 6483, IUER, 'IO_WGT', 'Error of pasrsing '// &
     &                  STR(1:I_LEN(STR))//'-th NAMFIL REWT card  IER='// &
     &                  STR1(1:I_LEN(STR))//'  "'//JBUF(J2)//'"' )
                   RETURN
              END IF
!
! ----------- Correction of bad weights: if weights are too small we set the
! ----------- minimal possible weight
!
              IF ( WEI_ARR(1) .LT. WEI_FLOOR_LIM*1.D12 ) WEI_ARR(1) = WEI_FLOOR_LIM*1.D12 
              IF ( WEI_ARR(3) .LT. WEI_FLOOR_LIM*1.D12 ) WEI_ARR(3) = WEI_FLOOR_LIM*1.D12 
!
              BAS(9:9)='/'
!
! ----------- The same baseline but with opposite order of the stations
!
              BASOPP = BAS(10:17)//'/'//BAS(1:8)
!
! ----------- Seaching the baseline BAS from the REWT card in the list of
! ----------- baseline stored in DBOBJ
!
              DO 430 J3=1,DBOBJ%L_BAS
                 IF ( BAS    .EQ. DBOBJ%C_BAS(J3) .OR. &
     &                BASOPP .EQ. DBOBJ%C_BAS(J3)      ) THEN
!
! ------------------- Transformation sigma in psec to weights in 1/sec
!
                      WEIGR_BAS(J3) = 1.D0/(WEI_ARR(1)*1.D-12)
                      WEIPH_BAS(J3) = 1.D0/(WEI_ARR(3)*1.D-12)
                      GOTO 420
                 END IF
 430          CONTINUE
!              CALL ERR_LOG ( 6484, IUER, 'IO_WGT', 'Baseline '//BAS//
!     #                      ' was found in NAMFIL, but not in DBOBJ' )
!              RETURN
 420       CONTINUE
      END IF
!
      IF ( IOP .EQ. 2 ) THEN
!
! -------- Writing information to the cards
!
           DO 440 J4=1,(INT4(NUMSTA)*(INT4(NUMSTA)-1))/2
              IF ( J4 .EQ. 1 ) THEN
                  IFL2 = INT2(1)
                ELSE
                  IFL2 = INT2(0)
              END IF
!
! ----------- Reding information in the J4-th card
!
              IF ( JBUF(J4)(34:42) == '*********' ) JBUF(J4)(34:42) = '     0.00'
              IF ( JBUF(J4)(44:52) == '*********' ) JBUF(J4)(44:52) = '     0.00'
              IF ( JBUF(J4)(54:62) == '*********' ) JBUF(J4)(54:62) = '     0.00'
              READ ( JBUF(J4), '( 5X, A8, 1X, A8, 4F10.2, 8X )', IOSTAT=IOS ) &
     &               BAS(1:8), BAS(10:17), WEI_ARR
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( IOS, STR1 )
                   CALL ERR_LOG ( 6485, IUER, 'IO_WGT', 'Error of pasrsing '// &
     &                  STR(1:I_LEN(STR))//'-th NAMFIL REWT card  IER='//STR1 )
                   RETURN
              END IF
              BAS(9:9)='/'
              BASOPP = BAS(10:17)//'/'//BAS(1:8)
!
! ----------- Seaching the baseline BAS from the REWT card in the list of
! ----------- baseline stored in DBOBJ
!
              DO 450 J5=1,DBOBJ%L_BAS
                 IF ( BAS    .EQ. DBOBJ%C_BAS(J5) .OR. &
     &                BASOPP .EQ. DBOBJ%C_BAS(J5)      ) THEN
                      IC = J5
                      GOTO 810
                 END IF
 450          CONTINUE
!
! ----------- It is possible situation when there is information the baseline
! ----------- in NAMFIL but there is no observations.
!
! ----------- Writing J4-card back to NAMFIL. We didn't change anything
! ----------- for this card. We rewrote it back in order to keep process
! ----------- read/writing smooth. Otherwise the order of cards may be changed
!
              CALL PUTCARD ( IDB2, 'REWT', IFL2, JBUF(J4), IER2 )
              IF ( IER2 .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J5, STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( INT4(IER2), STR1 )
                   CALL ERR_LOG ( 6487, IUER, 'IO_WGT', 'Error of writing '// &
     &                  STR(1:I_LEN(STR))//'-th NAMFIL REWT card  IER='//STR1 )
                   RETURN
              END IF
              GOTO 440 ! Nothing to do
!
 810          CONTINUE
!
! ----------- Transformation weights in 1/sec to sigma in psec
!
              WEI_ARR(1) = 1.D12/WEIGR_BAS(IC)
              WEI_ARR(3) = 1.D12/WEIPH_BAS(IC)
              IF ( WEI_ARR(1) > SIG_MAX ) WEI_ARR(1) = SIG_MAX
              IF ( WEI_ARR(2) > SIG_MAX ) WEI_ARR(2) = SIG_MAX
              IF ( WEI_ARR(3) > SIG_MAX ) WEI_ARR(3) = SIG_MAX
              IF ( WEI_ARR(4) > SIG_MAX ) WEI_ARR(4) = SIG_MAX
!
! ----------- Writing updated information back in the J4-th card
!
              WRITE ( JBUF(J4), '(A4, 1X, A8, 1X, A8, 4F10.2,8X)', IOSTAT=IOS ) &
     &               'REWT', BAS(1:8), BAS(10:17), WEI_ARR
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( IOS, STR1 )
                   CALL ERR_LOG ( 6488, IUER, 'IO_WGT', 'Error of writing '// &
     &                  ' to the '//STR(1:I_LEN(STR))//'-th NAMFIL REWT '// &
     &                  'card  IER='//STR1 )
                   RETURN
              END IF
!
! ----------- Writing J4-card back to NAMFIL
!
              CALL PUTCARD ( IDB2, 'REWT', IFL2, JBUF(J4), IER2 )
              IF ( IER2 .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J5, STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( INT4(IER2), STR1 )
                   CALL ERR_LOG ( 6489, IUER, 'IO_WGT', 'Error of writing '// &
     &                  STR(1:I_LEN(STR))//'-th NAMFIL REWT card  IER='//STR1 )
                   RETURN
              END IF
!
              IF ( SUPMET == SUPMET__META ) THEN
!
! ---------------- Store the weight in glbc3
!
                   CALL VTD_NAME_REPAIR ( BAS(1:8) )
                   CALL VTD_NAME_REPAIR ( BAS(10:17) )
!
! ---------------- Check, whether META_RW_BAS was defined
!
                   K_BAS = 0
                   DO 460 J6=1,META_N_BAS
                      IF ( ILEN(META_RW_BAS(1,J6)) > 0 .OR. &
     &                     ILEN(META_RW_BAS(2,J6)) > 0      ) THEN
                           K_BAS = K_BAS + 1
                      END IF
 460               CONTINUE 
!
                   IF ( K_BAS      == 0                     .AND. &
     &                  IOP        == 2                     .AND. &
     &                  META_N_BAS == (NUMSTA*(NUMSTA-1))/2       ) THEN
!
                        DO 470 J7=1,NUMSTA-1
                           DO 480 J8=J7+1,NUMSTA
                              K_BAS = K_BAS + 1
                              META_RW_BAS(1,K_BAS) = ISITN_CHR(J7)
                              META_RW_BAS(2,K_BAS) = ISITN_CHR(J8)
 480                       CONTINUE 
 470                    CONTINUE 
                   END IF
                   DO 490 J9=1,META_N_BAS
                      IF ( BAS(1:8)   == META_RW_BAS(1,J9) .AND. &
     &                     BAS(10:17) == META_RW_BAS(2,J9)       ) THEN
                           META_RW_DEL(IDATYP,J9) = WEI_ARR(1) 
                      END IF
                      IF ( BAS(1:8)   == META_RW_BAS(2,J9) .AND. &
     &                     BAS(10:17) == META_RW_BAS(1,J9)       ) THEN
                           META_RW_DEL(IDATYP,J9) = WEI_ARR(1) 
                      END IF
 490               CONTINUE 
              END IF
 440       CONTINUE
      END IF
      CALL CLOSENAMFIL()
      IF ( SUPMET == SUPMET__META ) THEN
           CALL USE_GLBFIL_3 ( 'OWC' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  IO_WGT  !#!
