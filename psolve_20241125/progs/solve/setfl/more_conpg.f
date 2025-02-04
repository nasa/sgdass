      SUBROUTINE MORE_CONPG ( )
! ************************************************************************
! *                                                                      *
! *   Routine  MORE_CONFG  allows user to change current values of some  *
! *   additional constraints. NB: change value of constraint doesn't     *
! *   mean that constraint will be neccessarily applied or not applied.  *
! *                                                                      *
! *  ###  20-SEP-98    MORE_CONPG   v1.1 (c)  L. Petrov  24-SEP-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'glbc4.i'
!
      INTEGER*4  IX, IY, ICH, NN, I, J1, ISIG, M_CNS
      PARAMETER  ( M_CNS=7 )
      CHARACTER  BUFSTR*79, STR*54, CCH*4, STR_SIG(M_CNS)*53, GET_VERSION*54
      EQUIVALENCE (ICH,CCH)
      INTEGER*4  BAS_CLK__POS, LIN_STA__POS, STA_WEA__POS, NNT_POS__POS, &
     &           NNR_POS__POS, SRC_COO__POS, NNR_SRC__POS, LAST__POS
      INTEGER*4  IY_LAST, IO
      DATA       BAS_CLK__POS, LIN_STA__POS, STA_WEA__POS, NNT_POS__POS, &
     &           NNR_POS__POS, SRC_COO__POS, NNR_SRC__POS, &
     &           LAST__POS/ 1, 2, 3, 4, 5, 6, 7, M_CNS /
      DATA       ( STR_SIG(NN), NN=1,M_CNS) &
     &           / &
     &           ' 1. BAS_CLK  Baseline-dependent clocks               ', &
     &           ' 2. LIN_STA  Linear combination of station position  ', &
     &           ' 3. STA_WEA  Station positions                       ', &
     &           ' 4. NNT_POS  No-net translation for station position ', &
     &           ' 5. NNR_POS  No-net rotation station position        ', &
     &           ' 6. SRC_COO  Source coordinates                      ', &
     &           ' 7. NNR_SRC  No-net rotation for sources             ' &
     &           /
      REAL*8     SIGMA(M_CNS), VAL
      INTEGER*2  INT2_ARG
      INTEGER*4  ILEN, I_LEN
!
! --- Take current values of sigmas of constraints and put them in temporaray
! --- array SIGMA
!
      SIGMA(BAS_CLK__POS) = BAS_CLK_SIGMA
      SIGMA(LIN_STA__POS) = LIN_STA_SIGMA
      SIGMA(STA_WEA__POS) = STA_WEA_SIGMA
      SIGMA(NNT_POS__POS) = NNT_POS_SIGMA
      SIGMA(NNR_POS__POS) = NNR_POS_SIGMA
      SIGMA(SRC_COO__POS) = SRC_COO_SIGMA
      SIGMA(NNR_SRC__POS) = NNR_SRC_SIGMA
!
 910  CONTINUE
      CALL SETCR_MN ( 0, 0 )
      CALL CLEAR_MN()
!
! --- Write out the menu header
!
      CALL ADDSTR_F ( "More constraints" )
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      CALL NL_MN()
      CALL NL_MN()
!
! --- Write at the screen topicks of menu
!
      DO 410 J1=1,M_CNS
         CALL SETCR_MN ( 0,   J1+1                    )
         CALL ADDSTR_F ( STR_SIG(J1)                  )
         CALL CLRCH    ( BUFSTR                       )
         WRITE         ( UNIT=BUFSTR, FMT='(1PE12.5)' ) SIGMA(J1)
         CALL ADDSTR_F ( BUFSTR(1:I_LEN(BUFSTR))      )
 410  CONTINUE
!
! --- Write at the screen two last lines
!
      IX = 0
      IY_LAST = LAST__POS+1 + 3
      CALL SETCR_MN ( IX, IY_LAST-1 )
      CALL ADDSTR_F ( '같같같같같같같같같같같같같같같같같같같같'// &
     &                '같같같같같같같같같같같같같같같같같같같�' )
      CALL SETCR_MN ( IX, IY_LAST )
      CALL ADDSTR_F ( "(B)ack page   (D)efault sigmas   "// &
     &                "(A)bort   # Change sigma" )
!
! --- Awaiting user input.
!
      IX = 1
      CALL SETCR_MN ( IX, IY_LAST )
      CALL SENKR_MN ( IX, IY, ICH )
      IF ( CCH(4:4) .EQ. CHAR(13) ) CCH(4:4) = ' '
      ISIG=IY-1
!
! --- First try to interprete a symbol as the index of constraint to be changed
!
      CALL CHIN ( CCH(4:4), I )
      IF ( I .GE. 1  .AND.  I .LE. M_CNS ) THEN
!
! -------- A number was entered
!
           ISIG = I
         ELSE IF ( CCH(4:4) .EQ. ' '  .AND.  IY .EQ. IY_LAST  .AND. &
     &             IX .LE. 13                                       ) THEN
!
! -------- Letter B
!
           CCH(4:4)='B'
         ELSE IF ( CCH(4:4) .EQ. ' '  .AND.  IY .EQ. IY_LAST  .AND. &
     &             IX .GE. 14  .AND.  IX .LE. 32 ) THEN
!
! -------- Letter D
!
           CCH(4:4)='D'
         ELSE IF ( CCH(4:4) .EQ. ' '  .AND.  IY .EQ. IY_LAST  .AND. &
     &             IX .GE. 33  .AND.  IX .LE. 42 ) THEN
!
! -------- Letter A
!
           CCH(4:4)='A'
         ELSE IF ( IY .NE. IY_LAST  .AND.  CCH(4:4) .EQ. ' ' ) THEN
!
! -------- Blank at some line
!
           ISIG = IY-1
!
! -------- If it is not the line wIth constraint -- nothing to do
!
           IF ( ISIG .LT. 1  .OR.  ISIG .GT. M_CNS ) GOTO 910
      END IF
!
! --- Making one of the actions
!
      IF ( CCH(4:4) .EQ. 'A' ) THEN
           RETURN
        ELSE IF ( CCH(4:4) .EQ. 'B' ) THEN
!
! -------- Saving values of constrains.
!
           BAS_CLK_SIGMA = SIGMA(BAS_CLK__POS)
           LIN_STA_SIGMA = SIGMA(LIN_STA__POS)
           STA_WEA_SIGMA = SIGMA(STA_WEA__POS)
           NNT_POS_SIGMA = SIGMA(NNT_POS__POS)
           NNR_POS_SIGMA = SIGMA(NNR_POS__POS)
           SRC_COO_SIGMA = SIGMA(SRC_COO__POS)
           NNR_SRC_SIGMA = SIGMA(NNR_SRC__POS)
!
! -------- Writing updated sigmas back to GLBFIL file
!
           CALL USE_GLBFIL_4 ( 'OWC' )
           RETURN
        ELSE IF ( CCH(4:4) .EQ. 'D' ) THEN
!
! -------- Setting defaults values of constraints
!
           SIGMA(BAS_CLK__POS) = BAS_CLK__SIG__DEF
           SIGMA(LIN_STA__POS) = LIN_STA__SIG__DEF
           SIGMA(STA_WEA__POS) = STA_WEA__SIG__DEF
           SIGMA(NNT_POS__POS) = NNT_POS__SIG__DEF
           SIGMA(NNR_POS__POS) = NNR_POS__SIG__DEF
           SIGMA(SRC_COO__POS) = SRC_COO__SIG__DEF
           SIGMA(NNR_SRC__POS) = NNR_SRC__SIG__DEF
!
           GOTO 910
        ELSE IF ( CCH(4:4) .EQ. 'R' ) THEN
           GOTO 910
      END IF
!
      IF ( ISIG .GE. 1 .AND. ISIG .LE. M_CNS ) THEN
 920       CONTINUE
!
! -------- Change the value of constraint
!
           CALL SETCR_MN ( 13, ISIG+1 )
           CALL CLRCH    ( BUFSTR )
           CALL ADDSTR_F ( BUFSTR(1:78-13) )
           BUFSTR      = 'Old value: '
           WRITE         ( UNIT=BUFSTR(12:), FMT='(1PE12.5)' ) SIGMA(ISIG)
           BUFSTR(26:) = 'NEW VALUE >> '
           CALL SETCR_MN ( 13, ISIG+1                )
           CALL ADDSTR_F ( BUFSTR(1:I_LEN(BUFSTR)+1) )
!
! -------- Awaiting user response
!
           CALL CLRCH    ( STR )
           CALL GETSTR_F ( STR )
!
! -------- Parse user response
!
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
           IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR(ILEN(STR)+1:)='.'
           READ ( UNIT=STR, FMT='(F8.4)', IOSTAT=IO ) VAL
           IF ( IO .NE. 0 ) THEN
!
! ------------- Erroneous input
!
                CALL SETCR_MN ( 13, ISIG+1      )
                CALL CLRCH    ( BUFSTR          )
                CALL ADDSTR_F ( BUFSTR(1:78-13) )
                CALL SETCR_MN ( 13, ISIG+1      )
                CALL ADDSTR_F ( '$$$  WRONG FORMAT OF '//STR(1:I_LEN(STR))// &
     &                          ' $$$' )
                CALL SENKR_MN ( IX, IY, ICH )
                GOTO 920
           END IF
!
           IF ( VAL .LT. 1.D-30 ) THEN
!
! ------------- Value appeared too small
!
                CALL SETCR_MN ( 13, ISIG+1      )
                CALL CLRCH    ( BUFSTR          )
                CALL ADDSTR_F ( BUFSTR(1:78-13) )
                CALL SETCR_MN ( 13, ISIG+1      )
                CALL ADDSTR_F ( '$$$  VALUE '//STR(1:I_LEN(STR))// &
     &                          ' IS LESS THAN 1.D-30 $$$' )
                CALL SENKR_MN ( IX, IY, ICH )
                GOTO 920
           END IF
!
           SIGMA(ISIG) = VAL
      END IF
      GOTO 910
!
      END  !#!  MORE_CONPG  #!#
