#include <mk5_preprocessor_directives.inc>
        SUBROUTINE MATVIEW ( MATYP, MV, MH, A, ZAG, FORM_I, IV, IH, &
     &                       IUER )
! ************************************************************************
! *                                                                      *
! *     Program  MATVIEW  displays a matrix A in the screen in an        *
! *   interactive mode. A user has a capacity to change parameters of    *
! *   the text window where the matrix is displayed and the format of    *
! *   displaying matrix elements.                                        *
! *                                                                      *
! * ________________________ Input parameters __________________________ *
! *                                                                      *
! *  MATYP ( INTEGER*4 ) -- Matrix organization type.                    *
! *                         The following types  are supported:          *
! *          MATYP=1 -- Rectangular  matrix in column represenation.     *
! *                     Column representatin is a naitve mode for        *
! *                     Fortran.                                         *
! *          MATYP=2 -- Rectangular matrix in coumn representation.      *
! *                     But the transponed matrix will be diplayed in    *
! *                     the screen. Each column will be diplayed         *
! *                     as a row.                                        *
! *          MATYP=3 -- Symmetrical matrix in the upper-triangular       *
! *                     representation.                                  *
! *          MATYP=4--  Symmetrical matrix in the lower-triangular       *
! *                     representation.                                  *
! *     MV ( INTEGER*4 ) -- The first dimension. It should corresond     *
! *                         to the declared dimension in main program    *
! *                         for rectangular matrix.                      *
! *     MH ( INTEGER*4 ) -- The second dimension of the matric (It is    *
! *                         ignored if MATYP=3 or MATYP=4).              *
! *      A ( REAL*8    ) -- Displayed matrix. Dimension: if MATYP=1,2    *
! *                         then, MV*MH, if MATYP=3, then MV*(MV+1))/2   *
! *    ZAG ( CHARACTER ) -- Title. This title will b diplayer at the     *
! *                         top of the text screen.                      *
! *   FORM ( CHARACTER ) -- Format for diplaying a matrix element in     *
! *                         Fortran format specification format.         *
! *                         Acceptable specifications: D, E, F, G, P, X. *
! *                         A multiplier is allowed before X.            *
! *                         Specification(s) shold be seprated by comma. *
! *                         Embracing commas are optional. Examples:     *
! *                         1X,F7.3, (1PE11.4)                           *
! *     IV ( INTEGER*4 ) -- The row index which will be initially        *
! *                         diplayed as the top row of the screen.       *
! *     IH ( INTEGER*4 ) -- The column index which will be initially     *
! *                         diplayed as the leftmost column of the       *
! *                         screen.                                      *
! *                                                                      *
! * ______________________ Modified parameters: ________________________ *
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
! *     NB: Further documentation can be found in files                  *
! *   $HELP_DIR/matview.hlp, $HELP_DIR/matview_1.hlp                     *
! *                                                                      *
! *  ###  27-JUN-1994   MATVIEW   v 2.5 (c)  L. Petrov  10-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT     NONE
        INTEGER*4    MATYP, MV, MH, IV, IH, IB, IE, IUER
        INTEGER*4    I, J
        INTEGER*8    LOCC, LOCL
        REAL*8       A(*)
        CHARACTER    ZAG*(*), FORM_I*(*)
        INTEGER*4    MAX_V, MAX_H, MV_HBF
        PARAMETER  ( MAX_V=256, MAX_H=384, MV_HBF=24 )
        CHARACTER    BUF(MAX_V)*(MAX_H), HELP_FILE*64, &
     &               HELP_BUF(MV_HBF)*(MAX_H), SO*1, SI*1, &
     &               CRLF*2, CR*1, AS*1, LS2R*2, STR*80, OUT*132, &
     &               FORM*32
        CHARACTER    LU_CORNER*1, RU_CORNER*1, LD_CORNER*1, RD_CORNER*1, &
     &               HRU_LINE*1, HRD_LINE*1, VR_LINE*1, POINTER_LEFT*1, &
     &               POINTER_RIGHT*1, POINTER_UP*1, POINTER_DOWN*1
        INTEGER*4    NV, NH, LH, LV, ISHV, ISHH, KV, KH, IER, IDG, IBL, &
     &               INUM, ILR, J1, J2, J3, J4, J5, J6, IPL, NK, &
     &               IL, IOS, IS, ICOM, ICH_PAR, IDG_N, IBL_N, INDN, &
     &               INUM_N, IT, IG, IP, IST, IRUS, LH_0, ISM, LSTR, IPL_LAST, &
     &               LHS_HBF, LV_HBF, IL1, ILN
        ADDRESS__TYPE :: ILOC
        LOGICAL*4    FL_IN_TERM, RD_HBF
        INTEGER*4, EXTERNAL ::  INSIM, I_LEN, ILEN, LIB$SKPC
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
#else
      LOGICAL*4, EXTERNAL :: FUNC_ISATTY
#endif

! ----- Place function of the
! ----- rectangular (MATYP=1),
! ----- rectanguler transposed (MATYP=2) and
! ----- symmetrical matrix in upper representation ( MATYP=3 )
! ----- symmetrical matrix in lower representation ( MATYP=4 )
!
        LOCC(MATYP,I,J)=( INT8(MV)*INT8(J-1) + I  )*INT8(MATYP)*INT8(MATYP-2)*INT8(MATYP-3)/2 + &
     &                  ( INT8(MV)*INT8(I-1) + J  )*INT8(MATYP-1)*INT8(3-MATYP)         + &
     &                  ( I+(J*INT8(J-1))/2 )*INT8(MATYP-2)*INT8(MATYP-1)/2
        LOCL(MV,I,J) = I + (INT8(2*MV-J)*INT8(J-1))/2
#ifdef SUN
      FL_IN_TERM = FUNC_ISATTY ( 1 ) ! Flag whether the unit 5(1) is a terminal
#else
#ifdef GNU
      FL_IN_TERM = ISATTY ( 5 ) ! Flag whether the unit 5 is a terminal
#else
      FL_IN_TERM = FUNC_ISATTY ( 5 ) ! Flag whether the unit 5 is a terminal
#endif
#endif
!
! ----- Getting the name of the HELP-file
!
        CALL CLRCH ( STR )
        CALL GETENVAR ( 'HELP_DIR', STR )
        IF ( ILEN(STR) .NE. 0 ) THEN
             HELP_FILE=STR(1:I_LEN(STR))//'/matview.hlp'
           ELSE
             HELP_FILE='matview.hlp'
        END IF
!
        SO=CHAR(14)
        SI=CHAR(15)
        CRLF=CHAR(13)//CHAR(10)
        CR=CHAR(13)
        LS2R=CHAR(27)//'}'
        RD_HBF=.FALSE.  !  Flag initialization: buffer HELP was not read
        ISHV=3 !  Index of the buffer for the beginning of the matrix
        ISHH=6 !  shift of the imaging the matrix counted from the left edge
        KV=IV  !  Initial value for the upper matrix row
        KH=IH  !  Initial value of the lecft matrix column
!
! ----- Get information about your screen
!
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
!
        IF ( IT .GE. 1  .AND.  IT .LE. 5 ) THEN
             LU_CORNER     = 'l'
             RU_CORNER     = 'k'
             LD_CORNER     = 'm'
             RD_CORNER     = 'j'
             HRD_LINE      = 'q'
             HRU_LINE      = 'q'
             VR_LINE       = 'x'
             POINTER_LEFT  = 'u'
             POINTER_RIGHT = 't'
             POINTER_UP    = 'v'
             POINTER_DOWN  = 'w'
          ELSE IF ( IT .EQ. 6  ) THEN
             LU_CORNER     = '/'
#ifdef SUN
             RU_CORNER     = CHAR(92)
             LD_CORNER     = CHAR(92)
#else
             RU_CORNER     = '\'
             LD_CORNER     = '\'
#endif
             RD_CORNER     = '/'
             HRD_LINE      = '_'
             HRU_LINE      = '°'
             VR_LINE       = '|'
             POINTER_LEFT  = '|'
             POINTER_RIGHT = '|'
             POINTER_UP    = '^'
             POINTER_DOWN  = 'v'
#ifdef HPUX
             CALL PRCH ( CHAR(27)//'&k0\' ) ! goto to HP-mode for hpterm
#endif
          ELSE IF ( IT .EQ. 7  ) THEN
             LU_CORNER     = '/'
#ifdef SUN
             RU_CORNER     = CHAR(92)
             LD_CORNER     = CHAR(92)
#else
             RU_CORNER     = '\'
             LD_CORNER     = '\'
#endif
             RD_CORNER     = '/'
             HRD_LINE      = '_'
             HRU_LINE      = '~'
             VR_LINE       = '|'
             POINTER_LEFT  = '|'
             POINTER_RIGHT = '|'
             POINTER_UP    = '^'
             POINTER_DOWN  = 'v'
        END IF
!
        IF ( MATYP .EQ. 1 ) THEN
             NV=MV
             NH=MH
          ELSE IF ( MATYP.EQ.2 ) THEN
             NV=MH
             NH=MV
          ELSE IF ( MATYP.EQ.3 ) THEN
             NV=MV
             NH=MV
          ELSE IF ( MATYP.EQ.4 ) THEN
             NV=MV
             NH=MV
        END IF
        NK=(NV*(NV+1))/2
!
! ----- Checks
!
        IF ( MATYP .LT. 1  .OR.  MATYP .GT. 4 ) THEN
             CALL CLRCH ( STR )
             CALL INCH  ( MATYP, STR )
             CALL ERR_LOG ( 9121, IUER, 'MATVIEW', 'Wrong value of the '// &
     &           'first argument: '//STR(1:I_LEN(STR))//' It was expected '// &
     &           '1, 2, 3 or 4' )
             RETURN
        END IF
!
! ----- Rewrite format string
!
        CALL CLRCH ( FORM )
        IF ( LEN(FORM_I) .GT. 0 ) FORM=FORM_I
!
!
! ----- Loop of interactive command processing
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
  910   CONTINUE
!
! -------- Get info about actual size of the screen
!
           CALL TERM_SIZE ( LSTR, LH )
           IF ( LSTR .LE. 6 ) LSTR = 24
           IF ( LH   .LE. 6 ) LSTR = 80
           LV=LSTR-ISHV*2  !  the number of matrix rows displayed in the screen
           LH_0 = LH
!
! -------- Format processing:  check of its syntax and computation of
! -------- IDG  -- The number of characters occupied by digital fields of
! --------         a matrix element
! -------- IBL  -- The number of blanks occupied by a matrix element
! -------- INUM -- The total number of characters occupied by a matrix element
!
           CALL ERR_PASS ( IUER, IER )
           CALL MATVIEW_FORM ( FORM, LH, IDG, IBL, INUM, IER )
           IF ( IER > 0 ) THEN
                CALL ERR_LOG ( 9123, IUER, 'MATVIEW', 'Error in format '// &
     &              'specification: '//FORM )
                RETURN
           END IF
!
! ----- Cleran the buffer
!
        DO 410 J1=1,MAX_V
           CALL CLRCH ( BUF(J1) )
  410   CONTINUE
!
! ----- We will write the matrix type and its dimension in the upper corner
! ----- of the screen
!
        IF ( MATYP.EQ.1 ) BUF(1)(1:)='REC'
        IF ( MATYP.EQ.2 ) BUF(1)(1:)='TRA'
        IF ( MATYP.EQ.3 ) BUF(1)(1:)='SYM'
        IF ( MATYP.EQ.4 ) BUF(1)(1:)='SYM'
        IL=ILEN(BUF(1)) !  Compute the line length
        CALL INCH ( MV, BUF(1)(IL+2:) )
        IL=ILEN(BUF(1)) !  Compute the line length
        BUF(1)(IL+1:)='*'
        IL=ILEN(BUF(1)) !  Compute the line length
        CALL INCH ( MH, BUF(1)(IL+1:) )
        IL=ILEN(BUF(1)) !  Compute the line length
!
! ----- Find position of the beginning of the title. If we have enough
! ----- epace then center it.
!
        ILR= ( LH-ILEN(ZAG) ) /2
        IF ( ILR.LT.IL+1 ) ILR=IL+1
!
! ----- Put hte title into the buffer
!
        BUF(1)(ILR:)=ZAG
!A        BUF(1)(LH:)=CRLF
!A        BUF(2)(LH:)=CRLF
!#        IF ( IT .GE. 1  .AND.  IT .LE. 5 ) THEN
!#             BUF(3)=SO//CR
!#          ELSE IF ( IT .EQ. 6 ) THEN
!#!B             BUF(3)=CR
!#        END IF
!
! ----- Build the upper frame of the matrix
!
        BUF(ISHV)(ISHH:ISHH)=LU_CORNER
        CALL REPEAT ( HRU_LINE, LH-12, BUF(ISHV)(ISHH+1:) )
        BUF(ISHV)(LH-ISHH:LH-ISHH)=RU_CORNER
!A        BUF(ISHV)(LH:)=CRLF
!
! ----- Build the left and rifht frames of the matrix
!
        DO 420 J2=1,LV
           BUF(J2+ISHV)(ISHH:ISHH)=VR_LINE
           BUF(J2+ISHV)(LH-ISHH:LH-ISHH)=VR_LINE
!A           BUF(J2+ISHV)(LH:)=CRLF
  420   CONTINUE
!
! ----- Build the low frame of the matrix
!
        BUF(ISHV+LV+1)(ISHH:ISHH)=LD_CORNER
        CALL REPEAT ( HRD_LINE, LH-12, BUF(LV+ISHV+1)(ISHH+1:) )
        BUF(ISHV+LV+1)(LH-ISHH:LH-ISHH)=RD_CORNER
!A        BUF(ISHV+LV)(LH:)=CRLF
!
! ----- Digitize the left and the right part of the frame
!
        DO 430 J3=1,LV
           CALL INCH ( KV+J3-1, BUF(J3+ISHV)(1:ISHH-1) ) ! decoding
           CALL CHASHR (        BUF(J3+ISHV)(1:ISHH-1) ) ! alignment
           BUF(J3+ISHV)(ISHH:ISHH)=POINTER_LEFT
           CALL INCH ( KV+J3-1, BUF(J3+ISHV)(LH-ISHH+1:LH) ) ! decoding
           CALL CHASHL (        BUF(J3+ISHV)(LH-ISHH+1:LH) ) ! alignment
           BUF(J3+ISHV)(LH-ISHH:LH-ISHH)=POINTER_RIGHT
  430   CONTINUE
!
! ----- Digitize the upper and lower frame
!
        IPL_LAST=0
        DO 440 J4=1,INUM
           IPL=ISHH + IDG + (J4-1)*(IDG+IBL) - IDG/2   ! compute position
           BUF(ISHV)(IPL:IPL)=POINTER_UP
           BUF(LV+ISHV+1)(IPL:IPL)=POINTER_DOWN
           CALL INCH   ( KH+J4-1, STR )  ! decode
           INDN=ILEN(STR)
           IF ( (IPL-IPL_LAST) .LE. INDN  ) GOTO 440
!
           BUF(2)(IPL-INDN+1:IPL)=STR
           CALL CHASHR ( BUF(2)(IPL-INDN+1:IPL) )  ! align
!
           BUF(LV+ISHV+2)(IPL-INDN+1:IPL)=STR
           CALL CHASHR ( BUF(LV+ISHV+2)(IPL-INDN+1:IPL) ) ! align
!
           IPL_LAST=IPL
  440   CONTINUE
!
! ----- Cycle over filling the buffer with values of the matrix elements
!
        DO 450 J5=1,LV
           IF ( J5-1+KV .LE. NV ) THEN
              DO 460 J6=1,INUM
                IF ( (MATYP.EQ.1 .AND. J6-1+KH .LE. NH)        .OR. &
     &               (MATYP.EQ.2 .AND. J6-1+KH .LE. NH)        .OR. &
     &               (MATYP.EQ.3 .AND. J6-1+KH .LE. NV  .AND.       &
     &                                (J5-1+KV).LE.(J6-1+KH) ) .OR. &
     &               (MATYP.EQ.4 .AND. J5-1+KV .LE. NV  .AND.       &
     &                                (J5-1+KV).GE.(J6-1+KH) )      ) THEN
!
! ---------------- Compute the index of the matrix element and ...
!
                   ILOC = LOCC ( MATYP, J5-1+KV, J6-1+KH )
                   IF ( MATYP .EQ. 4 ) ILOC = LOCL ( MV, J5-1+KV, J6-1+KH )
!
! ---------------- ...  write it into the buffer
!
                   IB=ISHH+1+ (J6-1)*(IDG+IBL)
                   IE=ISHH+1+     J6*(IDG+IBL)
!
                   IF ( A(ILOC) .EQ. 0.0D0 ) THEN
!
! --------------------- "clear" null
!
                        IP=(IB+IE)/2
                        BUF(ISHV+J5)(IP:IP)='0'
                    ELSE
!
! --------------------- a real number
!
                        WRITE ( UNIT=BUF(ISHV+J5)(IB:IE), FMT=FORM, &
     &                          IOSTAT=IOS ) A(ILOC)
                   END IF
!
! ---------------- Replace the lines of type 0.00000  with 0.0
!
                   IP=INDEX ( BUF(ISHV+J5)(IB:IE), '0.0' )
                   IF ( IP.NE.0 ) THEN
                        IF ( LIB$SKPC ( '0', BUF(ISHV+J5)(IB+IP+2:IE) ) &
     &                  .NE. 0 ) THEN
                             IF ( IB+IP+3 .LT. IE ) &
     &                       CALL CLRCH ( BUF(ISHV+J5)(IB+IP+3:IE) )
                        END IF
                   END IF
                END IF
  460         CONTINUE
           END IF
  450   CONTINUE
!
!
! ----- Build the last line of the screen
!
!A        BUF(24)=SI
!A        BUF(25)(LH:)=CRLF
        BUF(LV+ISHV*2)='MATview  V2.4  2004.09.27 (c) L. Petrov '// &
     &          '   Enter the command  >>' ! //LS2R
        ILN = ILEN( BUF(LV+ISHV*2) )
        IF ( ILN .GT. LH-1 ) THEN
             BUF(LV+ISHV*2)(1:LH-1) = BUF(LV+ISHV*2)(ILN-LH+2:ILN)
             IF ( LH .LT. LEN(BUF(1)) ) CALL CLRCH ( BUF(LV+ISHV*2)(LH:) )
        END IF
        IL1 = ILEN(BUF(1))
        IF ( IL1 .GT. LH-1 ) CALL CLRCH ( BUF(1)(LH:) )
!
        CALL PRCH ( CRLF )      !  currage rerurn
!
! ----- Display information which we have prepared and saved in the buffer
!
!#        CALL PRI_BUF ( BUF, MAX_V, , 4, -1, , -1 )
        CALL PRI_BUF ( BUF, LSTR, LEN(BUF(1)), 1 )
        IF ( .NOT. FL_IN_TERM  ) THEN
             WRITE ( 6, '(A)' ) 'MATVIEW: Standard input is not a terminal'
             CALL ERR_LOG ( 0, IUER )
             RETURN 
        END IF
!
        IF ( IT .GE. 1  .AND.  IT .LE. 5 ) THEN
             CALL ADR_CURSOR ( LSTR-2, 1  )  !  direct cursor adressing
          ELSE
             CALL ADR_CURSOR ( LSTR, 67   )  !  direct cursor adressing
        END IF
        IF ( IT.GE.1  .AND.  IT.LE.5 ) THEN
!
! ---------- Abolish diplaying the ciror
!
             CALL PRCH ( CHAR(155)//'?25l' )
        END IF
!
! ----- Wait for hitting  akey. The key code and the generalized keystroke
! ----- code are returned as variables AS and IS.
!
!#        CALL INSIM ( AS, IS )
        IS = INSIM ( AS, ISM )
        IF ( IT.GE.1  .AND.  IT.LE.5 ) THEN
!
! ---------- Restore cursor displaying
!
             CALL PRCH ( CHAR(155)//'?25h' )
        END IF
!
! ----- Syntax analysis of the the command to have been input.
! ----- Request of additional information if needed. The following variables
! ----- get values as a result of syntax parsing:
! ----- ICOM     --  Command code.
! ----- ICH_PAR  --  The first auxillary command argument of INTEGER*4 type,
! ----- OUT      --  the second auxillary command argument of CHARACTER type
!
        CALL MATVIEW_COM ( AS, IS, LSTR, LH, FORM, ICOM, ICH_PAR, OUT )
!
! ----- Do correction of the parameters of information displaying  as
! ----- a result of command parsing
!
        IF ( ICOM.EQ.1 ) THEN
!
! ---------- shift the matrix window left
!
             KV=KV-ICH_PAR
             IF ( KV .LE. 1 ) KV=1
          ELSE IF ( ICOM.EQ.2 ) THEN
!
! ---------- Shift the matrix window right
!
             KV=KV+ICH_PAR
          ELSE IF ( ICOM.EQ.3 ) THEN
!
! ---------- Shift the matrix window down
!
             KH=KH+ICH_PAR
          ELSE IF ( ICOM.EQ.4 ) THEN
!
! ---------- Shift the matrix window up
!
             KH=KH-ICH_PAR
             IF ( KH .LE. 1 ) KH=1
          ELSE IF ( ICOM.EQ.5 ) THEN
!
! ---------- set up margin
!
             KV=ICH_PAR
          ELSE IF ( ICOM.EQ.6 ) THEN
!
! ---------- set down margin
!
             KV=ICH_PAR - LV + 1
             IF ( KV .LT. 1 ) KV=1
          ELSE IF ( ICOM.EQ.7 ) THEN
!
! ---------- set right margin
!
             KH=ICH_PAR - INUM + 1
             IF ( KH .LT. 1 ) KH=1
          ELSE IF ( ICOM.EQ.8 ) THEN
!
! ---------- set left margin
!
             KH=ICH_PAR
          ELSE IF ( ICOM.EQ.9  ) THEN
!
! ---------- Return to initial values of matrix dislpaying
!
             KH=IH
             KV=IV
             IF ( LEN(FORM_I) .GT. 0 ) FORM=FORM_I
             IF ( LH .NE. LH_0 ) THEN
                  LH=LH_0
!#                  IF ( LH_0.EQ.80  ) CALL SESC_80
!#                  IF ( LH_0.EQ.132 ) CALL SESC_132
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL MATVIEW_FORM ( FORM, LH, IDG, IBL, INUM, IER )
             IF ( IER > 0 ) THEN
                  CALL ERR_LOG ( 9124, IUER, 'MATVIEW', 'Wrong format '// &
     &                          'was specified: '//FORM )
                  GOTO 910
             END IF
          ELSE IF ( ICOM.EQ.11 ) THEN
!
! ---------- show help information about Matview
!
             CALL MATVIEW_HELP ( HELP_FILE, RD_HBF, MV_HBF, HELP_BUF, &
     &                           LHS_HBF, LV_HBF, LH, LSTR )
          ELSE IF ( ICOM .EQ. 10 ) THEN
!
! ---------- Normal exit form the program
!
             CALL ERR_LOG ( 0, IUER )
             WRITE ( 6, '(A)' ) ' '
             RETURN
          ELSE IF ( ICOM.EQ.12 ) THEN
!
! ---------- Analysi of new format specification
!
             IER=-1
             CALL MATVIEW_FORM ( OUT, LH, IDG_N, IBL_N, INUM_N, IER )
             IF ( IER.EQ.0 ) THEN
!
! --------------- Analys showed the the new format specificiation is correct
! --------------- Renew the format
!
                  FORM = OUT(1:32)
                  IDG  = IDG_N
                  IBL  = IBL_N
                  INUM = INUM_N
             END IF
          ELSE IF ( ICOM.EQ.13 ) THEN
!
! ---------- Set 132 column mode
!
!#             LH=132
!#             CALL SESC_132
!
             CALL ERR_PASS ( IUER, IER )
             CALL MATVIEW_FORM ( FORM, LH, IDG, IBL, INUM, IER )
             IF ( IER > 0 ) THEN
                  CALL ERR_LOG ( 9125, IUER, 'MATVIEW', 'Wrong '// &
     &                          'format specification: '//FORM )
                  GOTO 910
             END IF
          ELSE IF ( ICOM.EQ.14 ) THEN
!
! ---------- Set 80 column mode
!
!#             LH=80
!#             CALL SESC_80
!
             CALL ERR_PASS ( IUER, IER )
             CALL MATVIEW_FORM ( FORM, LH, IDG, IBL, INUM, IER )
             IF ( IER > 0 ) THEN
                  CALL ERR_LOG ( 9126, IUER, 'MATVIEW', 'Wring '// &
     &                          'format specification mode'//FORM )
                  GOTO 910
             END IF
        END IF
!
! ----- Correction of the matrix displaying parameters
!
        IF ( KV.LT.1 ) KV=1
        IF ( KH.LT.1 ) KH=1
        GOTO 910
        END  SUBROUTINE MATVIEW  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE MATVIEW_1 ( NV, NH, A )
! ************************************************************************
! *                                                                      *
! *     Routine  MATVIEW_1  is a wrapper subroutine for the routine      *
! *   MATVIEW. It displayes rectangular matrix A of dimenstion NV*NH     *
! *   in the screen in an interactive mode. A user has a capacity to     *
! *   change parameters of the text window where the matrix is displayed *
! *   and the format of displaying matrix elements.                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  NV ( INTEGER*4 ) -- The first matrix dimension.                     *
! *  NH ( INTEGER*4 ) -- The second matrix dimension.                    *
! *   A ( REAL*8    ) -- Displayed matrix.                               *
! *                                                                      *
! *     NB: Further documentation can be found in files                  *
! *   $HELP_DIR/matview.hlp, $HELP_DIR/matview_1.hlp                     *
! *                                                                      *
! *  ### 27-JUN-1994   MATVIEW_1   V1.1  (c) L. Petrov 04-JUL-1994  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  NV, NH
        REAL*8     A(NV,NH)
        CALL MATVIEW ( 1, NV, NH, A, 'Untitled', '()', 1, 1, -3 )
        RETURN
        END  SUBROUTINE MATVIEW_1  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE MATVIEW_2 ( N, A )
! ************************************************************************
! *                                                                      *
! *     Routine  MATVIEW_2  is a wrapper subroutine for the routine      *
! *   MATVIEW. It displayes a symmetrical matrix A in the upper          *
! *   triangular representaiont of dimenstion NV*NH in the screen        *
! *   in an interactive mode. A user has a capacity to change parameters *
! *   of the text window where the matrix is displayed and the format of *
! *   displaying matrix elements.                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   N ( INTEGER*4 ) -- The matrix dimension.                           *
! *   A ( REAL*8    ) -- Displayed matrix.                               *
! *                                                                      *
! *     NB: Further documentation can be found in files                  *
! *   $HELP_DIR/matview.hlp, $HELP_DIR/matview_1.hlp                     *
! *                                                                      *
! *  ### 27-JUN-1994   MATVIEW_2   V1.1  (c) L. Petrov 04-JUL-1994  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  N
        REAL*8     A(*)
        CALL MATVIEW ( 3, N, N, A, 'Untitled', '()', 1, 1, -3 )
        RETURN
        END  !#!  MATVIEW_2  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE MATVIEW_3 ( N, A )
! ************************************************************************
! *                                                                      *
! *     Routine  MATVIEW_3  is a wrapper subroutine for the routine      *
! *   MATVIEW. It displayes a symmetrical matrix A in the lower          *
! *   triangular representaiont of dimenstion NV*NH in the screen        *
! *   in an interactive mode. A user has a capacity to change parameters *
! *   of the text window where the matrix is displayed and the format of *
! *   displaying matrix elements.                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   N ( INTEGER*4 ) -- The matrix dimension.                           *
! *   A ( REAL*8    ) -- Displayed matrix.                               *
! *                                                                      *
! *     NB: Further documentation can be found in files                  *
! *   $HELP_DIR/matview.hlp, $HELP_DIR/matview_1.hlp                     *
! *                                                                      *
! *  ### 27-SEP-2004   MATVIEW_3   V1.1  (c) L. Petrov  27-SEP-2004 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  N
        REAL*8     A(*)
        CALL MATVIEW ( 4, N, N, A, 'Untitled', '()', 1, 1, -3 )
        RETURN
        END  !#!  MATVIEW_3  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE MATVIEW_COM ( AS, ISYM, LSTR, LH, FORM, ICOM, ICH_PAR, OUT )
! ************************************************************************
! *                                                                      *
! *     χσπονοηατεμψξαρ ποδπςοηςαννα  MATVIEW_COM  αξαμιϊιςυετ λοδ       *
! *     λμαχιϋι, λοτοςαρ βωμα ξαφατα χο χςενρ οφιδαξιρ χχοδα λοναξδ      *
! *     πςοηςαννω  MATVIEW  ι χ ϊαχισινοστι οτ ςεϊυμψτατα ςαϊβοςα μιβο   *
! *     ϊαπςαϋιχαετ εύ³ λαλυΰ-μιβο ιξζοςναγιΰ, μιβο σςαϊυ χοϊχςαύαετ     *
! *     λοδ λοναξδω.                                                     *
! *                                                                      *
! * ________________________ χθοδξωε παςανετςω: ________________________ *
! *                                                                      *
! *       AS ( CHARACTER ) -- στςολα, σοστορύαρ ιϊ οδξοηο σινχομα λοδα   *
! *                           χχεδ³ξξοκ λμαχιϋι.                         *
! *     ISYM ( INTEGER*4 ) -- οβοβύ³ξξωκ λοδ χχεδ³ξξοκ λμαχιϋι.          *
! *     FORM ( CHARACTER ) -- ζοςνατ οτοβςαφεξιρ οδξοηο όμενεξτα         *
! *                           νατςιγω, υσταξοχμεξξωκ ξα νονεξτ ςαϊβοςα   *
! *                           λοναξδω.                                   *
! *                                                                      *
! * _______________________ χωθοδξωε παςανετςω: ________________________ *
! *                                                                      *
! *     ICOM ( INTEGER*4 ) -- λοδ χχεδ³ξξοκγ λοναξδω.                    *
! *  ICH_PAR ( INTEGER*4 ) -- πεςχωκ δοπομξιτεμψξωκ παςανετς λοναξδω.    *
! *      OUT ( CHARACTER ) -- χτοςοκ δοπομξιτεμψξωκ παςανετς λοναξδω.    *
! *                                                                      *
! *  ###  28-JUN-94   MATVIEW_COM  V2.3 (c) πΕΤÒΟΧ μ.ΰ.  14-MAR-97  ###  *
! *                                                                      *
! ************************************************************************
        CHARACTER  FORM*(*), OUT*(*), AS*(*), STR*20, POINT(14)*(32), PROMPT*80
        INTEGER*4  LSTR, LH, LH_LIM, ILP, IOS, IER
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
!#        DATA POINT /
!#     #               ' σΔΧΙΗ ΟΛΞΑ ΧΧΕÒΘ ΞΑ            ',  !  1
!#     #               ' σΔΧΙΗ ΟΛΞΑ ΧΞΙΪ ΞΑ             ',  !  2
!#     #               ' σΔΧΙΗ ΟΛΞΑ ΧΠÒΑΧΟ ΞΑ           ',  !  3
!#     #               ' σΔΧΙΗ ΟΛΞΑ ΧΜΕΧΟ ΞΑ            ',  !  4
!#     #               ' χ ΧΕÒΘΞΕΝ ΥΗΜΥ ΒΥΔΕΤ ΣΤÒΟΛΑ    ', !  5
!#     #               ' χ ΧΕÒΘΞΕΝ ΥΗΜΥ ΒΥΔΕΤ ΣΤÒΟΛΑ    ',  !  6
!#     #               ' χ ΜΕΧΟΝ ΥΗΜΥ ΒΥΔΕΤ ΣΤΟΜΒΕΓ     ',  !  7
!#     #               ' χ ΜΕΧΟΝ ΥΗΜΥ ΒΥΔΕΤ ΣΤΟΜΒΕΓ     ',  !  8
!#     #               '                                ',  !  9
!#     #               '                                ',  ! 10
!#     #               '                                ',  ! 11
!#     #               ' σΛΟÒÒΕΛΤΙÒΥΚΤΕ ΖΟÒΝΑΤ >>       ',  ! 12
!#     #               '                                ',  ! 13
!#     #               '                                ' / ! 14
        DATA POINT / &
     &               ' Shift window up at             ', &   !  1
     &               ' Shift window down at           ', &   !  2
     &               ' Shift window right at          ', &   !  3
     &               ' Shift window left at           ', &   !  4
     &               ' Set up up margin               ', &   !  5
     &               ' Set up down margin             ', &   !  6
     &               ' Set up right margin            ', &   !  7
     &               ' Set pup left margin            ', &   !  8
     &               '                                ', &   !  9
     &               '                                ', &   ! 10
     &               '                                ', &   ! 11
     &               ' Enter new format               ', &   ! 12
     &               '                                ', &   ! 13
     &               '                                '      / ! 14
!
        INTEGER*4  KEY_GOLD, KEY_UP, KEY_DOWN, KEY_RIGHT, KEY_LEFT, &
     &             KEY_SH_UP, KEY_SH_DOWN, KEY_SH_RIGHT, KEY_SH_LEFT, &
     &             KEY_HOME, KEY_INSLINE, KEY_DELLINE
        PARAMETER ( KEY_GOLD     =  92 )
        PARAMETER ( KEY_UP       = 515 )
        PARAMETER ( KEY_DOWN     = 516 )
        PARAMETER ( KEY_RIGHT    = 517 )
        PARAMETER ( KEY_LEFT     = 518 )
        PARAMETER ( KEY_SH_UP    = 527 )
        PARAMETER ( KEY_SH_DOWN  = 528 )
        PARAMETER ( KEY_SH_RIGHT = 514 )
        PARAMETER ( KEY_SH_LEFT  = 513 )
        PARAMETER ( KEY_HOME     = 531 )
        PARAMETER ( KEY_INSLINE  = 523 )
        PARAMETER ( KEY_DELLINE  = 524 )
 910    CONTINUE
!
! ----- ξαώαμψξωε οβξυμεξιρ
!
        CALL CLRCH ( OUT )
        ICOM=0
        ICH_PAR=0
!
! ----- πυσταρ λοναξδα
!
        IF ( ISYM.LE.32 ) RETURN
        IF ( ISYM .EQ. ICHAR('E') .OR. &
     &       ISYM .EQ. ICHAR('e') .OR. &
     &       ISYM .EQ. ICHAR('X') .OR. &
     &       ISYM .EQ. ICHAR('x') .OR. &
     &       ISYM .EQ. ICHAR('Q') .OR. &
     &       ISYM .EQ. ICHAR('q')      ) THEN
             ICOM=10
             RETURN
        END IF
!
        IF ( ISYM .EQ. ICHAR('F') .OR. ISYM .EQ. ICHAR('f') ) THEN
!
! ---------- λοναξδα σνεξω ζοςνατα
!
             ICOM=12
             CALL ADR_CURSOR ( LSTR, 1 )
             GOTO 810
        END IF
        IF ( ISYM .EQ. ICHAR('H') .OR. ISYM .EQ. ICHAR('h') ) THEN
!
! ---------- λοναξδα χωχοδα ξα όλςαξ ποδσλαϊλι
!
             ICOM=11
             RETURN
        END IF
        IF ( ISYM.EQ.ICHAR('1') ) ICOM=13
        IF ( ISYM.EQ.ICHAR('8') ) ICOM=14
        IF ( ICOM.EQ.13 .OR. ICOM.EQ.14 ) RETURN
!
! ----- πςωηαεν χ ξαώαμο πομρ χχοδα
!
        CALL ADR_CURSOR  ( LSTR, 67 )
!
! ----- ςαϊβος ESC-ποσμεδοχατεμψξοστεκ
!
        IF ( ISYM .EQ. KEY_GOLD ) THEN
!
! ---------- ξαφατα λμαχιϋα <PF1> : ξαδο χχεστι εύ£ οδξυ λμαχιϋυ
!
             ISYM = INSIM ( AS, IS )
             IADD=4 ! πςιϊξαλ τοηο, ώτο λοναξδα ξαώιξαμασψ σ <PF1>
           ELSE
             IADD=0 ! πςιϊξαλ τοηο, ώτο λοναξδα ξε ξαώιξαμασψ σ <PF1>
        END IF
!
        IF ( ISYM .GE. 256 ) THEN
!
! ---------- ξαϊξαώεξιε λοδοχ λοναξδ
!
             IF ( ISYM .EQ. KEY_HOME     ) ICOM = 9
             IF ( ISYM .EQ. KEY_UP       ) ICOM = 1 + IADD
             IF ( ISYM .EQ. KEY_DOWN     ) ICOM = 2 + IADD
             IF ( ISYM .EQ. KEY_RIGHT    ) ICOM = 3 + IADD
             IF ( ISYM .EQ. KEY_LEFT     ) ICOM = 4 + IADD
             IF ( ISYM .EQ. KEY_SH_UP    ) ICOM = 5
             IF ( ISYM .EQ. KEY_SH_DOWN  ) ICOM = 6
             IF ( ISYM .EQ. KEY_SH_RIGHT ) ICOM = 7
             IF ( ISYM .EQ. KEY_SH_LEFT  ) ICOM = 8
             IF ( ISYM .EQ. KEY_INSLINE  ) ICOM = 10
             IF ( ISYM .EQ. KEY_DELLINE  ) ICOM = 10
             IF ( ICOM .EQ. 10  ) GOTO 830
             IF ( ICOM .EQ. 9   ) GOTO 830
!
! ---------- λοδ δαξξοκ ESC-ποσμεδοχατεμψξοστι ξε βωμ ςασποϊξαξ λαλ ϊαλοξξαρ
! ---------- λοναξδα. πεςεθοδ χ ςεφτιιν χχοδα τελστοχοκ λοναξδω
!
             IF ( ICOM.EQ.0   ) GOTO 820
!
! ---------- ϊαπςοσ δοπομξιτεμψξοκ ιξζοςναγιι υ λοναξδ ιϊνεξεξιρ
! ---------- ποϊιγιοξιςοχαξιρ ολξα οτοβςαφεξιρ νατςιγω
!
             CALL CLSTR           !  στιςαεν τελυύυΰ στςολυ
!
! ---------- χωχοδ ποδσλαϊλι
!
             CALL PRCH ( POINT(ICOM) )
             CALL CURR ( 1 )            ! λυςσος χπςαχο ξα δομιξ σινχομ
             CALL CLRCH ( STR )         ! οώιστλα στςολι
!
! ---------- σοβστχεξξο χχοδ στςολι
!
!!             CALL INSTR ( '', %VAL(0), '', STR, %VAL(0) )
             READ ( 5, '(A)', IOSTAT=IOS ) STR
!
! ---------- πεςελοδιςοχαξιε χχεδ³ξξοκ στςολι χ γεμοε ώισμο
!
             CALL CHIN ( STR, ICH_PAR )
             IF ( ICH_PAR .LT. -11111 ) THEN
                  IER = 1
                ELSE
                  IER = 0
             END IF
             IF ( ILEN(STR).EQ.0 ) THEN
!
! --------------- ξιώεηο ξε βωμο χχεδεξο. ιξτεπςετιςυετσρ λαλ ξυμψ
!
                  IER=0
                  ICH_PAR=0
             END IF
!
             IF ( IER .NE. 0 ) THEN
                  CALL ADR_CURSOR  ( LSTR, 1 )  !  λυςσοςα -- χ ξαώαμο ποσμεδξεκ στςολι
                  CALL CLSTR           !  στιςαεν τελυύυΰ στςολυ
                  CALL NEG             !  πεςεθοδιν χ ςεφιν ξεηατιχα
                  CALL PRCH ( '$$$  error in format of the string "'// &
     &                         STR(1:I_LEN(STR))//'"  $$$' )
                  CALL UN_NEG          !  σξιναεν ςεφιν ξεηατιχα
                  CALL LIB$WAIT ( 2.0D0 ) !  φδ³ν 2 σελυξδω
                  CALL CLSTR           !  στιςαεν τελυύυΰ στςολυ
                  GOTO 910             !  ...  ι χσ³ πο ξοχοκ
             END IF
             GOTO 830  !  λοξεγ
        END IF
!
! ----- ποπωτλα ώτεξιρ λοναξδω χ τελστοχον ςεφινε
!
  820   CONTINUE
        CALL CLRCH ( STR )
        IER = -1
        IF ( ICHAR(AS).GT.32 ) THEN
             CALL INSTR ( '', 1, AS , STR, IER )  !  σοβστχεξξο χχοδ στςολι
          ELSE
             CALL INSTR ( '', %VAL(0),'', STR, IER )      !  σοβστχεξξο χχοδ στςολι
        END IF
!
! ----- πεςελοδιςυεν στςολυ χ σινχοδω χεςθξεηο ςεηιστςα
!
        CALL TRAN ( 11, STR, STR )
!
! ----- σιξταλσιώεσλικ ςαϊβος χχεδ³ξξοκ στςολι
!
        IF ( STR(1:1) .EQ. 'E' .OR. STR(1:2).EQ.'/E' ) ICOM=10
        IF ( STR(1:1) .EQ. 'X' .OR. STR(1:2).EQ.'/X' ) ICOM=10
        IF ( STR(1:1) .EQ. 'Q' .OR. STR(1:2).EQ.'/Q' ) ICOM=10
        IF ( STR(1:1) .EQ. 'H' .OR. STR(1:2).EQ.'/H' ) ICOM=11
        IF ( STR(1:1) .EQ. 'F' .OR. STR(1:2).EQ.'/F' ) ICOM=12
  810   CONTINUE
        IF ( ICOM.EQ.12 ) THEN
!
! ---------- χχοδ ξοχοηο ζοςνατα
!
             CALL ADR_CURSOR  ( LSTR, 1 )  !  λυςσοςα -- χ ξαώαμο ποσμεδξεκ στςολι
             CALL CLSTR           !  στιςαεν τελυύυΰ στςολυ
!!!             CALL CURU ( 1 )      !  λυςσος χχεςθ
             CALL ADR_CURSOR  ( LSTR-1, 1 )
             IER=-1
             CALL CLRCH ( PROMPT )
             PROMPT = 'Current format is: '//FORM(1:I_LEN(FORM))// &
     &                    '  '//POINT(ICOM)(1:I_LEN(POINT(ICOM)))//'  '
             ILP = ILEN(PROMPT) + 2
             LH_LIM = LH - ILEN(FORM)
             IF ( LH_LIM .LT. 2 ) LH_LIM = 2
             IF ( ILP .GT. LH_LIM ) THEN
                  PROMPT(1:LH_LIM-1) = PROMPT(ILP-LH_LIM+2:ILP)
                  CALL CLRCH ( PROMPT(LH_LIM:) )
                  ILP = LH_LIM-1
             END IF
             CALL INSTR ( PROMPT(1:ILP), ILEN(FORM), FORM, OUT, IER )
             IF ( ILEN(OUT).EQ.0 ) ICOM=0 !  ξιώεηο ξε χχεμι: υσταξαχμιχαεν
!                                         !  πςιϊξαλ πυστοκ λοναξδω
        END IF
!
  830   CONTINUE
        RETURN
        END  !#!  MATVIEW_COM  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE  MATVIEW_FORM ( FORM, LH, IDG, IBL, INUM, IUER )
! ************************************************************************
! *                                                                      *
! *     χσπονοηατεμψξαρ ποδπςοηςαννα  MATVIEW_FORM  ςαϊβιςαετ ζοςνατ     *
! *     οτοβςαφεξιρ οδξοηο όμενεξτα νατςιγω, πςοχεςρετ εηο πςαχιμψξοστψ  *
! *     ι χωώισμρετ δμιξω πομεκ ζοςνατα.                                 *
! *                                                                      *
! * ________________________ χθοδξωε παςανετςω: ________________________ *
! *                                                                      *
! *    FORM ( CHARACTER ) -- αξαμιϊιςυεναρ στςολα ζοςνατα.               *
! *      LH ( INTEGER*4 ) -- ϋιςιξα όλςαξα ( χ σινχομαθ ).               *
! *                                                                      *
! * _______________________ χωθοδξωε παςανετςω: ________________________ *
! *                                                                      *
! *     IDG ( INTEGER*4 ) -- λομιώεστχο σινχομοχ χ ζοςνατε, οτχεδ³ξξωθ   *
! *                          ποδ σανο ώισμο.                             *
! *     IBL ( INTEGER*4 ) -- λομιώεστχο σινχομοχ χ ζοςνατε, οτχεδ³ξξωθ   *
! *                          ποδ ςαϊδεμιτεμψξωε πςοβεμω.                 *
! *    INUM ( INTEGER*4 ) -- λομιώεστχο όμενεξτοχ νατςιγω, λοτοςοε       *
! *                          ςαϊνεστιτσρ ξα οδξοκ στςολε όλςαξα.         *
! *                                                                      *
! * ___________________ νοδιζιγιςυενωε παςανετςω: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- παςανετς οϋιβλι:                       *
! *             χθοδξοε ϊξαώεξιε  --  ςεφιν οβςαβοτλι οϋιβλι:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- χοϊχςαύεξιε λοδα οϋιβλι.                             *
! *      IUER=-1 -- χοϊχςαύεξιε λοδα IUER=0 χ σμυώαε ξοςναμψξοηο         *
! *                 ϊαχεςϋεξιρ ι χωχοδ διαηξοστιώεσλοηο σοοβύεξιρ        *
! *                 χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.                       *
! *      IUER<-1 -- χοϊχςαύεξιε λοδα IUER=0 χ σμυώαε ξοςναμψξοηο         *
! *                 ϊαχεςϋεξιρ, χωχοδ διαηξοστιώεσλοηο σοοβύεξιρ ι       *
! *                 ϊαχεςϋεξιε οβςαϊα χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.     *
! *      εσμι IUER οπυύεξ, το χθοδξοε ϊξαώεξιε πςιξιναετσρ ςαχξων -1     *
! *             χωθοδξοε ϊξαώεξιε  --  λοδ οϋιβλι ( εσμι IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             δοστυπεξ δμρ ϊαπισι ):                                   *
! *      IUER=0  --  ξοςναμψξοε ϊαχεςϋεξιε.                              *
! *                                                                      *
! *  ###  29-JUN-94  MATVIEW_FORM V1.0  (c) πΕΤÒΟΧ μ.ΰ.  04-JUL-94  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT     NONE
        INTEGER*4    LH, IDG, IBL, INUM, IUER
        CHARACTER    FORM*(*), FORM_DEF*12, REG*3, WORD*16
        INTEGER*4    MIND
        PARAMETER  ( MIND=16 )
        PARAMETER  ( REG = CHAR(0)//CHAR(32)//',' )
        INTEGER*4    LIND, IND(2,MIND)
        INTEGER*4    IL, IBB, IB, IM, IE, IW, IN1, IN2, IN3, IN4, INN, &
      &              INP, J1, IER
        EXTERNAL     ILEN
        INTEGER*4    ILEN
!
        DATA  FORM_DEF  / '(1PG11.4,1X)' /
!
! ----- ςαϊβος ζοςνατα
!
        IL=ILEN(FORM)
!
! ----- εσμι ξαώαμψξωε ι/ιμι λοξεώξωε σλοβλι βωμι οπυύεξω -- χοσπομξρεν ιθ
!
        IF ( FORM(1:1)  .NE.'('  ) FORM='('//FORM
        IL=ILEN(FORM)
        IF ( FORM(IL:IL).NE.')'  ) FORM=FORM(1:IL)//')'
        IL=ILEN(FORM)
!
! ----- εσμι ζοςνατ ολαϊαμσρ πυστων -- βες³ν ζοςνατ πο υνομώαξιΰ
!
        IF ( IL.EQ.2 ) FORM=FORM_DEF
        IL=ILEN(FORM)
!
! ----- πεςχοδιν ζοςνατ χ σινχοδω χεςθξεηο ςεηιστςα
!
        CALL TRAN ( 11, FORM, FORM )
!
! ----- ςαϊβιχαεν ζοςνατ ξα σμοχα. σώιταεν, ώτο σμοχα οτδεμεξω ϊαπρτωνι
!
        CALL EXWORD ( FORM(2:IL-1), MIND, LIND, IND, REG, IER )
!
! ----- ξαώαμψξωε οβξυμεξιρ
!
        IB=0
        IM=0
        IE=0
        IBL=0
        IDG=0
!
! ----- ςαϊβος ζοςνατξωθ σπεγιζιλαγικ
!
        DO 410 J1=1,LIND
!
! -------- πεςεξοσιν J1-οε σμοχο χ πεςενεξξυΰ  WORD
!
           CALL CLRCH ( WORD )
           WORD=FORM(IND(1,J1)+1:IND(2,J1)+1)
           IW=ILEN(WORD)
           IF ( IW .EQ. 0 ) THEN
                CALL ERR_LOG ( 9101, IUER, 'MATVIEW_FORM', &
     &              'Wrong format '//FORM(1:IL)//' : several commas '// &
     &              'follow each other' )
                RETURN
           END IF
!
! -------- πςοεςρεν: ξε σοδεςφιτ μι δαξξοε σμοχα σμεδυΰύιθ ζοςνατξωθ
! -------- σπεγιζιλαγικ:
!
           IN1=INDEX ( WORD, 'F' )
           IN2=INDEX ( WORD, 'D' )
           IN3=INDEX ( WORD, 'E' )
           IN4=INDEX ( WORD, 'G' )
           INN=MAX(IN1,IN2,IN3,IN4)
!
           IF ( INN.GT.0 ) THEN
!
! ------------- ο! δαξξοε σμοχο -- ζοςνατξαρ σπεγιζιλαγιρ χχοδα-χωχοδα ώισμα
! ------------- σ υδχοεξξοκ τοώξοστψΰ
!
                IF ( IM.NE.0 ) THEN
                   CALL ERR_LOG ( 9102, IUER, 'MATVIEW_FORM', &
     &                 'More than one format specificator was found in '// &
     &                 'the format specification '//FORM )
                   RETURN
                END IF
!
! ------------- ιύεν ςαϊδεμιτεμψ πομεκ
!
                INP=INDEX( WORD, '.' )
                IF ( INP.LE.INN+1 ) THEN
                     CALL ERR_LOG ( 9103, IUER, 'MATVIEW_FORM', &
     &                   'Error inparsing the format specificator: "'// &
     &                    FORM(1:IL)//'" the length field was not '// &
     &                   'specified' )
                   RETURN
                END IF
                IF ( INP.EQ.IW ) THEN
                     CALL ERR_LOG ( 9103, IUER, 'MATVIEW_FORM', &
     &                   'Error inparsing the format specificator: "'// &
     &                    FORM(1:IL)//'" the fraction field was not '// &
     &                   'specified' )
                   RETURN
                END IF
!
! ------------- πεςελοδιςυεν πομε δμιξω σπεγιζιλαγιι
!
                IF ( INP.EQ.0 ) INP = IW
                CALL CHIN ( WORD(INN+1:INP-1), IDG )
                IF ( IDG .LT. -11111 ) THEN
                     IER = 1
                   ELSE
                     IER = 0
                END IF
                IF ( IER.NE.0 ) THEN
                   CALL ERR_LOG ( 9105, IUER, 'MATVIEW_FORM', &
     &                 'Error in parsing of the format specification '// &
     &                  FORM(1:IL) )
                   RETURN
                END IF
                IM=1
           END IF
           IF ( WORD(IW:IW).EQ.'X' ) THEN
!
! ------------- ξετ! δαξξοε σμοχο ζοςνατξαρ σπεγιζιλαγιρ ςαϊδεμιτεμρ,
! ------------- σοστορύεηο ιϊ πςοβεμοχ
!
                IER=0
!
! ------------- πεςελοδιιςυεν δμιξυ σπεγιζιλαγιι
!
                IF ( IW.GT.1 ) THEN
                     CALL CHIN ( WORD(1:IW-1), IBB )
                     IF ( IBB .LT. -11111 ) THEN
                          IER = 1
                        ELSE
                          IER = 0
                     END IF
                  ELSE
                     IBB=1
                END IF
                IF ( IER.NE.0 ) THEN
                   CALL ERR_LOG ( 9106, IUER, 'MATVIEW_FORM', &
     &                 'Error in parsing of the format specification '// &
     &                  FORM(1:IL) )
                   RETURN
                END IF
!
! ------------- οπςεδεμρεν: ότοτ ςαϊδεμιτεμψ στοιτ δο ιμι ποσμε σπεγιζιλαγιι
! ------------- χχοδα-χωχοδα ώισμα σ δχοκξοκ τοώξοστψΰ
!
                IF ( IB.EQ.0 .AND. IM.EQ.0 ) IB=1
                IF ( IM.EQ.1 .AND. IE.EQ.0 ) IE=1
!
                IBL=IBL + IBB
           END IF
  410   CONTINUE
        IF ( IDG.EQ.0 ) THEN
             CALL ERR_LOG ( 9107, IUER, 'MATVIEW_FORM', &
     &           'Error in parsing of the format specification '// &
     &            FORM(1:IL)//'" : no specificators for a double '// &
     &           'precision variable, such as F, D, E, G' )
             RETURN
        END IF
!
        INUM=(LH-11)/(IDG+IBL)
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  MATVIEW_FORM  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE  MATVIEW_HELP ( HELP_FILE, RD_HBF, MV_HBF, HELP_BUF, &
     &                             LHS, LV_HBF, MH, MV )
! ************************************************************************
! *                                                                      *
! *     χσπονοηατεμψξαρ ποδπςοηςαννα  MATVIEW_HELP  χωχοδιτ ξα όλςοξ     *
! *     σπςαχοώξυΰ ιξζοςναγιΰ ο ποδπςοηςαννε  MATVIEW.                   *
! *                                                                      *
! * ________________________ χθοδξωε παςανετςω: ________________________ *
! *                                                                      *
! *  HELP_FILE ( CHARACTER ) -- ινρ ζακμα σπςαχοώξοκ ιξζοςναγιι.          *
! *     RD_HBF ( LOGICAL   ) -- πςιϊξαλ τοηο, ώιταμσρ μι ζακμ σπςαχοώξοκ *
! *                             ιξζοςναγιι ιμι ξετ.                      *
! *                                                                      *
! *  ###  30-JUN-94  MATVIEW_HELP V1.2  (c) πΕΤÒΟΧ μ.ΰ.  14-MAR-97 ###   *
! *                                                                      *
! ************************************************************************
        IMPLICIT    NONE
        LOGICAL     RD_HBF
        INTEGER*4   LHS, LV_HBF, MH, MV
        INTEGER*4   MV_HBF, LLH
        CHARACTER   HELP_FILE*(*), PREF*32, POST*16, CRLF*2, STR*10, ESC*1, &
     &              HELP_BUF(MV_HBF)*(*), PRE_INIT*32, POST_INIT*32
        INTEGER*4  IS, ICODE, J1, IT, IG, IP, IST, IRUS, LW_HBF, LHC, IPR, IER
        CHARACTER  ACODE*1
        INTEGER*4, EXTERNAL :: INSIM, ILEN, I_LEN
!
        CRLF=CHAR(13)//CHAR(10)
        ESC=CHAR(27)
!
!#        CALL VER$ARG ( 2 )
!
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( .NOT. RD_HBF ) THEN
!
! ----------- βυζες ποδσλαϊλι εύ³ ξε βωμ πςοώιταξ
!
              IER=0
!
! ----------- ώτεξιε βυζεςα ποδσλαϊλι ιϊ ζακμα
!
              CALL RD_TEXT ( HELP_FILE, MV_HBF, HELP_BUF, LV_HBF, IER )
              IF ( IER.NE.0 ) THEN
!
! ---------------- πςι ώτεξιι ϊαζιλσιςοχαξα οϋιβλα
!
                   CALL BELL ( 1 )       !  ηυδολ
                   CALL ADR_CURSOR  ( 24, 1 ) !  λυςσος -- χ ξαώαμο ποσμεδξεκ στςολι
                   CALL CLSTR            !  στιςαεν τελυύυΰ στςολυ
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
!#                   CALL PRCH  ( 'ξε ξακδεξ ζακμ '//
!#     #                          HELP_FILE(1:I_LEN(HELP_FILE))//
!#     #                          ', σοδεςφαύικ τελστ ποσλαϊλι '//
!#     #                          'IER='//STR(1:I_LEN(STR)) )
                   CALL PRCH ( 'Error '//STR(1:I_LEN(STR))// &
     &                 ' in reading help file '//HELP_FILE )
                   CALL CURR ( 1 )      !  λυςσος χπςαχο
                   CALL LIB$WAIT ( 3.0D0 ) !  φδ³ν 3 σελυξδω
!#                   CALL PRCH ( CRLF )   !  πεςχοδιν λαςετλυ
                   CALL BELL  ( 1 )     !  ηυδολ
                   RETURN
              END IF
!
! ----------- οπςεδεμεξιε ϋιςιξω βυζες ποδσλαϊλι
!
              LW_HBF=ILEN(HELP_BUF(2))
!
! ----------- οπςεδεμεξιε δισπμεκξωθ λοοςδιξατ βυζες ποδσλαϊλι: LHS, LHC
!
              LHS=(MV-LV_HBF)/2 + 1
!
! ----------- ζοςνιςοχαξιε πςεζιλσα δμρ στςολ βυζες ποδσλαϊλι
!
              CALL CLRCH ( PREF )
              CALL CLRCH ( POST )
              IF ( IT.GE.1  .AND.  IT.LE.5 ) THEN
                   LHC=(MH-LW_HBF)/2
                   PREF(1:2)=ESC//'['
                   CALL INCH ( LHC, PREF(ILEN(PREF)+1:) )
                   IPR=ILEN(PREF)+1
                   PREF(IPR:IPR)='C'
                ELSE IF ( IT .EQ. 6 ) THEN
                   LHC=(MH-LW_HBF)-1
                   PREF(1:3)=ESC//'&a'
                   CALL INCH ( LHC, PREF(ILEN(PREF)+1:) )
                   IPR=ILEN(PREF)+1
                   PREF(IPR:IPR)='C'
                   PREF(ILEN(PREF)+1:)=ESC//'&v2S'
                ELSE IF ( IT.LE.7 ) THEN
                   LHC=(MH-LW_HBF)/2
                   PREF(1:2)=ESC//'['
                   CALL INCH ( LHC, PREF(ILEN(PREF)+1:) )
                   IPR=ILEN(PREF)+1
                   PREF(IPR:IPR)='C'
                   PREF(ILEN(PREF)+1:)=ESC//'[30;43;1m'
              END IF
              IPR=ILEN(PREF)
!
! ----------- δοβαχμεξιε πςεζιλσα λ στςολαν βυζες ποδσλαϊλι
!
              DO 410 J1=1,LV_HBF
                 LLH = ILEN(HELP_BUF(J1))
                 IF ( LLH .GT. MH-1 ) LLH = MH-1
                 HELP_BUF(J1)=PREF(1:IPR)//HELP_BUF(J1)(1:LLH)
  410         CONTINUE
              RD_HBF=.TRUE.
        END IF
        CALL CLRCH ( PRE_INIT  )
        CALL CLRCH ( POST_INIT )
        IF ( IT .EQ. 6 ) THEN
!!             PRE_INIT  = ESC//'&v0m1y2I'
             PRE_INIT  = ESC//'&v0m0.41x0.76y0.39z2I'
             POST_INIT = ESC//'&v0m1b1x1y1z2I'
           ELSE IF ( IT .EQ. 7 ) THEN
             POST_INIT = ESC//'[0m'
        END IF
!
! ----- χωχοδ βυζεςα ποδσλαϊλι ξα όλςαξ σ ποχωϋεξξοκ ρςλοστψΰ ( εσμι τεςνιξαμ
! ----- τιπα Microterm 5530 )
!
        CALL ADR_CURSOR ( LHS, 1 )
        IF ( IT.GE.1  .AND. IT.LE.4 ) CALL BRIEF
!#        CALL PRI_BUF ( HELP_BUF, LV_HBF,, 21 )
        CALL PRCH    ( PRE_INIT )
        CALL PRI_BUF ( HELP_BUF, LV_HBF, LEN(HELP_BUF), 1 )
        IF ( IT.GE.1  .AND. IT.LE.4 ) CALL UN_BRIEF
!
! ----- χωχοδ ξα ποσμεδξΰΰ στςολυ ϊασταχλι
!
        CALL ADR_CURSOR ( MV, 1 )
        IS = INSIM ( ACODE, ICODE )
        CALL PRCH    ( POST_INIT )
        CALL ADR_CURSOR ( MV, 1 )
        RETURN
        END  !#!  MATVIEW_HELP  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION POST_MATVIEW ()
! ************************************************************************
! *                                                                      *
! *     χσπονοηατεμψξαρ πςογεδυςα ϊαχεςϋεξιρ δμρ ποδπςοηςαννω  MATVIEW.  *
! *     οξα χοσσταξαχμιχαετ ςεφιν οτοβςαφεξιρ αμζαχιτξο-γιζςοχοηο        *
! *     λυςσοςα.                                                         *
! *                                                                      *
! *  ###  04-JUL-94 POST_MATVIEW V1.0  (c) πΕΤÒΟΧ μ.ΰ.  04-JUL-94  ###   *
! *                                                                      *
! ************************************************************************
        INTEGER*4  POST_MATVIEW
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
!
! ----- χοσσταξαχμιχαετσρ λυςσος
!
        IF ( IT.GE.1  .OR.  IT.LE.5 ) CALL PRCH ( CHAR(155)//'?25h' )
        POST_MATVIEW = 1
        RETURN
        END  !#!  POST_MATVIEW  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MATVIEW_W ( FINAM, MATYP, MV, MH, MAT, ZAG, FORM, IV, IH, &
     &                       IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  MATVIEW_W  writes on disk file in format MATVIEW to see  *
! *    it futher by using seemat.                                        *
! *                                                                      *
! *  ###   02-JAN-97   MATVIEW_W    V1.2  (c) L. Petrov    6-Jan-97 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT     NONE
      CHARACTER    FINAM*(*), ZAG*(*), FORM*(*)
      INTEGER*4    MATYP, MV, MH, IV, IH, IUER
      REAL*8       MAT(*)
      CHARACTER    STR*20
      INTEGER*4    IER, MEL, LUN, IAR(5)
      INTEGER*4,   EXTERNAL :: I_LEN
!
      IF ( MATYP .LT. 1  .OR.  MATYP .GT. 4 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MATYP, STR )
           CALL ERR_LOG ( 9201, IUER, 'MATVIEW_W', 'Wrong value of the '// &
     &         'first argument: '//STR(1:I_LEN(STR))//' It was expected '// &
     &           '1, 2, 3 or 4' )
           RETURN
      END IF
!
! --- Openning output file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9202, IUER, 'MATVIEW_W', 'Error during openning '// &
     &          'output file '//FINAM(1:I_LEN(FINAM)) )
           RETURN
      END IF
!
! --- Writing line - identifier
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_STRING ( LUN, 'MATView', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9203, IUER, 'MATVIEW_W', 'Error during writing to'// &
     &          ' output file '//FINAM(1:I_LEN(FINAM)) )
           RETURN
      END IF
!
! --- Writing header of the matrix
!
      CALL ERR_PASS ( IUER, IER )
      IF ( LOC(ZAG) .NE. 0 ) THEN
           CALL WRBIN_STRING ( LUN, ZAG, IER )
         ELSE
!
! -------- File name in the case when header is omitted
!
           CALL WRBIN_STRING ( LUN, FINAM(1:I_LEN(FINAM)), IER )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
!
! --- Writing format specification
!
      CALL ERR_PASS ( IUER, IER )
      IF ( LOC(FORM) .NE. 0 ) THEN
           CALL WRBIN_STRING ( LUN, FORM, IER )
         ELSE
           CALL WRBIN_STRING ( LUN, '()', IER )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
!
! --- Writing 1) type of matrix, 2-3) their deminsion, 4-5) starting point
!
      IAR(1)=MATYP
      IAR(2)=MV
      IAR(3)=MH
      IAR(4)=IV
      IAR(5)=IH
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'I4', 5, IAR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
!
! --- Calculation number of elements
!
      IF ( MATYP .EQ. 1   .OR.  MATYP .EQ. 2 ) THEN
           MEL = MV*MH
         ELSE IF ( MATYP .EQ. 3  .OR. MATYP .EQ. 4 ) THEN
           MEL = (MV*(MV+1))/2
      END IF
!
! --- Writing elements of the matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'R8', MEL, MAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9204, IUER, 'MATVIEW_W', 'Error '// &
     &         'during writing to file '//FINAM(1:I_LEN(FINAM))// &
     &         ' elements of the matrix ' )
           RETURN
      END IF
!
! --- Closing output file
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9205, IUER, 'MATVIEW_W', 'Error '// &
     &         'during closing file '//FINAM )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MATVIEW_W  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MATVIEW_R ( M, FINAM, MATYP, MV, MH, MAT, ZAG, FORM, &
     &                       IV, IH, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  MATVIEW_R  reads from disk file in format MATVIEW  which *
! *    has been written earlier by MATWIEW_W for futher exposing.        *
! *                                                                      *
! *  ###   02-JAN-97   MATVIEW_R    V1.2  (c) L. Petrov   06-JAN-97 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT     NONE
      CHARACTER    FINAM*(*), ZAG*(*), FORM*(*)
      INTEGER*4    M, MATYP, MV, MH, IV, IH, IUER
      REAL*8       MAT(M)
      LOGICAL      LEX
      CHARACTER    STR*20, STR1*20
      INTEGER*4    IER, LUN, IAR(5), IO, LN, MEL, NEL
      INTEGER*4,   EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=FINAM, EXIST=LEX, IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 9301, IUER, 'MATVIEW_R', 'Wrong file name "'// &
     &          FINAM(1:I_LEN(FINAM))//'". IOSTAT='//STR(1:I_LEN(STR)) )
           RETURN
      END IF
!
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 9302, IUER, 'MATVIEW_R', 'File "'// &
     &          FINAM(1:I_LEN(FINAM))//'" not found' )
           RETURN
      END IF
!
! --- Openning input file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9303, IUER, 'MATVIEW_R', 'Error during openning '// &
     &          'input file '//FINAM(1:I_LEN(FINAM)) )
           RETURN
      END IF
!
! --- Reading line - identifier
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_STRING ( LUN, STR, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9304, IUER, 'MATVIEW_R', 'Error during reading '// &
     &         'from input file '//FINAM(1:I_LEN(FINAM)) )
           RETURN
      END IF
      IF ( STR(1:7) .NE. 'MATView' ) THEN
           CALL ERR_LOG ( 9305, IUER, 'MATVIEW_R', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' has not been written by MATVIEW_R' )
           RETURN
      END IF
!
! --- Reading header of the matrix
!
      CALL CLRCH ( ZAG )
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_STRING ( LUN, ZAG, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
!
! --- Reading format specification
!
      CALL CLRCH ( FORM )
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_STRING ( LUN, FORM, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
!
! --- Reading 1) type of matrix, 2-3) their deminsion, 4-5) starting point
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_ARRAY ( LUN, 'I4', 5, IAR, NEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
      MATYP=IAR(1)
      MV=IAR(2)
      MH=IAR(3)
      IV=IAR(4)
      IH =IAR(5)
!
      IF ( MATYP .LT. 1 .OR. MATYP .GT. 4 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MATYP, STR )
           CALL ERR_LOG ( 9306, IUER, 'MATVIEW_R', 'Wrong value of the '// &
     &         'argument MATVIEW: '//STR(1:I_LEN(STR))//' It was expected '// &
     &         '1, 2, 3 or 4' )
             RETURN
      END IF
!
! --- Calculation number of elements
!
      IF ( MATYP .EQ. 1  .OR.  MATYP .EQ. 2 ) THEN
           MEL = MV*MH
         ELSE IF ( MATYP .EQ. 3  .OR.  MATYP .EQ. 4 ) THEN
           MEL = (MV*(MV+1))/2
      END IF
      IF ( MEL .GT. M ) THEN
           CALL CLRCH ( STR       )
           CALL CLRCH ( STR1      )
           CALL INCH  ( M,   STR  )
           CALL INCH  ( MEL, STR1 )
           CALL ERR_LOG ( 9307, IUER, 'MATVIEW_R', 'Parameter M is too '// &
     &         'small: M='//STR(1:I_LEN(STR))//', but needed '//STR1 )
      END IF
!
! --- Reading elements of the matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_ARRAY ( LUN, 'R8', MEL, MAT, NEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9308, IUER, 'MATVIEW_R', 'Error '// &
     &         'during reading elements of the matrix from the file '// &
     &          FINAM(1:I_LEN(FINAM)) )
           RETURN
      END IF
!
! --- Closing input file
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9309, IUER, 'MATVIEW_R', 'Error '// &
     &         'during closing file '//FINAM )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MATVIEW_R  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MATVIEW_RA ( FINAM, MATYP, MV, MH, MAT, ZAG, FORM, IV, IH, &
     &                        IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  MATVIEW_RA  reads from disk file in format MATVIEW which *
! *    has been written earlier by MATWIEW_W for futher exposing.        *
! *                                                                      *
! *  ###   02-JAN-97   MATVIEW_RA   V1.2  (c) L. Petrov  03-AUG-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT     NONE
      CHARACTER    FINAM*(*), ZAG*(*), FORM*(*)
      INTEGER*4    MATYP, MV, MH, IV, IH, IUER
      REAL*8,      POINTER :: MAT(:)
      LOGICAL      LEX
      CHARACTER    STR*20, STR1*20
      INTEGER*4    IER, LUN, IAR(5), IO, LN, MEL, NEL
      INTEGER*4,   EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=FINAM, EXIST=LEX, IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 9731, IUER, 'MATVIEW_RA', 'Wrong file name "'// &
     &          FINAM(1:I_LEN(FINAM))//'". IOSTAT='//STR(1:I_LEN(STR)) )
           RETURN
      END IF
!
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 9732, IUER, 'MATVIEW_RA', 'File "'// &
     &          FINAM(1:I_LEN(FINAM))//'" not found' )
           RETURN
      END IF
!
! --- Openning input file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9733, IUER, 'MATVIEW_RA', 'Error during openning '// &
     &          'input file '//FINAM(1:I_LEN(FINAM)) )
           RETURN
      END IF
!
! --- Reading line - identifier
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_STRING ( LUN, STR, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9734, IUER, 'MATVIEW_RA', 'Error during reading '// &
     &         'from input file '//FINAM(1:I_LEN(FINAM)) )
           RETURN
      END IF
      IF ( STR(1:7) .NE. 'MATView' ) THEN
           CALL ERR_LOG ( 9735, IUER, 'MATVIEW_RA', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' has not been written by MATVIEW_R' )
           RETURN
      END IF
!
! --- Reading header of the matrix
!
      CALL CLRCH ( ZAG )
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_STRING ( LUN, ZAG, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
!
! --- Reading format specification
!
      CALL CLRCH ( FORM )
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_STRING ( LUN, FORM, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
!
! --- Reading 1) type of matrix, 2-3) their deminsion, 4-5) starting point
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_ARRAY ( LUN, 'I4', 5, IAR, NEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
           RETURN
      END IF
      MATYP = IAR(1)
      MV    = IAR(2)
      MH    = IAR(3)
      IV    = IAR(4)
      IH    = IAR(5)
!
      IF ( MATYP .LT. 1 .OR. MATYP .GT. 4 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MATYP, STR )
           CALL ERR_LOG ( 9736, IUER, 'MATVIEW_RA', 'Wrong value of the '// &
     &         'argument MATVIEW: '//STR(1:I_LEN(STR))//' It was expected '// &
     &         '1, 2, 3 or 4' )
           RETURN
      END IF
!
! --- Calculation number of elements
!
      IF ( MATYP .EQ. 1  .OR.  MATYP .EQ. 2 ) THEN
           MEL = MV*MH
         ELSE IF ( MATYP .EQ. 3  .OR.  MATYP .EQ. 4 ) THEN
           MEL = (MV*(MV+1))/2
      END IF
!
      IF ( ASSOCIATED(MAT) ) DEALLOCATE(MAT)
      ALLOCATE ( MAT(MEL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MEL, STR )
           CALL ERR_LOG ( 9738, IUER, 'MATVIEW_RA', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
! --- Reading elements of the matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_ARRAY ( LUN, 'R8', MEL, MAT, NEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9739, IUER, 'MATVIEW_RA', 'Error during reading '// &
     &         'elements of the matrix from the file '//FINAM(1:I_LEN(FINAM)) )
           RETURN
      END IF
!
! --- Closing input file
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9740, IUER, 'MATVIEW_RA', 'Error during closing '// &
     &         'file '//FINAM )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   MATVIEW_RA !#!#
