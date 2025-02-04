        SUBROUTINE RH_TAT ( RAD, ISS, HSTR, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine RH_TAT transform the angle from radians to the text      *
! *   string HH_MM_SS.FFFF in hours, minutes and seconds of time.        *
! *   Parameter ISS definds the number of decimal digits in fractional   *
! *   part.                                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     RAD   ( REAL*8    ) --  Angle in radians.                        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     ISS  ( INTEGER*4  ) -- The number of digits in fractional part.  *
! *                            Range: [0, 16].                           *
! *    HSTR  ( CHARACTER  ) -- Text string of the angle in hours,        *
! *                            minutes and seconds of time.              *
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
! *  ### 05-JUL-1991     RH_TAT    v1.2 (c)  L. Petrov  27-JUN-2009 ###  *
! *                                                                      *
! ************************************************************************
!!        INTEGER*4 BOSTR, HSTR(2)
        CHARACTER  HSTR*(*)
        REAL*8 RAD, RS, RD, RHH, RMM, PI, PI2
        PARAMETER ( PI=3.141592653589793D0 )
        PARAMETER ( PI2=PI*2.D0 )
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_R, LW$STR
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=4 )  !  λομιώεστχο ζοςναμψξωθ παςανετςοχ
!C
!C ----- πςοχεςλα σοοτχετστχιρ λομιώεστχα ζαλτιώεσλικθ ι ζοςναμψξωθ παςαντςοχ
!C
!        NA=NUM$ARG()  !  Mω υϊξαμι λομιώεστχο ζαλτιώεσλιθ παςανετοχ
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   λοξεγ πςοχεςλι  ...
!C
!C
!C ----- πςοχεςλι λοςςελτξοστι χθοδξωθ παςανετςοχ
!C
!        IF ( .NOT. LW$STR ( HSTR  ) ) THEN
!             CALL ERROR_M0 ( 3, IUER, 'RH_TAT', 'ξεδοστυπξα δμρ '//
!     $                                'ϊαπισι χωθοδξαρ στςολα' )
!             RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_R ( 1, 8, RAD  ) ) THEN
!             CALL ERR_LOG ( 4, IUER, 'RH_TAT', 'ξεδοστυπεξ δμρ '//
!     $                               'ώτεξιρ παςανετς RAD' )
!             RETURN
!        END IF
!C
        ISIGN=1
        RD=RAD
        IF( RD.LT.0.D0 ) THEN
            ISIGN=-1
            RD=-1.D0*RD
        END IF
!
        IF ( RD.GE.PI2 ) THEN
            ID=RD/PI2
            RD=RD-ID*PI2
        END IF
!
        RHH=24.D0*RD/(2.D0*PI)
        IH=IDINT(RHH)
        RMM=60.D0*(RHH-IH)
        IM=IDINT(RMM)
        RS=60.D0*(RMM-IM)
        IF ( DABS(RS - 60.0D0) < 0.5*10.0D0**(-ISS) ) THEN
             RS = 0.0
             IM = IM + 1
             IF ( IM .GE. 60 ) THEN
                  IM = 0
                  IH = IH + 1
                  IF ( IH .GE. 24 ) THEN
                       IH = 0
                  END IF
             END IF
        END IF
        IH=IH*ISIGN
!
        CALL PS_TAT ( ISIGN, IH, IM, RS, ISS, HSTR, IUER_PS )
        IF ( IUER_PS.NE.0 ) THEN
             CALL CLRCH ( HSTR )
             CALL ERR_LOG ( IUER_PS, IUER, 'RH_TAT', 'Error of '// &
     &                               'transformation ' )
             RETURN
        END IF
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  RH_TAT  #!#
