        SUBROUTINE RG_TAT ( RAD, ISS, GSTR, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine RG_TAT transform the angle from radians to the text      *
! *   string GGG_MM_SS.FFFF in hours, minutes and arc-seconds.           *
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
! *    GSTR  ( CHARACTER  ) -- Text string of the angle in degrees,      *
! *                            and arc-seconds.                          *
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
! *  ### 05-JUL-1991     RG_TAT    v1.2 (c)  L. Petrov  27-JUN-2009 ###  *
! *                                                                      *
! ************************************************************************
!!        INTEGER*4 BOSTR, GSTR(2)
        CHARACTER  GSTR*(*)
        REAL*8    RAD, RS, RD, RG, RM, PI, PI2
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
!C ----- πςοχςελα λοςςελτξοστι χθοδξωθ παςανετςοχ
!C
!        IF ( .NOT. LW$STR ( GSTR  ) ) THEN
!             CALL ERR_LOG ( 3, IUER, 'RG_TAT', 'ξεδοστυπξα δμρ '//
!     $                               'ϊαπισι χωθοδξαρ στςολα' )
!             RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_R ( 1, 8, RAD  ) ) THEN
!             CALL ERR_LOG ( 4, IUER, 'RG_TAT', 'ξεδοστυπεξ δμρ '//
!     $                               'ώτεξιρ παςανετς RAD' )
!             RETURN
!        END IF
!
        ISIGN=1
        RD=RAD
        IF( RD.LT.0.D0 ) THEN
            ISIGN=-1
            RD=-1.D0*RD
        END IF
!
        IF( RD.GE.PI2 ) THEN
            ID=RD/PI2
            RD=RD-ID*PI2
        END IF
!
        RG=RD*360.D0/PI2
        IG=IDINT(RG)
        RM=60.D0*(RG-IG)
        IM=IDINT(RM)
        RS=60.D0*(RM-IM)
        IG=IG*ISIGN
        IF ( DABS(RS - 60.0D0) < 0.5*10**(-ISS) ) THEN
             RS = 0.0
             IM = IM + 1
             IF ( IM .GE. 60 ) THEN
                  IM = 0
                  IG = IG + ISIGN
                  IF ( IG .GE. 360 ) THEN
                       IG = IG - 360
                    ELSE IF ( IG .LE. -360 ) THEN
                       IG = IG + 360
                  END IF
             END IF
        END IF
!
        CALL PS_TAT ( ISIGN, IG, IM, RS, ISS, GSTR, IUER_PS )
        IF ( IUER_PS.NE.0 ) THEN
             CALL CLRCH ( GSTR )
!!             CALL BOSTR ( 'RG_TAT  οϋιβλα.', GSTR )
             CALL ERR_LOG ( IUER_PS, IUER, 'RG_TAT', 'Error of '// &
     &                     'transformation ' )
             RETURN
        END IF
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  RG_TAT  #!#
