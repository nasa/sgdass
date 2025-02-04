        SUBROUTINE HR_TAT ( HSTR, RAD, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine HR_TAT  transforms the input string in format            *
! *   HH_MM_SS.FFFF  from time of seconds to radians.                    *
! *                                                                      *
! * ____________________ Input parameters: _____________________________ *
! *                                                                      *
! *     HSTR  ( CHARACTER )  --  string which contains the angle in      *
! *                              time in seconds in the form             *
! *                              HH_MM_SS.FFFF ( where FFFF fractional   *
! *                              part can have from 0 to 16 characters). *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     RAD   ( REAL*8    ) --  Angle in radians.                        *
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
! *             Error codes:                                             *
! *             ~~~~~~~~~~~~                                             *
! *                                                                      *
! *             IUER=0  --  sucesfull completion;                        *
! *             IUER=1  --  Errror: wrong string format;                 *
! *             IUER=2  --  Error: empty line;                           *
! *                                                                      *
! *     Comment:                                                         *
! *         If IUER=1 or IUER-2 then RAD=-1.1111111111111D11             *
! *                                                                      *
! *  ### 05-JUL-1991     HR_TAT    v1.0 (c)  L. Petrov  17-SEP-2001 ###  *
! *                                                                      *
! ************************************************************************
        CHARACTER HSTR_W*40
!!        INTEGER*4 BOSTR, HSTR(2)
        CHARACTER  HSTR*(*)
        INTEGER*4  IUER_INANG
        REAL*8 RAD, RS, PI, PI12
        PARAMETER ( PI=3.141592653589793D0 )
        PARAMETER ( PI12=PI/12.D0 )
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_W
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=3 )  !  λομιώεστχο ζοςναμψξωθ παςανετςοχ
!C
!C ----- πςοχεςλα σοοτχετστχιρ λομιώεστχα ζαλτιώεσλικθ ι ζοςναμψξωθ παςαντςοχ
!C
!        NA=NUM$ARG()  !  Mω υϊξαμι λομιώεστχο ζαλτιώεσλιθ παςανετοχ
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   λοξεγ πςοχεςλι  ...
!C
!        IB=BOSTR ( HSTR, HSTR_W )
!        IF ( IB.NE.1 ) THEN
!             CALL ERR_LOG ( 3, IUER, 'HR_TAT', 'χθοδξαρ στςολα '//
!     $                               'ξεδοστυπξα δμρ ώτεξιρ ' )
!             RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 8, RAD  ) ) THEN
!             CALL ERR_LOG ( 4, IUER, 'HR_TAT', 'ξεδοστυπεξ δμρ '//
!      $                               'ϊαπισι παςανετς RAD' )
!             RETURN
!        END IF
        CALL CLRCH ( HSTR_W )
        HSTR_W = HSTR
!
! ----- Extraction of subfields from the string στςολι HSTR_W
!
        IUER_INANG = IUER
        CALL INANG_TAT ( HSTR_W, IH, IM, RS, IUER_INANG )
        IF ( IUER_INANG .NE. 0 ) THEN
             RAD=-1.111111111111111D11
             CALL ERR_LOG ( IUER_INANG, IUER, 'HR_TAT', 'Error in decoding '// &
     &           'string '//HSTR_W )
             RETURN
        END IF
!
! ----- Transforming the angle to radians
!
        RAD=(( RS/60.D0 +IM )/60.D0 + IH )*PI12
!
! ----- Set sign
!
        IF ( INDEX ( HSTR_W(1:I_LEN(HSTR_W)), '-' ).NE.0 ) RAD=-1.D0*RAD
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  HR_TAT  #!#
