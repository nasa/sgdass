        SUBROUTINE REGRW4 ( N, T, D, W, IV, DR, SH, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine REGRW4 computes parameters of linear regression by using *
! *   weighted LSQ for the function D(t) with weights W(t).              *
! *                                                                      *
! *   Array IV consists of 0 and 1 and it marks the points which are     *
! *   used and which are not used. If IV(I)=1 then the I-th point        *
! *   participates in computation of regression, if IV(I)=0 then the     *
! *   I-th point is excluded from  computation.                          *
! *                                                                      *
! *   Regression line is defined by its shift SH and drift DR:           *
! *   y(t) = sh + dr*t                                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  N ( INTEGER*4      ) -- Number of elements of vectors D and T.      *
! *  T ( REAL*4         ) -- Array of arguments of the function.         *
! *                          Dimension: N.                               *
! *  D ( REAL*4         ) -- Array of values of the function.            *
! *                          Dimension: N.                               *
! *  W ( REAL*4,    OPT ) -- Array of weights. If the argument is        *
! *                          omitted then weights 1 are used for all     *
! *                          points.                                     *
! * IV ( INTEGER*4, OPT ) -- Participation vector. consists of 0 and 1.  *
! *                          If IV(I)=1 then the I-th point is taken     *
! *                          into account in computation of regression,  *
! *                          otherwise it is omitted.                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     DR ( REAL*4     ) -- Rate of change of regression.               *
! *     SH ( REAL*4     ) -- Value of the regression at t=T(1).          *
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
! * ###  06-MAY-1991   REGRW4    v1.5 (c) L. Petrov  μ.ΰ. 01-DEC-97 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IUER
        REAL*4    T(N), D(N), W(N), DR, SH
        INTEGER*4 IV(*), J1, NZ, I_LEN
        REAL*4    SD, ST, SDT, TT, STT, DET, DX, DY, WEI, SW
        CHARACTER STR*10
        LOGICAL   W_PRES, IV_PRES
        IF ( LOC(W) .EQ. 0 ) THEN
             W_PRES = .FALSE.
          ELSE
             W_PRES = .TRUE.
        END IF
        IF ( LOC(IV) .EQ. 0 ) THEN
             IV_PRES = .FALSE.
          ELSE
             IV_PRES = .TRUE.
        END IF
!
        SD  = 0.0D0
        SW  = 0.0D0
        ST  = 0.0D0
        SDT = 0.0D0
        STT = 0.0D0
        NZ  = 0
!
! ----- Computation of coeficients of normal system
!
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1).EQ.0 ) GOTO 410
           END IF
           IF ( W_PRES ) THEN
                WEI = W(J1)
              ELSE
                WEI = 1.D0
           END IF
!
           NZ  = NZ+1
           SW  = SW + WEI**2
           SD  = SD + D(J1)*WEI**2
           TT  = T(J1)-T(1)
           ST  = ST  + TT*WEI**2
           SDT = SDT + D(J1)*TT*WEI**2
           STT = STT + TT**2 * WEI**2
  410   CONTINUE
!
! ----- We found too few used elements. 
!
        IF ( NZ.LT.2 ) THEN
             CALL CLRCH ( STR )
             CALL INCH ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'REGRW4', 'Only '// &
     &                      STR(1:I_LEN(STR))//' elements remained' )
             RETURN
        END IF
!
! ----- Compute the determinant of the normal system
!
        DET = SW*STT - ST*ST
!
! ----- Comoputation of minors of the normal system
!
        DX = SW*SDT - SD*ST
        DY = SD*STT - ST*SDT
!
! ----- Determinatnt is too small
!
        IF ( DABS(DET) .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 4, IUER, 'REGRW4', 'Internal error: '// &
     &                      'zero determinant' )
             RETURN
        END IF
!
! ----- Solve normal system
!
        DR = DX/DET
        SH = DY/DET
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  REGRW4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE DESIG_TREL4 ( N, T, X, IV, ED_I, NZ, DR, SH, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DESIG_TREL4 computes linear regression using iterative  *
! *   LSQ  with outliers elimiation. At each step of iterations it       *
! *   checks whether at least one the residual exceeds ED_I times the    *
! *   standard deviation of residuals. If not, then iterations are       *
! *   stopped. If yes, then the point with the largest residual is       *
! *   marked as an outlier and the iteration is repeated.                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     N ( INTEGER*4 ) -- Number of elements of vectors D and T.        *
! *     T ( REAL*4    ) -- Array of arguments of the function.           *
! *                        Dimension: N.                                 *
! *     D ( REAL*4    ) -- Array of values of the function.              *
! *                        Dimension: N.                                 *
! *     W ( REAL*4    ) -- Array of weights. If the argument is          *
! *                        omitted then weights 1 are used for all       *
! *                        points.                                       *
! *                                                                      *
! *  ED_I ( REAL*4    ) -- The threshold of number of standard           *
! *                        deviations that if exceeded, an observation   *
! *                        is marked as an outlier.                      *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! * IV ( INTEGER*4, OPT ) -- Participation vector. consists of 0 and 1.  *
! *                          If IV(I)=1 then the I-th point is taken     *
! *                          into account in computation of regression,  *
! *                          otherwise it is omitted.                    *
! *                                                                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     DR ( REAL*4     ) -- Rate of change of regression.               *
! *     SH ( REAL*4     ) -- Value of the regression at t=T(1).          *
! *     NZ  ( INTEGER*4 ) -- The number of remained points.              *
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
! *  ###  06-MAY-1991     DESIG4  v1.4  (c) L. Petrov  08-AUG-1994  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        REAL*4  T(N), X(N), AV, D, VALKR, DR, SH
        INTEGER*4  IER
        INTEGER IV(N)
!
!        NA=NUM$ARG()
!        IF ( NA.LT.5 .AND. NA.GT.7 ) CALL VER$ARG ( 7 )
!
! ----- Compute regression coefficients SH and DR
!
        CALL ERR_PASS ( IUER, IER )
        CALL REGRW4 ( N, T, X, %VAL(0), IV, DR, SH, IER )
        IF ( IER.GT.0 ) THEN
             CALL ERR_LOG ( 1611, IUER, 'DESIG4', 'Error in computing '// &
     &                     'regression' )
             RETURN
        END IF
!
! ----- Compute 
! ----- AV -- the average
! ----- D  --  the dispersion 
!
        CALL ERR_PASS ( IUER, IER )
        CALL DISP_TR4 ( N, T, X, DR, SH, IV, AV, D, NZ, IER )
        IF ( IER.GT.0 ) THEN
             CALL ERR_LOG ( 1612, IUER, 'DESIG4', 'Error in computing '// &
     &                     'dispersion' )
             RETURN
        END IF
!
! ----- ϊαδαξιε  ED  --  ώισμο σταξδαςτξωθ οτλμοξεξικ οτξοσιτεμψξο
! ----- ςεηςεσσιοξξοκ πςρνοκ, βομψϋε λοτοςωθ σμεδυεT όμενεξτ οτνεώατψ λαλ
! ----- χωβςοσ
!
!        IF ( NA.EQ.5 ) THEN
!             TYPE 110
!  110        FORMAT(1X/2X,'???  ισλμΰώιτψ χωβςοσω, λοτοςωε πςεχωϋαΰτ'/
!     *       2X,'σλομψλο σςεδξελχαδςατιώξωθ οτλμοξεξικ  <3.>  ? '$)
!             ACCEPT 120,IQ,ED
!  120        FORMAT(Q,G15.7)
!             IF( IQ.EQ.0 ) ED=3.   !  πο υνομώαξιΰ  ED=3
!          ELSE
!             ED=ED_I
!             IF ( ED .LT. 1.D-5 ) ED=3.
!        END IF
         ED=ED_I
         IF ( ED .LT. 1.D-5 ) ED=3.0
!
! ##### Begin of iterative sycle
!
  610   IDEL=0
        VALKR=ED*D
        DO 410 J1=1,N
           IF( IV(J1) .EQ. 0 ) GOTO 410
!
! -------- IDEL  --  The number of excluded elenets at this step of the cycle
!
           TT=T(J1)-T(1)
           IF( DABS( (X(J1)-AV)-(DR*TT+SH) ) .GT. VALKR ) IDEL=IDEL+1
           IF( DABS( (X(J1)-AV)-(DR*TT+SH) ) .GT. VALKR ) IV(J1)=0
  410   CONTINUE
!
! ----- Once again compute shift and drift
!
        CALL ERR_PASS ( IUER, IER )
        CALL REGRW4 ( N, T, X, %VAL(0), IV, DR, SH, IER )
        IF ( IER.GT.0 ) THEN
             CALL ERR_LOG ( 1613, IUER, 'DESIG4', 'Error in computing '// &
     &                     'regression' )
             RETURN
        END IF
        CALL ERR_PASS ( IUER, IER )
        CALL DISP_TR4 ( N, T, X, DR, SH, IV, AV, D, NZ, IER )
        IF ( IER.GT.0 ) THEN
             CALL ERR_LOG ( 1614, IUER, 'DESIG4', 'Error in computing '// &
     &                     'dispersion' )
             RETURN
        END IF
!
! ----- If there are no excluded elements, stop iterations here
!
        IF ( IDEL .EQ. 0 ) THEN
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
        GOTO 610
!
! ##### End of the iteration cycle
!
        END  !#!  DESIG4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE REGRV_EL4 ( N, T, D, IV, IV_EL, A, B, IUER )
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαννα χωώισμεξιρ υημοχοηο λοόζζιγιεξτα ι σχοβοδξοηο ώμεξα *
! *     ςεηςεσσιοξξοκ πςρνοκ ζυξλγιι D(T) .                              *
! *                                                                      *
! *     ισπομψϊυετσρ χελτος υώαστιρ, λοτοςωκ οτνεώαετ όμενεξτω, λοτοςωε  *
! *     βυδυτ ςασσνατςιχατψσρ. χ οβςαβοτλυ βυδυτ χλμΰώατψσρ τομψλο τε    *
! *     όμενεξτω, δμρ λοτοςωθ IV(I)=IV_EL .                              *
! *                                                                      *
! *                                                                      *
! * ------------------------- χθοδξωε παςνετςω: ------------------------ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  ώισμο όμενεξτοχ υ χελτοςοχ  D  ι  T .   *
! *        T  ( REAL*4    )  --  χελτος αςηυνεξτοχ ζυξλγιι D .           *
! *        D  ( REAL*4    )  --  χελτος ϊξαώεξικ ζυξλγιι, δμρ λοτοςοκ    *
! *                              χωώισμρετσρ δισπεςσιρ ι σςεδξεε.        *
! *       IV  ( INTEGER*4 )  --  χελτος υώαστιρ.                         *
! *     IV_EL ( INTEGER*4 )  --  ϊξαώεξιε, λοτοςοε υλαϊωχαετ ξα          *
! *                              ισπομψϊυενωκ όμενεξτ.
! *                                                                      *
! * ------------------------- χωθοδξωε παςνετςω: ----------------------- *
! *                                                                      *
! *        A  ( REAL*4    )  --  υημοχοκ λοόζζιγιεξτ τςεξδα.             *
! *        B  ( REAL*4    )  --  σχοβοδξωκ ώμεξ τςεξδα.                  *
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
! *                 ϊαχεςϋεξιρ οβςαϊα χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.     *
! *      εσμι IUER οπυύεξ, το χθοδξοε ϊξαώεξιε πςιξιναετσρ ςαχξων -1     *
! *             χωθοδξοε ϊξαώεξιε  --  λοδ οϋιβλι ( εσμι IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             δοστυπεξ δμρ ϊαπισι ):                                   *
! *      IUER=0  --  ξοςναμψξοε ϊαχεςϋεξιε.                              *
! *      IUER=1  --  παςανετς  A  ξεδοστυπεξ δμρ ϊαπισι.                 *
! *      IUER=2  --  παςανετς  B  ξεδοστυπεξ δμρ ϊαπισι.                 *
! *      IUER=3  --  όμενεξτοχ, πο λοτοςων στςοιτσρ ςεηςεσσιοξξαρ πςρναρ *
! *                  νεξψϋε 2 .                                          *
! *      IUER=4  --  χξυτςεξξρρ οϋιβλα: οπςεδεμιτεμψ πο νοδυμΰ νεξψϋε    *
! *                  1.D-30.                                             *
! *                                                                      *
! *  ###  13-JUL-91   REGRV_EL4   V1.4  (c) πΕΤÒΟΧ μ.ΰ.  08-AUG-94  ###  *
! *                                                                      *
! ************************************************************************
        REAL*4    T(N), D(N), A, B
        INTEGER*4 IV(N), IV_EL
        REAL*4    SD, ST, SDT, STT, DET, DX, DY
        CHARACTER STR*10
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_W, PROBE_R, IV_PRES
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=8 )  !  λομιώεστχο ζοςναμψξωθ παςανετςοχ
!C
!C ----- πςοχεςλα σοοτχετστχιρ λομιώεστχα ζαλτιώεσλικθ ι ζοςναμψξωθ παςαντςοχ
!C
!        NA=NUM$ARG()  !  Mω υϊξαμι λομιώεστχο ζαλτιώεσλιθ παςανετοχ
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   λοξεγ πςοχεςλι  ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, A ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'REGRV_EL4', 'παςανετς A '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, B ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'REGRV_EL4', 'παςανετς B '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_R ( N, 4, IV ) ) THEN
!              CALL ERR_LOG ( 5, IUER, 'REGRV_EL4', 'παςανετς IV '//
!     $                                'ξεδοστυπεξ δμρ ώτεξιρ' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_R ( 1, 4, IV_EL ) ) THEN
!              CALL ERR_LOG ( 6, IUER, 'REGRV_EL4', 'παςανετς IV_EL '//
!     $                                'ξεδοστυπεξ δμρ ώτεξιρ' )
!              RETURN
!        END IF
!
        SD= 0.0D0
        ST= 0.0D0
        SDT=0.0D0
        STT=0.0D0
        NZ=0
!
! ----- χωώισμεξιε λοόζζιγιεξτοχ ξοςναμψξοκ σιστενω υςαχξεξικ
!
        DO 410 J1=1,N
           IF ( IV(J1).EQ.IV_EL ) THEN
                NZ=NZ+1
                SD=SD+D(J1)
                TT=T(J1)-T(1)
                ST=ST+TT
                SDT=SDT+D(J1)*TT
                STT=STT+TT**2
           END IF
  410   CONTINUE
!
! ----- ολαϊαμοσψ, ώτο ώισμο όμενεξτοχ, υώαστχυΰύιθ χ ποδσώεταθ, σμιϋλον ναμο
!
        IF ( NZ.LT.2 ) THEN
             CALL CLRCH ( STR )
             CALL INCH ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'REGRV_EL4', 'υώαστχυετ τομψλο '// &
     &                      STR(1:I_LEN(STR))//' όμενεξτοχ' )
             RETURN
        END IF
!
! ----- χωώισμεξιε οπςεδεμιτεμρ ξοςναμψξοκ σιστενω
!
        DET=NZ*STT-ST*ST
!
! ----- χωώισμεξιε νιξοςοχ νατςιγω ξοςναμψξοκ σιστενω
!
        DX=NZ*SDT-SD*ST
        DY=SD*STT-ST*SDT
!
! ----- οπεςεδεμιτεμψ σιστενω σμιϋλον ναμ
!
        IF ( DABS(DET) .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 4, IUER, 'REGRV_EL4', 'χξυτςεξξρρ οϋιβλα: '// &
     &                      'ξυμεχοκ οπςεδεμιτεμψ' )
             RETURN
        END IF
!
! ----- ςεϋεξιε ξοςναμψξοκ σιστενω υςαχξεξικ
!
        A=DX/DET
        B=DY/DET
        CALL ERR_LOG ( 0, IUER )
        END  !#!  REGRV_EL4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE WDISP4 ( N, X, W, IV, AV, D, DW, DSW, NZ, IUER )
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαννα WDISP4 χωώισμρετ λοςεξψ λχαδςατξωκ ιϊ ξεχϊχεϋεξξοκ  *
! *     δισπεςσιι -- D, λοςεξψ λχαδςατξωκ ιϊ χϊχεϋεξξοκ δισπεςσιι,       *
! *     ϋλαμιςοχαξξοκ ξα σςεδξεε ϊξαώεξιε  - DW, λοςεξψ λχαδςατξωκ ιϊ    *
! *     χϊχεϋεξξοκ δισπεςσιι - DSW, σςεδξεε  -- AV  ι ώισμο              *
! *     ισπομψϊοχαξξωθ οτσώετοχ  --  NZ  δμρ  νασσιχα  X  δμιξοκ N.      *
! *     W  --  νασσιχ χεσοχ χεμιώιξω X.                                  *
! *     ισπομψϊυετσρ νασσιχ  IV, σοστορύικ ιϊ  0 ιμι 1, λοτοςωκ οτνεώαετ *
! *     ξεισπομψϊοχαξξωε οτσώετω. εσμι  IV(I)=1, το  I-τωκ οτσώετ        *
! *     υώαστχυετ χ οβςαϊοχαξιι σςεδξεηο ι δισπεςσιι, εσμι  IV(J)=0, το  *
! *     J-τωκ όμενεξτ ισλμΰώαετσρ ιϊ ποδσώετοχ.                          *
! *                                                                      *
! * ------------------------- χθοδξωε παςνετςω: ------------------------ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  ώισμο όμενεξτοχ υ χελτοςα  X .          *
! *        X  ( REAL*4    )  --  χελτος, δμρ λοτοςοηο σώιταετσρ          *
! *                              δισπεςσιρ ι σςεδξεε.                    *
! *        W  ( REAL*4    )  --  χελτος χεσοχ χεμιώιξω X.                *
! *       IV  ( INTEGER*4, OPT )  --  χελτος, υλαϊωχαΰύικ ξα ισλμΰώ‚ξξωε *
! *                              ιϊ ποδσώετοχ όμενεξτω.                  *
! *                              εσμι παςανετς  IV  οπυύεξ, ισπομψϊυΰτσρ *
! *                              χσε όμενεξτω.                           *
! *                                                                      *
! * ------------------------- χωθοδξωε παςνετςω: ----------------------- *
! *                                                                      *
! *       AV  ( REAL*4    )  --  σςεδξεε ιϊ NZ όμενεξτοχ νασσιχα X .     *
! *        D  ( REAL*4    )  --  R.M.S. δμρ NZ όμενεξτοχ νασσιχα X .     *
! *       DW  ( REAL*4    )  --  ϋλαμιςοχαξξαρ ξα σςεδξεε ϊξαώεξιε       *
! *                              χεσα W.R.M.S. δμρ NZ όμενεξτοχ νασσιχα X*
! *      DSW  ( REAL*4    )  --  W.R.M.S. δμρ NZ όμενεξτοχ νασσιχα X .   *
! *       NZ  ( INTEGER*4, OPT )  --  ώισμο όμενεξτοχ, λοτοςωε χ         *
! *                              χ υώαστχοχοαμι ποδσώετε.                *
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
! *                 ϊαχεςϋεξιρ οβςαϊα χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.     *
! *      εσμι IUER οπυύεξ, το χθοδξοε ϊξαώεξιε πςιξιναετσρ ςαχξων -1     *
! *             χωθοδξοε ϊξαώεξιε  --  λοδ οϋιβλι ( εσμι IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             δοστυπεξ δμρ ϊαπισι ):                                   *
! *      IUER=0  --  ξοςναμψξοε ϊαχεςϋεξιε.                              *
! *      IUER=1  --  παςανετς  A  ξεδοστυπεξ δμρ ϊαπισι.                 *
! *      IUER=2  --  παςανετς  D  ξεδοστυπεξ δμρ ϊαπισι.                 *
! *      IUER=3  --  όμενεξτοχ, πο λοτοςων χωώισμρετσρ δισπεςσιρ,        *
! *                  νεξψϋε 2 .                                          *
! *      IUER=4  --  παςανετς  DW ξεδοστυπεξ δμρ ϊαπισι.                 *
! *                                                                      *
! *  ###  04-JUN-93     WDISP4    V2.6  (c) πΕΤÒΟΧ μ.ΰ.  20-OCT-95  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER    N, IV(N), NZ, IUER
        REAL*4     X(N), W(N), D, AV, DW, DSW, WW
        CHARACTER  STR*10
        INTEGER*4  J1, J2, NZZ
        LOGICAL*4  IV_PRES
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! .....................\\\
!                       \\\
!        LOGICAL   PRESENT, PROBE_W, PROBE_R, IV_PRES
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=10 )  !  λομιώεστχο ζοςναμψξωθ παςανετςοχ
!C
!C ----- πςοχεςλα σοοτχετστχιρ λομιώεστχα ζαλτιώεσλικθ ι ζοςναμψξωθ παςαντςοχ
!C
!        NA=NUM$ARG()  !  Mω υϊξαμι λομιώεστχο ζαλτιώεσλιθ παςανετοχ
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   λοξεγ πςοχεςλι  ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'WDISP4', 'παςανετς AV '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'WDISP4', 'παςανετς D '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, DW ) ) THEN
!              CALL ERR_LOG ( 3, IUER, 'WDISP4', 'παςανετς DW '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, DSW ) ) THEN
!              CALL ERR_LOG ( 4, IUER, 'WDISP4', 'παςανετς DSW '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!
!        IF ( PROBE_R ( N, 4, IV ) ) THEN
!             IV_PRES=.TRUE.
!          ELSE
!             IV_PRES=.FALSE.
!        END IF
!
        IF ( LOC(IV) .EQ. 0 ) THEN
             IV_PRES = .FALSE.
          ELSE
             IV_PRES = .TRUE.
        END IF
!
! ----- ξαώαμψξωε οβξυμεξιρ
!
        AV=0.D0
        NZZ=0
!
! ----- χωώισμεξιε σςεδξεηο
!
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1).EQ.0 ) GOTO 410
           END IF
           NZZ=NZZ+1             !  NZZ  --  ώισμο υώαστχοχαχϋιθ
!                                !           χ ποδσώεταθ όμενεξτοχ
           AV=AV+X(J1)
 410   CONTINUE
!
! ----- ολαϊαμοσψ, ώτο ώισμο όμενεξτοχ, υώαστχυΰύιθ χ ποδσώεταθ, σμιϋλον ναμο
!
        IF ( NZZ.LT.2 ) THEN
             CALL CLRCH   ( STR )
             CALL INCH    ( NZZ, STR )
             CALL ERR_LOG ( 3, IUER, 'WDISP4', 'υώαστχυετ τομψλο '// &
     &                      STR(1:I_LEN(STR))//' όμενεξτοχ' )
!!             IF ( PRESENT ( NZ, 8 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZ=NZZ
             IF ( LOC(NZ) .NE. 0 ) NZ = NZZ
             RETURN
        END IF
        AV=AV/NZZ
!
! ----- χωώισμεξιε δισπεςσιι
!
        D  = 0.0D0
        DW = 0.0D0
        WW = 0.0D0
        DO 420 J2=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J2).EQ.0 ) GOTO 420
           END IF
           D=D   + ( X(J2)-AV )**2
           DW=DW + ( W(J2)* (X(J2)-AV) )**2
           WW=WW +   W(J2)**2
  420   CONTINUE
!
        IF ( DABS(WW) .LT. 1.D-30 ) THEN
             WRITE ( 6,  * ) ' WW=',WW,' NZZ=',NZZ
             CALL ERR_LOG ( 4, IUER, 'WDISP', 'ισπομψϊοχαξξωε '// &
     &                      'χεσα ολαϊαμισψ ξυμεχωνι' )
             RETURN
        END IF
!
! ----- πςοχςελα: δοστατοώξοε μι ώισμο όμενεξτοχ ?
!
        D   = DSQRT ( D/(NZZ-1) )
        DSW = DSQRT ( DW/NZZ    )
        DW  = DSQRT ( DW/WW     )
!!        IF ( PRESENT ( NZ, 8 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZ=NZZ
        IF ( LOC(NZ) .NE. 0 ) NZ=NZZ
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  WDISP4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE WDISP_EL4 ( N, X, W, IV, IV_EL, AV, D, DW, DSW, &
     &                         CHI, NZ, IUER )
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαννα  WDISP_EL4  χωώισμρετ λοςεξψ λχαδςατξωκ ιϊ          *
! *     ξεχϊχεϋεξξοκ δισπεςσιι -- D, λοςεξψ λχαδςατξωκ ιϊ χϊχεϋεξξοκ     *
! *     δισπεςσιι, ϋλαμιςοχαξξοκ ξα σςεδξεε ϊξαώεξιε  - DW, λοςεξψ       *
! *     λχαδςατξωκ ιϊ χϊχεϋεξξοκ δισπεςσιι - DSW, σςεδξεε  -- AV  ι      *
! *     ώισμο ισπομψϊοχαξξωθ οτσώετοχ  --  NZ  δμρ  νασσιχα  X δμιξοκ N. *
! *     W  --  νασσιχ χεσοχ χεμιώιξω X.                                  *
! *     ισπομψϊυετσρ νασσιχ  IV, λοτοςωκ οτνεώαετ ισπομψϊυενωε οτσώετω.  *
! *     πςι ότον ισπομψϊυετσρ τομψλο τε οτσώ‚τω  X(I), δμρ λοτοςωθ       *
! *     IV(I)=IV_EL.                                                     *
! *                                                                      *
! * ------------------------- χθοδξωε παςνετςω: ------------------------ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  ώισμο όμενεξτοχ υ χελτοςα  X .          *
! *        X  ( REAL*4    )  --  χελτος, δμρ λοτοςοηο σώιταετσρ          *
! *                              δισπεςσιρ ι σςεδξεε.                    *
! *        W  ( REAL*4    )  --  χελτος χεσοχ χεμιώιξω X.                *
! *       IV  ( INTEGER*4, OPT )  --  χελτος, υλαϊωχαΰύικ ξα ισλμΰώεξξωε *
! *                              ιϊ ποδσώετοχ όμενεξτω.                  *
! *                              εσμι παςανετς  IV  οπυύεξ, ισπομψϊυΰτσρ *
! *                              χσε όμενεξτω.                           *
! *     IV_EL ( INTEGER*4, OPT ) --   ϊξαώεξιε, υλαϊωχαΰύεε ξα           *
! *                              ισπομψϊυενωκ οτσώ‚τ.                    *
! *                                                                      *
! * ------------------------- χωθοδξωε παςνετςω: ----------------------- *
! *                                                                      *
! *       AV  ( REAL*4    )  --  σςεδξεε ιϊ NZ όμενεξτοχ νασσιχα X .     *
! *        D  ( REAL*4    )  --  R.M.S. δμρ NZ όμενεξτοχ νασσιχα X .     *
! *       DW  ( REAL*4    )  --  ϋλαμιςοχαξξαρ ξα σςεδξεε ϊξαώεξιε       *
! *                              χεσα W.R.M.S. δμρ NZ όμενεξτοχ νασσιχα X*
! *      DSW  ( REAL*4    )  --  W.R.M.S. δμρ NZ όμενεξτοχ νασσιχα X .   *
! *      CHI  ( REAL*4    )  --  θι-λχαδςατ ξα στεπεξψ σχοβοδω.          *
! *       NZ  ( INTEGER*4 )  --  ώισμο όμενεξτοχ, λοτοςωε υώαστχοχαμι χ  *
! *                              ποδσώετε.                               *
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
! *                 ϊαχεςϋεξιρ οβςαϊα χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.     *
! *      εσμι IUER οπυύεξ, το χθοδξοε ϊξαώεξιε πςιξιναετσρ ςαχξων -1     *
! *             χωθοδξοε ϊξαώεξιε  --  λοδ οϋιβλι ( εσμι IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             δοστυπεξ δμρ ϊαπισι ):                                   *
! *      IUER=0  --  ξοςναμψξοε ϊαχεςϋεξιε.                              *
! *      IUER=1  --  παςανετς  A  ξεδοστυπεξ δμρ ϊαπισι.                 *
! *      IUER=2  --  παςανετς  B  ξεδοστυπεξ δμρ ϊαπισι.                 *
! *      IUER=3  --  όμενεξτοχ, πο λοτοςων χωώισμρετσρ δισπεςσιρ,        *
! *                  νεξψϋε 2 .                                          *
! *                                                                      *
! *  ###  21-JUN-94   WDISP_EL4   V1.6  (c) πΕΤÒΟΧ μ.ΰ.  20-OCT-95  ###  *
! *                                                                      *
! ************************************************************************
        REAL*4      X(N), W(N), D, AV, DW, DSW, WW, CHI
        INTEGER     IV(N)
        CHARACTER   STR*10
        LOGICAL*4   IV_PRES
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_W, PROBE_R, IV_PRES
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=12 )  !  λομιώεστχο ζοςναμψξωθ παςανετςοχ
!C
!C ----- πςοχεςλα σοοτχετστχιρ λομιώεστχα ζαλτιώεσλικθ ι ζοςναμψξωθ παςαντςοχ
!C
!        NA=NUM$ARG()  !  Mω υϊξαμι λομιώεστχο ζαλτιώεσλιθ παςανετοχ
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   λοξεγ πςοχεςλι  ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'WDISP_EL4', 'παςανετς AV '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'WDISP_EL4', 'παςανετς D '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, DW ) ) THEN
!              CALL ERR_LOG ( 4, IUER, 'WDISP_EL4', 'παςανετς DW '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, DW ) ) THEN
!              CALL ERR_LOG ( 4, IUER, 'WDISP_EL4', 'παςανετς DSW '//
!     $                                'ξεδοστυπεξ δμρ ϊαπισι' )
!              RETURN
!        END IF
!C
!        IF ( PROBE_R ( N, 4, IV ) ) THEN
!             IV_PRES=.TRUE.
!          ELSE
!             IV_PRES=.FALSE.
!        END IF
        IF ( LOC(IV) .EQ. 0 ) THEN
             IV_PRES = .FALSE.
          ELSE
             IV_PRES = .TRUE.
        END IF
!
! ----- ξαώαμψξωε οβξυμεξιρ
!
        AV=0.D0
        D =0.D0
        DW=0.D0
        WW=0.D0
        NZ=0
!
! ----- χωώισμεξιε σςεδξεηο
!
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1).EQ.0 ) GOTO 410
                IF ( IV(J1).NE.IV_EL ) GOTO 410
           END IF
           NZ=NZ+1             !  NZ  --  ώισμο υώαστχοχαχϋιθ
!                              !          χ ποδσώεταθ όμενεξτοχ
           AV=AV + X(J1)*W(J1)
           WW=WW + W(J1)
  410   CONTINUE
!
! ----- ολαϊαμοσψ, ώτο ώισμο όμενεξτοχ, υώαστχυΰύιθ χ ποδσώεταθ, σμιϋλον ναμο
!
        IF ( NZ.LT.2 ) THEN
             CALL CLRCH   ( STR )
             CALL INCH    ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'WDISP_EL4', 'υώαστχυετ τομψλο '// &
     &                      STR(1:I_LEN(STR))//' όμενεξτοχ' )
             RETURN
        END IF
!
        IF ( DABS(WW) .LT. 1.D-30 ) THEN
             WRITE ( 6, * ) ' WW=',WW, ' NZ=',NZ
             CALL ERR_LOG ( 4, IUER, 'WDISP_EL4', 'ισπομψϊοχαξξωε '// &
     &                      'χεσα ολαϊαμισψ ξυμεχωνι' )
             RETURN
        END IF
        AV=AV/WW
        WW=0.
!
! ----- χωώισμεξιε δισπεςσιι
!
        DO 420 J2=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J2).EQ.0 ) GOTO 420
                IF ( IV(J2).NE.IV_EL ) GOTO 420
           END IF
           D=D + ( X(J2)-AV )**2
           DW=DW + ( W(J2)* ( X(J2)-AV )) **2
           WW=WW +   W(J2)**2
  420   CONTINUE
!
! ----- πςοχεςλα: δοστατοώξοε μι ώισμο όμενεξτοχ ?
!
        D   = DSQRT ( D/(NZ-1) )
        CHI = DW
        DSW = DSQRT ( DW/NZ    )
        DW  = DSQRT ( DW/WW     )
        CALL ERR_LOG ( 0, IUER )
!
        RETURN
        END  !#!  WDISP_EL4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE REGR4 ( N, T, D, DR, SH, IUER )
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαννα χωώισμεξιρ υημοχοηο λοόζζιγιεξτα ι σχοβοδξοηο ώμεξα *
! *     ςεηςεσσιοξξοκ πςρνοκ ζυξλγιι D(T) .                              *
! *                                                                      *
! *     ισπομψϊυετσρ νασσιχ  IV, σοστορύικ ιϊ  0 ιμι 1 , λοτοςωκ         *
! *     οτνεώαετ ξεισπομψϊοχαξξωε οτσώετω. εσμι  IV(I)=1, το  I-τωκ      *
! *     οτσώετ υώαστχυετ χ χωώισμεξιι παςανετςοχ ςεηςεσσιοξξοκ πςρνοκ.   *
! *     εσμι  IV(J)=0, το J-τωκ όμενεξτ ισλμΰώαετσρ ιϊ ποδσώετοχ.        *
! *                                                                      *
! *                                                                      *
! * ------------------------- χθοδξωε παςνετςω: ------------------------ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  ώισμο όμενεξτοχ υ χελτοςοχ  D  ι  T .   *
! *        T  ( REAL*4    )  --  χελτος αςηυνεξτοχ ζυξλγιι D .           *
! *        D  ( REAL*4    )  --  χελτος ϊξαώεξικ ζυξλγιι, δμρ λοτοςοκ    *
! *                              χωώισμρετσρ δισπεςσιρ ι σςεδξεε.        *
! *                                                                      *
! * ------------------------- χωθοδξωε παςνετςω: ----------------------- *
! *                                                                      *
! *       DR  ( REAL*4    )  --  σχοβοδξωκ ώμεξ τςεξδα.                  *
! *       SH  ( REAL*4    )  --  υημοχοκ λοόζζιγιεξτ τςεξδα.             *
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
! *                 ϊαχεςϋεξιρ οβςαϊα χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.     *
! *      εσμι IUER οπυύεξ, το χθοδξοε ϊξαώεξιε πςιξιναετσρ ςαχξων -1     *
! *             χωθοδξοε ϊξαώεξιε  --  λοδ οϋιβλι ( εσμι IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             δοστυπεξ δμρ ϊαπισι ):                                   *
! *      IUER=0  --  ξοςναμψξοε ϊαχεςϋεξιε.                              *
! *      IUER=1  --  παςανετς  DR  ξεδοστυπεξ δμρ ϊαπισι.                *
! *      IUER=2  --  παςανετς  SH  ξεδοστυπεξ δμρ ϊαπισι.                *
! *      IUER=3  --  όμενεξτοχ, πο λοτοςων στςοιτσρ ςεηςεσσιοξξαρ πςρναρ *
! *                  νεξψϋε 2 .                                          *
! *      IUER=4  --  χξυτςεξξρρ οϋιβλα: οπςεδεμιτεμψ πο νοδυμΰ νεξψϋε    *
! *                  1.D-30.                                             *
! *                                                                      *
! *  ###  06-MAY-1991     REGR4  v1.6  (c) πΕΤÒΟΧ μ.ΰ.  26-FEB-2008 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IUER
        REAL*4    T(N), D(N), DR, SH
        INTEGER*4 J1
        REAL*4    SD, ST, SDT, TT, STT, DET, DX, DY, SW
        CHARACTER STR*10
!
        SD  = 0.0D0
        SW  = 0.0D0
        ST  = 0.0D0
        SDT = 0.0D0
        STT = 0.0D0
!
! ----- χωώισμεξιε λοόζζιγιεξτοχ ξοςναμψξοκ σιστενω υςαχξεξικ
!
        DO 410 J1=1,N
           SW  = SW + 1.0
           SD  = SD + D(J1)
           TT  = T(J1)-T(1)
           ST  = ST  + TT
           SDT = SDT + D(J1)*TT
           STT = STT + TT**2
  410   CONTINUE
!
! ----- χωώισμεξιε οπςεδεμιτεμρ ξοςναμψξοκ σιστενω
!
        DET = SW*STT - ST*ST
!
! ----- χωώισμεξιε νιξοςοχ νατςιγω ξοςναμψξοκ σιστενω
!
        DX = SW*SDT - SD*ST
        DY = SD*STT - ST*SDT
!
! ----- οπεςεδεμιτεμψ σιστενω σμιϋλον ναμ
!
        IF ( DABS(DET) .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 1014, IUER, 'REGR4', 'Zero determinant' )
             RETURN
        END IF
!
! ----- ςεϋεξιε ξοςναμψξοκ σιστενω υςαχξεξικ
!
        DR = DX/DET
        SH = DY/DET
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  SUBROUTINE  REGR4  !#!#
