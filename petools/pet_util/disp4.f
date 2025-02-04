        SUBROUTINE DISP4 ( N, X, IV, AV, D, NZ, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DISP4  computes the following quantities:               *
! *         AV -- arithmetic average,                                    *
! *         D  -- dispersion (rms);                                      *
! *         NZ -- the number of used potins                              *
! *   over the real*4 array X of length N. Array IV indicates which      *
! *   points are to be used: if IV(i) = 1 thern the X(i) element is used *
! *   for computation of the avarage and dispersion, if IV(k) = 0 then   *
! *   the k-th element is not counted for.                               *
! *                                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N  ( INTEGER*4 ) -- The numer of elements of vector X.            *
! *    X  ( REAL*4    ) -- The vector under investigation. Dimension: N. *
! *   IV  ( INTEGER*4, OPT )  --  Accompanied partipipation vector.      *
! *                               If IV  is omitted then all elements    *
! *                               are used.                              *
! *                                                                      *
! * ________________________ Ouput parameters: _________________________ *
! *                                                                      *
! *   AV  ( REAL*4    )  --  Average over NZ elements of the array X.    *
! *    D  ( REAL*4    )  --  Dispersion (rms) over NZ elements of the    *
! *                          input array X.                              *
! *   NZ  ( INTEGER*4, OPT )  -- The number of used elements.            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
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
! * ###  26-JUL-1989     DISP4     V1.4  (c) L. Petorv 08-AUG-1994  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER    N, IV(N), NZ, IUER
        REAL*4     X(N), D, AV
        INTEGER*4  NZZ, J1, J2
        CHARACTER  STR*10
        LOGICAL    IV_PRES
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_W, PROBE_R, IV_PRES
!        INTEGER*4 NUM$ARG, NA, N_ARG
!
!        PARAMETER ( N_ARG=7 )  !  The number of formal parameters
!C
!C ----- Check correspondence of the actual and formal arguments
!C
!        NA=NUM$ARG()  !  We learned the number of actual parameters
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   end of check ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'DISP4', 'Argument AV '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'DISP4', 'Argument D '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( PROBE_R ( N, 4, IV ) ) THEN
!             IV_PRES=.TRUE.
!          ELSE
!             IV_PRES=.FALSE.
!        END IF
!
! ----- Learn: if IV present?
!
        IF ( LOC(IV) .EQ. 0 ) THEN
             IV_PRES = .FALSE.
          ELSE
             IV_PRES = .TRUE.
        END IF
!
! ----- Initiallization
!
        AV=0.D0
        D=0.D0
        NZZ=0
!
! ----- Compute the average
!
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1).EQ.0 ) GOTO 410
           END IF
           NZZ=NZZ+1             !  NZZ  --  The number of used elements
!                                !
           AV=AV+X(J1)
  410   CONTINUE
!
! ----- Too few used elements
!
        IF ( NZZ.LT.2 ) THEN
             CALL CLRCH   ( STR )
             CALL INCH    ( NZZ, STR )
             CALL ERR_LOG ( 3, IUER, 'DISP4', 'The number if used elements '// &
     &                     'is too small: '//STR(1:I_LEN(STR)) )
!!             IF ( PRESENT ( NZ, 6 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZZ=NZ
             IF ( LOC(NZ) .GT. 0 ) NZZ=NZ
             RETURN
        END IF
        AV=AV/NZZ
!
! ----- Compute rms
!
        DO 420 J2=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J2).EQ.0 ) GOTO 420
           END IF
           D = D + ( X(J2)-AV )**2
  420   CONTINUE
!
! ----- Check: is the number of elements sufficient?
!
        D = DSQRT( D/(NZZ-1) )
!!        IF ( PRESENT ( NZ, 6 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZ=NZZ
        IF ( LOC(NZ) .NE. 0 ) NZ=NZZ
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  DISP4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE DISP_TR4 ( N, T, X, A, B, IV, AV, D, NZ, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DISP_TR4  computes the following quantities:            *
! *         AV -- arithmetic average,                                    *
! *         D  -- dispersion (rms);                                      *
! *         NZ -- the number of used potins                              *
! *   over the real*4 function X(T) - (A*t + B) of length N. Array IV    *
! *   indicates which points are to be used: if IV(K) = 1 thern the      *
! *   X(K) element is used for computation of the avarage and            *
! *   dispersion, if IV(K) = 0 then  the k-th element is not counted     *
! *   for.                                                               *
! *                                                                      *
! *   X(T) - ( A*t + B )  means that the linear function is subtracted   *
! *   from the X(T). Usually A*t + B represents the best fit of the      *
! *   linear function. Then X(T) - ( A*t + B ) is a detrended function.  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N  ( INTEGER*4 ) -- The numer of elements of vector X.            *
! *    T  ( REAL*4    ) -- Array of function arguments. Dimension: N.    *
! *    X  ( REAL*4    ) -- Array of function values. Dimension: N.       *
! *    A  ( REAL*4    ) -- The angular coefficient of the subtracted     *
! *                        linear function.                              *
! *    B  ( REAL*4    ) -- The free term of the subtracted linear        *
! *                        function.                                     *
! *   IV  ( INTEGER*4 ) -- Accompanied participation vector.             *
! *                        If IV(k) = 1, then the k-th point is used,    *
! *                        if IV(m) = 0, then the m-th is ignored.       *
! *                                                                      *
! * ________________________ Ouput parameters: _________________________ *
! *                                                                      *
! *   AV  ( REAL*4    )  --  Average over NZ elements of the array X.    *
! *    D  ( REAL*4    )  --  Dispersion (rms) over NZ elements of the    *
! *                          input array X.                              *
! *   NZ  ( INTEGER*4, OPT )  -- The number of used elements.            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
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
! *  ###  26-JUL-1991   DISP_TR4  v 1.5  (c) L. Petrov  02-AUG-2009  ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER    N, IV(N), NZ, IUER
        REAL*4     T(N), X(N), A, B, D, AV
        CHARACTER  STR*10
        INTEGER*4  J1, J2, NZZ
        REAL*4     TT
        LOGICAL*4  IV_PRES
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_W, PROBE_R, IV_PRES
!        INTEGER*4 NUM$ARG, NA, N_ARG
!
!        PARAMETER ( N_ARG=9 )  !  The number of formal parameters
!C
!C ----- Check correspondence of the actual and formal arguments
!C
!        NA=NUM$ARG()  !  We learned the number of actual parameters
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   end of check ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'DISP4', 'Argument AV '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'DISP4', 'Argument D '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( PROBE_R ( N, 4, IV ) ) THEN
!             IV_PRES=.TRUE.
!          ELSE
!             IV_PRES=.FALSE.
!        END IF
!
! ----- Check does the argument IV present?
!
        IF ( LOC(IV) .EQ. 0 ) THEN
             IV_PRES = .FALSE.
          ELSE
             IV_PRES = .TRUE.
        END IF
!
! ----- Initialization
!
        AV=0.D0
        D=0.D0
        NZZ=0
!
! ----- Compute the average
!
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1).EQ.0 ) GOTO 410
           END IF
           NZZ=NZZ+1             !  NZZ  --  Number of used elements
           TT=T(J1)-T(1)
           AV=AV+( X(J1) - ( A*TT+B ) )
  410   CONTINUE
!
! ----- too few used elements
!
        IF ( NZZ.LT.2 ) THEN
             CALL CLRCH ( STR )
             CALL INCH ( NZZ, STR )
             CALL ERR_LOG ( 3, IUER, 'DISP_TR4', 'too few used elements: '// &
     &                     'only '//STR(1:I_LEN(STR)) )
!!             IF ( PRESENT ( NZ, 9 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZZ=NZ
             IF ( LOC(NZ) .NE. 0 ) NZZ = NZ
             RETURN
        END IF
        AV=AV/NZZ
!
! ----- compute dispersion (RMS)
!
        DO 420 J2=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J2).EQ.0 ) GOTO 420
           END IF
           TT=T(J2)-T(1)
           D=D+( (X(J2)-AV) - ( A*TT+B ) )**2
  420   CONTINUE
!
        D = DSQRT( D/(NZZ-1) )
!!        IF ( PRESENT ( NZ, 9 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZZ=NZ
        IF ( LOC(NZ) .NE. 0 ) NZZ=NZ
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  DISP_TR4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE DISP_WTR4 ( N, T, X, W, A, B, IV, D, NZ, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DISP_WTR4  computes the weighted dispersion and the     *
! *   number of used points over the real*4 function X(T) - (A*t + B)    *
! *   of length N. Array IV indicates which points are to be used:       *
! *   if IV(K) = 1 thern the X(k) element is used for computation of     *
! *   the avarage and dispersion, if IV(m) = 0 then  the m-th element    *
! *   is not counted for.                                                *
! *                                                                      *
! *   X(T) - ( A*t + B )  means that the linear function is subtracted   *
! *   from the X(T). Usually A*t + B represents the best fit of the      *
! *   linear function. Then X(T) - ( A*t + B ) is a detrended function.  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N  ( INTEGER*4 ) -- The numer of elements of vector X.            *
! *    T  ( REAL*4    ) -- Array of function arguments. Dimension: N.    *
! *    X  ( REAL*4    ) -- Array of function values. Dimension: N.       *
! *    X  ( REAL*4    ) -- Array of weights. Dimension: N.               *
! *    A  ( REAL*4    ) -- The angular coefficient of the subtracted     *
! *                        linear function.                              *
! *    B  ( REAL*4    ) -- The free term of the subtracted linear        *
! *                        function.                                     *
! *   IV  ( INTEGER*4 ) -- Accompanied participation vector.             *
! *                        If IV(k) = 1, then the k-th point is used,    *
! *                        if IV(m) = 0, then the m-th is ignored.       *
! *                                                                      *
! *                                                                      *
! * ________________________ Ouput parameters: _________________________ *
! *                                                                      *
! *    D  ( REAL*4    ) -- Weighted dispersion (wrms) over NZ elements   *
! *                        of the input array X.                         *
! *   NZ  ( INTEGER*4, OPT )  -- The number of used elements.            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
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
! *  ### 26-JUL-1991   DISP_WTR4  v 1.6  (c) L. Petrov 27-FEB-1994  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER   N, IV(N), NZ, IUER
        REAL*4    A, B, T(N), X(N), W(N), D, AV, WW
        CHARACTER STR*10
        LOGICAL*4 IV_PRES
        INTEGER*4 J2, NZZ
        REAL*4    TT
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_W, PROBE_R, IV_PRES
!        INTEGER*4 NUM$ARG, NA, N_ARG
!
!        PARAMETER ( N_ARG=9 )  !  The number of formal parameters
!C
!C ----- Check correspondence of the actual and formal arguments
!C
!        NA=NUM$ARG()  !  We learned the number of actual parameters
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   end of check ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'DISP_WTR4', 'Argument AV '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'DISP_WTR4', 'Argument D '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( PROBE_R ( N, 4, IV ) ) THEN
!             IV_PRES=.TRUE.
!          ELSE
!             IV_PRES=.FALSE.
!        END IF
!
!
! ----- Check: whether IV present?
!
        IF ( LOC(IV) .EQ. 0 ) THEN
             IV_PRES = .FALSE.
          ELSE
             IV_PRES = .TRUE.
        END IF
!
! ----- Initiaization
!
        AV=0.D0
        D=0.D0
        WW= 0.0D0
        NZZ=0
!
! ----- compute dispersion (wrms)
!
        DO 420 J2=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J2).EQ.0 ) GOTO 420
           END IF
           NZZ = NZZ + 1
           TT = T(J2) - T(1)
           D  = D  + W(J2)**2 * ( X(J2) - ( A*TT+B ) )**2
           WW = WW + W(J2)**2
  420   CONTINUE
!
! ----- Too few used elements
!
        IF ( NZZ.LT.2 ) THEN
             CALL CLRCH ( STR )
             CALL INCH ( NZZ, STR )
             CALL ERR_LOG ( 3, IUER, 'DISP_TR4', 'Too few used elements: '// &
     &                     ' only '//STR )
!!             IF ( PRESENT ( NZ, 9 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZZ=NZ
             IF ( LOC(NZ) .NE. 0 ) NZZ = NZ
             RETURN
        END IF
!
!!        D = DSQRT( D/(NZZ-1) )
        D = DSQRT( D/WW )
!!        IF ( PRESENT ( NZ, 9 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZZ=NZ
        IF ( LOC(NZ) .NE. 0 ) NZZ=NZ
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  DISP_WTR4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE DISP_EL4 ( N, X, IV, IV_EL, AV, D, NZ, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DISP_EL4  computes the following quantities:            *
! *         AV -- arithmetic average,                                    *
! *         D  -- dispersion (rms);                                      *
! *         NZ -- the number of used potins                              *
! *   over the real*4 function X(T) - (A*t + B) of length N. Array IV    *
! *   indicates which points are to be used: if IV(K) = IV_EL then the   *
! *   X(k)  element is used for computation of the avarage and           *
! *   dispersion, if IV(m) .NE. IV_EL  then  the m-th element is not     *
! *   counted for.                                                       *
! *                                                                      *
! *   X(t) - ( A*t + B )  means that the linear function is subtracted   *
! *   from the X(t). Usually A*t + B represents the best fit of the      *
! *   linear function. Then X(t) - ( A*t + B ) is a detrended function.  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N  ( INTEGER*4 ) -- The numer of elements of vector X.            *
! *    T  ( REAL*4    ) -- Array of function arguments. Dimension: N.    *
! *    X  ( REAL*4    ) -- Array of function values. Dimension: N.       *
! *    A  ( REAL*4    ) -- The angular coefficient of the subtracted     *
! *                        linear function.                              *
! *    B  ( REAL*4    ) -- The free term of the subtracted linear        *
! *                        function.                                     *
! *   IV  ( INTEGER*4 ) -- Accompanied participation vector.             *
! *                        If IV(k) = IV_EL, then the k-th point is used,*
! *                        if IV(m) .NE. IV_EL then X(m) is ignred.      *
! *                                                                      *
! * ________________________ Ouput parameters: _________________________ *
! *                                                                      *
! *   AV  ( REAL*4    )  --  Average over NZ elements of the array X.    *
! *    D  ( REAL*4    )  --  Dispersion (rms) over NZ elements of the    *
! *                          input array X.                              *
! *   NZ  ( INTEGER*4, OPT )  -- The number of used elements.            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
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
! *  ###  07-FEB-1992   DISP_EL4   v 1.4 (c) L. Petrov 08-AUG-1994  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER    N, IV(N), IV_EL, NZ, IUER
        REAL*4     X(N), D, AV
        CHARACTER  STR*10
        LOGICAL*4  IV_PRES
        INTEGER*4  J1, J2
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_W, PROBE_R, IV_PRES
!        INTEGER*4 NUM$ARG, NA, N_ARG
!
!        PARAMETER ( N_ARG=8 )  !  The number of formal parameters
!C
!C ----- Check correspondence of the actual and formal arguments
!C
!        NA=NUM$ARG()  !  We learned the number of actual parameters
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   end of check ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'DISP_WTR4', 'Argument AV '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'DISP_WTR4', 'Argument D '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( PROBE_R ( N, 4, IV ) ) THEN
!             IV_PRES=.TRUE.
!          ELSE
!             IV_PRES=.FALSE.
!        END IF
!
! ----- Learn whether IV is present
!
        IF ( LOC(IV) .EQ. 0 ) THEN
             IV_PRES = .FALSE.
          ELSE
             IV_PRES = .TRUE.
        END IF
!
! ----- Initialization
!
        AV=0.D0
        D=0.D0
        NZ=0
!
! ----- Computation iof the average
!
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1) .NE. IV_EL ) GOTO 410
           END IF
           NZ=NZ+1             !  NZ  --  the number of used elements
           AV=AV + X(J1)
  410   CONTINUE
!
! ----- Too few used elements
!
        IF ( NZ.LT.2 ) THEN
             CALL CLRCH   ( STR )
             CALL INCH    ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'DISP_EL4', 'Too few used elements: '// &
     &                     'only '//STR )
             RETURN
        END IF
        AV=AV/NZ
!
! ----- Computation of dispersion
!
        DO 420 J2=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J2).NE.IV_EL ) GOTO 420
           END IF
           D = D + ( X(J2)-AV ) **2
  420   CONTINUE
!C
        D = DSQRT( D/(NZ-1) )
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  DISP_EL4  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE DISP_TREL4 ( N, T, X, A, B, IV, IV_EL, AV, D, NZ, &
     &                          IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  DISP_TREL4  computes the following quantities:          *
! *         AV -- arithmetic average,                                    *
! *         D  -- dispersion (rms);                                      *
! *         NZ -- the number of used potins                              *
! *   over the real*4 function X(T) - (A*t + B) of length N. Array IV    *
! *   indicates which points are to be used: if IV(K) = IV_EL then the   *
! *   X(k)  element is used for computation of the avarage and           *
! *   dispersion, if IV(m) .NE. IV_EL  then  the m-th element is not     *
! *   counted for.                                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N  ( INTEGER*4 ) -- The numer of elements of vector X.            *
! *    T  ( REAL*4    ) -- Array of function arguments. Dimension: N.    *
! *    X  ( REAL*4    ) -- Array of function values. Dimension: N.       *
! *    A  ( REAL*4    ) -- The angular coefficient of the subtracted     *
! *                        linear function.                              *
! *    B  ( REAL*4    ) -- The free term of the subtracted linear        *
! *                        function.                                     *
! *   IV  ( INTEGER*4 ) -- Accompanied participation vector.             *
! *                        If IV(k) = IV_EL, then the k-th point is used,*
! *                        if IV(m) .NE. IV_EL then X(m) is ignred.      *
! *                                                                      *
! * ________________________ Ouput parameters: _________________________ *
! *                                                                      *
! *   AV  ( REAL*4    )  --  Average over NZ elements of the array X.    *
! *    D  ( REAL*4    )  --  Dispersion (rms) over NZ elements of the    *
! *                          input array X.                              *
! *   NZ  ( INTEGER*4, OPT )  -- The number of used elements.            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
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
! *  ### 14-FEB-1992  DISP_TREL4  v1.4  (c) L. Petrov  08-AUG-1994  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  N, IV(N), IV_EL, NZ, IUER
        REAL*4     T(N), X(N), A, B, D, AV
        REAL*4     TT
        INTEGER*4  J1, J2
        CHARACTER  STR*10
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT, PROBE_W, PROBE_R, IV_PRES
!        INTEGER*4 NUM$ARG, NA, N_ARG
!
!        PARAMETER ( N_ARG=11 )  !  The number of formal parameters
!C
!C ----- Check correspondence of the actual and formal arguments
!C
!        NA=NUM$ARG()  !  We learned the number of actual parameters
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   end of check ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'DISP_TREL4', 'Argument AV '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'DISP_TREL4', 'Argument D '//
!     $                                'is unavailable for writing' )
!              RETURN
!        END IF
!C
!        IF ( PROBE_R ( N, 4, IV ) ) THEN
!             IV_PRES=.TRUE.
!          ELSE
!             IV_PRES=.FALSE.
!        END IF
!
! ----- Initialization
!
        AV=0.D0
        D=0.D0
        NZ=0
!
! ----- Computing the average
!
        DO 410 J1=1,N
           IF ( IV(J1).EQ.IV_EL ) THEN
                NZ=NZ+1             !  NZ -- the number of used elements
                TT=T(J1)-T(1)
                AV=AV+( X(J1) - ( A*TT+B ) )
           END IF
  410   CONTINUE
!
! ----- Too few used elements
!
        IF ( NZ.LT.2 ) THEN
             CALL CLRCH ( STR )
             CALL INCH ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'DISP_TREL4', 'Too few used elements: '// &
     &                      'only '//STR )
             RETURN
        END IF
        AV=AV/NZ
!
! ----- Computation of dispersion
!
        DO 420 J2=1,N
           IF ( IV(J2).EQ.IV_EL ) THEN
                TT=T(J2)-T(1)
                D=D+( (X(J2)-AV) - ( A*TT+B ) )**2
           END IF
  420   CONTINUE
!
        D = DSQRT( D/(NZ-1) )
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  DISP_TREL4  #!#
!
! ---------------------------------------------------------------------
!
        SUBROUTINE EXTR4 ( N, T, X, A, B )
! ************************************************************************
! *                                                                      *
! *     Auxiliary routine  EXTR4  subtract a linear function A*t + B    *
! *   from the finction X(T).  Array X gets new values.                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    N  ( INTEGER*4 ) -- The numer of elements of vector X.            *
! *    T  ( REAL*4    ) -- Array of function arguments. Dimension: N.    *
! *    A  ( REAL*4    ) -- The angular coefficient of the subtracted     *
! *                        linear function.                              *
! *    B  ( REAL*4    ) -- The free term of the subtracted linear        *
! *                        function.                                     *
! *                                                                      *
! * ________________________ Modified paramenters: _____________________ *
! *                                                                      *
! *    X  ( REAL*4    ) -- Array of function values. Dimension: N.       *
! *                                                                      *
! *  ###  26-JUL-1991     EXTR4     v1.0  (c) L. Petro 08-AUG-1994  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  N 
        REAL*4     T(N), X(N), A, B
        INTEGER*4  J1
!!        CALL   VER$ARG ( 5 )
        DO 410 J1=1,N
           X(J1)=X(J1) - A*(T(J1)-T(1)) - B
  410   CONTINUE
        RETURN
        END  !#!  EXTR4  #!#
