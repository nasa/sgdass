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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ###  06-MAY-1991   REGRW4    v1.5 (d) L. Petrov  ì.à. 01-DEC-97 ###  *
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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  06-MAY-1991     DESIG4  v1.4  (d) L. Petrov  08-AUG-1994  ###  *
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
! ----- úáäáîéå  ED  --  þéóìï óôáîäáòôîùè ïôëìïîåîéê ïôîïóéôåìøîï
! ----- òåçòåóóéïîîïê ðòñíïê, âïìøûå ëïôïòùè óìåäõåT üìåíåîô ïôíåþáôø ëáë
! ----- ÷ùâòïó
!
!        IF ( NA.EQ.5 ) THEN
!             TYPE 110
!  110        FORMAT(1X/2X,'???  éóëìàþéôø ÷ùâòïóù, ëïôïòùå ðòå÷ùûáàô'/
!     *       2X,'óëïìøëï óòåäîåë÷áäòáôéþîùè ïôëìïîåîéê  <3.>  ? '$)
!             ACCEPT 120,IQ,ED
!  120        FORMAT(Q,G15.7)
!             IF( IQ.EQ.0 ) ED=3.   !  ðï õíïìþáîéà  ED=3
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
! *     ðïäðòïçòáííá ÷ùþéóìåîéñ õçìï÷ïçï ëïüææéãéåîôá é ó÷ïâïäîïçï þìåîá *
! *     òåçòåóóéïîîïê ðòñíïê æõîëãéé D(T) .                              *
! *                                                                      *
! *     éóðïìøúõåôóñ ÷åëôïò õþáóôéñ, ëïôïòùê ïôíåþáåô üìåíåîôù, ëïôïòùå  *
! *     âõäõô òáóóíáôòé÷áôøóñ. ÷ ïâòáâïôëõ âõäõô ÷ëìàþáôøóñ ôïìøëï ôå    *
! *     üìåíåîôù, äìñ ëïôïòùè IV(I)=IV_EL .                              *
! *                                                                      *
! *                                                                      *
! * ------------------------- ÷èïäîùå ðáòíåôòù: ------------------------ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  þéóìï üìåíåîôï÷ õ ÷åëôïòï÷  D  é  T .   *
! *        T  ( REAL*4    )  --  ÷åëôïò áòçõíåîôï÷ æõîëãéé D .           *
! *        D  ( REAL*4    )  --  ÷åëôïò úîáþåîéê æõîëãéé, äìñ ëïôïòïê    *
! *                              ÷ùþéóìñåôóñ äéóðåòóéñ é óòåäîåå.        *
! *       IV  ( INTEGER*4 )  --  ÷åëôïò õþáóôéñ.                         *
! *     IV_EL ( INTEGER*4 )  --  úîáþåîéå, ëïôïòïå õëáúù÷áåô îá          *
! *                              éóðïìøúõåíùê üìåíåîô.
! *                                                                      *
! * ------------------------- ÷ùèïäîùå ðáòíåôòù: ----------------------- *
! *                                                                      *
! *        A  ( REAL*4    )  --  õçìï÷ïê ëïüææéãéåîô ôòåîäá.             *
! *        B  ( REAL*4    )  --  ó÷ïâïäîùê þìåî ôòåîäá.                  *
! *                                                                      *
! * ___________________ íïäéæéãéòõåíùå ðáòáíåôòù: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- ðáòáíåôò ïûéâëé:                       *
! *             ÷èïäîïå úîáþåîéå  --  òåöéí ïâòáâïôëé ïûéâëé:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- ÷ïú÷òáýåîéå ëïäá ïûéâëé.                             *
! *      IUER=-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ é ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ        *
! *                 ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.                       *
! *      IUER<-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ, ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ é       *
! *                 úá÷åòûåîéñ ïâòáúá ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.     *
! *      åóìé IUER ïðõýåî, ôï ÷èïäîïå úîáþåîéå ðòéîéíáåôóñ òá÷îùí -1     *
! *             ÷ùèïäîïå úîáþåîéå  --  ëïä ïûéâëé ( åóìé IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             äïóôõðåî äìñ úáðéóé ):                                   *
! *      IUER=0  --  îïòíáìøîïå úá÷åòûåîéå.                              *
! *      IUER=1  --  ðáòáíåôò  A  îåäïóôõðåî äìñ úáðéóé.                 *
! *      IUER=2  --  ðáòáíåôò  B  îåäïóôõðåî äìñ úáðéóé.                 *
! *      IUER=3  --  üìåíåîôï÷, ðï ëïôïòùí óôòïéôóñ òåçòåóóéïîîáñ ðòñíáñ *
! *                  íåîøûå 2 .                                          *
! *      IUER=4  --  ÷îõôòåîîññ ïûéâëá: ïðòåäåìéôåìø ðï íïäõìà íåîøûå    *
! *                  1.D-30.                                             *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  13-JUL-91   REGRV_EL4   V1.4  (d) ðÅÔÒÏ× ì.à.  08-AUG-94  ###  *
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
!        PARAMETER ( N_ARG=8 )  !  ëïìéþåóô÷ï æïòíáìøîùè ðáòáíåôòï÷
!C
!C ----- ðòï÷åòëá óïïô÷åôóô÷éñ ëïìéþåóô÷á æáëôéþåóëéêè é æïòíáìøîùè ðáòáíôòï÷
!C
!        NA=NUM$ARG()  !  Mù õúîáìé ëïìéþåóô÷ï æáëôéþåóëéè ðáòáíåôï÷
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   ëïîåã ðòï÷åòëé  ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, A ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'REGRV_EL4', 'ðáòáíåôò A '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, B ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'REGRV_EL4', 'ðáòáíåôò B '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_R ( N, 4, IV ) ) THEN
!              CALL ERR_LOG ( 5, IUER, 'REGRV_EL4', 'ðáòáíåôò IV '//
!     $                                'îåäïóôõðåî äìñ þôåîéñ' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_R ( 1, 4, IV_EL ) ) THEN
!              CALL ERR_LOG ( 6, IUER, 'REGRV_EL4', 'ðáòáíåôò IV_EL '//
!     $                                'îåäïóôõðåî äìñ þôåîéñ' )
!              RETURN
!        END IF
!
        SD= 0.0D0
        ST= 0.0D0
        SDT=0.0D0
        STT=0.0D0
        NZ=0
!
! ----- ÷ùþéóìåîéå ëïüææéãéåîôï÷ îïòíáìøîïê óéóôåíù õòá÷îåîéê
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
! ----- ïëáúáìïóø, þôï þéóìï üìåíåîôï÷, õþáóô÷õàýéè ÷ ðïäóþåôáè, óìéûëïí íáìï
!
        IF ( NZ.LT.2 ) THEN
             CALL CLRCH ( STR )
             CALL INCH ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'REGRV_EL4', 'õþáóô÷õåô ôïìøëï '// &
     &                      STR(1:I_LEN(STR))//' üìåíåîôï÷' )
             RETURN
        END IF
!
! ----- ÷ùþéóìåîéå ïðòåäåìéôåìñ îïòíáìøîïê óéóôåíù
!
        DET=NZ*STT-ST*ST
!
! ----- ÷ùþéóìåîéå íéîïòï÷ íáôòéãù îïòíáìøîïê óéóôåíù
!
        DX=NZ*SDT-SD*ST
        DY=SD*STT-ST*SDT
!
! ----- ïðåòåäåìéôåìø óéóôåíù óìéûëïí íáì
!
        IF ( DABS(DET) .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 4, IUER, 'REGRV_EL4', '÷îõôòåîîññ ïûéâëá: '// &
     &                      'îõìå÷ïê ïðòåäåìéôåìø' )
             RETURN
        END IF
!
! ----- òåûåîéå îïòíáìøîïê óéóôåíù õòá÷îåîéê
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
! *     ðïäðòïçòáííá WDISP4 ÷ùþéóìñåô ëïòåîø ë÷áäòáôîùê éú îå÷ú÷åûåîîïê  *
! *     äéóðåòóéé -- D, ëïòåîø ë÷áäòáôîùê éú ÷ú÷åûåîîïê äéóðåòóéé,       *
! *     ûëáìéòï÷áîîïê îá óòåäîåå úîáþåîéå  - DW, ëïòåîø ë÷áäòáôîùê éú    *
! *     ÷ú÷åûåîîïê äéóðåòóéé - DSW, óòåäîåå  -- AV  é þéóìï              *
! *     éóðïìøúï÷áîîùè ïôóþåôï÷  --  NZ  äìñ  íáóóé÷á  X  äìéîïê N.      *
! *     W  --  íáóóé÷ ÷åóï÷ ÷åìéþéîù X.                                  *
! *     éóðïìøúõåôóñ íáóóé÷  IV, óïóôïñýéê éú  0 éìé 1, ëïôïòùê ïôíåþáåô *
! *     îåéóðïìøúï÷áîîùå ïôóþåôù. åóìé  IV(I)=1, ôï  I-ôùê ïôóþåô        *
! *     õþáóô÷õåô ÷ ïâòáúï÷áîéé óòåäîåçï é äéóðåòóéé, åóìé  IV(J)=0, ôï  *
! *     J-ôùê üìåíåîô éóëìàþáåôóñ éú ðïäóþåôï÷.                          *
! *                                                                      *
! * ------------------------- ÷èïäîùå ðáòíåôòù: ------------------------ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  þéóìï üìåíåîôï÷ õ ÷åëôïòá  X .          *
! *        X  ( REAL*4    )  --  ÷åëôïò, äìñ ëïôïòïçï óþéôáåôóñ          *
! *                              äéóðåòóéñ é óòåäîåå.                    *
! *        W  ( REAL*4    )  --  ÷åëôïò ÷åóï÷ ÷åìéþéîù X.                *
! *       IV  ( INTEGER*4, OPT )  --  ÷åëôïò, õëáúù÷áàýéê îá éóëìàþîîùå *
! *                              éú ðïäóþåôï÷ üìåíåîôù.                  *
! *                              åóìé ðáòáíåôò  IV  ïðõýåî, éóðïìøúõàôóñ *
! *                              ÷óå üìåíåîôù.                           *
! *                                                                      *
! * ------------------------- ÷ùèïäîùå ðáòíåôòù: ----------------------- *
! *                                                                      *
! *       AV  ( REAL*4    )  --  óòåäîåå éú NZ üìåíåîôï÷ íáóóé÷á X .     *
! *        D  ( REAL*4    )  --  R.M.S. äìñ NZ üìåíåîôï÷ íáóóé÷á X .     *
! *       DW  ( REAL*4    )  --  ûëáìéòï÷áîîáñ îá óòåäîåå úîáþåîéå       *
! *                              ÷åóá W.R.M.S. äìñ NZ üìåíåîôï÷ íáóóé÷á X*
! *      DSW  ( REAL*4    )  --  W.R.M.S. äìñ NZ üìåíåîôï÷ íáóóé÷á X .   *
! *       NZ  ( INTEGER*4, OPT )  --  þéóìï üìåíåîôï÷, ëïôïòùå ÷         *
! *                              ÷ õþáóô÷ï÷ïáìé ðïäóþåôå.                *
! *                                                                      *
! * ___________________ íïäéæéãéòõåíùå ðáòáíåôòù: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- ðáòáíåôò ïûéâëé:                       *
! *             ÷èïäîïå úîáþåîéå  --  òåöéí ïâòáâïôëé ïûéâëé:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- ÷ïú÷òáýåîéå ëïäá ïûéâëé.                             *
! *      IUER=-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ é ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ        *
! *                 ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.                       *
! *      IUER<-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ, ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ é       *
! *                 úá÷åòûåîéñ ïâòáúá ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.     *
! *      åóìé IUER ïðõýåî, ôï ÷èïäîïå úîáþåîéå ðòéîéíáåôóñ òá÷îùí -1     *
! *             ÷ùèïäîïå úîáþåîéå  --  ëïä ïûéâëé ( åóìé IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             äïóôõðåî äìñ úáðéóé ):                                   *
! *      IUER=0  --  îïòíáìøîïå úá÷åòûåîéå.                              *
! *      IUER=1  --  ðáòáíåôò  A  îåäïóôõðåî äìñ úáðéóé.                 *
! *      IUER=2  --  ðáòáíåôò  D  îåäïóôõðåî äìñ úáðéóé.                 *
! *      IUER=3  --  üìåíåîôï÷, ðï ëïôïòùí ÷ùþéóìñåôóñ äéóðåòóéñ,        *
! *                  íåîøûå 2 .                                          *
! *      IUER=4  --  ðáòáíåôò  DW îåäïóôõðåî äìñ úáðéóé.                 *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  04-JUN-93     WDISP4    V2.6  (d) ðÅÔÒÏ× ì.à.  20-OCT-95  ###  *
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
!        PARAMETER ( N_ARG=10 )  !  ëïìéþåóô÷ï æïòíáìøîùè ðáòáíåôòï÷
!C
!C ----- ðòï÷åòëá óïïô÷åôóô÷éñ ëïìéþåóô÷á æáëôéþåóëéêè é æïòíáìøîùè ðáòáíôòï÷
!C
!        NA=NUM$ARG()  !  Mù õúîáìé ëïìéþåóô÷ï æáëôéþåóëéè ðáòáíåôï÷
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   ëïîåã ðòï÷åòëé  ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'WDISP4', 'ðáòáíåôò AV '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'WDISP4', 'ðáòáíåôò D '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, DW ) ) THEN
!              CALL ERR_LOG ( 3, IUER, 'WDISP4', 'ðáòáíåôò DW '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, DSW ) ) THEN
!              CALL ERR_LOG ( 4, IUER, 'WDISP4', 'ðáòáíåôò DSW '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
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
! ----- îáþáìøîùå ïâîõìåîéñ
!
        AV=0.D0
        NZZ=0
!
! ----- ÷ùþéóìåîéå óòåäîåçï
!
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1).EQ.0 ) GOTO 410
           END IF
           NZZ=NZZ+1             !  NZZ  --  þéóìï õþáóô÷ï÷á÷ûéè
!                                !           ÷ ðïäóþåôáè üìåíåîôï÷
           AV=AV+X(J1)
 410   CONTINUE
!
! ----- ïëáúáìïóø, þôï þéóìï üìåíåîôï÷, õþáóô÷õàýéè ÷ ðïäóþåôáè, óìéûëïí íáìï
!
        IF ( NZZ.LT.2 ) THEN
             CALL CLRCH   ( STR )
             CALL INCH    ( NZZ, STR )
             CALL ERR_LOG ( 3, IUER, 'WDISP4', 'õþáóô÷õåô ôïìøëï '// &
     &                      STR(1:I_LEN(STR))//' üìåíåîôï÷' )
!!             IF ( PRESENT ( NZ, 8 ) .AND. PROBE_W ( 1, 4, NZ ) ) NZ=NZZ
             IF ( LOC(NZ) .NE. 0 ) NZ = NZZ
             RETURN
        END IF
        AV=AV/NZZ
!
! ----- ÷ùþéóìåîéå äéóðåòóéé
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
             CALL ERR_LOG ( 4, IUER, 'WDISP', 'éóðïìøúï÷áîîùå '// &
     &                      '÷åóá ïëáúáìéóø îõìå÷ùíé' )
             RETURN
        END IF
!
! ----- ðòï÷òåëá: äïóôáôïþîïå ìé þéóìï üìåíåîôï÷ ?
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
! *     ðïäðòïçòáííá  WDISP_EL4  ÷ùþéóìñåô ëïòåîø ë÷áäòáôîùê éú          *
! *     îå÷ú÷åûåîîïê äéóðåòóéé -- D, ëïòåîø ë÷áäòáôîùê éú ÷ú÷åûåîîïê     *
! *     äéóðåòóéé, ûëáìéòï÷áîîïê îá óòåäîåå úîáþåîéå  - DW, ëïòåîø       *
! *     ë÷áäòáôîùê éú ÷ú÷åûåîîïê äéóðåòóéé - DSW, óòåäîåå  -- AV  é      *
! *     þéóìï éóðïìøúï÷áîîùè ïôóþåôï÷  --  NZ  äìñ  íáóóé÷á  X äìéîïê N. *
! *     W  --  íáóóé÷ ÷åóï÷ ÷åìéþéîù X.                                  *
! *     éóðïìøúõåôóñ íáóóé÷  IV, ëïôïòùê ïôíåþáåô éóðïìøúõåíùå ïôóþåôù.  *
! *     ðòé üôïí éóðïìøúõåôóñ ôïìøëï ôå ïôóþôù  X(I), äìñ ëïôïòùè       *
! *     IV(I)=IV_EL.                                                     *
! *                                                                      *
! * ------------------------- ÷èïäîùå ðáòíåôòù: ------------------------ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  þéóìï üìåíåîôï÷ õ ÷åëôïòá  X .          *
! *        X  ( REAL*4    )  --  ÷åëôïò, äìñ ëïôïòïçï óþéôáåôóñ          *
! *                              äéóðåòóéñ é óòåäîåå.                    *
! *        W  ( REAL*4    )  --  ÷åëôïò ÷åóï÷ ÷åìéþéîù X.                *
! *       IV  ( INTEGER*4, OPT )  --  ÷åëôïò, õëáúù÷áàýéê îá éóëìàþåîîùå *
! *                              éú ðïäóþåôï÷ üìåíåîôù.                  *
! *                              åóìé ðáòáíåôò  IV  ïðõýåî, éóðïìøúõàôóñ *
! *                              ÷óå üìåíåîôù.                           *
! *     IV_EL ( INTEGER*4, OPT ) --   úîáþåîéå, õëáúù÷áàýåå îá           *
! *                              éóðïìøúõåíùê ïôóþô.                    *
! *                                                                      *
! * ------------------------- ÷ùèïäîùå ðáòíåôòù: ----------------------- *
! *                                                                      *
! *       AV  ( REAL*4    )  --  óòåäîåå éú NZ üìåíåîôï÷ íáóóé÷á X .     *
! *        D  ( REAL*4    )  --  R.M.S. äìñ NZ üìåíåîôï÷ íáóóé÷á X .     *
! *       DW  ( REAL*4    )  --  ûëáìéòï÷áîîáñ îá óòåäîåå úîáþåîéå       *
! *                              ÷åóá W.R.M.S. äìñ NZ üìåíåîôï÷ íáóóé÷á X*
! *      DSW  ( REAL*4    )  --  W.R.M.S. äìñ NZ üìåíåîôï÷ íáóóé÷á X .   *
! *      CHI  ( REAL*4    )  --  èé-ë÷áäòáô îá óôåðåîø ó÷ïâïäù.          *
! *       NZ  ( INTEGER*4 )  --  þéóìï üìåíåîôï÷, ëïôïòùå õþáóô÷ï÷áìé ÷  *
! *                              ðïäóþåôå.                               *
! *                                                                      *
! * ___________________ íïäéæéãéòõåíùå ðáòáíåôòù: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- ðáòáíåôò ïûéâëé:                       *
! *             ÷èïäîïå úîáþåîéå  --  òåöéí ïâòáâïôëé ïûéâëé:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- ÷ïú÷òáýåîéå ëïäá ïûéâëé.                             *
! *      IUER=-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ é ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ        *
! *                 ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.                       *
! *      IUER<-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ, ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ é       *
! *                 úá÷åòûåîéñ ïâòáúá ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.     *
! *      åóìé IUER ïðõýåî, ôï ÷èïäîïå úîáþåîéå ðòéîéíáåôóñ òá÷îùí -1     *
! *             ÷ùèïäîïå úîáþåîéå  --  ëïä ïûéâëé ( åóìé IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             äïóôõðåî äìñ úáðéóé ):                                   *
! *      IUER=0  --  îïòíáìøîïå úá÷åòûåîéå.                              *
! *      IUER=1  --  ðáòáíåôò  A  îåäïóôõðåî äìñ úáðéóé.                 *
! *      IUER=2  --  ðáòáíåôò  B  îåäïóôõðåî äìñ úáðéóé.                 *
! *      IUER=3  --  üìåíåîôï÷, ðï ëïôïòùí ÷ùþéóìñåôóñ äéóðåòóéñ,        *
! *                  íåîøûå 2 .                                          *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  21-JUN-94   WDISP_EL4   V1.6  (d) ðÅÔÒÏ× ì.à.  20-OCT-95  ###  *
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
!        PARAMETER ( N_ARG=12 )  !  ëïìéþåóô÷ï æïòíáìøîùè ðáòáíåôòï÷
!C
!C ----- ðòï÷åòëá óïïô÷åôóô÷éñ ëïìéþåóô÷á æáëôéþåóëéêè é æïòíáìøîùè ðáòáíôòï÷
!C
!        NA=NUM$ARG()  !  Mù õúîáìé ëïìéþåóô÷ï æáëôéþåóëéè ðáòáíåôï÷
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG  )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   ëïîåã ðòï÷åòëé  ...
!C
!        IF ( .NOT. PROBE_W ( 1, 4, AV ) ) THEN
!              CALL ERR_LOG ( 1, IUER, 'WDISP_EL4', 'ðáòáíåôò AV '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, D ) ) THEN
!              CALL ERR_LOG ( 2, IUER, 'WDISP_EL4', 'ðáòáíåôò D '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, DW ) ) THEN
!              CALL ERR_LOG ( 4, IUER, 'WDISP_EL4', 'ðáòáíåôò DW '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
!              RETURN
!        END IF
!C
!        IF ( .NOT. PROBE_W ( 1, 4, DW ) ) THEN
!              CALL ERR_LOG ( 4, IUER, 'WDISP_EL4', 'ðáòáíåôò DSW '//
!     $                                'îåäïóôõðåî äìñ úáðéóé' )
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
! ----- îáþáìøîùå ïâîõìåîéñ
!
        AV=0.D0
        D =0.D0
        DW=0.D0
        WW=0.D0
        NZ=0
!
! ----- ÷ùþéóìåîéå óòåäîåçï
!
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1).EQ.0 ) GOTO 410
                IF ( IV(J1).NE.IV_EL ) GOTO 410
           END IF
           NZ=NZ+1             !  NZ  --  þéóìï õþáóô÷ï÷á÷ûéè
!                              !          ÷ ðïäóþåôáè üìåíåîôï÷
           AV=AV + X(J1)*W(J1)
           WW=WW + W(J1)
  410   CONTINUE
!
! ----- ïëáúáìïóø, þôï þéóìï üìåíåîôï÷, õþáóô÷õàýéè ÷ ðïäóþåôáè, óìéûëïí íáìï
!
        IF ( NZ.LT.2 ) THEN
             CALL CLRCH   ( STR )
             CALL INCH    ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'WDISP_EL4', 'õþáóô÷õåô ôïìøëï '// &
     &                      STR(1:I_LEN(STR))//' üìåíåîôï÷' )
             RETURN
        END IF
!
        IF ( DABS(WW) .LT. 1.D-30 ) THEN
             WRITE ( 6, * ) ' WW=',WW, ' NZ=',NZ
             CALL ERR_LOG ( 4, IUER, 'WDISP_EL4', 'éóðïìøúï÷áîîùå '// &
     &                      '÷åóá ïëáúáìéóø îõìå÷ùíé' )
             RETURN
        END IF
        AV=AV/WW
        WW=0.
!
! ----- ÷ùþéóìåîéå äéóðåòóéé
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
! ----- ðòï÷åòëá: äïóôáôïþîïå ìé þéóìï üìåíåîôï÷ ?
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
! *     ðïäðòïçòáííá ÷ùþéóìåîéñ õçìï÷ïçï ëïüææéãéåîôá é ó÷ïâïäîïçï þìåîá *
! *     òåçòåóóéïîîïê ðòñíïê æõîëãéé D(T) .                              *
! *                                                                      *
! *     éóðïìøúõåôóñ íáóóé÷  IV, óïóôïñýéê éú  0 éìé 1 , ëïôïòùê         *
! *     ïôíåþáåô îåéóðïìøúï÷áîîùå ïôóþåôù. åóìé  IV(I)=1, ôï  I-ôùê      *
! *     ïôóþåô õþáóô÷õåô ÷ ÷ùþéóìåîéé ðáòáíåôòï÷ òåçòåóóéïîîïê ðòñíïê.   *
! *     åóìé  IV(J)=0, ôï J-ôùê üìåíåîô éóëìàþáåôóñ éú ðïäóþåôï÷.        *
! *                                                                      *
! *                                                                      *
! * ------------------------- ÷èïäîùå ðáòíåôòù: ------------------------ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  þéóìï üìåíåîôï÷ õ ÷åëôïòï÷  D  é  T .   *
! *        T  ( REAL*4    )  --  ÷åëôïò áòçõíåîôï÷ æõîëãéé D .           *
! *        D  ( REAL*4    )  --  ÷åëôïò úîáþåîéê æõîëãéé, äìñ ëïôïòïê    *
! *                              ÷ùþéóìñåôóñ äéóðåòóéñ é óòåäîåå.        *
! *                                                                      *
! * ------------------------- ÷ùèïäîùå ðáòíåôòù: ----------------------- *
! *                                                                      *
! *       DR  ( REAL*4    )  --  ó÷ïâïäîùê þìåî ôòåîäá.                  *
! *       SH  ( REAL*4    )  --  õçìï÷ïê ëïüææéãéåîô ôòåîäá.             *
! *                                                                      *
! * ___________________ íïäéæéãéòõåíùå ðáòáíåôòù: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- ðáòáíåôò ïûéâëé:                       *
! *             ÷èïäîïå úîáþåîéå  --  òåöéí ïâòáâïôëé ïûéâëé:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- ÷ïú÷òáýåîéå ëïäá ïûéâëé.                             *
! *      IUER=-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ é ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ        *
! *                 ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.                       *
! *      IUER<-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ, ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ é       *
! *                 úá÷åòûåîéñ ïâòáúá ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.     *
! *      åóìé IUER ïðõýåî, ôï ÷èïäîïå úîáþåîéå ðòéîéíáåôóñ òá÷îùí -1     *
! *             ÷ùèïäîïå úîáþåîéå  --  ëïä ïûéâëé ( åóìé IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             äïóôõðåî äìñ úáðéóé ):                                   *
! *      IUER=0  --  îïòíáìøîïå úá÷åòûåîéå.                              *
! *      IUER=1  --  ðáòáíåôò  DR  îåäïóôõðåî äìñ úáðéóé.                *
! *      IUER=2  --  ðáòáíåôò  SH  îåäïóôõðåî äìñ úáðéóé.                *
! *      IUER=3  --  üìåíåîôï÷, ðï ëïôïòùí óôòïéôóñ òåçòåóóéïîîáñ ðòñíáñ *
! *                  íåîøûå 2 .                                          *
! *      IUER=4  --  ÷îõôòåîîññ ïûéâëá: ïðòåäåìéôåìø ðï íïäõìà íåîøûå    *
! *                  1.D-30.                                             *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  06-MAY-1991     REGR4  v1.6  (d) ðÅÔÒÏ× ì.à.  26-FEB-2008 ###  *
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
! ----- ÷ùþéóìåîéå ëïüææéãéåîôï÷ îïòíáìøîïê óéóôåíù õòá÷îåîéê
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
! ----- ÷ùþéóìåîéå ïðòåäåìéôåìñ îïòíáìøîïê óéóôåíù
!
        DET = SW*STT - ST*ST
!
! ----- ÷ùþéóìåîéå íéîïòï÷ íáôòéãù îïòíáìøîïê óéóôåíù
!
        DX = SW*SDT - SD*ST
        DY = SD*STT - ST*SDT
!
! ----- ïðåòåäåìéôåìø óéóôåíù óìéûëïí íáì
!
        IF ( DABS(DET) .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 1014, IUER, 'REGR4', 'Zero determinant' )
             RETURN
        END IF
!
! ----- òåûåîéå îïòíáìøîïê óéóôåíù õòá÷îåîéê
!
        DR = DX/DET
        SH = DY/DET
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  SUBROUTINE  REGR4  !#!#
