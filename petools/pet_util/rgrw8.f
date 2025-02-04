        SUBROUTINE RGRW8 ( N, T, D, W, IV, MEAN_T, DR_VAL, SH_VAL, &
     &                     DR_SIG, SH_SIG, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine RGRW8 computes parameters of linear regression by using  *
! *   weighted LSQ for the function D(t) with weights W(t):              *
! *                                                                      *
! *     D(t) = SH_VAL + DR_VAL * ( t - mean_t ).                         *
! *                                                                      *
! *   Array IV consists of 0 and 1 and it marks the points which are     *
! *   used and which are not used. If IV(I)=1 then the I-th point        *
! *   participates in computation of regression, if IV(I)=0 then the     *
! *   I-th point is excluded from  computation.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  N ( INTEGER*4      ) -- Number of elements of vectors D and T.      *
! *  T ( REAL*8         ) -- Array of arguments of the function.         *
! *                          Dimension: N.                               *
! *  D ( REAL*8         ) -- Array of values of the function.            *
! *                          Dimension: N.                               *
! *  W ( REAL*8,    OPT ) -- Array of weights. If the argument is        *
! *                          omitted then weights 1 are used for all     *
! *                          points.                                     *
! * IV ( INTEGER*4, OPT ) -- Participation vector. consists of 0 and 1.  *
! *                          If IV(I)=1 then the I-th point is taken     *
! *                          into account in computation of regression,  *
! *                          otherwise it is omitted.                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * MEAN_T ( REAL*8     ) -- Mean value of the argument.                 *
! * DR_VAL ( REAL*8     ) -- Rate of change of regression.               *
! * SH_VAL ( REAL*8     ) -- Value of the regression at MEAN_T.          *
! * DR_SIG ( REAL*8     ) -- Formal weighted uncertainty of the rate of  *
! *                          change. Multiplicative reweighting is       *
! *                          applied.                                    *
! * SH_SIG ( REAL*8     ) -- Formal weighted uncertainty of the value of *
! *                          regression at MEAN_T. Multiplicative        *
! *                          reweighting is applied.                     *
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
! *  ###  06-MAY-91     RGRW8     v2.1 (c)  L. Petrov  31-OCT-2002  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IUER
        REAL*8    T(N), D(N), W(N), MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG
        INTEGER*4 IV(*), J1, J2, J3, NZ, I_LEN
        REAL*8    SD, ST, SDT, TT, STT, DET, DX, DY, WEI, SW, WW, CHI
        CHARACTER STR*10
        LOGICAL   W_PRES, IV_PRES
!
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
        WW  = 0.0D0
        NZ  = 0
!
        MEAN_T = 0.0
        WW = 0.0
        DO 410 J1=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J1).EQ.0 ) GOTO 410
           END IF
           IF ( W_PRES ) THEN
                WEI = W(J1)
              ELSE
                WEI = 1.D0
           END IF
           MEAN_T = MEAN_T + T(J1)*WEI
           WW = WW + WEI
 410    CONTINUE
        MEAN_T = MEAN_T/WW
!
! ----- Computation of coeficients of normal system
!
        DO 420 J2=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J2).EQ.0 ) GOTO 420
           END IF
           IF ( W_PRES ) THEN
                WEI = W(J2)
              ELSE
                WEI = 1.D0
           END IF
!
           NZ  = NZ+1
           SW  = SW + WEI**2
           SD  = SD + D(J2)*WEI**2
           TT  = T(J2) - MEAN_T
           ST  = ST  + TT*WEI**2
           SDT = SDT + D(J2)*TT*WEI**2
           STT = STT + TT**2 * WEI**2
  420   CONTINUE
!
! ----- It was found that number of used points was tooo small
!
        IF ( NZ .LT. 3 ) THEN
             CALL CLRCH ( STR )
             CALL INCH ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'RGRW8', 'Only '//STR(1:I_LEN(STR))// &
     &           'elements were used. It should be at least 3' )
             RETURN
        END IF
!
! ----- compute determinant
!
        DET = SW*STT - ST*ST
!
! ----- Compute minors of normal matrix
!
        DX = SW*SDT - SD*ST
        DY = SD*STT - ST*SDT
!
! ----- Determinant is too small
!
        IF ( DABS(DET) .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 4, IUER, 'RGRW8', 'Trtap of internal control: '// &
     &           'determinant is about zero' )
             RETURN
        END IF
!
! ----- Solving normal system
!
        DR_VAL = DX/DET
        SH_VAL = DY/DET
        DR_SIG = DSQRT ( SW/DET  )
        SH_SIG = DSQRT ( STT/DET )
!
        CHI = 0.0
        DO 430 J3=1,N
           IF ( IV_PRES ) THEN
                IF ( IV(J3).EQ.0 ) GOTO 430
           END IF
           IF ( W_PRES ) THEN
                WEI = W(J3)
              ELSE
                WEI = 1.D0
           END IF
           CHI = CHI + (WEI*(D(J3) - ( SH_VAL + DR_VAL*( T(J3) - MEAN_T))))**2
 430    CONTINUE
!
        DR_SIG = DR_SIG*DSQRT ( CHI/(NZ-2) )
        SH_SIG = SH_SIG*DSQRT ( CHI/(NZ-2) )
!!             type *,' chi_ndg = ',dsqrt ( chi/(nz-2) ),' nz=',nz,' chi=',chi
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  RGRW8  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE RGR8_NOWEI ( N, T, D, MEAN_T, DR_VAL, SH_VAL, &
     &                           DR_SIG, SH_SIG, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine RGR8_NOWEI computes parameters of linear regression by   *
! *   using non-weighted LSQ for the function D(t):                      *
! *                                                                      *
! *     D(t) = SH_VAL + DR_VAL * ( t - mean_t ).                         *
! *                                                                      *
! *   Array IV consists of 0 and 1 and it marks the points which are     *
! *   used and which are not used. If IV(I)=1 then the I-th point        *
! *   participates in computation of regression, if IV(I)=0 then the     *
! *   I-th point is excluded from  computation.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  N ( INTEGER*4      ) -- Number of elements of vectors D and T.      *
! *  T ( REAL*8         ) -- Array of arguments of the function.         *
! *                          Dimension: N.                               *
! *  D ( REAL*8         ) -- Array of values of the function.            *
! *                          Dimension: N.                               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * MEAN_T ( REAL*8     ) -- Mean value of the argument.                 *
! * DR_VAL ( REAL*8     ) -- Rate of change of regression.               *
! * SH_VAL ( REAL*8     ) -- Value of the regression at MEAN_T.          *
! * DR_SIG ( REAL*8     ) -- Formal weighted uncertainty of the rate of  *
! *                          change. Multiplicative reweighting is       *
! *                          applied.                                    *
! * SH_SIG ( REAL*8     ) -- Formal weighted uncertainty of the value of *
! *                          regression at MEAN_T. Multiplicative        *
! *                          reweighting is applied.                     *
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
! *  ###  06-MAY-1991  RGR8_NOWEI  v2.2 (c)  L. Petrov  06-JUN-2009 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, IUER
        REAL*8    T(N), D(N), MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG
        INTEGER*4 J1, J2, J3, NZ, I_LEN
        REAL*8    SD, ST, SDT, TT, STT, DET, DX, DY, SW, WW, CHI
        CHARACTER STR*10
        LOGICAL   W_PRES, IV_PRES
!
        SD  = 0.0D0
        SW  = 0.0D0
        ST  = 0.0D0
        SDT = 0.0D0
        STT = 0.0D0
        NZ  = 0
!
        MEAN_T = 0.0
        WW = 0.0
        DO 410 J1=1,N
           MEAN_T = MEAN_T + T(J1)
           WW = WW + 1.0D0
 410    CONTINUE
        MEAN_T = MEAN_T/WW
!
! ----- Computation of coeficients of normal system
!
        DO 420 J2=1,N
!
           NZ  = NZ+1
           SW  = SW + 1.0D0
           SD  = SD + D(J2)
           TT  = T(J2) - MEAN_T
           ST  = ST  + TT
           SDT = SDT + D(J2)*TT
           STT = STT + TT**2
  420   CONTINUE
!
! ----- It was found that number of used points was tooo small
!
        IF ( NZ .LT. 3 ) THEN
             CALL CLRCH ( STR )
             CALL INCH ( NZ, STR )
             CALL ERR_LOG ( 3, IUER, 'RGR8_NOWEI', 'Only '//STR(1:I_LEN(STR))// &
     &           'elements were used. It should be at least 3' )
             RETURN
        END IF
!
! ----- compute determinant
!
        DET = SW*STT - ST*ST
!
! ----- Compute minors of normal matrix
!
        DX = SW*SDT - SD*ST
        DY = SD*STT - ST*SDT
!
! ----- Determinant is too small
!
        IF ( DABS(DET) .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 4, IUER, 'RGR8_NOWEI', 'Trap of internal control: '// &
     &           'determinant is about zero' )
             RETURN
        END IF
!
! ----- Solving normal system
!
        DR_VAL = DX/DET
        SH_VAL = DY/DET
        DR_SIG = DSQRT ( SW/DET  )
        SH_SIG = DSQRT ( STT/DET )
!
        CHI = 0.0
        DO 430 J3=1,N
           CHI = CHI + ((D(J3) - ( SH_VAL + DR_VAL*( T(J3) - MEAN_T))))**2
 430    CONTINUE
!
        DR_SIG = DR_SIG*DSQRT ( CHI/(NZ-2) )
        SH_SIG = SH_SIG*DSQRT ( CHI/(NZ-2) )
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  SUBROUTINE  RGR8_NOWEI  !#!#
