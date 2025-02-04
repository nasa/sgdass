      SUBROUTINE LEGENDRE_REGR ( MP, ARG, VAL, WEI, DEG, EST, ERR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LEGENDRE_REGR  computes coefficients of the normalized    *
! *   Legendre polynomial regression of degree DEG using the least       *
! *   squares. Results are written in the array EST. Legendre polynomial *
! *   are normalized in such a way that  \int_{-1}^{1} P_n(x)^2 = 1.0    *
! *   NB: arrays EST and ERR are sized as (0:DEG).                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MP ( INTEGER*4 ) -- The number of points in the input array.      *
! *   ARG ( REAL*8    ) -- Input array of arguments sorted in ascending  *
! *                        order. Dimension: MP.                         *
! *   VAL ( REAL*8    ) -- Input array of values of the function under   *
! *                        consideration. Dimension: MP.                 *
! *   WEI ( REAL*8    ) -- Input array of weights. Dimension: MP.        *
! *   DEG ( INTEGER*4 ) -- Maximum degree of the polynomials.            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   EST ( REAL*8    ) -- The array of estimates of the Legendre        *
! *                        polynomial. Dimension: (0:DEG).               *
! *   ERR ( REAL*8    ) -- The array of estimates of Legendre polynomial *
! *                        errors. Dimension: (0:DEG).                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 23-JUL-2008 LEGENDRE_REGR  v1.1 (c) L. Petrov  27-JAN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MP, DEG, IUER
      REAL*8     ARG(MP), VAL(MP), WEI(MP), EST(0:DEG), ERR(0:DEG)
      REAL*8,    ALLOCATABLE :: COV(:), EQU(:), RH(:)
      REAL*8     RCOND, R1
      CHARACTER  STR*32
      INTEGER*4  J1, J2, J3, J4, IND, IER
      REAL*8,    EXTERNAL :: LEGENDRE_POL 
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( DEG > 64 ) THEN
           CALL CLRCH ( STR )
           CALL INCH ( DEG, STR )
           CALL ERR_LOG ( 1711, IUER, 'LEGENDRE_REGR', 'deg '//TRIM(STR)// &
     &         ' was requested, while the maximum supported degree is 64' )
           RETURN 
      END IF
!
! --- Memory allocation
!
      ALLOCATE ( COV(((DEG+1)*(DEG+2))/2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (DEG+1)*(DEG+2), STR )
           CALL ERR_LOG ( 1712, IUER, 'LEGENDRE_REGR', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynammic memory' )
           RETURN 
      END IF
!
      ALLOCATE ( EQU(0:DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (DEG+1)*(DEG+2), STR )
           CALL ERR_LOG ( 1713, IUER, 'LEGENDRE_REGR', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynammic memory' )
           RETURN 
      END IF
!
      ALLOCATE ( RH(0:DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (DEG+1)*(DEG+2), STR )
           CALL ERR_LOG ( 1714, IUER, 'LEGENDRE_REGR', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynammic memory' )
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT_R8 ( ((DEG+1)*(DEG+2))/2, COV ) 
      CALL NOUT_R8 ( (DEG+1), RH ) 
!
! --- build the normal matrix
!
      DO 410 J1=1,MP
         DO 420 J2=0,DEG
            EQU(J2) = LEGENDRE_POL ( J2, ARG(1), ARG(MP), ARG(J1) )
 420     CONTINUE 
         CALL DIAD_CVT_S ( WEI(J1)*WEI(J1), DEG+1, EQU, EQU, COV ) 
!
         DO 430 J3=0,DEG
            RH(J3) = RH(J3) + WEI(J1)**2*EQU(J3)*VAL(J1)
 430     CONTINUE 
 410  CONTINUE 
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( DEG+1, COV, RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1715, IUER, 'LEGENDRE_REGR', 'Error during '// &
     &         'inversion of the normal matrix' )
!
           DEALLOCATE ( COV ) 
           DEALLOCATE ( EQU ) 
           DEALLOCATE ( RH  ) 
!
           RETURN
      END IF
!
! --- Find vector of the estimates of the parameters
!
      IER=-1
      CALL MUL_MV_SV_V ( DEG+1, COV, DEG+1, RH, DEG+1, EST, IER )
!
! --- Find estimates of formal errors
!
      IND = 0
      DO 440 J4=0,DEG
         IND = IND + J4+1
         ERR(J4) = DSQRT ( COV(IND) )
 440  CONTINUE 
!
      DEALLOCATE ( COV ) 
      DEALLOCATE ( EQU ) 
      DEALLOCATE ( RH  ) 
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE  LEGENDRE_REGR  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LEGENDRE_REGR_CNS ( MP, ARG, VAL, WEI, DEG, &
     &                               CNS_VAL, CNS_DER, CNS_DR2, EST, ERR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LEGENDRE_REGR  computes coefficients of the normalized    *
! *   Legendre polynomial regression of degree DEG using the least       *
! *   squares with applying constraints on the values of polynomial      *
! *   and/or its first and/or its second derivative. Results are written *
! *   in the output array EST and uncertainties of polynomial            *
! *   coefficients in ERR. Legendre polynomial are normalized in such    *
! *   a way that  \int_{-1}^{1} P_n(x)^2 = 1.0 .                         *
! *   NB: arrays EST and ERR are sized as (0:DEG).                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MP ( INTEGER*4 ) -- The number of points in the input array.      *
! *   ARG ( REAL*8    ) -- Input array of arguments sorted in ascending  *
! *                        order. Dimension: MP.                         *
! *   VAL ( REAL*8    ) -- Input array of values of the function under   *
! *                        consideration. Dimension: MP.                 *
! *   WEI ( REAL*8    ) -- Input array of weights. Dimension: MP.        *
! *   DEG ( INTEGER*4 ) -- Maximum degree of the polynomials.            *
! * CNS_VAL ( REAL*8  ) -- reciprical weight for constraint imposed on   *
! *                        the value.                                    *
! * CNS_DER ( REAL*8  ) -- reciprical weight for constraint imposed on   *
! *                        the first derivative.                         *
! * CNS_DR2 ( REAL*8  ) -- reciprical weight for constraint imposed on   *
! *                        the value.                                    *
! *   
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   EST ( REAL*8    ) -- The array of estimates of the Legendre        *
! *                        polynomial. Dimension: (0:DEG).               *
! *   ERR ( REAL*8    ) -- The array of estimates of Legendre polynomial *
! *                        errors. Dimension: (0:DEG).                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 23-JUL-2008 LEGENDRE_REGR_CNS v2.1 (c) L. Petrov 27-JAN-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MP, DEG, IUER
      REAL*8     ARG(MP), VAL(MP), WEI(MP), EST(0:DEG), ERR(0:DEG)
      REAL*8     CNS_VAL, CNS_DER, CNS_DR2
      REAL*8,    ALLOCATABLE :: COV(:), EQU(:), RH(:)
      REAL*8     RCOND, R1
      CHARACTER  STR*32
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IND, IER
      REAL*8,    EXTERNAL :: LEGENDRE_POL, LEGENDRE_DER, LEGENDRE_DR2
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( DEG > 64 ) THEN
           CALL CLRCH ( STR )
           CALL INCH ( DEG, STR )
           CALL ERR_LOG ( 1721, IUER, 'LEGENDRE_REGR_CNS', 'deg '//TRIM(STR)// &
     &         ' was requested, while the maximum supported degree is 64' )
           RETURN 
      END IF
!
! --- Memory allocation
!
      ALLOCATE ( COV(((DEG+1)*(DEG+2))/2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (DEG+1)*(DEG+2), STR )
           CALL ERR_LOG ( 1722, IUER, 'LEGENDRE_REGR_CNS', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynammic memory' )
           RETURN 
      END IF
!
      ALLOCATE ( EQU(0:DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (DEG+1)*(DEG+2), STR )
           CALL ERR_LOG ( 1723, IUER, 'LEGENDRE_REGR_CNS', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynammic memory' )
           RETURN 
      END IF
!
      ALLOCATE ( RH(0:DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (DEG+1)*(DEG+2), STR )
           CALL ERR_LOG ( 1724, IUER, 'LEGENDRE_REGR_CNS', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynammic memory' )
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT_R8 ( ((DEG+1)*(DEG+2))/2, COV ) 
      CALL NOUT_R8 ( (DEG+1), RH ) 
!
! --- build the normal matrix
!
      DO 410 J1=1,MP
         IF ( CNS_DER > 0.0D0 ) THEN
              DO 420 J2=0,DEG
                 EQU(J2) = LEGENDRE_DER ( J2, ARG(1), ARG(MP), ARG(J1) )
 420          CONTINUE 
              CALL DIAD_CVT_S ( 1.0D0/CNS_DER**2, DEG+1, EQU, EQU, COV ) 
         END IF
         IF ( CNS_DR2 > 0.0D0 ) THEN
              DO 430 J3=0,DEG
                 EQU(J3) = LEGENDRE_DR2 ( J3, ARG(1), ARG(MP), ARG(J1) )
 430          CONTINUE 
              CALL DIAD_CVT_S ( 1/CNS_DR2**2, DEG+1, EQU, EQU, COV ) 
         END IF
!
         DO 440 J4=0,DEG
            EQU(J4) = LEGENDRE_POL ( J4, ARG(1), ARG(MP), ARG(J1) )
 440     CONTINUE 
         CALL DIAD_CVT_S ( WEI(J1)*WEI(J1), DEG+1, EQU, EQU, COV ) 
         IF ( CNS_VAL > 0.0D0 ) THEN
              CALL DIAD_CVT_S ( 1/CNS_VAL**2, DEG+1, EQU, EQU, COV ) 
         END IF
!
         DO 450 J5=0,DEG
            RH(J5) = RH(J5) + WEI(J1)**2*EQU(J5)*VAL(J1)
 450     CONTINUE 
 410  CONTINUE 
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( DEG+1, COV, RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1725, IUER, 'LEGENDRE_REGR_CNS', 'Error during '// &
     &         'inversion of the normal matrix' )
!
           DEALLOCATE ( COV ) 
           DEALLOCATE ( EQU ) 
           DEALLOCATE ( RH  ) 
!
           RETURN
      END IF
!
! --- Find vector of the estimates of the parameters
!
      IER=-1
      CALL MUL_MV_SV_V ( DEG+1, COV, DEG+1, RH, DEG+1, EST, IER )
!
! --- Find estimates of formal errors
!
      IND = 0
      DO 470 J7=0,DEG
         IND = IND + J7+1
         ERR(J7) = DSQRT ( COV(IND) )
 470  CONTINUE 
!
      DEALLOCATE ( COV ) 
      DEALLOCATE ( EQU ) 
      DEALLOCATE ( RH  ) 
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE  LEGENDRE_REGR_CNS  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   LEGENDRE_POL ( DEG, ARG_BEG, ARG_END, ARG )
! ************************************************************************
! *                                                                      *
! *   Routine LEGENDRE_POL computed the Legandre polynomial of degree    *
! *   DEG defined at the interval [ARG_BEG, ARG_END] for arument ARG.    *
! *   The polynomial is normalized to 1: \int_{-1}^{1} P_n(x)^2 = 1.0    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DEG ( INTEGER*4 ) -- Polynomial degree.                          *
! * ARG_BEG ( REAL*8    ) -- Begining the interval.                      *
! * ARG_END ( REAL*8    ) -- End of the interval.                        *
! *     ARG ( REAL*8    ) -- Argument.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <LEGENDRE_POL> ( REAL*8    ) -- Normalized Legendre polynomial.      *
! *                                                                      *
! * ### 23-JUL-2008  LEGENDRE_POL   v1.1 (c)  L. Petrov  30-JUL-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG
      REAL*8     LEGENDRE_POL
      REAL*8     ARG_BEG, ARG_END, ARG 
      REAL*8     POL_LAST_LAST, POL_LAST, POL, X
      INTEGER*4  MAX_DEG, N$1
      PARAMETER  ( MAX_DEG = 64 )
      REAL*8     DSQ(0:MAX_DEG)
      DATA (DSQ(N$1), N$1=0,MAX_DEG) &  ! DSQRT ( 0.5D0 + K )
     & / &
     &   7.071067811865476D-01, & !  0
     &   1.224744871391589D+00, & !  1
     &   1.581138830084190D+00, & !  2
     &   1.870828693386971D+00, & !  3
     &   2.121320343559642D+00, & !  4
     &   2.345207879911715D+00, & !  5
     &   2.549509756796392D+00, & !  6
     &   2.738612787525831D+00, & !  7
     &   2.915475947422650D+00, & !  8
     &   3.082207001484488D+00, & !  9
     &   3.240370349203930D+00, & ! 10
     &   3.391164991562634D+00, & ! 11
     &   3.535533905932738D+00, & ! 12
     &   3.674234614174767D+00, & ! 13
     &   3.807886552931954D+00, & ! 14
     &   3.937003937005906D+00, & ! 15
     &   4.062019202317980D+00, & ! 16
     &   4.183300132670378D+00, & ! 17
     &   4.301162633521313D+00, & ! 18
     &   4.415880433163924D+00, & ! 19
     &   4.527692569068709D+00, & ! 20
     &   4.636809247747852D+00, & ! 21
     &   4.743416490252569D+00, & ! 22
     &   4.847679857416329D+00, & ! 23
     &   4.949747468305833D+00, & ! 24
     &   5.049752469181039D+00, & ! 25
     &   5.147815070493500D+00, & ! 26
     &   5.244044240850758D+00, & ! 27
     &   5.338539126015656D+00, & ! 28
     &   5.431390245600108D+00, & ! 29
     &   5.522680508593631D+00, & ! 30
     &   5.612486080160912D+00, & ! 31
     &   5.700877125495690D+00, & ! 32
     &   5.787918451395113D+00, & ! 33
     &   5.873670062235365D+00, & ! 34
     &   5.958187643906492D+00, & ! 35
     &   6.041522986797286D+00, & ! 36
     &   6.123724356957945D+00, & ! 37
     &   6.204836822995428D+00, & ! 38
     &   6.284902544988268D+00, & ! 39
     &   6.363961030678928D+00, & ! 40
     &   6.442049363362563D+00, & ! 41
     &   6.519202405202649D+00, & ! 42
     &   6.595452979136460D+00, & ! 43
     &   6.670832032063167D+00, & ! 44
     &   6.745368781616021D+00, & ! 45
     &   6.819090848492928D+00, & ! 46
     &   6.892024376045111D+00, & ! 47
     &   6.964194138592060D+00, & ! 48
     &   7.035623639735144D+00, & ! 49
     &   7.106335201775948D+00, & ! 50
     &   7.176350047203662D+00, & ! 51
     &   7.245688373094719D+00, & ! 52
     &   7.314369419163897D+00, & ! 53
     &   7.382411530116700D+00, & ! 54
     &   7.449832212875670D+00, & ! 55
     &   7.516648189186454D+00, & ! 56
     &   7.582875444051551D+00, & ! 57
     &   7.648529270389178D+00, & ! 58
     &   7.713624310270756D+00, & ! 59
     &   7.778174593052023D+00, & ! 60
     &   7.842193570679061D+00, & ! 61
     &   7.905694150420948D+00, & ! 62
     &   7.968688725254614D+00, & ! 63
     &   8.031189202104505D+00  & ! 64
     & /
      INTEGER*4  K_LAST, K_LAST_LAST, K, J1
!
      IF ( DEG == 0 ) THEN
           LEGENDRE_POL = DSQ(0)
           RETURN 
         ELSE IF ( DEG == 1 ) THEN
           LEGENDRE_POL = ((ARG - ARG_BEG) + (ARG - ARG_END))/ &
     &                    (ARG_END - ARG_BEG) * DSQ(1)
           RETURN 
         ELSE 
           X = ((ARG - ARG_BEG) + (ARG - ARG_END))/ &
     &         (ARG_END - ARG_BEG)
           POL_LAST = 1.D0
           POL      = X
!
           DO 410 J1=2,DEG
              POL_LAST_LAST = POL_LAST
              POL_LAST = POL
              POL = ( (2*J1-1)*X*POL_LAST - (J1-1)*POL_LAST_LAST )/J1
 410       CONTINUE 
           LEGENDRE_POL = POL*DSQ(DEG)
           RETURN
      END IF
      END  FUNCTION   LEGENDRE_POL  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   LEGENDRE_DER ( DEG, ARG_BEG, ARG_END, ARG )
! ************************************************************************
! *                                                                      *
! *   Routine LEGENDRE_DER computed the first derivative of the Legandre *
! *   polynomial of degree DEG defined at the interval                   *
! *   [ARG_BEG, ARG_END] for arument ARG.                                *
! *   The original Legendre polynomial is normalized to 1:               *
! *   \int_{-1}^{1} P_n(x)^2 = 1.0                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DEG ( INTEGER*4 ) -- Polynomial degree.                          *
! * ARG_BEG ( REAL*8    ) -- Begining the interval.                      *
! * ARG_END ( REAL*8    ) -- End of the interval.                        *
! *     ARG ( REAL*8    ) -- Argument.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <LEGENDRE_POL> ( REAL*8    ) -- Normalized Legendre polynomial.      *
! *                                                                      *
! * ### 06-MAR-2011   LEGENDRE_DER v1.0 (c)  L. Petrov  07-MAR-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG
      REAL*8     LEGENDRE_DER
      REAL*8     ARG_BEG, ARG_END, ARG 
      REAL*8     POL_LAST_LAST, POL_LAST, POL, X
      INTEGER*4  K_LAST, K_LAST_LAST, K, J1
!
      IF ( DEG == 0 ) THEN
           LEGENDRE_DER = 0.0D0
           RETURN 
         ELSE IF ( DEG == 1 ) THEN
           LEGENDRE_DER = DSQRT(6.D0)/(ARG_END - ARG_BEG)
           RETURN 
         ELSE 
           X = ((ARG - ARG_BEG) + (ARG - ARG_END))/ &
     &         (ARG_END - ARG_BEG)
           POL_LAST = 0.0D0
           POL      = 1.0D0
!
           DO 410 J1=2,DEG
              POL_LAST_LAST = POL_LAST
              POL_LAST = POL
              POL = ( (2*J1-1)*X*POL_LAST - J1*POL_LAST_LAST )/(J1-1)
 410       CONTINUE 
           LEGENDRE_DER = POL*DSQRT(4.0D0*DEG+2.0D0)/(ARG_END - ARG_BEG)
           RETURN
      END IF
      END  FUNCTION   LEGENDRE_DER  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   LEGENDRE_DR2 ( DEG, ARG_BEG, ARG_END, ARG )
! ************************************************************************
! *                                                                      *
! *   Routine LEGENDRE_DR2 computed the second derivative of the         *
! *   Legendre polynomial of degree DEG defined at the interval          *
! *   [ARG_BEG, ARG_END] for arument ARG.                                *
! *   The original Legendre polynomial is normalized to 1:               *
! *   \int_{-1}^{1} P_n(x)^2 = 1.0                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DEG ( INTEGER*4 ) -- Polynomial degree.                          *
! * ARG_BEG ( REAL*8    ) -- Begining the interval.                      *
! * ARG_END ( REAL*8    ) -- End of the interval.                        *
! *     ARG ( REAL*8    ) -- Argument.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <LEGENDRE_DR2> ( REAL*8    ) -- Normalized Legendre polynomial.      *
! *                                                                      *
! * ### 06-MAR-2011   LEGENDRE_DR2 v1.0 (c)  L. Petrov  19-APR-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG
      REAL*8     LEGENDRE_DR2
      REAL*8     ARG_BEG, ARG_END, ARG 
      REAL*8     POL_LAST_LAST, POL_LAST, POL, &
     &           DER_LAST_LAST, DER_LAST, DER, &
     &           DR2_LAST_LAST, DR2_LAST, DR2, X
      INTEGER*4  K_LAST, K_LAST_LAST, K, J1
!
      IF ( DEG == 0 ) THEN
           LEGENDRE_DR2 = 0.0D0
           RETURN 
         ELSE IF ( DEG == 1 ) THEN
           LEGENDRE_DR2 = 0.0
           RETURN 
         ELSE IF ( DEG == 2 ) THEN
           LEGENDRE_DR2 = 12.D0 * DSQRT(0.5D0 + DEG) / (ARG_END - ARG_BEG)**2 
           RETURN 
         ELSE 
           X = ((ARG - ARG_BEG) + (ARG - ARG_END))/ &
     &         (ARG_END - ARG_BEG)
           POL_LAST = 1.D0
           POL      = X
!
           DER_LAST = 0.0D0
           DER      = 1.0D0
!
           DR2_LAST = 0.0D0
           DR2      = 0.0D0
!
!    P (N,X) = ( (2*N-1)*  X*P (N-1,X)               - (N-1)*P(N-2,X)  ) / N
!    P'(N,X) = ( (2*N-1)*(   P (N-1,X) + X*P'(N-1,X) - (N-1)*P'(N-2,X) ) / N
!    P"(N,X) = ( (2*N-1)*( 2*P'(N-1,X) + X*P"(N-1,X) - (N-1)*P"(N-2,X) ) / N
!
           DO 410 J1=2,DEG
              POL_LAST_LAST = POL_LAST
              POL_LAST = POL
              POL =  ( (2*J1-1) * X *   POL_LAST                  - (J1-1)*POL_LAST_LAST )/J1
!
              DER_LAST_LAST = DER_LAST
              DER_LAST = DER
              DER =  ( (2*J1-1) * (     POL_LAST + X*DER_LAST )   - (J1-1)*DER_LAST_LAST )/J1
!
              DR2_LAST_LAST = DR2_LAST
              DR2_LAST = DR2
              DR2 =  ( (2*J1-1) * ( 2.0D0*DER_LAST + X*DR2_LAST ) - (J1-1)*DR2_LAST_LAST )/J1
 410       CONTINUE 
           LEGENDRE_DR2 = 4.0D0 * DSQRT(0.5D0 + DEG) * DR2/(ARG_END - ARG_BEG)**2 
           RETURN
      END IF
      END  FUNCTION   LEGENDRE_DR2  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   LEGENDRE_VAL ( DEG, ARG_BEG, ARG_END, ARG, LEG_COEF )
! ************************************************************************
! *                                                                      *
! *   Routine LEGENDRE_VAL computes the value of a function that was     *
! *   expanded into Legandre polynomials of degree DEG and defined at    *
! *   the interval [ARG_BEG, ARG_END] for argument ARG.                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      DEG ( INTEGER*4 ) -- Polynomial degree.                         *
! *  ARG_BEG ( REAL*8    ) -- Begining the interval.                     *
! *  ARG_END ( REAL*8    ) -- End of the interval.                       *
! *      ARG ( REAL*8    ) -- Argument.                                  *
! * LEG_COEF ( REAL*8    ) -- Array of Legender coefficients sized as    *
! *                           [0,DEG].                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <LEGENDRE_VAL> ( REAL*8 ) -- value of a function that is exanded     *
! *                              into Legandre polynomials.              *
! *                                                                      *
! * ### 30-JUL-2013  LEGENDRE_VAL  v1.0 (c)  L. Petrov  30-JUL-2013  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG
      REAL*8     LEGENDRE_VAL
      REAL*8     ARG_BEG, ARG_END, ARG, LEG_COEF(0:DEG)
      REAL*8     POL_LAST_LAST, POL_LAST, POL, X
      INTEGER*4  MAX_DEG, N$1
      PARAMETER  ( MAX_DEG = 64 )
      REAL*8     DSQ(0:MAX_DEG)
      DATA (DSQ(N$1), N$1=0,MAX_DEG) &  ! DSQRT ( 0.5D0 + K )
     & / &
     &   7.071067811865476D-01, & !  0
     &   1.224744871391589D+00, & !  1
     &   1.581138830084190D+00, & !  2
     &   1.870828693386971D+00, & !  3
     &   2.121320343559642D+00, & !  4
     &   2.345207879911715D+00, & !  5
     &   2.549509756796392D+00, & !  6
     &   2.738612787525831D+00, & !  7
     &   2.915475947422650D+00, & !  8
     &   3.082207001484488D+00, & !  9
     &   3.240370349203930D+00, & ! 10
     &   3.391164991562634D+00, & ! 11
     &   3.535533905932738D+00, & ! 12
     &   3.674234614174767D+00, & ! 13
     &   3.807886552931954D+00, & ! 14
     &   3.937003937005906D+00, & ! 15
     &   4.062019202317980D+00, & ! 16
     &   4.183300132670378D+00, & ! 17
     &   4.301162633521313D+00, & ! 18
     &   4.415880433163924D+00, & ! 19
     &   4.527692569068709D+00, & ! 20
     &   4.636809247747852D+00, & ! 21
     &   4.743416490252569D+00, & ! 22
     &   4.847679857416329D+00, & ! 23
     &   4.949747468305833D+00, & ! 24
     &   5.049752469181039D+00, & ! 25
     &   5.147815070493500D+00, & ! 26
     &   5.244044240850758D+00, & ! 27
     &   5.338539126015656D+00, & ! 28
     &   5.431390245600108D+00, & ! 29
     &   5.522680508593631D+00, & ! 30
     &   5.612486080160912D+00, & ! 31
     &   5.700877125495690D+00, & ! 32
     &   5.787918451395113D+00, & ! 33
     &   5.873670062235365D+00, & ! 34
     &   5.958187643906492D+00, & ! 35
     &   6.041522986797286D+00, & ! 36
     &   6.123724356957945D+00, & ! 37
     &   6.204836822995428D+00, & ! 38
     &   6.284902544988268D+00, & ! 39
     &   6.363961030678928D+00, & ! 40
     &   6.442049363362563D+00, & ! 41
     &   6.519202405202649D+00, & ! 42
     &   6.595452979136460D+00, & ! 43
     &   6.670832032063167D+00, & ! 44
     &   6.745368781616021D+00, & ! 45
     &   6.819090848492928D+00, & ! 46
     &   6.892024376045111D+00, & ! 47
     &   6.964194138592060D+00, & ! 48
     &   7.035623639735144D+00, & ! 49
     &   7.106335201775948D+00, & ! 50
     &   7.176350047203662D+00, & ! 51
     &   7.245688373094719D+00, & ! 52
     &   7.314369419163897D+00, & ! 53
     &   7.382411530116700D+00, & ! 54
     &   7.449832212875670D+00, & ! 55
     &   7.516648189186454D+00, & ! 56
     &   7.582875444051551D+00, & ! 57
     &   7.648529270389178D+00, & ! 58
     &   7.713624310270756D+00, & ! 59
     &   7.778174593052023D+00, & ! 60
     &   7.842193570679061D+00, & ! 61
     &   7.905694150420948D+00, & ! 62
     &   7.968688725254614D+00, & ! 63
     &   8.031189202104505D+00  & ! 64
     & /
      INTEGER*4  K_LAST, K_LAST_LAST, K, J1
!
      IF ( DEG == 0 ) THEN
           LEGENDRE_VAL = LEG_COEF(0)*DSQ(0)
           RETURN 
         ELSE IF ( DEG == 1 ) THEN
           X = ((ARG - ARG_BEG) + (ARG - ARG_END))/ &
     &         (ARG_END - ARG_BEG)
           LEGENDRE_VAL = LEGENDRE_VAL + &
     &                    LEG_COEF(1)*DSQ(1)*   &
     &                    ((ARG - ARG_BEG) + (ARG - ARG_END))/ &
     &                     (ARG_END - ARG_BEG)
           RETURN 
         ELSE 
           X = ((ARG - ARG_BEG) + (ARG - ARG_END))/ &
     &         (ARG_END - ARG_BEG)
           LEGENDRE_VAL = LEG_COEF(0)*DSQ(0)
           LEGENDRE_VAL = LEGENDRE_VAL + LEG_COEF(1)*DSQ(1)*X
           POL_LAST = 1.D0
           POL      = X
!
           DO 410 J1=2,DEG
              POL_LAST_LAST = POL_LAST
              POL_LAST = POL
              POL = ( (2*J1-1)*X*POL_LAST - (J1-1)*POL_LAST_LAST )/J1
              LEGENDRE_VAL = LEGENDRE_VAL + LEG_COEF(J1)*DSQ(J1)*POL
 410       CONTINUE 
           RETURN
      END IF
      END  FUNCTION   LEGENDRE_VAL  !#!  
