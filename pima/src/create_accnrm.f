      PROGRAM    CREATE_ACCNRM
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  MP, MS
      PARAMETER  ( MP = 10001 )
      PARAMETER  ( MS = 22   )
      REAL*8     RD(MP), RA(MP), T(MP), FUN(MP), VAL(MP), SPL(MP), &
     &           WORK(MP), RSTEP, RB(MP), N, MU1, MU2, NRM, &
     &           ARG_SPL(MS), COEF_SPL(MS), APP(MP), DIF(MP)
      CHARACTER  SEP*1
      REAL*8       EPS, N_22, MU1_22, MU2_22, NRM_22, &
     &                  N_24, MU1_24, MU2_24, NRM_24, &
     &                  N_44, MU1_44, MU2_44, NRM_44
      PARAMETER  ( EPS = 1.D-6 )
!
      PARAMETER  (   N_22 = 1.0D0 )
      PARAMETER  ( MU1_22 = 0.0D0 )
      PARAMETER  ( MU2_22 = 0.0D0 )
      PARAMETER  ( NRM_22 = 1.0D0 )
!
      PARAMETER  (   N_24 = 3.335875D0 )
      PARAMETER  ( MU1_24 = 0.0D0      )
      PARAMETER  ( MU2_24 = 0.981599D0 )
      PARAMETER  ( NRM_24 = 5.8784D0   )
!
      PARAMETER  (   N_44 = 3.335875D0 )
      PARAMETER  ( MU1_44 = 0.981599D0 )
      PARAMETER  ( MU2_44 = 0.981599D0 )
      PARAMETER  ( NRM_44 = 4.3048D0   )
!
      INTEGER*4  J1, J2, J3, J4, J5, J6, MODE, NS, IXC, IUER
      REAL*8,    EXTERNAL :: CHEB_VAL, ISPL8, FSPL8
      INTEGER*4, EXTERNAL :: IXMN8 
!
      MODE = 3
!
      RSTEP = 1.0D0/(MP-1)
      IF ( MODE == 1 ) THEN
           N   = N_22
           MU1 = MU1_22
           MU2 = MU2_22
           NRM = NRM_22
         ELSE IF ( MODE == 2 ) THEN
           N   = N_24
           MU1 = MU1_24
           MU2 = MU2_24
           NRM = NRM_24
         ELSE IF ( MODE == 3 ) THEN
           N   = N_44
           MU1 = MU1_44
           MU2 = MU2_44
           NRM = NRM_44
      END IF
!
      ARG_SPL(1)  = 0.00D0
      ARG_SPL(2)  = 0.02D0
      ARG_SPL(3)  = 0.20D0
      ARG_SPL(4)  = 0.40D0
      ARG_SPL(5)  = 0.55D0
      ARG_SPL(6)  = 0.70D0
      ARG_SPL(7)  = 0.80D0
      ARG_SPL(8)  = 0.88D0
      ARG_SPL(9)  = 0.92D0
      ARG_SPL(10) = 0.95D0
      ARG_SPL(11) = 0.97D0
      ARG_SPL(12) = 0.98D0
      ARG_SPL(13) = 0.99D0
      ARG_SPL(14) = 0.995D0
      ARG_SPL(15) = 0.997D0
      ARG_SPL(16) = 0.998D0
      ARG_SPL(17) = 0.9990D0
      ARG_SPL(18) = 0.9992D0
      ARG_SPL(19) = 0.9995D0
      ARG_SPL(20) = 0.9998D0
      ARG_SPL(21) = 0.9999D0
      ARG_SPL(22) = 1.0000D0
      NS = 22
!
      DO 410 J1=1,MP
         T(J1) = (J1-1)*RSTEP
         IF ( (1.0D0 - T(J1)) > EPS ) THEN
              VAL(J1) = 1.0D0/(PI__NUM*DSQRT(1.0D0 - T(J1)**2))/NRM * &
     &                  ( 2.0D0 + &
     &                    (N-1.0D0)**2* &
     &                    ( DEXP(-(MU1**2 - 2.0D0*T(J1)*MU1*MU2 + MU2**2)/ &
     &                            (2.0D0*(1.0D0 - T(J1)**2)) ) + &
     &                      DEXP(-(MU1**2 + 2.0D0*T(J1)*MU1*MU2 + MU2**2)/ &
     &                            (2.0D0*(1.0D0 - T(J1)**2)) ) &
     &                    ) + &
     &                    2.0D0*(N-1.0D0)* &
     &                    ( DEXP(-MU1**2/(2.0D0*(1.0D0 - T(J1)**2)) ) + &
     &                      DEXP(-MU2**2/(2.0D0*(1.0D0 - T(J1)**2)) ) &
     &                    ) &
     &                  )
            ELSE 
              VAL(J1) = 2.D0*VAL(J1-1) - VAL(J1-2) 
         END IF
 410  CONTINUE 
      IUER = -1
      CALL MAKE_SPLINE ( 1, MP, T, VAL, RSTEP, RSTEP, SPL, WORK, IUER )
!
      DO 420 J2=1,MS
         IXC = IXMN8 ( MP, T, ARG_SPL(J2) )
         IUER = -1
         IF ( J2 == 1 ) THEN
              FUN(J2) = 0.0D0
            ELSE 
              FUN(J2) = ISPL8 ( ARG_SPL(J2), MP, T, VAL, 1, IXC, SPL, IUER )
         END IF
 420  CONTINUE 
      IUER = -1
      CALL DIAGI_1 ( NS, ARG_SPL, FUN, IUER )
!
      IUER = -1
      CALL MAKE_SPLINE ( 1, NS, ARG_SPL, FUN, RSTEP, RSTEP, COEF_SPL, WORK, IUER )
!
      DO 430 J3=1,NS
         SEP = ', ' 
         IF ( J3 == NS ) SEP = ' '
         WRITE ( 6, 110 ) ARG_SPL(J3), FUN(J3), COEF_SPL(J3), SEP, J3
 110     FORMAT ( '     &        ', 1PD14.6, ',  ', 1PD14.6, ',  ', &
     &                              1PD14.6, A, ' & ! ', I2 )
 430  CONTINUE 
!
      DO 440 J4=1,MP
         IF ( J4 == 1 ) THEN
              RA(J4) = 0.0D0
            ELSE IF ( J4 == MP ) THEN
              IXC = IXMN8 ( NS, ARG_SPL, 1.D0-1.D-5 )
              RA(J4) = FSPL8 ( 1.D0-1.D-5, NS, ARG_SPL, FUN, IXC, COEF_SPL )
            ELSE 
              IXC = IXMN8 ( NS, ARG_SPL, T(J4)  )
              RA(J4) = FSPL8 ( T(J4), NS, ARG_SPL, FUN, IXC, COEF_SPL )
         END IF
 440  CONTINUE 
      IUER = -1
      CALL DIAGI_1 ( MP, T, RA, IUER )
!
      END  PROGRAM   CREATE_ACCNRM  !#!#
