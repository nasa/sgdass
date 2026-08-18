      SUBROUTINE DESIG_W8 ( N, X, E, NSIG, IV, NZ, AVR, RMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DESIG_W8 computes the average and root mean square of     *
! *   and array of X of dimension N and associated errors E using        *
! *   iterations till the maximum deviation is by module less than       *
! *   NSIG*rms.                                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    N ( INTEGER*4  ) -- Number of elements of vectors D and T.        *
! *    X ( REAL*8     ) -- Array of values of the function. Dimension: N *
! *    E ( REAL*8     ) -- Array of associated errors. Dimension: N.     *
! * NSIG ( REAL*8     ) -- The threshold to stop iterations. When the    *
! *                        maximum by module residual with respect to    *
! *                        the mean is less than NSIG*RMS, the           *
! *                        iterations are stopped.                       *
! *   IV ( INTEGER*4  ) -- Participation vector. consists of 0 and 1.    *
! *                      If IV(I)=1 then the I-th point is taken         *
! *                      into account in computation of the mean,        *
! *                      otherwise it is omitted.                        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * NZ     ( INTEGER*4  ) -- The number of points that we not used for   *
! *                          computation of the mean and the rms.        *
! * AVR    ( REAL*8     ) -- The mean value over N-NZ points.            *
! * RMS    ( REAL*8     ) -- The root mean square with respect to the    *
! *                          mean over N-NZ points.                      *
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
! *  ### 25-APR-2025    DESIG_W8   v1.0 (d)  L. Petrov  25-APR-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, IV(N), NZ, IUER
      REAL*8     X(N), E(N), NSIG, AVR, RMS
      REAL*8     ACC_AVR, ACC_SQR, ACC_WEI, DEV_MAX, NRM_MAX
      INTEGER*4  J1, J2, J3, IND_DEV, NP
!
      ACC_AVR = 0.0D0
      ACC_SQR = 0.0D0
      ACC_WEI = 0.0D0
      NP      = 0
!
      DO 410 J1=1,N
         IF ( IV(J1) .NE. 0 ) THEN
              ACC_AVR = ACC_AVR + X(J1)/E(J1)
              ACC_SQR = ACC_SQR + X(J1)**2/E(J1)
              ACC_WEI = ACC_WEI + 1.0D0/E(J1)
              NP = NP + 1
         END IF
 410  CONTINUE 
      IF ( NP < 2 ) THEN
           CALL ERR_LOG ( 7811, IUER, 'DESIG_W8', 'Too few points to '// &
     &         'start with' )
           RETURN 
      END IF
!
      AVR = ACC_AVR/ACC_WEI
      RMS = DSQRT ( ACC_SQR/ACC_WEI - AVR**2)
      NZ  = 0
      DO 420 J2=1,NP-4
         DEV_MAX = -1.0D0
         NRM_MAX = -1.0D0
         DO 430 J3=1,N
            IF ( IV(J3) .NE. 0 ) THEN
                 IF ( DABS( X(J3) - AVR )/E(J3) > NRM_MAX ) THEN
                      DEV_MAX = DABS( X(J3) - AVR )
                      NRM_MAX = DABS( X(J3) - AVR )/E(J3) 
                      IND_DEV = J3
                 END IF
            END IF
 430     CONTINUE 
!
         IF ( DEV_MAX > NSIG*RMS ) THEN
              NZ = NZ + 1
              ACC_AVR = ACC_AVR - X(IND_DEV)/E(IND_DEV)
              ACC_SQR = ACC_SQR - X(IND_DEV)**2/E(IND_DEV)
              ACC_WEI = ACC_WEI - 1.0D0/E(IND_DEV)
              IV(IND_DEV) = 0
            ELSE
              GOTO 820
         END IF
         AVR = ACC_AVR/ACC_WEI
         RMS = DSQRT ( ACC_SQR/ACC_WEI - AVR**2)
 420  CONTINUE 
 820  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DESIG_W8  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DESIG_TRW8 ( N, T, X, E, NSIG, IV, NZ, MEAN_T, &
     &                        SH, DR, RMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DESIG_W8 computes the linear model of a shift and drift   *
! *   and the root mean squares of post-fit residuals of using array     *
! *   of arguments (T), values (X), and associated errors (E) of         *
! *   dimension N using iterations till the maximum deviation is by      *
! *   module less than NSIG*rms.                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     N ( INTEGER*4 ) -- Number of elements of vectors D and T.        *
! *    X ( REAL*8     ) -- Array of values of the function. Dimension: N.*
! *    E ( REAL*8     ) -- Array of associated errors. Dimension: N.     *
! *   IV ( INTEGER*4  ) -- Participation vector. consists of 0 and 1.    *
! *                        If IV(I)=1 then the I-th point is taken       *
! *                        into account in computation of the mean,      *
! *                        otherwise it is omitted.                      *
! * NSIG ( REAL*8     ) -- The threshold to stop iterations. When the    *
! *                        maximum by module residual with respect to    *
! *                        the linear regression is less than NSIG*RMS,  *
! *                        the iterations are stopped.                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * NZ     ( INTEGER*4  ) -- The number of points that we not used for   *
! *                          computation of the mean and the rms.        *
! * MEAN_T ( REAL*8     ) -- Weighted mean value of the argument.        *
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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 25-APR-2025   DESIG_TRW8   v1.0 (d)  L. Petrov  25-APR-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, IV(N), NZ, IUER
      REAL*8     T(N), X(N), E(N), NSIG, MEAN_T, SH, DR, RMS
      REAL*8     SD, ST, SDT, TT, STT, SW, WW, DET, DX, DY, DEV_MAX, NRM_MAX
      INTEGER*4  J1, J2, J3, J4, J5, J6, IND_DEV, NP
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
      NP = 0
      DO 410 J1=1,N
         IF ( IV(J1) .NE. 0 ) THEN
              MEAN_T = MEAN_T + T(J1)/E(J1)
              WW = WW + 1.D0/E(J1)
              NP = NP + 1
         ENDIF
 410  CONTINUE
      MEAN_T = MEAN_T/WW
      IF ( NP < 3 ) THEN
          CALL ERR_LOG ( 7821, IUER, 'DESIG_TRW8', 'Too few points to '// &
     &        'start with' )
          RETURN 
      END IF
!
! --- Computation of coeficients of normal system
!
      DO 420 J2=1,N
         IF ( IV(J2) .NE. 0 ) THEN
              SW  = SW + 1.0D0/E(J2)**2
              SD  = SD + X(J2)/E(J2)**2
              TT  = T(J2) - MEAN_T
              ST  = ST  + TT/E(J2)**2
              SDT = SDT + X(J2)*TT/E(J2)**2
              STT = STT + TT**2/E(J2)**2
         ENDIF
  420 CONTINUE
!
! --- compute determinant
!
      DET = SW*STT - ST*ST
!
! --- Compute minors of normal matrix
!
      DX = SW*SDT - SD*ST
      DY = SD*STT - ST*SDT
!
! --- Determinant is too small
!
      IF ( DABS(DET) .LT. 1.D-30 ) THEN
           CALL ERR_LOG ( 7822, IUER, 'RGRW8', 'Trtap of internal control: '// &
     &         'determinant is about zero' )
           RETURN
      END IF
!
! --- Solving normal system
!
      DR = DX/DET
      SH = DY/DET
!
      RMS = 0.0D0
      NZ = 0
      DO 430 J3=1,N
         IF ( IV(J3) .NE. 0 ) THEN
              RMS = RMS + ( X(J3) - (SH + DR*(T(J3) - MEAN_T)) )**2
            ELSE
              NZ = NZ + 1
         END IF
 430  CONTINUE 
!
      IF ( N - NZ > 2 ) THEN
           RMS = DSQRT ( RMS/(N-NZ) )
         ELSE
           RMS = 0.0D0
      END IF
!
      DO 440 J4=1,NP-4
         DEV_MAX = -1.0D0
         NRM_MAX = -1.0D0
!
         DO 450 J5=1,N
            IF ( IV(J5) .NE. 0 ) THEN
                 IF ( DABS( X(J5) - (SH + DR*(T(J5) - MEAN_T)) )/E(J5) > NRM_MAX ) THEN
                      DEV_MAX = DABS( X(J5) - (SH + DR*(T(J5) - MEAN_T)) )
                      NRM_MAX = DABS( X(J5) - (SH + DR*(T(J5) - MEAN_T)) )/E(J5) 
                      IND_DEV = J5
                 END IF
            END IF
 450     CONTINUE 
!
         IF ( NRM_MAX > NSIG ) THEN
              NZ = NZ + 1
              SW  = SW - 1.0D0/E(IND_DEV)**2
              SD  = SD - X(IND_DEV)/E(IND_DEV)**2
              TT  = T(IND_DEV) - MEAN_T
              ST  = ST  - TT/E(IND_DEV)**2
              SDT = SDT + X(IND_DEV)*TT/E(IND_DEV)**2
              STT = STT + TT**2/E(IND_DEV)**2
              IV(IND_DEV) = 0
            ELSE
              GOTO 840
         END IF
!
! ------ Compute determinant
!
         DET = SW*STT - ST*ST
!
! ------ Compute minors of normal matrix
!
         DX = SW*SDT - SD*ST
         DY = SD*STT - ST*SDT
!
! ------ Determinant is too small
!
         IF ( DABS(DET) .LT. 1.D-30 ) THEN
              CALL ERR_LOG ( 7824, IUER, 'RGRW8', 'Trtap of internal control: '// &
     &            'determinant is about zero' )
              RETURN
         END IF
!
! ------ Solving normal system
!
         DR = DX/DET
         SH = DY/DET
 440  CONTINUE 
 840  CONTINUE 
!
      RMS = 0.0D0
      WW  = 0.0D0
      DO 460 J6=1,NP
         IF ( IV(J6) .NE. 0 ) THEN
              RMS = RMS + ( X(J6) - (SH + DR*(T(J6) - MEAN_T)) )**2/E(J6)
              WW  = WW  + 1.0D0/E(J6)
         END IF
 460  CONTINUE 
      RMS = DSQRT ( RMS/WW )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DESIG_TRW8  !#!  
