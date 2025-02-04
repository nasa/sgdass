      SUBROUTINE SPE_RES ( L_SPE, SPE, L_PAR, C_PAR, EST_VEC, COV_MAT, SCL, &
     &                     APR_POS_EPC, EST_POS_EPC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPE_RES puts in the spool file opened at the logical unit  *
! *   23 the SPE-section with information about results of estimation of *
! *   non-linear site motion modeled with expansion with B-spline basis. *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   L_SPE ( INTEGER*4 ) -- Number of stations, which position is       *
! *                          modeled with spline.                        *
! *     SPE ( RECORD    ) -- Array of records defined in                 *
! *                          $PSOLVE_ROOT/include/solve.i which keeps    *
! *                          information about spline parameterization   *
! *                          of some stations position variations        *
! *                          estimation. Dimension: L_SPE.               *
! *   L_PAR ( INTEGER*4 ) -- The total number of estimated global        *
! *                          parameters.                                 *
! *   C_PAR ( CHARACTER ) -- Array of parameter names. Dimension: L_PAR. *
! * EST_VEC ( REAL*8    ) -- Vector of estimates. Dimension: L_PAR.      *
! * COV_MAT ( REAL*8    ) -- Unscaled covariance matrix in packed upper  *
! *                          triangular representation. Dimension: L_PAR.*
! *     SCL ( REAL*8    ) -- Covariance scaling factor.                  *
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
! *  ### 07-MAR-2005     SPE_RES   v1.3 (c)  L. Petrov  05-AUG-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'prfil.i'
      INTEGER*4  L_SPE, L_PAR, IUER
      CHARACTER  C_PAR(L_PAR)*(*)
      REAL*8     COV_MAT(*), EST_VEC(*), SCL, APR_POS_EPC, EST_POS_EPC
      REAL*8     SCL_ARR(M_GPA)
      CHARACTER  PAR_NAM*20, CMP(3)*1, NOD_DATE*23, REF_DATE*23, STA_NAM*8, &
     &           STA_NAM_REPAIRED*8
      DATA       CMP     / 'X', 'Y', 'Z' /
      TYPE       ( SPE__TYPE    ) SPE(L_SPE)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, L_NOD, IND_PAR(M_GPA), &
     &           ISTA, K_PAR, IPAR, IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( L_PAR .LT. 1  .OR.  L_PAR .GT. M_GPA ) THEN
           WRITE ( 6, * ) 'L_PAR  = ', L_PAR, ' M_GPA = ', M_GPA
           CALL ERR_LOG ( 7471, IUER, 'SPE_RES', 'Parameter L_PAR is '// &
     &         'out of range [1, M_GPA]' )
           RETURN 
      END IF
!
! --- Write the begining section marker 
!
      WRITE ( 23, '(A)' ) '1  SPE'
      WRITE ( 23, '(A, I4)' ) 'L_SPE ', L_SPE
      REF_DATE = JD_TO_DATE ( EST_POS_EPC, IER )
      DO 410 J1=1,L_SPE
         STA_NAM = SPE(J1)%STATION
!@U         CALL UNDSCR ( STA_NAM )
         STA_NAM_REPAIRED = SPE(J1)%STATION
         CALL VTD_NAME_REPAIR ( STA_NAM_REPAIRED )
         ISTA = LTM_DIF ( 1, INT4(MAX_STA), ISITN_CHR, STA_NAM )
         IF ( ISTA < 1 ) THEN
!@             CALL ERR_LOG ( 7472, IUER, 'SPE_RES', 'Trap of internal '// &
!@     &            'control: station '//STA_NAM//' was not found' )
!@              RETURN 
              GOTO 410
         END IF
!
         WRITE ( 23, '(A, I4, A, I4,A, I4, A, A)' ) '   I_SPE ', J1, &
     &               ' N_NOD: ', SPE(J1)%K_NOD, &
     &               '   DEG: ', SPE(J1)%DEGREE, &
     &               '   NAM: ', SPE(J1)%STATION
         K_PAR = 0
         DO 420 J2=1,3  ! Cycle over coordinate components
            WRITE  ( 23, 210 ) SPE(J1)%STATION, CMP(J2), REF_DATE, &
     &                         VSITEC(J2,ISTA) + VSITEV(J2,ISTA)* &
     &                        (EST_POS_EPC - APR_POS_EPC)/86400.0D0
 210        FORMAT ( '   APS: ',A, 1X, A, 29X, 'R: ', A, ' C:  ', F14.5 )
!
            WRITE  ( 23, 220 ) SPE(J1)%STATION, CMP(J2), VSITEV(J2,ISTA)/ &
     &                        (86400.0D0*JYEAR__DAYS )
 220        FORMAT ( '   APV: ',A, 1X, A, 55X, ' C: ', 1PD15.7 )
!
! --------- Print estimates of spline coefficients
!
            DO 430 J3=1-SPE(J1)%DEGREE,SPE(J1)%K_NOD
!
! ------------ Build parameter name
!
               PAR_NAM(1:16) = STA_NAM//' '//CMP(J2)//'BSPLN '
               CALL INCH   ( J3, PAR_NAM(17:20) )
               CALL CHASHR (     PAR_NAM(17:20) )
!
! ------------ Search parameter name in the list
!
               IPAR = LTM_DIF ( 1, L_PAR, C_PAR, PAR_NAM )
               NOD_DATE = JD_TO_DATE ( J2000__JD + SPE(J1)%TIM(J3)/86400.0D0, &
     &                                 -3 )
               CALL VTD_NAME_REPAIR ( PAR_NAM(1:8) )
               IF ( IPAR > 0 ) THEN
!
! ----------------- Aga. We found it. Put the parameter lines into the list
!
                    K_PAR = K_PAR + 1
                    IND_PAR(K_PAR) = IPAR
                    SCL_ARR(K_PAR) = DSQRT(SCL)
!
! ----------------- ... and print the estimate
!
                    WRITE  ( 23, 230 ) PAR_NAM, IPAR, J3, NOD_DATE, EST_VEC(IPAR)
 230                FORMAT ( 3X,'EST: ', A, ' I: ',I5, ' N: ',I5, &
     &                          ' D: ', A23, ' V: ',1PD15.7 )
                  ELSE
                    WRITE ( 23, 240 ) PAR_NAM(1:8), CMP(J2), J3, NOD_DATE
 240                FORMAT ( 3X,'NOD: ', A, 1X, A, 19X, ' N: ',I5, ' D: ', A23 )
               END IF
 430        CONTINUE 
!
! --------- Print estimates of site position
!
            PAR_NAM = STA_NAM//' '//CMP(J2)//' COMPONENT'
            IPAR = LTM_DIF ( 1, L_PAR, C_PAR, PAR_NAM )
            CALL VTD_NAME_REPAIR ( PAR_NAM(1:8) )
            IF ( IPAR > 0 ) THEN
                 K_PAR = K_PAR + 1
                 IND_PAR(K_PAR) = IPAR
                 SCL_ARR(K_PAR) = DSQRT(SCL)
                 WRITE ( 23, 250 ) PAR_NAM, IPAR, EST_VEC(IPAR)
 250             FORMAT ( 3X,'POS: ', A, ' I: ',I5, 36X, ' V: ',1PD15.7 )
            END IF
!
! --------- Print estimates of site velocity
!
            PAR_NAM = STA_NAM//' '//CMP(J2)//' VELOCITY '
            IPAR = LTM_DIF ( 1, L_PAR, C_PAR, PAR_NAM )
            CALL VTD_NAME_REPAIR ( PAR_NAM(1:8) )
            IF ( IPAR > 0 ) THEN
                 K_PAR = K_PAR + 1
                 IND_PAR(K_PAR) = IPAR
                 SCL_ARR(K_PAR) = DSQRT(SCL)/(CENT__TO__SEC/100.0D0)
                 WRITE ( 23, 260 ) PAR_NAM, IPAR, EST_VEC(IPAR)/(CENT__TO__SEC/100.0D0)
 260             FORMAT ( 3X,'VEL: ', A, ' I: ',I5, 36X, ' V: ',1PD15.7 )
            END IF
 420     CONTINUE 
!
! ------ Print elements of covariance matrix
!
         DO 440 J4=1,K_PAR
            DO 450 J5=J4,K_PAR
               WRITE ( 23, 270 ) STA_NAM_REPAIRED, IND_PAR(J4), IND_PAR(J5), &
     &                           COV_MAT(LOCS(IND_PAR(J4),IND_PAR(J5)))* &
     &                           SCL_ARR(J4)*SCL_ARR(J5)
 270           FORMAT ( 3X,'COV: ', A, ' IND1: ', I5,' IND2: ', I5, &
     &                           ' C: ',1PD19.12 )
 450        CONTINUE 
 440     CONTINUE 
 410  CONTINUE 
      WRITE ( 23, '(A)' ) '2  SPE'
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPE_RES
