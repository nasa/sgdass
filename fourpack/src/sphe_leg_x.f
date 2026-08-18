#define BALF_SQ_COMP
#define BPRINT_PMN
      SUBROUTINE SPHE_LEG_X ( FSH, DEG, NORM, IPHS, LAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPHE_LEG_X evalutates all of the normalized Legendre       *
! *   associated functions and its partial derivavatives over eastern    *
! *   and northern direction up to degree DEG. Computation of associated *
! *   Legendre functions is made using the algebra of X-numbers          *
! *   introduced by Fukushima (2011). It provids correct answer for      *
! *   arbbitrary degree/order, although the maximum degree/order is      *
! *   still restricted by 65535 or logisitcal reasons.                   *
! *                                                                      *
! *   Notes:                                                             *
! *                                                                      *
! *   1.  The employed normalization is the "orthonormalized convention."*
! *       The integral of (plm*cos(m theta))**2 or                       *
! *       (plm*sin (m theta))**2 over all space is 1.                    *
! *                                                                      *
! *   2. The integral of plm**2 over (-1,1) is (2 - delta(0,m))/2pi.     *
! *      If NORM=1, then this is equal to 1/2pi.                         *
! *                                                                      *
! *   Output is written in FSH%PL                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- Internal data structure that keeps internal    *
! *                       arrays with intermediate results and their     *
! *                       status for possible re-use.                    *
! *  DEG ( INTEGER*4 ) -- Degree of the expansion                        *
! * NORM ( INTEGER*4 ) -- Normalization to be used when calculating      *
! *                       Legendre functions                             *
! *                       1 -- "geodesy";                                *
! *                       2 -- Schmidt;                                  *
! *                       3 -- unnormalized;                             *
! *                       4 -- orthonormalized;                          *
! *                       NB: currently computation of partial           *
! *                           derivatives over east and north directions *
! *                           is made only for "geodesy" normalization.  *
! * IPHS ( INTEGER*4 ) -- Phase flag.                                    *
! *                        1: Do not include the Condon-Shortley phase   *
! *                           factor of (-1)^m.                          *
! *                       -1: Apply the Condon-Shortley phase factor     *
! *                           of (-1)^m.                                 *
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
! *   Copyright (c) 2006-2011, Mark A. Wieczorek                         *
! *   All rights reserved.                                               *
! *                                                                      *
! * ### 21-AUG-2012  SPHE_LEG_X v2.0 modified by L. Petrov 19-OCT-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  DEG, IPHS, NORM, IUER
      REAL*8     LAT
      LOGICAL*4  FL_ERROR
      TYPE     ( X__TYPE ) :: UM_X(FSH__MAX_DEG), P_X, PM1_X, PM2_X, ALF_SQ_X
      CHARACTER  STR*128, STR1*128
      REAL*8     PLM, PMM, PM1, PM2, U, Z, FACT, ALF_SQ_R8, PUM
      INTEGER*4  J1, J2, J3, J4, J5, J6, IER
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6681, IUER, 'SPHE_LEG_X', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
!
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6682, IUER, 'SPHE_LEG_X', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
!
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6683, IUER, 'SPHE_LEG_X', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( NORM < 1 .OR. NORM > 4 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NORM, STR )
           CALL ERR_LOG ( 6684, IUER, 'SPHE_LEG_X', 'Wrong value '// &
     &         'of NORM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 4] was expected' )
           RETURN 
      END IF
!
      IF ( IPHS .NE. 1 .AND. IPHS .NE. -1 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IPHS, STR )
           CALL ERR_LOG ( 6685, IUER, 'SPHE_LEG_X', 'Wrong value '// &
     &         'of IPHS: '//STR(1:I_LEN(STR))//' -- either 1 or -1 '// &
     &         'was expected' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF ( FSH, DEG, NORM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6686, IUER, 'SPHE_LEG_X', 'Error in '// &
     &         'an attempt to precompute multiplicative factors used '// &
     &         ' in recursion relationships F1 and F2' )
           RETURN 
      END IF
!
      IF ( FSH%PL_STATUS == FSH__COMP         .AND. &
     &     FSH%PL_DEG    == DEG               .AND. &
     &     FSH%PL_NORM   == NORM              .AND. &
     &     DABS(LAT - FSH%LAT) < FSH__ANG_EPS       ) THEN
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE IF ( FSH%PL_STATUS == FSH__COMP .AND. &
     &             FSH%PL_DEG    == DEG       .AND. &
     &             FSH%PL_NORM   == NORM            ) THEN
           CONTINUE 
         ELSE 
           FL_ERROR = .FALSE.
!$OMP      CRITICAL
           IF ( ASSOCIATED ( FSH%PL ) ) THEN
                DEALLOCATE ( FSH%PL )
           END IF
           ALLOCATE ( FSH%PL(0:DEG,0:DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*(DEG+1)**2, STR )
                CALL ERR_LOG ( 6688, IUER, 'SPHE_LEG_X', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%PL' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%PL = 0.0D0
!
           IF ( ASSOCIATED ( FSH%PLT ) ) THEN
                DEALLOCATE ( FSH%PLT )
           END IF
           ALLOCATE ( FSH%PLT(0:DEG,0:DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*(DEG+1)**2, STR )
                CALL ERR_LOG ( 6689, IUER, 'SPHE_LEG_X', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%PLT' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%PLT = 0.0D0
!
           IF ( NORM == 1 ) THEN
                IF ( ASSOCIATED ( FSH%DPLT ) ) THEN
                     DEALLOCATE ( FSH%DPLT )
                END IF
                ALLOCATE ( FSH%DPLT(0:DEG,0:DEG), STAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR  )
                     CALL IINCH ( 8*(DEG+1)**2, STR )
                     CALL ERR_LOG ( 6690, IUER, 'SPHE_LEG_X', 'Failure in '// &
     &                   'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                   ' bytes of dymanic memory for array FSH%DPLT' )
                     FL_ERROR = .TRUE.
                     GOTO 820
                END IF
                FSH%DPLT = 0.0D0
                FSH%PL_STATUS = FSH__ALLO
                FSH%PL_DEG  = DEG
                FSH%PL_NORM = NORM
           END IF
 820       CONTINUE 
!$OMP      END CRITICAL
      END IF
!    
      FSH%PL= 0.0D0
!
! --- Calculate P(l,0)
!
      U = DCOS(LAT)
      Z = DSIN(LAT)
!      
      IF ( NORM == 1 .OR. NORM == 2 .OR. NORM == 3 ) THEN 
           PM2 = 1.0D0
        ELSE IF ( NORM == 4 ) THEN
           PM2 = 1.0D0/ DSQRT(4.0D0*PI)
      END IF 
      FSH%PL(0,0)  = PM2
      FSH%PLT(0,0) = PM2
      IF ( DEG == 0 ) RETURN 
!
      IF ( NORM == 1 ) THEN
           PM1 = Z* SQRI(3)
         ELSE IF ( NORM == 2 ) THEN
           PM1 = Z
         ELSE IF ( NORM == 3 ) THEN
           PM1 = Z
         ELSE IF ( NORM == 4 ) THEN
           PM1 = Z* SQRI(3)/ DSQRT(4.0D0*PI)
      END IF 
      FSH%PL(1,0)  = PM1
      FSH%PLT(0,1) = PM1
#ifdef ALF_SQ_COMP
      CALL FP_ZERO_X ( ALF_SQ_X )
      CALL FP_XFF    ( ALF_SQ_X, PM2 )
      CALL FP_XFF    ( ALF_SQ_X, PM1 )
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, 0, PM2
      WRITE ( 6, 210 ) 1, 0, PM1
 210  FORMAT ( 'SPHE_LEG_X: Deg: ', I5, ' Ord: ', I5, ' Pmn= ', 1PD22.15 )
#endif
!				
      IF ( DEG .GE. 2 ) THEN
!
! -------- Zonal terms
!
           CALL FP_F_TO_X ( PM1, PM1_X )
           CALL FP_F_TO_X ( PM2, PM2_X )
           CALL FP_F_TO_X ( U, UM_X(1) )
           DO 410 J1=2,DEG ! Cycle over order
              CALL FP_AYBZ ( FSH%F1(J1,0)*Z, PM1_X, -FSH%F2(J1,0), PM2_X, P_X )
              PM2_X = PM1_X
              PM1_X = P_X
              FSH%PL(J1,0)  = FP_X_TO_F ( P_X )
              FSH%PLT(0,J1) = FSH%PL(J1,0)  
              CALL FP_XPA ( UM_X(J1), U, UM_X(J1-1) )
#ifdef ALF_SQ_COMP
              CALL FP_XZZ ( ALF_SQ_X, P_X )
#endif
#ifdef PRINT_PMN
              WRITE ( 6, 210 ) J1, 0, FSH%PL(J1,0)  
#endif
 410       CONTINUE 
      END IF
!
! --- Calculate P(m,m), P(m+1,m), and P(l,m)
!
      IF ( NORM == 1 .OR. NORM == 2 ) THEN
           PMM = SQRI(2)
         ELSE IF ( NORM == 4 ) THEN
           PMM = SQRI(2)/ SQRT(4.0D0*PI)
      END IF
      FACT = -1.0D0
      DO 420 J2=1,DEG-1 ! Cycle over order
         FACT = FACT + 2.0D0
!
! ------ Calculate P(m,m)
!
         IF ( NORM == 1 .OR. NORM == 4 ) THEN
              PMM = IPHS* PMM* SQRI(2*J2+1)/ SQRI(2*J2)
              PM2 = PMM
           ELSE IF ( NORM == 2 ) THEN
              PMM = IPHS* PMM/ SQRI(2*J2)
              PM2 = PMM
           ELSE IF ( NORM == 3 ) THEN
              PMM = IPHS* PMM* U* FACT
              PM2 = PMM
         END IF
         CALL FP_F_TO_X ( PMM, PM2_X )
         CALL FP_YZ     ( PM2_X, UM_X(J2), PM2_X, PUM )
         FSH%PL(J2,J2)  = PUM
         FSH%PLT(J2,J2) = PUM
#ifdef ALF_SQ_COMP
         CALL FP_XZZ ( ALF_SQ_X, PM2_X )
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J2, J2, PUM
#endif
!
! ------ Semi-sectorial (m+1,m) element
! ------ PM1: p(j5+1,j5)
!
         IF ( NORM == 2 ) THEN
              PMM = PMM* SQRI(2*J2+1)
         END IF
         IF ( NORM == 2 .OR. NORM == 3 ) THEN
              PM1 = Z* SQRI(2*J2+1)* PM2
            ELSE 
              PM1 = Z* SQRI(2*J2+3)* PMM
         END IF
         CALL FP_F_TO_X ( PM1, PM1_X )
         CALL FP_YZ     ( PM1_X, UM_X(J2), PM1_X, PUM )
!
! ------ Calculate P(m+1,m)
!		
         FSH%PL(J2+1,J2)  = PUM
         FSH%PLT(J2,J2+1) = PUM
#ifdef ALF_SQ_COMP
         CALL FP_XZZ ( ALF_SQ_X, PM1_X )
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J2+1, J2, PUM
#endif
!
! ------ Zonal terms
!
! ------ Calculate P(l,m)
!
         DO 430 J3=J2+2,DEG ! Cycle over degree
            CALL FP_AYBZ ( FSH%F1(J3,J2)*Z, PM1_X, -FSH%F2(J3,J2), PM2_X, P_X )
            PM2_X = PM1_X
            PM1_X = P_X
            FSH%PL(J3,J2)  = FP_X_TO_F ( P_X )
            FSH%PLT(J2,J3) = FSH%PL(J3,J2)  
#ifdef ALF_SQ_COMP
            CALL FP_XZZ ( ALF_SQ_X, P_X )
#endif
#ifdef PRINT_PMN
            WRITE ( 6, 210 ) J3, J2, FSH%PL(J3,J2)
#endif
 430     CONTINUE 
!
         IF ( NORM == 1 ) THEN
              DO 440 J4=1,J2-1
                 IF ( J4 == 1 ) THEN
                      FSH%DPLT(J4,J2) = ( SQRI(J2-1)*SQRI(J2+2)*FSH%PL(J2,2) - &
     &                                    SQRI(J2)*SQRI(J2+1)*FSH%PL(J2,0)     )/2.0D0
                    ELSE
                      FSH%DPLT(J4,J2) = ( SQRI(J2+J4+1)*SQRI(J2-J4)*FSH%PL(J2,J4+1) - &
     &                                    SQRI(J2+J4)*SQRI(J2-J4+1)*FSH%PL(J2,J4-1)   )/2.0D0
                 END IF
 440          CONTINUE 
              FSH%DPLT(0,J2)  =  SQRI(J2)*SQRI(J2+1)/SQRI(2)*FSH%PL(J2,1)
              FSH%DPLT(J2,J2) = -SQRI(J2)/SQRI(2)*FSH%PL(J2,J2-1)
         END IF
 420  CONTINUE 
!
! --- Calculate P(deg,deg)
!      	
      IF ( NORM == 3 ) THEN
           FACT = FACT + 2.0D0
           PMM = IPHS* PMM* FACT* Z
         ELSE IF ( NORM == 2 ) THEN
           PMM = IPHS* PMM/ SQRI(2*DEG)  
         ELSE IF ( NORM == 1 .OR. NORM == 4 ) THEN
           PMM = IPHS* PMM* SQRI(2*DEG+1)/ SQRI(2*DEG)  
      END IF
      FSH%PL(DEG,DEG)   = PMM* FP_X_TO_F ( UM_X(DEG) )
      FSH%PLT(DEG,DEG)  = FSH%PL(DEG,DEG)   
      IF ( NORM == 1 ) THEN
           DO 450 J5=1,DEG-1
              FSH%DPLT(J5,DEG) = ( SQRI(DEG+J5+1)*SQRI(DEG-J5)*FSH%PL(DEG,J5+1) - &
     &                             SQRI(DEG+J5)*SQRI(DEG-J5+1)*FSH%PL(DEG,J5-1)   )/2.0D0
 450       CONTINUE 
           FSH%DPLT(1,1) = -FSH%PL(1,0)
           FSH%DPLT(0,DEG)= SQRI(DEG)*SQRI(DEG+1)/SQRI(2)*FSH%PL(DEG,1)
           FSH%DPLT(DEG,DEG) = -SQRI(DEG)/SQRI(2)*FSH%PL(DEG,DEG-1)
      END IF
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) DEG, DEG, PMM
#endif
#ifdef ALF_SQ_COMP
      CALL FP_XFF    ( ALF_SQ_X, PMM*FP_X_TO_F ( UM_X(DEG) ) )
!
! --- Compute the sum of squares of associated Legendre function.
! --- In order to prevent accuracy loss, we use the Horner scheme
! --- for summation
!
      ALF_SQ_R8 = FP_X_TO_F ( ALF_SQ_X )
!$OMP CRITICAL
      WRITE ( 6, 110 ) (ALF_SQ_R8 - (DEG+1)**2)/ALF_SQ_R8
!$OMP END CRITICAL
 110     FORMAT ( 'SPH_LEG_F  Alf_sq_err= ', 1PD25.12 )
#endif
!    
      FSH%LAT       = LAT
      FSH%PL_STATUS = FSH__COMP
      CALL ERR_LOG ( 0, IUER )
      RETURN
      CONTAINS 
        INCLUDE 'x_ariphmetic.f'
      END  SUBROUTINE  SPHE_LEG_X  !#!  
