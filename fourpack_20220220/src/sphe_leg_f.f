#define BALF_SQ_COMP
#define BPRINT_PMN
      SUBROUTINE SPHE_LEG_F ( FSH, DEG, NORM, IPHS, LAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPHE_LEG_F evaluates all of the normalized associated      *
! *   Legendre functions and its partial derivatives over eastern and    *
! *   northern direction up to degree DEG. The functions are initially   *
! *   scaled by 10^280 sin^m in order to minimize the effects of         *
! *   underflow at large m near the poles (see Holmes and Featherstone   *
! *   2002, J. Geodesy, 76, 279-299).                                    *
! *                                                                      *
! *   On i386 and x86_64 architectures with a maximum allowable double   *
! *   precision value of 2.225073858507203E-308 the scaled portion of    *
! *   the algorithm will not overflow for degrees less than or equal     *
! *   to 2800.                                                           *
! *                                                                      *
! *   For each value of degree m, the re-scaling factor is computed as   *
! *   rescalem=rescalem*sin(theta), with the initial value of rescalem   *
! *   being equal to 1/scalef (which is here equal to 10^280). This      *
! *   will gradually reduce this huge number to a tiny number, and will  *
! *   ultimately underflow. In order to prevent this underflow, when     *
! *   rescalem becomes less than 10^(-280), the subsequent re-scaling    *
! *   factors of sin(theta) will be directly applied to Plm, and then    *
! *   this number will be multiplied by the old value of rescalem.       *
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
! * ### 21-AUG-2012  SPHE_LEG_F v2.0 modified by L. Petrov 19-OCT-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  DEG, IPHS, NORM, IUER
      REAL*8     LAT
      LOGICAL*4  FL_ERROR
      REAL*16    ALF_SQ, ALF_ORD_SQ(0:FSH__MAX_DEG)
      CHARACTER  STR*128, STR1*128
      REAL*8     PLM, PMM, PM1, PM2, SCALEF, RESCALEM, U, Z, FACT
      INTEGER*4  J1, J2, J3, J4, J5, J6, IER
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6611, IUER, 'SPHE_LEG_F', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
!
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6612, IUER, 'SPHE_LEG_F', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
!
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6613, IUER, 'SPHE_LEG_F', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( NORM < 1 .OR. NORM > 4 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NORM, STR )
           CALL ERR_LOG ( 6614, IUER, 'SPHE_LEG_F', 'Wrong value '// &
     &         'of NORM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 4] was expected' )
           RETURN 
      END IF
!
      IF ( IPHS .NE. 1 .AND. IPHS .NE. -1 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IPHS, STR )
           CALL ERR_LOG ( 6615, IUER, 'SPHE_LEG_F', 'Wrong value '// &
     &         'of IPHS: '//STR(1:I_LEN(STR))//' -- either 1 or -1 '// &
     &         'was expected' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF ( FSH, DEG, NORM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6616, IUER, 'SPHE_LEG_F', 'Error in '// &
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
                CALL ERR_LOG ( 6617, IUER, 'SPHE_LEG_F', 'Failure in '// &
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
                CALL ERR_LOG ( 6618, IUER, 'SPHE_LEG_F', 'Failure in '// &
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
                     CALL ERR_LOG ( 6619, IUER, 'SPHE_LEG_F', 'Failure in '// &
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
      SCALEF = 1.0D-280
!
! --- Calculate P(l,0). These are not scaled.
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
      ALF_ORD_SQ(1:DEG) = REAL(0.0,KIND=16)
      ALF_ORD_SQ(0) = REAL(PM2,KIND=16)**2
      ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(PM1,KIND=16)**2
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, 0, PM2
      WRITE ( 6, 210 ) 1, 0, PM1
 210  FORMAT ( 'SPHE_LEG_F: Deg: ', I5, ' Ord: ', I5, ' Pmn= ', 1PD22.15 )
#endif
!				
      IF ( DEG .GE. 2 ) THEN
           DO 410 J1=2,DEG ! Cycle over order
              PLM = Z* FSH%F1(J1,0)* PM1 - FSH%F2(J1,0)* PM2
              FSH%PL(J1,0)  = PLM
              FSH%PLT(0,J1) = PLM
              PM2 = PM1
              PM1 = PLM
#ifdef ALF_SQ_COMP
              ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(PLM,KIND=16)**2
#endif
#ifdef PRINT_PMN
              WRITE ( 6, 210 ) J1, 0, PLM
#endif
 410       CONTINUE 
      END IF
!
! --- Calculate P(m,m), P(m+1,m), and P(l,m)
!
      IF ( NORM == 1 .OR. NORM == 2 ) THEN
           PMM = SQRI(2)* SCALEF
         ELSE IF ( NORM == 4 ) THEN
           PMM = SQRI(2)* SCALEF/ SQRT(4.0D0*PI)
      END IF
      RESCALEM = 1.0D0/ SCALEF
      FACT = -1.0D0
      DO 420 J2=1,DEG-1 ! Cycle over order
         RESCALEM = RESCALEM* U
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
              RESCALEM = 1.D0
              PMM = IPHS* PMM* U* FACT
              PM2 = PMM
         END IF
         FSH%PL(J2,J2)  = PMM* RESCALEM
         FSH%PLT(J2,J2) = FSH%PL(J2,J2)  
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(J2) = ALF_ORD_SQ(J2) + REAL(PMM,KIND=16)**2
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J2, J2, PM2*RESCALEM
#endif
!
         IF ( NORM == 2 ) THEN
              PMM = PMM* SQRI(2*J2+1)
         END IF
!
! ------ Calculate P(m+1,m)
!		
         IF ( NORM == 2 .OR. NORM == 3 ) THEN
              PM1 = Z* SQRI(2*J2+1)* PM2
            ELSE 
              PM1 = Z* SQRI(2*J2+3)* PMM
         END IF
         FSH%PL(J2+1,J2)  = PM1*RESCALEM
         FSH%PLT(J2,J2+1) = FSH%PL(J2+1,J2)  
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(J2) = ALF_ORD_SQ(J2) + REAL(PM1,KIND=16)**2
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J2+1, J2, PM1*RESCALEM
#endif
!
! ------ Calculate P(l,m)
!
         DO 430 J3=J2+2,DEG ! Cycle over degree
            PLM = Z* FSH%F1(J3,J2)* PM1 - FSH%F2(J3,J2)* PM2
            FSH%PL(J3,J2)  = PLM* RESCALEM
            FSH%PLT(J2,J3) = FSH%PL(J3,J2)  
            PM2  = PM1
            PM1  = PLM
#ifdef ALF_SQ_COMP
            ALF_ORD_SQ(J2) = ALF_ORD_SQ(J2) + REAL(PLM,KIND=16)**2
#endif
#ifdef PRINT_PMN
            WRITE ( 6, 210 ) J3, J2, PLM*RESCALEM
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
      RESCALEM = RESCALEM* U
      IF ( NORM == 3 ) THEN
           FACT = FACT + 2.0D0
           RESCALEM = 1.0D0
           PMM = IPHS* PMM* FACT* Z
         ELSE IF ( NORM == 2 ) THEN
           PMM = IPHS* PMM/ SQRI(2*DEG)  
         ELSE IF ( NORM == 1 .OR. NORM == 4 ) THEN
           PMM = IPHS* PMM* SQRI(2*DEG+1)/ SQRI(2*DEG)  
      END IF
      FSH%PL(DEG,DEG)   = PMM* RESCALEM
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
      WRITE ( 6, 210 ) DEG, DEG, PMM*RESCALEM
#endif
#ifdef ALF_SQ_COMP
      ALF_ORD_SQ(DEG) = REAL(PMM,KIND=16)**2
!
! --- Compute the sum of squares of associated Legendre function.
! --- In order to prevent accuracy loss, we use the Horner scheme
! --- for summation
!
      ALF_SQ = 0.0
      DO 460 J6=DEG,1,-1
         ALF_SQ = (ALF_SQ + ALF_ORD_SQ(J6))*(REAL(U,KIND=16))**2
 460  CONTINUE
      ALF_SQ = ALF_ORD_SQ(0) + ALF_SQ/REAL(SCALEF,KIND=16)**2
!$OMP CRITICAL
      WRITE ( 6, 110 ) (ALF_SQ - (DEG+1)**2)/ALF_SQ
!$OMP END CRITICAL
 110     FORMAT ( 'SPH_LEG_F  Alf_sq_err= ', 1PD25.12 )
#endif
!    
      FSH%LAT       = LAT
      FSH%PL_STATUS = FSH__COMP
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_LEG_F  !#!  
