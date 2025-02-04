      SUBROUTINE SPHE_COMP_VEC_TEST ( FSH, MD, DEG, LAT_VAL, LON_VAL, NORM, &
     &                         IPHS, SPH, RES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_COMP_VEC  determines the value and its derivative    *
! *   towards north and east at a given latitude and longitude           *
! *   corresponding to the given set of spherical harmonics.             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     FSH ( SPHE_TYPE ) -- Internal data structure that keeps internal *
! *                          arrays with intermediate results and their  *
! *                          status for possible re-use.                 *
! *      MD ( INTEGER*4 ) -- Dimension of the array for spherical        *
! *                          function.                                   *
! *     DEG ( INTEGER*4 ) -- Maximum degree of the transform. Should not *
! *                          exceed MD.                                  *
! * LAT_VAL ( REAL*8    ) -- Latitude of the point in radians.           *
! * LON_VAL ( REAL*8    ) -- Longitude of the point in radians.          *
! *    NORM ( INTEGER*4 ) -- Normalization to be used when calculating   *
! *                          Legendre functions                          *
! *                          1 -- "geodesy";                             *
! *                          2 -- Schmidt;                               *
! *                          3 -- unnormalized;                          *
! *                          4 -- orthonormalized;                       *
! *    IPHS ( INTEGER*4 ) -- Phase flag.                                 *
! *                          1: Do not include the Condon-Shortley phase *
! *                             factor of (-1)^m.                        *
! *                         -1: Apply the Condon-Shortley phase factor   *
! *                             of (-1)^m.                               *
! *    SPH ( REAL*8    ) -- Array with spherical transform coefficients. *
! *                         Dimension: (2,0:MD,0:MD). The first dimesion *
! *                         runs over cosine/sine compoenent, the second *
! *                         dimension runs over order l, the third       *
! *                         dimension runs over degre.                   *
! *                         NB: only coefficients l =< m are filled!     *
! *                         The part of array SPH l > m is filled with   *
! *                         zeroes.                                      *
! *    RES ( REAL*8    ) -- Vector of results: value, north derivative,  *
! *                         east derivative. Dimension: 3.               *
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
! * # 06-NOV-2012 SPHE_COMP_VEC v1.0 modified by L. Petrov 06-NOV-2012 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  MD, NORM, IPHS, DEG, IUER
      REAL*8     SPH(2,0:MD,0:MD,2), LAT_VAL, LON_VAL, RES(3)
      CHARACTER  STR*128, STR1*128
      REAL*8     SCALEF 
      INTEGER*4  DEG_MAX
      INTEGER*4  J1, J2, J3, NUM_THR_SAVED, NTHR, IER
      COMPLEX*16 COEF(2*DEG+3), COEFS(2*DEG+3), TEMPC
      REAL*8     X, Z, DPL, Z_MIN
      PARAMETER  ( Z_MIN = 1.D-5 )
      LOGICAL*1  FL_ERROR 
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_NUM_THREADS
!
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6741, IUER, 'SPHE_COMP_VEC', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
!
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6742, IUER, 'SPHE_COMP_VEC', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
!
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6743, IUER, 'SPHE_COMP_VEC', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( DEG > MD ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( MD,  STR1 )
           CALL ERR_LOG ( 6744, IUER, 'SPHE_COMP_VEC', 'Wrong DEG > MD: '// &
     &         'Deg: '//STR(1:I_LEN(STR))//' MD: '//STR1(1:I_LEN(STR1)) )
           RETURN 
      END IF 
!
      IF ( NORM < 1 .OR. NORM > 4 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NORM, STR )
           CALL ERR_LOG ( 6745, IUER, 'SPHE_COMP_VEC', 'Wrong value '// &
     &         'of NORM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 4] was expected' )
           RETURN 
      END IF
!
      IF ( IPHS .NE. 1 .AND. IPHS .NE. -1 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IPHS, STR )
           CALL ERR_LOG ( 6746, IUER, 'SPHE_COMP_VEC', 'Wrong value '// &
     &         'of IPHS: '//STR(1:I_LEN(STR))//' -- either 1 or -1 '// &
     &         'was expected' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF ( FSH, DEG, NORM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6747, IUER, 'SPHE_COMP_VEC', 'Error in '// &
     &         'an attempt to precompute multiplicative factors used '// &
     &         ' in recursion relationships F1 and F2' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_LEG ( FSH, DEG, NORM, IPHS, LAT_VAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6748, IUER, 'SPHE_COMP_VEC', 'Error in '// &
     &         'an attempt to precompute Legendre polynomials' )
           RETURN 
      END IF
!
      IF ( FSH%MS_STATUS == FSH__COMP              .AND. &
     &     FSH%MS_DEG    == DEG                    .AND. &
     &     DABS(FSH%LON - LON_VAL ) < FSH__ANG_EPS       ) THEN
!
           CONTINUE 
         ELSE IF ( FSH%MS_STATUS .NE. FSH__COMP  .OR. &
     &             FSH%MS_DEG    .NE. DEG             ) THEN
!
!$OMP      CRITICAL
           IF ( ASSOCIATED ( FSH%MSIN ) ) THEN
                DEALLOCATE ( FSH%MSIN )
           END IF
           ALLOCATE ( FSH%MSIN(DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*DEG, STR )
                CALL ERR_LOG ( 6749, IUER, 'SPHE_COMP_VEC', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%MSIN' )
                FL_ERROR = .TRUE.
                GOTO 810
           END IF
!
           IF ( ASSOCIATED ( FSH%MCOS ) ) THEN
                DEALLOCATE ( FSH%MCOS )
           END IF
           ALLOCATE ( FSH%MCOS(DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*DEG, STR )
                CALL ERR_LOG ( 6750, IUER, 'SPHE_COMP_VEC', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%MCOS' )
                FL_ERROR = .TRUE.
                GOTO 810
           END IF
!
           FSH%MS_STATUS = FSH__ALLO
           FSH%MS_DEG  = DEG
 810       CONTINUE 
!$OMP      END CRITICAL
      END IF
      IF ( FSH%MS_STATUS == FSH__COMP              .AND. &
     &     FSH%MS_DEG    == DEG                    .AND. &
     &     DABS(FSH%LON - LON_VAL ) < FSH__ANG_EPS       ) THEN
           CONTINUE 
         ELSE
!
! -------- M times cosine of longitude
!
           FSH%MCOS(1) = DCOS(LON_VAL)
           FSH%MSIN(1) = DSIN(LON_VAL)
           DO 410 J1=2,DEG
              FSH%MCOS(J1) = FSH%MCOS(J1-1)*FSH%MCOS(1) - FSH%MSIN(J1-1)*FSH%MSIN(1)
              FSH%MSIN(J1) = FSH%MSIN(J1-1)*FSH%MCOS(1) + FSH%MCOS(J1-1)*FSH%MSIN(1)
 410       CONTINUE 
           FSH%MS_STATUS = FSH__COMP
           FSH%LON = LON_VAL
      END IF
!
      SCALEF = 1.0D-280
      X = SIN(LAT_VAL)
      Z = COS(LAT_VAL)
      IF ( Z < Z_MIN ) Z = Z_MIN
!
      RES = 0.0D0
!
      IF ( .NOT. OMP_IN_PARALLEL() ) THEN
           NUM_THR_SAVED = OMP_GET_NUM_THREADS()
           CALL OMP_SET_NUM_THREADS ( %VAL(FSH%NUM_THR) )
           NTHR = FSH%NUM_THR
         ELSE 
!
! -------- Do serial if we are already in the parallel region
!
           NTHR = 1
      END IF
!@ !$OMP PARALLEL DO IF ( NTHR > 1 ), &
!@ !$OMP   PRIVATE ( J2, J3, ), REDUCTION (+: RES, NL, NH), SCHEDULE ( DYNAMIC )
      DO 420 J2=DEG,0,-1
         RES(1) = RES(1) + SPH(1,J2,0,1)*FSH%PL(J2,0)
         IF ( J2 > 0 ) THEN
              DPL = SQRI(J2)*SQRI(J2+1)/SQRI(2)*FSH%PL(J2,1)
!@              RES(3) = RES(3) + SPH(1,J2,0,2)*DPL
           ELSE 
              DPL = 0.0D0
         END IF
         RES(3) = RES(3) + SPH(1,J2,0,2)*FSH%DPLT(0,J2)
!@   write ( 6, * ) ' j2= ', int2(j2), ' dpl= ', dpl, fsh%dplt(0,j2) ! %%%%%%%%%%%%
         DO 430 J3=1,J2
            RES(1) = RES(1) + &
     &                  (  SPH(1,J2,J3,1)*FSH%MCOS(J3) + SPH(2,J2,J3,1)*FSH%MSIN(J3) )* &
     &               FSH%PLT(J3,J2)
            RES(2) = RES(2) + &
     &               J3*( -SPH(1,J2,J3,2)*FSH%MSIN(J3) + SPH(2,J2,J3,2)*FSH%MCOS(J3) )* &
     &               FSH%PLT(J3,J2)
            IF ( J3 == 1 ) THEN
                 IF ( J2 == 1 ) THEN
                      DPL = -FSH%PL(1,0)
                    ELSE 
                      DPL = ( SQRI(J2-1)*SQRI(J2+2)*FSH%PL(J2,2) - &
     &                        SQRI(J2)*SQRI(J2+1)*FSH%PL(J2,0)     )/2.0D0
                 END IF
               ELSE IF ( J3 == J2 ) THEN
                 DPL = -SQRI(J2)/SQRI(2)*FSH%PL(J2,J2-1)
               ELSE
                 DPL = ( SQRI(J2+J3+1)*SQRI(J2-J3)*FSH%PL(J2,J3+1) - &
     &                   SQRI(J2+J3)*SQRI(J2-J3+1)*FSH%PL(J2,J3-1)   )/2.0D0
            END IF
!            RES(3) = RES(3) + &
!     &                  (  SPH(1,J2,J3,2)*FSH%MCOS(J3) + SPH(2,J2,J3,2)*FSH%MSIN(J3) )* &
!     &                  DPL
            RES(3) = RES(3) + &
     &                  (  SPH(1,J2,J3,2)*FSH%MCOS(J3) + SPH(2,J2,J3,2)*FSH%MSIN(J3) )* &
     &                  FSH%DPLT(J3,J2)
! if ( j3== 2 )    write ( 6, * ) ' j2= ', int2(j2), ' dpl= ', dpl, fsh%dplt(j3,j2) ! %%%%%%%%%%%%
 430     CONTINUE 
 420  CONTINUE 
      RES(2) = RES(2)/Z
!@ !$OMP END PARALLEL DO
      IF ( .NOT. OMP_IN_PARALLEL() ) THEN
           CALL OMP_SET_NUM_THREADS ( %VAL(NUM_THR_SAVED) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_COMP_VEC_TEST  !#!  
