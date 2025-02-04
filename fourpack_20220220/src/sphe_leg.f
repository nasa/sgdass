#define BALF_SQ_COMP
#define BPRINT_PMN
      SUBROUTINE SPHE_LEG ( FSH, DEG, NORM, IPHS, LAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPHE_LEG_F evaluates all of the normalized associated      *
! *   Legendre functions and its partial derivatives over eastern and    *
! *   northern direction up to degree DEG.                               *
! *                                                                      *
! *   For degree/order less than 2800 it uses algorityhm of Holmes and   *
! *   Featherstone 2002, J. Geodesy, 76, 279-299 that rescales the       *
! *   intermediate values by 1.D-280. For degree/order greater than 2800 *
! *   it uses Fukushima algorithm tghat involes algebra of X-numbers.    *
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
! * ### 21-AUG-2012  SPHE_LEG v1.0 modified by L. Petrov 19-OCT-2015 ### *
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
           CALL ERR_LOG ( 6651, IUER, 'SPHE_LEG', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
!
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6652, IUER, 'SPHE_LEG', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
!
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6653, IUER, 'SPHE_LEG', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( NORM < 1 .OR. NORM > 4 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NORM, STR )
           CALL ERR_LOG ( 6654, IUER, 'SPHE_LEG_F', 'Wrong value '// &
     &         'of NORM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 4] was expected' )
           RETURN 
      END IF
!
      IF ( IPHS .NE. 1 .AND. IPHS .NE. -1 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IPHS, STR )
           CALL ERR_LOG ( 6655, IUER, 'SPHE_LEG_F', 'Wrong value '// &
     &         'of IPHS: '//STR(1:I_LEN(STR))//' -- either 1 or -1 '// &
     &         'was expected' )
           RETURN 
      END IF
!
! --- First check debugging variables and decide whether we have to force
! --- F-version of X-versions. This is only for debugging purposes!
!
      IF ( FSH%FF_FORCE_F == FSH__ENFORCE ) THEN
           WRITE ( 6, '(A)' ) 'SPHE_LEG: F-version'
           CALL SPHE_LEG_F ( FSH, DEG, NORM, IPHS, LAT, IUER )
           CALL ERR_LOG ( IER, IUER )
           RETURN
         ELSE IF ( FSH%FF_FORCE_X == FSH__ENFORCE ) THEN
           WRITE ( 6, '(A)' ) 'SPHE_LEG: X-version'
           CALL SPHE_LEG_X ( FSH, DEG, NORM, IPHS, LAT, IUER )
           CALL ERR_LOG ( IER, IUER )
           RETURN
      END IF
!
! --- If the dimension of the transform is below the threshold,
! --- use the F-version. Otherwise, use the X-version of the transform
!
      IF ( DEG .LE. FSH__MAX_SCL ) THEN
           CALL SPHE_LEG_F ( FSH, DEG, NORM, IPHS, LAT, IUER )
         ELSE 
           CALL SPHE_LEG_X ( FSH, DEG, NORM, IPHS, LAT, IUER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_LEG  !#!  
