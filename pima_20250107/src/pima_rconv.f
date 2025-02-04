      FUNCTION PIMA_RCONV ( N_LEVS, RDIG )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_RCONV performs non-linear conversion of the raw       *
! *   correlation coefficient produces by the digital correlator to the  *
! *   value that the perfect analogue correlator would have been         *
! *   produced. Thus, it re-normalizes the correlation coeffients and    *
! *   corrects the digitazation errors.                                  *
! *                                                                      *
! *  ### 20-DEC-2009  PIMA_RCONV   v1.0 (c)  L. Petrov  20-DEC-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima_rconv.i'
      INTEGER*4  N_LEVS
      REAL*4     PIMA_RCONV
      REAL*4     RDIG
      INTEGER*4  IXC
      INTEGER*4, EXTERNAL :: IXMN8 
      REAL*8,    EXTERNAL :: FSPL8
!
      IF ( RDIG < PIMA__RMIN .OR. &
     &     RDIG > PIMA__RMAX      ) THEN
           PIMA_RCONV = RDIG
         ELSE
           IF ( N_LEVS == 22 ) THEN
                PIMA_RCONV = 1.D0/P2I * ASIN(RDIG)
             ELSE IF ( N_LEVS == 24 ) THEN
                IXC = IXMN8 ( N_SPL, ARG_SPL, DBLE(RDIG) )
                PIMA_RCONV = FSPL8 ( DBLE(RDIG), N_SPL, ARG_SPL, FUN_24, &
     &                               IXC, COEF_SPL_24 )
             ELSE IF ( N_LEVS == 44 ) THEN
                IXC = IXMN8 ( N_SPL, ARG_SPL, DBLE(RDIG) )
                PIMA_RCONV = FSPL8 ( DBLE(RDIG), N_SPL, ARG_SPL, FUN_44, &
     &                               IXC, COEF_SPL_44 )
             ELSE 
                PIMA_RCONV = RDIG
         END IF
      END IF
      RETURN
      END  FUNCTION  PIMA_RCONV  !#!
!
! ------------------------------------------------------------------------
!
      FUNCTION PIMA_RCONV_AIPS ( N_LEVS, RDIG )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_RCONV performs non-linear conversion of the raw       *
! *   correlation coefficient produces by the digital correlator to the  *
! *   value that the perfect analogue correlator would have been         *
! *   produced. Thus, it re-normalizes the correlation coeffients and    *
! *   corrects the digitazation errors.                                  *
! *                                                                      *
! * ### 28-SEP-2009 PIMA_RCONV_AIPS v1.0 (c) L. Petrov  28-SEP-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'pima_rconv.i'
      INTEGER*4  N_LEVS
      REAL*4     PIMA_RCONV_AIPS
      REAL*4     RDIG
!
      IF ( RDIG < PIMA__RMIN .OR. &
     &     RDIG > PIMA__RMAX      ) THEN
           PIMA_RCONV_AIPS = RDIG
         ELSE
           IF ( N_LEVS == 22 ) THEN
                PIMA_RCONV_AIPS = R_CONV_22 ( NINT(RDIG*PIMA__RSCALE) )
              ELSE IF ( N_LEVS == 24 ) THEN
                PIMA_RCONV_AIPS = R_CONV_24 ( NINT(RDIG*PIMA__RSCALE) )
              ELSE IF ( N_LEVS == 44 ) THEN
                PIMA_RCONV_AIPS = R_CONV_44 ( NINT(RDIG*PIMA__RSCALE) )
              ELSE
                PIMA_RCONV_AIPS = 0.0
           END IF
      END IF
      RETURN
      END  FUNCTION  PIMA_RCONV_AIPS  !#!
