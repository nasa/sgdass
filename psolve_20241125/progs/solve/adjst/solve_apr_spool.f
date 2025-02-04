      SUBROUTINE SOLVE_APR_SPOOL ( LUN, BSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SOLVE_APR_SPOOL 
! *                                                                      *
! * ### 08-NOV-2006 SOLVE_APR_SPOOL v1.0 (c)  L. Petrov  08-NOV-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'bsp.i'
      INTEGER*4  LUN, IUER
      TYPE     ( BSPSTA__TYPE ) :: BSP(:)
      INTEGER*4  J1, J2
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! ---- Calc/Solve style of a priori
!
      WRITE ( 23, '(A,A)' ) '1  APR  APRIORI_STYLE:       ', 'CALC/SOLVE'
      WRITE ( 23, '(A,A)' ) '1  APR  SOLVE_STAPOS:        ', &
     &              STASUB_CHR(1:I_LEN(STASUB_CHR))
      WRITE ( 23, '(A,A)' ) '1  APR  SOLVE_STAVEL:        ', &
     &              VELSUB_CHR(1:I_LEN(VELSUB_CHR))
      WRITE ( 23, '(A,A)' ) '1  APR  SOLVE_SOUPOS:        ', &
     &              SRCSUB_CHR(1:I_LEN(SRCSUB_CHR))
      WRITE ( 23, '(A,A)' ) '1  APR  SOLVE_EOP:           ', &
     &              EOPDLY_CHR(1:I_LEN(EOPDLY_CHR))
      WRITE ( 23, '(A,A)' ) '1  APR  PRE_2005 HEO:        ', &
     &              HFEOPF_CHR(1:I_LEN(HFEOPF_CHR))
      WRITE ( 23, '(A,A)' ) '1  APR  POST_2005 HEO:       ', &
     &              FINAM_HEO(1:I_LEN(FINAM_HEO))
      WRITE ( 23, '(A,A)' ) '1  APR  ECCENTRICITY:        ', &
     &              ECCSUB_CHR(1:I_LEN(ECCSUB_CHR))
      WRITE ( 23, '(A,A)' ) '1  APR  AXIS OFFSETS:        ', &
     &              AXOSUB_CHR(1:I_LEN(AXOSUB_CHR))
      IF ( L_BSP > 0 ) THEN
           DO 410 J1=1,L_BSP
              WRITE ( 23, '(A,I1,A,A)' ) '1  APR  SPLINE_DSPL(',J1,'):      ', &
     &               BSP(J1)%FILE_NAME(1:I_LEN(BSP(J1)%FILE_NAME))
 410       CONTINUE 
         ELSE 
           WRITE ( 23, '(A,I1,A,A)' ) '1  APR  SPLINE_DSPL(',1,'):      ', &
     &               ' '
      END IF
!
      DO 420 J2=1,N_POSVAR
         WRITE ( 23, '(A,I1,A,A)' ) '1  APR  POSVAR_FIL(',J2,'):       ', &
     &                POSVAR_FIL(J2)(1:I_LEN(POSVAR_FIL(J2)))
         IF ( ILEN(POSVAR_FIL(J2)) == 0 ) GOTO 820
 420  CONTINUE 
 820  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SOLVE_APR_SPOOL  !#!#
