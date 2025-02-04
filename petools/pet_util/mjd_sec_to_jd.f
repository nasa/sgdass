      FUNCTION MJD_SEC_TO_JD ( MJD, SEC )
! ************************************************************************
! *                                                                      *
! *   Function MJD_SEC_TO_JD transforms a date presented in the form     *
! *   MJD, SEC  to  Julian date.                                         *
! *                                                                      *
! * ### 23-DEC-2002  MJD_SEC_TO_JD  v1.0 (c)  L. Petrov  23-DEC-2002 ### *
! *                                                                      *
! ************************************************************************
      REAL*8     MJD_SEC_TO_JD
      INTEGER*4  MJD
      REAL*8     SEC
      MJD_SEC_TO_JD = 2400000.5D0 + MJD + SEC/86400.0D0
      RETURN
      END  !#!  MJD_SEC_TO_JD  #!#
