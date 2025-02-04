      SUBROUTINE JD_TO_MJD_SEC ( JD, MJD, SEC )
! ************************************************************************
! *                                                                      *
! *   Routine JD_TO_MJD_SEC transforms Julian date to the pair           *
! *   (MJD, SEC ).                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          JD  ( REAL*8    ) -- Julian date.                           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <JD_TO_DATE> ( CHARACTER ) -- Date and time in SOLVE format.         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *              MJD ( INTEGER*4 ) -- Integer MJD at the midnight of the *
! *                                   date under consideration.          *
! *              SEC ( REAL*8    ) -- Time in seconds from the midnight. *
! *                                                                      *
! *  ### 17-SEP-2004 JD_TO_MJD_SEC v1.0 (c)  L. Petrov  17-SEP-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     JD, SEC
      INTEGER*4  MJD
!
      MJD = IDINT ( JD - 2400000.5D0 + 1.D-8 )
      SEC = (JD - 2400000.5D0 - MJD)*86400.0D0
      IF ( SEC .LE. 0.0D0 ) THEN
           SEC = SEC + 86400.0D0
           MJD = MJD - 1
      END IF
!
      IF ( SEC .GE. 86400.0D0 ) THEN
           SEC = SEC - 86400.0D0
           MJD = MJD + 1
      END IF
      RETURN
      END  SUBROUTINE  JD_TO_MJD_SEC 
