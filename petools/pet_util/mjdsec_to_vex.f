      FUNCTION   MJDSEC_TO_VEX ( MJD, SEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Function  MJDSEC_TO_VEX  transforms the date specified by a pair   *
! *   of MJD and SEC to the VEX format.                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *              MJD ( INTEGER*4 ) -- Integer MJD at the midnight of the *
! *                                   date under consideration.          *
! *              SEC ( REAL*8    ) -- Time in seconds from the midnight. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <MJDSEC_TO_VEX> ( CHARACTER ) -- Date and time in Vex format.        *
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
! * ### 13-OCT-2005   MJDSEC_TO_VEX   v1.3 (c) L. Petrov 11-JAN-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  MJDSEC_TO_VEX*22, JD_TO_DATE*23
      INTEGER*4  MJD, IUER
      REAL*8     SEC, J2000__JD   
      INTEGER*4  J2000__MJD  
      PARAMETER  ( J2000__MJD =   51544     ) ! 2000.01.01_00:00:00
      PARAMETER  ( J2000__JD  = 2451545.0D0 ) ! 2000.01.01_12:00:00
      INTEGER*4  MJD_BEG, IDAY, IER
      REAL*8     SEC_BEG
      CHARACTER  DAT*23
!
      DAT = JD_TO_DATE ( J2000__JD + (MJD-J2000__MJD) + &
     &                              SEC/86400.0D0 - 0.5D0, IUER )
      CALL DATE_TO_TIME ( DAT(1:4)//'.01.01_00:00:00.000', MJD_BEG, SEC_BEG, IER )
!
      IDAY = INT ( SEC/86400.0D0 )
      IF ( SEC < 0.0D0 ) IDAY = IDAY - 1
      WRITE   ( UNIT=DAT(6:8), FMT='(I3)' ) MJD - MJD_BEG + IDAY + 1
      CALL CHASHR  ( DAT(6:8) )
      CALL BLANK_TO_ZERO ( DAT(6:8) )
      MJDSEC_TO_VEX = DAT(1:4)//'y'//DAT(6:8)//'d'//DAT(12:13)//'h'// &
     &                DAT(15:16)//'m'//DAT(18:23)//'s'
!
      RETURN
      END  FUNCTION   MJDSEC_TO_VEX  !#!#
