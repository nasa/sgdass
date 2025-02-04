      PROGRAM    DIAGI_COLOR_DEMO
! ************************************************************************
! *                                                                      *
! *   Program DIAGI_COLOR_DEMO demonstrates DIAIG colors 1, 2, 3 that    *
! *   SGDFASS uses for Good, Neutral, and Bad points.                    *
! *                                                                      *
! * ### 26-FEB-2024  DIAGI_COLOR_DEMO v1.0 (c) L. Petrov 26-FEB-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  NPTS
      PARAMETER  ( NPTS = 128 )
      REAL*4     XL, XR, YB, YT, XC, YC, PAP_SIZE, XCOEF, RAD, &
     &           XRAD_WC, YRAD_WC, XARR(NPTS), YARR(NPTS)
      CHARACTER  CH*1
      INTEGER*4  IDEV, WHITE_COLOR, IUER
      INTEGER*4, EXTERNAL :: PGBEG
!
!     setenv DIAGI_COLOR /opt64/share/sgdass_color_cividis.txt
!     setenv DIAGI_COLOR /opt64/share/sgdass_color_grey.txt
!
      XCOEF = 1.0
      PAP_SIZE = 160.0
      RAD      = 0.04
      IDEV  = 1
!
! --- Setting plotting parameters
!
      DIAGI_S%IDEV = IDEV
      CALL DIAGI_SET ( 1, DIAGI_S )
!
! --- Openning plotting device
!
      IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
!
! --- Setting colours
!
      DIAGI_S%NCLR  = 32
      CALL DIAGI_CLS ( DIAGI_S, IUER )
!
! --- Setting whitish background
!
      CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
      WHITE_COLOR = BCG_CLR(1)
      CALL PGPAP   ( PAP_SIZE/25.4,  XCOEF )
      CALL PGSVP   (  0.0, 1.0,  0.0, 1.0  )
!
! --- Setting new world coodrinates
!
      CALL PGSWIN  ( -1.1, 1.1, -1.1, 1.1 )
!
! --- Setting default font type
!
      CALL PGSCF  ( 2 )
      CALL PGERAS()       ! Erase the screen
!
      CALL PGSAVE()
      CALL PGBBUF()
      CALL PGSCI   ( 1 )
      CALL PGSCF   ( 2 )
!
! --- Write the box
!
      XL = -0.91
      XR =  0.91
      YB = -0.6  
      YT =  0.6
      CALL PGSCI   ( WHITE_COLOR )
      CALL PGRECT  ( XL, XR, YB, YT )
      CALL PGSCI   ( 1 )
      CALL PGSLW   ( 3 )
      XARR(1) = XL; YARR(1) = YB; XARR(2) = XR; YARR(2) = YB; CALL PGLINE  ( 2, XARR, YARR )
      XARR(1) = XR; YARR(1) = YB; XARR(2) = XR; YARR(2) = YT; CALL PGLINE  ( 2, XARR, YARR )
      XARR(1) = XR; YARR(1) = YT; XARR(2) = XL; YARR(2) = YT; CALL PGLINE  ( 2, XARR, YARR )
      XARR(1) = XL; YARR(1) = YT; XARR(2) = XL; YARR(2) = YB; CALL PGLINE  ( 2, XARR, YARR )
!
! --- Show the Good point
!
      XC = -0.7
      YC =  0.2
      CALL PGSLW ( 5 )
      CALL PGSFS ( 1 )
      CALL PGSCI ( ITAB_CLR(1,1) )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGSFS ( 2 )
      CALL PGSCI ( 1 )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGSLW  ( 3 )
      CALL PGSCH  ( 1.6 )
      CALL PGTEXT ( XC+0.2, YC-0.02, 'Good point' )
!
! --- Show the Neutral point
!
      XC = -0.7
      YC =  0.0
      CALL PGSLW ( 5 )
      CALL PGSFS ( 1 )
      CALL PGSCI ( ITAB_CLR(2,1) )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGSFS ( 2 )
      CALL PGSCI ( 1 )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGSLW  ( 3 )
      CALL PGSCH  ( 1.6 )
      CALL PGTEXT ( XC+0.2, YC-0.02, 'Neutral point' )
!
! --- Show the Bad point
!
      XC = -0.7
      YC = -0.2
      CALL PGSLW ( 5 )
      CALL PGSFS ( 1 )
      CALL PGSCI ( ITAB_CLR(3,1) )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGSFS ( 2 )
      CALL PGSCI ( 1 )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGCIRC_PET ( NPTS, XC, YC, RAD, RAD )
      CALL PGSLW  ( 3 )
      CALL PGSCH  ( 1.6 )
      CALL PGTEXT ( XC+0.2, YC-0.02, 'Bad point' )
!
      XC = -0.9
      YC = -0.8
      CALL PGSCH  ( 1.25 )
      CALL PGTEXT ( XC, YC, 'Use environment DIAGI_COLOR to set the color space' )
!
      CALL PGEBUF()
      CALL PGUNSA()
      CALL PGUPDT()
!
! --- Hang the cursor at the end
!
      XC = 0.95
      YC = 0.0
      CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
      CALL PGCLOQ()
      END  PROGRAM  DIAGI_COLOR_DEMO  !#!#
