! * ---------------------------------------------------------------------
! *
! *                              ASTMATH.CMN
! *
! *
! *  This file contains the declarations to include math constants in a
! *    subroutine. The data statements assign all the relevant values.
! *    This isn't a common per se, but rather a common data initialization.
! *    These values should not change within a program.
! *
! *                          Companion code for
! *             Fundamentals of Astrodynamics and Applications
! *                                  2007
! *                            by david vallado
! *
! *     (W) 719-573-2600, email dvallado@agi.com
! *
! *     *****************************************************************
! *
! *  Current :
! *            15 mar 07  david vallado
! *                         misc documentation updates
! *  Changes :
! *            28 feb 03  david vallado
! *                         original baseline
! *
! *     *****************************************************************

        REAL*8 Small,    Rad2Deg,  Deg2Rad,  HalfPi,                    &
     &         Pi,       TwoPi,    Infinite, Undefined

        DATA Small      /0.00000001D0/
        DATA Infinite   /999999.9D0/
        DATA Undefined  /999999.1D0/
! c        DATA Halfpi     /1.57079632679489662D0/
! c        DATA Pi         /3.14159265358979324D0/
! c        DATA TwoPi      /6.28318530717958648D0/
! c        DATA Rad2Deg    /57.2957795130823208D0/
! c        DATA Deg2Rad    /0.01745329251994329D0/

! c       use machine precision instead
        Pi      =  4.0D0 * DATAN(1.0D0)
        HalfPi  =  0.5D0*pi
        TwoPi   =  2.0D0*pi
        Rad2Deg = 180.0D0/pi
        Deg2Rad = pi/180.0D0


