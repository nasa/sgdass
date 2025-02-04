!
!@csolsys.i
!
      Real*8 SPLANET(3,2,7), GPLANET(3,2,7), SJD
      COMMON / CSOLSYS / SPLANET, GPLANET, SJD
!  Mods:
!     98.07.22 - D. Gordon, common block name changed from 'SOLSYS' to
!                  'CSOLSYS' to avoid conflicts with G. Kaplan's 'NOVAS'
!                  routines at USNO (B. Archinal suggestion).
!
!
!       1. SPLANET(3,2,7)  -  The J2000.0 Solar System Barycentric positions
!                             and velocities of all planets except the Earth
!                             and Pluto. (meters, meters/sec) The first index
!                             runs over X,Y, and Z, the second runs over
!                             position and velocity, and the third runs over
!                             the planets, where
!                                    1 = Mercury
!                                    2 = Venus
!                                    3 = Mars
!                                    4 = Jupiter
!                                    5 = Saturn
!                                    6 = Uranus
!                                    7 = Neptune
!
!       2. GPLANET(3,2,7)  -  The J2000.0 Geocentric positions and velocities
!                             of all planets except the Earth and Pluto.
!                             (meters, meters/sec) The first index runs over
!                             X,Y, and Z, the second runs over position and
!                             velocity, and the third runs over the planets,
!                             where
!                                    1 = Mercury
!                                    2 = Venus
!                                    3 = Mars
!                                    4 = Jupiter
!                                    5 = Saturn
!                                    6 = Uranus
!                                    7 = Neptune
!
!       3. SJD            -   Initially the time of the previous ovservation.
!                             After subroutine PEP it is the current time.
!                             (Days)
