!
!     common for chain subroutines which may access the A900 catalog
!     via the srv_ routines, via the A900 server
!
!     id_dcb - the A900 server is a single program which must open
!              files for multiple users.  ID_DCB identifies which
!              set of dcbs a user owns for accessing the hashfile and
!              catalog file(s)
!
!     s_or_x - mode catalog opened in (shared or exclusive)
!              (flag to tell server whether it can remove (a/the) simulated
!               (shared/exclusive) fortran open lock
!                  1 = exclusive   0 = shared
!
!     ipot  - tells whether production or test catalog (= 1 for production
!                                                       = 2 for test)
!
!
      INTEGER*2 id_dcb,s_or_x,ipot
      COMMON /SERVCOMU/ id_dcb,s_or_x,ipot
