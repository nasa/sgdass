!This is the start of file &CORPAR
!
! modifications
!
! kdb 951101 Raise max_per from 2048 to 3000, at Brent Archinal's request.
! kdb 960514 Rename local max_par to max_summed_par to distinguish it from
!            the solve.i max_par.  Then set max_per to gsfcb.i's max_par.
!            Better documentation. Change max_summed_par from hard coded
!            number (2048000, which was apparently based on an earlier
!            limit of 2048 parameters per arc
!            for a maximum of 1000 arcs) to a formula based on
!            the number of arcs and the number of parameters per arc.
! pet 971105 Bound the value MAX_ARC with MAX_ARCS parameters specified in
!            solve.i
! pet 1999.10.13  Bound the value MAX_ARC with MAX_ARC_COREL, MAX_PER with
!                 MAX_PAR_COREL -- parameters specified in solve.i
! pet 2003.11.12  Changed syntax in order to compile under capricious 
!                 Intel Fortran 8.0
!
      INCLUDE 'solve.i'
!
!     max_arc APPEARS to be the maximum number of arcs supported.
!     max_per APPEARS to be the maximum number of covariance parameters
!             supported for an arc (that is, the maximum number of parameters
!             for which covariances may be found).
!     max_summed_par APPEARS to be the maximum number of covariance parameters
!             supported for all arcs in the solution.
!
      INTEGER*2 MAX_ARC,    MAX_PER
      INTEGER*4 MAX_ARC_I4, MAX_PER_I4
      INTEGER*4 MAX_SUMMED_PAR
      PARAMETER ( MAX_ARC        = MAX_ARC_COREL, &
     &            MAX_PER        = MAX_PAR_COREL, &
     &            MAX_ARC_I4     = MAX_ARC,       &
     &            MAX_PER_I4     = MAX_PER,       &
     &            MAX_SUMMED_PAR = MAX_ARC_I4*MAX_PER_I4 )
