!
      SUBROUTINE solstat_sect()
!
!     WRITES OUT THE SOLUTION/STATISTICS SECTION OF THE 1996 IERS SITE
!     SUBMISSION
!
!     written 4/24/96 by kdb
!
!     modifications
!c
!     001101 remove dual cbuf_unknowns declarations
!
      IMPLICIT NONE
!
!     Input variables: none
!
!     Output variables: none
!
!     common files
!
      INCLUDE 'gsnoop_com.i'
!
!     local variables
!
      character*10  cbuf_unknowns
      character*16 cbuf_varfact
      integer*2 ict,inbl
!
!     Print out the output file.
!
!     First the header.
!
      write(99,"('+SOLUTION/STATISTICS')")
      write(99,"('*_STATISTICAL PARAMETER_______ __VALUE(S)', &
     &         '______________________________________')")
!
!     Print the IERS variance factor, which is the sum of the squares of the
!     residuals divided by the degrees of freedom.  (This essentially the
!     VLBI chi square delays value printed by hausr).
!
      write(cbuf_varfact,"(E16.10)") IERS_VAR_FACTOR
      if (cbuf_varfact(1:1).eq.' ') cbuf_varfact(1:1) = '0'
      write(99,"(' VARIANCE FACTOR',15X,A16)") cbuf_varfact
!
!     Print the IERS number of observations (the VLBI number of observations
!     plus the number of constraints).
!
      write(99,"(' NUMBER OF OBSERVATIONS',8X,F9.1)") iers_num_obs
!
!     Print the IERS number of unknowns (the total number of VLBI parameters
!     (arc parameters plus global ones)
!
      write(cbuf_unknowns,"(i10)") iers_unknowns
      do ict = 10,1,-1
        if (cbuf_unknowns(ict:ict).ne.' ') inbl = ict
      enddo
      write(99,"(' NUMBER OF UNKNOWNS',12X,A)") cbuf_unknowns(inbl:10)
!
!     Print the ending to the section.
!
      write(99,"('-SOLUTION/STATISTICS')")
!
      return
      end
