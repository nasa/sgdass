!
! >>>>> INCLUDE-BLOCK with descriptions of data structures used for
!       manipulations with correlation matrix.
!
!       include block solve.i SHOULD BE declared before!
!
!       corel.i  22-MAY-2001  v1.00  (c)  L. Petrov  --  22-MAY-2001 20:01:43
!
      INTEGER*4    M_BBUF
      PARAMETER  ( M_BBUF = 4096*8 )
      TYPE      CORL__STRU
          INTEGER*4  FIRST_MARKER
          INTEGER*4  LUN     ! Fortran channel to file
          INTEGER*4  M_PAR   ! Total number of parameters
          INTEGER*4  L_PAR   ! Parameters counter
          CHARACTER  C_PAR(M_GPA)*20  ! Parameters name list
          INTEGER*4  IXREF_PAR(M_GPA) ! Cross reference table from par to ind
          INTEGER*4  M_COR   ! Total number of correlations
          INTEGER*4  L_COR   ! Correlations counter
          INTEGER*4  M_BUF   ! Total number of bytes in a buffer
          INTEGER*4  L_BUF   ! Buffer counter
          BYTE       B_BUF(M_BBUF)  ! binary buffer
          CHARACTER  FILSPL*128  ! Filename
          INTEGER*4  STATUS
          INTEGER*4  LAST_MARKER
      END TYPE  CORL__STRU ! CORL__STRU !
      INTEGER*4  UND__CRL, INIT__CRL, CPAR__CRL, CORR__CRL, END__CRL
      PARAMETER  ( UND__CRL  = 1801 )
      PARAMETER  ( INIT__CRL = 1802 )
      PARAMETER  ( CPAR__CRL = 1803 )
      PARAMETER  ( CORR__CRL = 1804 )
      PARAMETER  ( END__CRL  = 1805 )
!
      INTEGER*4  COMM__CRL, NPAR__CRL, NCOR__CRL
      PARAMETER  ( COMM__CRL  = 1901 )
      PARAMETER  ( NPAR__CRL  = 1902 )
      PARAMETER  ( NCOR__CRL  = 1903 )
!
      CHARACTER  ASCII__CRL*48, BINARY__CRL*48
      PARAMETER  ( ASCII__CRL = &
     &           '# ASCII  CRL_SPOOL Format. Revision 2001.05.18  ' )
      PARAMETER  ( BINARY__CRL   = &
     &           '# Binary CRL_SPOOL Format. Revision 2001.05.18  ' )
!
! <<<<< end of INCLUDE-BLOCK  corel.i
!
