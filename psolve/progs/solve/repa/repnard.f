      SUBROUTINE REPNARD  ( REC_NUM, DBFB, DBFBV, DBFBB, RNB )
! 
!C
!C    PURPOSE:        read scratch file NAMFxx
!C
!C    INPUT:          NAMFxx (direct access file)
!C
!C    PARAMETERS:
!C
!C    REC_NUM (out)   REC_NUM(1)  total # of loaded bands
!C                    REC_NUM(2)  total # of records in NAMFxx
!C                    REC_NUM(3)  # of first record of band 1 in NAMFxx
!C                    REC_NUM(4)  # of first record of band 2 in NAMFxx
!C                    ....
!C                    REC_NUM(MAX_DBS) # of first record of band 14 in NAMFxx
!C
!C                    !! contradiction: MAX_DBS=15 in solve.i !!
!C                    !! but in NAMFxx only 14 db areas in 1st record!!
!C    DBFB (out)      database names (1,...,14):
!C                    DBFB(1)     name of band 1
!C                    ....
!C                    DBFB(14)    name of band 14
!C
!C    DBFBV (out)     version numbers (1,...,14)
!C
!C    DBFBB (out)     kind of band: X or S (1,...,14)
!C
!C    RNB (out)       numbers of last record in residual file RESFxx
!C                    (1,...,14)
!C
!C    MAX_DBS (in)    max. number of databases in scratch files
!C                    (parameter in solve.i)
!C
!C    called subroutines:
!C    none
!C
!C    calling routine:
!C    REPA
!C
!C    02-08-02   Gerald Engelhardt  Wrote
!C    2003.09.24 Leonid Petrov      Updated. Removed opening file
!C
      INCLUDE 'solve.i'
      INCLUDE 'namfl.i'
      INCLUDE 'precm.i'
!
      INTEGER*4     J1
      INTEGER*4     REC_NUM(MAX_DBS+2), RNB(MAX_DBS)
      CHARACTER*9   DBFB(MAX_DBS)
      CHARACTER*4   DBFBV(MAX_DBS)
      CHARACTER*1   DBFBB(MAX_DBS)
!
! --- initialize field REC_NUM
!
      DO J1= 1, MAX_DBS + 2
         REC_NUM( J1 ) = 0
      END DO
!
! --- get buffer
!
      READ ( UNITNAM, REC=1 ) KBUF
!
! --- read buffer
!
      READ ( KBUF, '( 4X, 16I4 )' ) (REC_NUM( J1 ), J1 = 1, 16)
!
      DO J1 = 1, REC_NUM( 1 )
         READ ( UNITNAM, REC = REC_NUM( J1 + 2 ) ) KBUF
         READ ( KBUF, '( 11X, A9, A4, 15X, I7, X, A1 )' ) &
     &          DBFB( J1 ),DBFBV( J1 ),RNB( J1 ),DBFBB(J1)
      END DO
!
      RETURN
      END  !#!  REPNARD   #!#

