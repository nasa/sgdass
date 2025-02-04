      PROGRAM    DE_EPH_MAIN
      IMPLICIT   NONE 
      INCLUDE   'de440_eph.i'
      CHARACTER  FILEPH*128, NAM_PLAN*16, DATE_CHR*21
      TYPE ( DE_EPH__TYPE ) :: DE_EPH
      INTEGER*4  J1, MJD
      REAL*8     TAI, COO(3), VEL(3), ACC(3), COO1(3), VEL1(3), ACC1(3)
      INTEGER*4  IUER
!
#ifdef HPUX
      FILEPH = '/data1/apriori_files/JPL.DE403'
#else
      FILEPH = '/apr/eph/de440.eph'
#endif
!
      IUER = -1
      CALL READ_DE_EPH ( FILEPH, DE_EPH, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      WRITE ( 6, * ) ' DE_EPH%TIT = ', DE_EPH%TIT
      WRITE ( 6, * ) ' DE_EPH%DATE_BEG_JD = ', DE_EPH%DATE_BEG_JD 
      WRITE ( 6, * ) ' DE_EPH%DATE_END_JD = ', DE_EPH%DATE_END_JD 
      WRITE ( 6, * ) ' DE_EPH%STEP_DAY    = ', DE_EPH%STEP_DAY
      WRITE ( 6, * ) ' DE_EPH%NCON        = ', DE_EPH%NCON
      WRITE ( 6, * ) ' DE_EPH%NUMDE       = ', DE_EPH%NUMDE
!
      DATE_CHR = '1999_08_01:00:00:00.0'
      CALL DATE_TO_TIME ( DATE_CHR, MJD, TAI, IUER )
      NAM_PLAN = 'JUPITER'
      IUER = -1
      CALL PLANETA_DE_EPH ( DE_EPH, MJD, TAI, NAM_PLAN, COO, VEL, ACC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      WRITE ( 6, '(A)' ) 'Date: '//DATE_CHR
      WRITE ( 6, 110 ) NAM_PLAN, 2400000.5D0 + MJD + TAI/86400.0D0, &
     &                 COO(1), COO(2), COO(3)
 110  FORMAT ( A, ' Date: ',F13.5, ' Coo: ', 3(F17.2,1X) )
      WRITE ( 6, 120 ) NAM_PLAN, 2400000.5D0 + MJD + TAI/86400.0D0, &
     &                 VEL(1), VEL(2), VEL(3)
 120  FORMAT ( A, ' Date: ',F13.5, ' Vel: ', 3(F15.7,1X) )
      WRITE ( 6, 130 ) NAM_PLAN, 2400000.5D0 + MJD + TAI/86400.0D0, &
     &                 ACC(1), ACC(2), ACC(3)
 130  FORMAT ( A, ' Date: ',F13.5, ' Acc: ', 3(F15.9,1X) )
      END  !#!  DE_EPH_MAIN  #!#
