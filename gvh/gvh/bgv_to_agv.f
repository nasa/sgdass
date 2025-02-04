      PROGRAM    BGV_TO_AGV
! ************************************************************************
! *                                                                      *
! *   
! *                                                                      *
! *  ### 12-OCT-2007               v1.0 (c)  L. Petrov  12-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      TYPE ( GVH__STRU ) ::  GVH
      CHARACTER  FIL_BGV*128, FIL_AGV*128
      CHARACTER  STGR*128
      INTEGER*4  J1, J2, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FIL_BGV = '/vlbi/gvf_db/20061111_r06315j_sl1_v002.bgv'
      IL = ILEN(FIL_BGV)
      FIL_AVG = FIL_BGV(1:IL-3)//'.agv'
!
      IUER = -1
      CALL GVH_INIT ( GVH, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_READ_BGV ( GVH, 1, FIL_BGV, REMAINED_BYTES, IUER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8901, IUER, 'BGV_TO_AGV', 'Error in '// &
     &         'an atttempt to read input database file '//BGV_TO_BGV )
           CALL EXIT ( 1 )
      END IF
      IF ( REMAINED_BYTES .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( REMAINED_BYTES, STR )
           CALL ERR_LOG ( 8902, IUER, 'BGV_TO_AGV', 'The number of '// &
     &         'remaining bytes after reading input databae file '// &
     &          BGV_TO_BGV(1:I_LEN(BGV_TO_BGV))//' is not 0, but '//STR )
           CALL EXIT ( 1 )
      END IF
!
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREGET ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8903, IUER, 'BGV_TO_AGV', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           CALL EXIT ( 1 )
      END IF
 
 
 
 
 
 
 
 
