      SUBROUTINE READ_HTW_HEB ( HEB_DIR, DATE_HEB, HEB_H, HEB_T, HEB_W, IUER )
! ************************************************************************
! *                                                                      *
! *   Rpoutine READ_HTW_HEB 
! *                                                                      *
! * ### 30-JUL-2013  READ_HTW_HEB  v1.0 (c)  L. Petrov  30-JUL-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_H, HEB_T, HEB_W
      CHARACTER  HEB_DIR*(*), DATE_HEB*(*)
      CHARACTER  FINAM_TEMP*128, FINAM_H*128, FINAM_T*128, FINAM_W*128
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  IS, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Read H-file (geometric height)
!
      FINAM_H = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &          '/h/h_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb.bz2'
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FINAM_H, HEB_H, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8751, IUER, 'READ_HTW_HEB', 'Error in '// &
     &         'an attempt to read H-file '//FINAM_H )
           RETURN 
      END IF
!
! --- Read T-file (air temperature)
!
      FINAM_T = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &          '/t/t_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb.bz2'
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FINAM_T, HEB_T, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8751, IUER, 'READ_HTW_HEB', 'Error in '// &
     &         'an attempt to read T-file '//FINAM_T )
           RETURN 
      END IF
!
! --- Read W-file (partial water vapor pressure)
!
      FINAM_W = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &          '/w/w_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb.bz2'
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FINAM_W, HEB_W, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8751, IUER, 'READ_HTW_HEB', 'Error in '// &
     &         'an attempt to read W-file '//FINAM_W )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_HTW_HEB  !#!  
