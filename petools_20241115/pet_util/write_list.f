      SUBROUTINE WRITE_LIST ( FINAM, TITLE, L_LIS, C_LIS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRITE_LIST writes down the character list in to the output *
! *   file FINAM.                                                        *
! *                                                                      *
! *  ### 27-MAR-2002   WRITE_LIST  v1.0 (c)  L. Petrov  27-MAR-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  L_LIS, IUER
      CHARACTER  FINAM*(*), TITLE*(*), C_LIS(L_LIS)*(*)
      INTEGER*4  LUN, J1, IOS
      INTEGER*4, EXTERNAL :: I_LEN, GET_UNIT
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( LUN, * ) 'IOS = ',IOS
           CALL ERR_LOG ( 1271, IUER, 'WRITE_LIST', 'Error in an attempt '// &
     &         'to open the output file '//FINAM )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) TITLE(1:I_LEN(TITLE))
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) 'IOS = ',IOS
           CALL ERR_LOG ( 1272, IUER, 'WRITE_LIST', 'Error in an attempt '// &
     &         'to write in the file '//FINAM )
           RETURN
      END IF
!
      DO 410 J1=1,L_LIS
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) C_LIS(J1)(1:I_LEN(C_LIS(J1)))
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) 'IOS = ',IOS
              CALL ERR_LOG ( 1273, IUER, 'WRITE_LIST', 'Error in an attempt '// &
     &            'to write in the file '//FINAM )
              RETURN
         END IF
 410  CONTINUE
!
      CLOSE ( UNIT=LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_LIST  #!#
