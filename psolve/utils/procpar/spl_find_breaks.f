      PROGRAM    SPL_FIND_BREAK
! ************************************************************************
! *                                                                      *
! *   Program SPL_FIND_BREAK generates a list of clock breaks from       *
! *   a given listing of a VLBI solutions.                               *
! *                                                                      *
! * ### 21-AUG-2018  SPL_FIND_BREAK v1.1 (c)  L. Petrov  11-DEC-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MP
      PARAMETER  ( MP = 4*1024*1024 )
      CHARACTER  FIL*128, BUF(MP)*256, DB_NAME*10, EXP_NAME*8
      INTEGER*4  J1, J2, NP, IUER
!
!      FIL = '/vlbi/solutions/rfc_2018b/rfc_2018b_xs.spl'
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage spl_find_break splool_file' 
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FIL )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 410 J1=1,NP
         IF ( BUF(J1)(1:10) == ' Data base' ) THEN
              DB_NAME = BUF(J1)(12:21)
              EXP_NAME = BUF(J1)(31:39)
            ELSE IF ( BUF(J1)(6:6) == '.' .AND. BUF(J1)(17:20) == 'BR 0' ) THEN
              WRITE ( 6, '(A)' ) DB_NAME//'  '//EXP_NAME//'   '//BUF(J1)(8:20)//' '//BUF(J1)(60:74)//'  '//BUF(J1)(22:44)
         END IF
 410  CONTINUE 
      END  PROGRAM    SPL_FIND_BREAK  !#!#
