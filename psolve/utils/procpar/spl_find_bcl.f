      PROGRAM    SPL_FIND_BCL
! ************************************************************************
! *                                                                      *
! *   Program  SPL_FIND_BCL  generates a list of baseline-dependent      *
! *   clocks from that asceed by modulo a specified threshold in         *
! *   a given listing of a VLBI solutions.                               *
! *                                                                      *
! * ### 21-AUG-2018  SPL_FIND_BCL v1.1 (c)  L. Petrov  11-DEC-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MP
      PARAMETER  ( MP = 4*1024*1024 )
      CHARACTER  FIL*128, BUF(MP)*256, STR*128, DB_NAME*10
      REAL*8     CLK_BRK, MAX_CLK_BRK
      INTEGER*4  J1, J2, NP, IUER
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage spl_find_bck splool_file min_break_in_ps' 
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL )
           CALL GETARG ( 2, STR )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
           READ ( UNIT=STR, FMT='(F10.1)' ) MAX_CLK_BRK
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 410 J1=1,NP
         IF ( BUF(J1)(1:10) == ' Data base' ) THEN
              DB_NAME = BUF(J1)(12:21)
           ELSE IF ( BUF(J1)(6:6) == '.' .AND. BUF(J1)(26:37) == 'Clock offset' ) THEN
              READ ( UNIT=BUF(J1)(56:71), FMT='(F15.3)' ) CLK_BRK
              IF ( DABS(CLK_BRK) > MAX_CLK_BRK ) THEN
                   WRITE ( 6, '(A)' ) DB_NAME//' '//BUF(J1)(8:37)//' '//BUF(J1)(56:74)
              END IF
         END IF
 410  CONTINUE 
      END  PROGRAM    SPL_FIND_BCL  !#!#
