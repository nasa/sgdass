      SUBROUTINE MARK3_TO_GVF_NAME ( M_BUF, L_BUF, BUF, MARK3_NAME, &
     &                               GVF_NAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MARK3_TO_GVF_NAME  checks the lookup table and translate  *
! *   the MARK3-DBH name to the GVF name.                                *
! *                                                                      *
! * ### 16-FEB-2009 MARK3_TO_GVF_NAME v1.0 (c) L. Petrov 16-FEB-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INTEGER*4  M_BUF, L_BUF, IUER
      CHARACTER  BUF(*)*128, MARK3_NAME*(*), GVF_NAME*(*)
      CHARACTER  FINAM*128
      INTEGER*4  J1, J2, IER
!
      IF ( L_BUF .LE. 0 ) THEN
           FINAM = PRE_SAV_DIR(1:PRE_SV_LEN)//'/glo_to_gvf.csh'
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( FINAM, M_BUF, BUF, L_BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4861, IUER, 'MARK3_TO_GVF_NAME', 'Error in '// &
     &              'an attempt to read file '//FINAM )
                RETURN 
           END IF
      END IF
!
      CALL CLRCH  ( GVF_NAME )
      DO 410 J1=1,L_BUF
         IF ( MARK3_NAME == '$'//BUF(J1)(20:28) ) THEN
              GVF_NAME = BUF(J1)(64:73)
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MARK3_TO_GVF_NAME  !#!#
