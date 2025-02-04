      SUBROUTINE VTD_STA_SYN_REPL ( L_STA, C_STA ) 
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_STA_SYN_REPL
! *                                                                      *
! * ## 28-SEP-2008  VTD_STA_SYN_REPL v1.0 (c)  L. Petrov  28-SEP-2008 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  L_STA
      CHARACTER  C_STA(L_STA)*(*)
      INTEGER*4  J1
!
      DO 410 J1=1,L_STA
         IF ( C_STA(J1) == 'VLBA85_3' ) THEN
              C_STA(J1) = 'NRAO85_3'
            ELSE IF ( C_STA(J1) == 'WIDE85_3' ) THEN
              C_STA(J1) = 'NRAO85_3'
            ELSE IF ( C_STA(J1) == 'VLBA85_3' ) THEN
              C_STA(J1) = 'NRAO85_3'
            ELSE IF ( C_STA(J1) == 'MOJAVLBA' ) THEN
              C_STA(J1) = 'MOJAVE12'
            ELSE IF ( C_STA(J1) == 'LEFT85_1' ) THEN
              C_STA(J1) = 'NRAO85_1'
         END IF
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE VTD_STA_SYN_REPL  !#!  
