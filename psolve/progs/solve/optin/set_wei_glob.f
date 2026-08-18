      SUBROUTINE SET_WEI_GLOB ( WEI_GLOB )
! ************************************************************************
! *                                                                      *
! *   Routine SET_WEI_GLOB sets all baseline wetight to WEI_GLOB         *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 29-JUN-2025  SET_WEI_GLOB  v1.0 (d)  L. Petrov  29-JUN-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      REAL*8     WEI_GLOB
      INTEGER*2  ICONT, IERR
      CHARACTER  JBUF(MAX4_BSL)*70 
      INTEGER*4  J1, J2, NWEI, IUER
!
      NWEI = 0
      DO 410 J1=1,MAX4_BSL
         IF ( J1 == 1 ) THEN
              ICONT = 1
            ELSE 
              ICONT = 0
         END IF
         CALL GETCARD ( 1, 'REWT', ICONT, JBUF(J1), IERR )
         IF ( IERR == 1 ) THEN
              NWEI = J1-1
              GOTO 810
         END IF
         IF ( IERR .NE. 0 ) THEN
              WRITE ( 6, * ) ' J1= ', J1
              IUER = -1
              CALL ERR_LOG ( 2341, IUER, 'SET_WEI_GLOB', 'Trap of internal '// &
     &            'control in reading NAMF file' )
              CALL EXIT ( 1 ) 
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( NWEI > 0 ) THEN
           DO 420 J2=1,NWEI
              IF ( J2 == 1 ) THEN
                   ICONT = 1
                ELSE 
                   ICONT = 0
              END IF
              WRITE ( UNIT=JBUF(J2)(23:32), FMT='(F10.2)' ) WEI_GLOB*1.D12
              IF ( JBUF(J2)(23:32) == '**********' ) JBUF(J2)(23:32) = '9999999.99' 
              CALL PUTCARD ( 1, 'REWT', ICONT, JBUF(J2), IERR )
 420       CONTINUE 
      END IF
!
      RETURN
      END  SUBROUTINE  SET_WEI_GLOB   !#!  
