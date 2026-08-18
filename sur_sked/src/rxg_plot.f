      PROGRAM    RXG_PLOT
! ************************************************************************
! *                                                                      *
! *   Program    RXG_PLOT
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 11-JUL-2025    RXG_PLOT   v1.0 (d)  L. Petrov  11-JUL-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MP, MV, MIND
      PARAMETER  ( MP = 4*1024*1024 )
      PARAMETER  ( MV   =      1024 )
      PARAMETER  ( MIND =        32 )
      CHARACTER  FILIN*128, BUF(MP)*128
      REAL*8     FRQ(MV,2), RXG(MV,2)
      INTEGER*4  LIND, IND(2,MIND), NP, LP(2), J1, J2, J3, IUER
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage rxg_plot log_file'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILIN, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5601, IUER, 'RXG_PLOT', 'Cannot read '// &
     &         'input log file '//FILIN )
           CALL EXIT ( 1 ) 
      END IF
!
      LP = 0
      DO 410 J1=1,NP
         IF ( INDEX ( BUF(J1), '"' ) > 1 ) GOTO 410
         IF ( INDEX ( BUF(J1), 'rxg_file,' ) > 1 ) THEN
              CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//' ,', IUER )
              IF ( LIND == 5 ) THEN
                   IF ( ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), 'e+' ) > 0 .OR. &
     &                    INDEX ( BUF(J1)(IND(1,4):IND(2,4)), 'e-' ) > 0      ) .AND. &
     &                  ( INDEX ( BUF(J1)(IND(1,5):IND(2,5)), 'e+' ) > 0 .OR. &
     &                    INDEX ( BUF(J1)(IND(1,5):IND(2,5)), 'e-' ) > 0      )       ) THEN
!
                        IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'lcp' ) THEN
                             LP(1) = LP(1) + 1
                             READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F15.5)' ) FRQ(LP(1),1)
                             READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F15.5)' ) RXG(LP(1),1)
                          ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'rcp' ) THEN
                             LP(2) = LP(2) + 1
                             CALL TRAN ( 11, BUF(J1)(IND(1,4):IND(2,4)), BUF(J1)(IND(1,4):IND(2,4)) )
                             CALL TRAN ( 11, BUF(J1)(IND(1,4):IND(2,5)), BUF(J1)(IND(1,4):IND(2,5)) )
                             READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F15.5)' ) FRQ(LP(2),2)
                             READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F15.5)' ) RXG(LP(2),2)
                        END IF
                   END IF
              END IF
         END IF          
 410  CONTINUE 
!
      IF ( LP(1) > 0 .AND. LP(2) > 0 ) THEN
           FRQ(1:LP(1),1) = 1.D-3*FRQ(1:LP(1),1) 
           FRQ(1:LP(2),2) = 1.D-3*FRQ(1:LP(2),2) 
           CALL DIAGI_2 ( LP(1), FRQ(1,1), RXG(1,1), LP(2), FRQ(1,2), RXG(1,2), IUER )
        ELSE IF ( LP(1) > 0 ) THEN
           CALL DIAGI_1 ( LP(1), FRQ(1,1), RXG(1,1), IUER )
        ELSE IF ( LP(2) > 0 ) THEN
           CALL DIAGI_1 ( LP(2), FRQ(1,2), RXG(1,2), IUER )
      END IF 
!
      END  PROGRAM  RXG_PLOT  !#!#
