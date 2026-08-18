      PROGRAM    GTEC_IONO_ERR
! ************************************************************************
! *                                                                      *
! *   Program GTEC_IONO_ERR updatezs pSolve control file 
! *                                                                      *
! * ### 16-SEP-2018  GTEC_IONO_ERR  v1.0 (c) L. Petrov 29-DEC-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4    MS, MIND
      PARAMETER  ( MS = 256*1024 )
      PARAMETER  ( MIND = 128 )
      CHARACTER  FILGTEC*128
      CHARACTER  BUFG(MS)*64, SESS_NAME*12, DATE_STR*21, STR*16
      REAL*8     GTEC_VAL, GTEC_MEAN, GT1, GT2, GT3, AL1, AL2, AL3, DR1, DR2, &
     &           SH1, SH2, ALP
      INTEGER*4  J1, J2, J3, J4, J5, LIND, IND(2,MIND), NC, NG, KP, IP, IUER 
!
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gtec_iono_err gtec_file sess_name' 
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILGTEC )
           CALL GETARG ( 2, SESS_NAME )
      END IF
!
      GT1 =  7.0 ; AL1 = 0.60
      GT2 = 20.0 ; AL2 = 0.40
      GT3 = 60.0 ; AL3 = 0.30
      DR1 = (AL2 - AL1)/(GT2 - GT1)
      DR2 = (AL3 - AL2)/(GT3 - GT2)
!
      IUER = -1
      CALL RD_TEXT ( FILGTEC, MS, BUFG, NG, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DATE_STR = SESS_NAME(1:4)//'.'//SESS_NAME(5:6)//'.'//SESS_NAME(7:8)//'_00:00:00.0'
      KP = 0
      GTEC_MEAN = 0.0D0
      DO 420 J2=1,NG
         IF ( BUFG(J2)(1:10) == DATE_STR(1:10) ) THEN
              READ ( UNIT=BUFG(J2)(25:31), FMT=* ) GTEC_VAL
              GTEC_MEAN = GTEC_MEAN + GTEC_VAL 
              KP = KP + 1
         END IF
 420  CONTINUE 
      IF ( KP > 0 ) THEN
           GTEC_MEAN = GTEC_MEAN/KP
           IF ( GTEC_MEAN < GT2 ) THEN
                ALP = AL1 + DR1*(GTEC_MEAN - GT1)
             ELSE
                ALP = AL2 + DR2*(GTEC_MEAN - GT2)
           END IF
           WRITE ( UNIT=6, FMT='(F5.3)' ) ALP
         ELSE
           WRITE ( UNIT=6, FMT='(F6.3)' ) -1.0
      END IF
!
      END  PROGRAM  GTEC_IONO_ERR  !#!#
