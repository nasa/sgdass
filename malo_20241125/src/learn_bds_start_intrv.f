      SUBROUTINE LEARN_BDS_START_INTRV ( BDS_DIR,  SMP_INTRV,   &
     &                                   BDS_MJD_BEG, BDS_TAI_BEG, &
     &                                   BDS_MJD_END, BDS_TAI_END, &
     &                                   L_STA, C_STA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine LEARN_BDS_START_INTRV
! *                                                                      *
! * # 20-JUN-2014 LEARN_BDS_START_INTRV v1.0 (c) L. Petrov 20-JUN-2014 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'bindisp.i'
      CHARACTER  BDS_DIR*(*), C_STA(M__BDSLEN)*(*)
      REAL*8     SMP_INTRV, BDS_TAI_BEG, BDS_TAI_END
      INTEGER*4  L_STA, BDS_MJD_BEG, BDS_MJD_END, IUER
      CHARACTER  FILBDS*128, BUF(M__BDSLEN)*128, STR*128
      LOGICAL*1  LEX
      INTEGER*4  J1, J2, J3, J4, N_BDS, I_STA, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FILBDS = BDS_DIR(1:I_LEN(BDS_DIR))//'/bds_summary.txt'
      INQUIRE ( FILE=FILBDS, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4833, IUER, 'LEARN_BDS_START_INTRV', 'Cannot '// &
     &         'find summary in directory '//BDS_DIR )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILBDS, M__BDSLEN, BUF, N_BDS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4834, IUER, 'LEARN_BDS_START_INTRV', 'Cannot '// &
     &         'find summary in directory '//BDS_DIR )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(BINDISP_SUMMARY__LABEL)) .NE. BINDISP_SUMMARY__LABEL ) THEN
           CALL CLRCH ( STR ) 
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 4835, IUER, 'LEARN_BDS_START_INTRV', 'Unsupported '// &
     &         'summary file. Its label it is '// &
     &          STR(1:LEN(BINDISP_SUMMARY__LABEL))// &
     &         ' while label "'//BINDISP_SUMMARY__LABEL//'" was expected' )
           RETURN 
      END IF
!
      I_STA = 0
      DO 410 J1=1,N_BDS
         IF ( BUF(J1)(1:10) == 'MIN_EPOCH:' ) THEN
              READ ( UNIT=BUF(J1)(12:16), FMT='(I5)'    ) BDS_MJD_BEG
              READ ( UNIT=BUF(J1)(18:24), FMT='(F7.1)'  ) BDS_TAI_BEG
            ELSE IF ( BUF(J1)(1:10) == 'MAX_EPOCH:' ) THEN
              READ ( UNIT=BUF(J1)(12:16), FMT='(I5)'    ) BDS_MJD_END
              READ ( UNIT=BUF(J1)(18:24), FMT='(F7.1)'  ) BDS_TAI_END
            ELSE IF ( BUF(J1)(1:10) == 'SMP_INTRV:' ) THEN
              READ ( UNIT=BUF(J1)(12:23), FMT='(F12.5)' ) SMP_INTRV
            ELSE IF ( BUF(J1)(1:6) == 'L_STA:' ) THEN
              READ ( UNIT=BUF(J1)(8:18), FMT='(I11)'    ) L_STA
            ELSE IF ( BUF(J1)(1:4) == 'STA:' ) THEN
              I_STA = I_STA + 1
              C_STA(I_STA) = BUF(J1)(11:18)
         END IF
 410  CONTINUE 
      IF ( SMP_INTRV < 10.0 ) THEN
           CALL ERR_LOG ( 4836, IUER, 'LEARN_BDS_START_INTRV', 'Trap of internal '// &
     &         'control: SMP_INTRV is too small. Please check the '// &
     &         'summary file '//FILBDS )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LEARN_BDS_START_INTRV  !#!#
