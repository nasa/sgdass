!
!---> common +
!
      INTEGER*4  LUN_SER, IPAR_HANDLER, SPD_NUM_PROC, SPD_PIDS(SPD__M_REQ), &
     &           REM_FDS(SPD__M_REQ), SUB_REM_FD
      CHARACTER  FILE_PID*128, IP_ADR_STR*16, REQ_IDS(SPD__M_REQ)*6
      COMMON   / SPD_SIGNAL_COMMON / LUN_SER, FILE_PID, IPAR_HANDLER, &
                                     SPD_NUM_PROC, SPD_PIDS, REM_FDS, REQ_IDS, &
     &                               SUB_REM_FD
!---> common -
!
