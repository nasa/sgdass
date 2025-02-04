!@This is the start of file &PRECM
!
!  AEE 920220  Used bit 6 of word 2 of pre_ip as foreground/background flag.
!  pet 2000.07.05  Added PRE_SOL_DIR, PRE_LCL_DIR, PRE_SOL_LEN, PRE_ROOT_LEN
!  pet 2001.12.13  Added PRE_SPL_NAM, PRE_SPL_LEN, DBH_PROG
!  pet 2004.11.12  Added TEST_FIELD for testing
!
      CHARACTER*2 PRE_LETRS
      CHARACTER   PRE_SCR_DIR*128,  PRE_SAV_DIR*128, PRE_SOL_DIR*128, &
     &            PRE_ROOT_DIR*128, PRE_SPL_NAM*128
      INTEGER*2 PRE_SV_LEN, PRE_SD_LEN, PRE_ROOT_LEN, PRE_SOL_LEN, &
     &          PRE_SPL_LEN
      INTEGER*2 PRE_IP(5), PRE_FIRST, PRE_INTERA, PRE_IBATCH, PRE_ILETRS
      INTEGER*4 PIPE_IDS(4), DBH_PROG, TEST_FIELD
      LOGICAL*2 KTESTV,KSPOOL,KBACKSL,KSCREEN,KMINOUT,KBATCH,KFULLOUT
      LOGICAL*2 KGLOBALS, KLCLBSL, KGLBBSL, KPOSELL
      COMMON / PRECOM /  DBH_PROG, PIPE_IDS, PRE_IP, &
     &                   PRE_FIRST, PRE_INTERA, PRE_IBATCH, &
     &                   PRE_LETRS, &
     &                   KSPOOL, KTESTV, &
     &                   KBACKSL, KSCREEN, KMINOUT, KBATCH, KFULLOUT, &
     &                   KGLOBALS, KLCLBSL, KGLBBSL, KPOSELL, &
     &                   PRE_SD_LEN, PRE_SV_LEN, PRE_SOL_LEN, PRE_ROOT_LEN, &
     &                   PRE_SPL_LEN, TEST_FIELD
      EQUIVALENCE ( PRE_LETRS, PRE_ILETRS )
!
      COMMON / PRECOMCH / PRE_SCR_DIR, PRE_SAV_DIR, PRE_SOL_DIR, PRE_ROOT_DIR, &
     &                    PRE_SPL_NAM
!
