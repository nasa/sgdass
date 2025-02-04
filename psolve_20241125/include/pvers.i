!@This is the start of file PVERS
!  (used to store program version information)
!
!  Last update 15-OCT-2003 09:31:33
!
      INTEGER*2     NUM_PROGS, LEN_PROGS, LEN_DATES, LEN_COMMENTS
      PARAMETER   ( NUM_PROGS = 42 )
      PARAMETER   ( LEN_PROGS = 5, LEN_DATES = 10, LEN_COMMENTS = 32 )
      CHARACTER     PROG_NAMES(NUM_PROGS)*(LEN_PROGS)
      CHARACTER     PROG_DATES(NUM_PROGS)*(LEN_DATES)
      CHARACTER     PROG_COMMENTS(NUM_PROGS)*(LEN_COMMENTS)
!
      CHARACTER    CUR_PROG*5, CUR_DATE*10, CUR_COMMENT*32, CUR_VERSION*54
      CHARACTER    CUR_PROG_LONG*32
      INTEGER*4    PVERS_FIRST, PVERS_LAST, IFREE_BYTES
      PARAMETER  ( IFREE_BYTES = 188 ) ! Unused space in bytes (47 per program)
      BYTE         PVSERS_FILLER(IFREE_BYTES)
!
      COMMON / PVERS / &
     &         PVERS_FIRST, &
!
     &         PROG_NAMES, &
     &         PROG_DATES, &
     &         PROG_COMMENTS, &
!
     &         CUR_PROG, &
     &         CUR_DATE, &
     &         CUR_COMMENT, &
     &         CUR_VERSION, &
     &         CUR_PROG_LONG, &
!
     &         PVSERS_FILLER, &
     &         PVERS_LAST
