      SUBROUTINE BIG_WRITE ( LUN, OFFS_I8, LEN_I8, ARR_I1, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine BIG_WRITE writes a big data array ARR_I1 (may be more than *
! *   2Gb) of LEN_I8 bytes long on offset OFFS_I8 in the file opened at  *
! *   the logical unit LUN. If the data array is longer than 128Mb,      *
! *   it is written in chunks of 128Mb long. This routine is needed in   *
! *   order to circumvent archaic restriction on the length of data of   *
! *   system primitive WRITE.                                            *
! *                                                                      *
! *  ### 26-APR-2016    BIG_WRITE  v1.0 (c)  L. Petrov  26-APR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  LUN, IUER
      INTEGER*8  OFFS_I8, LEN_I8
      INTEGER*1  ARR_I1(LEN_I8)
      INTEGER*8    DATA_CHUNK_LEN
      PARAMETER  ( DATA_CHUNK_LEN = 128*1024*1024 )
      CHARACTER  STR*128
      INTEGER*8  CHUNKS_TO_WRITE, OFFSET_RET, OFFS_WRITE, BYTES_TO_WRITE
      INTEGER*4  IS, SEEK_SET, ARG_LN, J1
      INTEGER*8, EXTERNAL :: LSEEK
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(OFFS_I8), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. OFFS_I8 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 1411, IUER, 'BIG_WRITE', 'Failure in '// &
     &         'position the file into beginning the section: '// &
     &          STR )
           RETURN 
      END IF
      CHUNKS_TO_WRITE = 1 + LEN_I8/DATA_CHUNK_LEN
      OFFS_WRITE = 0
      DO 410 J1=1,CHUNKS_TO_WRITE
         IF ( J1 == CHUNKS_TO_WRITE ) THEN
              BYTES_TO_WRITE = LEN_I8 - (J1-1)*DATA_CHUNK_LEN
              IF ( BYTES_TO_WRITE == 0 ) THEN
                   GOTO 810
              END IF
           ELSE 
              BYTES_TO_WRITE = DATA_CHUNK_LEN
         END IF
         IS = WRITE ( %VAL(LUN), %VAL(LOC(ARR_I1) + OFFS_WRITE), &
     &                %VAL(BYTES_TO_WRITE) )
         IF ( IS == -1 ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR ( STR )
              CALL ERR_LOG ( 1412, IUER, 'BIG_WRITE', 'Failure in '// &
     &            'writing data: '//STR )
              RETURN 
            ELSE IF ( IS < BYTES_TO_WRITE ) THEN
              CALL CLRCH ( STR )
              CALL INCH8 ( OFFS_WRITE + IS, STR )
              CALL ERR_LOG ( 1413, IUER, 'BIG_WRITE', 'Not all data '// &
     &            'were written, only '//STR(1:I_LEN(STR))//' bytes' )
              RETURN 
         END IF
         OFFS_WRITE = OFFS_WRITE + IS
 410  CONTINUE 
 810  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE BIG_WRITE  !#!#
