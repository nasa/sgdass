      SUBROUTINE BIG_READ ( LUN, OFFS_I8, LEN_I8, ARR_I1, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine BIG_READ 
! *                                                                      *
! *  ### 27-JAN-2016    BIG_READ   v1.0 (c)  L. Petrov  27-JAN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  LUN, IUER
      INTEGER*8  OFFS_I8, LEN_I8
      INTEGER*1  ARR_I1(LEN_I8)
      INTEGER*8    DATA_CHUNK_LEN
      PARAMETER  ( DATA_CHUNK_LEN = 128*1024*1024 )
      CHARACTER  STR*128
      INTEGER*8  CHUNKS_TO_READ, OFFSET_RET, OFFS_READ, BYTES_TO_READ
      INTEGER*4  IS, SEEK_SET, ARG_LN, J1
      INTEGER*8, EXTERNAL :: LSEEK
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(OFFS_I8), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. OFFS_I8 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 1411, IUER, 'BIG_READ', 'Failure in '// &
     &         'position the file into beginning the section: '// &
     &          STR )
           RETURN 
      END IF
      CHUNKS_TO_READ = 1 + LEN_I8/DATA_CHUNK_LEN
      OFFS_READ = 0
      DO 410 J1=1,CHUNKS_TO_READ
         IF ( J1 == CHUNKS_TO_READ ) THEN
              BYTES_TO_READ = LEN_I8 - (J1-1)*DATA_CHUNK_LEN
              IF ( BYTES_TO_READ == 0 ) THEN
                   GOTO 810
              END IF
           ELSE 
              BYTES_TO_READ = DATA_CHUNK_LEN
         END IF
         IS = READ ( %VAL(LUN), %VAL(LOC(ARR_I1) + OFFS_READ), &
     &               %VAL(BYTES_TO_READ) )
         IF ( IS == -1 ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR ( STR )
              CALL ERR_LOG ( 1412, IUER, 'BIG_READ', 'Failure in '// &
     &            'reading data: '//STR )
              RETURN 
         END IF
         OFFS_READ = OFFS_READ + IS
 410  CONTINUE 
 810  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE BIG_READ   !#!  
