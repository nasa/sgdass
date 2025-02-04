      SUBROUTINE READ_HEB_SECT ( FILIN, HEB, IND3_SECT, IND4_SECT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_HEB_SECT
! *                                                                      *
! * ### 22-FEB-2013  READ_HEB_SECT v1.4 (c)  L. Petrov  13-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*(*), INTERNET_HOSTNAME*64, SYSNAME*128, HARDWARE*128, &
     &           TMP_DIR*128
      INTEGER*4  IND3_SECT(2), IND4_SECT(2), IUER
      CHARACTER  FILTMP*128, STR*128, COM*256
      LOGICAL*1  FL_BZIP2
      INTEGER*4  IS, IL, PID, NTHR, IER
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: GETPID, SYSTEM, ILEN, I_LEN, OMP_GET_THREAD_NUM
!
      CALL GETINFO_HOST ( INTERNET_HOSTNAME )
      IF ( INTERNET_HOSTNAME == 'localhost' ) THEN
           CALL GETINFO_SYSTEM ( SYSNAME, INTERNET_HOSTNAME, HARDWARE )
      END IF
!
      IL = ILEN(FILIN)
      IF ( IL < 4 ) IL = 4
      IF ( FILIN(IL-3:IL) == '.bz2' ) THEN
           PID = GETPID()
           CALL INCH ( PID, FILTMP(1:8) )
           CALL CHASHR    ( FILTMP(1:8) )
           CALL BLANK_TO_ZERO ( FILTMP(1:8) )
           IF ( OMP_IN_PARALLEL() ) THEN
                FILTMP(9:9) = '_'
                CALL INCH ( OMP_GET_THREAD_NUM(), FILTMP(10:13) )
                CALL CHASHR    ( FILTMP(10:13) )
                CALL BLANK_TO_ZERO ( FILTMP(10:13) )
           END IF 
!          
           IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &          INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &          INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &          INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &          INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'   .OR. &
     &          INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a'                  ) THEN
                TMP_DIR = '/dev/shm'
             ELSE 
                TMP_DIR = '/tmp'
           END IF
           FILTMP = TRIM(TMP_DIR)//'/'//FILTMP(1:I_LEN(FILTMP))//'.heb'
!
! -------- Honor environemnet variable OMP_NUM_THREADS.
! -------- We limit the number of threads for lbzip2
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CALL CHIN ( STR, NTHR )
                IF ( NTHR < 1 ) NTHR = 1
                CALL CLRCH ( STR ) 
                CALL INCH  ( NTHR, STR )
                STR = '-n '//STR
              ELSE
!
! ------------- ... or do not use any limit when the variable is not set up
!
                CALL CLRCH ( STR )
           END IF
!
           FL_BZIP2 = .TRUE.
           IF ( OMP_IN_PARALLEL() ) THEN
                COM = 'lbzip2 -n 1 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
              ELSE 
                COM = 'lbzip2 '//STR(1:I_LEN(STR))//' -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
           END IF
           IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                WRITE ( 6, * ) 'System: IS = ', IS
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6281, IUER, 'READ_HEB_SECT', 'Failure to '// &
     &              'uncompress the input heb-file '// &
     &              FILIN(1:I_LEN(FILIN))//' using command '// &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN 
           END IF
         ELSE
           FL_BZIP2 = .FALSE.
           FILTMP = FILIN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_HEADER ( FILTMP, HEB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6282, IUER, 'READ_HEB', 'Failure to parse '// &
     &         'the header of the input heb-file '//FILTMP )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL HEB_READ_DATA_SECT ( FILTMP, IND3_SECT, IND4_SECT, HEB, IER )
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6283, IUER, 'READ_HEB', 'Failure to read '// &
     &         'the data section of the input heb-file '//FILTMP )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
      END IF
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE READ_HEB_SECT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_READ_DATA_SECT ( FILIN, IND3_SECT, IND4_SECT, HEB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  HEB_READ_DATA_SECT
! *                                                                      *
! * ## 29-JAN-2013 HEB_READ_DATA_SECT v2.4 (c) L. Petrov 05-MAR-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*(*)
      CHARACTER  HDR*(HEB__HDS)
      INTEGER*1, ALLOCATABLE :: ARR_I1(:)
      INTEGER*2, ALLOCATABLE :: ARR_I2(:)
      INTEGER*4  IND3_SECT(2), IND4_SECT(2), IUER
      CHARACTER  STR*128
      INTEGER*4  LUN, IER, IS, SEEK_SET, SEEK_CUR, ARG_LN
      INTEGER*8  J1, J2, J3, J4
      INTEGER*8  CHUNKS_TO_READ, BYTES_TO_READ, OFFS, SHFT, &
     &           OFFSET_RET, LEN_ELEM, LEN_ALLOC, LEN_TO_READ
      INTEGER*8, EXTERNAL :: LSEEK
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ
!
      IF ( HEB%STATUS .NE. HEB__HDLO ) THEN
           CALL ERR_LOG ( 6361, IUER, 'HEB_READ_DATA_SECT', 'HEB Header '// &
     &         'has not been read. Please first run READ_HEB_HEADER' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6362, IUER, 'HEB_READ_DATA_SECT', 'Error in '// &
     &         'an attempt to open input file '//FILIN )
           RETURN 
      END IF
!
      IF ( HEB%DATA_FORMAT == HEB__R8 ) THEN
           IF ( ASSOCIATED ( HEB%VAL8 ) ) THEN
                DEALLOCATE ( HEB%VAL8 )
           END IF
           LEN_ELEM = 8
           LEN_TO_READ = INT8(LEN_ELEM)* &
     &                   HEB%DIMS(1)* &
     &                   HEB%DIMS(2)* &
     &                   INT8(IND3_SECT(2)-IND3_SECT(1)+1)* &
     &                   INT8(IND4_SECT(2)-IND4_SECT(1)+1)
           ALLOCATE ( HEB%VAL8(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), STAT=IER )
        ELSE IF ( HEB%DATA_FORMAT    == HEB__I1   .AND. &
     &            HEB%DATA_TRANSFORM == HEB__NONE       ) THEN
           IF ( ASSOCIATED ( HEB%VAL1 ) ) THEN
                DEALLOCATE ( HEB%VAL1 )
           END IF
           LEN_ELEM = 1
           LEN_TO_READ = HEB%DIMS(1)* &
     &                   HEB%DIMS(2)* &
     &                   INT8(IND3_SECT(2)-IND3_SECT(1)+1)* &
     &                   INT8(IND4_SECT(2)-IND4_SECT(1)+1)
           ALLOCATE ( HEB%VAL1(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), STAT=IER )
        ELSE IF ( HEB%DATA_FORMAT    == HEB__I2 ) THEN
           LEN_ELEM = 2
           LEN_TO_READ = INT8(LEN_ELEM)* &
     &                   HEB%DIMS(1)* &
     &                   HEB%DIMS(2)* &
     &                   INT8(IND3_SECT(2)-IND3_SECT(1)+1)* &
     &                   INT8(IND4_SECT(2)-IND4_SECT(1)+1)
           IF ( HEB%DATA_TRANSFORM == HEB__NONE ) THEN
                IF ( ASSOCIATED ( HEB%VAL2 ) ) THEN
                     DEALLOCATE ( HEB%VAL2 )
                END IF
                ALLOCATE ( HEB%VAL2(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), STAT=IER )
              ELSE
                IF ( ASSOCIATED ( HEB%VAL ) ) THEN
                     DEALLOCATE ( HEB%VAL )
                END IF
                ALLOCATE ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)),  STAT=IER )
           END IF
        ELSE IF ( HEB%DATA_FORMAT == HEB__R4 ) THEN
          IF ( ASSOCIATED ( HEB%VAL ) ) THEN
               DEALLOCATE ( HEB%VAL )
          END IF
          LEN_ELEM = 4
          LEN_TO_READ = INT8(LEN_ELEM)* &
     &                  HEB%DIMS(1)* &
     &                  HEB%DIMS(2)* &
     &                  INT8(IND3_SECT(2)-IND3_SECT(1)+1)* &
     &                  INT8(IND4_SECT(2)-IND4_SECT(1)+1)
          ALLOCATE ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),IND3_SECT(2)-IND3_SECT(1)+1, &
     &                                               IND4_SECT(2)-IND4_SECT(1)+1), STAT=IER )
        ELSE
           CALL ERR_LOG ( 6263, IUER, 'HEB_READ_DATA_SECT', 'does not support '// &
     &                   'data type '//HEB%DATA_FORMAT )
           RETURN 
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH8  ( LEN_ELEM*LEN_TO_READ, STR )
           WRITE ( 6, * ) 'HEB%DATA_FORMAT= ', HEB%DATA_FORMAT
           WRITE ( 6, * ) 'HEB%DIMS= ', HEB%DIMS
           CALL ERR_LOG ( 6264, IUER, 'HEB_READ_DATA_SECT', 'Error in '// &
     &         'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array HEB%VAL' )
           RETURN 
      END IF
      HEB%STATUS = HEB__ALLO
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_CUR', SEEK_CUR, ARG_LN )
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(HEB%DATA_OFFSET), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. HEB%DATA_OFFSET ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6265, IUER, 'HEB_READ_DATA_SECT', 'Failure in '// &
     &         'an attempt to seek for beginning the data section '// &
     &         'in the input file '//FILIN(1:I_LEN(FILIN))//' -- '//STR )
           RETURN 
      END IF
!
      IF ( HEB%DATA_FORMAT == HEB__I1 ) THEN
!
! -------- INTEGER*1 data. Allocate memory for data array
!
           LEN_ELEM = 1
           LEN_ALLOC = MIN ( LEN_TO_READ, HEB__DATA_CHUNK )
           ALLOCATE ( ARR_I1(LEN_ALLOC), STAT= IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL IINCH8  ( LEN_ELEM*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4), STR )
                CALL ERR_LOG ( 6266, IUER, 'HEB_READ_DATA_SECT', 'Error in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'dynamic memory for array ARR_I1' )
                RETURN 
           END IF
!
! -------- Read the data to an INTEGER*1 array
!
           CHUNKS_TO_READ = 1 + LEN_TO_READ/HEB__DATA_CHUNK
           SHFT = HEB%DIMS(1)*HEB%DIMS(2)*INT8(IND3_SECT(2) - 1) + &
     &            HEB%DIMS(1)*HEB%DIMS(2)*INT8(IND3_SECT(2) - IND3_SECT(1) + 1)* &
     &            INT8(IND4_SECT(2) - 1)
           OFFSET_RET = LSEEK( %VAL(LUN), %VAL(SHFT), %VAL(SEEK_CUR) )
           IF ( OFFSET_RET .NE. SHFT + HEB%DATA_OFFSET ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6267, IUER, 'HEB_READ_DATA_SECT', 'Failure in '// &
     &              'position the file into beginning the section' )
                RETURN 
           END IF
           OFFS = 0
           DO 410 J1=1,CHUNKS_TO_READ
              IF ( J1 == CHUNKS_TO_READ ) THEN
                   BYTES_TO_READ = LEN_TO_READ - (J1-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_READ == 0 ) THEN
                        IS = 0
                        GOTO 810
                   END IF
                 ELSE 
                   BYTES_TO_READ= HEB__DATA_CHUNK
              END IF
              IS = READ ( %VAL(LUN), ARR_I1, %VAL(BYTES_TO_READ) )
              IF ( IS .NE. BYTES_TO_READ ) GOTO 810
!
! ----------- Copy there the data and apply the offset and scale factor
!
              IF ( HEB%DATA_TRANSFORM == HEB__LOG ) THEN
                   CALL HEB_DT_FROM_I1_TO_R4_LOG ( BYTES_TO_READ, HEB%SCALE_FACTOR, HEB%OFFSET, &
     &                  ARR_I1, %VAL(LOC(HEB%VAL) + OFFS) )
                 ELSE IF ( HEB%DATA_TRANSFORM == HEB__SCOF ) THEN
                   CALL HEB_DT_FROM_I1_TO_R4_SCOF ( BYTES_TO_READ, HEB%SCALE_FACTOR, HEB%OFFSET, &
     &                  ARR_I1, %VAL(LOC(HEB%VAL) + OFFS) )
                 ELSE
                   CALL MEMCPY ( %VAL(LOC(HEB%VAL1) + OFFS), ARR_I1, %VAL(BYTES_TO_READ) )
              END IF
              OFFS = OFFS + IS
 410       CONTINUE 
 810       CONTINUE 
         ELSE IF ( HEB%DATA_FORMAT == HEB__I2 ) THEN
!
! -------- INTEGER*2 data. Allocate memory for data array
!
           LEN_ELEM = 2
           LEN_ALLOC = MIN ( HEB%DATA_LENGTH, HEB__DATA_CHUNK )
           ALLOCATE ( ARR_I2(LEN_ALLOC), STAT= IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH  ( LEN_ALLOC, STR )
                CALL ERR_LOG ( 6268, IUER, 'HEB_READ_DATA_SECT', 'Error in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'dynamic memory for array ARR_I2' )
                RETURN 
           END IF
!
! -------- Read the data to an INTEGER*2 array
!
           LEN_ELEM = 2
           CHUNKS_TO_READ = 1 + HEB%DATA_LENGTH/HEB__DATA_CHUNK
           OFFS = 0
           DO 420 J2=1,CHUNKS_TO_READ
              IF ( J2 == CHUNKS_TO_READ ) THEN
                   BYTES_TO_READ = LEN_TO_READ - (J2-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_READ == 0 ) THEN
                        IS = 0
                        GOTO 820
                   END IF
                 ELSE 
                   BYTES_TO_READ= HEB__DATA_CHUNK
              END IF
              IS = READ ( %VAL(LUN), ARR_I2, %VAL(BYTES_TO_READ) )
              IF ( IS .NE. BYTES_TO_READ ) GOTO 820
!
! ----------- Copy there the data and apply the offset and scale factor
!
              IF ( HEB%DATA_TRANSFORM == HEB__LOG ) THEN
                   CALL HEB_DT_FROM_I2_TO_R4_LOG ( BYTES_TO_READ/LEN_ELEM, HEB%SCALE_FACTOR, HEB%OFFSET, &
     &                  ARR_I2, %VAL(LOC(HEB%VAL) + 2*OFFS) )
                 ELSE
                   CALL HEB_DT_FROM_I2_TO_R4_SCOF ( BYTES_TO_READ/LEN_ELEM, HEB%SCALE_FACTOR, HEB%OFFSET, &
     &                  ARR_I2, %VAL(LOC(HEB%VAL) + 2*OFFS) )
              END IF
              OFFS = OFFS + IS
 420       CONTINUE 
 820       CONTINUE 
         ELSE IF ( HEB%DATA_FORMAT == HEB__R4 .OR. &
     &             HEB%DATA_FORMAT == HEB__I4      ) THEN
!
! -------- Read the data from the original array without any transformations
!
           LEN_ELEM = 4
           CHUNKS_TO_READ = 1 + LEN_TO_READ/HEB__DATA_CHUNK
           SHFT = INT8(HEB%DIMS(1))*INT8(HEB%DIMS(2))* &
     &            (INT8(IND3_SECT(1) - 1) + &
     &             HEB%DIMS(3)*INT8(IND4_SECT(1) - 1) &
     &            )
           SHFT = SHFT*INT8(LEN_ELEM)
           OFFSET_RET = LSEEK( %VAL(LUN), %VAL(SHFT), %VAL(SEEK_CUR) )
           IF ( OFFSET_RET .NE. SHFT + HEB%DATA_OFFSET ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6269, IUER, 'HEB_READ_DATA_SECT', 'Failure in '// &
     &              'position the file into beginning the section' )
                RETURN 
           END IF
           OFFS = 0
           DO 430 J3=1,CHUNKS_TO_READ
              IF ( J3 == CHUNKS_TO_READ ) THEN
                   BYTES_TO_READ = LEN_TO_READ - INT8(J3-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_READ == 0 ) THEN
                        IS = 0
                        GOTO 830
                   END IF
                 ELSE 
                   BYTES_TO_READ = HEB__DATA_CHUNK
              END IF
              IS = READ ( %VAL(LUN), %VAL(LOC(HEB%VAL) + OFFS), &
     &                    %VAL(BYTES_TO_READ) )
              IF ( IS .NE. BYTES_TO_READ ) GOTO 830
              OFFS = OFFS + IS
 430       CONTINUE 
 830       CONTINUE 
         ELSE IF ( HEB%DATA_FORMAT == HEB__R8      ) THEN
!
! -------- Read the data from the original array without any transformations
!
           LEN_ELEM = 8
           CHUNKS_TO_READ = 1 + LEN_TO_READ/HEB__DATA_CHUNK
           SHFT = HEB%DIMS(1)*HEB%DIMS(2)*INT8(IND3_SECT(2) - 1) + &
     &            HEB%DIMS(1)*HEB%DIMS(2)*INT8(IND3_SECT(2) - IND3_SECT(1) + 1)* &
     &            INT8(IND4_SECT(2) - 1)
           SHFT = SHFT*INT8(LEN_ELEM)
           OFFSET_RET = LSEEK( %VAL(LUN), %VAL(SHFT), %VAL(SEEK_CUR) )
           IF ( OFFSET_RET .NE. SHFT + HEB%DATA_OFFSET ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6270, IUER, 'HEB_READ_DATA_SECT', 'Failure in '// &
     &              'position the file into beginning the section' )
                RETURN 
           END IF
           OFFS = 0
           DO 440 J4=1,CHUNKS_TO_READ
              IF ( J4 == CHUNKS_TO_READ ) THEN
                   BYTES_TO_READ = LEN_TO_READ - INT8(J4-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_READ == 0 ) THEN
                        IS = 0
                        GOTO 840
                   END IF
                 ELSE 
                   BYTES_TO_READ = HEB__DATA_CHUNK
              END IF
              IS = READ ( %VAL(LUN), %VAL(LOC(HEB%VAL8) + OFFS), &
     &                    %VAL(BYTES_TO_READ) )
              IF ( IS .NE. BYTES_TO_READ ) GOTO 840
              OFFS = OFFS + IS
 440       CONTINUE 
 840       CONTINUE 
      END IF
!
      IF ( IS < 0 ) THEN
!
! -------- Failure in reading
!
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6271, IUER, 'HEB_READ_DATA_SECT', 'Error in '// &
     &         'reading the data section of the input HEB file '// &
     &          FILIN (1:I_LEN(FILIN ))//' '//STR )
           RETURN 
         ELSE IF ( IS < BYTES_TO_READ ) THEN
!
! -------- Not all the data are read
!
           WRITE ( 6, * ) ' IS = ', IS, &
     &                    ' DL = ', LEN_ELEM*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4)
           CALL ERR_LOG ( 6272, IUER, 'HEB_READ_DATA_SECT', 'Error in '// &
     &         'reading the data section of the input heb file '// &
     &          FILIN (1:I_LEN(FILIN ))//' -- it was not read to the end' )
           RETURN 
      END IF
      IF ( HEB%DATA_FORMAT == HEB__I1 ) THEN
           DEALLOCATE ( ARR_I1 )
         ELSE IF ( HEB%DATA_FORMAT == HEB__I2 ) THEN
           DEALLOCATE ( ARR_I2 )
      END IF
!
! --- Close the output file 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6273, IUER, 'HEB_READ_DATA_SECT', 'Error in '// &
     &         'attempt to close file '//FILIN  )
           RETURN 
      END IF
      HEB%STATUS = HEB__LOAD
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  HEB_READ_DATA_SECT  !#!#
