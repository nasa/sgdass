      SUBROUTINE WRITE_HEB ( HEB, DATA_ARR, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRITE_HEB 
! *                                                                      *
! * ### 28-JAN-2013     WRITE_HEB   v3.2  (c) L. Petrov 06-JUN-2016  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      REAL*4     DATA_ARR(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4))
      CHARACTER  FILOUT*(*)
      CHARACTER  HDR*(HEB__HDS)
      INTEGER*1, ALLOCATABLE :: ARR_I1(:)
      INTEGER*2, ALLOCATABLE :: ARR_I2(:)
      INTEGER*4  IUER
      CHARACTER  STR*128
      LOGICAL*1  LEX
      INTEGER*4  LUN, IER, IS
      INTEGER*8  J1, J2, J3, J4
      INTEGER*8  LEN_ELEM, BYTES_TO_WRITE, CHUNKS_TO_WRITE, &
     &           NEL_ALLOC, OFFS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, WRITE, UNLINK 
!
      IF ( HEB%DATA_FORMAT == HEB__I1 ) LEN_ELEM = 1
      IF ( HEB%DATA_FORMAT == HEB__I2 ) LEN_ELEM = 2
      IF ( HEB%DATA_FORMAT == HEB__I4 ) LEN_ELEM = 4
      IF ( HEB%DATA_FORMAT == HEB__I8 ) LEN_ELEM = 8
      IF ( HEB%DATA_FORMAT == HEB__R4 ) LEN_ELEM = 4
      IF ( HEB%DATA_FORMAT == HEB__R8 ) LEN_ELEM = 8
      HEB%DATA_LENGTH = LEN_ELEM*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4) 
!
! --- Initialization of the header
!
      CALL REPEAT ( ' ', LEN(HDR), HDR )
!
! --- Write the header
!
      HDR = HEB__LABEL//CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+83), FMT='("Dims:   ",I18,1X,I18,1X,I18,1X,I18,A)' ) HEB%DIMS
      CALL CHASHL ( HDR(ILEN(HDR)-64:ILEN(HDR)) )
      HDR(ILEN(HDR)+1:) = CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+23), FMT='("Data_Format:      ", A, A)'  ) HEB%DATA_FORMAT, CHAR(10)
#ifdef LITTLE_ENDIAN
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+23), FMT='("Endian:           ", A, A)'  ) HEB__LE, CHAR(10)
#else
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+23), FMT='("Endian:           ", A, A)'  ) HEB__BE, CHAR(10)
#endif
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+23), FMT='("Data_Compression: ", A, A)'  ) HEB%DATA_COMPRESSION, CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+23), FMT='("Data_Offset:      ", I4,A)'  ) HEB__HDS, CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+37), FMT='("Data_Length:      ", I18)'   ) HEB%DATA_LENGTH
      CALL CHASHL ( HDR(ILEN(HDR)-17:ILEN(HDR)) )
      HDR(ILEN(HDR)+1:) = CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+23), FMT='("Data_Transform:   ", A4,A)' ) HEB%DATA_TRANSFORM, CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+33), FMT='("Fill_Value:       ", 1PE14.7,A)' ) HEB%FILL_VALUE,  CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+33), FMT='("Offset:           ", 1PE14.7,A)' ) HEB%OFFSET, CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+33), FMT='("Scale_Factor:     ", 1PE14.7,A)' ) HEB%SCALE_FACTOR, CHAR(10)
!
      HDR(ILEN(HDR)+1:) = 'SDS_name:         '//HEB%SDS_NAME(1:I_LEN(HEB%SDS_NAME))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'Units:            '//HEB%UNITS(1:I_LEN(HEB%UNITS))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'File_name:        '//FILOUT(1:I_LEN(FILOUT))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'Prod_name:        '//HEB%PROD_NAME(1:I_LEN(HEB%PROD_NAME))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'History:          '//HEB%HISTORY(1:I_LEN(HEB%HISTORY))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'Source:           '//HEB%SOURCE(1:I_LEN(HEB%SOURCE))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'Title:            '//HEB%TITLE(1:I_LEN(HEB%TITLE))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'Institution:      '//HEB%INSTITUTION(1:I_LEN(HEB%INSTITUTION))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'References:       '//HEB%REFERENCES(1:I_LEN(HEB%REFERENCES))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'Prod_date_time:   '//HEB%PROD_DATE_TIME(1:I_LEN(HEB%PROD_DATE_TIME))//CHAR(10)
      HDR(ILEN(HDR)+1:) = 'Version_ID:       '//HEB%VERSION_ID(1:I_LEN(HEB%VERSION_ID))//CHAR(10)
      DO 410 J1=1,HEB__MC
         IF ( ILEN(HEB%COMMENT(J1)) > 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR(1:1) )
              HDR(ILEN(HDR)+1:) = 'Comment('//STR(1:1)//'):       '// &
     &                            HEB%COMMENT(J1)(1:I_LEN(HEB%COMMENT(J1)))//CHAR(10)
         END IF
 410  CONTINUE 
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+32), FMT='("MJD, Time:   ", 5X,I5,1X,F7.1,A)' ) HEB%MJD, HEB%UTC, CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+33), FMT='("Min_Value:        ", 1PE14.7,A)' ) HEB%MIN_VALUE,   CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+33), FMT='("Max_Value:        ", 1PE14.7,A)' ) HEB%MAX_VALUE,   CHAR(10)
      WRITE ( UNIT=HDR(ILEN(HDR)+1:ILEN(HDR)+48), FMT='("Valid_Range:      ", 1PE14.7,1X,1PE14.7,A)' ) HEB%VALID_RANGE, CHAR(10)
!
! --- Put the final line termination sign
!
      HDR(LEN(HDR):LEN(HDR)) = CHAR(10)
!
! --- Check weather the output file exists
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
      IF ( LEX  .AND.  HEB%STATUS .NE. HEB__HDON ) THEN
!
! -------- Exists? Then remove it
!
           IS = UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) ) 
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR ) 
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6231, IUER, 'WRITE_HEB', 'Error in '// &
     &              'an attempt to remove old output file '// &
     &              FILOUT(1:I_LEN(FILOUT))//' '//STR )
                RETURN 
           END IF 
      END IF
!
! --- Open the output file for writing
!
      CALL ERR_PASS ( IUER, IER )
      IF ( HEB%STATUS .EQ. HEB__HDON ) THEN
           CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
         ELSE
           CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6232, IUER, 'WRITE_HEB', 'Error in '// &
     &         'an attempt to open output file '//FILOUT )
           RETURN 
      END IF
!
! --- Write the header
!
      IS = WRITE ( %VAL(LUN), %REF(HDR), %VAL(HEB__HDS) )
      IF ( IS < 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6233, IUER, 'WRITE_HEB', 'Error in '// &
     &         'an atempt to write to the header of the output HEB file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' '//STR )
           RETURN 
         ELSE IF ( IS < LEN(HDR) ) THEN
           CALL ERR_LOG ( 6234, IUER, 'WRITE_HEB', 'Error in '// &
     &         'writing the header of the output HEB file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- it was not written to the end' )
           RETURN 
      END IF
      IF ( HEB%STATUS .EQ. HEB__HDON ) THEN
!
! -------- Special case: writing the header only
!
! -------- Close the output file 
!
           CALL ERR_PASS   ( IUER, IER )
           CALL BINF_CLOSE ( LUN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6241, IUER, 'WRITE_HEB', 'Error in '// &
     &              'attempt to close file '//FILOUT )
                RETURN 
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( HEB%DATA_FORMAT == HEB__I1 ) THEN
!
! -------- INTEGER*1 data. Allocate memory for data array
!
           NEL_ALLOC = MIN ( HEB%DATA_LENGTH, HEB__DATA_CHUNK )
           ALLOCATE ( ARR_I1(NEL_ALLOC), STAT= IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL IINCH8  ( NEL_ALLOC, STR )
                CALL ERR_LOG ( 6235, IUER, 'WRITE_HEB', 'Error in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'dynamic memory for array ARR_I1' )
                RETURN 
           END IF
!
! -------- Write the data as an INTEGER*1 array.
! -------- NB: Since the data section may be longer than the system
! -------- buffer, we write the data by chunks
!
           CHUNKS_TO_WRITE = 1 + HEB%DATA_LENGTH/HEB__DATA_CHUNK 
           OFFS = 0
           DO 420 J2=1,CHUNKS_TO_WRITE
              IF ( J2 == CHUNKS_TO_WRITE ) THEN
                   BYTES_TO_WRITE = HEB%DATA_LENGTH - (J2-1)*HEB__DATA_CHUNK 
                   IF ( BYTES_TO_WRITE == 0 ) THEN
                        IS = 0
                        GOTO 820
                   END IF
                 ELSE 
                   BYTES_TO_WRITE = HEB__DATA_CHUNK 
              END IF
!
! ----------- Copy the data into the ARR_I1 buffer and apply offset and scale factor
!
              IF ( HEB%DATA_TRANSFORM == HEB__LOG ) THEN
                   CALL HEB_DT_FROM_R4_TO_I1_LOG  ( BYTES_TO_WRITE, HEB%SCALE_FACTOR, HEB%OFFSET, &
     &                  HEB%VALID_RANGE, %VAL(LOC(DATA_ARR) + 4*OFFS), ARR_I1  )
                ELSE IF ( HEB%DATA_TRANSFORM == HEB__SCOF ) THEN
                   CALL HEB_DT_FROM_R4_TO_I1_SCOF ( BYTES_TO_WRITE, HEB%SCALE_FACTOR, HEB%OFFSET, &
     &                  HEB%VALID_RANGE, %VAL(LOC(DATA_ARR) + 4*OFFS), ARR_I1  )
                ELSE 
                   CALL MEMCPY ( ARR_I1, %VAL(LOC(DATA_ARR) + OFFS), %VAL(BYTES_TO_WRITE) )
              END IF
 720          CONTINUE 
              IS = WRITE ( %VAL(LUN), ARR_I1, %VAL(BYTES_TO_WRITE) )
              IF ( HEB%DATA_TRANSFORM == HEB__LOG  .OR. &
     &             HEB%DATA_TRANSFORM == HEB__SCOF      ) THEN
                   OFFS = OFFS + 4*(IS/LEN_ELEM)
                 ELSE 
                   OFFS = OFFS +   (IS/LEN_ELEM)
              END IF
              IF ( IS < 0 ) GOTO 820
              IF ( IS < BYTES_TO_WRITE ) THEN
                   BYTES_TO_WRITE = BYTES_TO_WRITE - IS
                   GOTO 720
              END IF
 420       CONTINUE 
 820       CONTINUE 
         ELSE IF ( HEB%DATA_FORMAT == HEB__I2  ) THEN
!
! -------- INTEGER*2 data. Allocate memory for data array
!
           NEL_ALLOC = MIN ( HEB%DATA_LENGTH/LEN_ELEM, HEB__DATA_CHUNK/LEN_ELEM )
           ALLOCATE ( ARR_I2(NEL_ALLOC), STAT= IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL IINCH8  ( LEN_ELEM*NEL_ALLOC, STR )
                CALL ERR_LOG ( 6236, IUER, 'WRITE_HEB', 'Error in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'dynamic memory for array ARR_I2' )
                RETURN 
           END IF
!
! -------- Write the data as an INTEGER*2 array
!
           CHUNKS_TO_WRITE = 1 + HEB%DATA_LENGTH/HEB__DATA_CHUNK
           OFFS = 0 
           DO 430 J3=1,CHUNKS_TO_WRITE
              IF ( J3 == CHUNKS_TO_WRITE ) THEN
                   BYTES_TO_WRITE = HEB%DATA_LENGTH - (J3-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_WRITE == 0 ) THEN
                        IS = 0
                        GOTO 830
                   END IF
                 ELSE 
                   BYTES_TO_WRITE = HEB__DATA_CHUNK
              END IF
!
! ----------- Copy the data into the ARR_I2 buffer and apply offset and scale factor
!
              IF ( HEB%DATA_TRANSFORM == HEB__LOG ) THEN
                   CALL HEB_DT_FROM_R4_TO_I2_LOG  ( BYTES_TO_WRITE/LEN_ELEM, HEB%SCALE_FACTOR, &
     &                  HEB%OFFSET, HEB%VALID_RANGE, %VAL(LOC(DATA_ARR) + 2*OFFS), ARR_I2 )
                 ELSE
                   CALL HEB_DT_FROM_R4_TO_I2_SCOF ( BYTES_TO_WRITE/LEN_ELEM, HEB%SCALE_FACTOR, &
     &                  HEB%OFFSET, HEB%VALID_RANGE, %VAL(LOC(DATA_ARR) + 2*OFFS), ARR_I2 )
              END IF
!
              IS = WRITE ( %VAL(LUN), ARR_I2, %VAL(BYTES_TO_WRITE) )
              OFFS = OFFS + IS
              IF ( IS .NE. BYTES_TO_WRITE ) GOTO 850
 430       CONTINUE 
 830       CONTINUE 
         ELSE IF ( HEB%DATA_FORMAT == HEB__R4 .OR. &
     &             HEB%DATA_FORMAT == HEB__I4 .OR. &
     &             HEB%DATA_FORMAT == HEB__R8      ) THEN
!
! -------- Write the data as an original array
!
           CHUNKS_TO_WRITE = 1 + HEB%DATA_LENGTH/HEB__DATA_CHUNK
           OFFS = 0
           DO 440 J4=1,CHUNKS_TO_WRITE
              IF ( J4 == CHUNKS_TO_WRITE ) THEN
                   BYTES_TO_WRITE = HEB%DATA_LENGTH - (J4-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_WRITE == 0 ) THEN
                        IS = 0
                        GOTO 840
                   END IF
                 ELSE 
                   BYTES_TO_WRITE = HEB__DATA_CHUNK
              END IF
 740          CONTINUE 
              IS = WRITE ( %VAL(LUN), %VAL(LOC(DATA_ARR) + OFFS), &
     &                     %VAL(BYTES_TO_WRITE) )
              IF ( IS < 0 ) GOTO 840
              OFFS = OFFS + IS
              IF ( IS < BYTES_TO_WRITE ) THEN
                   BYTES_TO_WRITE = BYTES_TO_WRITE - IS
                   GOTO 740
              END IF
 440       CONTINUE 
 840       CONTINUE 
      END IF
 850  CONTINUE 
!
      IF ( IS < 0 ) THEN
!
! -------- Failure in writing
!
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6239, IUER, 'WRITE_HEB', 'Error in '// &
     &         'writing the data section to the output HEB file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' '//STR )
           RETURN 
         ELSE IF ( IS < BYTES_TO_WRITE ) THEN
!
! -------- Not all the data are written
!
           WRITE ( 6, * ) ' IS = ', IS, ' BTW = ', BYTES_TO_WRITE
           WRITE ( 6, * ) ' CHUNKS_TO_WRITE = ', CHUNKS_TO_WRITE
           WRITE ( 6, * ) ' HEB%DATA_LENGTH = ', HEB%DATA_LENGTH 
           CALL ERR_LOG ( 6240, IUER, 'WRITE_HEB', 'Error in '// &
     &         'writing the data section of the output heb file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' -- it was not written to the end' )
           RETURN 
      END IF
!
! --- Close the output file 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6241, IUER, 'WRITE_HEB', 'Error in '// &
     &         'attempt to close file '//FILOUT )
           RETURN 
      END IF
!
      IF ( ALLOCATED ( ARR_I1 ) ) DEALLOCATE ( ARR_I1 )
      IF ( ALLOCATED ( ARR_I2 ) ) DEALLOCATE ( ARR_I2 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRITE_HEB  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_DT_FROM_R4_TO_I1_LOG ( NEL, SCALE, OFFSET, &
     &                                      VALID_RANGE, ARR_R4, ARR_I1 )
      IMPLICIT   NONE 
      INTEGER*8  NEL
      REAL*4     SCALE, OFFSET, VALID_RANGE(2)
      REAL*4     ARR_R4(NEL)
      INTEGER*1  ARR_I1(NEL)
      INTEGER*8  J1
!
      DO 410 J1=1,NEL
         IF ( ARR_R4(J1) < VALID_RANGE(1) ) THEN
              ARR_R4(J1) = VALID_RANGE(1) 
         END IF
         IF ( ARR_R4(J1) > VALID_RANGE(2) ) THEN
              ARR_R4(J1) = VALID_RANGE(2) 
         END IF
         ARR_I1(J1) = NINT( (LOG(ARR_R4(J1)) - OFFSET)/SCALE )
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_DT_FROM_R4_TO_I1_LOG !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_DT_FROM_R4_TO_I1_SCOF ( NEL, SCALE, OFFSET, &
     &                                       VALID_RANGE, ARR_R4, ARR_I1 )
      IMPLICIT   NONE 
      INTEGER*8  NEL
      REAL*4     SCALE, OFFSET, VALID_RANGE(2)
      INTEGER*1  ARR_I1(NEL)
      REAL*4     ARR_R4(NEL)
      INTEGER*8  J1
!
      DO 410 J1=1,NEL
         IF ( ARR_R4(J1) < VALID_RANGE(1) ) THEN
              ARR_R4(J1) = VALID_RANGE(1) 
         END IF
         IF ( ARR_R4(J1) > VALID_RANGE(2) ) THEN
              ARR_R4(J1) = VALID_RANGE(2) 
         END IF
         ARR_I1(J1) = NINT( (ARR_R4(J1) - OFFSET)/SCALE )
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_DT_FROM_R4_TO_I1_SCOF  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_DT_FROM_R4_TO_I2_LOG ( NEL, SCALE, OFFSET, &
     &                                      VALID_RANGE, ARR_R4, ARR_I2 )
      IMPLICIT   NONE 
      INTEGER*8  NEL
      REAL*4     SCALE, OFFSET, VALID_RANGE(2)
      REAL*4     ARR_R4(NEL)
      INTEGER*2  ARR_I2(NEL)
      INTEGER*2  I2_VAL
      INTEGER*8  J1
!
      DO 410 J1=1,NEL
         IF ( ARR_R4(J1) < VALID_RANGE(1) ) THEN
              ARR_R4(J1) = VALID_RANGE(1) 
         END IF
         IF ( ARR_R4(J1) > VALID_RANGE(2) ) THEN
              ARR_R4(J1) = VALID_RANGE(2) 
         END IF
         ARR_I2(J1) = NINT( (LOG(ARR_R4(J1)) - OFFSET)/SCALE )
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_DT_FROM_R4_TO_I2_LOG  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_DT_FROM_R4_TO_I2_SCOF ( NEL, SCALE, OFFSET, &
     &                                       VALID_RANGE, ARR_R4, ARR_I2 )
      IMPLICIT   NONE 
      INTEGER*8  NEL
      REAL*4     SCALE, OFFSET, VALID_RANGE(2)
      REAL*4     ARR_R4(NEL)
      INTEGER*2  ARR_I2(NEL)
      INTEGER*8  J1
!
      DO 410 J1=1,NEL
         IF ( ARR_R4(J1) < VALID_RANGE(1) ) THEN
              ARR_R4(J1) = VALID_RANGE(1) 
         END IF
         IF ( ARR_R4(J1) > VALID_RANGE(2) ) THEN
              ARR_R4(J1) = VALID_RANGE(2) 
         END IF
         ARR_I2(J1) = NINT( (ARR_R4(J1) - OFFSET)/SCALE )
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_DT_FROM_R4_TO_I2_SCOF  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_MINMAX ( HEB, DATA_ARR, ABS_VAL_MAX )
! ************************************************************************
! *                                                                      *
! *   Axisulliar routine HEB_MINMAX
! *                                                                      *
! *  ### 17-APR-2014   HEB_MINMAX  v1.0 (c)  L. Petrov  17-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      REAL*4     DATA_ARR(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &           ABS_VAL_MAX
      LOGICAL*1  FL_FIRST
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
!
      FL_FIRST = .TRUE.
      DO 410 J1=1,HEB%DIMS(4)
         DO 420 J2=1,HEB%DIMS(3)
            DO 430 J3=1,HEB%DIMS(2)
                DO 440 J4=1,HEB%DIMS(1)
                   IF ( IS_R4_NAN ( DATA_ARR(J4,J3,J2,J1) )      ) GOTO 440
                   IF ( ABS(DATA_ARR(J4,J3,J2,J1)) > ABS_VAL_MAX ) GOTO 440
                   IF ( FL_FIRST ) THEN
                        HEB%MIN_VALUE = DATA_ARR(J4,J3,J2,J1)
                        HEB%MAX_VALUE = DATA_ARR(J4,J3,J2,J1)
                        FL_FIRST = .FALSE.
                      ELSE 
                        HEB%MIN_VALUE = MIN ( HEB%MIN_VALUE, DATA_ARR(J4,J3,J2,J1) )
                        HEB%MAX_VALUE = MAX ( HEB%MAX_VALUE, DATA_ARR(J4,J3,J2,J1) )
                   END IF
 440            CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_MINMAX  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_SETMIN ( HEB, DATA_ARR, MIN_VAL )
! ************************************************************************
! *                                                                      *
! *   Axisulliar routine HEB_SETMIN
! *                                                                      *
! *  ### 23-SEP-2014   HEB_SETMIN  v1.2 (c)  L. Petrov  08-JAN-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      REAL*4     DATA_ARR(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &           MIN_VAL
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
!
      DO 410 J1=1,HEB%DIMS(4)
         DO 420 J2=1,HEB%DIMS(3)
            DO 430 J3=1,HEB%DIMS(2)
                DO 440 J4=1,HEB%DIMS(1)
                   IF ( IS_R4_NAN ( DATA_ARR(J4,J3,J2,J1) )      ) GOTO 440
                   IF ( DATA_ARR(J4,J3,J2,J1) < MIN_VAL ) THEN
                        DATA_ARR(J4,J3,J2,J1) = MIN_VAL
                   END IF
 440            CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_SETMIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_SETMAX ( HEB, DATA_ARR, MAX_VAL )
! ************************************************************************
! *                                                                      *
! *   Axisulliar routine HEB_SETMAX
! *                                                                      *
! *  ### 17-APR-2015   HEB_SETMAX  v1.1 (c)  L. Petrov  08-JAN-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      REAL*4     DATA_ARR(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &           MAX_VAL
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
!
      DO 410 J1=1,HEB%DIMS(4)
         DO 420 J2=1,HEB%DIMS(3)
            DO 430 J3=1,HEB%DIMS(2)
                DO 440 J4=1,HEB%DIMS(1)
                   IF ( IS_R4_NAN ( DATA_ARR(J4,J3,J2,J1) )      ) GOTO 440
                   IF ( DATA_ARR(J4,J3,J2,J1) > MAX_VAL ) THEN
                        DATA_ARR(J4,J3,J2,J1) = MAX_VAL
                   END IF
 440            CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_SETMAX  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_MINMAX_R8 ( HEB, DATA_ARR, ABS_VAL_MAX )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine HEB_MINMAX_R8                                    *
! *                                                                      *
! * ### 17-APR-2014   HEB_MINMAX_R8  v1.0 (c)  L. Petrov 17-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      REAL*8     DATA_ARR(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &           ABS_VAL_MAX
      LOGICAL*1  FL_FIRST
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
!
      FL_FIRST = .TRUE.
      DO 410 J1=1,HEB%DIMS(4)
         DO 420 J2=1,HEB%DIMS(3)
            DO 430 J3=1,HEB%DIMS(2)
                DO 440 J4=1,HEB%DIMS(1)
                   IF ( IS_R8_NAN ( DATA_ARR(J4,J3,J2,J1) )      ) GOTO 440
                   IF ( ABS(DATA_ARR(J4,J3,J2,J1)) > ABS_VAL_MAX ) GOTO 440
                   IF ( FL_FIRST ) THEN
                        HEB%MIN_VALUE = DATA_ARR(J4,J3,J2,J1)
                        HEB%MAX_VALUE = DATA_ARR(J4,J3,J2,J1)
                        FL_FIRST = .FALSE.
                      ELSE 
                        HEB%MIN_VALUE = MIN ( HEB%MIN_VALUE, DATA_ARR(J4,J3,J2,J1) )
                        HEB%MAX_VALUE = MAX ( HEB%MAX_VALUE, DATA_ARR(J4,J3,J2,J1) )
                   END IF
 440            CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_MINMAX_R8  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_MINMAX_R8_SECT ( HEB, IB1, IE1, IB2, IE2, IB3, IE3, &
     &                                IB4, IE4, DATA_ARR, ABS_VAL_MAX )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine HEB_MINMAX_R8_SECT                               *
! *                                                                      *
! * ### 08-JAN-2017 HEB_MINMAX_R8_SECT v1.0 (c) L. Petrov 08-JAN-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      INTEGER*4  IB1, IE1, IB2, IE2, IB3, IE3, IB4, IE4
      REAL*8     DATA_ARR(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &           ABS_VAL_MAX
      LOGICAL*1  FL_FIRST
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
!
      FL_FIRST = .TRUE.
      DO 410 J1=IB4,IE4
         DO 420 J2=IB3,IE3
            DO 430 J3=IB2,IE2
                DO 440 J4=IB1,IE1
                   IF ( IS_R8_NAN ( DATA_ARR(J4,J3,J2,J1) )      ) GOTO 440
                   IF ( ABS(DATA_ARR(J4,J3,J2,J1)) > ABS_VAL_MAX ) GOTO 440
                   IF ( FL_FIRST ) THEN
                        HEB%MIN_VALUE = DATA_ARR(J4,J3,J2,J1)
                        HEB%MAX_VALUE = DATA_ARR(J4,J3,J2,J1)
                        FL_FIRST = .FALSE.
                      ELSE 
                        HEB%MIN_VALUE = MIN ( HEB%MIN_VALUE, DATA_ARR(J4,J3,J2,J1) )
                        HEB%MAX_VALUE = MAX ( HEB%MAX_VALUE, DATA_ARR(J4,J3,J2,J1) )
                   END IF
 440            CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_MINMAX_R8_SECT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_SETMIN_R8 ( HEB, DATA_ARR, MIN_VAL )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine HEB_SETMIN_R8*
! *                                                                      *
! * ### 23-SEP-2014   HEB_SETMIN_R8  v1.0 (c)  L. Petrov 23-SEP-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      REAL*8     DATA_ARR(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &           MIN_VAL
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
!
      DO 410 J1=1,HEB%DIMS(4)
         DO 420 J2=1,HEB%DIMS(3)
            DO 430 J3=1,HEB%DIMS(2)
                DO 440 J4=1,HEB%DIMS(1)
                   IF ( IS_R8_NAN ( DATA_ARR(J4,J3,J2,J1) )      ) GOTO 440
                   IF ( DATA_ARR(J4,J3,J2,J1) < MIN_VAL ) THEN
                        DATA_ARR(J4,J3,J2,J1) = MIN_VAL 
                   END IF
 440            CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_SETMIN_R8  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_SETMIN_R8_SECT ( HEB, IB1, IE1, IB2, IE2, IB3, IE3, IB4, IE4, &
     &                                DATA_ARR, MIN_VAL )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine HEB_SETMIN_R8_SECT
! *                                                                      *
! * ### 08-JAN-2017 HEB_SETMIN_R8_SECT v1.0 (c) L. Petrov 23-SEP-2014 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      INTEGER*4  IB1, IE1, IB2, IE2, IB3, IE3, IB4, IE4
      REAL*8     DATA_ARR(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), &
     &           MIN_VAL
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
!
      DO 410 J1=IB4,IE4
         DO 420 J2=IB3,IE3
            DO 430 J3=IB2,IE2
                DO 440 J4=IB1,IE1
                   IF ( IS_R8_NAN ( DATA_ARR(J4,J3,J2,J1) )      ) GOTO 440
                   IF ( DATA_ARR(J4,J3,J2,J1) < MIN_VAL ) THEN
                        DATA_ARR(J4,J3,J2,J1) = MIN_VAL 
                   END IF
 440            CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_SETMIN_R8_SECT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_R8_TO_R4 ( MEL, ARR_R8, ARR_R4 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program SPD_R8_TO_R4
! *                                                                      *
! *  ### 23-SEP-2014  SPD_R8_TO_R4  v1.0 (c)  L. Petrov 23-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*8  MEL
      REAL*8     ARR_R8(MEL)
      REAL*4     ARR_R4(MEL)
      ARR_R4 = ARR_R8
      RETURN
      END  SUBROUTINE SPD_R8_TO_R4  !#!#
