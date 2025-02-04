      PROGRAM    SC_LUT
! ************************************************************************
! *                                                                      *
! *   Program SC_LUT creates the look up table for samling correction    *
! *   array.
! *                                                                      *
! *  ### 05-FEB-2016     SC_LUT    v2.1 (c)  L. Petrov  14-FEB-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB, HEB_OUT
      CHARACTER  FILIN*128, FILOUT*128, C_FIL(MALO__FIL)*128, FILNAM*128, STR*128
      CHARACTER    SC_LUT__LABEL*28
      PARAMETER  ( SC_LUT__LABEL = 'SC_LUT v  2.1  of 2016.02.14' )
      REAL*8     PRES_MIN
      INTEGER*4  IUER
      INTEGER*8  DIR_DESC(16), MEL
      INTEGER*4, ALLOCATABLE :: LUT(:), LUT_ACC(:)
      INTEGER*4  J1, J2, J3, J4, J5, L_FIL, LEV, IS, IP, NEL, NEL_ACC, IND
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, IFIND_SORT_PL
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
!     $MALO_DIR/bin/sc_lut /s0/sc_load/mod44w_sc_21599_2699.heb $MALO_DIR/share/mod44w_sc_21599_2699_lut.heb 1.0E-9
!
      PRES_MIN = 0.0
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: input_file output_file pres_min'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILOUT )
           CALL GETARG ( 3, STR    )
           READ ( UNIT=STR, FMT='(F10.5)' ) PRES_MIN
      END IF
!
      IUER = -1
      CALL READ_HEB_HEADER ( FILIN, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6901, IUER, 'SC_LUT', 'Failure to parse '// &
     &         'the header of the first input heb-file '//C_FIL(1) )
           CALL EXIT ( 1 )
      END IF
!    
      MEL = HEB%DIMS(1)*HEB%DIMS(2)
      ALLOCATE ( LUT(MEL), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*MEL, STR )
           IUER = -1
           CALL ERR_LOG ( 6902, IUER, 'SC_LUT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes memory for array LUT' )
           CALL EXIT ( 1 )
      END IF
      ALLOCATE ( LUT_ACC(MEL), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*MEL, STR )
           IUER = -1
           CALL ERR_LOG ( 6903, IUER, 'SC_LUT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes memory for array LUT' )
           CALL EXIT ( 1 )
      END IF
!
      NEL_ACC = 0
      LUT_ACC = 0
      IUER = -1
      CALL READ_HEB ( FILIN, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6904, IUER, 'SC_LUT', 'Failed to read '// &
     &         'the input file with displacements excterd by unit pressure '// &
     &          FILIN )
           CALL EXIT ( 1 )
      END IF
!
      IND = 0
      NEL = 0
      DO 410 J1=1,HEB%DIMS(2)
         DO 420 J2=1,HEB%DIMS(1)
            IND = IND + 1
            IF ( HEB%DIMS(4) == 2 ) THEN
                 IF ( ABS(HEB%VAL(J2,J1,1,1)) > PRES_MIN .OR. &
                      ABS(HEB%VAL(J2,J1,1,2)) > PRES_MIN .OR. &
                      ABS(HEB%VAL(J2,J1,2,1)) > PRES_MIN .OR. &
                      ABS(HEB%VAL(J2,J1,2,2)) > PRES_MIN .OR. &
                      ABS(HEB%VAL(J2,J1,3,1)) > PRES_MIN .OR. &
                      ABS(HEB%VAL(J2,J1,3,2)) > PRES_MIN      ) THEN
                      IP = IFIND_SORT_PL ( NEL_ACC, LUT_ACC, IND )
                      IF ( IP > 0 ) GOTO 420
                      NEL = NEL + 1
                      IF ( NEL > MEL ) THEN
                           CALL CLRCH  ( STR )
                           CALL IINCH8 ( MEL, STR )
                           IUER = -1
                           CALL ERR_LOG ( 6905, IUER, 'SC_LUT', 'Trap of '// &
     &                         'internal control: parameter MEL '//STR(1:I_LEN(STR))// &
     &                         'is too small' )
                           CALL EXIT ( 1 )
                      END IF
                      LUT(NEL) = IND
                 END IF
              ELSE
                 IF ( ABS(HEB%VAL(J2,J1,1,1)) > PRES_MIN .OR. &
                      ABS(HEB%VAL(J2,J1,2,1)) > PRES_MIN .OR. &
                      ABS(HEB%VAL(J2,J1,3,1)) > PRES_MIN      ) THEN
                      IP = IFIND_SORT_PL ( NEL_ACC, LUT_ACC, IND )
!!   write ( 6, * ) 'j2 = ', int2(j2), ' j1 = ', int2(j1), ' ip= ', ip ! %%%
                      IF ( IP > 0 ) GOTO 420
                      NEL = NEL + 1
                      IF ( NEL > MEL ) THEN
                           CALL CLRCH  ( STR )
                           CALL IINCH8 ( MEL, STR )
                           IUER = -1
                           CALL ERR_LOG ( 6906, IUER, 'SC_LUT', 'Trap of '// &
     &                         'internal control: parameter MEL '//STR(1:I_LEN(STR))// &
     &                         'is too small' )
                           CALL EXIT ( 1 )
                      END IF
                      LUT(NEL) = IND
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
      IF ( NEL > 0 ) THEN
           DO 430 J3=1,NEL
              LUT_ACC(NEL_ACC+J3) = LUT(J3)
 430       CONTINUE 
           NEL_ACC = NEL_ACC + NEL
           CALL SORT_FAST_I4 ( NEL_ACC, LUT_ACC )
      END IF
      WRITE ( 6, * ) 'PRES_MIN= ', PRES_MIN, ' NEL = ', NEL, ' NEL_ACC= ', NEL_ACC
!
      HEB_OUT%DIMS(1) = NEL_ACC
      HEB_OUT%DIMS(2) = 1
      HEB_OUT%DIMS(3) = 1
      HEB_OUT%DIMS(4) = 1
      HEB_OUT%DATA_FORMAT = HEB__I4
      HEB_OUT%DATA_OFFSET      = HEB__HDS
      HEB_OUT%ENDIAN           = HEB__LE
      HEB_OUT%DATA_TRANSFORM   = HEB__NONE
      HEB_OUT%DATA_COMPRESSION = HEB__NONE
      HEB_OUT%OFFSET           = 0.0
      HEB_OUT%SCALE_FACTOR     =  1.0
      HEB_OUT%FILL_VALUE       = -1
      HEB_OUT%UNITS            = 'd/l'
      HEB_OUT%PROD_DATE_TIME   = GET_CDATE()
      HEB_OUT%FILE_NAME        = FILOUT
      HEB_OUT%HISTORY          = ' '
      HEB_OUT%SOURCE           = ' '
      HEB_OUT%SDS_NAME         = 'Look up table for sampling correction'
      HEB_OUT%TITLE            = HEB_OUT%SDS_NAME         
      HEB_OUT%PROD_NAME        = HEB_OUT%SDS_NAME         
      HEB_OUT%INSTITUTION      = 'Astrogeo Center'
      HEB_OUT%REFERENCES       = 'http://astrogeo.org/malo/'
      HEB_OUT%VERSION_ID       = SC_LUT__LABEL
      HEB_OUT%MJD              = J2000__MJD
      HEB_OUT%UTC              = 0.0
      HEB_OUT%TAI              = 0.0
      HEB_OUT%MIN_VALUE        = 1
      HEB_OUT%MAX_VALUE        = LUT_ACC(NEL_ACC)
      HEB_OUT%VALID_RANGE(1)   = HEB_OUT%MIN_VALUE
      HEB_OUT%VALID_RANGE(2)   = HEB_OUT%MAX_VALUE
!
      CALL CLRCH ( STR )
      CALL INCH8 ( HEB%DIMS(2)/2 -1, STR )
      HEB_OUT%COMMENT(1)       = 'Original loading degree: '//STR(1:I_LEN(STR))
      CALL CLRCH  ( STR )
      WRITE ( UNIT=STR(1:11), FMT='(1PE11.4)' ) PRES_MIN
      HEB_OUT%COMMENT(2)       = 'Mimimal pressure: '//STR(1:I_LEN(STR))
      CALL CLRCH  ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(F8.6)' ) NEL_ACC/(1.0*MEL)
      HEB_OUT%COMMENT(3)       = 'Filling ratio: '//STR(1:I_LEN(STR))
      HEB_OUT%COMMENT(4)       = 'Input scamling correction file: '//FILIN(1:I_LEN(FILIN))
!
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, LUT_ACC, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6907, IUER, 'SC_LUT', 'Failure in '// &
     &         'writing sampling correction into the output file '// &
     &          FILOUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, * ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
      CALL EXIT ( 0 )
      END  PROGRAM  SC_LUT  !#!  
