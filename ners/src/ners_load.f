      SUBROUTINE NERS_LOAD ( NERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_LOAD
! *                                                                      *
! *  ### 16-JUN-2016  NERS_LOAD    v2.7 (c)  L. Petrov  24-AUG-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  IUER
      CHARACTER  STR*128
      LOGICAL*1  LEX
      REAL*8     UTC_CUR, UTC_M_TAI
      INTEGER*4  LUN, IS, J1, ID, LPAR_12, LPAR_3, UNIX_DATE, &
     &           FCS_STATIC_SIZE, M_WAIT_TRIES, IER
      CHARACTER  NERS_IO_LOCK_FILE*128, NERS_READ_LOCK_FILE*128, &
     &           NERS_WRITE_LOCK_FILE*128, TEMP_LEAPSEC_FILE*128, COM*256
      LOGICAL*1  FL_V21, FL_V22, FL_V23
      INTEGER*1  BUF(512)
      INTEGER*8  SIZE_I8
      INTEGER*4, EXTERNAL :: GETPID, FILE_INFO, ILEN, I_LEN, LINDEX, READ, TIME
!
      ID = LINDEX ( NERS%CNF%FCS_FILE, '/' )
      IF ( ID == 0 ) ID = ILEN(NERS%CNF%FCS_FILE)
      NERS_IO_LOCK_FILE    = NERS%CNF%FCS_FILE(1:ID)//NERS__IO_LOCK
      NERS_READ_LOCK_FILE  = NERS%CNF%FCS_FILE(1:ID)//NERS__READ_LOCK
      NERS_WRITE_LOCK_FILE = NERS%CNF%FCS_FILE(1:ID)//NERS__WRITE_LOCK
!
      IF ( NERS%FCS_STATUS .EQ. NERS__LOAD ) THEN
           CALL ERR_LOG ( 4411, IUER, 'NERS_LOAD', 'Trap '// &
     &         'of internal control: NERS data structure has already '// &
     &         'been loaded. Please first call routine NERS_QUIT' )
           RETURN 
        ELSE IF ( NERS%FCS_STATUS .NE. NERS__INIT ) THEN
           CALL ERR_LOG ( 4412, IUER, 'NERS_LOAD', 'Trap '// &
     &         'of internal control: NERS data structure was not '// &
     &         'initialized. Please first call routine NERS_INIT' )
           RETURN 
      END IF
!
      IS = FILE_INFO ( TRIM(NERS%CNF%FCS_FILE)//CHAR(0), UNIX_DATE, &
     &                 SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4413, IUER, 'NERS_LOAD', 'Input binary EOP '// &
     &         'file '//TRIM(NERS%CNF%FCS_FILE)//' '//STR )
           RETURN 
      END IF
!
! --- Set write lock
!
      CALL ERR_PASS ( IUER, IER )
      CALL SET_WRITE_LOCK ( NERS_IO_LOCK_FILE, NERS_READ_LOCK_FILE, &
     &                      NERS_WRITE_LOCK_FILE, NERS%CNF%LOCK_TIMEOUT, &
     &                      NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4414, IUER, 'NERS_LOAD', 'Error in setting '// &
     &         'write lock for the ners message file' )
           RETURN 
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( NERS%CNF%FCS_FILE, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4415, IUER, 'NERS_LOAD', 'Error in opening '// &
     &         'the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
      IS = READ ( %VAL(LUN), NERS%FCS%NERS_FMT )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4416, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//'in reading from binary intput '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. LEN(NERS__BIN_FMT) ) THEN
           CALL ERR_LOG ( 4417, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
      FL_V21 = .FALSE.
      FL_V22 = .FALSE.
      FL_V23 = .FALSE.
      IF ( NERS%FCS%NERS_FMT .EQ. NERS__BIN_FMT ) THEN
           CONTINUE 
         ELSE IF ( NERS%FCS%NERS_FMT .EQ. NERS__BIN_FMT_21 ) THEN
           FL_V21 = .TRUE.
         ELSE IF ( NERS%FCS%NERS_FMT .EQ. NERS__BIN_FMT_22 ) THEN
           FL_V22 = .TRUE.
         ELSE IF ( NERS%FCS%NERS_FMT .EQ. NERS__BIN_FMT_23 ) THEN
           FL_V23 = .TRUE.
         ELSE 
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 4418, IUER, 'NERS_LOAD', 'Input file '// &
     &          NERS%CNF%FCS_FILE(1:I_LEN(NERS%CNF%FCS_FILE))//' has wrong '// &
     &         'format. The first line is '//NERS%FCS%NERS_FMT// &
     &         ' while '//NERS__BIN_FMT//' was expected. You may need '// &
     &         'upgrade NERS client library' )
           RETURN 
      END IF
!
! --- Read static variables
!
      FCS_STATIC_SIZE = LOC(NERS%FCS%NERS_STATUS) - LOC(NERS%FCS) + SIZEOF(NERS%FCS%NERS_STATUS)
      IF ( FL_V21 ) FCS_STATIC_SIZE  = 428
      IF ( FL_V22 ) FCS_STATIC_SIZE  = 512
      IF ( FL_V23 ) FCS_STATIC_SIZE  = 1792
      IF ( .NOT. FL_V23 ) THEN
           IS = READ ( %VAL(LUN), NERS%FCS, %VAL(FCS_STATIC_SIZE) )
         ELSE
           IS = READ ( %VAL(LUN), NERS%FCS, %VAL(128) )
           NERS%FCS%E3Z_APR_MOD = NERS__E3Z_D93
           FCS_STATIC_SIZE = FCS_STATIC_SIZE - 128
           IS = READ ( %VAL(LUN), %VAL(LOC(NERS%FCS%HEO_MOD)), %VAL(FCS_STATIC_SIZE) )
      END IF
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4419, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//'in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. FCS_STATIC_SIZE ) THEN
           WRITE ( 6, * ) 'IS = ', IS, ' FCS_STATIC_SIZE= ', FCS_STATIC_SIZE
           CALL ERR_LOG ( 4420, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '// &
     &          NERS%CNF%FCS_FILE )
           RETURN 
      END IF
      IF ( FL_V21 ) THEN
!
! -------- Correction of the FCS sub-object of version 2.10 for missing fields
!
           CALL MEMCPY  ( %VAL(LOC(BUF)),          %VAL(LOC(NERS%FCS)), %VAL(FCS_STATIC_SIZE) )
           CALL MEMCPY  ( %VAL(LOC(NERS%FCS)),     %VAL(LOC(BUF)),      %VAL(208) )
           CALL MEMCPY  ( %VAL(LOC(NERS%FCS)+248), %VAL(LOC(BUF)+208),  %VAL(96) )
           CALL MEMCPY  ( %VAL(LOC(NERS%FCS)+376), %VAL(LOC(BUF)+304),  %VAL(96) )
           CALL MEMCPY  ( %VAL(LOC(NERS%FCS)+480), %VAL(LOC(BUF)+400),  %VAL(12) )
           CALL MEMCPY  ( %VAL(LOC(NERS%FCS)+496), %VAL(LOC(BUF)+412),  %VAL(16) )
           NERS%FCS%NL = 0
           NERS%FCS%LTP_MOD = 'none'
           NERS%FCS%TAI_LAST_EOPS_L = -1.0D0
           CALL CLRCH ( NERS%FCS%EANG_MOD(25:32) )
           CALL CLRCH ( NERS%FCS%NERS_URL(97:128) )
      END IF
!
      IF ( FL_V21 .OR. FL_V22 ) THEN
!
! -------- Correction of the FCS sub-object of versions 2.10 or 2.20 for missing fields
!
           CALL MEMCPY  ( %VAL(LOC(BUF)),           %VAL(LOC(NERS%FCS)), %VAL(512) )
           CALL MEMCPY  ( %VAL(LOC(NERS%FCS)),      %VAL(LOC(BUF)),      %VAL(376) )
           CALL MEMCPY  ( %VAL(LOC(NERS%FCS)+1656), %VAL(LOC(BUF)+376),  %VAL(136) )
           CALL CLRCH   ( NERS%FCS%URL_C ) !! Clean the URL of the C04 EOP series
           CALL CLRCH   ( NERS%FCS%URL_U ) !! Clean the URL of the IGS Ultra-rapid EOP series
           CALL CLRCH   ( NERS%FCS%URL_R ) !! Clean the URL of the IGS Rapid EOP series
           CALL CLRCH   ( NERS%FCS%URL_I ) !! Clean the URL of the IVS Intensive EOP series
           CALL CLRCH   ( NERS%FCS%URL_J ) !! Clean the URL of the IAA Intensive EOP series
           CALL CLRCH   ( NERS%FCS%URL_S ) !! Clean the URL of the 24 hour IVS EOP series
           CALL CLRCH   ( NERS%FCS%URL_F ) !! Clean the URL of the final IGS series
           CALL CLRCH   ( NERS%FCS%URL_A ) !! Clean the URL of the AAM forecast data
           CALL CLRCH   ( NERS%FCS%URL_L ) !! Clean the URL of the the long-term forecast
           CALL CLRCH   ( NERS%FCS%URL_RESERVED ) !! Clean the Url reserved for the future
      END IF
      LPAR_12 =  NERS%FCS%NK_12 + NERS__MDEG - 1
      LPAR_3  =  NERS%FCS%NK_3  + NERS__MDEG - 1
!
! === Allocate memory for dynamic arrays
!
      ALLOCATE ( NERS%FCS%ARG_12(NERS%FCS%NK_12), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4421, IUER, 'NERS_LOAD', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_12' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%ARG_3(NERS%FCS%NK_3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4422, IUER, 'NERS_LOAD', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_3' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%ARG_C(NERS%FCS%NC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4423, IUER, 'NERS_LOAD', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_C' )
           RETURN
      END IF
!
      IF ( NERS%FCS%NL > 0 ) THEN
           ALLOCATE ( NERS%FCS%ARG_L(NERS%FCS%NL), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4424, IUER, 'NERS_LOAD', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%ARG_L' )
                RETURN
           END IF
      END IF
!
      ALLOCATE ( NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4425, IUER, 'NERS_LOAD', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%ARG_UTC_M_TAI' )
           RETURN 
      END IF
      IF ( NERS%FCS%L_HEO > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEO_ARG(NERS%FCS%L_HEO,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4426, IUER, 'NERS_LOAD', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%HEO_ARG' ) 
                RETURN
           END IF
      END IF
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEOR_ARG(NERS%FCS%L_HEOR,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4427, IUER, 'NERS_LOAD', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%HEOR_ARG' ) 
                RETURN
           END IF
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_E12(1-NERS__MDEG:NERS%FCS%NK_12-1,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4428, IUER, 'NERS_LOAD', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_E12' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_E3(1-NERS__MDEG:NERS%FCS%NK_3-1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4429, IUER, 'NERS_LOAD', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_E3' )
           RETURN
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_C(1-NERS__MDEG:NERS%FCS%NC-1,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4430, IUER, 'NERS_LOAD', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_C' )
           RETURN
      END IF
!
      IF ( NERS%FCS%NL > 0 ) THEN
           ALLOCATE ( NERS%FCS%BSPL_L(1-NERS__MDEG:NERS%FCS%NL-1,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4431, IUER, 'NERS_LOAD', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%BSPL_L' )
                RETURN
           END IF
      END IF
!
      ALLOCATE ( NERS%FCS%BSPL_UTC_M_TAI(NERS%FCS%NJ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4432, IUER, 'NERS_LOAD', 'Error in '// &
     &         'an attempt to allocate memory for array NERS%FCS%BSPL_UTC_M_TAI' )
           RETURN 
      END IF
      IF ( NERS%FCS%L_HEO > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEO_AMP(NERS%FCS%L_HEO,2,2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4433, IUER, 'NERS_LOAD', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%HEO_AMP' ) 
                RETURN
           END IF
      END IF
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           ALLOCATE ( NERS%FCS%HEOR_AMP(NERS%FCS%L_HEOR,2,2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4434, IUER, 'NERS_LOAD', 'Error in '// &
     &              'an attempt to allocate memory for array NERS%FCS%HEOR_AMP' ) 
                RETURN
           END IF
      END IF
!
! === Read dynamic arrays
!
! --- Read array ARG_12
!
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_12, %VAL(SIZEOF(NERS%FCS%ARG_12)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4435, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_12) ) THEN
           write ( 6, * ) 'NERS%FCS%ARG_12= ', NERS%FCS%ARG_12, ' IS= ', IS
           CALL ERR_LOG ( 4436, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
! --- Read array ARG_3
!
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_3, %VAL(SIZEOF(NERS%FCS%ARG_3)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4437, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_3) ) THEN
           WRITE ( 6, * ) 'IS = ', IS, ' SIZ = ', SIZEOF(NERS%FCS%ARG_3)
           CALL ERR_LOG ( 4438, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
! --- Read array ARG_C
!
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_C, %VAL(SIZEOF(NERS%FCS%ARG_C)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4439, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_C) ) THEN
           CALL ERR_LOG ( 4440, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
      IF ( NERS%FCS%NL > 0 ) THEN
!
! -------- Read array ARG_L
!
           IS = READ ( %VAL(LUN), NERS%FCS%ARG_L, %VAL(SIZEOF(NERS%FCS%ARG_L)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 4441, IUER, 'NERS_LOAD', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_L) ) THEN
                CALL ERR_LOG ( 4442, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
           END IF
      END IF
!
! --- Read array ARG_UTC_M_TAI
!
      NERS%FCS%ARG_UTC_M_TAI = -1.0
      IS = READ ( %VAL(LUN), NERS%FCS%ARG_UTC_M_TAI, %VAL(SIZEOF(NERS%FCS%ARG_UTC_M_TAI)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4443, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%ARG_UTC_M_TAI) ) THEN
           CALL ERR_LOG ( 4444, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
      IF ( NERS%FCS%L_HEO > 0 ) THEN
!
! -------- Read array HEO_ARG
!
           IS = READ ( %VAL(LUN), NERS%FCS%HEO_ARG, %VAL(SIZEOF(NERS%FCS%HEO_ARG)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 4445, IUER, 'NERS_LOAD', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEO_ARG) ) THEN
                CALL ERR_LOG ( 4446, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
           END IF
      END IF
!
! --- Read array HEOR_ARG
!
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
           IS = READ ( %VAL(LUN), NERS%FCS%HEOR_ARG, %VAL(SIZEOF(NERS%FCS%HEOR_ARG)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 4447, IUER, 'NERS_LOAD', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEOR_ARG) ) THEN
                CALL ERR_LOG ( 4448, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
           END IF
      END IF
!
! --- Read array BSPL_E12
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_E12, %VAL(SIZEOF(NERS%FCS%BSPL_E12)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4449, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_E12) ) THEN
           CALL ERR_LOG ( 4450, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
! --- Read array BSPL_E3
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_E3, %VAL(SIZEOF(NERS%FCS%BSPL_E3)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4451, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_E3) ) THEN
           CALL ERR_LOG ( 4452, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
! --- Read array BSPL_C
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_C, %VAL(SIZEOF(NERS%FCS%BSPL_C)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4453, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_C) ) THEN
           CALL ERR_LOG ( 4454, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
! --- Read array BSPL_L
!
      IF ( NERS%FCS%NL > 0 ) THEN
           IS = READ ( %VAL(LUN), NERS%FCS%BSPL_L, %VAL(SIZEOF(NERS%FCS%BSPL_L)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 4455, IUER, 'NERS_LOAD', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_L) ) THEN
                CALL ERR_LOG ( 4456, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
           END IF
      END IF
!
! --- Read array BSPL_UTC_M_TAI
!
      IS = READ ( %VAL(LUN), NERS%FCS%BSPL_UTC_M_TAI, &
     &            %VAL(SIZEOF(NERS%FCS%BSPL_UTC_M_TAI)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4457, IUER, 'NERS_LOAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading from binary input '// &
     &         'EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(NERS%FCS%BSPL_UTC_M_TAI) ) THEN
           CALL ERR_LOG ( 4458, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &         'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
!
      IF ( NERS%FCS%L_HEO > 0 ) THEN
!
! -------- Read array HEO_AMP
!
           IS = READ ( %VAL(LUN), NERS%FCS%HEO_AMP, %VAL(SIZEOF(NERS%FCS%HEO_AMP)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 4459, IUER, 'NERS_LOAD', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEO_AMP) ) THEN
                CALL ERR_LOG ( 4460, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
           END IF
      END IF
!
! --- Read array HEOR_ARG
!
      IF ( NERS%FCS%L_HEOR > 0 ) THEN
!
! -------- Read array HEOR_AMP
!
           IS = READ ( %VAL(LUN), NERS%FCS%HEOR_AMP, %VAL(SIZEOF(NERS%FCS%HEOR_AMP)) )
           IF ( IS == -1 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 4461, IUER, 'NERS_LOAD', 'Error '// &
     &               STR(1:I_LEN(STR))//' in reading from binary input '// &
     &              'EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
             ELSE IF ( IS .NE. SIZEOF(NERS%FCS%HEOR_AMP) ) THEN
                WRITE ( 6, * ) 'IS= ', IS, ' SI = ', SIZEOF(NERS%FCS%HEOR_AMP) 
                CALL ERR_LOG ( 4462, IUER, 'NERS_LOAD', 'Not all bytes '// &
     &              'were read from the input binary EOP file '//NERS%CNF%FCS_FILE )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4463, IUER, 'NERS_LOAD', 'Error in closing '// &
     &         'the input binary EOP file '//NERS%CNF%FCS_FILE )
           RETURN 
      END IF
      CALL LIFT_READ_WRITE_LOCKS ( NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK )
!
      UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
!
      NERS%FCS_STATUS = NERS__LOAD
      CALL ERR_PASS   ( IUER, IER )
      CALL NERS_GET_UTCMTAI ( NERS, UTC_CUR, UTC_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4464, IUER, 'NERS_LOAD', 'Error in getting '// &
     &         'UTC minus TAI difference' )
           RETURN 
      END IF
      NERS%UTC_LOAD = UTC_CUR
      NERS%TIM_LOAD = NERS%UTC_LOAD  - UTC_M_TAI
      NERS%UTC_FILE = UNIX_DATE - UNIX__J2000_UTC
      NERS%TIM_FILE = NERS%UTC_FILE - NERS%UTC_FILE 
!
! --- Check the leap second file
!
      IS = FILE_INFO ( TRIM(NERS%CNF%LEAPSEC_FILE)//CHAR(0), UNIX_DATE, SIZE_I8 )
!
! --- We write done leap second file if
! --- 1) it does not exist or
! --- 2) too short or
! --- 3) too old
!
      IF ( IS .NE. 0                                   .OR. &
     &     SIZE_I8 < NERS__LPS_FIL_LEN_MIN             .OR. &
     &     (TIME(%VAL(0)) - UNIX_DATE) > NERS__LPS_AGE      ) THEN
!
! -------- The leap second file either does not exist or too old
!
           DO 410 J1=1,NERS%CNF%N_TRIES
!
! ----------- Write the leap second file into the temporary file
!
              WRITE ( UNIT=STR(1:8), FMT='(I8.8)' ) GETPID()
              TEMP_LEAPSEC_FILE = TRIM(NERS%CNF%LEAPSEC_FILE)//'_'//STR(1:8)
!
              CALL ERR_PASS ( IUER, IER )
              CALL NERS_WRITE_LEAPSEC ( NERS, TEMP_LEAPSEC_FILE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4466, IUER, 'NERS_LOAD', 'Error in an attempt '// &
     &                 'to save local copy of leap second file' )
                   RETURN 
              END IF
              COM = 'chmod u+rw,o+rw,g+rw '//TRIM(TEMP_LEAPSEC_FILE)//'> /dev/null 2>&1'
              IS  =  SYSTEM ( TRIM(COM)//CHAR(0) )
!
! ----------- Lift the write lock file
!
              CALL LIFT_READ_WRITE_LOCKS ( NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK )
!
! ----------- Set read lock
!
              CALL ERR_PASS ( IUER, IER )
              CALL SET_READ_LOCK ( NERS_IO_LOCK_FILE, NERS_READ_LOCK_FILE, &
     &                             NERS_WRITE_LOCK_FILE, NERS%CNF%LOCK_TIMEOUT, &
     &                             NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4465, IUER, 'NERS_LOAD', 'Error in setting '// &
     &                 'read lock for the ners leap second file' )
                   RETURN 
              END IF
              SIZE_I8 = 0
              IS = FILE_INFO ( TRIM(TEMP_LEAPSEC_FILE)//CHAR(0), UNIX_DATE, SIZE_I8 )
              IF ( SIZE_I8 > 0 ) THEN
!
! ---------------- Rename a temporary file into the permanent file
!
                   CALL RENAME ( TRIM(TEMP_LEAPSEC_FILE)//CHAR(0), TRIM(NERS%CNF%LEAPSEC_FILE)//CHAR(0) )
                 ELSE
!
! ---------------- This is a pathological case
!
                   CALL UNLINK ( TRIM(TEMP_LEAPSEC_FILE)//CHAR(0) )
                   CALL LIFT_READ_WRITE_LOCKS ( NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK )
                   IF ( J1 .NE. NERS%CNF%N_TRIES ) THEN
                        CALL NSLEEP ( NERS__LPS_WRITE_TIMEOUT )
                   END IF
                   GOTO 410
              END IF
!
! ----------- Lift the read lock
!
              CALL LIFT_READ_WRITE_LOCKS ( NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK )
              GOTO 810
 410       CONTINUE 
           CALL LIFT_READ_WRITE_LOCKS ( NERS%CNF%FD_READ_LOCK, NERS%CNF%FD_WRITE_LOCK )
 810       CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_LOAD  !#!#
