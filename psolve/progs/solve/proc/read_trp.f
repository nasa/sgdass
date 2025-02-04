      SUBROUTINE READ_TRP ( FINAM, TRP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_TRP  reads and parses the file FINAM with external    *
! *   atmospheric path delays. The file is supposed to conform           *
! *   specifications "TROPO_PATH_DELAY  Exchange format".                *
! *   The results of parsing are put in the fields of object TRP.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FINAM ( CHARACTER ) -- Name of the file with external atmospheric    *
! *                        path delays.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   TRP ( RECORD    ) -- The object that keeps the field of the        *
! *                        external atmospheric path delays model.       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 or IUER=-2 --     *
! *                                  in the case of error the message    *
! *                                  will be put on stdout.              *
! *                           Output: If input value IUER == -2, the     *
! *                                   variable is mot modified,          *
! *                                   otherwise IUER=0 in the case of    *
! *                                   successful completion and non-zero *
! *                                   in the case of error.              *
! *                                                                      *
! *  ### 08-FEB-2008    READ_TRP   v2.0 (c)  L. Petrov  03-OCT-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FINAM*(*)
      INCLUDE   'astro_constants.i'
      INCLUDE   'trp.i'
      TYPE      ( TRP__TYPE )           :: TRP
      TYPE      ( TRP_OBS_O_REC__TYPE ) :: SCA_O_REC
      TYPE      ( TRP_OBS_D_REC__TYPE ) :: SCA_D_REC
      TYPE      ( TRP_STA_REC__TYPE   ) :: STA_REC
      INTEGER*4  IUER
      INTEGER*8  SIZE_I8
      REAL*8       FE, PHI_EPS 
      PARAMETER  ( FE      = 1.D0/298.257D0  ) ! Earth's flattening
      PARAMETER  ( PHI_EPS = 1.D-8 ) 
      CHARACTER, ALLOCATABLE :: BUF(:)*(M_LLN__TRP)
      CHARACTER  C_SCA(MSCA__TRP)*16, C_STA_ARR(MSTA__TRP)*8, &
     &           C_STA(MSTA__TRP)*8, C_GLO_STA(MSTA__TRP)*8, STR*128
      LOGICAL*4  FL_VER_V10, FL_VER_V11, FL_USAGE_REC
      INTEGER*4  UNIX_DATE, IS, J1, J2, J3, J4, M_BUF, N_BUF, &
     &           I_SCA, I_STA, K_SCA, K_STA, K_OBS, I_SCA_OLD, L_STA, &
     &           L_STA_ARR(MSCA__TRP), IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ADD_CLIST, FILE_INFO, ILEN, I_LEN, LTM_DIF
!
! --- Learn the file size
!
      IS = FILE_INFO ( FINAM(1:I_LEN(FINAM))//CHAR(0), UNIX_DATE, SIZE_I8 )
!
! --- Deterime the approsimate number of lines in the input file
!
      M_BUF = SIZE_I8/(M_LLN__TRP_V1+1) + 4*M_HDR__TRP + 1024
!
! --- Allocate the buffer for parsing the input file
!
      ALLOCATE ( BUF(M_BUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( M_BUF*M_LLN__TRP, STR )
           CALL ERR_LOG ( 8451, IUER, 'READ_TRP', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of operative memory for '// &
     &         ' a temporary array' )
           RETURN
      END IF
!
! --- Read the input file with the external atmospheric path delay
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8452, IUER, 'READ_TRP', 'Error in attempt to '// &
     &         'read the file with the external atmospheric path delays '// &
     &          FINAM )
           RETURN
      END IF
!
! --- Check the first line. It should have the format label
!
      FL_VER_V10 = .FALSE.
      FL_VER_V11 = .FALSE.
      IF ( BUF(1)(1:LEN(TRP__LABEL)) == TRP__LABEL_V10 ) THEN
           FL_VER_V10 = .TRUE.
         ELSE IF ( BUF(1)(1:LEN(TRP__LABEL)) == TRP__LABEL_V11 ) THEN
           FL_VER_V11 = .TRUE.
         ELSE IF ( BUF(1)(1:LEN(TRP__LABEL)) == TRP__LABEL ) THEN
           CONTINUE
         ELSE
           CALL ERR_LOG ( 8453, IUER, 'TRP_INIT', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' with external atmospheric path '// &
     &         'delays has wrong format: the first line is '// &
     &          BUF(1)(1:I_LEN(BUF(1)))//' while '//TRP__LABEL// &
     &         ' was expected' )
           RETURN
      END IF
!
! --- Check the last line. It should have the format label
!
      IF ( BUF(N_BUF)(1:LEN(TRP__LABEL)) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 8454, IUER, 'TRP_INIT', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' with external atmospheric path '// &
     &         'apparently was truncated: the last line '// &
     &          BUF(N_BUF)(1:I_LEN(N_BUF))//' is not the same '// &
     &         'as the first line '//BUF(1) )
           RETURN
      END IF
!
! --- Intialisation
!
      CALL NOUT ( SIZEOF(TRP), TRP       )
      CALL NOUT ( MSCA__TRP,   L_STA_ARR )
      TRP%FILE_NAME = FINAM
!
! --- The first run through the file. We collect the number of stations
! --- and the number of scans
!
      DO 420 J2=2,N_BUF-1
         IF ( BUF(J2)(1:1) == 'E' ) THEN
              CONTINUE
            ELSE IF ( BUF(J2)(1:1) == 'H' ) THEN
              CONTINUE
            ELSE IF ( BUF(J2)(1:1) == 'M' ) THEN
              CONTINUE
            ELSE IF ( BUF(J2)(1:1) == 'U' ) THEN
              CONTINUE
            ELSE IF ( BUF(J2)(1:1) == 'S' ) THEN
              TRP%N_STA = TRP%N_STA + 1
            ELSE IF ( BUF(J2)(1:1) == 'O' ) THEN
!
! ----------- Copy the input line into SCA_REC
!
              CALL LIB$MOVC3 ( SIZEOF(SCA_D_REC), %REF(BUF(J2)), SCA_D_REC )
!
! ----------- Get the scan index and update the scan counter if needed
!
              CALL ERR_PASS ( IUER, IER )
              I_SCA = ADD_CLIST ( MSCA__TRP, TRP%N_SCA, C_SCA, &
     &                            SCA_D_REC%SCAN_NAME, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 8455, IUER, 'READ_TRP', 'Trap of '// &
     &                 'internal control during an attempt to update '// &
     &                 'scan list in processing line '//STR(1:I_LEN(STR))// &
     &                 'of the external atmospheric path delay file '// &
     &                  FINAM )
                   RETURN
              END IF
!
! ----------- Get the station index fro the scan and update the station
! ----------- counter for this scan if needed
!
              I_STA = ADD_CLIST ( MSTA__TRP, L_STA_ARR(TRP%N_SCA), &
     &                            C_STA_ARR, SCA_D_REC%STA_NAM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 8456, IUER, 'READ_TRP', 'Trap of '// &
     &                 'internal control during an attempt to update '// &
     &                 'scan list in processing line '//STR(1:I_LEN(STR))// &
     &                 'of the external atmospheric path delay file '// &
     &                  FINAM )
                   RETURN
              END IF
            ELSE IF ( BUF(J2)(1:1) == 'D' ) THEN
!
! ----------- Copy the input line into SCA_REC
!
              CALL LIB$MOVC3 ( SIZEOF(SCA_D_REC), %REF(BUF(J2)), SCA_D_REC )
!
! ----------- Get the scan index and update the scan counter if needed
!
              CALL ERR_PASS ( IUER, IER )
              I_SCA = ADD_CLIST ( MSCA__TRP, TRP%N_SCA, C_SCA, &
     &                            SCA_D_REC%SCAN_NAME, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 8457, IUER, 'READ_TRP', 'Trap of '// &
     &                 'internal control during an attempt to update '// &
     &                 'scan list in processing line '//STR(1:I_LEN(STR))// &
     &                 'of the external atmospheric path delay file '// &
     &                  FINAM )
                   RETURN
              END IF
!
! ----------- Get the station index fro the scan and update the station
! ----------- counter for this scan if needed
!
              I_STA = ADD_CLIST ( MSTA__TRP, L_STA_ARR(TRP%N_SCA), &
     &                            C_STA_ARR, SCA_D_REC%STA_NAM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 8458, IUER, 'READ_TRP', 'Trap of '// &
     &                 'internal control during an attempt to update '// &
     &                 'scan list in processing line '//STR(1:I_LEN(STR))// &
     &                 'of the external atmospheric path delay file '// &
     &                  FINAM )
                   RETURN
              END IF
            ELSE IF ( BUF(J2)(1:1) == '#' ) THEN
              GOTO 420
            ELSE IF ( BUF(J2)(1:1) == ' ' ) THEN
              GOTO 420
            ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 8459, IUER, 'READ_TRP', 'Error in parsing '// &
     &            'line '//STR(1:I_LEN(STR))//' of the external file '// &
     &            'with atmospheric path delay '//FINAM(1:I_LEN(FINAM))// &
     &            ' -- unknown record type' )
              RETURN
         END IF
 420  CONTINUE
!
! --- Now we knwop the number of stations and the number of scans.
! --- Allocate memory for some fields
!
      ALLOCATE ( TRP%C_SCA(TRP%N_SCA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( TRP%N_SCA*10, STR )
           CALL ERR_LOG ( 8460, IUER, 'READ_TRP', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'TRP%C_SCA' )
           RETURN
      END IF
      ALLOCATE ( TRP%SCA(TRP%N_SCA),   STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( TRP%N_SCA*SIZEOF(TRP%SCA(1)), STR )
           CALL ERR_LOG ( 8461, IUER, 'READ_TRP', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'TRP%SCA' )
           RETURN
      END IF
!
      ALLOCATE ( TRP%STA(TRP%N_STA),   STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( TRP%N_STA*SIZEOF(TRP%STA(1)), STR )
           CALL ERR_LOG ( 8462, IUER, 'READ_TRP', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'TRP%STA' )
           RETURN
      END IF
!
! --- The second run. Collect path delays and related information
!
      K_STA = 0
      K_SCA = 0
      I_SCA_OLD = 0
      TRP%MODE_ZEN        = UNDF__TRP   
      TRP%MODE_SLANT      = UNDF__TRP   
      TRP%MODE_PART_ZEN   = UNDF__TRP   
      TRP%MODE_PART_NORTH = UNDF__TRP   
      TRP%MODE_PART_EAST  = UNDF__TRP   
      FL_USAGE_REC        = .FALSE.
      DO 430 J3=2,N_BUF-1
         IF ( BUF(J3)(1:1) == 'E' ) THEN
              TRP%DB_NAME = BUF(J3)(4:13)
            ELSE IF ( BUF(J3)(1:1) == 'H' ) THEN
              TRP%MK3_DBNM = BUF(J3)(4:13)
            ELSE IF ( BUF(J3)(1:1) == 'M' ) THEN
              TRP%MODEL = BUF(J3)(4:13)
            ELSE IF ( BUF(J3)(1:1) == 'U' ) THEN
              IF ( INDEX ( BUF(J3), 'ZEN'   ) > 0 ) TRP%MODE_ZEN   = USE__TRP
              IF ( INDEX ( BUF(J3), 'SLANT' ) > 0 ) TRP%MODE_SLANT = USE__TRP
              IF ( INDEX ( BUF(J3), 'DERZ'  ) > 0 ) TRP%MODE_PART_ZEN   = USE__TRP
              IF ( INDEX ( BUF(J3), 'DERN'  ) > 0 ) TRP%MODE_PART_NORTH = USE__TRP
              IF ( INDEX ( BUF(J3), 'DERE'  ) > 0 ) TRP%MODE_PART_EAST  = USE__TRP
              FL_USAGE_REC = .TRUE.
            ELSE IF ( BUF(J3)(1:1) == 'S' ) THEN
              K_STA = K_STA + 1
!
! ----------- Copy the station record
!
              CALL LIB$MOVC3 ( SIZEOF(STA_REC), %REF(BUF(J3)), &
     &                         STA_REC )
!
! ----------- Parse the station record
!
              TRP%STA(K_STA)%NAME = STA_REC%NAME
              READ ( UNIT=STA_REC%X_COO,   FMT='(F13.4)' ) TRP%STA(K_STA)%COO(1)
              READ ( UNIT=STA_REC%Y_COO,   FMT='(F13.4)' ) TRP%STA(K_STA)%COO(2)
              READ ( UNIT=STA_REC%Z_COO,   FMT='(F13.4)' ) TRP%STA(K_STA)%COO(3)
              READ ( UNIT=STA_REC%LAT_GCN, FMT='(F8.4)'  ) TRP%STA(K_STA)%PHI_GCN
              READ ( UNIT=STA_REC%LONG,    FMT='(F8.4)'  ) TRP%STA(K_STA)%LONG
              READ ( UNIT=STA_REC%HEI_ELL, FMT='(F6.2)'  ) TRP%STA(K_STA)%HEI_ELL
              TRP%STA(K_STA)%PHI_GCN = TRP%STA(K_STA)%PHI_GCN*DEG__TO__RAD
              TRP%STA(K_STA)%LONG    = TRP%STA(K_STA)%LONG*DEG__TO__RAD
              C_GLO_STA(K_STA) = STA_REC%NAME
              IF ( DABS(TRP%STA(K_STA)%PHI_GCN - P2I ) < PHI_EPS ) THEN
                   TRP%STA(K_STA)%PHI_GDT = TRP%STA(K_STA)%PHI_GCN
                 ELSE IF ( DABS(TRP%STA(K_STA)%PHI_GCN + P2I ) < PHI_EPS ) THEN
                   TRP%STA(K_STA)%PHI_GDT = TRP%STA(K_STA)%PHI_GCN
                 ELSE 
                   TRP%STA(K_STA)%PHI_GDT = &
     &                     DATAN ( DTAN(TRP%STA(K_STA)%PHI_GCN)/(1.0D0 - FE**2) )
              END IF
            ELSE IF ( BUF(J3)(1:1) == 'O' ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
!
! ----------- Copy the scan record
!
              CALL LIB$MOVC3 ( SIZEOF(SCA_O_REC), %REF(BUF(J3)), SCA_O_REC )
              I_SCA = LTM_DIF ( 1, TRP%N_SCA, C_SCA, SCA_O_REC%SCAN_NAME )
              IF ( I_SCA < 1 ) THEN
                   CALL ERR_LOG ( 8463, IUER, 'READ_TRP', 'Trap of '// &
     &                 'internal control during parsing line '// &
     &                  STR(1:I_LEN(STR))// &
     &                 ' of the external atmospheric path delay file '// &
     &                  FINAM )
                   RETURN
              END IF
!
              IF ( I_SCA > I_SCA_OLD ) THEN
!
! ---------------- Aga! This is the new scan.
!
                   TRP%C_SCA(I_SCA) = SCA_O_REC%SCAN_NAME 
                   TRP%SCA(I_SCA)%L_STA = L_STA_ARR(I_SCA) ! Set the number of stations
!
! ---------------- Allocate dynamic memory
!
                   ALLOCATE ( TRP%SCA(I_SCA)%DAT(TRP%SCA(I_SCA)%L_STA), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8464, IUER, 'READ_TRP', 'Failure to '// &
     &                      'allocate dynamic memory for TRP%SCA(I_SCA)%DAT' )
                        RETURN
                   END IF
!
                   ALLOCATE ( TRP%SCA(I_SCA)%IND_STA(TRP%SCA(I_SCA)%L_STA), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8465, IUER, 'READ_TRP', 'Failure to '// &
     &                      'allocate dynamic memory for TRP%SCA(I_SCA)%C_STA' )
                        RETURN
                   END IF
!
! ---------------- Initialization
!
                   CALL NOUT_I4 ( TRP%SCA(I_SCA)%L_STA, TRP%SCA(I_SCA)%IND_STA )
                   CALL NOUT ( SIZEOF(TRP%SCA(I_SCA)%DAT(1))*TRP%SCA(I_SCA)%L_STA, &
     &                         TRP%SCA(I_SCA)%DAT )
                   L_STA = 0
              END IF
!
! ----------- Find the station index for this scan
!
              I_STA = ADD_CLIST ( MSTA__TRP, L_STA, C_STA, SCA_O_REC%STA_NAM, IER )
              IF ( IER .NE. 0 .OR. I_STA < 1 ) THEN
                   CALL ERR_LOG ( 8466, IUER, 'READ_TRP', 'Trap of '// &
     &                 'internal control during an attempt to update '// &
     &                 'scan list in processing line '//STR(1:I_LEN(STR))// &
     &                 'of the external atmospheric path delay file '// &
     &                  FINAM )
                   RETURN
              END IF
!
! ----------- Find the index of this station in the global station list
!
              TRP%SCA(I_SCA)%IND_STA(I_STA) = LTM_DIF ( 1, TRP%N_STA, &
     &                                            C_GLO_STA, SCA_O_REC%STA_NAM )
!
! ----------- Parse scan record of the input file
!
              READ ( UNIT=SCA_O_REC%ATM_PRES,   FMT='(F6.1)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%PRES
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8467, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed ATM_PRES in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%ATM_PRES//'" ' )
                   RETURN
              END IF
              TRP%SCA(I_SCA)%DAT(I_STA)%PRES = 100.0D0*TRP%SCA(I_SCA)%DAT(I_STA)%PRES
!
              READ ( UNIT=SCA_O_REC%TEMP_C,     FMT='(F5.1)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%TEMP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8468, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed TEMP_C in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%TEMP_C//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_O_REC%EL_DEG,     FMT='(F8.5)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%EL
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8469, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed EL_DEG in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%EL_DEG//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_O_REC%AZ_DEG,     FMT='(F9.5)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%AZ
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8470, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed AZ_DEG in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%AZ_DEG//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_O_REC%PATH_DEL,   FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DEL_TOT_SLANT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8471, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed PATH_DEL in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%PATH_DEL//'" ' )
                   RETURN
              END IF
              READ ( UNIT=SCA_O_REC%DER_ZEN,    FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DER_ZEN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8472, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DER_ZEN in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%DER_ZEN//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_O_REC%DER_GR_N,    FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DER_TILT_NORTH
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8473, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DER_GR_N in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%DER_GR_N//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_O_REC%DER_GR_E,    FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DER_TILT_EAST
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8474, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DER_GR_E in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%DER_GR_E//'" ' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( SCA_O_REC%DATE_TAI, &
     &                            TRP%SCA(I_SCA)%MJD, &
     &                            TRP%SCA(I_SCA)%TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8475, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DATE_TAI in processing line '// &
     &                  STR(1:I_LEN(STR))//'of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_O_REC%DATE_TAI//'" ' )
                   RETURN
              END IF
!
! ----------- Transform units to radians
!
              TRP%SCA(I_SCA)%DAT(I_STA)%EL = TRP%SCA(I_SCA)%DAT(I_STA)%EL*DEG__TO__RAD
              TRP%SCA(I_SCA)%DAT(I_STA)%AZ = TRP%SCA(I_SCA)%DAT(I_STA)%AZ*DEG__TO__RAD
!
              IF ( TRP%SCA(I_SCA)%DAT(I_STA)%DEL_TOT_SLANT .EQ. 0.0D0 .AND. &
     &             TRP%SCA(I_SCA)%DAT(I_STA)%DER_ZEN       .EQ. 0.0D0       ) THEN
                   TRP%SCA(I_SCA)%DAT(I_STA)%FL_SLANT = .FALSE.
                 ELSE 
                   TRP%SCA(I_SCA)%DAT(I_STA)%FL_SLANT = .TRUE.
              END IF
              IF ( TRP%SCA(I_SCA)%DAT(I_STA)%DER_TILT_NORTH .EQ. 0.0D0 .AND. &
     &             TRP%SCA(I_SCA)%DAT(I_STA)%DER_TILT_EAST  .EQ. 0.0D0 ) THEN
                   TRP%SCA(I_SCA)%DAT(I_STA)%FL_TILT  = .FALSE.
                 ELSE 
                   TRP%SCA(I_SCA)%DAT(I_STA)%FL_TILT  = .TRUE.
              END IF
              TRP%SCA(I_SCA)%DAT(I_STA)%FL_HDR = .FALSE.
              TRP%SCA(I_SCA)%DAT(I_STA)%FL_NHD = .FALSE.
!
              I_SCA_OLD = I_SCA ! Store the current scan index
            ELSE IF ( BUF(J3)(1:1) == 'D' ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
!
! ----------- Copy the scan record
!
              CALL LIB$MOVC3 ( SIZEOF(SCA_D_REC), %REF(BUF(J3)), SCA_D_REC )
              I_SCA = LTM_DIF ( 1, TRP%N_SCA, C_SCA, SCA_D_REC%SCAN_NAME )
              IF ( I_SCA < 1 ) THEN
                   CALL ERR_LOG ( 8476, IUER, 'READ_TRP', 'Trap of '// &
     &                 'internal control during parsing line '// &
     &                  STR(1:I_LEN(STR))// &
     &                 ' of the external atmospheric path delay file '// &
     &                  FINAM )
                   RETURN
              END IF
!
              IF ( I_SCA > I_SCA_OLD ) THEN
!
! ---------------- Aga! This is the new scan.
!
                   TRP%C_SCA(I_SCA) = SCA_D_REC%SCAN_NAME 
                   TRP%SCA(I_SCA)%L_STA = L_STA_ARR(I_SCA) ! Set the number of stations
!
! ---------------- Allocate dynamic memory
!
                   ALLOCATE ( TRP%SCA(I_SCA)%DAT(TRP%SCA(I_SCA)%L_STA), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8477, IUER, 'READ_TRP', 'Failure to '// &
     &                      'allocate dynamic memory for TRP%SCA(I_SCA)%DAT' )
                        RETURN
                   END IF
!
                   ALLOCATE ( TRP%SCA(I_SCA)%IND_STA(TRP%SCA(I_SCA)%L_STA), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8478, IUER, 'READ_TRP', 'Failure to '// &
     &                      'allocate dynamic memory for TRP%SCA(I_SCA)%C_STA' )
                        RETURN
                   END IF
!
! ---------------- Initialization
!
                   CALL NOUT_I4 ( TRP%SCA(I_SCA)%L_STA, TRP%SCA(I_SCA)%IND_STA )
                   CALL NOUT ( SIZEOF(TRP%SCA(I_SCA)%DAT(1))*TRP%SCA(I_SCA)%L_STA, &
     &                         TRP%SCA(I_SCA)%DAT )
                   L_STA = 0
              END IF
!
! ----------- Find the station index for this scan
!
              I_STA = ADD_CLIST ( MSTA__TRP, L_STA, C_STA, SCA_D_REC%STA_NAM, IER )
              IF ( IER .NE. 0 .OR. I_STA < 1 ) THEN
                   CALL ERR_LOG ( 8479, IUER, 'READ_TRP', 'Trap of '// &
     &                 'internal control during an attempt to update '// &
     &                 'scan list in processing line '//STR(1:I_LEN(STR))// &
     &                 'of the external atmospheric path delay file '// &
     &                  FINAM )
                   RETURN
              END IF
!
! ----------- Find the index of this station in the global station list
!
              TRP%SCA(I_SCA)%IND_STA(I_STA) = LTM_DIF ( 1, TRP%N_STA, &
     &                                            C_GLO_STA, SCA_D_REC%STA_NAM )
!
! ----------- Parse scan record of the input file
!
              READ ( UNIT=SCA_D_REC%ATM_PRES,   FMT='(F6.1)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%PRES
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8480, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed ATM_PRES in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%ATM_PRES//'" ' )
                   RETURN
              END IF
              TRP%SCA(I_SCA)%DAT(I_STA)%PRES = 100.0D0*TRP%SCA(I_SCA)%DAT(I_STA)%PRES
!
              READ ( UNIT=SCA_D_REC%TEMP_C,     FMT='(F5.1)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%TEMP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8481, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed TEMP_C in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%TEMP_C//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_D_REC%EL_DEG,     FMT='(F8.5)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%EL
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8482, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed EL_DEG in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%EL_DEG//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_D_REC%AZ_DEG,     FMT='(F9.5)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%AZ
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8483, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed AZ_DEG in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%AZ_DEG//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_D_REC%DEL_TOT_SLANT, FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DEL_TOT_SLANT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8484, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed PATH_DEL in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%DEL_TOT_SLANT//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_D_REC%DEL_HDR_SLANT, FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DEL_HDR_SLANT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8485, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DER_ZEN in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%DEL_HDR_SLANT//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_D_REC%DEL_NHD_SLANT, FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DEL_NHD_SLANT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8486, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DER_ZEN in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%DEL_NHD_SLANT//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_D_REC%DEL_TOT_ZEN, FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DEL_TOT_ZEN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8487, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed PATH_DEL in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%DEL_TOT_ZEN//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_D_REC%DEL_HDR_ZEN, FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DEL_HDR_ZEN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8488, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DER_ZEN in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%DEL_HDR_ZEN//'" ' )
                   RETURN
              END IF
!
              READ ( UNIT=SCA_D_REC%DEL_NHD_ZEN, FMT='(F15.7)', IOSTAT=IER ) &
     &               TRP%SCA(I_SCA)%DAT(I_STA)%DEL_NHD_ZEN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8489, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DER_ZEN in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%DEL_NHD_ZEN//'" ' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( SCA_D_REC%DATE_TAI, &
     &                            TRP%SCA(I_SCA)%MJD, &
     &                            TRP%SCA(I_SCA)%TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8490, IUER, 'READ_TRP', 'Error in '// &
     &                 'parsing filed DATE_TAI in processing line '// &
     &                  STR(1:I_LEN(STR))//' of the external atmospheric '// &
     &                 'path delay file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- "'//SCA_D_REC%DATE_TAI//'" ' )
                   RETURN
              END IF
!
! ----------- Transform units to radians
!
              TRP%SCA(I_SCA)%DAT(I_STA)%EL = TRP%SCA(I_SCA)%DAT(I_STA)%EL*DEG__TO__RAD
              TRP%SCA(I_SCA)%DAT(I_STA)%AZ = TRP%SCA(I_SCA)%DAT(I_STA)%AZ*DEG__TO__RAD
              TRP%SCA(I_SCA)%DAT(I_STA)%DER_ZEN = &
     &                TRP%SCA(I_SCA)%DAT(I_STA)%DEL_NHD_SLANT/ &
     &                TRP%SCA(I_SCA)%DAT(I_STA)%DEL_NHD_ZEN
!
              TRP%SCA(I_SCA)%DAT(I_STA)%FL_SLANT = .TRUE.
              TRP%SCA(I_SCA)%DAT(I_STA)%FL_TILT  = .FALSE.
              TRP%SCA(I_SCA)%DAT(I_STA)%FL_HDR   = .TRUE.
              TRP%SCA(I_SCA)%DAT(I_STA)%FL_NHD   = .TRUE.
!
              I_SCA_OLD = I_SCA ! Store the current scan index
            ELSE IF ( BUF(J3)(1:1) == '#' ) THEN
              GOTO 430
            ELSE IF ( BUF(J3)(1:1) == ' ' ) THEN
              GOTO 430
            ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL ERR_LOG ( 8491, IUER, 'READ_TRP', 'Error in parsing '// &
     &            'line '//STR(1:I_LEN(STR))//' of the external file '// &
     &            'with atmospheric path delay '//FINAM(1:I_LEN(FINAM))// &
     &            ' -- unknown record type' )
              RETURN
         END IF
 430  CONTINUE
      IF ( .NOT. FL_VER_V10  .AND.  .NOT. FL_USAGE_REC ) THEN
            CALL ERR_LOG ( 8492, IUER, 'READ_TRP', 'Error in parsing '// &
     &            'the external file with atmospheric path delay '// &
     &            FINAM(1:I_LEN(FINAM))//' -- usage record was not '// &
     &            'found' )
            RETURN
      END IF
!
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE READ_TRP  !#!
