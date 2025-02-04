      SUBROUTINE VTD_READ_NZO ( FILNZO, M_ARR, L_ARR, MJD_ARR, &
     &                          TIM_ARR, POS_ARR, VEL_ARR, NZO_NAME, &
     &                          OBJ_TYPE, CENTER_NAME, REF_NAME, TIM_CODE, &
     &                          COO_CODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_READ_NZO  reads the input file FILNZO with            *
! *   coordinates of the near zone object, parses it and return arrays   *
! *   of coordinates of the near zone object and time tags associated    *
! *   with these coordinates.                                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   FILNZO ( CHARACTER ) -- Name of the file with coordinates of the   *
! *                           the near zone objects. The file should     *
! *                           comply specifications of the               *
! *                           "Apriori positions of a near zone object". *
! *    M_ARR ( INTEGER*4 ) -- The maximal points in the output arrays    *
! *                           of near zone objects.                      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    L_ARR ( INTEGER*4 ) -- The number of points in the array of       *
! *                           the near zone object positions.            *
! *  MJD_ARR ( INTEGER*4 ) -- Array of MJD dates of position arrays of   *
! *                           near zone objects. Dimension: L_ARR.       *
! *  TAI_ARR ( INTEGER*4 ) -- Array of TAU time tags of dates of         *
! *                           position arrays of near zone objects.      *
! *                           Dimension: L_ARR.                          *
! *  POS_ARR ( INTEGER*4 ) -- Array of positions of the near zone        *
! *                           object. Units: meters. Dimension: L_ARR,3. *
! *  VEL_ARR ( INTEGER*4 ) -- Array of velocities of the near zone       *
! *                           object. Units: m/s. Dimension: L_ARR,3.    *
! * NZO_NAME ( CHARACTER ) -- Name of the near zone object.              *
! * OBJ_TYPE ( CHARACTER ) -- Type of the near zone object:              *
! *                           VTD__SPA_REC                               *
! *                           VTD__GEO_SAT                               *
! *                           VTD__MOO_ORB                               *
! *                           VTD__MOO_LND                               *
! *                           VTD__PLA_LND                               *
! *                           VTD__PLA_ORB                               *
! *                           VTD__SOL_ORB                               *
! *                           
! * CENTER_NAME ( CHARACTER ) -- Name of the center of the coordinate    *
! *                              system. Supported names:                *
! *                              "EARTH BARYCENTER"                      *
! *                              "SOLAR SYSTEM BARICENTER"               *
! *    REF_NAME ( CHARACTER ) -- Reference system name. Supported names: *
! *                              "EME2000"                               *
! * TIM_CODE ( INTEGER*4 ) -- Time code of time tags. One of             *
! *                           VTD__TDB, VTD__TDT .                       *
! * COO_CODE ( INTEGER*4 ) -- Code of the metric of the coordinate       *
! *                           system. Supported codes:                   *
! *                           VTD__BRS, VTD__IERS1992, VTD__GRS .        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 02-JAN-2006  VTD_READ_NZO   v3.1 (c) L. Petrov  27-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      CHARACTER  FILNZO*(*), NZO_NAME*(*), OBJ_TYPE*(*), CENTER_NAME*(*), &
     &           REF_NAME*(*) 
      INTEGER*4  TIM_CODE, COO_CODE, M_ARR, L_ARR, MJD_ARR(M_ARR), IUER
      REAL*8     TIM_ARR(M_ARR), POS_ARR(M_ARR,3), VEL_ARR(M_ARR,3)
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      CHARACTER  STR*80, DATE_STR*32, FMT_LABEL*16, TS_NAME*8, REG*4
      CHARACTER  VTD__NZO_LABEL1*72, VTD__NZO_LABEL2*10, VTD__NZO_LABEL3*10
      PARAMETER  ( VTD__NZO_LABEL1 = &
     &            '# Apriori positions of a near zone object.  Format version of 2023.04.16' )
      PARAMETER  ( VTD__NZO_LABEL2 = 'CCSDS_OEM_VERS = 2.0' )
      PARAMETER  ( VTD__NZO_LABEL3 = &
     &            '# Apriori positions of a near zone object.  Format version of 2006.01.02' )
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'=' )
      LOGICAL*1  FL_META
      INTEGER*4  MIND
      PARAMETER  ( MIND = 128 )
      INTEGER*4  J1, J2, J3, J4, MBUF, NBUF, LIND, IND(2,MIND), IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      MBUF = M_ARR + 256
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2671, IUER, 'VTD_READ_NZO', 'Failure to allocate '// &
     &         'dynamic memory for temporary buffer' )
           RETURN 
      END IF
      CENTER_NAME = '???' 
      REF_NAME    = '???'
      TS_NAME     = '???'
      POS_ARR     = 0.0D0
      VEL_ARR     = 0.0D0
      OBJ_TYPE    = '???'
      NZO_NAME    = '???'
!
! --- Read the file with apriori near zone object positions
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT  ( FILNZO, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2672, IUER, 'VTD_READ_NZO', 'Error in attempt to '// &
     &         'read input file '//FILNZO )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(VTD__NZO_LABEL1)) == VTD__NZO_LABEL1 ) THEN
           FMT_LABEL = 'VTD_1'
        ELSE IF ( BUF(1)(1:LEN(VTD__NZO_LABEL2)) == VTD__NZO_LABEL2 ) THEN
           FMT_LABEL = 'CCSDS'
        ELSE IF ( BUF(1)(1:LEN(VTD__NZO_LABEL3)) == VTD__NZO_LABEL3 ) THEN
           FMT_LABEL = 'VTD_3'
        ELSE 
           CALL ERR_LOG ( 2673, IUER, 'VTD_READ_NZO', 'The first line of '// &
     &         'the input file '//FILNZO(1:I_LEN(FILNZO))//' does not '// &
     &         'have a label of recognizable format for the catalogue of '// &
     &         'positions ofthe near zone object' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Parse the file
!
      L_ARR = 0
      FL_META = .FALSE.
      DO 410 J1=2,NBUF
!
! ------ Bypass comments
!
         IF ( BUF(J1)(1:1)   == '#' ) GOTO 410
         IF ( BUF(J1)(1:1)   == '*' ) GOTO 410
         IF ( BUF(J1)(1:1)   == '$' ) GOTO 410
         IF ( ILEN(BUF(J1))  ==  0  ) GOTO 410
         IF ( BUF(J1)(11:11) == ' '   .AND. &
     &        BUF(J1)(10:10) .NE. ' ' .AND. &
     &        BUF(J1)(12:12) .NE. ' '       ) THEN
              BUF(J1)(11:11) = '-'
         END IF
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( BUF(J1)(1:13)  == 'CREATION_DATE' ) GOTO 410
         IF ( BUF(J1)(1:10)  == 'ORIGINATOR'    ) GOTO 410
         IF ( BUF(J1)(1:12)  == 'OBJECT_NAME:'   ) THEN
              CALL CLRCH ( NZO_NAME )
              NZO_NAME = BUF(J1)(IND(1,2):IND(2,2))
              GOTO 410
         END IF
         IF ( BUF(J1)(1:12)  == 'OBJECT_NAME '   ) THEN
              CALL CLRCH ( NZO_NAME )
              NZO_NAME = BUF(J1)(IND(1,2):IND(2,2))
              GOTO 410
         END IF
         IF ( LIND .GE. 2 ) THEN
              IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'OBJECT_TYPE:'  ) THEN
                   OBJ_TYPE = BUF(J1)(IND(1,2):IND(2,2))
                   GOTO 410
              END IF
         END IF
         IF ( LIND .GE. 3 ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'OBJECT_TYPE:'  ) THEN
                   OBJ_TYPE = BUF(J1)(IND(1,3):IND(2,3))
                   GOTO 410
              END IF
         END IF
         IF ( BUF(J1)(1:19)  == 'TIME_ARGUMENT_CODE:'  ) THEN
              TS_NAME = BUF(J1)(IND(1,2):IND(2,2))
              GOTO 410
         END IF
         IF ( BUF(J1)(1:12)  == 'TIME_SYSTEM '  ) THEN
              TS_NAME = BUF(J1)(IND(1,2):IND(2,2))
              GOTO 410
         END IF
         IF ( BUF(J1)(1:7)   == 'COMMENT'       ) GOTO 410
         IF ( BUF(J1)(1:16)  == 'COORDINATE_CODE:'  ) THEN
!
! ----------- Get coordinate system code
!
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'BRS'  ) THEN
                   COO_CODE = VTD__BRS
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'I92'  ) THEN
                   COO_CODE = VTD__IERS1992
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'GRS'  ) THEN
                   COO_CODE = VTD__GRS
                ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'ECF'  ) THEN
                   COO_CODE = VTD__ECF
                 ELSE 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2678, IUER, 'VTD_READ_NZO', 'Unknown '// &
  &                    'coordinate system code '//BUF(J1)(IND(1,2):IND(2,2))//' when '// &
  &                    'parsing line '//STR(1:I_LEN(STR))//' of near zone'// &
  &                    'object catalogue file '//FILNZO(1:I_LEN(FILNZO)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              GOTO 410
         END IF
         IF ( BUF(J1)(1:18)  == 'EPHEMERIDE_ORIGIN:'  ) THEN
              STR = BUF(J1)(IND(1,2):IND(2,2))
              GOTO 410
         END IF
!
! ------ Look for the field "META"
!
         IF ( BUF(J1)(1:10) == 'META_START' ) THEN
              FL_META = .TRUE.
              GOTO 410
            ELSE IF ( BUF(J1)(1:9) == 'META_STOP' ) THEN
              FL_META = .FALSE.
              GOTO 410
         END IF
         CALL TRAN ( 11, BUF(J1), BUF(J1) )
         IF ( FL_META ) THEN
              IF ( LIND < 2 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2674, IUER, 'VTD_READ_NZO', 'Too few words '// &
     &                 'in line '//STR(1:I_LEN(STR))//' of the input file '// &
     &                  FILNZO(1:I_LEN(FILNZO))//' less than 2' ) 
                   RETURN 
              END IF
!
              IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'OBJECT_NAME' ) THEN
                   NZO_NAME = BUF(J1)(IND(1,2):IND(2,LIND)) 
                 ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'CENTER_NAME' ) THEN
                   CENTER_NAME = BUF(J1)(IND(1,2):IND(2,LIND)) 
                 ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'REF_FRAME'   ) THEN
                   REF_NAME = BUF(J1)(IND(1,2):IND(2,LIND)) 
                 ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TIME_SYSTEM' ) THEN
                   TS_NAME  = BUF(J1)(IND(1,2):IND(2,LIND)) 
              END IF 
              GOTO 410
         END IF
!
         L_ARR = L_ARR + 1
         IF ( L_ARR > M_ARR ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_ARR, STR )
              CALL ERR_LOG ( 2675, IUER, 'VTD_READ_NZO', 'Too many epochs '// &
     &            'in the input file '//FILNZO(1:I_LEN(FILNZO))//' more '// &
     &            'than M_ARR: '//STR )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
         IF ( L_ARR == 1 .AND. FMT_LABEL == 'VTD_3' ) THEN
!
! ----------- Get name
!
              IF ( BUF(J1)(31:31) == ' ' ) THEN
                   NZO_NAME = BUF(J1)(32:39)
                 ELSE
                   NZO_NAME = BUF(J1)(31:39)
              END IF
!
! ----------- Get time tag code
!
              IF ( BUF(J1)(41:43) == 'TAI'  ) THEN
                   TIM_CODE = VTD__TAI
                 ELSE IF ( BUF(J1)(41:43) == 'TDB'  ) THEN
                   TIM_CODE = VTD__TDB
                 ELSE IF ( BUF(J1)(41:43) == 'UTC'  ) THEN
                   TIM_CODE = VTD__UTC
!@                   CALL ERR_LOG ( 2676, IUER, 'VTD_READ_NZO', 'Time '// &
!@  &                    'scale tag UTC currently is not supported. This '// &
!@  &                    'flag was found in parsing the near zone object '// &
!@     &                 'catalogue file '//FILNZO(1:I_LEN(FILNZO)) )
!@                   DEALLOCATE ( BUF )
!@                   RETURN 
                 ELSE 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2677, IUER, 'VTD_READ_NZO', 'Unknown '// &
  &                    'time scale tag '//BUF(J1)(41:43)//' when parsing '// &
  &                    'line '//STR(1:I_LEN(STR))//' of near zone'// &
  &                    'object catalogue file '//FILNZO(1:I_LEN(FILNZO)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
! ----------- Get coordinate system code
!
              IF ( BUF(J1)(46:48) == 'BRS'  ) THEN
                   COO_CODE = VTD__BRS
                 ELSE IF ( BUF(J1)(46:48) == 'I92'  ) THEN
                   COO_CODE = VTD__IERS1992
                 ELSE IF ( BUF(J1)(46:48) == 'GRS'  ) THEN
                   COO_CODE = VTD__GRS
                 ELSE IF ( BUF(J1)(46:48) == 'ECF'  ) THEN
                   COO_CODE = VTD__ECF
                 ELSE 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2678, IUER, 'VTD_READ_NZO', 'Unknown '// &
  &                    'coordinate system code '//BUF(J1)(41:43)//' when '// &
  &                    'parsing line '//STR(1:I_LEN(STR))//' of near zone'// &
  &                    'object catalogue file '//FILNZO(1:I_LEN(FILNZO)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
         END IF
!
! ------ Get date
!
         IF ( FMT_LABEL == 'VTD_1' ) THEN
              DATE_STR = BUF(J1)(10:39)
            ELSE IF ( FMT_LABEL == 'VTD_3' ) THEN
              DATE_STR = BUF(J1)(1:30)
            ELSE IF ( FMT_LABEL == 'CCSDS' ) THEN
              IF ( LIND  < 7 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2679, IUER, 'VTD_READ_NZO', 'Trap of '// &
     &                 'internal control during parsing line '// &
     &                  STR(1:I_LEN(STR))//' of near zone object '// &
     &                  'catalogue file '//FILNZO(1:I_LEN(FILNZO))// &
     &                  ' -- too few words: less than 7' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              DATE_STR = BUF(J1)(1:23)
              IP = INDEX ( DATE_STR, 'T' ) 
              IF ( IP > 0 ) DATE_STR(IP:IP) = '_'
         END IF
!
         CALL ERR_PASS     ( IUER, IER )
         CALL DATE_TO_TIME ( DATE_STR, MJD_ARR(L_ARR), TIM_ARR(L_ARR), &
     &                       IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 2680, IUER, 'VTD_READ_NZO', 'Error in '// &
     &            'decoding date at the '//STR(1:I_LEN(STR))//'-th line '// &
     &            ' of the input file '//FILNZO )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
         IF ( FMT_LABEL == 'VTD_1' ) THEN
!
! ----------- Get X-coordinate
!
              READ ( UNIT=BUF(J1)(47:68),   FMT='(F22.8)', IOSTAT=IER ) POS_ARR(L_ARR,1)
              IF ( IER .NE. 0 ) THEN 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2681, IUER, 'VTD_READ_NZO', 'Error '// &
     &                 'in decoding source position field '// &
     &                  BUF(J1)(51:72)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file '//FILNZO )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
! ----------- Get Y-coordinate
!
              READ ( UNIT=BUF(J1)(70:91),  FMT='(F22.8)', IOSTAT=IER ) &
     &               POS_ARR(L_ARR,2)
              IF ( IER .NE. 0 ) THEN 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2682, IUER, 'VTD_READ_NZO', 'Error '// &
     &                 'in decoding source position field '// &
     &                  BUF(J1)(74:95)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of catalogue of near zone objects '//FILNZO ) 
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
! ----------- Get Z-coordinate
!
              READ ( UNIT=BUF(J1)(93:114), FMT='(F22.8)', IOSTAT=IER ) &
     &               POS_ARR(L_ARR,3)
              IF ( IER .NE. 0 ) THEN 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2683, IUER, 'VTD_READ_NZO', 'Error '// &
     &                 'in decoding source position field '// &
     &                  BUF(J1)(97:118)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of catalogue of near zone objects '//FILNZO ) 
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
           ELSE IF ( FMT_LABEL == 'VTD_3' ) THEN
!
! ----------- Get X-coordinate
!
              READ ( UNIT=BUF(J1)(51:72),   FMT='(F22.8)', IOSTAT=IER ) POS_ARR(L_ARR,1)
              IF ( IER .NE. 0 ) THEN 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2684, IUER, 'VTD_READ_NZO', 'Error '// &
     &                 'in decoding source position field '// &
     &                  BUF(J1)(51:72)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file '//FILNZO )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
! ----------- Get Y-coordinate
!
              READ ( UNIT=BUF(J1)(74:95),  FMT='(F22.8)', IOSTAT=IER ) &
     &               POS_ARR(L_ARR,2)
              IF ( IER .NE. 0 ) THEN 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2685, IUER, 'VTD_READ_NZO', 'Error '// &
     &                 'in decoding source position field '// &
     &                  BUF(J1)(74:95)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of catalogue of near zone objects '//FILNZO ) 
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
! ----------- Get Z-coordinate
!
              READ ( UNIT=BUF(J1)(97:118), FMT='(F22.8)', IOSTAT=IER ) &
     &               POS_ARR(L_ARR,3)
              IF ( IER .NE. 0 ) THEN 
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2686, IUER, 'VTD_READ_NZO', 'Error '// &
     &                 'in decoding source position field '// &
     &                  BUF(J1)(97:118)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of catalogue of near zone objects '//FILNZO ) 
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
            ELSE IF ( FMT_LABEL == 'CCSDS' ) THEN
              DO 420 J2=1,3
                 READ ( UNIT=BUF(J1)(IND(1,1+J2):IND(2,1+J2)), FMT='(F22.8)', IOSTAT=IER ) POS_ARR(L_ARR,J2)
                 IF ( IER .NE. 0 ) THEN 
                      CALL CLRCH ( STR ) 
                      CALL INCH  ( J1, STR )
                      CALL ERR_LOG ( 2687, IUER, 'VTD_READ_NZO', 'Error '// &
     &                    'in decoding source position field '// &
     &                     BUF(J1)(IND(1,1+J2):IND(2,1+J2))//' at line '// &
     &                     STR(1:I_LEN(STR))//' of the input file '//FILNZO )
                      DEALLOCATE ( BUF )
                      RETURN 
                 END IF
                 POS_ARR(L_ARR,J2) = POS_ARR(L_ARR,J2)*1.D3
 420          CONTINUE
!
              DO 430 J3=1,3
                 READ ( UNIT=BUF(J1)(IND(1,4+J3):IND(2,4+J3)), FMT='(F22.8)', IOSTAT=IER ) VEL_ARR(L_ARR,J3)
                 IF ( IER .NE. 0 ) THEN 
                      CALL CLRCH ( STR ) 
                      CALL INCH  ( J1, STR )
                      CALL ERR_LOG ( 2688, 'VTD_READ_NZO', 'Error '// &
     &                    'in decoding source position field '// &
     &                     BUF(J1)(IND(1,1+J2):IND(2,1+J2))//' at line '// &
     &                     STR(1:I_LEN(STR))//' of the input file '//FILNZO )
                      DEALLOCATE ( BUF )
                      RETURN 
                 END IF
                 VEL_ARR(L_ARR,J3) = VEL_ARR(L_ARR,J3)*1.D3
 430          CONTINUE 
         END IF
 410  CONTINUE 
      DEALLOCATE ( BUF )
      IF ( FMT_LABEL == 'VTD_1' .OR. FMT_LABEL == 'CCSDS' ) THEN
           IF ( CENTER_NAME == '???' ) THEN
                CONTINUE 
           END IF
           IF ( REF_NAME    == '???') THEN
                CONTINUE 
           END IF
           IF ( TS_NAME     == '???') THEN
              ELSE IF ( TS_NAME == 'TAI' ) THEN
                 TIM_CODE = VTD__TAI
              ELSE IF ( TS_NAME == 'TDT' ) THEN
                 TIM_CODE = VTD__TDT
              ELSE IF ( TS_NAME == 'TDB' ) THEN
                 TIM_CODE = VTD__TDB
              ELSE IF ( TS_NAME == 'UTC' ) THEN
                 TIM_CODE = VTD__UTC
              ELSE 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_READ_NZO  !#!#
