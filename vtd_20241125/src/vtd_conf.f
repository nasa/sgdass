      SUBROUTINE VTD_CONF ( CONFIG_FILE, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_CONF  parses configuration file of the package for    *
! *   computation of VLBI Time Delay (VTD) and stores results of parsing *
! *   in the internal fields of the object VTD.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * CONFIG_FILE ( CHARACTER ) -- Full path name of the configuration     *
! *                              file.                                   *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 27-JAN-2004    VTD_CONF   v1.20 (c)  L. Petrov 16-APR-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  CONFIG_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  M_BUF, MIND
      PARAMETER  ( M_BUF = 256,  MIND = 32 )
      LOGICAL*4  LEX
      CHARACTER  BUF(M_BUF)*128, DELIM*3, STR*80, STR1*80
      PARAMETER  ( DELIM = CHAR(0)//CHAR(9)//CHAR(32) )
      INTEGER*4  LIND, IND(2,MIND), I_CNF, N_BUF, I_PSV, J1, J2, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
!
      CALL NERS_VERSION ( 'NERS__LABEL', STR )
      IF ( STR .NE. NERS__LABEL ) THEN
           CALL ERR_LOG ( 2311, IUER, 'VTD_CONF', 'Trap of internal '// &
     &         'control: vtd was compiled against '//NERS__LABEL// &
     &         ', but linked against '//STR(1:I_LEN(STR))// &
     &         '. Please re-compule VTD' )
           RETURN 
      END IF
!
! --- Check whether configuration file exists
!
      INQUIRE ( FILE=CONFIG_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2312, IUER, 'VTD_CONF', 'Configuration file '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' was not found' )
           RETURN
      END IF
!
! --- Read configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONFIG_FILE, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2313, IUER, 'VTD_CONF', 'Error reading '// &
     &         'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE)) )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(VTD_CNF__LABEL)) == VTD_CNF__LABEL ) THEN
           CONTINUE 
         ELSE IF ( BUF(1)(1:LEN(VTD_CNF__LABEL)) == &
     &       '# VTD Control file.   Format version of 2010.05.18' ) THEN
           CALL ERR_LOG ( 2314, IUER, 'VTD_CONF', 'Your VTD control file '// &
     &         'is obsolete. Please run program vtd_control_update '// &
     &         CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' for update' )
           RETURN
         ELSE 
           CALL ERR_LOG ( 2315, IUER, 'VTD_CONF', 'Wrong file format: '// &
     &         'the first line of the file used as VTD configuration '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' is '// &
     &          BUF(1)(1:I_LEN(BUF(1)))//' while the label line '// &
     &          VTD_CNF__LABEL//' was expected. Please check the file! '// &
     &         'If your control file is valid, but just has an old '// &
     &         'format version, you can upgrade it with command '// &
     &         '   vtd_control_update '//CONFIG_FILE )
           RETURN
      END IF
!
! --- Scan the buffer with copy of configuration file
!
      I_CNF = 0
      DO 410 J1=2,N_BUF
!
! ------ Skip comments
!
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '#'  ) GOTO 410
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
         CALL ERR_PASS ( IUER, IER )
         CALL RESOLVE_ENV ( BUF(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2316, IUER, 'VTD_CONF', 'Failure in an attempt '// &
     &            'to resolve environment varaibles when processing '// &
     &            'the '//STR(1:I_LEN(STR))//'-th line of the control file '// &
     &             CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' -- "'// &
     &             BUF(J1)(1:I_LEN(BUF(J1)))//'"' )
              RETURN
         END IF
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! ------ It should be exactly two words. Let's check it
!
         IF ( LIND .LT. 2 ) THEN
              CALL ERR_LOG ( 2317, IUER, 'VTD_CONF', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ' : too few words' )
              RETURN
         END IF
!
! ------ Transform the keyword to the letters of upper case
!
         CALL TRAN ( 11, BUF(J1)(IND(1,1):IND(2,1)), &
     &                   BUF(J1)(IND(1,1):IND(2,1))  )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'STATION_DESCRIPTION:' ) THEN
              VTD%CONF%FINAM_STADESC = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 2318, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' was not found' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'STATION_COORDINATES:' ) THEN
              VTD%CONF%FINAM_STACOO  = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 2319, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' was not found' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'STATION_VELOCITIES:' ) THEN
              VTD%CONF%FINAM_STAVEL  = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%FINAM_STAVEL == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_STAVEL )
                 ELSE 
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2320, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'STATION_ECCENTRICITIES:' ) THEN
              VTD%CONF%FINAM_STAECC  = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 2321, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' was not found' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOURCE_COORDINATES:' ) THEN
              VTD%CONF%FINAM_SOUCOO(1)  = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 2322, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' was not found' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOURCE_COORDINATES_2ND:' ) THEN
              VTD%CONF%FINAM_SOUCOO(2)  = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%FINAM_SOUCOO(2)  == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_SOUCOO(2) )
                 ELSE
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2323, IUER, 'VTD_CONF', 'Error in '// &
     &                     'parsing the '//STR(1:I_LEN(STR))//'-th line in '// &
     &                     'the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                     ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                     ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOURCE_COORDINATES_3RD:' ) THEN
              VTD%CONF%FINAM_SOUCOO(3)  = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%FINAM_SOUCOO(3)  == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_SOUCOO(3) )
                 ELSE
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2324, IUER, 'VTD_CONF', 'Error in '// &
     &                     'parsing the '//STR(1:I_LEN(STR))//'-th line in '// &
     &                     'the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                     ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                     ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOURCE_COORDINATES_4TH:' ) THEN
              VTD%CONF%FINAM_SOUCOO(4)  = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%FINAM_SOUCOO(4) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_SOUCOO(4) )
                 ELSE
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2325, IUER, 'VTD_CONF', 'Error in '// &
     &                     'parsing the '//STR(1:I_LEN(STR))//'-th line in '// &
     &                     'the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                     ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                     ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOURCE_PRLX_PRP_MOTION:' ) THEN
              VTD%CONF%FINAM_SOUPRL_PRP = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%FINAM_SOUPRL_PRP == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_SOUPRL_PRP )
                 ELSE
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2326, IUER, 'VTD_CONF', 'Error in '// &
     &                     'parsing the '//STR(1:I_LEN(STR))//'-th line in '// &
     &                     'the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                     ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                     ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DIR_NZO:' ) THEN
              VTD%CONF%DIR_NZO = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%DIR_NZO == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%DIR_NZO )
                 ELSE
                   IF ( .NOT. IS_DIR_EXIST ( VTD%CONF%DIR_NZO, STR ) ) THEN
                        CALL ERR_LOG ( 2327, IUER, 'VTD_CONF', 'Error in '// &
     &                     'parsing the '//STR(1:I_LEN(STR))//'-th line in '// &
     &                     'the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                     ' : directory '//BUF(J1)(IND(1,2):IND(2,2))//' '//STR )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DE_EPHEMERIDES:' ) THEN
              VTD%CONF%FINAM_DE_EPH = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 2328, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' was not found' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'LEAP_SECOND:' ) THEN
              VTD%CONF%FINAM_LEAPSEC = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%FINAM_LEAPSEC == VTD__NERS_STR ) THEN
                   CONTINUE 
                 ELSE
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2329, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                      'configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EOP_SERIES:' ) THEN
              VTD%CONF%FINAM_EOP = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%FINAM_EOP == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_EOP )
                 ELSE IF ( VTD%CONF%FINAM_EOP == VTD__NERS_STR ) THEN
                   CONTINUE 
                 ELSE
                   INQUIRE ( FILE=VTD%CONF%FINAM_EOP, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2330, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'AEM_FILE:' ) THEN
              CALL CLRCH ( VTD%CONF%FINAM_AEM )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   CONTINUE 
                 ELSE
                   VTD%CONF%FINAM_AEM = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%FINAM_AEM, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2331, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'ERM_FILE:' ) THEN
              VTD%CONF%FINAM_ERM = BUF(J1)(IND(1,2):IND(2,2))
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_ERM )
                 ELSE
                   INQUIRE ( FILE=VTD%CONF%FINAM_ERM, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2332, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'UZT_MODEL:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%UZT_MODEL = UZT__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DICKMAN1993' ) THEN
                   VTD%CONF%UZT_MODEL = UZT__DICKMAN1993
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DICKMAN1993_PRINCIPAL' ) THEN
                   VTD%CONF%UZT_MODEL = UZT__DICKMAN_PRINCIPLE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DICKMAN1993_SHORT' ) THEN
                   VTD%CONF%UZT_MODEL = UZT__DICKMAN_SHORT
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'RE2014' ) THEN
                   VTD%CONF%UZT_MODEL = UZT__RE2014
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'RE2014_SHORT' ) THEN
                   VTD%CONF%UZT_MODEL = UZT__RE2014_SHORT
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NERS' ) THEN
                   VTD%CONF%UZT_MODEL = UZT__NONE
                 ELSE
                   CALL ERR_LOG ( 2333, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown UT1 zonal tides variations model: '// &
     &                  BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'UZT_USE:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%UZT_USE = UZT__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'ADD' ) THEN
                   VTD%CONF%UZT_USE = UZT__ADD
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'INTERPOLATE' ) THEN
                   VTD%CONF%UZT_USE = UZT__INTERPOLATE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SUBTRACT' ) THEN
                   VTD%CONF%UZT_USE = UZT__SUBTRACT
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NERS' ) THEN
                   VTD%CONF%UZT_USE = UZT__NONE
                 ELSE
                   CALL ERR_LOG ( 2334, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown UT1 zonal tides variations model: '// &
     &                  BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EROT_COMPAT:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%EROT_COMPAT = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CALC10' ) THEN
                   VTD%CONF%EROT_COMPAT = VTD__CALC10
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NO_SARG' ) THEN
                   VTD%CONF%EROT_COMPAT = VTD__NO_SARG
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NERS' ) THEN
                   VTD%CONF%EROT_COMPAT = VTD__NONE
                 ELSE
                   CALL ERR_LOG ( 2335, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown Earth rotation model compatibility '// &
     &                  'code. Supported values: NONE and CALC10' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EOP_TIME_SCALE:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TAI' ) THEN
                   VTD%CONF%EOP_TIME_SCALE = VTD__TAI
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE'  .AND. &
     &                     ILEN(VTD%CONF%FINAM_AEM) > 0                  ) THEN
                   VTD%CONF%EOP_TIME_SCALE = VTD__UNDF
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TDT' ) THEN
                   VTD%CONF%EOP_TIME_SCALE = VTD__TDT
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TDB' ) THEN
                   VTD%CONF%EOP_TIME_SCALE = VTD__TDB
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'UTC' ) THEN
                   VTD%CONF%EOP_TIME_SCALE = VTD__UTC
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'UT1' ) THEN
                   VTD%CONF%EOP_TIME_SCALE = VTD__UT1
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NERS' ) THEN
                   VTD%CONF%EOP_TIME_SCALE = VTD__TAI
                 ELSE
                   CALL ERR_LOG ( 2336, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown EOP time scale: '// &
     &                  BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POSVAR_FIL:' .OR. &
     &                BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POSVAR_MOD:' .OR. &
     &                BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POSVAR_INT:' .OR. &
     &                BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POSVAR_USE:'      ) THEN
!
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
!
! ---------------- Special case: not position variations is requiested
!
                   I_PSV = -1
                   I_CNF = I_CNF + 1
                 ELSE
                   IF ( LIND .LT. 3 ) THEN
                        CALL ERR_LOG ( 2337, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line in '// &
     &                      'the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : too few words. 3 words are expected' )
                        RETURN
                   END IF
!
                   CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), I_PSV )
                   IF ( I_PSV .LE. 0  .OR. I_PSV .GT. VTD__M_PSF ) THEN
                        CALL ERR_LOG ( 2338, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line in '// &
     &                      'the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the second word should have an index of the '//&
     &                      'position variation file' )
                        RETURN
                   END IF
!
                   IF ( I_PSV .EQ. 1 ) I_CNF = I_CNF + 1
                   IF ( I_PSV .EQ. -1 ) THEN
                       CALL ERR_LOG ( 2339, IUER, 'VTD_CONF', 'Error in '// &
     &                     'parsing the '//STR(1:I_LEN(STR))//'-th line in '// &
     &                     'the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      BUF(J1)(IND(1,1):IND(2,1))//' specified twice, '// &
     &                     'the first one with value NONE.' )
                       RETURN
                   END IF
!
                   IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POSVAR_FIL:' ) THEN
                        VTD%CONF%POSVAR_FIL(I_PSV) = BUF(J1)(IND(1,3):IND(2,3))
                      ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POSVAR_MOD:' ) THEN
                        IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'HARMONIC_MODEL' ) THEN
                             VTD%CONF%POSVAR_MOD(I_PSV) = PSV__HMD
                          ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'TIME_SERIES' ) THEN
                             VTD%CONF%POSVAR_MOD(I_PSV) = PSV__TSR
                          ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'B_SPLINE' ) THEN
                             VTD%CONF%POSVAR_MOD(I_PSV) = PSV__BSP
                          ELSE
                             CALL ERR_LOG ( 2340, IUER, 'VTD_CONF', 'Error in '// &
     &                           'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                           'in the configuration file '// &
     &                            CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                           ' : the third word should be HARMONIC_MODEL or '// &
     &                           'TIME_SERIES or B_SPLINE' )
                             RETURN
                        END IF
                      ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POSVAR_INT:' ) THEN
                        CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                                  BUF(J1)(IND(1,2):IND(2,2))  )
                        IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'CLOSE_POINT' ) THEN
                             VTD%CONF%POSVAR_INT(I_PSV) = PSV__CLS
                           ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'LINEAR' ) THEN
                             VTD%CONF%POSVAR_INT(I_PSV) = PSV__LIN
                           ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'SPLINE' ) THEN
                             VTD%CONF%POSVAR_INT(I_PSV) = PSV__SPL
                           ELSE
                             CALL ERR_LOG ( 2341, IUER, 'VTD_CONF', 'Error in '// &
     &                           'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                           'in the configuration file '// &
     &                            CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                           ' : the third word should be CLOSE_POINT or '// &
     &                           'LINEAR or SPLINE' )
                             RETURN
                        END IF
                      ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POSVAR_USE:' ) THEN
                        CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                                  BUF(J1)(IND(1,2):IND(2,2))  )
                        IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'REQUIRED' ) THEN
                             VTD%CONF%POSVAR_USE(I_PSV) = PSV__REQ
                           ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'IF_AVAILABLE' ) THEN
                             VTD%CONF%POSVAR_USE(I_PSV) = PSV__AVL
                           ELSE
                             CALL ERR_LOG ( 2342, IUER, 'VTD_CONF', 'Error in '// &
     &                           'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                           'in the configuration file '// &
     &                            CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                           ' : the third word should be REQUIRED or '// &
     &                           'IF_AVAILABLE' )
                             RETURN
                        END IF
                   END IF
              END IF ! not 'NONE'
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PRECESSION_EXPRESSION:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'LIESKE_1977' ) THEN
                   VTD%CONF%PREC_EXP = PREC__LIESKE1977
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SIMON_1994' ) THEN
                   VTD%CONF%PREC_EXP = PREC__SIMON1994
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IERS_1996' ) THEN
                   VTD%CONF%PREC_EXP = PREC__IERS1996
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CAPITAINE_2003' ) THEN
                   VTD%CONF%PREC_EXP = PREC__CAPITAINE2003
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CAPITAINE_2005' ) THEN
                   VTD%CONF%PREC_EXP = PREC__CAPITAINE2005
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE'           ) THEN
                   VTD%CONF%PREC_EXP = VTD__UNDF
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. VTD__NERS_STR ) THEN
                   VTD%CONF%PREC_EXP = VTD__NERS
                 ELSE
                   CALL ERR_LOG ( 2343, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown precession expression code' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'NUTATION_EXPANSION:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WAHR1980' ) THEN
                   VTD%CONF%NUT_EXP = NUT__WAHR1980
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IERS1996' ) THEN
                   VTD%CONF%NUT_EXP = NUT__IERS1996
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'REN2000'  ) THEN
                   VTD%CONF%NUT_EXP = NUT__REN2000
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MHB2000'  ) THEN
                   VTD%CONF%NUT_EXP = NUT__MHB2000
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MHB2000_TRANSF' ) THEN
                   VTD%CONF%NUT_EXP = NUT__MHB2000_TRANSF
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MHB2000_ADDON'  ) THEN
                   VTD%CONF%NUT_EXP = NUT__MHB2000_ADDON
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NUT__PETA'      ) THEN
                   VTD%CONF%NUT_EXP = NUT__PETA
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NUT__PETB'      ) THEN
                   VTD%CONF%NUT_EXP = NUT__PETB
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NUT__PETC'      ) THEN
                   VTD%CONF%NUT_EXP = NUT__PETC
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE'           ) THEN
                   VTD%CONF%NUT_EXP = VTD__UNDF
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. VTD__NERS_STR ) THEN
                   VTD%CONF%NUT_EXP = VTD__NERS
                 ELSE
                   CALL ERR_LOG ( 2344, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : unknown nutation expansion code' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'GEODESIC_NUTATION:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'YES' ) THEN
                   VTD%CONF%NUT_GDS = NUT__GDS_YES
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%NUT_GDS = NUT__GDS_NO
                 ELSE
                   CALL ERR_LOG ( 2345, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : only value YES or NONE is allowed for the '// &
     &                  'GEODETIC_NUTATION keyword' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'HARMONIC_EOP_FILE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_HEO )
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. VTD__NERS_STR ) THEN
                   VTD%CONF%FINAM_HEO = VTD__NERS_STR
                 ELSE
                   VTD%CONF%FINAM_HEO = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2346, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                  END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOLID_EARTH_TIDES_2ND_DEGREE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MDG97EL' ) THEN
                   VTD%CONF%STD_2ND_MODEL = SOTID__MDG97EL
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MDG97AN' ) THEN
                   VTD%CONF%STD_2ND_MODEL = SOTID__MDG97AN
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DDW99EH' ) THEN
                   VTD%CONF%STD_2ND_MODEL = SOTID__DDW99EH
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DDW99IN' ) THEN
                   VTD%CONF%STD_2ND_MODEL = SOTID__DDW99IN
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'LOVE' ) THEN
                   VTD%CONF%STD_2ND_MODEL = SOTID__LOVE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MATHEWS_2000' ) THEN
                   VTD%CONF%STD_2ND_MODEL = SOTID__MAT00
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MATHEWS_2001' ) THEN
                   VTD%CONF%STD_2ND_MODEL = SOTID__MAT01
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%STD_2ND_MODEL = SOTID__2D_NONE
                 ELSE
                    CALL ERR_LOG ( 2347, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown solid Earth tides model code' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOLID_EARTH_TIDES_3RD_DEGREE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MDG97' ) THEN
                   VTD%CONF%STD_3RD_MODEL = SOTID__3D_MDG97
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%STD_3RD_MODEL = SOTID__3D_NONE
                 ELSE
                    CALL ERR_LOG ( 2348, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown solid Earth tides code' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOLID_EARTH_TIDES_ZERO_FREQ:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'ZERO' ) THEN
                   VTD%CONF%STD_ZF_MODEL = SOTID__ZF_ZERO
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MDG97AN' ) THEN
                   VTD%CONF%STD_ZF_MODEL = SOTID__ZF_MDG97AN
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'FLUID' ) THEN
                   VTD%CONF%STD_ZF_MODEL = SOTID__ZF_FLUID
                 ELSE
                    CALL ERR_LOG ( 2349, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown solid Earth tides at zero frequency' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'POLE_TIDE_MODEL:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MDG97EL' ) THEN
                   VTD%CONF%PTD_MODEL = SOTID__MDG97EL
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MDG97AN' ) THEN
                   VTD%CONF%PTD_MODEL = SOTID__MDG97AN
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DDW99EH' ) THEN
                   VTD%CONF%PTD_MODEL = SOTID__DDW99EH
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DDW99IN' ) THEN
                   VTD%CONF%PTD_MODEL = SOTID__DDW99IN
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'LOVE' ) THEN
                   VTD%CONF%PTD_MODEL = SOTID__LOVE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MATHEWS_2000' ) THEN
                   VTD%CONF%PTD_MODEL = SOTID__MAT00
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MATHEWS_2001' ) THEN
                   VTD%CONF%PTD_MODEL = SOTID__MAT01
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%PTD_MODEL = SOTID__2D_NONE
                 ELSE
                    CALL ERR_LOG ( 2350, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown pole tide model code' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'MEAN_POLE_MODEL:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   VTD%CONF%MEAN_POLE_MODEL = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'IERS2010' ) THEN
                   VTD%CONF%MEAN_POLE_MODEL = VTD__MPL_IERS2010
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'IERS2022' ) THEN
                   VTD%CONF%MEAN_POLE_MODEL = VTD__MPL_IERS2022
                 ELSE
                   CALL ERR_LOG ( 2351, IUER, 'VTD_CONF', 'Unsupported mean pole '// &
     &                 'model '//BUF(J1)(IND(1,1):IND(2,1))//' -- one of '// &
     &                 'NONE, IERS2010, IERS2022' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'ANTENNA_DEFORMATIONS_FILE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_AGD )
                 ELSE 
                   VTD%CONF%FINAM_AGD = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%FINAM_AGD, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2352, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'ANTENNA_THERMAL_EXPANSION:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_ANTI )
                 ELSE 
                   IF ( LIND < 9 ) THEN
                        CALL ERR_LOG ( 2353, IUER, 'VTD_CONF', 'Too few '// &
     &                      'words for the ANTENNA_THERMAL_EXPANSION '// &
     &                      'keyword. This keyword requires 8 qualifiers' )
                        RETURN 
                   END IF
                   IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'MODEL' ) THEN
                        CONTINUE 
                      ELSE
                        CALL ERR_LOG ( 2354, IUER, 'VTD_CONF', 'Wrong 2nd '// &
     &                      'word after the ANTENNA_THERMAL_EXPANSION '// &
     &                      'keyword: '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ', while MODEL was expected' )
                        RETURN 
                   END IF
!
                   VTD%CONF%FINAM_ANTI = BUF(J1)(IND(1,3):IND(2,3))
                   INQUIRE ( FILE=VTD%CONF%FINAM_ANTI, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2355, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,3):IND(2,3))// &
     &                      ' was not found' )
                        RETURN
                   END IF
                   IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'INSITU' ) THEN
                        CONTINUE 
                      ELSE 
                        CALL ERR_LOG ( 2356, IUER, 'VTD_CONF', 'Wrong 4th '// &
     &                      'word after the ANTENNA_THERMAL_EXPANSION '// &
     &                      'keyword: '//BUF(J1)(IND(1,4):IND(2,4))// &
     &                      ', while INSITU was expected' )
                        RETURN 
                   END IF
!
                   VTD%CONF%FINAM_AHM = BUF(J1)(IND(1,5):IND(2,5))
                   IF ( VTD%CONF%FINAM_AHM == 'NO' .OR. &
     &                  VTD%CONF%FINAM_AHM == 'NONE'    ) THEN
!
                        VTD%STATUS_AHM = VTD__NONE
                      ELSE 
                        VTD%STATUS_AHM = VTD__USE
                        INQUIRE ( FILE=VTD%CONF%FINAM_AHM, EXIST=LEX )
                        IF ( .NOT. LEX ) THEN
                             CALL ERR_LOG ( 2357, IUER, 'VTD_CONF', 'Error '// &
     &                           'in parsing the '//STR(1:I_LEN(STR))// &
     &                           '-th line in the configuration file '// &
     &                            CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                           ' : file '//BUF(J1)(IND(1,5):IND(2,5))// &
     &                           ' was not found' )
                             RETURN
                        END IF
                   END IF
                   IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'TEMPERATURE' ) THEN
                        CONTINUE 
                      ELSE 
                        CALL ERR_LOG ( 2358, IUER, 'VTD_CONF', 'Wrong 6th '// &
     &                      'word after the ANTENNA_THERMAL_EXPANSION '// &
     &                      'keyword: '//BUF(J1)(IND(1,6):IND(2,6))// &
     &                      ', while TEMPERATURE was expected' )
                        RETURN 
                   END IF 
!
                   IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'METEO' ) THEN
                        VTD%CONF%ATHD_DATA_FROM = VTD__METEO
                      ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'SPD' ) THEN
                        VTD%CONF%ATHD_DATA_FROM = VTD__SPD
                      ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'SPD' ) THEN
                        CALL ERR_LOG ( 2359, IUER, 'VTD_CONF', 'Wrong 7th '// &
     &                      'word after the ANTENNA_THERMAL_EXPANSION '// &
     &                      'keyword: '//BUF(J1)(IND(1,7):IND(2,7))// &
     &                      ', while METEO or SPD were expected' )
                        RETURN 
                   END IF
!
                   IF ( BUF(J1)(IND(1,8):IND(2,8)) == 'LAG' ) THEN
                        CONTINUE 
                      ELSE 
                        CALL ERR_LOG ( 2360, IUER, 'VTD_CONF', 'Wrong 6th '// &
     &                      'word after the ANTENNA_THERMAL_EXPANSION '// &
     &                      'keyword: '//BUF(J1)(IND(1,8):IND(2,8))// &
     &                      ', while LAG was expected' )
                        RETURN 
                   END IF 
!
                   READ ( UNIT=BUF(J1)(IND(1,9):IND(2,9)), FMT='(F15.5)', IOSTAT=IER ) &
     &                    VTD%CONF%ATHD_LAG
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2361, IUER, 'VTD_CONF', 'Wrong 9th '// &
     &                      'word after the ANTENNA_THERMAL_EXPANSION '// &
     &                      'keyword: a real number was expected. Wrong format?')
                        RETURN 
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'HYDROSTATIC_MAPPING_FUNCTION:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%HMF_MODEL = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NMFH' ) THEN
                   VTD%CONF%HMF_MODEL = VTD__NMFH
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MMF' ) THEN
                   VTD%CONF%HMF_MODEL = VTD__MMF
                   IF ( LIND < 3 ) THEN
                        CALL ERR_LOG ( 2362, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the file name with the MMF should be '// &
     &                      'supplied in the third word' )
                        RETURN
                   END IF
                   VTD%CONF%FINAM_MMF = BUF(J1)(IND(1,3):IND(2,3)) 
                   INQUIRE ( FILE=VTD%CONF%FINAM_MMF, EXIST=LEX ) 
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2363, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the file name with the MMF '// &
     &                      VTD%CONF%FINAM_MMF(1:I_LEN(VTD%CONF%FINAM_MMF))// &
     &                      ' was not found' )
                        RETURN 
                   END IF 
                 ELSE
                    CALL ERR_LOG ( 2364, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown hydrostatic mapping function code' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'WET_MAPPING_FUNCTION:'          ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%WMF_MODEL = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NMFW' ) THEN
                   VTD%CONF%WMF_MODEL = VTD__NMFW
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NMFH' ) THEN
                   VTD%CONF%WMF_MODEL = VTD__NMFH
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SPD'  ) THEN
                   VTD%CONF%WMF_MODEL = VTD__SPD
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IONO_350' ) THEN
                   VTD%CONF%WMF_MODEL = VTD__IONO_350
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IONO_400' ) THEN
                   VTD%CONF%WMF_MODEL = VTD__IONO_400
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IONO_450' ) THEN
                   VTD%CONF%WMF_MODEL = VTD__IONO_450
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IONO_500' ) THEN
                   VTD%CONF%WMF_MODEL = VTD__IONO_500
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MMF' ) THEN
                   VTD%CONF%WMF_MODEL = VTD__MMF
                   IF ( LIND < 3 ) THEN
                        CALL ERR_LOG ( 2365, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the file name with the MMF should be '// &
     &                      'supplied in the third word' )
                        RETURN
                   END IF
                   VTD%CONF%FINAM_MMF = BUF(J1)(IND(1,3):IND(2,3)) 
                   INQUIRE ( FILE=VTD%CONF%FINAM_MMF, EXIST=LEX ) 
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2366, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the file name with the MMF '// &
     &                      VTD%CONF%FINAM_MMF(1:I_LEN(VTD%CONF%FINAM_MMF))// &
     &                      ' was not found' )
                        RETURN 
                   END IF 
                 ELSE
                    CALL ERR_LOG ( 2367, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown wet mapping function code' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'ATMOSPHERE_PATH_DELAY_PARTIAL:' ) THEN
              I_CNF = I_CNF + 1
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NMFW' ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__NMFW
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NMFH' ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__NMFH
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TOTAL_SCALE'  ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__TOTS
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WATER_SCALE'  ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__WATS
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'GAUSSIAN_LAYER'  ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__GL
                   IF ( LIND < 4 ) THEN
                        CALL ERR_LOG ( 2368, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : three qualifiers after the keyword '// &
     &                      'ATMOSPHERE_PATH_DELAY_PARTIAL were expected' )
                        RETURN
                   END IF
!
                   READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F12.6)', &
     &                    IOSTAT=IER ) VTD%CONF%ATM_PARTIAL_PAR(1)
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2369, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : failure in parsing the 3rd qualifier '// &
     &                      BUF(J1)(IND(1,3):IND(2,3))//' -- the layer height '// &
     &                      'in meters. A real number was expected' )
                        RETURN
                   END IF
!
                   READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F12.6)', &
     &                    IOSTAT=IER ) VTD%CONF%ATM_PARTIAL_PAR(2)
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2370, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : failure in parsing the 4th qualifier '// &
     &                      BUF(J1)(IND(1,3):IND(2,3))//' -- the layer FWHM '// &
     &                      'thickness in meters. A real number was expected' )
                        RETURN
                   END IF
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IONO_350' ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__IONO_350
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IONO_400' ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__IONO_400
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IONO_450' ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__IONO_450
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IONO_500' ) THEN
                   VTD%CONF%ATM_PARTIAL_TYPE = VTD__IONO_500
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'METEO_DEF:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IMA' ) THEN
                   VTD%CONF%METEO_DEF = VTD__IMA
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CALC' ) THEN
                   VTD%CONF%METEO_DEF = VTD__CALC
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%METEO_DEF = VTD__NONE
                 ELSE
                    CALL ERR_LOG ( 2371, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown default meteorologial code' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'HYDROSTATIC_ZENITH_DELAY:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SAASTAMOINEN' ) THEN
                   VTD%CONF%HZD_MODEL = VTD__SAA
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MMF' ) THEN
                   VTD%CONF%HZD_MODEL = VTD__MMF
                   IF ( LIND < 3 ) THEN
                        CALL ERR_LOG ( 2372, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the file name with the MMF should be '// &
     &                      'supplied in the third word' )
                        RETURN
                   END IF
                   VTD%CONF%FINAM_MMF = BUF(J1)(IND(1,3):IND(2,3)) 
                   INQUIRE ( FILE=VTD%CONF%FINAM_MMF, EXIST=LEX ) 
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2373, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the file name with the MMF '// &
     &                      VTD%CONF%FINAM_MMF(1:I_LEN(VTD%CONF%FINAM_MMF))// &
     &                      ' was not found' )
                        RETURN 
                   END IF 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%HZD_MODEL = VTD__NONE
                 ELSE
                    CALL ERR_LOG ( 2374, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown hydrostatic zenith delay code' )
                    RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'WET_ZENITH_DELAY:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%WZD_MODEL = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MMF' ) THEN
                   VTD%CONF%WZD_MODEL = VTD__MMF
                   IF ( LIND < 3 ) THEN
                        CALL ERR_LOG ( 2375, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the file name with the MMF should be '// &
     &                      'supplied in the third word' )
                        RETURN
                   END IF
                   VTD%CONF%FINAM_MMF = BUF(J1)(IND(1,3):IND(2,3)) 
                   INQUIRE ( FILE=VTD%CONF%FINAM_MMF, EXIST=LEX ) 
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2376, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the file name with the MMF '// &
     &                      VTD%CONF%FINAM_MMF(1:I_LEN(VTD%CONF%FINAM_MMF))// &
     &                      ' was not found' )
                        RETURN 
                   END IF 
                 ELSE
                   CALL ERR_LOG ( 2377, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown wet zenith delay code' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'ATMOSPHERE_TILT_PARTIALS:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%ATD_PARTIALS = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NORTH_EAST' ) THEN
                   VTD%CONF%ATD_PARTIALS = VTD__NE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MACMILLAN_1995' ) THEN
                   VTD%CONF%ATD_PARTIALS = VTD__MM95
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TILT_NMFH' ) THEN
                   VTD%CONF%ATD_PARTIALS = VTD__TNMFH
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TILT_NMFW' ) THEN
                   VTD%CONF%ATD_PARTIALS = VTD__TNMFW
                 ELSE
                   CALL ERR_LOG ( 2378, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown atmosphere tilt partial derivative '// &
     &                  BUF(J1)(IND(1,1):IND(2,1)) )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'GRS_METRIC:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IAU2000' ) THEN
                   VTD%CONF%GRS_METRIC = VTD__METRIC_IAU2000
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'ITRF2000' ) THEN
                   VTD%CONF%GRS_METRIC = VTD__METRIC_ITRF2000
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'ITRF1996' ) THEN
                   VTD%CONF%GRS_METRIC = VTD__METRIC_ITRF1992
                 ELSE
                   CALL ERR_LOG ( 2379, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown metric tenzor code' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'AXIS_OFFSET_MODEL:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'YES' ) THEN
                   VTD%CONF%AXOF_MODEL = VTD__YES
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%AXOF_MODEL = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CALC9' ) THEN
                   VTD%CONF%AXOF_MODEL = VTD__CALC
                 ELSE
                   CALL ERR_LOG ( 2380, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown axis offset model code' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'TROP_AXOF_COUPLING:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'YES' ) THEN
                   VTD%CONF%TROP_AXOF_COUPL = VTD__YES
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%TROP_AXOF_COUPL = VTD__NONE
                 ELSE
                   CALL ERR_LOG ( 2381, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown troposphere axis offset coupling' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'TROP_GEOMETRIC_COUPLING:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'YES' ) THEN
                   VTD%CONF%TROP_GEOM_COUPL = VTD__YES    
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%TROP_GEOM_COUPL = VTD__NONE   
                 ELSE
                   CALL ERR_LOG ( 2382, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown troposphere gemetric delay coupling '// &
     &                  'option' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PARALLACTIC_ANGLE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'YES' ) THEN
                   VTD%CONF%PARALLACTIC_ANGLE = VTD__YES    
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%TROP_GEOM_COUPL = VTD__NONE   
                 ELSE
                   CALL ERR_LOG ( 2383, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown parallactic angle option' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOURCE_STRUCTURE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE'      ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_STRUC )
                 ELSE
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2384, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : file '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                      ' was not found' )
                        RETURN
                   END IF
                   VTD%CONF%FINAM_STRUC = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SLANT_PATH_DELAY_MODEL:' .OR. &
     &                BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SLANTED_PATH_DELAY_MODEL:'     ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%SPD_MODEL )
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == TRP__SPD_3D  ) THEN
                   VTD%CONF%SPD_MODEL = TRP__SPD_3D  
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == TRP__EXT_3D ) THEN
                   VTD%CONF%SPD_MODEL = TRP__EXT_3D  
                 ELSE 
                   CALL ERR_LOG ( 2385, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : SLANT_PATH_DELAY_MODEL value '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))//' is not supported: '// &
     &                'one of SPD_3D, EXT_3D, or NONE were expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SLANT_PATH_DELAY_BIAS_FILE:' .OR. &
     &                BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SLANTED_PATH_DELAY_BIAS_FILE:'    ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%SPD_BIAS_FILE )
                 ELSE 
                   INQUIRE ( FILE=BUF(J1)(IND(1,2):IND(2,2)), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2386, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                      CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : SLANT_PATH_BIAS_FILE file '// &
     &                      BUF(J1)(IND(1,2):IND(2,2))//' was not found' )
                        RETURN
                   END IF
                   VTD%CONF%SPD_BIAS_FILE = BUF(J1)(IND(1,2):IND(2,2)) 
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EXTERNAL_DELAY_DIR:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%DIR_EPD(1) )
                 ELSE 
                   VTD%CONF%DIR_EPD(1) = BUF(J1)(IND(1,2):IND(2,2))
                   IF ( .NOT. IS_DIR_EXIST ( VTD%CONF%DIR_EPD(1), STR1 ) ) THEN
                        CALL ERR_LOG ( 2387, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : cannot open directory '// &
     &                      VTD%CONF%DIR_EPD(1)(1:I_LEN(VTD%CONF%DIR_EPD(1)))// &
     &                      ' : '//STR1 )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EXTERNAL_DELAY_DIR_2ND:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%DIR_EPD(2) )
                 ELSE 
                   VTD%CONF%DIR_EPD(2) = BUF(J1)(IND(1,2):IND(2,2))
                   IF ( .NOT. IS_DIR_EXIST ( VTD%CONF%DIR_EPD(2), STR1 ) ) THEN
                       CALL ERR_LOG ( 2388, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : cannot open directory '// &
     &                      VTD%CONF%DIR_EPD(2)(1:I_LEN(VTD%CONF%DIR_EPD(2)))// &
     &                      ' : '//STR1 )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EXTERNAL_DELAY_DIR_3RD:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%DIR_EPD(3) )
                 ELSE 
                   VTD%CONF%DIR_EPD(3) = BUF(J1)(IND(1,2):IND(2,2))
                   IF ( .NOT. IS_DIR_EXIST ( VTD%CONF%DIR_EPD(3), STR1 ) ) THEN
                        CALL ERR_LOG ( 2389, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : cannot open directory '// &
     &                      VTD%CONF%DIR_EPD(3)(1:I_LEN(VTD%CONF%DIR_EPD(3)))// &
     &                      ' : '//STR1 )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'EXTERNAL_DELAY_DIR_4TH:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%DIR_EPD(4) )
                 ELSE 
                   VTD%CONF%DIR_EPD(4) = BUF(J1)(IND(1,2):IND(2,2))
                   IF ( .NOT. IS_DIR_EXIST ( VTD%CONF%DIR_EPD(4), STR1 ) ) THEN
                        CALL ERR_LOG ( 2390, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : cannot open directory '// &
     &                      VTD%CONF%DIR_EPD(4)(1:I_LEN(VTD%CONF%DIR_EPD(4)))// &
     &                      ' : '//STR1 )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_MODEL:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE'      ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_MODEL )
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. VIO__GNSS_TEC ) THEN
                   VTD%CONF%IONO_MODEL = VIO__GNSS_TEC 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. VIO__GNSS_TEC_MOD ) THEN
                   VTD%CONF%IONO_MODEL = VIO__GNSS_TEC_MOD 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. VIO__GNSS_TEC_MHI ) THEN
                   VTD%CONF%IONO_MODEL = VIO__GNSS_TEC_MHI 
                 ELSE
                   CALL ERR_LOG ( 2391, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                 CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : unsupported code of the ionosphere model '// &
     &                 VTD%CONF%IONO_MODEL(1:I_LEN(VTD%CONF%IONO_MODEL))// &
     &                 ' -- only NONE or '//VIO__GNSS_TEC//' or '//VIO__GNSS_TEC_MOD// &
     &                 ' are supported' )
                   RETURN 
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_SCALE:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT=*, IOSTAT=IER  ) VTD%CONF%IONOSPHERE_SCALE
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2392, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'in the configuration file '// &
     &                 CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : IONOSPHER_SCALE should be a real number, but got '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(1) )
                 ELSE 
                   VTD%CONF%IONO_FILE(1) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(1), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2393, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 1st ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(1)(1:I_LEN(VTD%CONF%IONO_FILE(1)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE_2ND:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(2) )
                 ELSE 
                   VTD%CONF%IONO_FILE(2) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(2), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2394, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 2nd ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(2)(1:I_LEN(VTD%CONF%IONO_FILE(2)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE_3RD:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(3) )
                 ELSE 
                   VTD%CONF%IONO_FILE(3) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(3), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2395, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 3rd ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(3)(1:I_LEN(VTD%CONF%IONO_FILE(3)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE_4TH:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(4) )
                 ELSE 
                   VTD%CONF%IONO_FILE(4) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(4), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2396, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 4th ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(4)(1:I_LEN(VTD%CONF%IONO_FILE(4)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE_5TH:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(5) )
                 ELSE 
                   VTD%CONF%IONO_FILE(5) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(4), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2397, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 5th ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(5)(1:I_LEN(VTD%CONF%IONO_FILE(5)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE_6TH:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(6) )
                 ELSE 
                   VTD%CONF%IONO_FILE(6) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(4), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2398, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 6th ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(6)(1:I_LEN(VTD%CONF%IONO_FILE(6)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE_7TH:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(7) )
                 ELSE 
                   VTD%CONF%IONO_FILE(7) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(4), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2399, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 7th ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(7)(1:I_LEN(VTD%CONF%IONO_FILE(7)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE_8TH:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(8) )
                 ELSE 
                   VTD%CONF%IONO_FILE(8) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(4), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2400, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 8th ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(8)(1:I_LEN(VTD%CONF%IONO_FILE(8)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'IONOSPHERE_DATA_FILE_9TH:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%IONO_FILE(9) )
                 ELSE 
                   VTD%CONF%IONO_FILE(9) = BUF(J1)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=VTD%CONF%IONO_FILE(4), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2401, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the 9th ionosphere file '// &
     &                      VTD%CONF%IONO_FILE(9)(1:I_LEN(VTD%CONF%IONO_FILE(9)))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'SOU_DEBIAS_MODEL:' ) THEN
              VTD%CONF%SOU_DEBIAS_MODEL = BUF(J1)(IND(1,2):IND(2,2))
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'GALACTIC_ABERRATION:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE'        ) THEN
                   VTD%CONF%GAL_ABR = VTD__NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'YES' ) THEN
                   VTD%CONF%GAL_ABR = VTD__YES
                 ELSE 
                   CALL ERR_LOG ( 2402, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown value of GALACTIC_ABERRATION keyword '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' while NONE or YES '// &
     &                  'were expected' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'GEOM_EXPR_FAR_ZONE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'KS_1999' ) THEN
                   VTD%CONF%GEOM_EXPR_FAR_ZONE  = VTD__KS1999
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'PK_2001' ) THEN
                   VTD%CONF%GEOM_EXPR_FAR_ZONE  = VTD__PK2001
                 ELSE
                   CALL ERR_LOG ( 2403, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown code '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                  ' of the VLBI theoretic delay expression for the far zone' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'GEOM_EXPR_NEAR_ZONE:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'LIGHT_TIME' ) THEN
                   VTD%CONF%GEOM_EXPR_NEAR_ZONE  = VTD__LT
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'JN_2019' ) THEN
                   VTD%CONF%GEOM_EXPR_NEAR_ZONE  = VTD__JN2019
                 ELSE
                   CALL ERR_LOG ( 2404, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown code '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                  ' of the VLBI theoretic delay '// &
     &                  'expression for the near zone' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DOPPLER_EXPR:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'COARSE_2006' ) THEN
                   VTD%CONF%DOPPLER_EXPR = VTD__CS2006
                 ELSE
                   CALL ERR_LOG ( 2405, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown code of the expression for '// &
     &                  'the Doppler frequency shift' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'DELAY_RATE:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                        BUF(J1)(IND(1,2):IND(2,2))  )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'YES' ) THEN
                   VTD%CONF%FL_RATE = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   VTD%CONF%FL_RATE = .FALSE.
                 ELSE
                   CALL ERR_LOG ( 2406, IUER, 'VTD_CONF', 'Error in '// &
     &                  'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                  'in the configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                  ' : unknown code for DELAY_RATE option ' )
                   RETURN
              END IF
              I_CNF = I_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'PHASE_OFFSET_FILE:' ) THEN
              VTD%CONF%FINAM_PHASE_OFFS = BUF(J1)(IND(1,2):IND(2,2))
              IF ( VTD%CONF%FINAM_PHASE_OFFS == 'NONE' ) THEN
                   CALL CLRCH ( VTD%CONF%FINAM_PHASE_OFFS ) 
                 ELSE
                   INQUIRE ( FILE=VTD%CONF%FINAM_PHASE_OFFS, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 2407, IUER, 'VTD_CONF', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' : the station offset file '// &
     &                      VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))// &
     &                      ' does not exist' )
                        RETURN
                   END IF
              END IF
              I_CNF = I_CNF + 1
            ELSE
              CALL ERR_LOG ( 2408, IUER, 'VTD_CONF', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &            'in the configuration file '// &
     &             CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ' : unknown keyword '//BUF(J1)(IND(1,1):IND(2,1)) )
              RETURN
         END IF
 410  CONTINUE
!
! --- Check whether all keywords were specified
!
      IF ( I_CNF .NE. VTD__M_CNF ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( I_CNF, STR )
           CALL INCH  ( VTD__M_CNF, STR1 )
           CALL ERR_LOG ( 2409, IUER, 'VTD_CONF', 'Wrong number of '// &
     &         'keywords in the control file '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' -- '//STR(1:I_LEN(STR))// &
     &         ' while '//STR1(1:I_LEN(STR1))//' were expected' )
           RETURN
      END IF
!
! --- Store the name of configuration file
!
      VTD%CONF%CONFIG_FINAM = CONFIG_FILE
!
      IF ( VTD%CONF%ATHD_DATA_FROM   == VTD__SPD .AND. &
     &     ILEN(VTD%CONF%DIR_EPD(1)) == 0              ) THEN
           IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 2410, IUER, 'VTD_CONF', 'Inconsistent '// &
     &               'model: anthenna thermal deformation SPD requires '//&
     &               'specifictin of external file with path delays '// &
     &               'that contains also air temperature from '// &
     &               'a numerical weather model' )
                 RETURN
            END IF
      END IF
!
      IF ( VTD%CONF%ATM_PARTIAL_TYPE == VTD__TOTS .AND. & 
           ILEN(VTD%CONF%DIR_EPD(1)) == 0               ) THEN
           CALL ERR_LOG ( 2411, IUER, 'VTD_CONF', 'Inconsistent '// &
     &         'model: atmosphere partial derivative TOTAL_SCALE '// &
     &         'requires using slant path delays for the a priori model' )
           RETURN
      END IF
!
      IF ( VTD%CONF%ATM_PARTIAL_TYPE == VTD__WATS .AND. & 
           ILEN(VTD%CONF%DIR_EPD(1)) == 0               ) THEN
           CALL ERR_LOG ( 2412, IUER, 'VTD_CONF', 'Inconsistent '// &
     &         'model: atmosphere partial derivative WATER_SCALE '// &
     &         'requires using slant path delays for the a priori model' )
           RETURN
      END IF
!
! --- Sets default verbosity level
!
      VTD%CONF%IVRB = 0
      VTD%CONF%FL_WARN = .TRUE.
!
      CALL GETENVAR ( 'VTD_VERBOSITY', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, VTD%CONF%IVRB )
      END IF
!
      CALL GETENVAR ( 'VTD_WARNING', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:1) == 'T' .OR. STR(1:1) == 'Y' ) THEN
           VTD%CONF%FL_WARN = .TRUE.
         ELSE IF ( STR(1:1) == 'F' .OR. STR(1:1) == 'N' ) THEN
           VTD%CONF%FL_WARN = .FALSE.
      END IF
!
! --- Clear the array of test variables
!
      CALL NOUT_I4 ( VTD__M_TST, VTD%CONF%TEST )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_CONF  !#!#
