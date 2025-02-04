   SUBROUTINE VEX_PARSER ( VEX, FIL_VEX, ISDR, IUER )
! 
! ***************************************************************************
! *                                                                         *
! *   Routine VEX_PARSER parses the VLBI Experiment (VEX) schedule file     *
! *                                                                         *
! *   INPUT:                                                                *
! *            FIL_VEX   =  VEX file                     { CHAR }           *
! *                         For documentation on format specification       *
! *                         visit vlbi.org/vlbi-standards/vex/              *
! *                                                                         *
! *            IUER      =  Error Handler                { INT, OPT }       *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                                                                         *
! *            ISDR      =  Severity Level                { INT, OPT }      *
! *                         This is meant to define the level of output     *
! *                         the user wishes to derive from the parser.      *
! *                         Default, ISDR = 0                               *
! *                         = 0 -- Silent mode: no output.                  *
! *                         = 1 -- Statistics mode: Just outputs results of *
! *                                                 VEX_COUNT.              *
! *                         = 2 --  ????                                    *
! *                         = 3 -- Epithet mode: Just outputs site names    *
! *                                              and how many they are.     * 
! *                         = 4 --                                          *
! *                         = 5 -- Debugging mode: The parser expects a     *
! *                                                complete VEX file with   *
! *                                                all expected parameters  *
! *                                                available                *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            VEX       =  Parsed Object                { DERIVED TYPE }   *
! *                         For more on the parsed data, see vex.i, and     *
! *                         edit it accordingly to include more data        *
! *                         blocks.                                         *
! *                                                                         *
! *  ### 21-JAN-2020  VEX_PARSER   v1.0 (c)  L. Petrov  21-JAN-2020 ###     *
! *  ### 08-JUN-2020  VEX_PARSER   v2.0 (c)  N. Habana  08-JUN-2020 ###     *
! *       - Included the $FREQ Block.                                       *
! *       - Added site positions to the station structure.                  *
! *  ### 12-JUN-2020  VEX_PARSER   v3.0 (c)  N. Habana  12-JUN-2020 ###     *
! *       - Added velocities and reference position epochs to the station   *
! *         structure.                                                      *
! *       - Fixed bug related to station and source indices.                *
! *       - Added support of all fields defiend in EXPER section.           *
! *  ### 16-JUN-2020   VEX_PARSER  v3.1 (c)  N. Habana  16-JUN-2020 ###     *
! *       - Linked the frequency block to the mode block.                   *
! *       - Updated the ISDR outputs for ISDR = 0,1,3, and 5.               *
! *  ### 01-JUL-2020  VEX_PARSER   v3.2 (c)  N. Habana  01-JUL-2020 ###     *
! *       - Changed default values for VEX%MJD_START, VEX%MJD_STOP,         *
! *         VEX%UTC_START, and VEX%UTC_STOP to zero, in their respective    *
! *         data type.                                                      * 
! *  ### 02-JUL-2020  VEX_PARSER   v4.0 (c)  N. Habana  02-JUL-2020 ###     *
! *       - edited the collection of IND_SOU and IND_STA to account         *
! *         for the fact that $SITE, $SCHEDULE, $SOURCE all depend          *
! *         on each other but can also be arranged in any order.            *
! *       - The source and station lists are now pre-gathered using         *
! *         VEX_LIST.                                                       *
! *  ### 13-JUL-2020  VEX_PARSER   v5.0 (c)  N. Habana  13-JUL-2020 ###     *
! *       - Re-wrote previous versions and now they are sorted into a       *
! *         group in the order we would like to deal with them in.          *
! *  ### 17-JUL-2020  VEX_PARSER   v6.0 (c)  N. Habana  17-JUL-2020 ###     *
! *       - Included a sorting of the schedules according to observation    *
! *         time.                                                           *
! *  ### 20-JUL-2020  VEX_PARSER   v6.1 (c)  N. Habana  20-JUL-2020 ###     *
! *       - If the experiment start and end times are undefined. We now use *
! *         the times of the initial and end schedules.                     *
! *  ### 24-AUG-2020  VEX_PARSER   v6.2 (c)  N. Habana  24-AUG-2020 ###     *
! *       - Silenced the antenna flag, FL_ANT, for ISDR = 5, as we don't    *
! *         actually read from the antenna block (yet).                     *
! *  ### 31-OCT-2020  VEX_PARSER   v7.0 (c)  N. Habana  31-OCT-2020 ###     *
! *       - Converted SKY_FRQ, and CHA_BW from Mhz to Hz.                   *
! *       - Added the IF block.                                             *
! *  ### 01-NOV-2020  VEX_PARSER   v7.1 (c)  N. Habana  01-NOV-2020 ###     *
! *       - Added a count for no. of S and X bands frequencies in the       *
! *         $FREQ and $IF blocks.                                           *
! *  ### 10-NOV-2020  VEX_PARSER   v7.2 (c)  N. Habana  10-NOV-2020 ###     *
! *       - Added N_FRQ to vex.i, and all instances of N_FRQ in             *
! *         vex_parser.f are changed to VEX%N_FRQ.                          *
! *  ### 11-NOV-2020  VEX_PARSER   v7.3 (c)  N. Habana  11-NOV-2020 ###     *
! *       - Added the station and source lists to VEX_TYPE.                 *
! *  ### 20-NOV-2020  VEX_PARSER   v7.4 (c)  N. Habana  20-NOV-2020 ###     *
! *       - Sorted the source structure alphabetically.                     *
! *  ### 24-NOV-2020  VEX_PARSER   v7.5 (c)  N. Habana  24-NOV-2020 ###     *
! *  ### 03-DEC-2020  VEX_PARSER   v7.5 (c)  N. Habana  03-DEC-2020 ###     *
! *       - The sorting of sources, stations, and scans that was            *
! *         previously done at the end of the parsing, is now done after    *
! *         each respective block.                                          *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      TYPE ( VEX_TYPE ) :: VEX                 ! Vex Object (defined in vex.i)
      CHARACTER  FIL_VEX*(*)                   ! Vex File Name
      CHARACTER  DELIM*5                       ! Deliminator
      CHARACTER  RA_STR*32, DEC_STR*32         ! RA and DEC strings
      CHARACTER  VEX_DATE_STR*18               ! Date in Vex Format
      CHARACTER  STR_2_NUM*10
      INTEGER*4  MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4  MAXL_STRING                   ! Max. String length
      INTEGER*4  N_SECTS                       ! No. of Sections
      PARAMETER  ( N_SECTS = 10 )
      PARAMETER  ( MAXL_STRING = 4096 )           
      PARAMETER  ( MP = 128*1024 )             
      PARAMETER  ( MIND = 512 )                 
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'='//':' ) ! Null, Space, Tab, Equal-sign, Colon
      CHARACTER  BUF(MP)*(MAXL_STRING), SECT_ID*32                 ! Read File, File Section
      CHARACTER  SECTS(N_SECTS)*32
!***!      CHARACTER  J_SOU(VEX__MSOU)*16
      CHARACTER  CI_STA(VEX__MSTA)*2
!***!      CHARACTER  CS_STA(VEX__MSTA)*16 
      CHARACTER  ANT_LIST(VEX__MSTA)*16
      INTEGER*4  L_SOU, L_STA                 ! No, of Source and Station X-ters
      CHARACTER  STR*128                      ! Read String
      REAL*8     SCAN_STP
      INTEGER*4  IUER
      INTEGER*4  ISDR, N_IFS           ! Error Handler Input, No. of freq
      INTEGER*4  J0, J1, J2, J3, J4, J5, J6, J7, J8
      INTEGER*4  I0, IP, IE, I2, ISTA, ISOU, ISCA
      INTEGER*4  NP, LIND, LINDC, IND(2,MIND), INDC(2,MIND), IER, LN
      INTEGER*4  N_SCA_STN, N_FRQ_CHA, N_PRO_MOD      ! No. stns in scan, channels in freq., & proc modes
      INTEGER*4  N_IF_DEF, N_IS_PCAL                  ! No. if_def, P-cal is present
      INTEGER*8  IVAL_I8
      CHARACTER  WRAP_ID*3, MODE_NAME*32
      LOGICAL*1  SECTS_LOOPING
      LOGICAL*1  FL_GLO, FL_EXP, FL_PRO, FL_STA, FL_SOU, FL_SCA    ! Block Flags
      LOGICAL*1  FL_STE, FL_ANT, FL_FRQ, FL_MOD, FL_IFS
!***!      LOGICAL*1  FL_TC, FL_HSM, FL_NSC, FL_NTS, FL_SN, FL_IAU      ! Some parameter flags
      INTEGER*4, EXTERNAL :: ILEN
      CHARACTER, EXTERNAL :: VEX_TO_DATE*19
      INTEGER*4, EXTERNAL :: ADD_CLIST, LTM_DIF
#ifdef GNU
      INTEGER*4, EXTERNAL :: VEX_COMPAR_SCA, VEX_COMPAR_SOU
      INTEGER*4, EXTERNAL :: VEX_COMPAR_STA
#else
      INTEGER*2, EXTERNAL :: VEX_COMPAR_SCA, VEX_COMPAR_SOU
      INTEGER*2, EXTERNAL :: VEX_COMPAR_STA
#endif
!
! --- Reading the Vex File to variable, BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_VEX, MP, BUF, NP, IER ) 
      IF ( IER .NE. 0 ) THEN                       ! i.e. if there is error
           CALL ERR_LOG ( 8111, IUER, 'VEX_PARSER', 'Error in reading ' &
     &          //'input vex file '//FIL_VEX )                  
           RETURN 
      END IF
      IF ( VEX%STATUS .NE. VEX__INIT ) CALL VEX_CLEAN ( VEX ) 
!
! --- Is this a VEX File?
!
      IF ( BUF(1)(1:7) .NE. 'VEX_rev' ) THEN
           CALL ERR_LOG ( 8112, IUER, 'VEX_PARSER', 'FORMAT ERROR: '//  &
     &          'VEX Files should always start with "VEX_rev" not '//   &
     &         '"'//BUF(1)(1:7)//'"' )
           RETURN
      END IF
!
! --- Counting the No. of Stations, Sources, and Scans
! --- N.B.: If these blocks are not there, the VEX_COUNT will notify 
!           you, hence their respective flags are neglected in 
!           VEX_PARSER
!
      CALL ERR_PASS ( IUER, IER )
      CALL VEX_COUNT ( VEX%N_STA, VEX%N_SOU, VEX%N_SCA, VEX%N_FRQ,      &
     &                 N_IFS, FIL_VEX, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8113, IUER, 'VEX_PARSER', 'Error in '// &
     &         'VEX_COUNT' ) 
           RETURN
      END IF
!
! --- For the Stats Case
!
      IF ( ISDR .EQ. 1 ) THEN
          WRITE ( 6, '(A)' ) 'COUNT OUTPUT: VEX_PARSER running '//   &
     &                       'in stat mode, for file: '//TRIM(FIL_VEX)
      END IF
!
! --- The actual number of elements (currently) corresponding to 
!     Sources & Stations
!
      L_SOU =  0
      L_STA =  0
!
! --- Allocating the (null pointed) structures found in VEX_TYPE to 
!     variables.
!     Array sizes based on the counts derived from above
!
      ALLOCATE ( VEX%STA(VEX%N_STA), STAT = IER )
      ALLOCATE ( VEX%SOU(VEX%N_SOU), STAT = IER ) 
      ALLOCATE ( VEX%SCA(VEX%N_SCA), STAT = IER )
      ALLOCATE ( VEX%FRQ(VEX%N_FRQ), STAT = IER )
      ALLOCATE ( VEX%IFS(N_IFS), STAT = IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8114, IUER, 'VEX_PARSER', 'Failed to allocate'  &
     &               //'VEX%STA, VEX%SOU, VEX%SCA, and VEX%FRQ' )
           RETURN 
      END IF
!
! --- Initialise the flags
!
      FL_GLO = .FALSE.  ! Global block 
      FL_EXP = .FALSE.  ! Experiment block
      FL_PRO = .FALSE.  ! Procedures block
      FL_SOU = .FALSE.  ! Sources block
      FL_SCA = .FALSE.  ! Schedule block
      FL_STA = .FALSE.  ! Station block
      FL_STE = .FALSE.  ! Site block
!****!      FL_ANT = .FALSE.  ! Antenna block
      FL_FRQ = .FALSE.  ! Frequency block
      FL_MOD = .FALSE.  ! Mode Block
      FL_IFS = .FALSE.  ! IF Block
!
!****!      FL_SN      = .FALSE.  ! Scan Number
!****!      FL_TC      = .FALSE.  ! Tape change
!****!      FL_HSM     = .FALSE.  ! Headstack Motion
!****!      FL_NSC     = .FALSE.  ! New Source Command
!****!      FL_NTS     = .FALSE.  ! New Tape Setup
!****!      FL_IAU     = .FALSE.  ! IAU Name
!
! --- Section List in the preferred order
!
      SECTS(1)  = 'GLOBAL'
      SECTS(2)  = 'EXPER'
      SECTS(3)  = 'SITE'
      SECTS(4)  = 'STATION'
      SECTS(5)  = 'PROCEDURES'
      SECTS(6)  = 'SOURCE'
      SECTS(7)  = 'SCHED'
      SECTS(8)  = 'MODE'
      SECTS(9)  = 'FREQ'
      SECTS(10) = 'IFS'
!
! --- Begin the Parsing Process
!
!****!      SECT_ID = '??'
!
! --- Outer loop to define blocks
!
      DO 310 J0 = 1, N_SECTS
!
! ------ Inner Loop to collect Blocks of interest
!         
         SECT_ID = '??'
         J2      = 0
         J3      = 0
         J4      = 0
         J5      = 0
         J6      = 0
         J7      = 0
         J8      = 0
         DO 410 J1 = 1, NP
!
! --------- Bypass empty lines
!
            IF ( ILEN(BUF(J1)) == 0 )  GOTO 410
!
! --------- Bypass comment lines.
!
            IF ( BUF(J1)(1:1) == '*' ) GOTO 410
!
! --------- Extract words from the read line
!
            CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! --------- Flag to indicate we are beginning a new Block
!
            IF ( BUF(J1)(1:1) == '$' ) THEN        
               SECT_ID = BUF(J1)(IND(1,1)+1:IND(2,1))
               IE = INDEX ( SECT_ID, ';' ) 
               IF ( IE > 0 ) CALL CLRCH ( SECT_ID(IE:) )
!
! ------------ N.B. There are two possible names for the PROCEDURES BLOCK
!
               IF ( SECT_ID == 'PROCS' ) THEN
                  SECT_ID = 'PROCEDURES'
               END IF
!
! ------------ N.B. "IF" is a reserved word, so when in that block, rename
!                   the section to "IFS"
!
               IF ( SECT_ID == 'IF' ) THEN
                  SECT_ID = 'IFS'
               END IF
            END IF
!
! --------- Is that Block, the one we are interested in right now?
! 
            IF ( SECT_ID == SECTS(J0) ) THEN 
!
! ------------ Handle the GLOBAL BLOCK
!                
               IF ( SECT_ID == 'GLOBAL' ) THEN
                  FL_GLO  = .TRUE.
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'revision' .AND.   &
     &                 BUF(J1)(IND(1,2):IND(2,2)) == 'number' ) THEN
! ------------------
                     IF ( BUF(J1)(IND(1,3):IND(1,3)) == '"' )           &
     &                        IND(1,3) = IND(1,3) + 1
! ------------------
                     IF ( BUF(J1)(IND(2,3):IND(2,3)) == '"' )           &
     &                        IND(2,3) = IND(2,3) - 1
! ------------------
                     IF ( IND(2,3) .GE. IND(1,3) ) THEN
                        VEX%REVISION = BUF(J1)(IND(1,3):IND(2,3))
                     ELSE
                        CALL CLRCH ( VEX%REVISION )
                     END IF
                  END IF
!
! ------------ Handle the EXPERIMENT BLOCK
!
               ELSE IF ( SECT_ID == 'EXPER' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$EXPER;' ) THEN

                     FL_EXP  = .TRUE.
!
! ------------------ N.B: Not all VEX Files will have the nominal start and 
!                         stop times for the experiment block. So we set the 
!                         default values for both to 0 MJD and 0.D0 UTC.
!
                     VEX%MJD_START  = 0
                     VEX%UTC_START  = 0.D0
                     VEX%MJD_STOP   = 0
                     VEX%UTC_STOP   = 0.D0
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'exper_name') THEN         
                     CALL CLRCH ( VEX%EXPER_NAME )
                     IF ( BUF(J1)(IND(1,2):IND(1,2)) == '"' )           &
     &                  IND(1,2) = IND(1,2) + 1
                        IND(2,3) = IND(2,3) - 1
                     IF ( BUF(J1)(IND(2,3):IND(2,3)) == '"' )           &
     &                  IND(2,3) = IND(2,3) - 1
                     IF ( IND(2,3) .GE. IND(1,3) ) THEN
                        CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), VEX%EXPER_NAME )      ! Convert to Caps
                     ELSE
                        CALL CLRCH ( VEX%EXPER_NAME )
                     END IF
                     IE = INDEX ( VEX%EXPER_NAME, ';' ) 
                     IF ( IE > 0 ) CALL CLRCH ( VEX%EXPER_NAME(IE:) )
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) ==                    &
     &                    'exper_description'     ) THEN
                     IF ( BUF(J1)(IND(1,2):IND(1,2)) == '"' )           &
     &                        IND(1,2) = IND(1,2) + 1
                     IND(2,LIND) = IND(2,LIND) - 1
                     IF ( BUF(J1)(IND(2,LIND):IND(2,LIND)) == '"' )     &
     &                        IND(2,LIND) = IND(2,LIND) - 1
                     IF ( IND(2,LIND) .GE. IND(1,2)     ) THEN
                        VEX%EXPER_DESCR =                               &
     &                      BUF(J1)(IND(1,2):IND(2,LIND))
                     ELSE 
                        CALL CLRCH ( VEX%EXPER_DESCR )
                     END IF
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) ==                    &
     &                     'exper_nominal_start'  ) THEN
                     VEX_DATE_STR    =  BUF(J1)(IND(1,2):IND(2,2))
                     STR             = VEX_TO_DATE ( VEX_DATE_STR, IER )
                     CALL DATE_TO_TIME ( STR, VEX%MJD_START,            &
     &                        VEX%UTC_START, IER )
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) ==                    &
     &                     'exper_nominal_stop'   ) THEN
                     VEX_DATE_STR   =  BUF(J1)(IND(1,2):IND(2,2))
                     STR            = VEX_TO_DATE ( VEX_DATE_STR, IER )
                     CALL DATE_TO_TIME (STR, VEX%MJD_STOP,              &
     &                        VEX%UTC_STOP, IER)
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) ==                    &
     &                     'contact_name'         ) THEN
                     VEX%CONTACT_NAME = BUF(J1)(IND(1,2):IND(2,LIND))
                     IE = INDEX ( VEX%CONTACT_NAME, ';' ) 
                     IF ( IE > 0 ) CALL CLRCH ( VEX%CONTACT_NAME(IE:) )
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) ==                    &
     &                     'scheduler_name'       ) THEN
                     VEX%SCHEDULER_NAME  =                              &
     &                   BUF(J1)(IND(1,2):IND(2,LIND)-1)
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) ==                    &
     &                     'scheduler_email'      ) THEN
                     VEX%SCHEDULER_EMAIL =                              &
     &                   BUF(J1)(IND(1,2):IND(2,LIND)-1)
                  END IF
!
! ------------ Handle the SITE BLOCK
! ------------ N.B: The parameters derived from here go to the
!                   VEX__STA_TYPE. Stations in the SITE block are ALWAYS 
!                   in the same order as in the STATION block, so we can 
!                   use the same variable, J4.
!
               ELSE IF (SECT_ID == 'SITE' ) THEN
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SITE;' ) THEN
                     FL_STE   = .TRUE.
                     J4       =  0
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN 
                     J4 = J4 + 1
                     IF ( J4 > VEX%N_STA) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( J1, STR )
                          CALL ERR_LOG ( 8115, IUER, 'VEX_PARSER', 'Error in '// &
     &                        'parsing line '//TRIM(STR)//' -- more $SITE '// &
     &                        'definitions than $STATIONs' )
                          RETURN 
                     END IF
!
! ------------------ N.B: Not all VEX Files will have the velocity or 
!                         position epoch defined. So we set the former 
!                         to a default of zero on all axis, and the 
!                         latter is set to J2000 default.
! 
                     VEX%STA(J4)%SITE_VEL(1) = 0.D0
                     VEX%STA(J4)%SITE_VEL(2) = 0.D0
                     VEX%STA(J4)%SITE_VEL(3) = 0.D0
! ------------------
                     VEX%STA(J4)%SITE_POS_EPOCH_MJD = 51544
!
! --------------- Site position are mandatory, though.
!
                  ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) ==                &
     &                           'site_position' ) THEN
! ------------------
                     READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)),            &
     &                      FMT='(F15.6)', IOSTAT=IER )                 &
     &                  VEX%STA(J4)%SITE_POS(1)                ! X [m]
! ------------------ 
                     READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)),            &
     &                      FMT='(F15.6)', IOSTAT=IER )                 &
     &                  VEX%STA(J4)%SITE_POS(2)                 ! Y [m]
! ------------------
                     READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)),            &
     &                      FMT='(F15.6)', IOSTAT=IER )                 &
     &                  VEX%STA(J4)%SITE_POS(3)                 ! Z [m]
!
! --------------- Velocity read
!
                  ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) ==                &
     &                         'site_velocity' ) THEN
! ------------------
                     READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)),            &
     &                      FMT='(F15.6)', IOSTAT=IER )                 &
     &                  VEX%STA(J4)%SITE_VEL(1)           ! XDOT [m/s]
! ------------------ 
                     READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)),            &
     &                      FMT='(F15.6)', IOSTAT=IER )                 &
     &                  VEX%STA(J4)%SITE_VEL(2)           ! YDOT [m/s]
! ------------------
                     READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)),            & 
     &                      FMT='(F15.6)', IOSTAT=IER )                 & 
     &                  VEX%STA(J4)%SITE_VEL(3)           ! ZDOT [m/s]
!
! --------------- Reference Position Epoch read
!
                  ELSEIF ( BUF(J1)(IND(1,1):IND(2,1)) ==                &
     &                            'site_position_epoch' ) THEN
                     READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)-1),          &
     &                         FMT='(2I10)', IOSTAT=IER )               &
     &                     VEX%STA(J4)%SITE_POS_EPOCH_MJD
                  END IF
!
! ------------ Handle the STATION BLOCK
!
               ELSE IF ( SECT_ID == 'STATION' ) THEN
! --------------- 
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$STATION;' ) THEN
                     FL_STA   = .TRUE.
                     J4       =  0
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN 
                     J4 = J4 + 1
                     CALL CLRCH ( VEX%C_STA(J4) )
                     ISTA = 0
                  END IF
!
! --------------- Parse the station parameters to the VEX_STA_TYPE
!
                  IF ( BUF(J1)(IND(1,2):IND(2,2)) == '$SITE' ) THEN
                     CALL CLRCH ( VEX%STA(J4)%SITE_NAME )
                     CALL TRAN ( 11, BUF(J1)(IND(1,3):IND(2,3)),      &
     &                           VEX%STA(J4)%SITE_NAME )      ! Name in Caps
                     IE = INDEX ( VEX%STA(J4)%SITE_NAME, ';' ) 
                     IF ( IE > 0 ) CALL CLRCH ( VEX%STA(J4)%SITE_NAME(IE:) )
                     VEX%C_STA(J4) = VEX%STA(J4)%SITE_NAME 
!****!                 FL_STE                 = .TRUE.
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) ==               &
     &                          '$ANTENNA' ) THEN
                     CALL CLRCH ( VEX%STA(J4)%ANT_NAME )
                     CALL TRAN ( 11, BUF(J1)(IND(1,3):IND(2,3)),      &
     &                           VEX%STA(J4)%ANT_NAME )    ! Name in Caps
                     IE = INDEX ( VEX%STA(J4)%ANT_NAME, ';' ) 
                     IF ( IE > 0 ) CALL CLRCH ( VEX%STA(J4)%ANT_NAME(IE:) )
                     VEX%C_STA(J4) = VEX%STA(J4)%ANT_NAME
!****!                 FL_ANT                 = .TRUE.
!----------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN 
                     CALL CLRCH ( VEX%STA(J4)%SITE_ID  )
                     VEX%STA(J4)%SITE_ID  = BUF(J1)(IND(1,2):IND(2,2))
                     CALL TRAN ( 12, VEX%STA(J4)%SITE_ID,               &
     &                           VEX%STA(J4)%SITE_ID )
                     IE = INDEX ( VEX%STA(J4)%SITE_ID, ';' ) 
                     IF ( IE > 0 ) CALL CLRCH ( VEX%STA(J4)%SITE_ID(IE:) )
                     CI_STA(J4) = VEX%STA(J4)%SITE_ID
                  END IF                  
!
! --------------- Get the length of the station list
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'enddef;' ) THEN
                     ISTA = ILEN( VEX%C_STA(J4) )
                  END IF
!
! --------------- Done Handling the Station Block 
!
                  IF ( (J4 .EQ. VEX%N_STA) .AND. (ISTA .NE. 0) ) THEN
!
! ------------------ Sort the station list, and structure alphabetically
!
                     CALL SORT_FAST_CH ( VEX%N_STA, VEX%C_STA )
                     CALL FOR_QSORT ( VEX%STA, VEX%N_STA,               &
     &                       SIZEOF(VEX%STA(1)),  VEX_COMPAR_STA )
! ------------------
                     GOTO 410
                  END IF
!
! ------------ Handle the PROCEDURES BLOCK
! ------------ N.B: The parameters derived from this block go to the 
!                   VEX_STA_TYPE. However, unlike the other parameters 
!                   found in the Station Block, these may appear once or 
!                   more and apply to every station. The indices are 
!                   applied to STA.
!
               ELSE IF ( SECT_ID == 'PROCEDURES' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$PROCEDURES;' .OR.&
     &                 BUF(J1)(IND(1,1):IND(2,1)) == '$PROCS' ) THEN
                     FL_PRO     = .TRUE.
                     N_PRO_MOD  = 0
                  END IF   
!
! --------------- Update the procedures count
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN
                     N_PRO_MOD = N_PRO_MOD + 1
!
! --------------- Parse the procedures parameters to VEX_STA_TYPE
!
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'procedure_name_prefix' )  THEN
                     VEX%STA(N_PRO_MOD)%PROCEDURE_NAME_PREFIX =         &
     &               BUF(J1)(IND(1,2):IND(2,2)-1)
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'tape_change' ) THEN      ! (2nd Word) {Int}
!****!                   FL_TC = .TRUE.
                     CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)),            &
     &                       VEX%STA(N_PRO_MOD)%TAPE_CHANGE )
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'headstack_motion' )  THEN  ! (2nd Word) {Int}
!****!                   FL_HSM   = .TRUE.
                     CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)),            &
     &                       VEX%STA(N_PRO_MOD)%HEADSTACK_MOTION )
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'new_source_command' )  THEN  ! (2nd Word) {Int}
!****!                   FL_NSC   = .TRUE.
                     CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)),            &
     &                       VEX%STA(N_PRO_MOD)%NEW_SOURCE_COMMAND ) 
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'new_tape_setup' )      THEN  ! (2nd Word) {Int}
!****!                   FL_NTS  = .TRUE.
                     CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)),            &
     &                       VEX%STA(N_PRO_MOD)%NEW_TAPE_SETUP )
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'setup_always' )        THEN   ! (3rd Word) {Int}
                     CALL CHIN ( BUF(J1)(IND(1,3):IND(2,3)),            &
     &                       VEX%STA(N_PRO_MOD)%SETUP_ALWAYS )
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'parity_check' )        THEN  ! (3rd Word) {Int}
                     CALL CHIN ( BUF(J1)(IND(1,3):IND(2,3)),            &
     &                       VEX%STA(N_PRO_MOD)%PARITY_CHECK )
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'tape_prepass' )        THEN  ! (3rd Word) {Int}
                     CALL CHIN ( BUF(J1)(IND(1,3):IND(2,3)),            &
     &                       VEX%STA(N_PRO_MOD)%TAPE_PREPASS )
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'preob_cal' )           THEN  ! (3rd Word) {Int}
                     CALL CHIN ( BUF(J1)(IND(1,3):IND(2,3)),            &
     &                       VEX%STA(N_PRO_MOD)%PREOB_CAL )
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'midob_cal' )           THEN  ! (3rd Word) {Int}
                     CALL CHIN ( BUF(J1)(IND(1,3):IND(2,3)),            &
     &                       VEX%STA(N_PRO_MOD)%MIDOB_CAL )
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'postob_cal' )          THEN  ! (3rd Word) {Int}
                     CALL CHIN ( BUF(J1)(IND(1,3):IND(2,3)),            &
     &                       VEX%STA(N_PRO_MOD)%POSTOB_CAL )
                  END IF
!
! ------------ Handle the SOURCE BLOCK
!
               ELSE IF ( SECT_ID == 'SOURCE' ) THEN
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SOURCE;' ) THEN
                     FL_SOU  = .TRUE.
                     J2      =  0
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN 
                     J2 = J2 + 1
                     CALL CLRCH ( VEX%C_SOU(J2) )
                     ISOU = 0
                  END IF
!
! --------------- Parse the source parameters to the VEX_SOU_TYPE
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'source_name' )THEN
                     VEX%SOU(J2)%NAME = BUF(J1)(IND(1,2):IND(2,2))
                     IE = INDEX ( VEX%SOU(J2)%NAME, ';' )
                     IF ( IE > 0 ) CALL CLRCH ( VEX%SOU(J2)%NAME(IE:) )
                     VEX%C_SOU(J2)        = VEX%SOU(J2)%NAME
                  END IF  
!
! ----------------- We comment out these lines, since they break 
! ----------------- the source association
!
!#                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'IAU_name' ) THEN
!#                     VEX%SOU(J2)%IAU_NAME = BUF(J1)(IND(1,2):IND(2,2)-1)  
!#                     VEX%C_SOU(J2)            = VEX%SOU(J2)%IAU_NAME
!#                     FL_IAU               = .TRUE.
!#                  END IF
!
! --------------- Parse the RA and DEC
! --------------- N.B: They may be on the same line deliminated by 
!                      ";" or fall on different lines
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ra' )  THEN
                     LN  = LEN(BUF(J1)(IND(1,2):IND(2,2)-1))
                     CALL VEX_TO_TIME ( BUF(J1)(IND(1,2):IND(2,2)-1),    &
     &                        LN, RA_STR, IER )
                     CALL HR_TAT ( RA_STR, VEX%SOU(J2)%RA, IER )
!
! ------------------ Same line
!
                     IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'dec' )  THEN
                        LN  = LEN(BUF(J1)(IND(1,4):IND(2,4)-1))
                        CALL VEX_TO_ANG ( BUF(J1)(IND(1,4):IND(2,4)-1),  &
     &                           LN, DEC_STR, IER )
                        CALL GR_TAT ( DEC_STR, VEX%SOU(J2)%DEC, IER)
                     END IF
!
! --------------- Diff. Line
!
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'dec' ) THEN
                     LN  = LEN(BUF(J1)(IND(1,2):IND(2,2)-1))
                     CALL VEX_TO_ANG ( BUF(J1)(IND(1,2):IND(2,2)-1),    &
     &                        LN, DEC_STR, IER )
                     CALL GR_TAT ( DEC_STR, VEX%SOU(J2)%DEC, IER)
                  END IF
!
! --------------- Get the length of the source list
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'enddef;' ) THEN
                     ISOU = ILEN( VEX%C_SOU(J2) )
                  END IF
!
! --------------- Done Handling the SOURCE Block 
!           
                  IF ( (J2 .EQ. VEX%N_SOU) .AND. (ISOU .NE. 0) ) THEN
!
! ------------------ Sort the sources list, and structure alphabetically
!
                     CALL SORT_FAST_CH ( VEX%N_SOU, VEX%C_SOU )
                     CALL FOR_QSORT ( VEX%SOU, VEX%N_SOU,               &
     &                       SIZEOF(VEX%SOU(1)),  VEX_COMPAR_SOU )
! ------------------
                     GOTO 410
                  END IF
!
! ------------ Handle the SCHEDULE BLOCK
!
               ELSE IF ( SECT_ID == 'SCHED' ) THEN
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SCHED;' ) THEN
                     FL_SCA   = .TRUE.
                     J3       =  0
                  END IF
!
! --------------- Parse the schedule parameters to the VEX_SCA_TYPE
! --------------- N.B the start, mode and source may be on the same line.
!                     The log book has them in that order, besides 
!                     stating that the "start" should come first, there 
!                     is no stipulation on whether "mode" or source 
!                     should come right after.
!
!
! --------------- start a scan
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'scan' ) THEN
!*****!               FL_SN      = .TRUE.
                     J3         =  J3 + 1
                     VEX%SCA(J3)%SCAN_NAME = BUF(J1)(IND(1,2):IND(2,2)) 
                     IE = INDEX ( VEX%SCA(J3)%SCAN_NAME, ';' ) 
                     IF ( IE > 0 ) CALL CLRCH ( VEX%SCA(J3)%SCAN_NAME(IE:) )
                     N_SCA_STN  =  0
                     ISCA       =  0
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'start' ) THEN
                     VEX_DATE_STR     =  BUF(J1)(IND(1,2):IND(2,2))
                     STR = VEX_TO_DATE ( VEX_DATE_STR, IER )
                     CALL DATE_TO_TIME ( STR, VEX%SCA(J3)%MJD,          &
     &                      VEX%SCA(J3)%UTC, IER )
!
! ------------------ If they are on the same line
! ------------------ Option 1: start->mode->source {Getting mode} 
!
                     IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'mode' )  THEN   
                        VEX%SCA(J3)%MODE = BUF(J1)(IND(1,4):IND(2,4)-1) 
                     END IF
!
! ------------------ Option 2: start->source->mode {Getting mode}
!               
                     IF ( BUF(J1)(IND(1,5):IND(2,5)) == 'mode' )  THEN 
                        VEX%SCA(J3)%MODE = BUF(J1)(IND(1,6):IND(2,6)-1) 
                     END IF
!
! ------------------ Option 2: start->source->mode {Getting source}
!
                     IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'source' ) THEN
                          VEX%SCA(J3)%SOU_NAME = BUF(J1)(IND(1,4):IND(2,4))
                          IE = INDEX ( VEX%SCA(J3)%SOU_NAME, ';' ) 
                          IF ( IE > 0 ) CALL CLRCH ( VEX%SCA(J3)%SOU_NAME(IE:) )
!
! --------------------- Check source name against the list of sources in 
!                       and index the name with it's list entry index.
!
                        VEX%SCA(J3)%IND_SOU  = LTM_DIF ( 0, VEX%N_SOU,  &
     &                        VEX%C_SOU, VEX%SCA(J3)%SOU_NAME )
!
! --------------------- Source name not found on list.
!
                        IF ( VEX%SCA(J3)%IND_SOU  < 1 ) THEN
                           VEX%SCA(J3)%IND_SOU  = LTM_DIF ( 0,          &
     &                         VEX%N_SOU, VEX%C_SOU,                    &
     &                         VEX%SCA(J3)%SOU_NAME  ) 
!%NH%!                           CALL ERR_LOG ( 8116, IUER, 'VEX_PARSER',     &
!%NH%!     &                           'Error: '//VEX%SCA(J3)%SOU_NAME//      &
!%NH%!     &                           ' not found in '//FIL_VEX//            &
!%NH%!     &                           ' listed sources.' )
                        END IF
                     END IF
!
! ------------------ Option 1: start->mode->source {Getting source}
!
                     IF ( BUF(J1)(IND(1,5):IND(2,5)) == 'source' ) THEN
                        VEX%SCA(J3)%SOU_NAME = BUF(J1)(IND(1,6):IND(2,6))
                        IE = INDEX ( VEX%SCA(J3)%SOU_NAME, ';' ) 
                        IF ( IE > 0 ) CALL CLRCH ( VEX%SCA(J3)%SOU_NAME(IE:) )
                        VEX%SCA(J3)%IND_SOU  = LTM_DIF ( 0, VEX%N_SOU,  &
     &                         VEX%C_SOU, VEX%SCA(J3)%SOU_NAME )
                        IF ( VEX%SCA(J3)%IND_SOU  < 1 ) THEN
!
! ------------------------ Source not found on list
!
                           VEX%SCA(J3)%IND_SOU  = LTM_DIF ( 0,          &
     &                          VEX%N_SOU, VEX%C_SOU,                   &
     &                          VEX%SCA(J3)%SOU_NAME )
!%NH%!                           CALL ERR_LOG ( 8117, IUER, 'VEX_PARSER',     &
!%NH%!     &                           'Error: '//VEX%SCA(J3)%SOU_NAME//      &
!%NH%!     &                           ' not found in '//FIL_VEX//            &
!%NH%!     &                           ' listed sources.' )
                        END IF
                     END IF
!
! --------------- If on different lines
!
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'mode' ) THEN
                     VEX%SCA(J3)%MODE  = BUF(J1)(IND(1,2):IND(2,2))
                     IE = INDEX ( VEX%SCA(J3)%MODE, ';' ) 
                     IF ( IE > 0 ) CALL CLRCH ( VEX%SCA(J3)%MODE(IE:) )
! --------------- 
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'source') THEN
                     VEX%SCA(J3)%SOU_NAME = BUF(J1)(IND(1,2):IND(2,2))
                     IE = INDEX ( VEX%SCA(J3)%SOU_NAME, ';' ) 
                     IF ( IE > 0 ) CALL CLRCH ( VEX%SCA(J3)%SOU_NAME(IE:) )
                     VEX%SCA(J3)%IND_SOU  = LTM_DIF ( 0, VEX%N_SOU,     &
     &                     VEX%C_SOU, VEX%SCA(J3)%SOU_NAME )
                     IF ( VEX%SCA(J3)%IND_SOU  < 1 ) THEN
                        VEX%SCA(J3)%IND_SOU  = LTM_DIF ( 0, VEX%N_SOU,  &
     &                        VEX%C_SOU, VEX%SCA(J3)%SOU_NAME )
                     END IF
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'station' ) THEN
                     N_SCA_STN = N_SCA_STN + 1
!
! ------------------ Add Station ID to Character list of Stations (if absent)
!                    (Second word) {2 Characters}
! ------------------ First seek the station name in the CS_STA (site name)
!
                     VEX%SCA(J3)%IND_STA(N_SCA_STN) = 0
                     IF ( IND(2,2) - IND(1,2) == 1 ) THEN
!
! --------------------- First, seek in the site id list
!
                        CALL TRAN ( 12, BUF(J1)(IND(1,2):IND(2,2)),     &
     &                        BUF(J1)(IND(1,2):IND(2,2)) )
                        VEX%SCA(J3)%IND_STA(N_SCA_STN) =                &
     &                        LTM_DIF ( 0, VEX%N_STA, CI_STA,           &
     &                            BUF(J1)(IND(1,2):IND(2,2)) )
                     END IF
                     IF ( VEX%SCA(J3)%IND_STA(N_SCA_STN) < 1 ) THEN
!
! --------------------- Second, seek the station name in the CS_STA
!                       (site name)
!                 
                        CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)),     &
     &                        BUF(J1)(IND(1,2):IND(2,2)) )
                        VEX%SCA(J3)%IND_STA(N_SCA_STN) =                &
     &                       LTM_DIF ( 0, VEX%N_STA, VEX%C_STA,            &
     &                           BUF(J1)(IND(1,2):IND(2,2)) )
                     END IF
                     IF ( VEX%SCA(J3)%IND_STA(N_SCA_STN) < 1 ) THEN
!
! --------------------- Third, seek the station name in the VEX%C_STA 
!                       (antenna name)
!
                        CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)),     & 
     &                        BUF(J1)(IND(1,2):IND(2,2)) )
                        VEX%SCA(J3)%IND_STA(N_SCA_STN) =                &
     &                       LTM_DIF ( 0, VEX%N_STA, VEX%C_STA,            &
     &                           BUF(J1)(IND(1,2):IND(2,2)) )
                        IF ( VEX%SCA(J3)%IND_STA(N_SCA_STN) < 1 ) THEN
!
! ------------------------ Station is still not found after checking all 
!                          possible lists, i.e. STA_ID_LIST, STA_LIST, 
!                          and ANT_LIST
!
                           CALL ERR_LOG ( 8118, IUER, 'VEX_PARSER',     &
     &                          'Station '//BUF(J1)(IND(1,2):IND(2,2))//&
     &                          ' not found in '//FIL_VEX//             &
     &                          ' listed stations or antennas.' )
                           RETURN 
                        END IF
                     END IF
!
! ------------------ Start Offset for that station (Third word) {Integer} 
!
                     STR_2_NUM = BUF(J1)(IND(1,3):IND(2,3))
                     IF ( INDEX (STR_2_NUM , '.')<1 ) THEN   
                        STR_2_NUM = TRIM (STR_2_NUM)//'.0'      
                     END IF                             
                     READ ( UNIT=STR_2_NUM, FMT='(F10.5)', IOSTAT=IER ) &
     &                    VEX%SCA(J3)%START_OFFSET(N_SCA_STN)
!
! ------------------ Time it took to scan that station
!                    (Fifth word less Third Word) {Integer}
!
                     STR_2_NUM = BUF(J1)(IND(1,5):IND(2,5))
                     IF ( INDEX (STR_2_NUM , '.') < 1 ) THEN   
                          STR_2_NUM = TRIM (STR_2_NUM)//'.0'      
                     END IF                             
                     READ ( UNIT=STR_2_NUM, FMT='(F10.5)', IOSTAT=IER ) SCAN_STP
!
! ------------------ Correction to get duration by substracting the start 
!                    from the stop time.
!
                     VEX%SCA(J3)%SCAN_DUR(N_SCA_STN) =                  &
     &                    SCAN_STP - VEX%SCA(J3)%START_OFFSET(N_SCA_STN)
!
! ------------------ The wrap mode is an integer derived from the value of 
!                    point sectr. It is not always the ninth word, as 
!                    the pass may often be neglected in some files. However,
!                    it is always the second last word.
!                    So, we now use LIND.
!
!******!               WRAP_ID = BUF(J1)(IND(1,9)+1:IND(2,9)) ! [Neglect the &]
                     WRAP_ID = BUF(J1)(IND(1,LIND-1):IND(2,LIND-1)) 
                     IF ( WRAP_ID .EQ. '&n') THEN
                        VEX%SCA(J3)%WRAP_MODE(N_SCA_STN) = 1
                     ELSE IF ( WRAP_ID .EQ. '&cw' ) THEN
                        VEX%SCA(J3)%WRAP_MODE(N_SCA_STN) = 2
                     ELSE IF ( WRAP_ID .EQ. '&ccw' ) THEN
                        VEX%SCA(J3)%WRAP_MODE(N_SCA_STN) = 3
                     END IF
! ---------------
                  ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) ==               &
     &                          'endscan;' ) THEN
                      VEX%SCA(J3)%N_STA   = N_SCA_STN
                      ISCA                = 1
                  END IF
!
! --------------- Done Handling the Schedule Block
!
                  IF ( (J3 .EQ. VEX%N_SCA) .AND. (ISCA .EQ. 1) ) THEN 
!
! ------------------ Sort the scans according to time epoch
!
                     CALL FOR_QSORT ( VEX%SCA, VEX%N_SCA,               &
     &                        SIZEOF(VEX%SCA(1)), VEX_COMPAR_SCA )
! ------------------
                     GOTO 410
                  END IF
!
! ------------ Handle the MODE BLOCK
!
               ELSE IF ( SECT_ID == 'MODE' ) THEN
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$MODE;' ) THEN
                     FL_MOD   = .TRUE.
                     J6       =  0
                     J7       =  0
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN
                       MODE_NAME =  BUF(J1)(IND(1,2):IND(2,2))
                       IE = INDEX ( MODE_NAME, ';' ) 
                       IF ( IE > 0 ) CALL CLRCH ( MODE_NAME(IE:) )
                  END IF
!
! --------------- Grab some of the Frequency details from the mode block
!
                  IF ( BUF(J1)(IND(1,2):IND(2,2)) == '$FREQ' ) THEN           
                     J6    = J6 + 1
                     VEX%FRQ(J6)%FRQ_NAME = BUF(J1)(IND(1,3):IND(2,3))
                     VEX%FRQ(J6)%MODE     = MODE_NAME
                     VEX%FRQ(J6)%N_STA    = LIND - 3
!
! ------------------ Assign the Station IDs associated with the 
!                    particular Frequency
!                    N.B: - This assumes there are never any comments
!                           after the end of line deliminator, ';'.
!                         - It is also assumed that, in $MODE Block,
!                           the $FREQ are arranged in the same order as
!                           they come.
!
                     DO 420 I2 = 1, VEX%FRQ(J6)%N_STA
!
! --------------------- If this is the last Station ID
!
                        IF ( I2 .EQ. VEX%FRQ(J6)%N_STA )  THEN
                           CALL CLRCH ( VEX%FRQ(J6)%SITE_ID(I2) )
                           VEX%FRQ(J6)%SITE_ID(I2) =                    &
     &                     BUF(J1)(IND(1,I2+3):IND(2,I2+3)-1)      ! drop the ";"
                           CALL TRAN ( 12, VEX%FRQ(J6)%SITE_ID(I2),     &
     &                          VEX%FRQ(J6)%SITE_ID(I2) )
                        ELSE
                           CALL CLRCH ( VEX%FRQ(J6)%SITE_ID(I2) )
                           VEX%FRQ(J6)%SITE_ID(I2) =                    &
     &                      BUF(J1)(IND(1,I2+3):IND(2,I2+3))
                           CALL TRAN ( 12, VEX%FRQ(J6)%SITE_ID(I2),     &
     &                          VEX%FRQ(J6)%SITE_ID(I2) )
                        END IF
 420                 CONTINUE
                  END IF
!
! --------------- Grab some of the IF details from the mode block
!
                  IF ( BUF(J1)(IND(1,2):IND(2,2)) == '$IF' ) THEN           
                     J7    = J7 + 1
                     VEX%IFS(J7)%IF_NAME  = BUF(J1)(IND(1,3):IND(2,3))
                     VEX%IFS(J7)%MODE     = MODE_NAME
                     VEX%IFS(J7)%N_STA    = LIND - 3
!
! ------------------ Assign the Station IDs associated with the 
!                    particular IF
!                    N.B: - This assumes there are never any comments
!                           after the end of line deliminator, ';'.
!                         - It is also assumed that, in $MODE Block,
!                           the $IF are arranged in the same order as
!                           they come.
!
                     DO 421 I2 = 1, VEX%IFS(J7)%N_STA
!
! --------------------- If this is the last Station ID
!
                        IF ( I2 .EQ. VEX%IFS(J7)%N_STA )  THEN
                           CALL CLRCH ( VEX%IFS(J7)%SITE_ID(I2) )
                           VEX%IFS(J7)%SITE_ID(I2) =                    &
     &                     BUF(J1)(IND(1,I2+3):IND(2,I2+3)-1)      ! drop the ";"
                           CALL TRAN ( 12, VEX%IFS(J7)%SITE_ID(I2),     &
     &                          VEX%IFS(J7)%SITE_ID(I2) )
                        ELSE
                           CALL CLRCH ( VEX%IFS(J7)%SITE_ID(I2) )
                           VEX%IFS(J7)%SITE_ID(I2) =                    &
     &                         BUF(J1)(IND(1,I2+3):IND(2,I2+3))
                           CALL TRAN ( 12, VEX%IFS(J7)%SITE_ID(I2),     &
     &                          VEX%IFS(J7)%SITE_ID(I2) )
                        END IF
 421                 CONTINUE
                  END IF
!
! ------------ Handle the FREQ BLOCK
!
               ELSE IF ( SECT_ID == 'FREQ') THEN
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$FREQ;' ) THEN
                       FL_FRQ   = .TRUE.
                       J5       =  0
                       GOTO 410
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN 
                       J5          = J5 + 1
                       VEX%FRQ(J5)%N_CHA = 0
!
                       IF ( VEX%FRQ(J5)%FRQ_NAME .NE. BUF(J1)(IND(1,2):IND(2,2)-1) ) THEN
                            CALL ERR_LOG ( 8119, IUER, 'VEX_PARSER',        &
     &                          'Frequency name in $FREQ Block does not'// &
     &                          ' match the one declared from $MODE Block')
                            RETURN 
                       END IF
                  END IF
!
! --------------- Parse the frequency parameters to VEX__FRQ_TYPE
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'sample_rate') THEN
                       READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F15.6)',IOSTAT=IER ) VEX%FRQ(J5)%SAMPLE_RATE ! Mbps
                       VEX%FRQ(J5)%SAMPLE_RATE = 1.D6*VEX%FRQ(J5)%SAMPLE_RATE ! bps
                       READ ( UNIT=BUF(J1)(IND(1,5)+1:IND(1,5)+1), FMT='(I1)', IOSTAT=IER ) VEX%FRQ(J5)%BIT_SAMPLE
                  END IF
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'chan_def' ) THEN
                       VEX%FRQ(J5)%N_CHA   =  VEX%FRQ(J5)%N_CHA + 1
                       CALL EXWORD ( BUF(J1), MIND, LINDC, INDC, ":", IER )
!
! -------------------- Read the netside
!
                       STR = BUF(J1)(INDC(1,3):INDC(2,3))
                       CALL CHASHL ( STR )
                       VEX%FRQ(J5)%NET_SB(VEX%FRQ(J5)%N_CHA) = STR(1:1)
!
! -------------------- Read chan width and convert it from MHz to Hz
!
                       STR = BUF(J1)(INDC(1,4):INDC(2,4))
                       IP = INDEX ( STR, 'MHz' )
                       READ ( UNIT=STR(1:IP-1), FMT='(F15.6)', IOSTAT=IER ) VEX%FRQ(J5)%CHA_BW(VEX%FRQ(J5)%N_CHA)
                       IVAL_I8 = NINT( 1.D6*VEX%FRQ(J5)%CHA_BW(VEX%FRQ(J5)%N_CHA), KIND=8 )
                       VEX%FRQ(J5)%CHA_BW(VEX%FRQ(J5)%N_CHA) = IVAL_I8
!
! -------------------- Read Sky frequency and convert it from MHz to Hz
!
                       STR = BUF(J1)(INDC(1,2):INDC(2,2))
                       IP = INDEX ( STR, 'MHz' )
                       READ ( UNIT=STR(1:IP-1), FMT='(F15.6)', IOSTAT=IER ) VEX%FRQ(J5)%SKY_FRQ(VEX%FRQ(J5)%N_CHA)
                       IVAL_I8 = NINT(  1.D6*VEX%FRQ(J5)%SKY_FRQ(VEX%FRQ(J5)%N_CHA), KIND=8 )
                       VEX%FRQ(J5)%SKY_FRQ(VEX%FRQ(J5)%N_CHA) = IVAL_I8
                       IF ( VEX%FRQ(J5)%NET_SB(VEX%FRQ(J5)%N_CHA) == 'L' ) THEN
                            VEX%FRQ(J5)%SKY_FRQ(VEX%FRQ(J5)%N_CHA) = VEX%FRQ(J5)%SKY_FRQ(VEX%FRQ(J5)%N_CHA) - VEX%FRQ(J5)%CHA_BW(VEX%FRQ(J5)%N_CHA) 
                       END IF
!
                       STR = BUF(J1)(INDC(1,5):INDC(2,5))
                       CALL CHASHL ( STR )
                       VEX%FRQ(J5)%CHAN_ID(VEX%FRQ(J5)%N_CHA) = STR(2:)
                       IF ( LINDC .GE. 6 ) THEN
!
! ------------------------- Read the BBC ID. NB: this field can be omitted
!
                            STR = BUF(J1)(INDC(1,5):INDC(2,5))
                            CALL CHASHL ( STR )
                            VEX%FRQ(J5)%BBC_ID(VEX%FRQ(J5)%N_CHA) = STR(2:)
                          ELSE
                            VEX%FRQ(J5)%BBC_ID(VEX%FRQ(J5)%N_CHA) = '??'
                       END IF 
                       IF ( LINDC .GE. 7 ) THEN
!
! ------------------------- Read the phase cal extraction flag. NB: this field can be omitted
!
                            STR = BUF(J1)(INDC(1,5):INDC(2,5))
                            CALL CHASHL ( STR )
                            VEX%FRQ(J5)%PHASE_CAL_ID(VEX%FRQ(J5)%N_CHA) = STR(2:)
                          ELSE
                            VEX%FRQ(J5)%PHASE_CAL_ID(VEX%FRQ(J5)%N_CHA) = '??'
                       END IF 
                  END IF
!
! --------------- Done Handling the Frequency Block 
!           
                  IF ( J5 .EQ. VEX%N_FRQ) GOTO 410
!
! ------------ Handle the IF BLOCK
!                
               ELSE IF ( SECT_ID == 'IFS' ) THEN
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$IF;' ) THEN
                     FL_IFS   = .TRUE.
                     J8       =  0
                  END IF
! ---------------
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN 
                     J8        = J8 + 1
                     N_IF_DEF  = 0
                     VEX%IFS(J8)%N_SX_IF(1) = 0        ! S-Band counts
                     VEX%IFS(J8)%N_SX_IF(2) = 0        ! X-Band counts
! ------------------
                     IF ( VEX%IFS(J8)%IF_NAME ==                        &
     &                        BUF(J1)(IND(1,2):IND(2,2)-1) ) THEN
                        GO TO 450
                    ELSE
                        CALL ERR_LOG ( 8120, IUER, 'VEX_PARSER',        &
     &                       'IF name in $IF Block does not match '//   &
     &                       'the one declared from $MODE Block' )
                        RETURN 
                     END IF
 450                 CONTINUE
                  END IF
!#_______________________________________#!
!
! --------------- Parse the IF parameters to VEX__IFS_TYPE
! --------------- This assumes that the non-phase_cal related sections 
!                 are always defined. In the event that they are not
!                 defined, then the freq. spacing is set to 1MHz, while 
!                 the base freq. is set to 0Hz.
!
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'if_def' ) THEN
                     N_IF_DEF =  N_IF_DEF + 1
!
! ------------------ Is the phase cal base freq defined? If so, then 
!                    the 11th word would be "Hz".
!
                     IF ( BUF(J1)(IND(1,11):IND(2,11)) == 'Hz' ) THEN
! ---------------------
                        VEX%IFS(J8)%IF_ID(N_IF_DEF)    =                &
     &                      BUF(J1)(IND(1,2)+1:IND(2,2))      ! drop '&'
! ---------------------
                        VEX%IFS(J8)%PHY_NAME(N_IF_DEF) =                &
     &                      BUF(J1)(IND(1,3):IND(2,3))
! ---------------------
                        VEX%IFS(J8)%POL(N_IF_DEF)      =                &
     &                      BUF(J1)(IND(1,4):IND(2,4))
! ---------------------
                        READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)),         & 
     &                         FMT='(F15.6)', IOSTAT=IER )              &
     &                  VEX%IFS(J8)%TOTAL_LO(N_IF_DEF)           ! [MHz]
                        VEX%IFS(J8)%TOTAL_LO(N_IF_DEF)  =               &
     &                      1.D6*VEX%IFS(J8)%TOTAL_LO(N_IF_DEF)  ! Hz
!
! --------------------- Use the Total LO to count the S and X bands.
!
                        IF ((VEX%IFS(J8)%TOTAL_LO(N_IF_DEF) .GE. 2.1D9) &
     &                                         .AND.                    &
     &                      (VEX%IFS(J8)%TOTAL_LO(N_IF_DEF) .LE. 2.4D9))& 
     &                                          THEN
                           VEX%IFS(J8)%N_SX_IF(1) =                     &
     &                         VEX%IFS(J8)%N_SX_IF(1)  +  1
! ------------------------
                        ELSEIF                                          &
     &                     ((VEX%IFS(J8)%TOTAL_LO(N_IF_DEF) .GE. 7.9D9) &
     &                                         .AND.                    &
     &                      (VEX%IFS(J8)%TOTAL_LO(N_IF_DEF) .LE. 9.1D9))& 
     &                                          THEN
                           VEX%IFS(J8)%N_SX_IF(2) =                     &
     &                         VEX%IFS(J8)%N_SX_IF(2)  +  1
                        END IF
! ---------------------
                        VEX%IFS(J8)%NET_SB(N_IF_DEF)   =                &
     &                      BUF(J1)(IND(1,7):IND(2,7))
! ---------------------
                        READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)),         & 
     &                         FMT='(F15.6)', IOSTAT=IER )              &
     &                  VEX%IFS(J8)%P_CAL_FREQ_SPACE(N_IF_DEF)   ! [MHz]
                        VEX%IFS(J8)%P_CAL_FREQ_SPACE(N_IF_DEF)  =       &
     &                      1.D6*VEX%IFS(J8)%P_CAL_FREQ_SPACE(N_IF_DEF) ! Hz
! ---------------------
                        READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)),       & 
     &                         FMT='(F15.6)', IOSTAT=IER )              &
     &                  VEX%IFS(J8)%P_CAL_BASE_FREQ(N_IF_DEF)     ! [Hz]
!
! ------------------ If the phase cal base freq is not defined.
!
                     ELSE
! ---------------------
                        VEX%IFS(J8)%IF_ID(N_IF_DEF)    =                &
     &                      BUF(J1)(IND(1,2)+1:IND(2,2))      ! drop '&'
! ---------------------
                        VEX%IFS(J8)%PHY_NAME(N_IF_DEF) =                &
     &                      BUF(J1)(IND(1,3):IND(2,3))
! ---------------------
                        VEX%IFS(J8)%POL(N_IF_DEF)      =                &
     &                      BUF(J1)(IND(1,4):IND(2,4))
! ---------------------
                        READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)),         & 
     &                         FMT='(F15.6)', IOSTAT=IER )              &
     &                  VEX%IFS(J8)%TOTAL_LO(N_IF_DEF)           ! [MHz]
                        VEX%IFS(J8)%TOTAL_LO(N_IF_DEF)  =               &
     &                      1.D6*VEX%IFS(J8)%TOTAL_LO(N_IF_DEF)  ! [Hz]
!
! --------------------- Use the Total LO to count the S and X bands.
!
                        IF ((VEX%IFS(J8)%TOTAL_LO(N_IF_DEF) .GE. 2.1D9) &
     &                                         .AND.                    &
     &                      (VEX%IFS(J8)%TOTAL_LO(N_IF_DEF) .LE. 2.4D9))& 
     &                                          THEN
                           VEX%IFS(J8)%N_SX_IF(1) =                     &
     &                         VEX%IFS(J8)%N_SX_IF(1)  +  1
! ------------------------
                        ELSEIF                                          &
     &                     ((VEX%IFS(J8)%TOTAL_LO(N_IF_DEF) .GE. 7.9D9) &
     &                                         .AND.                    &
     &                      (VEX%IFS(J8)%TOTAL_LO(N_IF_DEF) .LE. 9.1D9))& 
     &                                          THEN
                           VEX%IFS(J8)%N_SX_IF(2) =                     &
     &                         VEX%IFS(J8)%N_SX_IF(2)  +  1
                        END IF
! ---------------------
                        VEX%IFS(J8)%NET_SB(N_IF_DEF)   =                &
     &                      BUF(J1)(IND(1,7):IND(2,7))
! ---------------------
                        VEX%IFS(J8)%P_CAL_FREQ_SPACE(N_IF_DEF) = 1.0D6  ! [Hz]
! ---------------------
                        VEX%IFS(J8)%P_CAL_BASE_FREQ(N_IF_DEF)  = 0.D0   ! [Hz]
                     END IF
                  END IF
!#_______________________________________#!
!
! --------------- Done Handling the IF Block 
!
                  IF ( J8 .EQ. N_IFS ) GOTO 410
! ---------------
               END IF
            ELSE
!
! ------------ If we are not in the block we would like to be in.
!
               GO TO 410
            END IF
 410     CONTINUE
!
!*****!  311     CONTINUE
!
 310  CONTINUE
!
 312  CONTINUE
!
! --- Sort the source and station lists alphabetically
!
!@@      CALL SORT_FAST_CH ( VEX%N_STA, VEX%C_STA )
!@@      CALL SORT_FAST_CH ( VEX%N_SOU, VEX%C_SOU )
!
! --- Check if the MJD was parsed correctly. If not, then use the 
!     the time stamps of the initial and final schedules.
!     N.B: The logic here is that, it is assumed that if MJD_START=0,
!          then MJD_STOP=0, UTC_START=0.D0, and UTC_STOP=0.D0, i.e. 
!          the rest are also undefined.
!
      IF ( VEX%MJD_START .EQ. 0 ) THEN
         VEX%MJD_START = VEX%SCA(1)%MJD
         VEX%MJD_STOP  = VEX%SCA(VEX%N_SCA)%MJD
         VEX%UTC_START = VEX%SCA(1)%UTC
         VEX%UTC_STOP  = VEX%SCA(VEX%N_SCA)%UTC
      END IF
!
! --- Sort the scans according to time epoch
!
!@@      CALL FOR_QSORT ( VEX%SCA, VEX%N_SCA, SIZEOF(VEX%SCA(1)),          &
!@@     &          VEX_COMPAR_SCA )
!
! --- Sort the sources structure according to source name
!
!@@      CALL FOR_QSORT ( VEX%SOU, VEX%N_SOU, SIZEOF(VEX%SOU(1)),          &
!@@     &          VEX_COMPAR_SOU )
!
! --- For the silent case 
!
      IF ( ISDR .EQ. 0 ) THEN
          WRITE (6,*) 'NO OUTPUT: VEX_PARSER running is silent mode '//  &
                 'for  '//FIL_VEX
          CALL VEX_CLEAN ( VEX )
         RETURN
      ENDIF
!
! --- For the Sites Case
!
      IF ( ISDR .EQ. 3 ) THEN
         WRITE (6,*) 'SITE NAME OUTPUT: VEX_PARSER running in '//       &
     &        'stat mode processing file: '//FIL_VEX
         WRITE (6,*) ' '
!
! ------ Clear everything except for the station block
!
         IF ( ASSOCIATED ( VEX%SOU ) ) DEALLOCATE ( VEX%SOU )
         IF ( ASSOCIATED ( VEX%SCA ) ) DEALLOCATE ( VEX%SCA )
         IF ( ASSOCIATED ( VEX%FRQ ) ) DEALLOCATE ( VEX%FRQ )
         CALL CLRCH ( VEX%REVISION ) 
         CALL CLRCH ( VEX%EXPER_NAME )
         CALL CLRCH ( VEX%CONTACT_NAME )
         VEX%N_SOU = 0
         VEX%N_SCA = 0
!
! ------ Leave only the site names in the station block
!
         DO 510 I2 = 1, VEX%N_STA
            WRITE (6,*) 'ISTA ', I2, ' Site_name: ', VEX%STA(I2)%SITE_NAME
            CALL CLRCH ( VEX%STA(I2)%ANT_NAME )
            CALL CLRCH ( VEX%STA(I2)%SITE_ID )
            CALL CLRCH ( VEX%STA(I2)%PROCEDURE_NAME_PREFIX )
            VEX%STA(I2)%SITE_POS(:)          = 0.D0
            VEX%STA(I2)%SITE_VEL(:)          = 0.D0
            VEX%STA(I2)%SITE_POS_EPOCH_MJD   = 0
            VEX%STA(I2)%TAPE_CHANGE          = 0
            VEX%STA(I2)%HEADSTACK_MOTION     = 0
            VEX%STA(I2)%NEW_SOURCE_COMMAND   = 0
            VEX%STA(I2)%NEW_TAPE_SETUP       = 0
            VEX%STA(I2)%SETUP_ALWAYS         = 0
            VEX%STA(I2)%PARITY_CHECK         = 0
            VEX%STA(I2)%TAPE_PREPASS         = 0
            VEX%STA(I2)%PREOB_CAL            = 0
            VEX%STA(I2)%MIDOB_CAL            = 0
            VEX%STA(I2)%POSTOB_CAL           = 0
 510     CONTINUE
         RETURN
      END IF
!
! --- Block Parsing Errors
!
      IF ( ISDR .EQ. 5 ) THEN
         IF ( .NOT. FL_GLO ) THEN
            CALL ERR_LOG ( 8121, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $GLOBAL; block in, '    &
     &           //FIL_VEX//                                            &
     &           ' ,hence could not parse any parameter from it.' )
            RETURN 
         END IF
!
         IF ( .NOT. FL_EXP ) THEN
            CALL ERR_LOG ( 8122, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $EXPER; block in, '     &
     &           //FIL_VEX//                                            &
     &           ' ,hence could not parse any parameter from it.' )
            RETURN
         END IF
!
         IF ( .NOT. FL_PRO ) THEN
            CALL ERR_LOG ( 8123, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $PROCEDURES; block in,' &
     &           //FIL_VEX//                                            &
     &           ' ,hence could not parse any parameters from it.' )
            RETURN
         END IF
!
         IF ( .NOT. FL_SOU ) THEN
            CALL ERR_LOG ( 8124, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $SOURCE; block in, '    &
     &           //FIL_VEX//                                            &
     &           ' ,hence could not parse any parameter from it.' )
            RETURN 
         END IF
!
         IF ( .NOT. FL_STA ) THEN
            CALL ERR_LOG ( 8125, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $STATION block in, '    &
     &           //FIL_VEX//                                            &
     &          ' ,hence could not parse any parameter from it.' )
            RETURN 
         END IF
!
         IF ( .NOT. FL_SCA ) THEN
            CALL ERR_LOG ( 8126, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $SCHEDULE; block in, '  &
     &           //FIL_VEX//                                            &
     &           ' ,hence could not parse any parameters from it.' )
            RETURN 
         END IF
!
         IF ( .NOT. FL_STE ) THEN
            CALL ERR_LOG ( 8127, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $SITE; block in, '      &
     &           //FIL_VEX//                                            &
     &           ' ,hence could not parse any parameter from it.' )
            RETURN 
         END IF
!****!!
!****!         IF ( .NOT. FL_ANT ) THEN
!****!            CALL ERR_LOG ( 8128, IUER, 'VEX_PARSER', 'Wrong File '      &
!****!     &           //'Format: Did not encounter a $ANTENNA; block in, '   &
!****!     &           //FIL_VEX//                                            &
!****!     &           ' ,hence could not parse any parameter from it.' )
!****!            RETURN 
!****!         END IF
!
         IF ( .NOT. FL_FRQ ) THEN
            CALL ERR_LOG ( 8129, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $FREQ; block in, '      &
     &           //FIL_VEX//                                            &
     &           ' ,hence could not parse any parameter from it.' )
            RETURN 
         END IF
!
         IF ( .NOT. FL_IFS ) THEN
            CALL ERR_LOG ( 8130, IUER, 'VEX_PARSER', 'Wrong File '      &
     &           //'Format: Did not encounter a $IF; block in, '        &
     &           //FIL_VEX//                                            &
     &           ' ,hence could not parse any parameter from it.' )
            RETURN 
         END IF
!
!****!         IF ( .NOT. FL_SN ) THEN
!****!            CALL ERR_LOG ( 8131, IUER, 'VEX_PARSE', 'WARNING: All '     &
!****!     &           //'Scans are to begin with a unique scan number, '     &
!****!     &           //'"scan #####". '//FIL_VEX//' is missing this.' ) 
!****!                RETURN 
!****!         END IF
      END IF
!
!****! %%%%%%%%%%%%% FOR ISDR = 2 and 4 %%%%%%%%%%%%%%%%%% TO BE CONT...
!
! --- (Selected) Parameter Parsing errors !!!
!
!***!%%%%%%%%%%%%%%%%% MUTE FOR NOW %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!***!      IF ( FL_RA ) THEN
!***!           CALL ERR_LOG ( 8132, IUER, 'VEX_PARSE', 'WARNING: Did not '  &
!***!     &          //'find the right ascension variable, "ra", to parse.'  &
!***!     &          //CHAR(10)//'N.B: VEX statements are case relevant. ' ) 
!***!           RETURN 
!***!      END IF
!***!!
!***!      IF ( FL_DEC ) THEN
!***!           CALL ERR_LOG ( 8133, IUER, 'VEX_PARSE', 'Did not find the '  &
!***!     &          //'declination variable, "dec" to parse.'//CHAR(10)     &
!***!     &          //'N.B: VEX statements are case relevant.' ) 
!***!           RETURN 
!***!      END IF
!***!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!***!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!***!      IF ( .NOT. FL_TC ) THEN
!***!           CALL ERR_LOG ( 8134, IUER, 'VEX_PARSE', 'Did not find the '  &
!***!     &          //'REQUIRED $PROCEDURES; variable "tape_change". '      &
!***!     &          //CHAR(10)//'N.B.: VEX statements are case relevant.' ) 
!***!           RETURN 
!***!      END IF
!***!      IF ( .NOT. FL_HSM ) THEN
!***!           CALL ERR_LOG ( 8135, IUER, 'VEX_PARSE', 'Did not find the '  &
!***!     &          //'REQUIRED $PROCEDURES; variable "headstack_motion". ' &
!***!     &         //CHAR(10)//'N.B.: VEX statements are case relevant.' ) 
!***!           RETURN 
!***!      END IF
!***!      IF ( .NOT. FL_NSC ) THEN
!***!           CALL ERR_LOG ( 8136, IUER, 'VEX_PARSE', 'Did not find the '  &
!***!     &          //'REQUIRED $PROCEDURES; variable "new_source_command"' &
!***!     &         //CHAR(10)//'N.B.: VEX statements are case relevant.' ) 
!***!           RETURN 
!***!      END IF
!***!!
!***!      IF ( .NOT. FL_NTS ) THEN
!***!           CALL ERR_LOG ( 8137, IUER, 'VEX_PARSE', 'Did not find the '  &
!***!     &          //'REQUIRED $PROCEDURES; variable "new_tape_setup". '   &
!***!     &         //CHAR(10)//'N.B.: VEX statements are case relevant.' ) 
!***!           RETURN 
!***!      END IF
!***!!
!***!      IF ( .NOT. FL_IAU ) THEN
!***!           VEX%SOU(:)%IAU_NAME = CHAR(0)
!***!           WRITE (6, *) 'The VEX_file was missing IAU names, hence'//   &
!***!     &           ' output is set to null'
!***!      END IF
!***!!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VEX_PARSER   !#!#
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE VEX_CLEAN ( VEX )
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      TYPE ( VEX_TYPE ) :: VEX
!
      IF ( ASSOCIATED ( VEX%STA ) ) DEALLOCATE ( VEX%STA )
      IF ( ASSOCIATED ( VEX%SOU ) ) DEALLOCATE ( VEX%SOU )
      IF ( ASSOCIATED ( VEX%STA ) ) DEALLOCATE ( VEX%STA )
      IF ( ASSOCIATED ( VEX%FRQ ) ) DEALLOCATE ( VEX%FRQ )
      IF ( ASSOCIATED ( VEX%IFS ) ) DEALLOCATE ( VEX%IFS )
!
      CALL CLRCH ( VEX%REVISION ) 
      CALL CLRCH ( VEX%EXPER_NAME )
      CALL CLRCH ( VEX%CONTACT_NAME )
      VEX%N_STA = 0 
      VEX%N_SOU = 0
      VEX%N_SCA = 0
      VEX%N_FRQ = 0
!***!      VEX%N_IFS  = 0
      VEX%STATUS = VEX__INIT      
!
      RETURN
      END  !#!
!
!----------------------------------------------------------------------------
!
      SUBROUTINE VEX_COUNT ( N_STA, N_SOU, N_SCA, N_FRQ, N_IFS,         &
     &                       FIL_VEX, IUER )
! ***************************************************************************
! *                                                                         *
! *   Routine VEX_COUNT counts the number of: stations, sources and         *
! *   scans in a VLBI Experiment schedule file.                             *
! *                                                                         *
! *   INPUT:                                                                *
! *            FIL_VEX   =  VEX file                     { CHAR }           *
! *                         For documentation on format specification       *
! *                         visit vlbi.org/vlbi-standards/vex/              *
! *                                                                         *
! *            IUER      =  Error Handler                { INT, OPT }       *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            N_STA     =  Number of Stations           { INT }            *
! *            N_SOU     =  Number of Sources            { INT }            *
! *            N_SCA     =  Number of Schedules (Scans)  { INT }            * 
! *            N_FRQ     =  Number of Frequencies        { INT }            *
! *            N_IFS     =  Number of IF's               { INT }            *
! *                                                                         *
! *  ### 31-MAR-2020  VEX_COUNT   v1.0 (c)    N. Habana    31-MAR-2020 ###  *
! *                                                                         *
! *  ### 08-JUN-2020  VEX_COUNT   v2.0 (c)    N. Habana    08-JUN-2020 ###  *
! *      Included the $FREQ Block.                                          * 
! *                                                                         *
! *  ### 31-OCT-2020  VEX_COUNT   v3.0 (c)    N. Habana    31-OCT-2020 ###  *
! *      Included the $IF Block.                                            * 
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INTEGER*4  N_STA, N_SOU, N_SCA, N_FRQ, N_IFS, IUER
      CHARACTER  FIL_VEX*(*)                   ! Vex File Name
      CHARACTER  DELIM*6                       ! Space Delimiter
      INTEGER*4  MP, MIND, MAXL_STRING         ! Max No. of lines, Max. Index 
      PARAMETER  ( MAXL_STRING=4096 )          ! Maximum String length
      PARAMETER  ( MP = 128*1024 )             
      PARAMETER  ( MIND = 128 )                
      CHARACTER  BUF(MP)*(MAXL_STRING), SECT_ID*32            ! Read File, File Section
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'="'//"'" ) ! Null, Space, Tab, Equal-sign and quotes
      INTEGER*4  J1, J2, NP, LIND, IND(2,MIND), IER
      LOGICAL*1  FL_STA, FL_SOU, FL_SCA, FL_FRQ, FL_IFS       ! Flags
      INTEGER*4, EXTERNAL :: ILEN
!
! --- Read the Vex File
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_VEX, MP, BUF, NP, IER ) 
      IF ( IER .NE. 0 ) THEN                      ! i.e. if there is error
           CALL ERR_LOG ( 8139, IUER, 'VEX_COUNT', 'Error in reading '  &
     &        // 'input vex file '//FIL_VEX )
           RETURN 
      END IF
      
!
! --- Go through the file, but only focusing on counting the number of occurences
!     required for output.
!
      SECT_ID = '??'
      FL_STA = .FALSE.
      FL_SOU = .FALSE.
      FL_SCA = .FALSE.
      FL_FRQ = .FALSE.
      FL_IFS = .FALSE.
!
      DO 410 J1=1,NP
         IF ( ILEN(BUF(J1)) == 0 ) GOTO 410     ! if empty line
         IF ( BUF(J1)(1:1) == '*' ) GOTO 410    ! if line is a comment
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
! ------
         IF ( BUF(J1)(1:1) == '$' ) THEN
              SECT_ID = '??'
         END IF
!
! ------ Count the number of Stations
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$STATION;' ) THEN
              SECT_ID  = 'STATION'
              FL_STA   = .TRUE.
              N_STA    =  0
         END IF
!-------
         IF ( SECT_ID == 'STATION' ) THEN
              IF ( BUF(J1)(IND(1,1):IND(2,1) ) == 'def' ) THEN
                   N_STA = N_STA + 1
              END IF
         END IF
!
! ------ Count the number of Sources
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SOURCE;' ) THEN
              SECT_ID  = 'SOURCE'
              FL_SOU   = .TRUE.
              N_SOU    =  0
         END IF
! ------
         IF ( SECT_ID == 'SOURCE' ) THEN
              IF (BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) N_SOU = N_SOU + 1
         END IF
!
! ------ Count the number of Scans 
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SCHED;' ) THEN
              SECT_ID  = 'SCHED'
              FL_SCA   = .TRUE.
              N_SCA    =  0
         END IF
! ------
         IF ( SECT_ID == 'SCHED' ) THEN
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'scan' ) THEN    ! start a scan
                 N_SCA  = N_SCA + 1
            END IF
         END IF
!
! ------ Count the number of frequencies 
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$FREQ;' ) THEN
              SECT_ID  = 'FREQ'
              FL_FRQ   = .TRUE.
              N_FRQ    =  0
         END IF
! ------
         IF ( SECT_ID == 'FREQ' ) THEN
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN         
                 N_FRQ  = N_FRQ + 1
            END IF
         END IF
!
! ------ Count the number of IF's 
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$IF;' ) THEN
              SECT_ID  = 'IFS'
              FL_IFS   = .TRUE.
              N_IFS    =  0
         END IF
! ------
         IF ( SECT_ID == 'IFS' ) THEN
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN         
                 N_IFS  = N_IFS + 1
            END IF
         END IF

 410  CONTINUE
! 
      IF ( .NOT. FL_STA ) THEN
           CALL ERR_LOG ( 8140, IUER, 'VEX_COUNT', 'Wrong File Format: '&
     &          //'Did not encounter a $STATION; block, hence no '      &
     &         // 'stations could be counted in, '//FIL_VEX//'.' )
           RETURN 
      END IF
! ---
      IF ( .NOT. FL_SOU ) THEN
           CALL ERR_LOG ( 8141, IUER, 'VEX_COUNT', 'Wrong File Format: '&
     &          //'Did not encounter a $SOURCE; block, hence no sources'&
     &         // ' could be counted in, '//FIL_VEX//'.' )
           RETURN 
      END IF
! ---
      IF ( .NOT. FL_SCA ) THEN
           CALL ERR_LOG ( 8142, IUER, 'VEX_COUNT', 'Wrong File Format: '&
     &          //'Did not encounter a $SCHED; block, hence no scans '  &
     &         // 'could be counted in, '//FIL_VEX//'.' )
           RETURN 
      END IF
! ---
      IF ( .NOT. FL_FRQ ) THEN
           CALL ERR_LOG ( 8143, IUER, 'VEX_COUNT', 'Wrong File Format: '&
     &          //'Did not encounter a $FREQ; block, hence no '         &
     &         // 'frequencies could be counted in, '//FIL_VEX//'.' )
           RETURN 
      END IF
! ---
      IF ( .NOT. FL_IFS ) THEN
           N_IFS = 0
           WRITE (6,*) 'VEX_PARSER: Warning: No IF section was found '// &
     &                 'in vex file '//TRIM(FIL_VEX)
      END IF
!
      CALL ERR_LOG ( 0, IUER)
      RETURN
      END SUBROUTINE VEX_COUNT  !#!#
