      SUBROUTINE VEX_FLAGS ( FLG, FIL_VEX, IUER )
! 
! ***************************************************************************
! *                                                                         *
! *   Routine VEX_FLAG checks for the available blocks in FIL_VEX           *
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
! *            VEX_FLAG  =  Parsed Object                { DERIVED TYPE }   *
! *                         For more on the parsed data, see vex.i, and     *
! *                         edit it accordingly to include more data        *
! *                         blocks.                                         *
! *                                                                         *
! *  ###  11-SEP-2020  VEX_FLAGS   v1.0 (c)  N. Habana  11-SEP-2020   ###   *
! *  ###  03-NOV-2020  VEX_FLAGS   v2.0 (c)  N. Habana  03-NOV-2020   ###   *
! *    -  Included the $IF block                                            *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      TYPE ( VEX__FLG_TYPE ) :: FLG
      CHARACTER  FIL_VEX*(*)
      CHARACTER  DELIM*5
      INTEGER*4  MP, MIND
      INTEGER*4  MAXL_STRING
      INTEGER*4  N_SECTS
      PARAMETER  ( N_SECTS = 10 )
      PARAMETER  ( MAXL_STRING = 1024 )           
      PARAMETER  ( MP = 128*1024 )             
      PARAMETER  ( MIND = 512 )                 
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'='//':' ) ! Null, Space, Tab, Equal-sign, Colon
      CHARACTER  BUF(MP)*(MAXL_STRING), SECT_ID*32
      CHARACTER  SECTS(N_SECTS)*32
      CHARACTER  STR*12
      REAL*8     SCAN_STP
      INTEGER*4  IUER
      INTEGER*4  J0, J1
      INTEGER*4  I2
      INTEGER*4  NP, LIND, IND(2,MIND), IER, LN
      LOGICAL*1  SECTS_LOOPING
      INTEGER*4, EXTERNAL :: ILEN
!
! --- Reading the Vex File to variable, BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_VEX, MP, BUF, NP, IER ) 
      IF ( IER .NE. 0 ) THEN                       ! i.e. if there is error
           CALL ERR_LOG ( 2001, IUER, 'VEX_FLAG', 'Error in reading ' &
     &          //'input vex file '//FIL_VEX )                  
           RETURN 
      END IF
!
! --- Is this a VEX File?
!
      IF ( BUF(1)(1:7) .NE. 'VEX_rev' ) THEN
           CALL ERR_LOG ( 2002, IUER, 'VEX_FLAG', 'FORMAT ERROR: '//  &
     &          'VEX Files should always start with "VEX_rev" not '//   &
     &         '"'//BUF(1)(1:7)//'"' )
           RETURN
      END IF
!
! --- Initialise the flags
!
      FLG%GLO = .FALSE.  ! Global block 
      FLG%EXP = .FALSE.  ! Experiment block
      FLG%PRO = .FALSE.  ! Procedures block
      FLG%SOU = .FALSE.  ! Sources block
      FLG%SCA = .FALSE.  ! Schedule block
      FLG%STA = .FALSE.  ! Station block
      FLG%STE = .FALSE.  ! Site block
      FLG%FRQ = .FALSE.  ! Frequency block
      FLG%MOD = .FALSE.  ! Mode Block
      FLG%IFS = .FALSE.  ! IF block
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
! --- Outer loop to define blocks
!
      DO 310 J0 = 1, N_SECTS
!
! ------ Inner Loop to collect Blocks of interest
!
         SECT_ID = '??'
! ------
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
               SECT_ID = BUF(J1)(IND(1,1)+1:IND(2,1)-1)
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
                  FLG%GLO  = .TRUE.
!
! ------------ Handle the EXPERIMENT BLOCK
!
               ELSE IF ( SECT_ID == 'EXPER' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$EXPER;' ) THEN
                     FLG%EXP  = .TRUE.
                  END IF
!
! ------------ Handle the SITE BLOCK
!
               ELSE IF (SECT_ID == 'SITE' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SITE;' ) THEN
                     FLG%STE   = .TRUE.
                  END IF
!
! ------------ Handle the STATION BLOCK
!
               ELSE IF ( SECT_ID == 'STATION' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$STATION;' ) THEN
                     FLG%STA   = .TRUE.
                  END IF
!
! ------------ Handle the PROCEDURES BLOCK
!
               ELSE IF ( SECT_ID == 'PROCEDURES' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$PROCEDURES;' .OR.&
     &                 BUF(J1)(IND(1,1):IND(2,1)) == '$PROCS' ) THEN
                     FLG%PRO     = .TRUE.
                  END IF
!
! ------------ Handle the SOURCE BLOCK
!
               ELSE IF ( SECT_ID == 'SOURCE' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SOURCE;' ) THEN
                     FLG%SOU  = .TRUE.
                  END IF
!
! ------------ Handle the SCHEDULE BLOCK
!
               ELSE IF ( SECT_ID == 'SCHED' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SCHED;' ) THEN
                     FLG%SCA   = .TRUE.
                  END IF
!
! ------------ Handle the MODE BLOCK
!
               ELSE IF ( SECT_ID == 'MODE' ) THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$MODE;' ) THEN
                     FLG%MOD   = .TRUE.
                  END IF
!
! ------------ Handle the FREQ BLOCK
!
               ELSE IF ( SECT_ID == 'FREQ') THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$FREQ;' ) THEN
                     FLG%FRQ   = .TRUE.
                  END IF
!
! ------------ Handle the IFS BLOCK
!
               ELSE IF ( SECT_ID == 'IFS') THEN
                  IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$IF;' ) THEN
                     FLG%IFS   = .TRUE.
                  END IF
!
               END IF
            ELSE
!
! ------------ If we are not in the block we would like to be in.
!
               GO TO 410
            END IF
 410     CONTINUE
 310  CONTINUE

      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  !VEX_FLAG   !#!#
