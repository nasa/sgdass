      SUBROUTINE GET_STA_ID ( FIL_STA_LST, STA_NAM, STA_ID, IUER )
!
! ********************************************************************************
! *                                                                              *
! *   Routine GET_STA_ID                                                         *
! *                                                                              *
! *   Get the station ID given the station name.                                 *
! *                                                                              *
! *   INPUT:                                                                     *
! *          FIL_STA_LST   =  A priori file with station names   { CHAR }        *
! *                                                                              *
! *          STA_NAM       =  IVS Station name                   { CHAR }        *
! *                                                                              *
! *          IUER          =  Error Handler                      { INT*4, OPT }  *
! *                           If IUER=0 no error message will be printed,  even  *
! *                           in the event of an error. However, for other       *
! *                           possible values, i.e. IUER=-1,-2, & -3, the error  *
! *                           message will print to screen. For the latter case, *
! *                           i.e., IUER = -3, after printing the program will   *
! *                           terminate. Default, IUER = -1                      *
! *                                                                              *
! *   OUTPUT:                                                                    *
! *         STA_ID         =  Correlator Station name            { CHAR }        *
! *                                                                              *
! *  ### 02-OCT-2023    GET_STA_ID     v1.0 (c)    N. Habana   02-OCT-2023 ###   *
! *                                                                              *
! ********************************************************************************
!
      IMPLICIT   NONE
      CHARACTER  FIL_STA_LST*128, STA_NAM*16, STA_ID*8
      INTEGER*4  IUER, IER
      CHARACTER  DELIM*3
      INTEGER*4  MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4  MAXL_STRING                   ! Max. String length
      PARAMETER  ( MAXL_STRING = 512 )           
      PARAMETER  ( MP = 128*1024 )             
      PARAMETER  ( MIND = 512 )                 
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) ) ! Null, Space, Tab
      CHARACTER  BUF(MP)*(MAXL_STRING)                   ! Read File
      LOGICAL*1  LEX
      INTEGER*4  NP, LIND, IND(2,MIND), LN, IFLAG
      INTEGER*4  J0, J1, J2, J3
      INTEGER*4, EXTERNAL :: ILEN
!     
! --- Check if the input file exists
!
      INQUIRE ( FILE=FIL_STA_LST, EXIST=LEX )
      IF ( .NOT. LEX ) THEN ! 
         IUER = -1
         CALL ERR_LOG ( 5901, IUER, 'GET_STA_ID',                       &
     &           'Cannot find file '//TRIM(FIL_STA_LST) )
         RETURN
      END IF ! lex
!
! --- If the file exists read it to buffer, BUF
!
      IUER = -1
      CALL RD_TEXT  ( FIL_STA_LST, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 5902, IUER, 'GET_STA_ID',                       &
     &           'Error reading input station list file '//FIL_STA_LST )
         RETURN
      END IF
!
! --- Is this a station list file?
!           
      IF ( BUF(1) .NE.                                                  &
     &     "# STATION-NAMES.  Format version of 2006.01.06" ) THEN
         IUER = -1
         CALL ERR_LOG ( 5903, IUER, 'GET_STA_ID',                       &
     &           TRIM(FIL_STA_LST)//' is not a station list file' )
         RETURN
      END IF
!
! --- Correct for possible name mix ups.
!
      IF ( STA_NAM == "MGO12M" ) STA_NAM = "MACGO12M"
!
! --- Go through file
!
      IFLAG = 0
      
      DO 410 J1 = 1, NP
! ------
         IF ( ILEN(BUF(J1)) == 0 )  GOTO 410            ! Bypass empty lines
! ------
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410            ! Bypass comment lines.
!
! ------ Extract words from the read line
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! ------ Is this the station we are interested in?
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == STA_NAM ) THEN
            IFLAG = 1
            STA_ID = BUF(J1)(IND(1,1):IND(2,1))
            RETURN
         END IF
 410  CONTINUE
      
      
      IF ( IFLAG == 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 5904, IUER, 'GET_STA_ID',                       &
     &           TRIM(STA_NAM)//' is not listed in file '//             &
     &           TRIM(FIL_STA_LST) )
         RETURN
      END IF
! ---
      CALL ERR_LOG ( 0, IUER)
      RETURN
      END SUBROUTINE !#! GET_STA_ID 1
!
! ------------------------------------------------------------------------------- 
!
      SUBROUTINE GET_STA_NAM ( FIL_STA_LST, STA_NAM, STA_ID, IUER )
!
! ********************************************************************************
! *                                                                              *
! *   Routine GET_STA_NAM                                                        *
! *                                                                              *
! *   Get the station name given the station ID.                                 *
! *                                                                              *
! *   INPUT:                                                                     *
! *          FIL_STA_LST   =  A priori file with station names   { CHAR }        *
! *                                                                              *
! *          STA_ID         =  Correlator Station name            { CHAR }        *
! *                                                                              *
! *          IUER          =  Error Handler                      { INT*4, OPT }  *
! *                           If IUER=0 no error message will be printed,  even  *
! *                           in the event of an error. However, for other       *
! *                           possible values, i.e. IUER=-1,-2, & -3, the error  *
! *                           message will print to screen. For the latter case, *
! *                           i.e., IUER = -3, after printing the program will   *
! *                           terminate. Default, IUER = -1                      *
! *                                                                              *
! *   OUTPUT:                                                                    *
! *          STA_NAM        =  IVS Station name                   { CHAR }        *
! *                                                                              *
! *  ### 02-OCT-2023    GET_STA_NAM     v1.0 (c)    N. Habana   02-OCT-2023 ###   *
! *                                                                              *
! ********************************************************************************
!
      IMPLICIT   NONE
      CHARACTER  FIL_STA_LST*128, STA_NAM*16, STA_ID*8
      INTEGER*4  IUER, IER
      CHARACTER  DELIM*3
      INTEGER*4  MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4  MAXL_STRING                   ! Max. String length
      PARAMETER  ( MAXL_STRING = 512 )           
      PARAMETER  ( MP = 128*1024 )             
      PARAMETER  ( MIND = 512 )                 
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) ) ! Null, Space, Tab
      CHARACTER  BUF(MP)*(MAXL_STRING)                   ! Read File
      LOGICAL*1  LEX
      INTEGER*4  NP, LIND, IND(2,MIND), LN, IFLAG
      INTEGER*4  J0, J1, J2, J3
      INTEGER*4, EXTERNAL :: ILEN

!     
! --- Check if the input file exists
!
      INQUIRE ( FILE=FIL_STA_LST, EXIST=LEX )
      IF ( .NOT. LEX ) THEN ! 
         IUER = -1
         CALL ERR_LOG ( 5801, IUER, 'GET_STA_NAM',                       &
     &           'Cannot find file '//TRIM(FIL_STA_LST) )
         RETURN
      END IF ! lex
!
! --- If the file exists read it to buffer, BUF
!
      IUER = -1
      CALL RD_TEXT  ( FIL_STA_LST, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 5802, IUER, 'GET_STA_NAM',                       &
     &           'Error reading input station list file '//FIL_STA_LST )
         RETURN
      END IF
!
! --- Is this a station list file?
!
      IF ( BUF(1) .NE.                                                  &
     &     "# STATION-NAMES.  Format version of 2006.01.06" ) THEN
         IUER = -1
         CALL ERR_LOG ( 5803, IUER, 'GET_STA_NAM',                       &
     &           TRIM(FIL_STA_LST)//' is not a station list file' )
         RETURN
      END IF      
!     
! --- Go through file
!
      IFLAG = 0
      
      DO 410 J1 = 1, NP
! ------
         IF ( ILEN(BUF(J1)) == 0 )  GOTO 410            ! Bypass empty lines
! ------
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410            ! Bypass comment lines.
!
! ------ Extract words from the read line
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! ------ Is this the station we are interested in?
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == STA_ID ) THEN
            IFLAG = 1
            STA_NAM = BUF(J1)(IND(1,2):IND(2,2))
            RETURN
         END IF
 410  CONTINUE
      
      
      IF ( IFLAG == 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 5804, IUER, 'GET_STA_NAM',                       &
     &           TRIM(STA_ID)//' is not listed in file '//               &
     &           TRIM(FIL_STA_LST) )
         RETURN
      END IF
! ---
      CALL ERR_LOG ( 0, IUER)
      RETURN
      END SUBROUTINE !#!#! GET_STA_NAM 2
