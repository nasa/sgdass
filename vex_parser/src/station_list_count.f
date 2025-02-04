      SUBROUTINE  STATION_LIST_COUNT( FIL_STA, N_STA, IUER )
!     
! ***************************************************************************
! *                                                                         *
! *   Subroutine STATION_LIST_COUNT counts the number of stations in a      *
! *   station list file.                                                    *
! *                                                                         *
! *   INPUT:                                                                *
! *            FIL_STA   =  Station List File         { CHAR }              *
! *                         N.B:  This file was manually edited, hence not  *
! *                               subject to international standardization  *
! *                               and the parsing is based on out format    *
! *                               preferences.                              *
! *                                                                         *
! *            IUER      =  Error Handler             { INT, OPT }          *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                         Default, IUER = -1                              *
! *                                                                         *
! *   OUTPUT:                                                               *
! *           N_STA      =  No. of stations           { INT }               *
! *                                                                         *
! * ### 15-SEP-2020 STATION_LIST_COUNT  v1.0 (c)  N. Habana 15-SEP-2020 ### *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'stp_tsys.i'
      CHARACTER  FIL_STA*(*)
      INTEGER*4  N_STA, IUER, IER, J1
      CHARACTER  DELIM*4                ! Deliminator
      INTEGER*4  MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4  MAXL_STRING                   ! Max. String length
      PARAMETER  ( MAXL_STRING=256 )           
      PARAMETER  ( MP = 128*1024 )             
      PARAMETER  ( MIND = 128 )                 
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) )      ! Null, Space, Tab
      CHARACTER  BUF(MP)*(MAXL_STRING)                        ! Read File
      INTEGER*4  NP, LIND, IND(2,MIND), LN
      INTEGER*4, EXTERNAL :: ILEN
!     
! --- Reading the station list file to variable, BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_STA, MP, BUF, NP, IER ) 
      IF ( IER .NE. 0 ) THEN                       ! i.e. if there is error
           CALL ERR_LOG ( 1091, IUER, 'STATION_LIST_COUNT',             &
     &          'Error in reading station lists from '//FIL_STA )                  
           RETURN 
      END IF
!
! --- Initialise count
!
      N_STA  =  0
! ---
      DO 410 J1 = 1, NP
!
! ------ Bypass empty lines
!
         IF ( ILEN(BUF(J1)) == 0 ) THEN
            GO TO 410
!
! ------ Bypass comment
!
         ELSE IF ( BUF(J1)(1:1) == '#' ) THEN
            GO TO 410
!
! ------ You're looking at a station name
!
         ELSE
            N_STA  =  N_STA + 1
         END IF
         
 410  CONTINUE
!
      RETURN
      END SUBROUTINE !#!
!
! -------------------------------------------------------------------------
!
      SUBROUTINE  STATION_LIST( FIL_STA, N_STA, STA_LIST, IUER )
!     
! ***************************************************************************
! *                                                                         *
! *   Subroutine STATION_LIST lists the  stations in a station list file.   *
! *                                                                         *
! *   INPUT:                                                                *
! *            FIL_STA   =  Station List File         { CHAR }              *
! *                         N.B:  This file was manually edited, hence not  *
! *                               subject to international standardization  *
! *                               and the parsing is based on out format    *
! *                               preferences.                              *
! *                                                                         *
! *            N_STA     =  No. of Stns               { INT }               *
! *                                                                         *
! *            IUER      =  Error Handler             { INT, OPT }          *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                         Default, IUER = -1                              *
! *                                                                         *
! *   OUTPUT:                                                               *
! *           STA_LIST   = Station List               { CHAR }              *
! *                                                                         *
! * ### 15-SEP-2020 STATION_LIST_COUNT  v1.0 (c)  N. Habana 15-SEP-2020 ### *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'stp_tsys.i'
      CHARACTER  FIL_STA*(*)
      INTEGER*4  N_STA, IUER, IER, J1
      CHARACTER  STA_LIST(N_STA)*16
      CHARACTER  DELIM*4        ! Deliminator
      INTEGER*4  MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4  MAXL_STRING                   ! Max. String length
      PARAMETER  ( MAXL_STRING=256 )           
      PARAMETER  ( MP = 128*1024 )             
      PARAMETER  ( MIND = 128 )                 
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) )      ! Null, Space, Tab
      CHARACTER  BUF(MP)*(MAXL_STRING)                        ! Read File
      INTEGER*4  NP, LIND, IND(2,MIND), LN
      INTEGER*4  CNT
      INTEGER*4, EXTERNAL :: ILEN
!     
! --- Reading the station list file to variable, BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_STA, MP, BUF, NP, IER ) 
      IF ( IER .NE. 0 ) THEN                       ! i.e. if there is error
           CALL ERR_LOG ( 1091, IUER, 'STATION_LIST_COUNT',             &
     &          'Error in reading station lists from '//FIL_STA )                  
           RETURN 
      END IF
!
! --- Initialise count
!
      CNT  =  0
! ---
      DO 410 J1 = 1, NP
!
! ------ Bypass empty lines
!
         IF ( ILEN(BUF(J1)) == 0 ) THEN
            GO TO 410
!
! ------ Bypass comment
!
         ELSE IF ( BUF(J1)(1:1) == '#' ) THEN
            GO TO 410
!
! ------ You're looking at a station name
!
         ELSE
            CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
            CNT  =  CNT + 1
            STA_LIST(CNT) = BUF(J1)(IND(1,1):IND(2,1))
         END IF         
 410  CONTINUE
!
      RETURN
      END SUBROUTINE  !#!#!

