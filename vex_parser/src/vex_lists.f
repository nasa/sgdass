      SUBROUTINE VEX_LIST ( STA_ID_LIST, STA_LIST, SOU_LIST, ANT_LIST,  &
     &              FIL_VEX, IUER ) 
! ***************************************************************************
! *                                                                         *
! *   Routine VEX_LIST lists the stations, and sources in a VLBI            *
! *   Experiment schedule file.                                             *
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
! *            STA_ID_LIST  =  Station ID list             { CHAR }         *
! *            STA_LIST     =  Station name list           { CHAR }         *
! *            SOU_LIST     =  Source name list            { CHAR }         *
! *            ANT_LIST     =  Antenna name list           { CHAR }         *
! *                                                                         *
! *  ### 02-JUL-2020  VEX_LIST    v1.0 (c)    N. Habana    02-JUL-2020 ###  *
! *  ### 03-JUL-2020  VEX_LIST    v1.1 (c)    N. Habana    03-JUL-2020 ###  *
! *      Transformed all list entries to capital letters.                   *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      CHARACTER  FIL_VEX*(*)                   
      CHARACTER  STA_ID_LIST(VEX__MSTA)*2, STA_LIST(VEX__MSTA)*16
      CHARACTER  SOU_LIST(VEX__MSOU)*16, ANT_LIST(VEX__MSTA)*16
      CHARACTER  DELIM*6                       ! Space Delimiter
      INTEGER*4  IUER, MP, MIND, MAXL_STRING   ! Max No. of lines, Max. Index 
      PARAMETER  ( MAXL_STRING=256 )           ! Max. String length
      PARAMETER  ( MP = 128*1024 )             
      PARAMETER  ( MIND = 128 )                
      CHARACTER  BUF(MP)*(MAXL_STRING), SECT_ID*32   ! Read File, File Section
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'="'//"'" ) 
      INTEGER*4  J1, J2, J3, NP, LIND, IND(2,MIND), IER
      LOGICAL*1  FL_STA, FL_SOU        ! Flags
      INTEGER*4, EXTERNAL :: ILEN
      INTEGER*4, EXTERNAL :: ADD_CLIST, LTM_DIF
!
! --- Read the Vex File
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_VEX, MP, BUF, NP, IER ) 
      IF ( IER .NE. 0 ) THEN                      
           CALL ERR_LOG ( 8301, IUER, 'VEX_LIST', 'Error in reading '   &
     &        // 'input vex file '//FIL_VEX )
           RETURN 
      END IF
      
!
! --- Go through the file, but only focusing on grabbing the required
!     list items for output.
!
      SECT_ID = '??'
      FL_STA = .FALSE.
      FL_SOU = .FALSE.
      J2     = 0
      J3     = 0
!                CALL TRAN (12,VEX%STA(J4)%SITE_ID,VEX%STA(J4)%SITE_ID)
      DO 410 J1=1,NP
         IF ( ILEN(BUF(J1)) == 0 ) GOTO 410     ! if empty line
         IF ( BUF(J1)(1:1) == '*' ) GOTO 410    ! if line is a comment
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
! ------
         IF ( BUF(J1)(1:1) == '$' ) THEN
              SECT_ID = '??'
         END IF
!
! ------ Handle the Station block
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$STATION;' ) THEN
            SECT_ID  = 'STATION'
            FL_STA   = .TRUE.
!****!            J2       =  0
         END IF
!-------
         IF ( SECT_ID == 'STATION' ) THEN
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN 
               J2 = J2 + 1
!
! ------------ Update Station ID list
!
               STA_ID_LIST(J2) = BUF(J1)(IND(1,2):IND(2,2)-1)
               CALL TRAN ( 12, STA_ID_LIST(J2), STA_ID_LIST(J2) )
            END IF
!
! --------- Update Station name list
!
            IF ( BUF(J1)(IND(1,2):IND(2,2)) == '$SITE' )   THEN
               STA_LIST(J2) = BUF(J1)(IND(1,3):IND(2,3)-1)
               CALL TRAN ( 11, STA_LIST(J2), STA_LIST(J2) )  ! Caps
!
! --------- Update Antenna name list
!
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == '$ANTENNA' ) THEN
               ANT_LIST(J2) =  BUF(J1)(IND(1,3):IND(2,3)-1)
               CALL TRAN ( 11, ANT_LIST(J2), ANT_LIST(J2) )  ! Caps
            END IF
         END IF
!
! ------ Handle the Source block
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == '$SOURCE;' ) THEN
              SECT_ID  = 'SOURCE'
              FL_SOU   = .TRUE.
!****!              J3       =  0
         END IF
! ------
         IF ( SECT_ID == 'SOURCE' ) THEN
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN 
               J3 = J3 + 1
            END IF
!
! --------- Update the Source name list
! --------- N.B: Some VEX files also contain an IAU name, which is not
!                required to be similar to the source name. So we neglect
!                it in this routine.
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'source_name' )   THEN
               SOU_LIST(J3)     = BUF(J1)(IND(1,2):IND(2,2)-1)
               CALL TRAN ( 11, SOU_LIST(J2), SOU_LIST(J2) )  ! Caps
            END IF  
         END IF
 410  CONTINUE
! 
      IF ( .NOT. FL_STA ) THEN
           CALL ERR_LOG ( 8302, IUER, 'VEX_LIST', 'Wrong File Format: ' &
     &          //'Did not encounter a $STATION; block, hence no '      &
     &          // 'stations or antennas could be listed in, '          &
     &          //FIL_VEX//'.' )
           RETURN 
      END IF
! ---
      IF ( .NOT. FL_SOU ) THEN
           CALL ERR_LOG ( 8303, IUER, 'VEX_LIST', 'Wrong File Format: ' &
     &          //'Did not encounter a $SOURCE; block, hence no sources'&
     &          // ' could be listed in, '//FIL_VEX//'.' )
           RETURN 
      END IF
!---
      IF ( J2 .EQ. 0 ) THEN 
           CALL ERR_LOG ( 8304, IUER, 'VEX_LIST', 'The $STATION block ' &
     &          //'is empty.' )
           RETURN 
      END IF
!---
      IF ( J3 .EQ. 0 ) THEN 
           CALL ERR_LOG ( 8304, IUER, 'VEX_LIST', 'The $SOURCE block '  &
     &          //'is empty.' )
           RETURN 
      END IF
!--- 
      CALL ERR_LOG (0, IUER)
      RETURN
      END SUBROUTINE VEX_LIST  !#!#!#!#!

