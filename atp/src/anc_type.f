      SUBROUTINE ANC_TYPE ( FILIN, ANC, IUER )
!
! ************************************************************************
! *                                                                      *
! *   Routine ANC_TYPE                                                   *
! *   Check if this experiment is an SDE or VLBI experiment              *
! *                                                                      *
! *   INPUT:                                                             *
! *       FILIN   =  Known Source list for SDE's       { CHAR }          *
! *                                                                      *
! *       ANC    =  Parsed Antenna Calibration file    { DERIVED TYPE }  *
! *                                                                      *
! *       IUER    =  Error Handler                     { INT, OPT }      *
! *                  If IUER=0 no error message will be printed,         *
! *                  even in the event of an error. However, for         *
! *                  other possible values, i.e. IUER=-1,-2, & -3,       *
! *                  the error message will print to screen. For         *
! *                  the latter case, i.e., IUER = -3, after             *
! *                  after printing the program will terminate.          *
! *                  Default, IUER = -1                                  *
! *                                                                      *
! *   OUTPUT:                                                            *
! *       ANC%EXP_TYPE = Update experiment type        { DERIVED TYPE }  *
! *                      Uses only 'SDE' or 'VLBI'                       *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 26-NOV-2025  ANC_TYPE    v1.0 (d)  N. Habana  26-NOV-2025  ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      CHARACTER  FILIN*(*)
      TYPE     ( ANC__TYP   ) :: ANC      
      INTEGER*4  IUER, IER
      CHARACTER  DELIM*3, STR*128, STR1*16, STR2*16
      INTEGER*4  MIND, MP, NP
      PARAMETER  ( MP   = ANC__MSTR )
      PARAMETER  ( MIND = 32 )
      CHARACTER  BUF(MP)*(ANC__MSTR)
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) ) ! Null, Space, Tab
      INTEGER*4  NBUF, LIND, IND(2,MIND)
      INTEGER*4  J1, J2, J3, LCNT, NSDE
      CHARACTER  DOO_SOU(ANC__MSOU)*8, SDE_SOU(ANC__MSOU)*8
      INTEGER*4, EXTERNAL :: LTM_DIF, FILE_INFO, I_LEN, ILEN
      LOGICAL*1  IS_SDE
!     
! --- Set default type to VLBI
!
      ANC%EXP_TYPE = 'VLBI'
!     
! --- If there is SEFD information, we know that we have a VLBI file
!
      IF ( (ANC%NUM_SEFD > 0) .OR. (ANC%NUM_EPO_SEFD > 0) ) THEN
         ANC%EXP_TYPE = 'VLBI'
         GOTO 110
      END IF
!     
! --- Read the list of SDE sources
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MP, BUF, NP, IER ) 
      IF ( IER .NE. 0 ) THEN                       ! i.e. if there is error
         CALL ERR_LOG ( 8000, IUER, 'VEX_PARSER',                     &
     &           'Error reading input sde source list '//FILIN )                  
         RETURN 
      END IF
!
! --- Parse the source list to  
!
      CALL CLRCH (DOO_SOU)
      CALL CLRCH (SDE_SOU)
!
      NSDE = 0
      DO 210 J1 = 1, NP
!
! ------ Extract the list information and 
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
         IF ( BUF(J1)       == '#' ) GOTO 210
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 210
!
! ------ Parse to the sde source list
!
         NSDE = NSDE + 1
         SDE_SOU(NSDE) = BUF(J1)(IND(1,1):IND(2,1))
 210  CONTINUE
!
! --- Parse the source names from ANC%DOO to an array 
!
      DO 310 J1 = 1, ANC%NUM_DOO
         DOO_SOU(J1) = ANC%DOO(J1)%SOU_NAM
 310  CONTINUE
!
! --- Check if any of the sources in the SDE list are in the 
!     DATA_ON block
!
      IS_SDE = .FALSE.
!
      DO 410 J1 = 1, NSDE
         LCNT = LTM_DIF ( 0, ANC%NUM_DOO, DOO_SOU, SDE_SOU(J1) )
!
! ------ If we found any of the SDE sources
!
         IF ( LCNT > 0 ) IS_SDE = .TRUE.
!
         IF ( IS_SDE ) THEN
            ANC%EXP_TYPE = 'SDE'
            EXIT
         END IF
 410  CONTINUE
!     
 110  CONTINUE
!         
      RETURN
      END SUBROUTINE 
