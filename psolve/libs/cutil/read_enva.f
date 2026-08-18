      SUBROUTINE READ_ENVA ( IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_ENVA reads definitions of environment variables from  *
! *   a file in the Solve working direcotery PSOVLE_WORK_DIR and sets    *
! *   their value for them to be accessible in the context of the        *
! *   current process.                                                   *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 21-JUN-2025    READ_ENVA  v1.0 (d)  L. Petrov  21-JUN-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 128 ) 
      PARAMETER  ( MIND =  32 ) 
      CHARACTER  BUF(MBUF)*128, FNAME*128, ENV_NAME*32, ENV_VALUE*128
      LOGICAL*1  LEX
      INTEGER*4  J1, NV, LIND, IND(2,MIND), IER
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'ENVA'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FNAME, MBUF, BUF, NV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4741, IUER, 'READ_ENVA', 'Error in reading '// &
     &         'file with environment variables '//FNAME )
           RETURN 
      END IF 
!
      DO 410 J1=1,NV
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )         
         IF ( LIND < 2 ) GOTO 410
         ENV_NAME  = BUF(J1)(IND(1,1):IND(2,1))
         ENV_VALUE = BUF(J1)(IND(1,2):IND(2,LIND))
         CALL SETENV   ( TRIM(ENV_NAME)//CHAR(0), TRIM(ENV_VALUE)//CHAR(0), %VAL(1) )
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE READ_ENVA  !#!  
