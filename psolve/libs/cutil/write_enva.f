      SUBROUTINE WRITE_ENVA ( NV, ENV_NAMES, ENV_VALUES, IUER )
! ************************************************************************
! *                                                                      *
! *   Program WRITE_ENVA
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 21-JUN-2025  WRITE_ENVA   v1.0 (d)  L. Petrov  21-JUN-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INTEGER*4  NV, IUER
      CHARACTER  ENV_NAMES(NV)*(*), ENV_VALUES(NV)*(*)
      INTEGER*4  MV
      PARAMETER  ( MV = 1024 )
      CHARACTER  FNAME*128, BUF(MV)*128, STR*128
      LOGICAL*1  LEX
      INTEGER*4  J1, IER, IS
      INTEGER*4, EXTERNAL :: UNLINK
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'ENVA'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( NV == 0 .AND. .NOT. LEX ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( NV == 0 .AND. LEX ) THEN
           IS = UNLINK ( TRIM(FNAME)//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 4751, IUER, 'WRITE_ENVA', 'Error in attempt '// &
     &             'to remove file with environment variables '//FNAME )
                RETURN 
           END IF
         ELSE
           IF ( NV > MV .OR. NV < 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( NV, STR )
                CALL ERR_LOG ( 4752, IUER, 'WRITE_ENVA', 'Argument NV '// &
     &             'is too small: '//STR )
                RETURN 
           END IF
!
           DO 410 J1=1,NV
              CALL CLRCH ( BUF(J1) )
              BUF(J1)(1:32) = ENV_NAMES(J1)
              BUF(J1)(36:)  = ENV_VALUES(J1)
 410       CONTINUE 
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT  ( NV, BUF, FNAME, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4753, IUER, 'WRITE_ENVA', 'Error in attempt '// &
     &             'to write a file with environment variables '//FNAME )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE WRITE_ENVA  !#!  
