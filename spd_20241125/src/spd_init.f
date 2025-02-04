      SUBROUTINE SPD_INIT ( SPD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_INIT 
! *                                                                      *
! *  ### 10-DEC-2007    SPD_INIT   v1.0 (c)  L. Petrov  10-DEC-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      CHARACTER  STR*128
      INTEGER*4  NLON, NLAT, IUER
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      CALL NOUT ( SIZEOF(SPD), SPD )
      SPD%LON => NULL()
      SPD%LAT => NULL()
      SPD%LEV => NULL()
      SPD%REF_3D => NULL()
      SPD%SPR_3D => NULL()
      SPD%STM_3D => NULL()
!
      SPD%STA => NULL()
      SPD%UTC_M_TAI = -1.D30
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE   SPD_INIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_4D_INIT ( SPD_4D, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_INIT 
! *                                                                      *
! *  ### 10-DEC-2007    SPD_INIT   v1.0 (c)  L. Petrov  10-DEC-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_4D__TYPE ) :: SPD_4D
      CHARACTER  STR*128
      INTEGER*4  NLON, NLAT, IUER
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      SPD_4D%DIMS = 0
      CALL NOUT ( SIZEOF(SPD_4D), SPD_4D )
      SPD_4D%LON  => NULL()
      SPD_4D%LAT  => NULL()
      SPD_4D%LEV  => NULL()
      SPD_4D%TIM  => NULL()
      SPD_4D%REFR => NULL()
      SPD_4D%STATUS = SPD__INIT
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE   SPD_4D_INIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_CHECK_SHARE_FILE ( FIL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_SEARCH_SHARE_FILE  searches the filename FIL first in *
! *   the current directory, if not found it appends the directory path  *
! *   of MALO shared dirctory and searches there. If it does not find in *
! *   both places it gives up and issues an errror message.              *
! *                                                                      *
! * ## 31-JAN-2014 SPD_CHECK_SHARE_FILE v1.0 (c) L. Petrov 31-JAN-2014 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd_local.i'
      CHARACTER  FIL*(*)
      LOGICAL*1  LEX
      INTEGER*4  IUER
      CHARACTER  FIL_TRY*128, SHARE_DIR*128
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      
      INQUIRE ( FILE=FIL, EXIST=LEX )
#ifndef SPC
      IF ( .NOT. LEX ) THEN
           FIL_TRY = SPD__SHARE//'/'//FIL
           INQUIRE ( FILE=FIL_TRY, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                FIL_TRY = SPD__DOC//'/'//FIL
                INQUIRE ( FILE=FIL_TRY, EXIST=LEX )
                IF ( .NOT. LEX ) THEN
                     IER = 0
                     FIL_TRY = FIL
                     CALL MALO_CHECK_SHARE_FILE ( FIL_TRY, IER )
                     IF ( IER == 0 ) THEN
                          FIL = FIL_TRY 
                          CALL ERR_LOG ( 0, IUER )
                          RETURN 
                        ELSE 
                          IER = 0
                          CALL MALO_CHECK_MODEL_FILE ( FIL_TRY, IER )
                          IF ( IER == 0 ) THEN
                               FIL = FIL_TRY 
                               CALL ERR_LOG ( 0, IUER )
                               RETURN 
                             ELSE
                               CALL ERR_LOG ( 6121, IUER, 'SPD_CHECK_SHARE_FILE', &
     &                             'Cannot find shared file '// &
     &                              FIL(1:I_LEN(FIL))//' neither in neither current '// &
     &                             'directory, nor in SPD shared directory '// &
     &                              SPD__SHARE//', nor in MALO shared '// &
     &                             'directory '//SHARE_DIR )
                              RETURN 
                          END IF
                     END IF
                END IF
           END IF
           FIL = FIL_TRY
      END IF
#endif 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_CHECK_SHARE_FILE !#!#
