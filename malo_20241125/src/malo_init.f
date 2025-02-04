      SUBROUTINE MALO_INIT ( MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_INIT
! *                                                                      *
! *  ### 12-OCT-2012    MALO_INIT  v1.2 (c)  L. Petrov  15-FEB-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i' 
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  IUER
      INTEGER*4  IER
!
      CALL NOUT ( SIZEOF(MALO), MALO )
!
      MALO%LEV       => NULL()
      MALO%LAT       => NULL()
      MALO%LON       => NULL()
      MALO%TIM       => NULL()
      MALO%MODC      => NULL()
      MALO%MJD_ARR   => NULL()
      MALO%TAI_ARR   => NULL()
      MALO%SPR       => NULL()
      MALO%SPH       => NULL()
      MALO%LSM       => NULL()
      MALO%LOVE      => NULL()
      MALO%PPWTEM_4D => NULL()
      MALO%REFRA_4D  => NULL()
      MALO%PRES_3D   => NULL()
!
      MALO%LEAPSEC%FINAM_LEAPSEC = MALO__LEAPSEC_FILE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_CHECK_SHARE_FILE ( MALO%LEAPSEC%FINAM_LEAPSEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5121, IUER, 'MALO_INIT', 'Cannot find '// &
     &         'leap second file' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_LOAD_LEAPSEC ( MALO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5122, IUER, 'MALO_INIT', 'Failure to load '// &
     &         'leap second data' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_INIT  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_CHECK_SHARE_FILE ( FIL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_SEARCH_SHARE_FILE searches the filename FIL first in  *
! *   the current directory, if not found it appends the directory path  *
! *   of MALO shared dirctory and searches there. If it does not find in *
! *   both places it gives up and issues an errror message.              *
! *                                                                      *
! * # 23-OCT-2013 MALO_CHECK_SHARE_FILE v1.0 (c) L. Petrov 23-OCT-2013 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo_local.i'
      CHARACTER  FIL*128, FIL_TRY*128
      LOGICAL*1  LEX
      INTEGER*4  IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      
      INQUIRE ( FILE=FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           FIL_TRY = MALO_SHARE(1:I_LEN(MALO_SHARE))//'/'//FIL
           INQUIRE ( FILE=FIL_TRY, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 5131, IUER, 'MALO_CHECK_SHARE_FILE', &
     &              'Cannot find shared file '// &
     &               FIL(1:I_LEN(FIL))//' in neither current '// &
     &              'directory nor in MALO shared directory '//MALO_SHARE )
                RETURN 
           END IF
           FIL = FIL_TRY
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_CHECK_SHARE_FILE !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_CHECK_MODEL_FILE ( FIL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_CHECK_MODEL_FILE searches the filename FIL first in   *
! *   the current directory, if not found it appends the directory path  *
! *   of MALO MODEL directory and searches there. If it does not find    *
! *   there, it seaches MALO_SHARE directory. If it does not find it in  *
! *   all three places, it gives up and issues an errror message.        *
! *                                                                      *
! * # 23-MAY-2017 MALO_CHECK_MODEL_FILE v1.1 (c) L. Petrov 06-SEP-2018 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo_local.i'
      CHARACTER  FIL*128, FIL_TRY*128
      LOGICAL*1  LEX
      INTEGER*4  IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      
      INQUIRE ( FILE=FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           FIL_TRY = MALO_MODEL(1:I_LEN(MALO_MODEL))//'/'//FIL
           INQUIRE ( FILE=FIL_TRY, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                FIL_TRY = MALO_SHARE(1:I_LEN(MALO_SHARE))//'/'//FIL
                INQUIRE ( FILE=FIL_TRY, EXIST=LEX )
                IF ( .NOT. LEX ) THEN
                     CALL ERR_LOG ( 5131, IUER, 'MALO_CHECK_MODEL_FILE', &
     &                   'Cannot find MODEL file '// &
     &                    FIL(1:I_LEN(FIL))//' in neither current '// &
     &                   'directory nor in MALO MODEL directory '//TRIM(MALO_MODEL)// &
     &                   ', nor in MALO_SHARE directory '//MALO_SHARE )
                     RETURN 
                END IF
           END IF
           FIL = FIL_TRY
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_CHECK_MODEL_FILE !#!#
