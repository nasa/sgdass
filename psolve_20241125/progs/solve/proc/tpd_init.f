      SUBROUTINE TPD_INIT ( TPD, DBNAME_CH, VTD_CONF_USE, NOBS, NSTA, &
     &                      NSOU, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine TPD_INIT 
! *                                                                      *
! *  ### 07-NOV-2007   TPD_INIT    v1.0 (c)  L. Petrov  07-NOV-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      TYPE     ( TPD__TYPE     ) :: TPD
      INTEGER*4  NOBS, NSTA, NSOU, IUER
      CHARACTER  DBNAME_CH*(*), VTD_CONF_USE*(*)
      INTEGER*8  SIZE_I8 
      INTEGER*4  IS, IER
      CHARACTER  STR*128
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO 
!
      IF ( NOBS < 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NOBS, STR ) 
           CALL ERR_LOG ( 7411, IUER, 'TPD_INIT', 'Wrong total '// &
     &         'number of observations' )
           RETURN 
         ELSE IF ( NOBS > MAX_OBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NOBS, STR ) 
           CALL INCH  ( MAX_OBS, STR(101:120) ) 
           CALL ERR_LOG ( 7412, IUER, 'TPD_INIT', 'Wrong total number of '// &
     &         'observations: '//STR(1:ILEN(STR(1:20)))//' -- exceeds '// &
     &         'the Solve maximum: session: '//STR(101:) )
           RETURN 
      END IF
!
      IF ( NSTA < 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NSTA, STR ) 
           CALL ERR_LOG ( 7413, IUER, 'TPD_INIT', 'Wrong total '// &
     &         'number of stations' )
           RETURN 
         ELSE IF ( NSTA > MAX_ARC_STA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NSTA, STR ) 
           CALL INCH  ( INT4(MAX_ARC_STA), STR(101:120) ) 
           CALL ERR_LOG ( 7414, IUER, 'TPD_INIT', 'Wrong total number of '// &
     &         'stations: '//STR(1:ILEN(STR(1:20)))//' -- exceeds '// &
     &         'the Solve maximum: session: '//STR(101:) )
           RETURN 
      END IF
!
      IF ( NSOU < 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NSOU, STR ) 
           CALL ERR_LOG ( 7415, IUER, 'TPD_INIT', 'Wrong total '// &
     &         'number of sources' )
           RETURN 
         ELSE IF ( NSOU > MAX_ARC_SRC ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NSOU, STR ) 
           CALL INCH  ( INT4(MAX_ARC_SRC), STR(101:120) ) 
           CALL ERR_LOG ( 7416, IUER, 'TPD_INIT', 'Wrong total number of '// &
     &         'sources: '//STR(1:ILEN(STR(1:20)))//' -- exceeds '// &
     &         'the Solve maximum: session: '//STR(101:) )
           RETURN 
      END IF
!
      CALL NOUT ( SIZEOF(TPD), TPD )
!
! --- Fill the header
!
      TPD%HEADER%LABEL     = TPD__LABEL
      TPD%HEADER%VTD_FILE  = VTD_CONF_USE
      TPD%HEADER%DB_NAME   = DBNAME_CH
      IF ( NORATE_FLAG ) THEN
           TPD%HEADER%RATE_USE = SOLVE__NO 
	 ELSE 
           TPD%HEADER%RATE_USE = SOLVE__YES
      END IF
      TPD%HEADER%NOBS = NOBS
      TPD%HEADER%NSTA = NSTA
      TPD%HEADER%NSOU = NSOU
      TPD%HEADER%UTC_M_TAI = 1.D30 ! On purpose as a trap
      IF ( ILEN(VTD_CONF_USE) == 0 ) THEN
           TPD%HEADER%VTD_UNIX_DATE = 0
         ELSE 
           IS = FILE_INFO ( VTD_CONF_USE(1:I_LEN(VTD_CONF_USE))//CHAR(0), &
     &                      TPD%HEADER%VTD_UNIX_DATE, SIZE_I8 )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR ) 
                CALL ERR_LOG ( 7417, IUER, 'TPD_INIT', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to learn '// &
     &              'the modification date of the VTD file '// &
     &               VTD_CONF_USE(1:I_LEN(VTD_CONF_USE)) )
                RETURN 
           END IF
      END IF
!
! --- Allocate memory for stations
!
      ALLOCATE ( TPD%STA(NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( NSTA*SIZEOF(TPD%STA(1)), STR )
           CALL ERR_LOG ( 7418, IUER, 'TPD_INIT', 'Error '// &
     &         'in attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory for TPD' )
           RETURN 
      END IF
!
! --- Allocate memory for sources
!
      ALLOCATE ( TPD%SOU(NSOU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( NSOU*SIZEOF(TPD%SOU(1)), STR )
           CALL ERR_LOG ( 7419, IUER, 'TPD_INIT', 'Error '// &
     &         'in attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory for TPD' )
           RETURN 
      END IF
!
! --- Allocate memory for parameters
!
      ALLOCATE ( TPD%PARAM(NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( NOBS*SIZEOF(TPD%PARAM(1)), STR )
           CALL ERR_LOG ( 7420, IUER, 'TPD_INIT', 'Error '// &
     &         'in attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory for TPD' )
           RETURN 
      END IF
!
! --- Allocate memory for delays
!
      ALLOCATE ( TPD%DELAY(NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( NOBS*SIZEOF(TPD%DELAY(1)), STR )
           CALL ERR_LOG ( 7421, IUER, 'TPD_INIT', 'Error '// &
     &         'in attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory for TPD' )
           RETURN 
      END IF
!
      IF ( TPD%HEADER%RATE_USE == SOLVE__YES ) THEN
! 
! -------- Allocate memory for rates
!
           ALLOCATE ( TPD%RATE(NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( NOBS*SIZEOF(TPD%RATE(1)), STR )
                CALL ERR_LOG ( 7422, IUER, 'TPD_INIT', 'Error '// &
     &              'in attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &              'of dynamic memory for TPD' )
               RETURN 
          END IF
      END IF
!			
      IF ( KBATCH ) THEN
           CALL CLRCH  ( STR )
           CALL INCH   ( IARCNM, STR(1:5) )
           CALL CHASHR ( STR(1:5) )
           CALL BLANK_TO_ZERO ( STR(1:5) )
           TPD%FILE_NAME = TPD_DIR(1:I_LEN(TPD_DIR))//'/'//PRE_LETRS// &
     &                     '_'//STR(1:5)//'.tpd'
         ELSE 
           TPD%FILE_NAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'TPDF'//PRE_LETRS
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TPD_INIT  !#!#
