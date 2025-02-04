      SUBROUTINE EDC_INIT ( EDC, NOBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EDC_INIT
! *                                                                      *
! *  ### 25-OCT-2007   EDC_INIT  v1.0 (c)   L. Petrov  25-OCT-2007  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'edc.i'
      TYPE     ( EDC__TYPE     ) :: EDC
      CHARACTER  STR*128
      INTEGER*4  NOBS, IUER
      INTEGER*4  J1, J2, IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL CLRCH ( EDC%HEA%DB_NAME )
      EDC%HEA%DB_NAME = DBNAME_CH
      EDC%HEA%PRC_NAME = 'Template'
      EDC%HEA%N_OBS = NOBS
      EDC%HEA%N_SCA = NUMSCA
      EDC%HEA%N_STA = NUMSTA
      EDC%HEA%N_SOU = NUMSTR
!
      STR = GET_CDATE()
      CALL DATE_TO_TIME ( STR, EDC%HEA%MJD_CRE, EDC%HEA%TIM_CRE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7611, IUER, 'EDC_INIT', 'Trap of internal '// &
     &         'control: wrong curretn date' )
           RETURN 
      END IF
!
      EDC%HEA%MJD_SES = 0
      EDC%HEA%TAI_SES = 0.0D0
!
      ALLOCATE ( EDC%C_STA(EDC%HEA%N_STA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7612, IUER, 'EDC_INIT', 'Failure to allocate '// &
     &         'dynamic array for station names' )
           RETURN 
      END IF
!
      ALLOCATE ( EDC%C_SOU(EDC%HEA%N_SOU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7613, IUER, 'EDC_INIT', 'Failure to allocate '// &
     &         'dynamic array for source names' )
           RETURN 
      END IF
!
      ALLOCATE ( EDC%OBS(EDC%HEA%N_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(EDC%OBS(1))*EDC%HEA%N_OBS, STR )
           CALL ERR_LOG ( 7614, IUER, 'EDC_INIT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'observation sub-object of EDC' )
           RETURN 
      END IF
!
      DO 410 J1=1,EDC%HEA%N_STA
         EDC%C_STA(J1) = ISITN_CHR(J1)
 410  CONTINUE 
!
      DO 420 J2=1,EDC%HEA%N_SOU
         EDC%C_SOU(J2) = ISTRN_CHR(J2)
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EDC_INIT  !#!#
