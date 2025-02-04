      SUBROUTINE VTD_GET_IONO ( VIO_FILE, MJD_BEG, UTC_BEG, LEPC, VIO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_GET_IONO  parses the file with ionosphere total        *
! *   electron contentsd (TEC) maps, parses it, reads the header and     *
! *   read the data on a global grid for the specified dat arange that   *
! *   is defined by the start epoch MJD_BEG/UTC_EPC and the number of    *
! *   epochs. Results of parsing fill internal fields of the data        *
! *   structure VIO.                                                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * MJD_BEG ( INTEGER*4  ) -- MJD date of the beginning of the range.    *
! * UTC_BEG ( REAL*8     ) -- UTC time tag of the beginning of the range.*
! *    LEPC ( INTEGER*4  ) -- The number of epochs to be read.           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     VIO ( IONO__TYPE ) -- Data structure that keeps the data         *
! *                           related to ionosphere TEC maps.            *
! *                           It contains the header that describes      *
! *                           the dataset and the data.                  *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 10-MAY-2010  VTD_GET_IONO   v1.2 (c) L. Petrov  17-FEB-2022  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      CHARACTER  VIO_FILE*(*)
      INTEGER*4  MJD_BEG, LEPC, IUER
      REAL*8     UTC_BEG
      TYPE     ( IONO__TYPE ) :: VIO
      CHARACTER  STR*128, STR1*128
      INTEGER*4  KEPC, IND_EPC_BEG, IND_EPC_END, REC_LEN, SEEK_SET, &
     &           LUN, IS, LN, IDAY, IER
      INTEGER*8  OFFS
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ
      INTEGER*8,     EXTERNAL :: LSEEK64
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN
!                
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, LN )
!
! --- Initialization
!
      IF ( VIO%STATUS_VAL == VIO__ALLO .OR. &
     &     VIO%STATUS_VAL == VIO__READ      ) THEN
           DEALLOCATE ( VIO%TEC_VAL )
      END IF
!
! --- Read and parse the header
!
      CALL ERR_PASS ( IUER, IER )
      CALL VIO_GET_HEADER ( VIO_FILE, VIO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4411, IUER, 'VTD_GET_IONO', 'Failure to read the '// &
     &         'header of the input file with TEC model' )
           RETURN 
      END IF
!
! --- Get the index of the first epoch of the range
!
      IND_EPC_BEG = IDNINT ( ( (MJD_BEG - VIO%HEADER%MJD_BEG)*86400.0D0 + &
     &                         (UTC_BEG - VIO%HEADER%UTC_BEG) - SHR__VIO*VIO%HEADER%TIM_STEP )/ &
     &                       VIO%HEADER%TIM_STEP ) + 1
!
! --- Check whether the requested start date is within the range that
! --- the file contains
!
      IF ( IND_EPC_BEG < 1 ) THEN
           IER  = 0
           STR  = MJDSEC_TO_DATE ( MJD_BEG, UTC_BEG, IER )
           IER  = 0
           STR1 = MJDSEC_TO_DATE ( VIO%HEADER%MJD_BEG, VIO%HEADER%UTC_BEG, IER )
           CALL ERR_LOG ( 4412, IUER, 'VTD_GET_IONO', 'Input start date '// &
     &          ' MJD_BEG/UTC_BEG: '//STR(1:21)//' is ealier than '// &
     &          ' the first epoch in the input file with the TEC model '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR1(1:21) )
           RETURN 
      END IF
      IF ( IND_EPC_BEG > VIO%HEADER%NEPC ) THEN
           IER  = 0
           STR  = MJDSEC_TO_DATE ( MJD_BEG, UTC_BEG, IER )
           IER  = 0
           STR1 = MJDSEC_TO_DATE ( VIO%HEADER%MJD_BEG, VIO%HEADER%UTC_BEG + &
     &                             VIO%HEADER%TIM_STEP*(VIO%HEADER%NEPC+1), &
     &                             IER )
           CALL ERR_LOG ( 4413, IUER, 'VTD_GET_IONO', 'Input start date '// &
     &          ' MJD_BEG/UTC_BEG: '//STR(1:21)//' is later than '// &
     &          ' the last epoch in the input file with the TEC model '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR1(1:21) )
           RETURN 
      END IF
!
      IND_EPC_END = IND_EPC_BEG + LEPC - 1
      IF ( IND_EPC_END > VIO%HEADER%NEPC ) THEN
           IER  = 0
           STR  = MJDSEC_TO_DATE ( MJD_BEG, UTC_BEG + &
     &                             (LEPC-1)*VIO%HEADER%TIM_STEP, IER )
           IER  = 0
           STR1 = MJDSEC_TO_DATE ( VIO%HEADER%MJD_BEG, VIO%HEADER%UTC_BEG + &
     &                             VIO%HEADER%NEPC*VIO%HEADER%TIM_STEP, IER )
           CALL ERR_LOG ( 4414, IUER, 'VTD_GET_IONO', 'Input end date '// &
     &          ' MJD_BEG/UTC_BEG/LEPC: '//STR(1:21)//' is later '// &
     &          'than the last epoch in the input file with the TEC model '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR1(1:21) )
           RETURN 
      END IF
!
      VIO%HEADER%NEPC = LEPC
      ALLOCATE ( VIO%TEC_VAL(VIO%HEADER%NLON,VIO%HEADER%NLAT,VIO%HEADER%NEPC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 2*VIO%HEADER%NLON*VIO%HEADER%NLAT*VIO%HEADER%NEPC, STR )
           CALL ERR_LOG ( 4414, IUER, 'VTD_GET_IONO', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for TEC maps' )
           RETURN 
      END IF
!
      VIO%STATUS_VAL = VIO__ALLO 
      REC_LEN = 2*VIO%HEADER%NLON*VIO%HEADER%NLAT*VIO%HEADER%NEPC
!
! --- Open the file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( VIO_FILE, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4415, IUER, 'VTD_GET_IONO', 'Failure '// &
     &         'to open existing binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
      END IF
!
! --- Position the TEC ionosphere file
!
      OFFS = SIZEOF(VIO%HEADER) + &
     &       (IND_EPC_BEG-1)*2*VIO%HEADER%NLON*VIO%HEADER%NLAT
#ifdef ADR_32BIT
      IS = LSEEK64 ( %VAL(LUN), %VAL(OFFS), %VAL(SEEK_SET) )
#else
      IS = LSEEK   ( %VAL(LUN), %VAL(OFFS), %VAL(SEEK_SET) )
#endif
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4416, IUER, 'VTD_GET_IONO', 'Failure '// &
     &         'to seek the beginning of binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
      END IF
!
! --- Read the block of data
!
      IS = READ ( %VAL(LUN), VIO%TEC_VAL, %VAL(REC_LEN) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4417, IUER, 'VTD_GET_IONO', 'Failure '// &
     &         'to read the TEC map from the binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
        ELSE IF ( IS .NE. REC_LEN ) THEN
           CALL ERR_LOG ( 4418, IUER, 'VTD_GET_IONO', 'Failure '// &
     &         'to read the data record from the binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- not all bytes have '// &
     &         'been read' )
           RETURN 
      END IF
      VIO%STATUS_VAL = VIO__READ
!
! --- Close the output file
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4419, IUER, 'VTD_GET_IONO', 'Failure '// &
     &         'to close binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
!
      VIO%HEADER%UTC_BEG = VIO%HEADER%UTC_BEG + (IND_EPC_BEG-1)*VIO%HEADER%TIM_STEP
      IDAY = VIO%HEADER%UTC_BEG/86400.0D0
      VIO%HEADER%MJD_BEG = VIO%HEADER%MJD_BEG + IDAY
      VIO%HEADER%UTC_BEG = VIO%HEADER%UTC_BEG - IDAY*86400.D0
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_GET_IONO  !#!  
