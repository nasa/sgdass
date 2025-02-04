      SUBROUTINE VTD_LOAD_IONO ( MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                           M_IOF, VIONO_FILE, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_LOAD_IONO  reads a set of binary files with GPS       *
! *   ionosphere TEC maps, parses them, determines whether at least one  *
! *   of them has the data for the specified range, extract the data     *
! *   for that date range, and writes results of parsing in fields of    *
! *   the data structure VTD. Then VTD_LOAD_IONO computes coefficients   *
! *   of the 3D spline that interpolates the TEC maps for entire Earth   *
! *   surface for the specified date range.                              *
! *                                                                      *
! *   VTD_LOAD_IONO tris files in array VIONO_FILE consecutively till    *
! *   it finds the first file that has the data covering the requrested  *
! *   date range. If such a file is found, no further files are scanned. *
! *   If no file with with data for the requested range is found,        *
! *   and error message is generated.                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MJD_BEG ( INTEGER*4  ) -- MJD date of the beginning of the range. *
! *    TAI_BEG ( REAL*8     ) -- TAI time of the beginning of the range. *
! *    MJD_END ( INTEGER*4  ) -- MJD date of the end of the range.       *
! *    TAI_END ( REAL*8     ) -- TAI time of the end of the range.       *
! *      M_IOF ( INTEGER*4  ) -- The maximum number of ionosphere files. *
! * VIONO_FILE ( CHARACTER  ) -- Array of binary files with GPS          *
! *                              ionosphere TEC maps in VIONO format.    *
! *                              Dimension: M_IOF.                       *
! *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     VTD ( VTD__TYPE ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
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
! *  ### 13-MAY-2010  VTD_LOAD_IONO v1.2 (c) L. Petrov  17-FEB-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( IONO__TYPE ) :: VIONO
      INTEGER*4  MJD_BEG, MJD_END, M_IOF, IUER
      REAL*8     TAI_BEG, TAI_END
      CHARACTER  VIONO_FILE(M_IOF)*(*)
      LOGICAL*1  LEX
      CHARACTER  FILE_IO_LOCK*128, FILE_READ_LOCK*128, FILE_WRITE_LOCK*128
      CHARACTER  STR*128, STR1*128
      INTEGER*4  J1, J2, MJD_IONO_BEG, MJD_IONO_END, MJD_BEG_USE, IDAY, &
     &           IND_EPC_BEG, IND_EPC_END, LEPC, ID, FD_READ_LOCK, FD_WRITE_LOCK, &
     &           MN_EXTRA, IER
      REAL*8     UTC_IONO_BEG, UTC_IONO_END, UTC_BEG_USE, EPS
      PARAMETER  ( EPS = 1.D-8 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
! --- Initialization
!
      IF ( ASSOCIATED ( VTD%IONO%TEC_VAL ) ) DEALLOCATE ( VTD%IONO%TEC_VAL )
      IF ( ASSOCIATED ( VTD%IONO%TEC_SPL ) ) DEALLOCATE ( VTD%IONO%TEC_SPL )
      IF ( ASSOCIATED ( VTD%IONO%LON_VAL ) ) DEALLOCATE ( VTD%IONO%LON_VAL )
      IF ( ASSOCIATED ( VTD%IONO%LAT_VAL ) ) DEALLOCATE ( VTD%IONO%LAT_VAL )
      IF ( ASSOCIATED ( VTD%IONO%TIM_VAL ) ) DEALLOCATE ( VTD%IONO%TIM_VAL )
!
      CALL NOUT ( SIZEOF(VTD%IONO%HEADER), VTD%IONO%HEADER )
      CALL NOUT ( SIZEOF(VTD%IONO),        VTD%IONO        )
!
      CALL NOUT ( SIZEOF(VIONO%HEADER), VIONO%HEADER )
      CALL NOUT ( SIZEOF(VIONO),        VIONO        )
!
! --- Check all IONOSPHERE files in search for the file with appropriate 
! --- date range
!
      DO 410 J1=1,M_IOF
         IF ( ILEN(VIONO_FILE(J1)) == 0      .OR. &
     &        VIONO_FILE(J1)       == 'NONE'      ) GOTO 410
!
! ------ Check whether the file exists
!
         INQUIRE ( FILE=VIONO_FILE(J1), EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              CALL ERR_LOG ( 4452, IUER, 'VTD_LOAD_IONO', 'Ionosphere TEC '// &
     &            'map file in internal binary format '// &
     &            VIONO_FILE(J1)(1:I_LEN(VIONO_FILE(J1)))//' was not found' )
              RETURN
         END IF
!
! ------ Build the names of lock files
!
         ID = LINDEX ( VIONO_FILE(J1), '/' ) 
         FILE_IO_LOCK    = VIONO_FILE(J1)(1:ID)//VTD__IO_LOCK_NAME
         FILE_READ_LOCK  = VIONO_FILE(J1)(1:ID)//VTD__READ_LOCK_NAME
         FILE_WRITE_LOCK = VIONO_FILE(J1)(1:ID)//VTD__WRITE_LOCK_NAME
!
         CALL ERR_PASS ( IUER, IER )
         CALL SET_WRITE_LOCK ( FILE_IO_LOCK, FILE_READ_LOCK, FILE_WRITE_LOCK, &
     &                         VTD__LOCK_TIMEOUT, FD_READ_LOCK, FD_WRITE_LOCK, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4451, IUER, 'VTD_LOAD_IONO', 'Error in setting '// &
     &            'up write lock while reading TEC maps' )
              RETURN
         END IF
!
! ------ Read the header
!
         CALL ERR_PASS ( IUER, IER )
         CALL VIO_GET_HEADER ( VIONO_FILE(J1), VIONO, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4453, IUER, 'VTD_LOAD_IONO', 'Failure in an '// &
     &            'attempt to read the header of the ionosphere TEC '// &
     &            'map file in internal binary format '//VIONO_FILE(J1) )
              CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
              RETURN
         END IF
!
         IF ( VIONO%HEADER%TIM_STEP .GE. 24*3600.0D0 - EPS ) THEN
              MN_EXTRA = 1
            ELSE IF ( VIONO%HEADER%TIM_STEP .GE. 4*3600.0D0 - EPS ) THEN
              MN_EXTRA = 2
            ELSE IF ( VIONO%HEADER%TIM_STEP .GE. 2*3600.0D0 - EPS ) THEN
              MN_EXTRA = 3
            ELSE IF ( VIONO%HEADER%TIM_STEP .GE. 1*3600.0D0 - EPS ) THEN
              MN_EXTRA = 5
         END IF
!
! ------ Extract the start date and compute the end date
!
         MJD_IONO_BEG = VIONO%HEADER%MJD_BEG
         UTC_IONO_BEG = VIONO%HEADER%UTC_BEG
!
         MJD_IONO_END = VIONO%HEADER%MJD_BEG
         UTC_IONO_END = VIONO%HEADER%UTC_BEG + VIONO%HEADER%NEPC* &
     &                  VIONO%HEADER%TIM_STEP ! NB: This is the last epoch of beginning of the map
         IDAY = IDINT( UTC_IONO_END/86400.0D0 )
         IF ( UTC_IONO_END < 0.0D0 ) IDAY = IDAY -1
         MJD_IONO_END = MJD_IONO_END + IDAY
         UTC_IONO_END = UTC_IONO_END - IDAY*86400.0D0
!
! ------ NB: We negelect the difference between UTC and TAI hear
!
         IF ( ( (MJD_BEG - MJD_IONO_BEG )*86400.0D0 + &
     &          (TAI_BEG - UTC_IONO_BEG )             > 0.0D0 ) .AND. &
     &        ( (MJD_IONO_END - MJD_END )*86400.0D0 + &
     &          (UTC_IONO_BEG - TAI_END)            > 0.0D0  )       ) THEN
!
! ----------- This file contains ionosphere tec data forthe date
! ----------- range needed for VTD computations
!
!
! ----------- Compute the first epoch for which the data will be downloaded.
! ----------- We read extra MN_EXTRA epochs for improving interpolation
!
              MJD_BEG_USE = MJD_BEG
              UTC_BEG_USE = TAI_BEG - MN_EXTRA*VIONO%HEADER%TIM_STEP
!
! ----------- ... however it may happen that we cannot add extra epochs
! ----------- at the beginning because we are very close to the begnning
! ----------- of the data range. Then we use the frist epoch
!
              IF ( ( (MJD_BEG_USE - VIONO%HEADER%MJD_BEG)*86400.0D0 + &
     &               (UTC_BEG_USE - VIONO%HEADER%UTC_BEG) ) < 0.0D0 ) THEN
                    MJD_BEG_USE = VIONO%HEADER%MJD_BEG
                    UTC_BEG_USE = VIONO%HEADER%UTC_BEG
              END IF
!
              IDAY = IDINT( UTC_BEG_USE/86400.0D0 )
              IDAY = UTC_BEG_USE/86400.0D0
              IF ( UTC_BEG_USE < 0.0D0 ) IDAY = IDAY -1
              MJD_BEG_USE = MJD_BEG_USE + IDAY
              UTC_BEG_USE = UTC_BEG_USE - IDAY*86400.0D0
!
! ----------- Compute LEPC -- the number of epochs to be read. We again add
! ----------- several epochs to the end for improving interpolation
!
              LEPC = IDINT ( ( (MJD_END - MJD_BEG)*86400.0D0 + &
     &                         (TAI_END - TAI_BEG) )/VIONO%HEADER%TIM_STEP ) + &
     &                     2*MN_EXTRA + 1
              IND_EPC_BEG = IDINT( ( (MJD_BEG_USE - VIONO%HEADER%MJD_BEG)*86400.0D0 + &
     &                               (UTC_BEG_USE - VIONO%HEADER%UTC_BEG) )/ &
     &                               VIONO%HEADER%TIM_STEP ) + 1
              IND_EPC_END = IND_EPC_BEG + LEPC-1
!
! ----------- ... however if the file is too short and does not have the extra
! ----------- epoch, we adjust LEPC
!
              IF ( IND_EPC_END > VIONO%HEADER%NEPC ) THEN
                   LEPC = LEPC - (IND_EPC_END - VIONO%HEADER%NEPC)
              END IF
!
! ----------- Read the header of the ionosphere data and put it in VTD%IONO.
! ----------- Put the body of the ionosphere data
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_GET_IONO ( VIONO_FILE(J1), MJD_BEG_USE, UTC_BEG_USE, &
     &                             LEPC, VTD%IONO, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4454, IUER, 'VTD_LOAD_IONO', 'Failure in '// &
     &                 'an attempt to read the contents of the ionosphere '// &
     &                 'TEC map file in internal binary format '// &
     &                  VIONO_FILE(J1) )
                   CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
                   RETURN
              END IF
!
! ----------- Compute interpolating spline
!
              CALL ERR_PASS ( IUER, IER )
              CALL COMP_IONO_SPL ( VTD%IONO, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4455, IUER, 'VTD_LOAD_IONO', 'Error in '// &
     &                 'an attempt to compute coefficients of the '// &
     &                 '3D spline that interpolates ionosphere TEC map' )
                   CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
                   RETURN
              END IF
              CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
              GOTO 810
         END IF
!
! ------ Lift the write lock
!
         CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
 410  CONTINUE 
!
      STR  = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, -2 )
      STR1 = MJDSEC_TO_DATE ( MJD_END, TAI_END, -2 )
      CALL ERR_LOG ( 4456, IUER, 'VTD_LOAD_IONO', 'Failure in an '// &
     &    'attempt to find the ionosphere data that would be suitable '// &
     &    'for the range [ '//STR(1:21)//' , '//STR1(1:21)//' ] ' )
      RETURN
!
! --- Happy end
!
 810  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_LOAD_IONO  !#!#
