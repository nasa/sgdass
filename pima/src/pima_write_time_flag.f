      SUBROUTINE PIMA_WRITE_TIME_FLAG ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_WRITE_TIME_FLAG  writes time flags stored in PIM     *
! *   object to the output file PIM%CONF%TIME_FLAG_FILE.                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     PIM ( PIMA__TYP ) -- Object with information related to program  *
! *                          PIMA.                                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ## 19-NOV-2014 PIMA_WRITE_TIME_FLAG v1.0 (c) L. Petrov 19-NOV-2014 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      CHARACTER  STR*128
      LOGICAL*1  FL_FLAG
      INTEGER*4  IND_OBS, FRG_IND, J1, J2, J3, LUN, IOS, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
! --- Check whether the name of the output file is specified
!
      IF ( ILEN(PIM%CONF%TIME_FLAG_FILE) == 0 ) THEN
           CALL ERR_LOG ( 9151, IUER, 'PIMA_WRITE_TIME_FLAG', 'Trap of '// &
     &         'internal control: time flag file has not been specified' )
           RETURN
      END IF
!
! --- Check whether there are observations in PIMA experiment
!
      IF ( PIM%CONF%FRIB_NOBS .LE. 0 ) THEN
           CALL ERR_LOG ( 9152, IUER, 'PIMA_WRITE_TIME_FLAG', 'Trap of '// &
     &         'internal control: no observations were found for '// &
     &         'experment '//PIM%CONF%SESS_CODE )
           RETURN
      END IF
!
! --- Get the unit number and open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=PIM%CONF%TIME_FLAG_FILE, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 9152, IUER, 'PIMA_WRITE_TIME_FLAG', 'Error in '// &
     &          'an attempt to open output file '//PIM%CONF%TIME_FLAG_FILE )
           RETURN
      END IF
!
! --- Write the header
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) PIMA__FLAG_LABEL
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 9153, IUER, 'PIMA_WRITE_TIME_FLAG', 'Failure in '// &
     &         'writing the first record in the output file '// &
     &          PIM%CONF%TIME_FLAG_FILE )
           RETURN
      END IF
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Created on '//GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Cycle over observations
!
      DO 410 J1=1,PIM%NOBS
         IND_OBS = J1
         FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
!
! ------ Check whether the array with flags is present
!
         FL_FLAG = .FALSE.
         IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%USER_FLAG ) ) THEN
!
! ----------- Write down the flags. Actually they are weights. 
! ----------- We write then down only if they are not 1.0
!
              DO 420 J2=1,PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND)
                 IF ( PIM%OBS(IND_OBS)%USER_FLAG(J2) < 1.0 ) THEN
                      WRITE ( UNIT=LUN, FMT=110, IOSTAT=IOS ) IND_OBS, J2, &
     &                                           PIM%OBS(IND_OBS)%USER_FLAG(J2) 
 110                  FORMAT ( I6, 2X, I4, 2X, F8.5 )
                      FL_FLAG = .TRUE.
                 END IF
 420          CONTINUE 
         END IF
         IF ( FL_FLAG ) WRITE ( UNIT=LUN, FMT='(A)' ) '# '
 410  CONTINUE 
!
! --- Write the trailer
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) PIMA__FLAG_LABEL
!
! --- Close the file and -- good bye.
!
      CLOSE ( UNIT=LUN )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_WRITE_TIME_FLAG  written file: '// &
     &             PIM%CONF%TIME_FLAG_FILE(1:I_LEN(PIM%CONF%TIME_FLAG_FILE))
      END IF
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_WRITE_TIME_FLAG  !#!  
