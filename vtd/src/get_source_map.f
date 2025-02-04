      SUBROUTINE A_GET_SOURCE_MAP ( FINAM, MAP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_SOURCE_MAP reads the source map in FITS format and    *
! *   loads it into the object MAP.                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FINAM ( CHARACTER )  -- name of the file with map in FITS format.   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    MAP ( RECORD    )  -- object which keeps information about the    *
! *                          map.                                        *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
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
! * ## 26-FEB-2004   GET_SOURCE_MAP  v1.1 (c)  L. Petrov  01-MAR-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'sou_map.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      CHARACTER  FINAM*(*)
      INTEGER*4  IUER
      CHARACTER  COMMENT*128, STR*32
      REAL*8     PI, PI2, P2I
      PARAMETER  ( PI=3.141592653589793D0, PI2=PI*2.D0, P2I = PI/2.0D0 )
      LOGICAL*4  ANYF
      REAL*4     NULLVAL 
      INTEGER*4  STATUS, UNIT, READWRITE, BLOCKSIZE, NKEYS, NSPACE
      INTEGER*4  BITPIX, INUM, HDU_TYPE, GROUP, IOS, J1, J2, J3, IER
      INTEGER*4  GET_UNIT, I_LEN
      INTEGER*4  LOCC, MV, I, J
      LOCC(MV,I,J) = ( MV*(J-1) + I )
!
      READWRITE = 0
      UNIT = GET_UNIT ( )
!
! --- Open fits file
!
      STATUS = 0
      CALL FTOPEN_ ( UNIT, FINAM, READWRITE, BLOCKSIZE, STATUS )
      IF ( STATUS .NE. 0 ) THEN
           CALL FTPRINTERROR ( STATUS )
           CALL ERR_LOG ( 4811, IUER, 'GET_SOURCE_MAP', 'Error in opening '// &
     &                   'fits file '//FINAM )
           RETURN
      END IF
!
! --- Read the header
!
      CALL FTGHSP_ ( UNIT, NKEYS, NSPACE, STATUS )
      IF ( STATUS .NE. 0 ) THEN
           CALL FTPRINTERROR ( STATUS )
           CALL ERR_LOG ( 4812, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &         'to read the header of fits file '//FINAM )
           CALL FTCLOS_ ( UNIT, STATUS )
           RETURN
      END IF
!
! --- REad necesary keys
!
      DO 410 J1=1,NKEYS
         CALL FTGKYJ_ ( UNIT, 'BITPIX', BITPIX, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4813, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword BITPIX from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
!
         CALL FTGKYJ_ ( UNIT, 'NAXIS1', MAP%DIM1, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4814, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword NAXIS1 from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
!
         CALL FTGKYJ_ ( UNIT, 'NAXIS2', MAP%DIM2, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4815, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword NAXIS2 from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
!
         CALL FTGKYD_ ( UNIT, 'CDELT1', MAP%STEP_RA, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4816, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword CDELT1 from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
         MAP%STEP_RA = MAP%STEP_RA*PI/180.0
!
         CALL FTGKYD_ ( UNIT, 'CDELT2', MAP%STEP_DL, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4817, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword CDELT2 from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
         MAP%STEP_DL = MAP%STEP_DL*PI/180.0
!
         CALL FTGKYD_ ( UNIT, 'CRVAL3', MAP%FREQ, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4818, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword CRVAL3 from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
         MAP%FREQ = PI2*MAP%FREQ
!
         CALL FTGKYS_ ( UNIT, 'OBJECT', MAP%NAME, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4819, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword OBJECT from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
!
         CALL FTGKYD_ ( UNIT, 'DATAMAX', MAP%FLUX_MAX, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4820, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword DATAMAX from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
!
         CALL FTGKYD_ ( UNIT, 'BMAJ', MAP%BEAM_MAJ, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4821, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword BMAJ from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
!
         CALL FTGKYD_ ( UNIT, 'BMIN', MAP%BEAM_MIN, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4822, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword BMIN from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
!
         CALL FTGKYD_ ( UNIT, 'BPA', MAP%BEAM_POS_ANG, COMMENT, STATUS )
         IF ( STATUS .NE. 0 ) THEN
              CALL FTPRINTERROR ( STATUS )
              CALL ERR_LOG ( 4823, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &            'to get the keyword BMIN from the header of the fits file' )
              CALL FTCLOS_ ( UNIT, STATUS )
              RETURN
         END IF
         MAP%BEAM_POS_ANG = MAP%BEAM_POS_ANG*PI/180.0D0
 410  CONTINUE 
!
! --- Learn the number of HDU 
!
      CALL FTTHDU_ ( UNIT, INUM, STATUS )
!
! --- Position top the first image in the file
!
      CALL FTMAHD_ ( UNIT, 1, HDU_TYPE, STATUS )
!
! --- Check whether ther memory for the image was allocated. If allocated, then
! --- free it, since amount of allocated memory may change
!
      IF ( ASSOCIATED(MAP%IMAGE) ) THEN
           DEALLOCATE ( MAP%IMAGE ) 
      END IF
!
! --- Allocate memory for the image
!
      ALLOCATE ( MAP%IMAGE(MAP%DIM1,MAP%DIM2), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MAP%DIM1*MAP%DIM2, STR )
           CALL ERR_LOG ( 4824, IUER, 'VTD_MOMENT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '//     &
     &         'memory' )
           RETURN 
      END IF
!
      CALL NOUT ( 4*MAP%DIM1*MAP%DIM2, MAP%IMAGE )
      NULLVAL = 0.0
      GROUP = 1
!
! --- Read the image
!
      CALL FTGPVE_ ( UNIT, GROUP, 1, MAP%DIM1*MAP%DIM2, NULLVAL, &
     &               MAP%IMAGE, ANYF, STATUS )
      IF ( STATUS .NE. 0 ) THEN
           CALL FTPRINTERROR ( STATUS )
           CALL ERR_LOG ( 4825, IUER, 'GET_SOURCE_MAP', 'Error in attempt '// &
     &         'to get the image from the fits file' )
           CALL FTCLOS_ ( UNIT, STATUS )
           RETURN
      END IF
!
      CALL FTCLOS_ ( UNIT, STATUS )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#! SUBROUTINE  GET_SOURCE_MAP  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FTPRINTERROR ( STATUS )
! ************************************************************************
! *                                                                      *
! * This subroutine prints out the descriptive text corresponding to the *
! * error status value and prints out the contents of the internal       *
! * error message stack generated by FITSIO whenever an error occurs.    *
! *                                                                      *
! ************************************************************************
      INTEGER*4 STATUS
      CHARACTER ERRTEXT*30, ERRMESSAGE*80
!
! --- Check if status is OK (no error); if so, simply return
!
      IF ( STATUS .LE. 0 ) RETURN
!
! --- The FTGERR subroutine returns a descriptive 30-character text string that
! --- corresponds to the integer error status number.  A complete list of all
! --- the error numbers can be found in the back of the FITSIO User's Guide.
! 
      CALL FTGERR_ ( STATUS, ERRTEXT )
      WRITE ( 6, * ) 'FITSIO Error Status =',status,': ',errtext
!
! --- FITSIO usually generates an internal stack of error messages whenever
! --- an error occurs.  These messages provide much more information on the
! --- cause of the problem than can be provided by the single integer error
! --- status value.  The FTGMSG subroutine retrieves the oldest message from
! --- the stack and shifts any remaining messages on the stack down one
! --- position.  FTGMSG is called repeatedly until a blank message is
! --- returned, which indicates that the stack is empty.  Each error message
! --- may be up to 80 characters in length.  Another subroutine, called
! --- FTCMSG, is available to simply clear the whole error message stack in
! --- cases where one is not interested in the contents.
!
      CALL FTGMSG_ ( ERRMESSAGE )
      DO WHILE ( ERRMESSAGE .NE. ' ' )
         WRITE ( 6, '(A)' ) ERRMESSAGE(1:I_LEN(ERRMESSAGE))
         CALL FTGMSG_ ( ERRMESSAGE )
      END DO
      END  SUBROUTINE  FTPRINTERROR 
