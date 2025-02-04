      SUBROUTINE GHEO ( FINAM_HEO, NAME_HEO, L_HEO, STAT_HEO, ADR_HEO, &
     &                  HEO_EPOCH_SEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GHEO allocates dynamic memory for HEO data structures,     *
! *   reads input Harmonic Earth Orientation file and loads amplitudes   *
! *   and rates of change of the Harmonic Earth Orientation model into   *
! *   the data structure with address ADR_HEO.                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FINAME_HEO ( CHARACTER ) -- Name of the input file with Harmonic     *
! *                             Earth Orientation variations in HEO      *
! *                             format.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   NAME_HEO ( CHARACTER ) -- Name of the mode of harmonic variations  *
! *                             in the Earth rotation.                   *
! *      L_HEO ( INTEGER*4 ) -- The number of Harmonic Earth             *
! *                             Orientation variations constituents      *
! *                             which were found in the file.            *
! *    ADR_HEO ( INTEGER*4 ) -- Address of the array of records with the *
! *                             Harmonic Earth Orientation variations.   *
! * HEO_EPOCH_SEC ( REAL*8 ) -- Epoch of the Harmonic Earth Orientation  *
! *                             in HEO_EPOCH_JD. This epoch is used for  *
! *                             computing contribution of the rate of    *
! *                             changes of the amplitudes.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
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
! *  ### 19-SEP-2003      GHEO     v1.1 (c)  L. Petrov  10-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'heo.i'
      CHARACTER  FINAM_HEO*(*), NAME_HEO*(*)
      INTEGER*4  L_HEO, STAT_HEO, IUER
      ADDRESS__TYPE :: ADR_HEO
      REAL*8     HEO_EPOCH_SEC
      CHARACTER  STR*80
      TYPE ( HEO__STRUC ) :: HEO
      LOGICAL*4  LEX
      ADDRESS__TYPE :: MEM_LEN, MEM_ADR
      INTEGER*4  N_HEO, IER
      INTEGER*4  I_LEN
!
! --- Check whether the HEO file really exists
!
      INQUIRE ( FILE=FINAM_HEO, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8871, IUER, 'GHEO', 'File with expansion of '// &
     &         'harmonic Earth orientation parameters '// &
     &         FINAM_HEO(1:I_LEN(FINAM_HEO))//' was not found' ) 
           RETURN 
      END IF
!
! --- Grab dynamic memory for HEO data structure
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM (  IER, MEM_LEN,             MEM_ADR, 1, &
     &                      M__HEO*SIZEOF(HEO),  ADR_HEO     )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH   ( MEM_LEN, STR )
           CALL ERR_LOG ( 8872, IUER, 'GHEO', 'Error in an attempt to grab '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' ) 
           RETURN 
      END IF
!
! --- Read and parse HEO file
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL READ_HEO ( FINAM_HEO, M__HEO, L_HEO, %VAL(ADR_HEO), NAME_HEO, &
     &                HEO_EPOCH_SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8873, IUER, 'GHEO', 'Error in reading input file '// &
     &          FINAM_HEO(1:I_LEN(FINAM_HEO))//' with harmonic Earth '// &
     &         'Orientation parameter variations' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GHEO  #!#
