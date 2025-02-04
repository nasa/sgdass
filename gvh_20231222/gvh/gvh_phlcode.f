      SUBROUTINE GVH_PHLCODE ( GVH, LCODE, NOBS, NSTA, DIM1, DIM2, ARRAY, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_PHLCODE  puts contents of lcode LCODE into the        *
! *   GVH internal data structure. The contents is the array of          *
! *   variable dimension ARRAY which corresponds to the observation NOBS *
! *   at the baseline with the station NSTA (either #1 or #2). The type  *
! *   and and maximal dimension of array LCODE is defined in the         *
! *   definition of LCODE and stored in GVH. Routine GVH_PREPUT should   *
! *   be called before the first use of GVH_PHLCODE. Contents of the     *
! *   array is added in the HEAP section of the appropriate segment of   *
! *   GVH.                                                               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    LCODE ( CHARACTER ) -- 8-characters long LCODE name.              *
! *     NOBS ( INTEGER*4 ) -- Observation index. This argument is        *
! *                           ignored if LCODE is of session class.      *
! *     NSTA ( INTEGER*4 ) -- Station index of the NOBS-th observation:  *
! *                           1 or 2. 1 means the reference station of   *
! *                           the baseline, 2 means remote station.      *
! *                           This argument is used only if LCODE is     *
! *                           of station class, and ignored otherwise.   *
! *    DIM1  ( INTEGER*4 ) -- First actual dimension of the array ARRAY  *
! *    DIM2  ( INTEGER*4 ) -- Second actual dimension of the array ARRAY *
! *    ARRAY ( UNDEFINED ) -- Array of elements to be put in the GVH.    *
! *                           Its type is defined in the LCODE           *
! *                           definition.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      GVH ( GVH__STRU ) -- Data structure which keeps internal        *
! *                           information related to the database of     *
! *                           an astro/geo VLBI experiment.              *
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
! *  ### 02-DEC-2001   GVH_PHLCODE  v1.3 (c) L. Petrov  22-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) ::  GVH
      TYPE     ( GVH_DESC__STRU ) ::  GVH_DESC
      CHARACTER  LCODE*(*)
      CHARACTER  DESCR*80, STR*32, STR1*32
      INTEGER*4  NOBS, NSTA, DIM1, DIM2, ARRAY(*), IUER
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA, &
     &           IND_SCA, IND_STA1, IND_STA2, POS_STA1, POS_STA2, IP, &
     &           SEG_IND, NUM_FIELDS, LEN_HEAP, IER
      INTEGER*8  LCODE_I8
      ADDRESS__TYPE ADR_DATA, ADR_CONV, IADR
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4241, IUER, 'GVH_PHLCODE', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4242, IUER, 'GVH_PHLCODE', 'The GVH observations '// &
     &         'cache table has not been populated' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%LCODE_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4243, IUER, 'GVH_PHLCODE', 'The GVH observations '// &
     &         'cache table has not been populated' )
           RETURN
      END IF
!
      IP = LTM_DIF ( 1, GVH__NMLCODE, GVH__MLCODE, LCODE )
      IF ( IP .GT. 0 ) THEN
           CALL ERR_LOG ( 4244, IUER, 'GVH_PHLCODE', 'Lcode '//LCODE// &
     &         ' is mandatory. You can put mandatory lcode into the '// &
     &         'database only by using GVH_ACTIVATE' )
           RETURN
      END IF
!
      IF ( DIM1 .LT. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM1, STR )
           CALL ERR_LOG ( 4245, IUER, 'GVH_PHLCODE', 'Wrong value of '// &
     &                   'parameter DIM1: '//STR )
           RETURN
      END IF
!
      IF ( DIM2 .LT. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM2, STR )
           CALL ERR_LOG ( 4246, IUER, 'GVH_PHLCODE', 'Wrong value of '// &
     &                   'parameter DIM2: '//STR )
           RETURN
      END IF
!
! --- Get inquiry of the lcode
!
      IER = IUER
      CALL MEMCPY ( LCODE_I8, LCODE )
      CALL GVH_LCODE_TAB_INQ ( %VAL(GVH%CACHE%LCODE_ADR), GVH%CACHE%NUM_LCODE, &
     &                         LCODE, LCODE_I8, GVH%LCODE_CACHE_I8, &
     &                         GVH%IND_LCODE_CACHE, DESCR, CLASS, TYP, DIMS, &
     &                         LEN_REC, LEN_DATA, SEG_IND, NUM_FIELDS, &
     &                         ADR_DATA, ADR_CONV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4247, IUER, 'GVH_PHLCODE', 'Error in an attempt '// &
     &         'to find lcode '//LCODE//' in cache table' )
           RETURN
      END IF
!
! --- Chec dimensions
!
      IF ( DIMS(1) .GT. 0 ) THEN
           CALL ERR_LOG ( 4248, IUER, 'GVH_PHLCODE', 'Lcode '//LCODE// &
     &         ' was not described as an array of varaible length, it is '// &
     &         'the arrays of fixed length. Please, use GET_PLCODE '// &
     &         'for putting values of this lcode' )
           RETURN
      END IF
!
      IF ( DIM1 .GT. -DIMS(1) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM1, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( -DIMS(1), STR )
           CALL ERR_LOG ( 4249, IUER, 'GVH_PHLCODE', 'Wrong value of '// &
     &                   'parameter DIM1: '//STR(1:I_LEN(STR))// &
     &                   ' -- it exceeded the maximal dimension '//STR1 )
           RETURN
      END IF
!
      IF ( DIM2 .GT. -DIMS(2) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM2, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( -DIMS(2), STR )
           CALL ERR_LOG ( 4250, IUER, 'GVH_PHLCODE', 'Wrong value of '// &
     &                   'parameter DIM2: '//STR(1:I_LEN(STR))// &
     &                   ' -- it exceeded the maximal dimension '//STR1 )
           RETURN
      END IF
!
      IF ( CLASS == GVH__STA  .AND. ( NSTA .NE. 1  .AND.  NSTA .NE. 2 ) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NSTA, STR )
           CALL ERR_LOG ( 4251, IUER, 'GVH_PHLCODE', 'Wrong value of station '// &
     &         'index NSTA: '//STR(1:I_LEN(STR))//' was found in an attempt '// &
     &         'to process request for inserting '//'LCODE "'//LCODE// &
     &         '" -- values 1 or 2 are expected' )
           RETURN
      END IF
!
      IF ( ( CLASS == GVH__SCA  .OR.  &
     &       CLASS == GVH__STA  .OR.  &
     &       CLASS == GVH__BAS        ) .AND. &
             NOBS .LE. 0  .OR.  NOBS .GT. GVH%CACHE%NUM_OBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NOBS, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( GVH%CACHE%NUM_OBS, STR1 )
           CALL ERR_LOG ( 4252, IUER, 'GVH_PHLCODE', 'Wrong value of '// &
     &         'the observation index NOBS: '//STR(1:I_LEN(STR))// &
     &         ' was found in an attempt to process request for inserting '// &
     &         'LCODE "'//LCODE//'" -- the values in the range [1, '// &
     &         STR1(1:I_LEN(STR1))//' was expected' )
           RETURN
      END IF
!
      IF ( CLASS .NE. GVH__SES ) THEN
           IER = IUER
           CALL GVH_OBS_TAB_INQ ( GVH%CACHE%NUM_OBS, %VAL(GVH%CACHE%OBS_ADR), &
     &                            NOBS, IND_SCA, IND_STA1, IND_STA2, POS_STA1, &
     &                            POS_STA2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( NOBS, STR )
                CALL ERR_LOG ( 4253, IUER, 'GVH_PHLCODE', 'Error in an '// &
     &              'attempt to find observation '//STR(1:I_LEN(STR))// &
     &              ' in the observations cache table' )
                RETURN
           END IF
      END IF
!
! --- Learn the size of heap mempory to be allocated for the array of variable
! --- length...
!
      LEN_HEAP = GVH__TYPE_LEN(TYP)*DIM1*DIM2
      GVH_DESC%DIMS(1) = DIM1
      GVH_DESC%DIMS(2) = DIM2
!
! --- ... and allocate this memory
!
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, LEN_HEAP, GVH_DESC%ADR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LEN_HEAP, STR )
           CALL ERR_LOG ( 4254, IUER, 'GVH_PHLCODE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for LCODE '// &
     &          LCODE )
           RETURN
      END IF
!
! --- Compute the address were we will put descriptor
!
      IF ( CLASS .EQ. GVH__SES ) THEN
           IADR = ADR_DATA
         ELSE IF ( CLASS .EQ. GVH__SCA ) THEN
           IADR = ADR_DATA + SIZEOF(GVH_DESC)*(IND_SCA-1)
         ELSE IF ( CLASS .EQ. GVH__STA  .AND.  NSTA .EQ. 1 ) THEN
           IADR = ADR_DATA + SIZEOF(GVH_DESC)*(POS_STA1-1)
         ELSE IF ( CLASS .EQ. GVH__STA  .AND.  NSTA .EQ. 2 ) THEN
           IADR = ADR_DATA + SIZEOF(GVH_DESC)*(POS_STA2-1)
         ELSE IF ( CLASS .EQ. GVH__STA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NSTA, STR )
           CALL ERR_LOG ( 4255, IUER, 'GVH_PHLCODE', 'Wrong station index '// &
     &         'in the baseline: '//STR(1:I_LEN(STR))//' only 1 or 2 are '// &
     &         'allowed' )
          RETURN
         ELSE IF ( CLASS .EQ. GVH__BAS ) THEN
           IADR = ADR_DATA + LEN_REC*(NOBS-1)
      END IF
!
! --- Physical copying the desciptor descriptor
!
      CALL MEMCPY ( %VAL(IADR), GVH_DESC, %VAL(SIZEOF(GVH_DESC)) )
!
! --- Physical copying contents of the array to its place in HEAP
!
      CALL MEMCPY ( %VAL(GVH_DESC%ADR), %REF(ARRAY), %VAL(LEN_HEAP) )
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_PHLCODE  !#!#
