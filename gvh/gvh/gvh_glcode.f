      SUBROUTINE GVH_GLCODE ( GVH, LCODE, IND_OBS, IND_STA, MLEN, &
     &                        ADIM1, ADIM2, ARRAY, IUER )
! ************************************************************************
! *                                                                      *
! *   Routene  GVH_GLODE  gets contents of lcode LCODE into the          *
! *   GVH internal data structure. The contents is the array of ARRAY    *
! *   corresponds to the observation with index IND_OBS at the baseline  *
! *   with the station IND_STA (either #1 or #2). The type and dimension *
! *   of array LCODE is defined in the definition of LCODE and stored    *
! *   in GVH. Routine GVH_PREGET should be called before the first use   *
! *   of GVH_GLCODE.                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      GVH ( GVH__STRU ) -- Data structure which keeps internal        *
! *                           information related to the database of     *
! *                           an astro/geo VLBI experiment.              *
! *    LCODE ( CHARACTER ) -- 8-characters long LCODE name.              *
! *  IND_OBS ( INTEGER*4 ) -- Observation index. This argument is        *
! *                           ignored if LCODE is of session class.      *
! *  IND_STA ( INTEGER*4 ) -- Station index of the NOBS-th observation:  *
! *                           1 or 2. 1 means the reference station of   *
! *                           the baseline, 2 means remote station.      *
! *                           This argument is used only if LCODE is     *
! *                           of station class, and ignored otherwise.   *
! *     MLEN ( INTEGER*4 ) -- Maximal size of elements in the output     *
! *                           array ARRAY in bytes.                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    ADIM1 ( INTEGER*4 ) -- First dimension of the output array ARRAY. *
! *    ADIM1 ( INTEGER*4 ) -- Second dimension of the output array ARRAY.*
! *    ARRAY ( UNDEFINED ) -- Array of elements to be put in the GVH.    *
! *                           Its type and dimension is defined in the   *
! *                           LCODE definition. In the case of arrays of *
! *                           variable lenght LCODE definition defines   *
! *                           the maximal dimensions.                    *
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
! *  ### 27-NOV-2001   GVH_GLCODE   v1.4 (c) L. Petrov  05-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU      ) ::  GVH
      TYPE     ( GVH_DESC__STRU ) ::  GVH_DESC
      CHARACTER  LCODE*(*)
      INTEGER*4  IND_OBS, IND_STA, MLEN, ADIM1, ADIM2, IUER
      INTEGER*4  ARRAY(*)
      CHARACTER  DESCR*80, STR*32, STR1*32
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA, &
     &           IND_SCA, IND_STA1, IND_STA2, POS_STA1, POS_STA2, IP, &
     &           SEG_IND, NUM_FIELDS, J1, IER
      INTEGER*8  LCODE_I8
      ADDRESS__TYPE  ADR_DATA, ADR_CONV, IADR
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4271, IUER, 'GVH_GLCODE', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4272, IUER, 'GVH_GLCODE', 'The GVH observations '// &
     &         'cache table has not been populated' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%LCODE_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4273, IUER, 'GVH_GLCODE', 'The GVH lcode '// &
     &         'cache table has not been populated' )
           RETURN
      END IF
!
      IF ( MLEN < 1 ) THEN
           CALL ERR_LOG ( 4274, IUER, 'GVH_GLCODE', 'Trap of internal '// &
     &         'control: wrong parameter MLEN: '//TRIM(STR)//' while '// &
     &         'a positive integer was expected' )
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
           CALL ERR_LOG ( 4275, IUER, 'GVH_GLCODE', 'Error in an attempt to '// &
     &         'find lcode '//LCODE//' in cache table' )
           RETURN
      END IF
      IF ( CLASS == GVH__STA .AND. ( IND_STA .NE. 1 .AND. IND_STA .NE. 2 ) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_STA, STR )
           CALL ERR_LOG ( 4276, IUER, 'GVH_GLCODE', 'Wrong value of station '// &
     &         'index IND_STA: '//STR(1:I_LEN(STR))//' was found in an attempt '// &
     &         'to process request for reading '//'LCODE "'//LCODE// &
     &         '" -- values 1 or 2 are expected' )
           RETURN
      END IF
!
      IF ( ( CLASS == GVH__SCA  .OR.  &
     &       CLASS == GVH__STA  .OR.  &
     &       CLASS == GVH__BAS        ) .AND. &
             IND_OBS .LE. 0  .OR.  IND_OBS .GT. GVH%CACHE%NUM_OBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_OBS, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( GVH%CACHE%NUM_OBS, STR1 )
           CALL ERR_LOG ( 4277, IUER, 'GVH_GLCODE', 'Wrong value of '// &
     &         'the observation index IND_OBS: '//STR(1:I_LEN(STR))// &
     &         ' was found in an attempt to process request for reading '// &
     &         'LCODE "'//LCODE//'" -- the values in the range [1, '// &
     &         STR1(1:I_LEN(STR1))//' was expected' )
           RETURN
      END IF
!
      IF ( CLASS .NE. GVH__SES ) THEN
           IER = IUER
           CALL GVH_OBS_TAB_INQ ( GVH%CACHE%NUM_OBS, %VAL(GVH%CACHE%OBS_ADR), &
     &                      IND_OBS, IND_SCA, IND_STA1, IND_STA2, POS_STA1, &
     &                      POS_STA2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IND_OBS, STR )
                CALL ERR_LOG ( 4278, IUER, 'GVH_GLCODE', 'Error in an '// &
     &              'attempt to find observation '//STR(1:I_LEN(STR))// &
     &              ' in the observations cache table' )
                RETURN
           END IF
      END IF
!
      IF ( DIMS(1) .GT. 0  ) THEN
           ADIM1 = DIMS(1)
           ADIM2 = DIMS(2)
!
! -------- Normal LCODE (not HEAP)
!
           IF ( DIMS(1)*DIMS(2)*GVH__TYPE_LEN(TYP) .GT. MLEN ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( MLEN, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( DIMS(1)*DIMS(2)*GVH__TYPE_LEN(TYP), STR1 )
                CALL ERR_LOG ( 4279, IUER, 'GVH_GLCODE', 'Error in '// &
     &              'retrieving lcode '//LCODE//' -- parameter MLEN: '// &
     &               STR(1:I_LEN(STR))//' was not sufficient, since the '// &
     &              'actual data length is '//STR1(1:I_LEN(STR1))//' bytes' )
                RETURN
           END IF
      END IF
!
! --- Compute the address from which we have to copy the data
!
      IF ( CLASS .EQ. GVH__SES ) THEN
           IADR = ADR_DATA
         ELSE IF ( CLASS .EQ. GVH__SCA ) THEN
           IADR = ADR_DATA + LEN_REC*(IND_SCA-1)
         ELSE IF ( CLASS .EQ. GVH__STA  .AND.  IND_STA .EQ. 1 ) THEN
           IADR = ADR_DATA + LEN_REC*(POS_STA1-1)
         ELSE IF ( CLASS .EQ. GVH__STA  .AND.  IND_STA .EQ. 2 ) THEN
           IADR = ADR_DATA + LEN_REC*(POS_STA2-1)
         ELSE IF ( CLASS .EQ. GVH__STA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_STA, STR )
           CALL ERR_LOG ( 4280, IUER, 'GVH_GLCODE', 'Wrong station '// &
     &                   'index in the baseline: '//STR(1:I_LEN(STR))// &
     &                   ' only 1 or 2 are allowed' )
           RETURN
         ELSE IF ( CLASS .EQ. GVH__BAS ) THEN
           IADR = ADR_DATA + LEN_REC*(IND_OBS-1)
      END IF
!
! --- Physical copying
!
      IF ( DIMS(1) .GT. 0  ) THEN
!
! -------- Normal LCODE (not HEAP)
!
           CALL MEMCPY ( %REF(ARRAY), %VAL(IADR), %VAL(LEN_REC) )
         ELSE
!
! -------- HEAP LCODE. First copy descriptor
!
           CALL MEMCPY ( %REF(GVH_DESC), %VAL(IADR), %VAL(LEN_REC) )
           ADIM1 = GVH_DESC%DIMS(1)
           ADIM2 = GVH_DESC%DIMS(2)
           IF ( GVH%ENDIAN_SWAP ) THEN
                CALL ENDIAN_CNV_I4 ( ADIM1 )
                CALL ENDIAN_CNV_I4 ( ADIM2 )
           END IF
           IF ( ADIM1*ADIM2*GVH__TYPE_LEN(TYP) .GT. MLEN ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( MLEN, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( ADIM1*ADIM2*GVH__TYPE_LEN(TYP), STR1 )
                CALL ERR_LOG ( 4281, IUER, 'GVH_GLCODE', 'Error in '// &
     &              'retrieving lcode '//LCODE//' -- parameter MLEN: '// &
     &               STR(1:I_LEN(STR))//' was not sufficient, since the '// &
     &              'actual data length is '//STR(1:I_LEN(STR1))//' bytes' )
                RETURN
           END IF
!
           IF ( ADIM1*ADIM2*GVH__TYPE_LEN(TYP) .GT. 0 ) THEN
!
! ------------- Then copy array from the heap
!
                CALL MEMCPY ( %REF(ARRAY), %VAL(GVH_DESC%ADR), &
     &                        %VAL(ADIM1*ADIM2*GVH__TYPE_LEN(TYP)) )
           END IF
      END IF
!
      IF ( GVH%ENDIAN_SWAP ) THEN
           IF ( TYP .EQ. GVH__I2 ) THEN
                CALL ENDIAN_ARRAY_CNV_I2 ( ADIM1*ADIM2, ARRAY )
              ELSE IF ( TYP .EQ. GVH__I4 ) THEN
                CALL ENDIAN_ARRAY_CNV_I4 ( ADIM1*ADIM2, ARRAY )
              ELSE IF ( TYP .EQ. GVH__R4 ) THEN
                CALL ENDIAN_ARRAY_CNV_R4 ( ADIM1*ADIM2, ARRAY )
              ELSE IF ( TYP .EQ. GVH__R8 ) THEN
                CALL ENDIAN_ARRAY_CNV_R8 ( ADIM1*ADIM2, ARRAY )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_GLCODE  !#!#
