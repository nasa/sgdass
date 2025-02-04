      SUBROUTINE GVH_PLCODE ( GVH, LCODE, NOBS, NSTA, ARRAY, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_PLCODE  puts contents of lcode LCODE into the         *
! *   GVH internal data structure. The contents is the array of fixed    *
! *   dimension ARRAY which corresponds to the observation NOBS at the   *
! *   baseline with the station NSTA (either #1 or #2). The type and     *
! *   dimension of array LCODE is defined in the definition of LCODE and *
! *   stored in GVH. Routine GVH_PREPUT should be called before the      *
! *   first use of GVH_PLCODE. Contents of the array is added in the     *
! *   DATA section of the appropriate segment of GVH.                    *
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
! *    ARRAY ( UNDEFINED ) -- Array of elements to be put in the GVH.    *
! *                           Its type and dimension is defined in the   *
! *                           LCODE definition.                          *
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
! *  ### 23-NOV-2001   GVH_PLCODE   v1.3 (c) L. Petrov  01-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) ::  GVH
      CHARACTER  LCODE*(*)
      CHARACTER  STR*32, STR1*32
      INTEGER*1  ARRAY(*)
      INTEGER*4  NOBS, NSTA, IUER
      CHARACTER  DESCR*80
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA, &
     &           IND_SCA, IND_STA1, IND_STA2, POS_STA1, POS_STA2, IP, &
     &           SEG_IND, NUM_FIELDS, IER
      INTEGER*8  LCODE_I8
      ADDRESS__TYPE  IADR, ADR_DATA, ADR_CONV
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF
!
! --- Check GVH state
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4091, IUER, 'GVH_PLCODE', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4092, IUER, 'GVH_PLCODE', 'The GVH observations '// &
     &         'cache table has not been initialized' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%LCODE_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4093, IUER, 'GVH_PLCODE', 'The GVH observations '// &
     &         'cache table has not been populated' )
           RETURN
      END IF
!
      IP = LTM_DIF ( 1, GVH__NMLCODE, GVH__MLCODE, LCODE )
      IF ( IP .GT. 0 ) THEN
           CALL ERR_LOG ( 4094, IUER, 'GVH_PLCODE', 'Lcode '//LCODE// &
     &         ' is mandatory. You can put mandatory lcode into the '// &
     &         'database only by using GVH_PREPUT' )
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
           CALL ERR_LOG ( 4095, IUER, 'GVH_PLCODE', 'Error in an attempt to '// &
     &         'find lcode '//LCODE//' in cache table' )
           RETURN
      END IF
!
      IF ( CLASS == GVH__STA  .AND. ( NSTA .NE. 1  .AND.  NSTA .NE. 2 ) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NSTA, STR )
           CALL ERR_LOG ( 4096, IUER, 'GVH_PLCODE', 'Wrong value of station '// &
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
           CALL ERR_LOG ( 4097, IUER, 'GVH_PLCODE', 'Wrong value of '// &
     &         'the observation index NOBS: '//STR(1:I_LEN(STR))// &
     &         ' was found in an attempt to process request for inserting '// &
     &         'LCODE "'//LCODE//'" -- the values in the range [1, '// &
     &         STR1(1:I_LEN(STR1))//'] was expected' )
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
                CALL ERR_LOG ( 4098, IUER, 'GVH_PLCODE', 'Error in an '// &
     &              'attempt to find observation '//STR(1:I_LEN(STR))// &
     &              ' in the observations cache table' )
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
         ELSE IF ( CLASS .EQ. GVH__STA  .AND.  NSTA .EQ. 1 ) THEN
           IADR = ADR_DATA + LEN_REC*(POS_STA1-1)
         ELSE IF ( CLASS .EQ. GVH__STA  .AND.  NSTA .EQ. 2 ) THEN
           IADR = ADR_DATA + LEN_REC*(POS_STA2-1)
         ELSE IF ( CLASS .EQ. GVH__STA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NSTA, STR )
           CALL ERR_LOG ( 4099, IUER, 'GVH_PLCODE', 'Wrong station index in '// &
     &         'the baseline: '//STR(1:I_LEN(STR))//' only 1 or 2 are allowed' )
           RETURN
         ELSE IF ( CLASS .EQ. GVH__BAS ) THEN
           IADR = ADR_DATA + LEN_REC*(NOBS-1)
      END IF
!
! --- Physical copying
!
      CALL MEMCPY ( %VAL(IADR), %REF(ARRAY), %VAL(LEN_REC) )
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
!
      RETURN
      END  SUBROUTINE  GVH_PLCODE  !#!#
