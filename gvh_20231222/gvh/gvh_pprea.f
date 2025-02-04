      SUBROUTINE GVH_PPREA ( GVH, ISEG, KEYWORD, VALUE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_PPREA puts the record to preamble of the ISEG-th      *
! *   section. The record is added to the end of the list of records.    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISEG ( INTEGER*4 ) -- Index of the segment where the preamble    *
! *                           section will be sought. The index should   *
! *                           be in the range of available segments.     *
! *                                                                      *
! *  KEYWORD ( CHARACTER ) -- String with the keyword. The keyword       *
! *                           itself should not contain characters with  *
! *                           decimal codes 10 and 26.                   *
! *   VALUE ( CHARACTER  ) -- String with the value which corresponds to *
! *                           this keyword. The value itself should not  *
! *                           contain characters with decimal codes 10   *
! *                           and 26.                                    *
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
! *  ### 20-NOV-2001   GVH_PPREA   v2.0 (c)  L. Petrov  18-NOV-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE      ( GVH__STRU ) ::  GVH
      CHARACTER  KEYWORD*(*), VALUE*(*)
      INTEGER*4  ISEG, IUER
      CHARACTER  STR*32, STR1*32
      INTEGER*4  NK, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4021, IUER, 'GVH_PPREA', 'The GVH data structure '// &
     &         'was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
! --- Check validity of ISEG argument
!
      IF ( ISEG .LE.  0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4022, IUER, 'GVH_PPREA', 'Wrong argument '// &
     &         'ISEG '//STR )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH__MSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH__MSEG, STR1 )
           CALL ERR_LOG ( 4023, IUER, 'GVH_PPREA', 'Wrong argument '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the '// &
     &         'maximal number of segments GVH__MSEG: '//STR1 )
           RETURN
      END IF
!
! --- Increment the keywords counter
!
      GVH%PREA(ISEG)%NKWD = GVH%PREA(ISEG)%NKWD  + 1
      IF ( GVH%PREA(ISEG)%NKWD  .GT.  GVH__MKWD ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( GVH__MKWD, STR )
           CALL ERR_LOG ( 4024, IUER, 'GVH_PPREA', 'The number of keywords '// &
     &         'in preamble section exceed the current limit GVH__MKWD: '// &
     &          STR )
           RETURN
      END IF
      NK = GVH%PREA(ISEG)%NKWD ! for shorting
!
      GVH%PREA(ISEG)%KWD_LEN(NK) = I_LEN(KEYWORD)+1
!
! --- Allocate dynamic memory for keeping the keyword
!
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, GVH%PREA(ISEG)%KWD_LEN(NK), &
     &                         GVH%PREA(ISEG)%KWD_ADR(NK), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4025, IUER, 'GVH_PPREA', 'The error in attempt to '// &
     &         'allocate memory for a preamble keyword' )
           RETURN
      END IF
!
! --- Copy the keyword
!
      CALL MEMCPY ( %VAL(GVH%PREA(ISEG)%KWD_ADR(NK)), &
     &              %REF(KEYWORD(1:I_LEN(KEYWORD))//GVH__KEYWORD_DEL), &
     &              %VAL(GVH%PREA(ISEG)%KWD_LEN(NK))  )
!
      GVH%PREA(ISEG)%VAL_LEN(NK) = I_LEN(VALUE)+1
!
! --- Allocate dynamic memory for keeping the value
!
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, GVH%PREA(ISEG)%VAL_LEN(NK), &
     &                         GVH%PREA(ISEG)%VAL_ADR(NK), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4026, IUER, 'GVH_PPREA', 'The error in attempt to '// &
     &         'allocate memory for a preamble value' )
           RETURN
      END IF
!
! --- Copy the value
!
      CALL MEMCPY ( %VAL(GVH%PREA(ISEG)%VAL_ADR(NK)), &
     &              %REF(VALUE(1:I_LEN(VALUE))//GVH__VALUE_DEL), &
     &              %VAL(GVH%PREA(ISEG)%VAL_LEN(NK))  )
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_PPREA  !#!#
