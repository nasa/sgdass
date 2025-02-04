      SUBROUTINE GVH_GPREA ( GVH, ISEG, IND_REC, KEYWORD, VALUE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_GPREA  gets the IND_REC-th record from the preamble   *
! *   of the ISEG-th section: the keyword and the value. The argument    *
! *   ISEG should be in the range of available segments. The argument    *
! *   IND_REC should be in the range [1, GVH%PREA(ISEG)%NKWD].           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      GVH ( GVH__STRU ) -- Data structure which keeps internal        *
! *                           information related to the database of     *
! *                           an astro/geo VLBI experiment.              *
! *     ISEG ( INTEGER*4 ) -- Index of the segment where the preamble    *
! *                           section will be sought. The index should   *
! *                           be in the range of available segments.     *
! *  IND_REC ( INTEGER*4 ) -- The record index.                          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
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
! *  ###  16-NOV-2005  GVH_GPREA   v1.0 (c)  L. Petrov  16-NOV-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      CHARACTER  KEYWORD*(*), VALUE*(*)
      CHARACTER  STR*32, STR1*32, STR2*32
      INTEGER*4  ISEG, IND_REC, IUER
      TYPE ( GVH__STRU ) ::  GVH
      INTEGER*4  LN, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4121, IUER, 'GVH_GPREA', 'The GVH data structure '// &
     &         'was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
! --- Check validity of ISEG argument
!
      IF ( ISEG .LE.  0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4122, IUER, 'GVH_GPREA', 'Wrong argument '// &
     &         'ISEG '//STR )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH__MSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH__MSEG, STR1 )
           CALL ERR_LOG ( 4123, IUER, 'GVH_GPREA', 'Wrong argument '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the '// &
     &         'maximal number of segments GVH__MSEG: '//STR1 )
           RETURN
      END IF
!
! --- Chgeck the counter
!
      IF ( IND_REC > GVH%PREA(ISEG)%NKWD ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_REC, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( ISEG, STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( GVH%PREA(ISEG)%NKWD, STR2 )
           CALL ERR_LOG ( 4124, IUER, 'GVH_GPREA', 'Wrong parameter '// &
     &          'IND_REC: '//STR(1:I_LEN(STR))//' it exceeded the number of '// &
     &          'keywords in the preamble section in '//STR1(1:I_LEN(STR1))// &
     &          ' segment: '//STR2 )
           RETURN
      END IF
!
! --- Copy the keyword ...
!
      LN = MIN ( GVH%PREA(ISEG)%KWD_LEN(IND_REC), LEN(KEYWORD) )
      CALL MEMCPY ( %REF(KEYWORD), %VAL(GVH%PREA(ISEG)%KWD_ADR(IND_REC)), &
     &                             %VAL(LN) )
!
! --- ... and the value
!
      LN = MIN ( GVH%PREA(ISEG)%VAL_LEN(IND_REC), LEN(VALUE)   )
      CALL MEMCPY ( %REF(VALUE),   %VAL(GVH%PREA(ISEG)%VAL_ADR(IND_REC)), &
     &                             %VAL(LN) )
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_GPREA  !#!#
