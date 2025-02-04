      SUBROUTINE GVH_INQ_LCODE ( GVH, LCODE, DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_INQ_LCODE  searches for lcode LCODE in the GVH        *
! *   internal data structure and returns information about it: type,    *
! *   dimensions, number of fields, segment index, record length, length *
! *   of the section with data and the address of the data. If the lcode *
! *   is not found, the output parameters are zero, and IUER is zero     *
! *   as well. Failure to find lcode is not considered as an error.      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      GVH ( GVH__STRU ) -- Data structure which keeps internal        *
! *                           information related to the database of     *
! *                           an astro/geo VLBI experiment.              *
! *    LCODE ( CHARACTER ) -- 8-characters long LCODE name.              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      DESCR ( CHARACTER ) -- Lcode short description.                 *
! *      CLASS ( INTEGER*4 ) -- Class of the parameter. One of           *
! *                             GVH__SES -- session class                *
! *                             GVH__SCA -- scan class                   *
! *                             GVH__STA -- station class                *
! *                             GVH__BAS -- baseline class               *
! *        TYP ( INTEGER*4 ) -- Type of the parameter. One of            *
! *                             GVH__C1 -- character*1                   *
! *                             GVH__I2 -- integer*2                     *
! *                             GVH__I4 -- integer*4                     *
! *                             GVH__R4 -- real*4                        *
! *                             GVH__R8 -- real*8                        *
! *                             GVH__I8 -- integer*8                     *
! *       DIMS ( INTEGER*4 ) -- Array of lcode dimensions:               *
! *                             DIMS(1) -- first dimension               *
! *                             DIMS(2) -- second dimension              *
! * NUM_FIELDS ( INTEGER*4 ) -- Total number of fields occupied by       *
! *                             lcode -- the number of arrays, each of   *
! *                             DIMS(1)xDIMS(2)                          *
! *    SEG_IND ( INTEGER*4 ) -- Segment index.                           *
! *    LEN_REC ( INTEGER*4 ) -- Record length -- i.e the length of the   *
! *                             array associated with lcode in bytes.    *
! *   LEN_DATA ( INTEGER*4 ) -- Length of the entire section in bytes    *
! *                             for all arrays lcodes associated with    *
! *                             the lcode.                               *
! *   ADR_DATA ( ADDRESS   ) -- Address of the first element of the      *
! *                             lcode.                                   *
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
! *  ### 26-NOV-2005 GVH_INQ_LCODE  v1.2 (c) L. Petrov  04-SEP-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU      ) ::  GVH
      TYPE     ( GVH_DESC__STRU ) ::  GVH_DESC
      CHARACTER  LCODE*(*)
      INTEGER*4  IUER
      CHARACTER  DESCR*(*)
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA, &
     &           IND_SCA, IND_STA1, IND_STA2, POS_STA1, POS_STA2, IP, &
     &           SEG_IND, NUM_FIELDS, J1, IER
      INTEGER*8  LCODE_I8
      ADDRESS__TYPE ADR_DATA, ADR_CONV
      CHARACTER  STR*32, STR1*32
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4281, IUER, 'GVH_INQ_LCODE', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4282, IUER, 'GVH_INQ_LCODE', 'The GVH '// &
     &         'observations cache table has not been populated' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%LCODE_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4283, IUER, 'GVH_INQ_LCODE', 'The GVH '// &
     &         'observations cache table has not been populated' )
           RETURN
      END IF
!
! --- Get inquiry of the lcode
!
      IER = 0
      CALL MEMCPY ( LCODE_I8, LCODE )
      CALL GVH_LCODE_TAB_INQ ( %VAL(GVH%CACHE%LCODE_ADR), GVH%CACHE%NUM_LCODE, &
     &                         LCODE, LCODE_I8, GVH%LCODE_CACHE_I8, &
     &                         GVH%IND_LCODE_CACHE, DESCR, CLASS, TYP, DIMS, &
     &                         LEN_REC, LEN_DATA, SEG_IND, NUM_FIELDS, &
     &                         ADR_DATA, ADR_CONV, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( DESCR )
           CLASS      = 0
           TYP        = 0
           DIMS(1)    = 0 
           DIMS(2)    = 0
           NUM_FIELDS = 0
           SEG_IND    = 0
           LEN_REC    = 0
           LEN_DATA   = 0
           ADR_DATA   = 0
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_INQ_LCODE !#!#
