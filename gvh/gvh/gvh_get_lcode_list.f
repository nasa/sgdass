      SUBROUTINE GVH_GET_LCODE_LIST ( GVH, M_LCD, L_LCD, C_LCD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVH_GET_LCODE_LIST 
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      GVH ( GVH__STRU ) -- Data structure which keeps internal        *
! *                           information related to the database of     *
! *                           an astro/geo VLBI experiment.              *
! *    M_LCD ( INTEGER*4 ) -- The maximum number of lcodes.              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    L_LCD ( INTEGER*4 ) -- The number of loaded lcodes.               *
! *    C_LCD ( CHARACTER ) -- arrays of lcodes. Dimension: L_LCD         *
! *                                                                      *
! * ## 01-AUG-2007  GVH_GET_LCODE_LIST v1.0 (c) L. Petrov 01-AUG-2007 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU      ) ::  GVH
      INTEGER*4  M_LCD, L_LCD, IUER
      CHARACTER  C_LCD(M_LCD)*(*)
      CHARACTER  STR*32, STR1*32
      INTEGER*4  J1
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4471, IUER, 'GVH_GET_LCODE_LIST', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4472, IUER, 'GVH_GET_LCODE_LIST', 'The GVH '// &
     &         'observation cache table has not been populated' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%LCODE_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4473, IUER, 'GVH_GET_LCODE_LIST', 'The GVH '// &
     &         'lcode cache table has not been populated' )
           RETURN
      END IF
!
      IF ( LEN(C_LCD(1)) .NE. 8 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LEN(C_LCD(1)), STR )
           CALL ERR_LOG ( 4474, IUER, 'GVH_GET_LCODE_LIST', 'The length '// &
     &         'of C_LCD chracater array is '//STR(1:I_LEN(STR))// &
     &         ' but 8 is epxected' )
           RETURN
      END IF
!
      L_LCD = GVH%CACHE%NUM_LCODE
      IF ( L_LCD > M_LCD ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M_LCD, STR  )
           CALL INCH  ( L_LCD, STR1 )
           CALL ERR_LOG ( 4475, IUER, 'GVH_GET_LCODE_LIST', 'Input '// &
     &         'parameter M_LCD is too small: '//STR(1:I_LEN(STR))// &
     &         ' while the database contains '//STR1(1:I_LEN(STR1))// &
     &         ' lcodes' )
           RETURN 
      END IF
!
      DO 410 J1=1,L_LCD
         CALL MEMCPY ( %REF(C_LCD(J1)), GVH%LCODE_CACHE_I8(J1), %VAL(8) )
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE GVH_GET_LCODE_LIST  !#!#
