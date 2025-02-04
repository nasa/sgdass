      SUBROUTINE GVH_DTOC ( GVH, LCODE, ISEG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_DTOC  removes the definition of the old lcode from    *
! *   the  table of contents of the ISEG-th segment.                     *
! *   NB: If a mandatory LCODE is deleted, all other lcodes should       *
! *   be updated!!!                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    LCODE ( CHARACTER ) -- 8-characters long LCODE name.              *
! *     ISEG ( INTEGER*4 ) -- Index of the segment where the lcode       *
! *                           definition will be added. The index should *
! *                           be in the range of available segments.     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GVH ( GVH__STRU      ) -- Data structure which keeps internal    *
! *                               information related to the database of *
! *                               an astro/geo VLBI experiment.          *
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
! *  ### 31-JUL-2007     GVH_DTOC    v1.1 (c) L. Petrov 24-JUN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) ::  GVH
      CHARACTER  LCODE*(*)
      INTEGER*4  ISEG, IUER
      CHARACTER  STR*32, STR1*32, LCODE_STR*8, DESCR_STR*80
      INTEGER*4  J1, J2, J3, J4, NT, I1, I2, I3, IP, DIMS(2), IER
      ADDRESS__TYPE :: I4
      INTEGER*4  IND_LCODE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IND_LCODE = 0
      DO 410 J1=1,GVH%TOCS(ISEG)%NTOC
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), &
     &                           J1, GVH__GET, LCODE_STR, DESCR_STR,   &
     &                           I1, I2, DIMS, I3, I4, IER )
         IF ( LCODE_STR .EQ. LCODE ) THEN
              IND_LCODE = J1
         END IF
 410  CONTINUE 
!
      IF ( IND_LCODE == 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG( 4471, IUER, 'GVH_DTOC', 'Lcode '//LCODE// &
     &         ' was not found in the '//STR(1:I_LEN(STR))//'-th segment' )
           RETURN 
      END IF
!
      IF ( IND_LCODE < GVH%TOCS(ISEG)%NTOC ) THEN
           DO 420 J2=IND_LCODE,GVH%TOCS(ISEG)%NTOC-1
              CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), &
     &                               J2+1, GVH__GET, LCODE_STR, DESCR_STR,   &
     &                               I1, I2, DIMS, I3, I4, IER )
              CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), &
     &                               J2, GVH__PUT, LCODE_STR, DESCR_STR,   &
     &                               I1, I2, DIMS, I3, I4, IER )
 420       CONTINUE 
      END IF
      GVH%TOCS(ISEG)%NTOC = GVH%TOCS(ISEG)%NTOC-1
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVH_DTOC  !#!#
