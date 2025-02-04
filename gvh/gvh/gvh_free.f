      SUBROUTINE GVH_FREE ( GVH, ADR, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine GVH_FREE
! *                                                                      *
! *  ### 02-DEC-2001   GVH_FREE    v1.0 (c)  L. Petrov  02-DEC-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE ( GVH__STRU ) ::  GVH
      CHARACTER  STR*32
      INTEGER*4  IUER
      ADDRESS__TYPE  ADR
      INTEGER*4  J1
!
      IF ( ADR == 0 ) THEN
           CALL ERR_LOG ( 0, IUER, ' ', ' ' )
           RETURN 
      END IF
!
      DO 410 J1=1,GVH%DMA
         IF ( GVH%MEMADR(J1) .EQ. ADR ) THEN
              CALL FREE_MEM ( GVH%MEMADR_ORIG(J1) )
              GVH%DMS = GVH%DMS - GVH%MEMLEN(J1)
              GVH%MEMADR(J1) = 0
              GVH%MEMADR_ORIG(J1) = 0
              GVH%MEMLEN(J1) = 0
              ADR = 0
              CALL ERR_LOG ( 0, IUER, ' ', ' ' )
              RETURN
         END IF
 410  CONTINUE
!
      CALL CLRCH ( STR )
      CALL INCH8 ( ADR, STR )
      CALL ERR_LOG ( 4222, IUER, 'GVH_FREE', 'Trap of internal control: '// &
     &              'attempt to free the address of memory which was not '// &
     &              'allocated by GVH: '//STR )
      RETURN
      END  !#!  GVH_FREE  #!#
!
