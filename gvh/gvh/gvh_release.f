      SUBROUTINE GVH_RELEASE ( GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVH_RELEASE releases all dynamic memory structure          *
! *   allocated by GVH and initializes GVH.                              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GVH ( GVH__STRU  ) -- Data structure which keeps internal        *
! *                           information related to the database of     *
! *                           an astro/geo VLBI experiment.              *
! *    IUER ( INTEGER*4  ) -- Universal error handler.                   *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 17-NOV-2005  GVH_RELEASE  v1.0 (c)  L. Petrov  17-NOV-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) ::  GVH
      CHARACTER  STR*32
      INTEGER*4  IUER
      ADDRESS__TYPE  ADR
      INTEGER*4  J1
!
      IF ( GVH%DMA > 0 ) THEN
!
! -------- Release dynamic memory
!
           DO 410 J1=GVH%DMA,1,-1
              CALL FREE_MEM ( GVH%MEMADR_ORIG(J1) )
              GVH%DMS = GVH%DMS - GVH%MEMLEN(J1)
              GVH%MEMADR(J1) = 0
              GVH%MEMADR_ORIG(J1) = 0
              GVH%MEMLEN(J1) = 0
 410       CONTINUE
      END IF
!
! --- Initialize GVH
!
      CALL NOUT ( SIZEOF(GVH), GVH )
      GVH%NSEG = 1
      GVH%SEG = 1
      GVH%ENDIAN_SWAP = .FALSE.
      GVH%STATUS = GVH__INITIALIZED
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_RELEASE  !#!#

