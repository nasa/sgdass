      SUBROUTINE GVH_INIT ( GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_INIT initializes GVH. It should be called before      *
! *   the first use of GVH.                                              *
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
! *  ### 20-NOV-2001   GVH_INIT    v1.0 (c)  L. Petrov  20-NOV-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE ( GVH__STRU ) ::  GVH
      INTEGER*4  IUER
      INTEGER*4  LEN_GVH
!
      CALL NOUT ( SIZEOF(GVH), GVH )
      GVH%NSEG = 1
      GVH%SEG = 1
      GVH%ENDIAN_SWAP = .FALSE.
      GVH%STATUS = GVH__INITIALIZED
!!      write ( 6, * ) ' len_gvh=',len_gvh ! %%
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_INIT  !#!#
