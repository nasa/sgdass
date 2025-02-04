      SUBROUTINE FREE_MEM ( MEM_ADR )
! ************************************************************************
! *                                                                      *
! *   Routine FREE_MEM realeases the block of dynyamic memory with       *
! *   starting address and may provide additional useful diagnostic.     *
! *                                                                      *
! *  ###  01-AUG-97    FREE_MEM    v1.3  (c)  L. Petrov 31-MAY-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      ADDRESS__TYPE :: MEM_ADR
      INTEGER*4  LUN, IS, ARR(2)
      INTEGER*4, EXTERNAL :: GET_UNIT
!
      INTEGER*4  MEMORY_DEBUG_FLAG, MEMORY_DEBUG__YES, I88
      PARAMETER  ( MEMORY_DEBUG__YES = 1020304908 )
      COMMON     / MEMORY_DEBUG / MEMORY_DEBUG_FLAG
!
#ifdef SUN
      CALL FREE_ ( MEM_ADR )
#else
      CALL FREE  ( MEM_ADR )
#endif
!      IF ( IS .NE. 0 ) THEN
!           WRITE ( 6, * ) 'Error in memory freeing: ',IS
!!
!! -------- This code is to cause a deliberate crash in the case of FREE
!! -------- returned error code. This crash will allow to unwind stack
!! -------- which enormously facilitates diagnositc
!!
!           IS = 3
!           IS = ARR(IS)
!      END IF
!
      IF ( MEMORY_DEBUG_FLAG  .EQ.  MEMORY_DEBUG__YES ) THEN
           lun = get_unit ()
           open ( unit=lun, file='/tmp/mem.mem', status='unknown', &
     &            position='append', iostat=i88)
           write ( lun, 110 ) mem_adr
           close ( unit=lun )
 110       format ( ' free_mem:  adr = ',i18 )
      END IF
!
      MEM_ADR = 0
!
      RETURN
      END  !#!  FREE_MEM  #!#
