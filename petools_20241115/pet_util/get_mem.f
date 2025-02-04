      SUBROUTINE GET_MEM ( MEM_SIZE, MEM_ADR )
! ************************************************************************
! *                                                                      *
! *   Routine GET_MEM allocates MEM_SIZE bytes of memory. The first      *
! *   address of the allocated chunk of MEM_ADR. Memory is properly      *
! *   aligned.                                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *                                                                      *
! *   MEM_SIZE ( INTEGER*8 ) -- Size of memory in bytes.                 *
! *                             Unsigned integer.                        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *                                                                      *
! *   MEM_ADR  ( INTEGER*8 ) -- Address of the allocated memory on       *
! *                             success, 0 on failure.                   *
! *                                                                      *
! *  ### 23-APR-1992    GET_MEM    v1.6 (c)  L. Petrov  18-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      ADDRESS__TYPE :: MEM_SIZE, MEM_ADR
      ADDRESS__TYPE :: IS
      INTEGER*4  LUN, GET_UNIT
      INTEGER*4  ALGN
!!      PARAMETER  ( ALGN = 64 ) ! memory alignment: 64 bytes, i.e. 512 bits
      PARAMETER  ( ALGN = 512 ) ! memory alignment: 512 bytes, i.e. 1 page
      INTEGER*4  ARR(2)
!
      INTEGER*4  MEMORY_DEBUG_FLAG, MEMORY_DEBUG__YES, I88
      PARAMETER  ( MEMORY_DEBUG__YES = 1020304908 )
      COMMON     / MEMORY_DEBUG / MEMORY_DEBUG_FLAG
#if defined LINUX || defined DARWIN || defined SUN 
      ADDRESS__TYPE :: MALLOC, POSIX_MEMALIGN 
#endif
!
      IF ( MEMORY_DEBUG_FLAG  .EQ.  MEMORY_DEBUG__YES ) THEN
           lun = get_unit ()
           open ( unit=lun, file='/tmp/mem.mem', status='unknown', &
     &            position='append', iostat=i88 )
      END IF
!
#if defined LINUX || defined DARWIN || defined SUN 
      IS = POSIX_MEMALIGN ( MEM_ADR, %VAL(ALGN), %VAL(MEM_SIZE) )
      IF ( IS .NE. 0 ) MEM_ADR = 0
#endif
#ifdef HPUX
      CALL MALLOC_ ( ALGN+MEM_SIZE, MEM_ADR )
#endif
#ifdef SUN
      MEM_ADR = MALLOC ( %VAL(ALGN+MEM_SIZE) )
#endif
!
      IF ( MEMORY_DEBUG_FLAG  .EQ.  MEMORY_DEBUG__YES ) THEN
           write ( lun, 110 ) mem_adr, mem_size
           close ( unit=lun )
 110       format ( '  get_mem:  adr = ',i18, ' size=', i18 )
           write ( 6, 110 ) mem_adr, mem_size
      END IF
!
      RETURN
      END  SUBROUTINE  GET_MEM  !#!#
