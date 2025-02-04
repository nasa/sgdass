      SUBROUTINE GET_MEM32 ( MEM_SIZE, MEM_ADR )
! ************************************************************************
! *                                                                      *
! *   Routine GET_MEM32 allocates MEM_SIZE bytes of memory. The first    *
! *   address of the allocated chunk of MEM_ADR. Memory is properly      *
! *   aligned.                                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   MEM_SIZE ( INTEGER*4 ) -- Size of memory in bytes.                 *
! *                             Unsigned integer.                        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *                                                                      *
! *   MEM_ADR  ( INTEGER*4 ) -- Address of the allocated memory on       *
! *                             success, 0 on failure.                   *
! *                                                                      *
! *  ### 23-APR-1992    GET_MEM32   v1.3 (c)  L. Petrov  12-JUN-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MEM_SIZE, MEM_ADR, LUN, IS, GET_UNIT
      INTEGER*4  ALGN
      PARAMETER  ( ALGN = 64 ) ! memory alignment: 64 bytes, i.e. 512 bits
!
      INTEGER*4  MEMORY_DEBUG_FLAG, MEMORY_DEBUG__YES, I88
      PARAMETER  ( MEMORY_DEBUG__YES = 1020304908 )
      COMMON     / MEMORY_DEBUG / MEMORY_DEBUG_FLAG
#if defined LINUX || defined DARWIN || defined SUN 
      INTEGER*4  MALLOC, POSIX_MEMALIGN 
#endif
!
      IF ( MEMORY_DEBUG_FLAG  .EQ.  MEMORY_DEBUG__YES ) THEN
           lun = get_unit ()
           open ( unit=lun, file='/tmp/mem.mem', status='unknown', &
     &            position='append', iostat=i88 )
      END IF
!
#if defined LINUX || defined DARWIN
      IS = POSIX_MEMALIGN ( MEM_ADR, %VAL(ALGN), %VAL(MEM_SIZE) )
      IF ( IS .NE. 0 ) MEM_ADR = 0
#endif
#ifdef HPUX
      CALL MALLOC_ ( MEM_SIZE, MEM_ADR )
#endif
#ifdef SUN
      MEM_ADR = MALLOC ( %VAL(MEM_SIZE) )
#endif
!
      IF ( MEMORY_DEBUG_FLAG  .EQ.  MEMORY_DEBUG__YES ) THEN
           write ( lun, 110 ) mem_adr, mem_size
           close ( unit=lun )
 110       format ( '  get_mem:  adr = ',i18,' size=',i12 )
      END IF
!
      RETURN
      END  !#!  GET_MEM32  #!#
