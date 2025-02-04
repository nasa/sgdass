      SUBROUTINE GVH_ALLOCATE ( GVH, LEN, ADR, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine  GVH_ALLOCATE  allocates space for an internal   *
! *   data structure kept in GVH structure.                              *
! *                                                                      *
! *  ### 23-NOV-2001  GVH_ALLOCATE  v1.1 (c) L. Petrov  04-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INTEGER*4  LEN, IUER
      ADDRESS__TYPE ADR
      TYPE    ( GVH__STRU ) ::  GVH
      ADDRESS__TYPE ADR_ORIG, IS
      CHARACTER  STR*32, STR1*32
      INTEGER*4  ALGN
      PARAMETER  ( ALGN = 16 )
      INTEGER*4  DEBUG
!
      INTEGER*4, EXTERNAL :: I_LEN
      ADDRESS__TYPE, EXTERNAL :: ADDRESS_ALIGN
#if defined LINUX || defined DARWIN || defined SUN
      ADDRESS__TYPE :: MALLOC, POSIX_MEMALIGN 
#endif
!
      DEBUG = 0
      ADR = 0
      ADR_ORIG = 0
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4101, IUER, 'GVH_ALLOCATE', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
! --- Allocate dynamic memory
!
#if defined LINUX || defined DARWIN || defined SUN
      IS = POSIX_MEMALIGN ( ADR_ORIG, %VAL(ALGN), %VAL(LEN+ALGN) )
      IF ( IS .NE. 0 ) ADR_ORIG = 0
#else
      CALL MALLOC_ ( LEN+ALGN, ADR_ORIG ) 
#endif
      IF ( ADR_ORIG .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LEN, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( GVH%DMS, STR1 )
           CALL ERR_LOG ( 4102, IUER, 'GVH_ALLOCATE', 'The error in attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory. '// &
     &         'GVH has already allocated '//STR1(1:I_LEN(STR1))// &
     &         ' bytes of dynamic memory' )
           RETURN
      END IF
      ADR = ADDRESS_ALIGN ( ADR_ORIG, ALGN )
!
! --- Keep the descriptor of the area of allocated dynamic memory
!
      GVH%DMA = GVH%DMA + 1
      IF ( GVH%DMA .GT. GVH__MSLOTS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( GVH__MSLOTS, STR )
           CALL ERR_LOG ( 4103, IUER, 'GVH_ALLOCATE', 'GVH exceeded amount '// &
     &         'of internal slots for keeping dynamic memory: GVH__MSLOTS: '// &
     &          STR )
           RETURN
      END IF
      IF ( DEBUG == 1 ) THEN
           WRITE ( 6, * ) 'GVH_ALLOCATE dma= ', GVH%DMA, ' adr= ', ADR_ORIG, ' len= ', LEN
      END IF
      GVH%MEMADR_ORIG(GVH%DMA) = ADR_ORIG
      GVH%MEMADR(GVH%DMA) = ADR
      GVH%MEMLEN(GVH%DMA) = LEN + ALGN
      GVH%DMS = GVH%DMS + LEN
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_ALLOCATE  !#!#
