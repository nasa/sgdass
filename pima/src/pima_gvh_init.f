      SUBROUTINE PIMA_GVH_INIT ( PIM, GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GVH_INIT 
! *                                                                      *
! *  ### 09-JUL-2009  PIMA_GVH_INIT   v1.0 (c) L. Petrov 09-JUL-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INCLUDE   'pima_db.i'
      INCLUDE   'gvh.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( GVH__STRU  ) :: GVH
      INTEGER*4  IUER
      INTEGER*4  IER
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4511, IUER, 'PIMA_GVH_INIT', 'Error in '// &
     &                   'initialization of GVH structures' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '1 CHARACTER ASCII', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4512, IUER, 'PIMA_GVH_INIT', 'Error in writing '// &
     &                   'preamlbe of GVH structure' )
           RETURN 
      END IF
!
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '2 INTEGER*2 IEEE-231', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '3 INTEGER*4 IEEE-231', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '4 REAL*4 IEEE 754-1985', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '5 REAL*8 IEEE 754-1985', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '6 INTEGER*8 IEEE-231', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:', '81 Session', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:', '82 Scan', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:', '83 Station', IER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:', '84 Baseline', IER )
      CALL GVH_PPREA ( GVH, 1, 'GVH_VERSION:', GVH__LABEL, IER )
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PPREA ( GVH, 1, 'GENERATOR:', PIMA__LABEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4513, IUER, 'PIMA_GVH_INIT', 'Error in writing '// &
     &                   'preamlbe of GVH structure' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GVH_INIT  !#!#
