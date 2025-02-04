      SUBROUTINE REPA_INQLINE ( REP, IND_BAS, IND_CLR, IND_PT, OUT )
! ************************************************************************
! *                                                                      *
! *   Function REPA_INQLINE is the quit-function called by REPA.         *
! *                                                                      *
! * ### 07-DEC-2004    REPA_INQLINE   v1.0 (c) L. Petrov 07-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'repa.i'
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  IND_BAS, IND_CLR, IND_PT
      CHARACTER  OUT*(*)
!
      CALL CLRCH ( OUT )
!
      OUT = 'Example '
      RETURN
      END  SUBROUTINE  REPA_NQLINE
