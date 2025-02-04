      SUBROUTINE GVH_OBS_TAB_INQ ( NUMOBS, OBS_TAB, IND_BAS, IND_SCA, &
     &                           IND_STA1, IND_STA2, POS_STA1, POS_STA2, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_OBS_TAB_INQ
! *                                                                      *
! * ### 23-NOV-2001  GVH_OBS_TAB_INQ  v1.0 (c) L. Petrov 23-NOV-2001 ### *
! *                                                                      *
! ************************************************************************
      INCLUDE   'gvh.i'
      INTEGER*4  NUMOBS, IND_BAS, IND_SCA, IND_STA1, IND_STA2, POS_STA1, &
     &           POS_STA2, IUER
      TYPE ( GVH_OBS__STRU ) ::  OBS_TAB(NUMOBS)
      CHARACTER  STR*32, STR1*32
!
      IF ( IND_BAS .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_BAS, STR )
           CALL ERR_LOG ( 4121, IUER, 'GVH_OBS_TAB_INQ', 'Wrong value of '// &
     &         ' IND_BAS: '//STR )
           RETURN
      END IF
!
      IF ( IND_BAS .GT. NUMOBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_BAS, STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( NUMOBS,  STR1 )
           CALL ERR_LOG ( 4121, IUER, 'GVH_OBS_TAB_INQ', 'Wrong value of '// &
     &         ' IND_BAS: '//STR(1:I_LEN(STR))//' -- it exceeds NUMOBS: '// &
     &           STR1 )
           RETURN
      END IF
!
      IND_SCA  = OBS_TAB(IND_BAS)%IND_SCA
      IND_STA1 = OBS_TAB(IND_BAS)%IND_STA1
      IND_STA2 = OBS_TAB(IND_BAS)%IND_STA2
      POS_STA1 = OBS_TAB(IND_BAS)%POS_STA1
      POS_STA2 = OBS_TAB(IND_BAS)%POS_STA2
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  !#!  GVH_OBS_TAB_INQ  #!#
