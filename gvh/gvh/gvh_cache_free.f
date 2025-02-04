      SUBROUTINE GVH_CACHE_FREE ( GVH, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine GVH_CACHE_FREE frees dydnamic memory allocated to the      *
! *   cache table and sets the statiosn: cashe table is not initialized. *
! *                                                                      *
! *  ### 31-JUL-2007 GVH_CACHE_FREE  v1.0 (c) L. Petrov  31-JUL-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU        ) :: GVH
      TYPE     ( GVH_LCODE2__STRU ) :: GVH_LCODE2_REC
      INTEGER*4  IUER
      CHARACTER  STR*32, STR1*32, LCODE_STR*8, DESCR_STR*80
      INTEGER*4  J1, J2, NT, I1, I2, I3, IP, DIMS(2), IER
      ADDRESS__TYPE  ADR_DATA
!
      IF ( GVH%CACHE%OBS_STATUS == GVH__INITIALIZED .OR. &
     &     GVH%CACHE%OBS_STATUS == GVH__POPULATED        ) THEN
           CONTINUE 
!
           IER = IUER
           CALL GVH_FREE ( GVH, GVH%CACHE%OBS_ADR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4461, IUER, 'GVH_CACHE_FREE', 'The error '// &
     &              'in an attempt to deallocate dynamic memory for '// &
     &              'station cache table' )
                RETURN
           END IF
!
!           IER = IUER
!           CALL GVH_FREE ( GVH, GVH%CACHE%LCODE_ADR, IER )
!           IF ( IER .NE. 0 ) THEN
!                CALL ERR_LOG ( 4462, IUER, 'GVH_CACHE_FREE', 'The error '// &
!     &              'in an attempt to deallocate dynamic memory for '// &
!     &              'station cache table' )
!                RETURN
!           END IF
!
           DO 410 J1=1,GVH%NSEG
              DO 420 J2=1,GVH%TOCS(J1)%NTOC
!                 CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(J1)%ADR), &
!     &                                  J2, GVH__GET, LCODE_STR, DESCR_STR,   &
!     &                                  I1, I2, DIMS, I3, ADR_DATA, IER )
!
!                 write ( 6, * ) ' adr_data = ', adr_data ! %%%
!                 IER = IUER
!                 CALL GVH_FREE ( GVH, ADR_DATA, IER )
!                 IF ( IER .NE. 0 ) THEN
!                      CALL ERR_LOG ( 4463, IUER, 'GVH_CACHE_FREE', &
!     &                    'The error in an attempt to deallocate '// &
!     &                    'dynamic memory for station cache table' )
!                      RETURN
!                 END IF
 420         CONTINUE 
 410       CONTINUE 
!
           GVH%CACHE%OBS_STATUS = GVH__UNDEFINED   
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_CACHE_FREE  !#!#
