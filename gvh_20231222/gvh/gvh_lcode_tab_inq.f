      SUBROUTINE GVH_LCODE_TAB_INQ ( GVH_LCODE2, NUM_LCODE, LCODE, &
     &                               LCODE_I8, LCODE_TAB_I8, IND_LCODE_CACHE, &
     &                               DESCR, CLASS, TYP, DIMS, LEN_REC, &
     &                               LEN_DATA, SEG_IND, NUM_FIELDS, ADR_DATA, &
     &                               ADR_CONV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_LCODE_TAB_INQ
! *                                                                      *
! * ### 23-NOV-2001 GVH_LCODE_TAB_INQ v2.0 (c) L. Petrov 03-NOV-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INTEGER*4  NUM_LCODE, CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA, SEG_IND, &
     &           NUM_FIELDS, IND_LCODE_CACHE(NUM_LCODE), &
     &           IUER
      ADDRESS__TYPE :: ADR_DATA, ADR_CONV
      INTEGER*8  LCODE_I8, LCODE_TAB_I8(NUM_LCODE)
      CHARACTER  LCODE*(*), DESCR*(*)
      CHARACTER  LCODE_TRIAL*8
      TYPE     ( GVH_LCODE2__STRU ) ::  GVH_LCODE2(NUM_LCODE)
      INTEGER*4  IL, IR, IM, J1, IND_CACHE, IND_LCODE, IER
!
      IF ( NUM_LCODE .LE. 0 ) THEN
           CALL ERR_LOG ( 4111, IUER, 'GVH_LCODE_TAB_INQ', 'Lcode '//LCODE// &
     &         ' was not found in cache table since the cache table is empty' )
           RETURN
      END IF
!
! --- Binary search
!
      IL = 1
      IR = NUM_LCODE
 910  CONTINUE 
         IM = IL + (IR - IL)/2
         IF ( IL .GT. IR ) THEN
              IND_CACHE = 0
              GOTO 810
         END IF
         IF ( LCODE_I8 .LT. LCODE_TAB_I8(IM) ) THEN
              IR=IM-1
              GOTO 910
            ELSE IF ( LCODE_I8 .GT. LCODE_TAB_I8(IM) ) THEN
              IL=IM+1
              GOTO 910
            ELSE IF ( LCODE_I8 .EQ. LCODE_TAB_I8(IM) ) THEN
              IND_CACHE = IM
              GOTO 810
         END IF               
 810  CONTINUE 
      IF ( IND_CACHE == 0 ) THEN
           DO 410 J1=1,NUM_LCODE
              IF ( IUER .EQ. -1 ) THEN
                   WRITE ( 6, * ) ' J1=',J1,' LCODE=', GVH_LCODE2(J1)%LCODE
              END IF
 410       CONTINUE 
           CALL ERR_LOG ( 4112, IUER, 'GVH_LCODE_TAB_INQ', 'Lcode '//LCODE// &
     &         ' was not found in cache table' )
           RETURN 
      END IF
!
      IND_LCODE   = IND_LCODE_CACHE(IND_CACHE)
      LCODE_TRIAL = GVH_LCODE2(IND_LCODE)%LCODE
      DESCR       = GVH_LCODE2(IND_LCODE)%DESCR
      CLASS       = GVH_LCODE2(IND_LCODE)%CLASS
      TYP         = GVH_LCODE2(IND_LCODE)%TYP
      DIMS(1)     = GVH_LCODE2(IND_LCODE)%DIMS(1)
      DIMS(2)     = GVH_LCODE2(IND_LCODE)%DIMS(2)
      LEN_REC     = GVH_LCODE2(IND_LCODE)%LEN_REC
      LEN_DATA    = GVH_LCODE2(IND_LCODE)%LEN_DATA
      SEG_IND     = GVH_LCODE2(IND_LCODE)%SEG_IND    
      NUM_FIELDS  = GVH_LCODE2(IND_LCODE)%NUM_FIELDS
      ADR_DATA    = GVH_LCODE2(IND_LCODE)%ADR_DATA
      ADR_CONV    = GVH_LCODE2(IND_LCODE)%ADR_CONV
!
      IF ( LCODE_TRIAL .EQ. LCODE ) THEN
           CALL ERR_LOG ( 0, IUER, ' ', ' ' )
           RETURN
         ELSE 
           CALL ERR_LOG ( 4113, IUER, 'GVH_LCODE_TAB_INQ', 'Lcode '//LCODE// &
     &         ' was not found in cache table' )
           RETURN 
      END IF
      END  SUBROUTINE  GVH_LCODE_TAB_INQ  !#!#
