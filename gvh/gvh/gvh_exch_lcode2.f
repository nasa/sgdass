      SUBROUTINE GVH_EXCH_LCODE2 ( GVH_LCODE2, ITOC, OPCODE, LCODE, CLASS, &
     &                             TYP, DIMS, LEN_REC, LEN_DATA, SEG_IND, &
     &                             NUM_FIELDS, ADR_DATA, ADR_CONV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_EXCH_LCODE2
! *                                                                      *
! * ### 27-NOV-2001  GVH_EXCH_LCODE2  v2.0 (c) L. Petrov 03-NOV-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      CHARACTER  LCODE*8
      INTEGER*4  ITOC
      INTEGER*4  OPCODE
      INTEGER*4  CLASS
      INTEGER*4  TYP
      INTEGER*4  DIMS(2)
      INTEGER*4  LEN_REC
      INTEGER*4  LEN_DATA
      INTEGER*4  NUM_FIELDS
      INTEGER*4  SEG_IND
      ADDRESS__TYPE ADR_DATA
      ADDRESS__TYPE ADR_CONV
      INTEGER*4  IUER
      TYPE ( GVH_LCODE2__STRU ) ::   GVH_LCODE2(ITOC)
!
      IF ( OPCODE .EQ. GVH__PUT ) THEN
           CALL CLRCH ( GVH_LCODE2(ITOC)%LCODE )
           GVH_LCODE2(ITOC)%LCODE      = LCODE
           GVH_LCODE2(ITOC)%CLASS      = CLASS
           GVH_LCODE2(ITOC)%TYP        = TYP
           GVH_LCODE2(ITOC)%DIMS(1)    = DIMS(1)
           GVH_LCODE2(ITOC)%DIMS(2)    = DIMS(2)
           GVH_LCODE2(ITOC)%LEN_REC    = LEN_REC
           GVH_LCODE2(ITOC)%LEN_DATA   = LEN_DATA
           GVH_LCODE2(ITOC)%SEG_IND    = SEG_IND
           GVH_LCODE2(ITOC)%NUM_FIELDS = NUM_FIELDS
           GVH_LCODE2(ITOC)%ADR_DATA   = ADR_DATA
           GVH_LCODE2(ITOC)%ADR_CONV   = ADR_CONV
         ELSE IF ( OPCODE .EQ. GVH__GET ) THEN
           CALL CLRCH ( LCODE )
           LCODE      = GVH_LCODE2(ITOC)%LCODE
           CLASS      = GVH_LCODE2(ITOC)%CLASS
           TYP        = GVH_LCODE2(ITOC)%TYP
           DIMS(1)    = GVH_LCODE2(ITOC)%DIMS(1)
           DIMS(2)    = GVH_LCODE2(ITOC)%DIMS(2)
           LEN_REC    = GVH_LCODE2(ITOC)%LEN_REC
           LEN_DATA   = GVH_LCODE2(ITOC)%LEN_DATA
           SEG_IND    = GVH_LCODE2(ITOC)%SEG_IND    
           NUM_FIELDS = GVH_LCODE2(ITOC)%NUM_FIELDS
           ADR_DATA   = GVH_LCODE2(ITOC)%ADR_DATA
           ADR_CONV   = GVH_LCODE2(ITOC)%ADR_CONV
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_EXCH_LCODE2  !#!#
