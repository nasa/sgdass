      SUBROUTINE GVH_EXCH_LCODE1 ( FL_ENDIAN_SWAP, GVH_LCODE1, ITOC, OPCODE, &
     &                             LCODE, DESCR, CLASS, TYP, DIMS, LEN_DATA, &
     &                             ADR_DATA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_EXCH_LCODE1
! *                                                                      *
! * ### 27-NOV-2001  GVH_EXCH_LCODE1  v1.0 (c) L. Petrov 27-NOV-2001 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      LOGICAL*4  FL_ENDIAN_SWAP
      CHARACTER  LCODE*(*), DESCR*(*)
      INTEGER*4  ITOC, OPCODE, CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA, &
     &           IUER
      ADDRESS__TYPE :: ADR_DATA
      TYPE ( GVH_LCODE1__STRU ) ::   GVH_LCODE1(ITOC)
!
      IF ( OPCODE .EQ. GVH__PUT ) THEN
           CALL CLRCH ( GVH_LCODE1(ITOC)%LCODE )
           CALL CLRCH ( GVH_LCODE1(ITOC)%DESCR )
           GVH_LCODE1(ITOC)%LCODE    = LCODE
           GVH_LCODE1(ITOC)%DESCR    = DESCR
           GVH_LCODE1(ITOC)%CLASS    = CLASS
           GVH_LCODE1(ITOC)%TYP      = TYP
           GVH_LCODE1(ITOC)%DIMS(1)  = DIMS(1)
           GVH_LCODE1(ITOC)%DIMS(2)  = DIMS(2)
           GVH_LCODE1(ITOC)%LEN_DATA = LEN_DATA
           GVH_LCODE1(ITOC)%ADR_DATA = ADR_DATA
         ELSE IF ( OPCODE .EQ. GVH__GET ) THEN
           CALL CLRCH ( LCODE )
           CALL CLRCH ( DESCR )
           LCODE    = GVH_LCODE1(ITOC)%LCODE
           DESCR    = GVH_LCODE1(ITOC)%DESCR
           CLASS    = GVH_LCODE1(ITOC)%CLASS
           TYP      = GVH_LCODE1(ITOC)%TYP
           DIMS(1)  = GVH_LCODE1(ITOC)%DIMS(1)
           DIMS(2)  = GVH_LCODE1(ITOC)%DIMS(2)
           LEN_DATA = GVH_LCODE1(ITOC)%LEN_DATA
           ADR_DATA = GVH_LCODE1(ITOC)%ADR_DATA
!
           IF ( FL_ENDIAN_SWAP ) THEN
!
! ------------- Big/Little Enedian swapping
!
                CALL ENDIAN_CNV_I4 ( CLASS    )
                CALL ENDIAN_CNV_I4 ( TYP      )
                CALL ENDIAN_CNV_I4 ( DIMS(1)  )
                CALL ENDIAN_CNV_I4 ( DIMS(2)  )
                CALL ENDIAN_CNV_I4 ( LEN_DATA )
#ifdef ADR_32BIT
                CALL ENDIAN_CNV_I4 ( ADR_DATA )
#else
                CALL ENDIAN_CNV_I8 ( ADR_DATA )
#endif
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  !#!  GVH_EXCH_LCODE1  #!#
