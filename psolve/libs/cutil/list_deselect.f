      SUBROUTINE LIST_DESELECT()
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
! *                                                                      *
! *   Subroutine  LIST_DESELECT  updates socom and prfil for deslection  *
! *   certain stations and sources. If no station or source is deslected *
! *   then nothing will be changed. But if some station(s), source(s)    *
! *   are deslected from the solution then variuables in socom and prfil *
! *   connected with them will updated and those stations/sources will   *
! *   be eliminated from that arrays.                                    *
! *                                                                      *
! *   It is necessary to do it when the first session from global batch  *
! *   solution forms the list of global paramters which will be          *
! *   augmented after runnig another sessions in forward solution.       *
! *                                                                      *
! *   Who When       What                                                *
! *                                                                      *
! *   PET 16-JAN-98  Nasty coding error corrected. (It was 41 instead    *
! *                  of J1) at the line 60                               *
! *                                                                      *
! *  ###  10-DEC-97  LIST_DESELECT  v1.1  (c) L. Petrov  16-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INTEGER*2  TOTSTA_NEW, NUMSTR_NEW, J1, J2
      LOGICAL*4  CHECK_STABIT
      LOGICAL*2  KBIT, CGM_TYPE_SAVE
!
! --- Temporary setting "No CGM_TYP" -- it is done to activate CHECK_STABIT
!
      CGM_TYPE_SAVE = CGM_TYPE
      CGM_TYPE = .FALSE.
!
      TOTSTA_NEW = 0
      DO 410 J1=1,TOTSTA
         IF ( CHECK_STABIT ( J1 ) ) THEN
!
! ----------- The J1-th station was selected in solution
!
              TOTSTA_NEW = TOTSTA_NEW + 1
!
! ----------- Transferring station-dependent stuff in socom and prfil to close
! ----------- possible hole for removal the previous station(s)
!
              ISITN_CHR(TOTSTA_NEW)   = ISITN_CHR(J1)
              VAXOF(TOTSTA_NEW)       = VAXOF(J1)
              VSITEC(1,TOTSTA_NEW)    = VSITEC(1,J1)
              VSITEC(2,TOTSTA_NEW)    = VSITEC(2,J1)
              VSITEC(3,TOTSTA_NEW)    = VSITEC(3,J1)
              VSITEV(1,TOTSTA_NEW)    = VSITEV(1,J1)
              VSITEV(2,TOTSTA_NEW)    = VSITEV(2,J1)
              VSITEV(3,TOTSTA_NEW)    = VSITEV(3,J1)
              BARO_CAL(TOTSTA_NEW)    = BARO_CAL(J1)
              BARO_HEIGHT(TOTSTA_NEW) = BARO_HEIGHT(J1)
              MONUMENTS(1,TOTSTA_NEW) = MONUMENTS(1,J1)
              MONUMENTS(2,TOTSTA_NEW) = MONUMENTS(2,J1)
              MONUMENTS(3,TOTSTA_NEW) = MONUMENTS(3,J1)
              MONUMENTS(4,TOTSTA_NEW) = MONUMENTS(4,J1)
              MONUMENTS(5,TOTSTA_NEW) = MONUMENTS(5,J1)
              VSITED(TOTSTA_NEW)      = VSITED(J1)
              PSITED(TOTSTA_NEW)      = PSITED(J1)
!CC
              IF ( KBIT ( LTIDE(1,1), J1 ) ) THEN
                   CALL SBIT ( LTIDE(1,1), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LTIDE(1,1), TOTSTA_NEW, INT2(0) )
              END IF
!
              IF ( KBIT ( LTIDE(1,2), J1 ) ) THEN
                   CALL SBIT ( LTIDE(1,2), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LTIDE(1,2), TOTSTA_NEW, INT2(0) )
              END IF
!
              IF ( KBIT ( LTIDE(1,3), J1 ) ) THEN
                   CALL SBIT ( LTIDE(1,3), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LTIDE(1,3), TOTSTA_NEW, INT2(0) )
              END IF
!CC
              IF ( KBIT ( LAXOF(1), J1 ) ) THEN
                   CALL SBIT ( LAXOF(1), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LAXOF(1), TOTSTA_NEW, INT2(0) )
              END IF
!CC
              IF ( KBIT ( LSITEC(1,1), J1 ) ) THEN
                   CALL SBIT ( LSITEC(1,1), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LSITEC(1,1), TOTSTA_NEW, INT2(0) )
              END IF
!
              IF ( KBIT ( LSITEC(1,2), J1 ) ) THEN
                   CALL SBIT ( LSITEC(1,2), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LSITEC(1,2), TOTSTA_NEW, INT2(0) )
              END IF
!
              IF ( KBIT ( LSITEC(1,3), J1 ) ) THEN
                   CALL SBIT ( LSITEC(1,3), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LSITEC(1,3), TOTSTA_NEW, INT2(0) )
              END IF
!CC
              IF ( KBIT ( LSITEV(1,1), J1 ) ) THEN
                   CALL SBIT ( LSITEV(1,1), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LSITEV(1,1), TOTSTA_NEW, INT2(0) )
              END IF
!
              IF ( KBIT ( LSITEV(1,2), J1 ) ) THEN
                   CALL SBIT ( LSITEV(1,2), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LSITEV(1,2), TOTSTA_NEW, INT2(0) )
              END IF
!
              IF ( KBIT ( LSITEV(1,3), J1 ) ) THEN
                   CALL SBIT ( LSITEV(1,3), TOTSTA_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LSITEV(1,3), TOTSTA_NEW, INT2(0) )
              END IF
         END IF
 410  CONTINUE
      TOTSTA = TOTSTA_NEW
!C
      NUMSTR_NEW = 0
      DO 420 J2=1,NUMSTR
         IF ( KBIT ( ISRSEL(1), J2 ) ) THEN
!
! ----------- J2-th source was selected in solutiuon
!
              NUMSTR_NEW = NUMSTR_NEW + 1
!
! ----------- Transferring source-dependent stuff in socom and prfil to close
! ----------- possible hole for removal the previous source(s)
!
              ISTRN_CHR(NUMSTR_NEW) = ISTRN_CHR(J2)
              VSTARC(1,NUMSTR_NEW)  = VSTARC(1,J2)
              VSTARC(2,NUMSTR_NEW)  = VSTARC(2,J2)
!C
              IF ( KBIT ( LSTAR(1,1), J2 ) ) THEN
                   CALL SBIT ( LSTAR(1,1), NUMSTR_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LSTAR(1,1), NUMSTR_NEW, INT2(0) )
              END IF
!
              IF ( KBIT ( LSTAR(1,2), J2 ) ) THEN
                   CALL SBIT ( LSTAR(1,2), NUMSTR_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LSTAR(1,2), NUMSTR_NEW, INT2(0) )
              END IF
!C
              IF ( KBIT ( LPROP(1,1), J2 ) ) THEN
                   CALL SBIT ( LPROP(1,1), NUMSTR_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LPROP(1,1), NUMSTR_NEW, INT2(0) )
              END IF
!
              IF ( KBIT ( LPROP(1,2), J2 ) ) THEN
                   CALL SBIT ( LPROP(1,2), NUMSTR_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( LPROP(1,2), NUMSTR_NEW, INT2(0) )
              END IF
!C
              IF ( KBIT ( ISRSEL(1), J2 ) ) THEN
                   CALL SBIT ( ISRSEL(1), NUMSTR_NEW, INT2(1) )
                ELSE
                   CALL SBIT ( ISRSEL(1), NUMSTR_NEW, INT2(0) )
              END IF
         END IF
 420  CONTINUE
      NUMSTR = NUMSTR_NEW
!
! --- Restoring CGM_TYPE status
!
      CGM_TYPE = CGM_TYPE_SAVE
!
      RETURN
      END  !#!  LIST_DESELECT  #!#
