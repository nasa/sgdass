      SUBROUTINE PIMA_FRG_MERGE ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRG_MERGE updates the data structure by merging       *
! *   frequency table of several frequency groups into one group.        *
! *   The merged group overwrites the first frequency groups. This can   *
! *   be done for a case when visibilities for the same accumulation     *
! *   period (AP) are spread over several frequency groups. This         *
! *   operation allows to process intermediate frequencies of several    *
! *   frequency groups together.                                         *
! *                                                                      *
! *   NB: frequency merging is not applicable for a case when they are   *
! *       not recorded simultaneously, i.e. at moment t(k) the i-th      *
! *       group is observed, and at moment t(k+1) the j-th group is      *
! *       observed.                                                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     PIM ( PIMA__TYPE ) -- Object with information related to package *
! *                           PIMA.                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ### 23-NOV-2014  PIMA_FRG_MERGE  v1.1 (c) L. Petrov  20-OCT-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  IUER
      TYPE     ( PIM_FRQ__TYPE ) :: FRQ(PIM__MFRQ)
      REAL*8,    POINTER         :: FREQ_ARR(:,:,:) => NULL()
      CHARACTER  STR*128
      INTEGER*4  NFRQ, NFRG, IFRQ, J1, J2, J3, J4, J5, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( .NOT. ( PIM%CONF%FRG_LIST(1) == 1 .AND.  &
     &             PIM%CONF%FRG_LIST(2) == PIM%NFRG ) ) THEN
           WRITE ( 6, * ) ' PIM%CONF%FRG_LIST(1:2) = ', PIM%CONF%FRG_LIST(1:2) 
           WRITE ( 6, * ) ' PIM%NFRG = ', PIM%NFRG
           CALL ERR_LOG ( 8151, IUER, 'PIMA_FRG_MERGE', 'Currently, only '// &
     &         'a case when the 1st merged group is 1 and the last merged '// &
     &         'group is the last frequency group is supported' )
           RETURN
      END IF
!
      NFRG = 1
      NFRQ = PIM%NFRQ*PIM%NFRG
!
      ALLOCATE ( FREQ_ARR(PIM%NCHN,NFRQ,NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%NCHN*NFRQ*NFRG, STR )
           CALL ERR_LOG ( 8152, IUER, 'PIMA_FRG_MERGE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &         'array FREQ_ARR' )
           RETURN
      END IF
!
      IFRQ = 0
      DO 410 J1=1,PIM%NFRG
         DO 420 J2=1,PIM%NFRQ
            IFRQ = IFRQ + 1
            FRQ(IFRQ) = PIM%FRQ(J2,J1)
            FREQ_ARR(1:PIM%NCHN,IFRQ,NFRG) = PIM%FREQ_ARR(1:PIM%NCHN,J2,J1)
            PIM%REF_FRQ(J2,J1) = IFRQ
            PIM%REV_FRQ(IFRQ) = J2
            PIM%REV_FRG(IFRQ) = J1
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 WRITE ( 6, 110 ) J2, J1, IFRQ, PIM%REF_FRQ(J2,J1), &
     &                            PIM%REV_FRQ(IFRQ), PIM%REV_FRG(IFRQ), &
     &                            FREQ_ARR(1,IFRQ,NFRG) 
 110             FORMAT ( 'PIMA_FRG_MERGE: New IND_FRQ: ', I4, ' Old IND_FRG: ', I1, &
     &                    ' IFRQ: ', I4, ' Ref_frq= ', I4, ' Rev_frq: ', I2, &
     &                    ' Rev_frg: ', I2, ' Freq: ', 1PD15.7, ' Hz ' )
            END IF
 420     CONTINUE 
 410  CONTINUE 
      DEALLOCATE ( PIM%FREQ_ARR )
!
      PIM%NFRG = NFRG
      PIM%NFRQ = NFRQ
      ALLOCATE ( PIM%FREQ_ARR(PIM%NCHN,PIM%NFRQ,PIM%NFRG), STAT=IER )
      PIM%FRQ(1:PIM%NFRQ,1) = FRQ(1:PIM%NFRQ)
      PIM%FREQ_ARR(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NFRG) = FREQ_ARR(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NFRG) 
!
      DEALLOCATE ( FREQ_ARR )
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, 120 ) PIM%CONF%FRG_LIST(1), PIM%CONF%FRG_LIST(2)
 120       FORMAT ( 'PIMA_FRG_MERGE merged frequency groups ', I1, &
     &              ' through ', I1 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_FRG_MERGE  !#!#
