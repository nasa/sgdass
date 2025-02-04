      SUBROUTINE STA_AMBUPD ( ISTA, BAND, KP, INDS, IAMB, FUSE, DBOBJ, SCAINF, &
     &                        OBSBAS, PAMBI, KAMB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  STA_AMBUPD  updates baseline-dependent phase delay        *
! *   ambiguities and suppression status for all observations affected   *
! *   by ISTA-th station in OBSBAS, PAMBI data structures after running  *
! *   SCADAM algorithm.                                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    ISTA ( INTEGER*4 ) -- Index of the station to be considered in    *
! *                          the list of stations DBOBJ.LIS_STA .        *
! *    BAND ( INTEGER*4 ) -- Band under consideration: X or S. Band      *
! *                          codes are defined in pamb.i                 *
! *      KP ( INTEGER*4 ) -- Number of scans.                            *
! *    INDS ( INTEGER*4 ) -- Array of indices of the scan counters for   *
! *                          SCADAM data strucutres. Dimension: KP.      *
! *                          It is a cross reference table between scan  *
! *                          counters in DBOBJ data structures and       *
! *                          SCADAM data structures. l-th scan in SCADAM *
! *                          corresponds to INDS(l) scan in DBOBJ.       *
! *    IAMB ( INTEGER*4 ) -- Array of integer station dependent          *
! *                          ambiguities for ISTA-th station. Dimension: *
! *                          KP. IAMB(k) keeps station dependent phase   *
! *                          delay ambiguity for the INDS(k)-th scan in  *
! *                          DBOBJ data structure.                       *
! *    FUSE ( LOGICAL*4 ) -- Usage array. If FUSE(k) = .TRUE. it means   *
! *                          that station-dependent ambiguity for the    *
! *                          INDS(k)-th scan is successfully resolved.   *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *  SCAINF ( RECORD    ) -- Data structure which keeps values of        *
! *                          parameters which control work of algorithm  *
! *                          SCADAM and result of work of algorithm      *
! *                          SCADAM.                                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  OBSBAS ( RECORD    ) -- Array of data structures which keeps        *
! *                          baseline dependent information about the    *
! *                          session.                                    *
! *   PAMBI ( RECORD    ) -- Array of data structures keeping            *
! *                          information about phase delays, their       *
! *                          errors, ambiguities and etc.                *
! *    KAMB ( INTEGER*4 ) -- Number of ambiguities (at each band) to     *
! *                          have been changed.                          *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  02-NOV-98   STA_AMBUPD   v1.3  (c)  L. Petrov  08-JUN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INTEGER*4  ISTA, BAND, KP, KAMB, IUER
      TYPE     ( DBOBJ_O__STRU ) :: DBOBJ
      TYPE     ( BAS_O__STRU   ) :: OBSBAS(DBOBJ%L_OBS)
      TYPE     ( PAMBI__STRU   ) :: PAMBI(DBOBJ%L_OBS)
      TYPE     ( SCAINF__STRU  ) :: SCAINF
      LOGICAL*4  FUSE(KP), F_FOUND
      INTEGER*4  INDS(KP), IAMB(KP)
      CHARACTER  STR*32
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  J1, J2, IPL_STA1, IPL_STA2, IER
      INTEGER*4, EXTERNAL :: IFIND_PL, I_LEN
!
! --- Scan all observations
!
      DO 410 J1=1,DBOBJ%L_OBS
         IPL_STA1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                         INT4(OBSBAS(J1)%ISITE(1)) )
         IPL_STA2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                         INT4(OBSBAS(J1)%ISITE(2)) )
!
         F_FOUND = .FALSE.
!
! ------ Look all scans in search of the scan which contain the
! ------ J1-th observation
!
         DO 420 J2=1,KP
            IF ( INT4(OBSBAS(J1)%IND_SCA) .EQ. INDS(J2) ) F_FOUND = .TRUE.
!
! --------- Set status: ambiguity was computed and updated
!
            IF ( INT4(OBSBAS(J1)%IND_SCA) .EQ. INDS(J2) ) THEN
                 IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM .OR. &
     &                SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR .OR. &
     &                SCAINF%ARF_TYPE .EQ. ARFTYPE__PXGS     ) THEN
!
                      SCAINF%UPD_OBS(PAMB__XBAND,J1) = .TRUE.
                 END IF
                 IF ( SCAINF%ARF_TYPE .EQ. ARFTYPE__COMM .OR. &
     &                SCAINF%ARF_TYPE .EQ. ARFTYPE__EXPR .OR. &
     &                SCAINF%ARF_TYPE .EQ. ARFTYPE__PSGS     ) THEN
!
                      SCAINF%UPD_OBS(PAMB__SBAND,J1) = .TRUE.
                 END IF
            END IF
!
            IF ( INT4(OBSBAS(J1)%IND_SCA) .EQ. INDS(J2)  .AND. &
     &           ( IAMB(J2) .NE. 0  .OR.  .NOT.  FUSE(J2) )  ) THEN
!
! -------------- J2-th scan corresponds to the J1-th observation and it
! -------------- requires an action
!
                 IF ( IAMB(J2) .NE. 0  .AND.  IPL_STA1 .EQ. ISTA ) THEN
!
! ------------------- The first station of the baseline is the station under
! ------------------- consideration: update ambiguity counter
!
                      CALL ERR_PASS   ( IUER, IER )
                      CALL AMB_UPDATE ( BAND, -IAMB(J2), OBSBAS(J1), PAMBI(J1), &
     &                                  IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 5421, IUER, 'STA_AMBUPD', 'Error '// &
     &                         'updating ambiguity for '//STR(1:I_LEN(STR))// &
     &                         '-th obseravation at '//BAND_STR(BAND))
                           RETURN
                      END IF
                      KAMB = KAMB + 1
                 END IF
!
                 IF ( IAMB(J2) .NE. 0  .AND.  IPL_STA2 .EQ. ISTA ) THEN
!
! ------------------- The second station of the baseline is the station under
! ------------------- consideration: update ambiguity counter
!
                      CALL ERR_PASS   ( IUER, IER )
                      CALL AMB_UPDATE ( BAND, IAMB(J2), OBSBAS(J1), PAMBI(J1), &
     &                                  IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 5422, IUER, 'STA_AMBUPD', 'Error '// &
     &                         'updating ambiguity for '//STR(1:I_LEN(STR))// &
     &                         '-th obseravation at '//BAND_STR(BAND))
                           RETURN
                      END IF
                      KAMB = KAMB + 1
                 END IF
!
! -------------- Set status "phase ambiguity is not resolved" for the J1-th
! -------------- observation if it was marked as not used for phase delay
! -------------- solution
!
                 IF ( .NOT. FUSE(J2)  .AND.  ISTA .EQ. IPL_STA1 ) THEN
                      IF ( BAND .EQ. PAMB__XBAND ) THEN
                           CALL SBIT (OBSBAS(J1)%SUPSTAT(1), XAMB__SPS, INT2(1))
                           OBSBAS(J1)%AUTO_SUP = IBSET ( OBSBAS(J1)%AUTO_SUP, &
     &                                                   INT4(XAMB__SPS) ) 
                         ELSE IF ( BAND .EQ. PAMB__SBAND ) THEN
                           CALL SBIT (OBSBAS(J1)%SUPSTAT(1), SAMB__SPS, INT2(1))
                           OBSBAS(J1)%AUTO_SUP = IBSET ( OBSBAS(J1)%AUTO_SUP, &
     &                                                   INT4(SAMB__SPS) ) 
                      END IF
                 END IF
!
                 IF ( .NOT. FUSE(J2)  .AND.  ISTA .EQ. IPL_STA2 ) THEN
                      IF ( BAND .EQ. PAMB__XBAND ) THEN
                           CALL SBIT (OBSBAS(J1)%SUPSTAT(1), XAMB__SPS, INT2(1))
                           OBSBAS(J1)%AUTO_SUP = IBSET ( OBSBAS(J1)%AUTO_SUP, &
     &                                                   INT4(XAMB__SPS) ) 
                         ELSE IF ( BAND .EQ. PAMB__SBAND ) THEN
                           CALL SBIT (OBSBAS(J1)%SUPSTAT(1), SAMB__SPS, INT2(1))
                           OBSBAS(J1)%AUTO_SUP = IBSET ( OBSBAS(J1)%AUTO_SUP, &
     &                                                   INT4(SAMB__SPS) ) 
                      END IF
                 END IF
            END IF
 420     CONTINUE
!
! ------ If the J1-th observation has been done at the ISTA-th station but
! ------ was not found in INDS list of indices we set status "phase ambiguity
! ------ is not resolved"
!
         IF ( .NOT. F_FOUND ) THEN
              IF ( ISTA .EQ. IPL_STA1  .OR.  ISTA .EQ. IPL_STA2 ) THEN
                   IF ( BAND .EQ. PAMB__XBAND ) THEN
                        CALL SBIT ( OBSBAS(J1)%SUPSTAT(1), XAMB__SPS, INT2(1) )
                        OBSBAS(J1)%AUTO_SUP = IBSET ( OBSBAS(J1)%AUTO_SUP, &
     &                                                INT4(XAMB__SPS) ) 
                      ELSE IF ( BAND .EQ. PAMB__SBAND ) THEN
                        CALL SBIT ( OBSBAS(J1)%SUPSTAT(1), SAMB__SPS, INT2(1) )
                        OBSBAS(J1)%AUTO_SUP = IBSET ( OBSBAS(J1)%AUTO_SUP, &
     &                                                INT4(SAMB__SPS) ) 
                   END IF
              END IF
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  STA_AMBUPD  !#!#
