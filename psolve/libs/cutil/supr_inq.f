      FUNCTION   SUPR_INQ ( SUPSTAT, UACSUP, CODE )
! ************************************************************************
! *                                                                      *
! *   Function  SUPR_INQ makes inquire of suppression status of the      *
! *   observation. It returns values  .TRUE. or .FALSE.  in according    *
! *   with status of the tested observation and inquiry code.            *
! *                                                                      *
! * _________________________ Input parameters _________________________ *
! *                                                                      *
! *   SUPSTAT ( INTEGER*2 ) -- suppression status. Array of 2 words.     *
! *      CODE ( INTEGER*2 ) -- inquiry code.                             *
! *                                                                      *
! *  ###  15-APR-98   SUPR_INQ    v2.0   (c)  L. Petrov 26-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      LOGICAL*4  SUPR_INQ
      INTEGER*2  SUPSTAT(2), UACSUP, CODE, IDATYP_OLD, SUPSTAT_OLD(2), IP
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: DATYP_INQ
!
      SUPR_INQ = .TRUE.
!
      IF ( .NOT. KBIT ( SUPSTAT, SET1__SPS ) ) THEN
           WRITE ( 6, 110 ) ( KBIT ( SUPSTAT, IP ), IP = 1,32 )
 110       FORMAT ( 'SUPSTAT (bits 1-32) = ', 4(8L1,1X) )
           CALL FERR ( INT2(432), &
     &         'SUPR_INQ: SET1__SPS in SUPSTAT bit is not '// &
     &         'been set on: it means that suppression status was '// &
     &         'not defined', INT2(0), INT2(0) )
      END IF
      IF ( CODE .GE. 1  .AND.  CODE .LE. MAXC__SPS ) THEN
!
! -------- Inquire of the value of status bit
!
           SUPR_INQ = KBIT ( SUPSTAT, CODE )
         ELSE IF ( CODE .EQ. USED__SPS ) THEN
           IF ( .NOT. KBIT ( SUPSTAT, SET2__SPS ) ) THEN
                WRITE ( 6, 110 )  SUPSTAT(1), SUPSTAT(2)
                CALL FERR ( INT2(434), &
     &              'SUPR_INQ: SET2__SPS in SUPSTAT bit is not '// &
     &              'been set on: it means that usage status was '//'not defined', &
     &               INT2(0), INT2(0) )
           END IF
!
! -------- Inquire: is the observation used in estimation?
! -------- It is used if
! -------- 1) it was not marked as bad and was not suppressed by user OR
! -------- 2) it was bad (but recoverable), but user overcame veto
!
           IF ( KBIT ( SUPSTAT, GOOD__SPS ) ) THEN
!
! ------------- Observation has global status good. If user action is not
! ------------- "suppress, despite the observation looks good" then
! ------------- the observation will be considered as actually good
!
                IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------------ Phase delay case
!
                     IF ( KBIT ( UACSUP,  PSUP__UAS ) ) THEN
                          SUPR_INQ = .FALSE.
                       ELSE
                          SUPR_INQ = .TRUE.
                     END IF
                   ELSE
!
! ------------------ Group delay case
!
                     IF ( KBIT ( UACSUP, GSUP__UAS ) ) THEN
                          SUPR_INQ = .FALSE.
                       ELSE
                          SUPR_INQ = .TRUE.
                     END IF
                END IF
!
! ------------- Finally check decimation
!
                IF ( KBIT ( SUPSTAT, DECM__SPS ) ) THEN
                     SUPR_INQ = .FALSE.
                END IF                
             ELSE IF ( KBIT ( SUPSTAT, CBAD__SPS ) ) THEN
!
! ------------- Observation has global status "conditionally bad". If user
! ------------- action is not "restore, despite the observation looks bad" then
! ------------- the observation will be considered as actually bad
!
                IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------------ Phase delay case
!
                     IF ( KBIT ( UACSUP,  POVV__UAS ) ) THEN
                          SUPR_INQ = .TRUE.
                       ELSE
                          SUPR_INQ = .FALSE.
                     END IF
                  ELSE
!
! ------------------ Group delay case
!
                     IF ( KBIT ( UACSUP,  GOVV__UAS ) ) THEN
                          SUPR_INQ = .TRUE.
                       ELSE
                          SUPR_INQ = .FALSE.
                     END IF
                END IF
!
! ------------- Finally check decimation
!
                IF ( KBIT ( SUPSTAT, DECM__SPS ) ) THEN
                     SUPR_INQ = .FALSE.
                END IF                
             ELSE
!
! ------------- If observation was not "conditionally good" and not
! ------------- "conditionally bad" it means that it is unrecoverable.
! ------------- We will never try to use it
!
                SUPR_INQ = .FALSE.
           END IF
         ELSE IF ( CODE .EQ. RECO__SPS ) THEN
           IF ( .NOT. KBIT ( SUPSTAT, SET2__SPS ) ) THEN
                WRITE ( 6, 110 )  SUPSTAT
                CALL FERR ( INT2(436), &
     &              'SUPR_INQ: SET2__SPS in SUPSTAT bit is not '// &
     &              'been set on: it means that usage status was '//'not defined', &
     &               INT2(0), INT2(0) )
           END IF
!
! -------- Inquire: is observation recoverable?
!
           SUPR_INQ = KBIT ( SUPSTAT, GOOD__SPS )
!
! -------- However, if the observatin was a subject of decimation, 
! -------- it is considered unrecoverable
!
           IF ( KBIT ( SUPSTAT, DECM__SPS ) ) THEN
                SUPR_INQ = .FALSE.
           END IF                
         ELSE IF ( CODE .EQ. URPH__SPS ) THEN
!
! -------- Inquire: is the observation unrecoverable for phase delay solution
! -------- type?
!
! -------- First, keep solution type and suppression status
!
           IDATYP_OLD = IDATYP
           SUPSTAT_OLD(1) = SUPSTAT(1)
           SUPSTAT_OLD(2) = SUPSTAT(2)
!
! -------- Set temporarily phase delay combination solution type
!
           IDATYP         = P_PXS__DTP
!
! -------- Set usage status code for phase delay solution type
!
           CALL SUPUSE_SET ( SUPSTAT )
!
! -------- Check unrecoverablity bit
!
           SUPR_INQ = KBIT ( SUPSTAT, UNRC__SPS )
!
! -------- Finally, restore solution type and suppression status
!
           IDATYP     = IDATYP_OLD
           SUPSTAT(1) = SUPSTAT_OLD(1)
           SUPSTAT(2) = SUPSTAT_OLD(2)
         ELSE
           WRITE ( 6, * ) ' CODE = ',CODE
           CALL FERR ( INT2(438), 'SUPR_IUQ: Wrong code for SUPR_INQ', &
     &          INT2(0), INT2(0) )
      END IF
!
      RETURN
      END  !#!  SUPR_INQ  #!#
