      SUBROUTINE PAS_CHECK ( DBOBJ, OBSBAS, KOBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PAS_CHECK scans parameter "phase delay ambiguity spacing" *
! *   for both X and S bands and check their consistency. Database is    *
! *   considered as a consistent if all ambiguity spacing for all        *
! *   observations at one band which are considered as potentially       *
! *   recovered are the same. If there are observations with another     *
! *   value of phase delay ambiguity spacing (It is a bug in Fourfit?)   *
! *   then  PAS_CHECK  divide all observations on groups with the same   *
! *   value of ambiguity spacing for each band and leave observations    *
! *   fich belong to the group with majority observations while it set   *
! *   flag WPAS__SPS "Wrong phase delay ambiguity spacings" what makes   *
! *   them unrecovereable for all types of phase delay solutions.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about      *
! *                            the session.                              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      KOBS ( INTEGER*4 ) -- Number of observations (for both bands)   *
! *                            which were flagged as unrecoverable for   *
! *                            phase solutions in result of work of      *
! *                            PAS_CHECK.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  21-OCT-98    PAS_CHECK   v1.1  (c)  L. Petrov  08-JUN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'obser.i'
      INTEGER*4  KOBS, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      INTEGER*4  M_BIN
      REAL*8     EPS_SP
      PARAMETER  ( EPS_SP = 1.D-14 )
      PARAMETER  (  M_BIN = 32     )
      REAL*8     SPA_BIN(M_BIN), SPA
      INTEGER*4  L_BIN, K_BIN(M_BIN), I_BIN, MAX_BIN, J1, J2, J3, J4, J5
      CHARACTER  STR*20, STR1*20
      LOGICAL*4  FL_RECO
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      KOBS = 0
!
! --- Cycle on both bands. We considered first X-band ambiguity spacings, then
! --- S-band ambiguity spacings.
!
      DO 410 J1=1,2
         L_BIN = 0
!
! ------ The first run. Look at all observations and gather then into bins.
!
         DO 420 J2=1,DBOBJ%L_OBS
!
! --------- We bypass observations which are considered as unrecoverable
!
            IF ( SUPMET == SUPMET__META ) THEN
                 FL_RECO = META_SUPR_INQ ( OBSBAS(J2)%AUTO_SUP, &
     &                                     OBSBAS(J2)%USER_SUP, &
     &                                     OBSBAS(J2)%USER_REC, &
     &                                     RECO__SPS )
               ELSE 
                 FL_RECO = SUPR_INQ ( OBSBAS(J2)%SUPSTAT(1), &
     &                                OBSBAS(J2)%UACSUP, RECO__SPS )
            END IF
            IF ( .NOT. FL_RECO ) GOTO 420 
!
! --------- Get ambiguity spacing value
!
            IF ( J1 .EQ. 1 ) THEN
                 SPA = 1.D0/OBSBAS(J2)%FREQ_OBSV_PH
               ELSE IF ( J1 .EQ. 2 ) THEN
                 SPA = 1.D0/OBSBAS(J2)%FREQ_OBSV_PH_OPP
            END IF
!
! --------- Take decision: to which bin the observation belongs?
!
            IF ( L_BIN .LE. 0 ) THEN
!
! -------------- Oh! It is the first observation
!
                 L_BIN = 1
                 SPA_BIN(L_BIN) = SPA
                 K_BIN(L_BIN)   = 1
                 GOTO 420
               ELSE
!
! -------------- Scan all bins
!
                 DO 430 J3=1,L_BIN
                    IF ( DABS( SPA_BIN(J3) - SPA ) .LT. EPS_SP ) THEN
!
! ---------------------- We established that it belongs to the j3-th bin
!
                         K_BIN(J3) = K_BIN(J3) + 1
                         GOTO 420
                    END IF
 430             CONTINUE
!
! -------------- Well. The j2-th observation is away form all other bins.
! -------------- We create a new bin...
!
                 L_BIN = L_BIN + 1
                 IF ( L_BIN .GT. M_BIN ) THEN
                      CALL CLRCH ( STR )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( M_BIN, STR )
                      IF ( J1 .EQ. 1 ) THEN
                           STR1 = 'X-band'
                         ELSE
                           STR1 = 'S-band'
                      END IF
                      CALL ERR_LOG ( 5401, IUER, 'PAS_CHECK', 'Too strong '// &
     &                    'fragmentation of phase delay ambiguity spacings '// &
     &                    ' at '//STR1(1:I_LEN(STR1))//' detected: more '// &
     &                    'than '//STR(1:I_LEN(STR))//' segments. It is '// &
     &                    'an indication of serious problems in the '// &
     &                    'database what prevents further processing '// &
     &                    'phase delays' )
                 END IF
!
! -------------- ... and put it
!
                 K_BIN(L_BIN) = 1
                 SPA_BIN(L_BIN) = SPA
            END IF
 420     CONTINUE
!
         IF ( L_BIN .GT. 1 ) THEN
!
! ----------- It turned out that at least one pair of observations with
! ----------- different spacings have been disclosed. Now have to decide
! ----------- which bin has the most observations
!
              MAX_BIN = 0
              I_BIN   = 0
              DO 440 J4=1,L_BIN
                 IF ( K_BIN(J4) .GT. MAX_BIN ) THEN
                      I_BIN = J4
                      MAX_BIN = K_BIN(I_BIN)
                 END IF
 440          CONTINUE
!
! ----------- Well. We have learned that the I_BIN-th bin has the most
! ----------- of observations. No we set flag  WPAS__SPS  for observation not
! ----------- from the I_BIN-th bin and clear this bit for obsservations
! ----------- which belong to it.
!
              DO 450 J5=1,DBOBJ%L_OBS
!
! -------------- We bypass observations which are considered as unrecoverable
!
                 IF ( SUPMET == SUPMET__META ) THEN
                      FL_RECO = META_SUPR_INQ ( OBSBAS(J5)%AUTO_SUP, &
     &                                          OBSBAS(J5)%USER_SUP, &
     &                                          OBSBAS(J5)%USER_REC, &
     &                                          RECO__SPS )
                   ELSE 
                      FL_RECO = SUPR_INQ ( OBSBAS(J5)%SUPSTAT(1), &
     &                                     OBSBAS(J5)%UACSUP, RECO__SPS )
                 END IF
!                 
                 IF ( .NOT. FL_RECO ) GOTO 450 
!
                 IF ( J1 .EQ. 1 ) THEN
                      SPA = 1.D0/OBSBAS(J5)%FREQ_OBSV_PH
                    ELSE
                      SPA = 1.D0/OBSBAS(J5)%FREQ_OBSV_PH_OPP
                 END IF
!
                 IF ( DABS(SPA - SPA_BIN(I_BIN)) .LE. EPS_SP ) THEN
!
! ------------------- We clear bit of "Wrong phase ambiguity spacings".
! ------------------- But we can do it ONLY if we analyse the first band
!
                      IF ( J1 .EQ. 1 ) THEN
                           CALL SBIT ( OBSBAS(J5)%SUPSTAT(1), WPAS__SPS, &
     &                                 INT2(0) )
                           OBSBAS(J5)%AUTO_SUP = IBCLR ( OBSBAS(J5)%AUTO_SUP, &
     &                                                   INT4(WPAS__SPS) ) 
                      END IF
                    ELSE
                      CALL SBIT ( OBSBAS(J5)%SUPSTAT(1), WPAS__SPS, &
     &                            INT2(1) )
                      OBSBAS(J5)%AUTO_SUP = IBSET ( OBSBAS(J5)%AUTO_SUP, &
     &                                              INT4(WPAS__SPS) ) 
                      KOBS = KOBS + 1
                 END IF
 450          CONTINUE
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PAS_CHECK  #!#
