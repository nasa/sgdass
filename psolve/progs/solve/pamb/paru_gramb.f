      SUBROUTINE PARU_GRAMB ( DBOBJ, OBSSCA, OBSBAS, RES, OPCODE, MARGIN, &
     &                        IVRB, N_AMB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARU_GRAMB  
! *                                                                      *
! *  ### 24-JUN-2007  PARU_GRAMB   v1.1 (c)  L. Petrov  12-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INTEGER*4  OPCODE, IVRB, N_AMB, IUER
      REAL*8     MARGIN
      TYPE ( DBOBJ_O__STRU ) :: DBOBJ
      TYPE ( BAS_O__STRU   ) :: OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU   ) :: OBSSCA(*)
      TYPE ( RES_O__STRU   ) :: RES(DBOBJ%L_OBS)
      LOGICAL*4  FL_USED, FL_RECO
      CHARACTER  STR*128
      REAL*8     AMB_SP, AMB_SP_MIN, AMB_SP_MAX
      PARAMETER  ( AMB_SP_MIN = 1.D-11 )
      PARAMETER  ( AMB_SP_MAX = 1.D-5 )
      INTEGER*4  J1, J2, J3, J4, J5, NAMC, IER
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      N_AMB = 0
!
      DO 410 J1=1,DBOBJ%L_OBS
!
! ------ Check whether the observations is used and whether it is recoverable
!
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                                  OBSBAS(J1)%USER_SUP, &
     &                                  OBSBAS(J1)%USER_REC, &
     &                                  USED__SPS )
              FL_RECO = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                                  OBSBAS(J1)%USER_SUP, &
     &                                  OBSBAS(J1)%USER_REC, &
     &                                  RECO__SPS )
            ELSE 
              FL_USED = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), &
     &                             OBSBAS(J1)%UACSUP, USED__SPS )
              FL_RECO = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), &
     &                             OBSBAS(J1)%UACSUP, RECO__SPS )
         END IF
!         
         IF ( ( OPCODE == 1  .AND.       FL_USED        ) .OR. &
     &        ( OPCODE == 2  .AND. .NOT. FL_USED  .AND.        &
     &                                   FL_RECO        ) .OR. &
     &        ( OPCODE == 3                       )     ) THEN
!
! ----------- Consider only observation 
! ----------- which is NOT USED     and 
! ----------- which is RECOVERABLE

!
              AMB_SP = RES(J1)%AMB_SP 
              IF ( IDATYP == G_GXS__DTP  .OR. &
     &             IDATYP == GRPONL__DTP      ) THEN
                   AMB_SP = RES(J1)%AMBION_SP
                 ELSE 
                   AMB_SP = RES(J1)%AMB_SP
              END IF
              IF ( AMB_SP < AMB_SP_MIN  .OR. &
     &             AMB_SP > AMB_SP_MAX       ) THEN
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) AMB_SP
                   CALL INCH ( J1, STR(21:28) )
                   IER = -1
                   CALL ERR_LOG ( 6961, IER, 'PARU_GRAMB', 'The group '// &
     &                 'delay ambiguity spacing '//STR(1:I_LEN(STR))// &
     &                 ' is out of range. Solution type '// &
     &                 DATYP__ABR(IDATYP*6+1:(IDATYP+1)*6)// &
     &                 ' .Observation # '//STR(21:28) )
                   GOTO 410
              END IF
!
              NAMC = IDNINT ( RES(J1)%PSF_DEL/AMB_SP ) 
              IF ( NAMC .NE. 0   .AND.  &
     &             NAMC > -32767 .AND.  &
     &             NAMC <  32767        ) THEN
!
                   IF ( RES(J1)%PSF_DEL - NAMC*AMB_SP < MARGIN*AMB_SP ) THEN
                        RES(J1)%PSF_DEL = RES(J1)%PSF_DEL - NAMC*AMB_SP 
                        RES(J1)%OC_DEL  = RES(J1)%OC_DEL  - NAMC*AMB_SP 
                        IF ( IDATYP == GS__DTP          ) THEN
                             RES(J1)%NUMAMB_GR_S_NEW  = RES(J1)%NUMAMB_GR_S_NEW - NAMC 
                             OBSBAS(J1)%TAUGR_OBS_OPP = OBSBAS(J1)%TAUGR_OBS_OPP - &
     &                                                  NAMC*AMB_SP
                           ELSE 
                             RES(J1)%NUMAMB_GR_NEW = RES(J1)%NUMAMB_GR_NEW - NAMC 
                             OBSBAS(J1)%TAUGR_OBS  = OBSBAS(J1)%TAUGR_OBS - &
     &                                               NAMC*AMB_SP
                        END IF
                        N_AMB = N_AMB + 1
                        IF ( IVRB .GE. 3 ) THEN
                             WRITE ( UNIT=STR, FMT=110 ) N_AMB, J1, &
     &                         DBOBJ%C_STA(OBSBAS(J1)%ISITE(1)), &
     &                         DBOBJ%C_STA(OBSBAS(J1)%ISITE(2)), &
     &                         DBOBJ%C_SOU(OBSSCA(OBSBAS(J1)%IND_SCA)%ISTAR), &
     &                         NAMC
 110                         FORMAT ( I6,' )  Obs: # ',I6, 2X, A,'/',A, &
     &                                   ' Sou: ',A,' Amb: ',I6 )
                             WRITE (  6, '(A)' ) STR(1:I_LEN(STR))
                             WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                        END IF
                   END IF
              END IF
         END IF
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE  PARU_GRAMB  !#!#
