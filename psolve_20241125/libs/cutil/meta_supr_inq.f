      FUNCTION   META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, CODE )
! ************************************************************************
! *                                                                      *
! *   Function  META_SUPR_INQ inquires of suppression status of the      *
! *   observation. It returns values  .TRUE. or .FALSE.  in accordance   *
! *   with status of the tested observation and inquiry code.            *
! *                                                                      *
! * _________________________ Input parameters _________________________ *
! *                                                                      *
! *   AUTO_SUP ( INTEGER*4 ) -- Automatic suppression status.            *
! *   USER_SUP ( INTEGER*4 ) -- User action for suppression.             *
! *   USER_REC ( INTEGER*4 ) -- User action for recovery.                *
! *       CODE ( INTEGER*2 ) -- Inquiry code.                            *
! *                                                                      *
! * ### 06-JUN-2007  META_SUPR_INQ  v1.5 (c)  L. Petrov 14-FEB-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      LOGICAL*4  META_SUPR_INQ
      INTEGER*4  AUTO_SUP, USER_SUP, USER_REC
      INTEGER*2  CODE, INT2_ARG
      INTEGER*4  IARR(2), IP
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      IF ( .NOT. BTEST ( AUTO_SUP, INT4(INIT__SPS) ) ) THEN
           WRITE ( 6, 110 )  AUTO_SUP
 110       FORMAT ( 'AUTO_SUP = ',B32 ,' (binary)' )
           CALL ERR_LOG ( 9101, -2, 'META_SUPR_INQ', 'Bit INIT__SPS is not '// &
     &         'set. It means the suppression status was not defined' )
!
! -------- Deliberate crash
!
           IP = 3
           IARR(IP) = IP
      END IF
!
      IF ( CODE .EQ. GOOD__SPS ) THEN
           IF ( IDATYP == GX__DTP      .OR.  &
     &          IDATYP == PX__DTP      .OR.  &
     &          IDATYP == PX_GX__DTP   .OR.  &
     &          IDATYP == SNG_X__DTP   .OR.  &
     &          IDATYP == GRPRAT__DTP  .OR.  &
     &          IDATYP == PHSRAT__DTP  .OR.  &
     &          IDATYP == SNBRAT__DTP  .OR.  &
     &          IDATYP == GRPONL__DTP  .OR.  &
     &          IDATYP == PHSONL__DTP  .OR.  &
     &          IDATYP == SNBONL__DTP  .OR.  &
     &          IDATYP == RATONL__DTP        ) THEN
!
                IF ( BTEST ( AUTO_SUP, INT4(NOFX__SPS) ) ) THEN
                     META_SUPR_INQ = .FALSE.
                   ELSE 
                     META_SUPR_INQ = .TRUE.
                END IF
              ELSE IF ( IDATYP == GS__DTP     .OR. &
     &                  IDATYP == PS__DTP     .OR. &
     &                  IDATYP == PS_GS__DTP  .OR. &
     &                  IDATYP == SNG_S__DTP  .OR. &
     &                  IDATYP == G_GXS__DTP  .OR. &
     &                  IDATYP == PX_GXS__DTP .OR. &
     &                  IDATYP == PS_GXS__DTP .OR. &
     &                  IDATYP == PX_GS__DTP  .OR. &
     &                  IDATYP == PS_GX__DTP  .OR. &
     &                  IDATYP == P_PXS__DTP       ) THEN
!
                IF ( BTEST ( AUTO_SUP, INT4(NOFS__SPS) ) ) THEN
                     META_SUPR_INQ = .FALSE.
                   ELSE 
                     META_SUPR_INQ = .TRUE.
                END IF
              ELSE IF ( IDATYP == FUSED__DTP ) THEN
                IF ( BTEST ( AUTO_SUP, INT4(FURE__SPS) ) ) THEN
                     META_SUPR_INQ = .TRUE.
                   ELSE
                     META_SUPR_INQ = .FALSE.
                END IF
              ELSE 
                IF ( BTEST ( AUTO_SUP, INT4(NOFS__SPS) ) .OR. &
     &               BTEST ( AUTO_SUP, INT4(NOFX__SPS) )     ) THEN
                     META_SUPR_INQ = .FALSE.
                   ELSE 
                     META_SUPR_INQ = .TRUE.
                END IF
           END IF
!
           IF ( BTEST ( AUTO_SUP, INT4(DECM__SPS) )  ) THEN
                META_SUPR_INQ = .FALSE.
           END IF
           IF ( BTEST ( AUTO_SUP, INT4(EXTS__SPS) )  ) THEN
                META_SUPR_INQ = .FALSE.
           END IF
           RETURN 
      END IF
!
      META_SUPR_INQ = .FALSE.
!
      IF ( BTEST ( AUTO_SUP, INT4(CUEL__SPS) ) ) RETURN 
      IF ( BTEST ( AUTO_SUP, INT4(DSBS__SPS) ) ) RETURN 
      IF ( BTEST ( AUTO_SUP, INT4(DSSO__SPS) ) ) RETURN 
      IF ( BTEST ( AUTO_SUP, INT4(DECM__SPS) ) ) RETURN 
      IF ( BTEST ( AUTO_SUP, INT4(EXTS__SPS) ) ) RETURN 
!
      IF ( BTEST ( AUTO_SUP, INT4(BQCX__SPS) ) .OR. &
     &     BTEST ( AUTO_SUP, INT4(NOFX__SPS) )      ) THEN
!
           IF ( IDATYP == PS__DTP    .OR. &
     &          IDATYP == GS__DTP    .OR. &
     &          IDATYP == SNG_S__DTP .OR. &
     &          IDATYP == PS_GS__DTP .OR. &
     &          IDATYP == FUSED__DTP       ) THEN
!
                CONTINUE 
              ELSE
                RETURN ! not used
           END IF
      END IF
!
      IF ( BTEST ( AUTO_SUP, INT4(BQCS__SPS) ) .OR. &
     &     BTEST ( AUTO_SUP, INT4(NOFS__SPS) )      ) THEN
           IF ( IDATYP == PS__DTP     .OR. &
     &          IDATYP == GS__DTP     .OR. &
     &          IDATYP == SNG_S__DTP  .OR. &
     &          IDATYP == PS_GS__DTP  .OR. &
     &          IDATYP == G_GXS__DTP  .OR. &
     &          IDATYP == PX_GXS__DTP .OR. &
     &          IDATYP == PS_GXS__DTP .OR. &
     &          IDATYP == PX_GS__DTP  .OR. &
     &          IDATYP == PS_GX__DTP  .OR. &
     &          IDATYP == P_PXS__DTP       ) THEN
!
                RETURN ! not used
              ELSE
                CONTINUE 
           END IF
      END IF
      IF ( ( BTEST ( AUTO_SUP, INT4(BQCS__SPS) ) .OR. &
     &       BTEST ( AUTO_SUP, INT4(NOFS__SPS) )      ) .AND. &
     &     ( BTEST ( AUTO_SUP, INT4(BQCX__SPS) ) .OR. &
     &       BTEST ( AUTO_SUP, INT4(NOFX__SPS) )      )       ) THEN
           IF ( IDATYP == FUSED__DTP ) THEN
                RETURN ! not used
           END IF
      END IF
!
      IF ( IDATYP == GRPRAT__DTP  .OR. &
     &     IDATYP == PHSRAT__DTP  .OR. &
     &     IDATYP == PHSRAT__DTP  .OR. &
     &     IDATYP == SNBRAT__DTP  .OR. &
     &     IDATYP == GRPONL__DTP  .OR. &
     &     IDATYP == PHSONL__DTP  .OR. &
     &     IDATYP == SNBONL__DTP  .OR. &
     &     IDATYP == RATONL__DTP       ) THEN
!
           IF ( BTEST ( AUTO_SUP, INT4(IOUS__SPS) ) ) THEN
                IF ( BTEST ( AUTO_SUP, INT4(GION__SPS) ) ) RETURN 
                IF ( BTEST ( AUTO_SUP, INT4(GIO1__SPS) ) ) RETURN 
                IF ( BTEST ( AUTO_SUP, INT4(GIO2__SPS) ) ) RETURN 
                IF ( BTEST ( AUTO_SUP, INT4(GIO3__SPS) ) ) RETURN 
                IF ( BTEST ( AUTO_SUP, INT4(GIO4__SPS) ) ) RETURN 
           END IF
      END IF
      IF ( BTEST ( AUTO_SUP, INT4(LSNR__SPS) ) ) THEN
           META_SUPR_INQ = .FALSE.
           RETURN 
      END IF
!
      IF ( CODE .EQ. USED__SPS ) THEN
           IF ( BTEST ( USER_SUP, INT4(IDATYP) ) ) RETURN  ! the observation not used
           META_SUPR_INQ = .TRUE.
           RETURN
         ELSE IF ( CODE .EQ. RECO__SPS ) THEN
           META_SUPR_INQ = .TRUE.
           RETURN 
         ELSE
           WRITE ( 6, * ) ' CODE = ',CODE
           CALL ERR_LOG ( 9102, -2, 'META_SUPR_INQ', 'Wrong code' )
!
! -------- Deliberate crash
!
           IP = 3
           IARR(IP) = IP
      END IF
!
      RETURN
      END  FUNCTION   META_SUPR_INQ  !#!#
