      SUBROUTINE PAMB_FREEZE ( IDBF, N_OBS, IACT_FREEZE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_FREEZE  freezes or unfreezes suppression status for  *
! *   phase delay observables. Freezing means setting bit  XAMB__SPS or  *
! *   SAMB__SPS  in SUPSTAT for observation with are manually suppressed *
! *   for the phase delay solutions. After setting that bit those        *
! *   observations becomes unrecoverable what prohibits their recovering *
! *   until the bits remain set up. Unfreezeng lifts bit  XAMB_SPS  or   *
! *   SAMB__SPS what enables us to restore them.                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        IDBF ( INTEGER*4 ) -- Index of the database in the scratch    *
! *                              file.                                   *
! *       N_OBS ( INTEGER*4 ) -- Number of observations in the database. *
! * IACT_FREEZE ( INTEGER*4 ) -- Code of operation. Supported codes:     *
! *                         21 -- freeze X-band delays;                  *
! *                         22 -- freeze S-band delays;                  *
! *                         23 -- freeze both X-band and S-band delays;  *
! *                         31 -- unfreeze X-band delays;                *
! *                         32 -- unfreeze S-band delays;                *
! *                         33 -- unfreeze both X-band and S-band delays;*
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        IUER ( INTEGER*4, OPT ) -- Universal error handler.           *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  19-DEC-98   PAMB_FREEZE  v1.1  (c)  L. Petrov 08-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INTEGER*4  IDBF, N_OBS, IACT_FREEZE, IUER
      INTEGER*4  J1
      INTEGER*2  IDATYP_OLD
      CHARACTER  STR*20
      LOGICAL*4  USE_FLAG
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
!
! --- Setting temporarily phase delay type solution
!
      IDATYP_OLD = IDATYP
      IDATYP     = P_PXS__DTP
!
! --- Openning obsfil
!
      CALL ACS_OBSFIL ( 'O' )
!
! --- Cycle over all observation and reading scratch file
!
      DO 410 J1=IDBF,IDBF+N_OBS-1
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
! ------ Learn usage flag for phase delay solution
!
         IF ( SUPMET == SUPMET__META ) THEN
              USE_FLAG = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                   USED__SPS )
            ELSE 
              USE_FLAG = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
         END IF
!
         IF ( IACT_FREEZE .EQ. 21 ) THEN
!
! ----------- Freeze X-band phase delays
!
              IF ( USE_FLAG ) THEN
                   CALL SBIT ( SUPSTAT, XAMB__SPS, INT2(1) )
                   AUTO_SUP = IBSET ( AUTO_SUP, INT4(XAMB__SPS) )
              END IF
            ELSE IF ( IACT_FREEZE .EQ. 22 ) THEN
!
! ----------- Freeze S-band phase delays
!
              IF ( USE_FLAG ) THEN
                   CALL SBIT ( SUPSTAT, SAMB__SPS, INT2(1) )
                   AUTO_SUP = IBSET ( AUTO_SUP, INT4(SAMB__SPS) )
              END IF
            ELSE IF ( IACT_FREEZE .EQ. 23 ) THEN
!
! ----------- Freeze both X- and S-band phase delays
!
              IF ( USE_FLAG ) THEN
                   CALL SBIT ( SUPSTAT, XAMB__SPS, INT2(1) )
                   CALL SBIT ( SUPSTAT, SAMB__SPS, INT2(1) )
                   AUTO_SUP = IBSET ( AUTO_SUP, INT4(XAMB__SPS) )
                   AUTO_SUP = IBSET ( AUTO_SUP, INT4(SAMB__SPS) )
              END IF
            ELSE IF ( IACT_FREEZE .EQ. 31 ) THEN
!
! ----------- Unfreeze X-band phase delays
!
              CALL SBIT ( SUPSTAT, XAMB__SPS, INT2(0) )
              AUTO_SUP = IBCLR ( AUTO_SUP, INT4(XAMB__SPS) )
            ELSE IF ( IACT_FREEZE .EQ. 32 ) THEN
!
! ----------- Unfreeze S-band phase delays
!
              CALL SBIT ( SUPSTAT, SAMB__SPS, INT2(0) )
              AUTO_SUP = IBCLR ( AUTO_SUP, INT4(SAMB__SPS) )
            ELSE IF ( IACT_FREEZE .EQ. 33 ) THEN
!
! ----------- Unfreeze both X-band and S-band phase delays
!
              CALL SBIT ( SUPSTAT, XAMB__SPS, INT2(0) )
              CALL SBIT ( SUPSTAT, SAMB__SPS, INT2(0) )
              AUTO_SUP = IBCLR ( AUTO_SUP, INT4(XAMB__SPS) )
              AUTO_SUP = IBCLR ( AUTO_SUP, INT4(SAMB__SPS) )
            ELSE
              CALL CLRCH ( STR )
              CALL INCH ( IACT_FREEZE, STR )
              CALL ERR_LOG ( 6971, IUER, 'PAMB_FREEZE', 'Unsupported value '// &
     &                      'of IACT_FREEZE actiuon: '//STR )
              IDATYP = IDATYP_OLD
              RETURN
         END IF
!
! ------ Writing the record of scratch file back
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410  CONTINUE
!
! --- Closing scratch file
!
      CALL ACS_OBSFIL ( 'C' )
!
! --- Restoring solution type
!
      IDATYP = IDATYP_OLD
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PAMB_FREEZE  #!#
