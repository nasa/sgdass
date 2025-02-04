      SUBROUTINE SOTID_SET ( GEN_LOVE, MODEL_2D, ORDER_2D, ZERO_FREQ_LOVE, &
     &                       MODEL_3D, TIDCNF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SOTID_SET  sets configuration parameters of SOTID         *
! *   software. Values of each configuration parameter are checked and   *
! *   if the check is OK, SOTID_SET puts it in the field of TIDCNF.      *
! *                                                                      *
! * ________________________ Input parameters:  ________________________ *
! *                                                                      *
! *       GEN_LOVE ( INTEGER*4 ) -- Which generalized Love numbers are   *
! *                                 to be used. Suppported values:       *
! *                              SOTID__PRN_ONLY                         *
! *                              SOTID__PL_ONLY                          *
! *                              SOTID__GEN_ALL                          *
! *       MODEL_2D ( INTEGER*4 ) -- Which model for the Love numbers of  *
! *                                 the second degree is to be applied.  *
! *                              SOTID__MDG97EL                          *
! *                              SOTID__MDG97AN                          *
! *                              SOTID__DDW99EH                          *
! *                              SOTID__DDW99IN                          *
! *                              SOTID__LOVE                             *
! *                              SOTID__MAT00                            *
! *                              SOTID__MAT01                            *
! *       ORDER_2D ( INTEGER*4 ) -- Tidal waves of which order of the    *
! *                                 second degree should be taken into   *
! *                                 account: zonal, diurnal,             *
! *                                 semi-dirunal or their combination.   *
! *                                 NB: SOTID__2D_012ORD should be       *
! *                                 normally used for computation. Other *
! *                                 values are reserved for testing or   *
! *                                 for a special purpose.               *
! *                              SOTID__2D_NONE                          *
! *                              SOTID__2D_0ORD                          *
! *                              SOTID__2D_01ORD                         *
! *                              SOTID__2D_02ORD                         *
! *                              SOTID__2D_012ORD                        *
! *                              SOTID__2D_1ORD                          *
! *                              SOTID__2D_12ORD                         *
! *                              SOTID__2D_2ORD                          *
! * ZERO_FREQ_LOVE ( INTEGER*4 ) -- Which Love numbers for the tides     *
! *                                 of the zeroth frequency should be    *
! *                                 used.                                *
! *                              SOTID__ZF_ZERO                          *
! *                              SOTID__ZF_LOVE                          *
! *                              SOTID__ZF_MDG97EL                       *
! *                              SOTID__ZF_MDG97AN                       *
! *                              SOTID__ZF_FLUID                         *
! *       MODEL_3D ( INTEGER*4 ) -- Which model for the Love numbers of  *
! *                                 the third degree is to be applied.   *
! *                              SOTID__3D_NONE                          *
! *                              SOTID__3D_MDG97                         *
! *                                                                      *
! *   Named constants SOTID__xxx are defined in sotid_data.i             *
! *   Their descriptipon can be found in documentation for SOTID.        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *         TIDCNF ( RECORD    ) -- Object which holds configuration     *
! *                                 parameters of SOTID.                 *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 10-JUL-2002   SOTID_SET   v1.0 (c)  L. Petrov  10-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'sotid_type.i'
      INCLUDE   'sotid_data.i'
      TYPE ( TIDCNF__STRU ) ::  TIDCNF
      INTEGER*4  GEN_LOVE, MODEL_2D, ORDER_2D, ZERO_FREQ_LOVE, MODEL_3D, IUER
      CHARACTER  STR*16
!
! --- Check GEN_LOVE argument
!
      IF ( GEN_LOVE .EQ. SOTID__PRN_ONLY ) THEN
           TIDCNF%GEN_LOVE = SOTID__PRN_ONLY
         ELSE IF ( GEN_LOVE .EQ. SOTID__PL_ONLY ) THEN
           TIDCNF%GEN_LOVE = SOTID__PL_ONLY
         ELSE IF ( GEN_LOVE .EQ. SOTID__GEN_ALL ) THEN
           TIDCNF%GEN_LOVE = SOTID__GEN_ALL
         ELSE
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(I12)' ) GEN_LOVE
           CALL CHASHL ( STR )
           CALL ERR_LOG ( 5711, IUER, 'SOTID_SET', 'Wrong value of '// &
     &         'the argument GEN_LOVE. GEN_LOVE ='//STR )
           RETURN
      END IF
!
! --- The purpose of the code below is actually to detect whether the arguments
! --- have correct values. Then the values are filling the fields of the
! --- TIDCNF record.
!
      IF ( MODEL_2D .EQ. SOTID__LOVE ) THEN
           TIDCNF%MODEL_2D = SOTID__LOVE
         ELSE IF ( MODEL_2D .EQ. SOTID__MDG97EL ) THEN
           TIDCNF%MODEL_2D = SOTID__MDG97EL
         ELSE IF ( MODEL_2D .EQ. SOTID__MDG97AN ) THEN
           TIDCNF%MODEL_2D = SOTID__MDG97AN
         ELSE IF ( MODEL_2D .EQ. SOTID__DDW99EH ) THEN
           TIDCNF%MODEL_2D = SOTID__DDW99EH
         ELSE IF ( MODEL_2D .EQ. SOTID__DDW99IN ) THEN
           TIDCNF%MODEL_2D = SOTID__DDW99IN
         ELSE IF ( MODEL_2D .EQ. SOTID__MAT00   ) THEN
           TIDCNF%MODEL_2D = SOTID__MAT00
         ELSE IF ( MODEL_2D .EQ. SOTID__MAT01   ) THEN
           TIDCNF%MODEL_2D = SOTID__MAT01
         ELSE
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(I12)' ) MODEL_2D
           CALL CHASHL ( STR )
           CALL ERR_LOG ( 5712, IUER, 'SOTID_SET', 'Wrong value of '// &
     &         'the argument MODEL_2D. MODEL_2D='//STR )
           RETURN
      END IF
!
! --- Check ORDER_2D argument
!
      IF ( ORDER_2D .EQ. SOTID__2D_NONE     ) THEN
           TIDCNF%ORDER_2D  = SOTID__2D_NONE
         ELSE IF ( ORDER_2D .EQ. SOTID__2D_0ORD ) THEN
           TIDCNF%ORDER_2D  = SOTID__2D_0ORD
         ELSE IF ( ORDER_2D .EQ. SOTID__2D_01ORD ) THEN
           TIDCNF%ORDER_2D  = SOTID__2D_01ORD
         ELSE IF ( ORDER_2D .EQ. SOTID__2D_02ORD ) THEN
           TIDCNF%ORDER_2D  = SOTID__2D_02ORD
         ELSE IF ( ORDER_2D .EQ. SOTID__2D_012ORD ) THEN
           TIDCNF%ORDER_2D  = SOTID__2D_012ORD
         ELSE IF ( ORDER_2D .EQ. SOTID__2D_1ORD ) THEN
           TIDCNF%ORDER_2D  = SOTID__2D_1ORD
         ELSE IF ( ORDER_2D .EQ. SOTID__2D_12ORD ) THEN
           TIDCNF%ORDER_2D  = SOTID__2D_12ORD
         ELSE IF ( ORDER_2D .EQ. SOTID__2D_2ORD ) THEN
           TIDCNF%ORDER_2D  = SOTID__2D_2ORD
         ELSE
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(I12)' ) ORDER_2D
           CALL CHASHL ( STR )
           CALL ERR_LOG ( 5713, IUER, 'SOTID_SET', 'Wrong value of '// &
     &         'the argument ORDER_2D. SEDOND_DEGREE ='//STR )
           RETURN
      END IF
!
! --- Check ZERO_FREQ_LOVE argument
!
      IF ( ZERO_FREQ_LOVE .EQ. SOTID__ZF_ZERO ) THEN
           TIDCNF%ZF_LOVE = SOTID__ZF_ZERO
         ELSE IF ( ZERO_FREQ_LOVE .EQ. SOTID__ZF_LOVE ) THEN
           TIDCNF%ZF_LOVE = SOTID__ZF_LOVE
         ELSE IF ( ZERO_FREQ_LOVE .EQ. SOTID__ZF_MDG97EL ) THEN
           TIDCNF%ZF_LOVE = SOTID__ZF_MDG97EL
         ELSE IF ( ZERO_FREQ_LOVE .EQ. SOTID__ZF_MDG97AN ) THEN
           TIDCNF%ZF_LOVE = SOTID__ZF_MDG97AN
         ELSE IF ( ZERO_FREQ_LOVE .EQ. SOTID__ZF_FLUID ) THEN
           TIDCNF%ZF_LOVE = SOTID__ZF_FLUID
         ELSE
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(I12)' ) TIDCNF%ZF_LOVE
           CALL CHASHL ( STR )
           CALL ERR_LOG ( 5714, IUER, 'SOTID_SET', 'Wrong value of '// &
     &         'the argument ORDER_2D. SEDOND_DEGREE ='//STR )
           RETURN
      END IF
!
! --- Check MODEL_3D argument
!
      IF ( MODEL_3D .EQ. SOTID__3D_NONE ) THEN
           TIDCNF%MODEL_3D = SOTID__3D_NONE
         ELSE IF ( MODEL_3D .EQ. SOTID__3D_MDG97 ) THEN
           TIDCNF%MODEL_3D = SOTID__3D_MDG97
         ELSE
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(I12)' ) MODEL_3D
           CALL CHASHL ( STR )
           CALL ERR_LOG ( 5715, IUER, 'SOTID_SET', 'Wrong value of '// &
     &         'the argument MODEL_3D. MODEL_3D='//STR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  !#!  SOTID_SET  #!#
