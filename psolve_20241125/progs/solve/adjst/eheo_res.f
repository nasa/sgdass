      SUBROUTINE EHEO_RES ( L_EHEO, EHEO, MJD_EHEO_REF, TAI_EHEO_REF, &
     &                      L_PAR, C_PAR, EST_VEC, COV_MAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EHEO_RES  creates the ouptu HEO file from the contents     *
! *   of Solve solutions and writes it in the spool file.                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       L_EHEO ( INTEGER*4  ) -- The number of frequencies for the     *
! *                                estimates of the Harmonic variations  *
! *                                in Earth's Orientation.               *
! *         EHEO ( EHEO__TYPE ) -- Array of derived objects defined in   *
! *                                $PSOLVE_ROOT/include/solve.i which    *
! *                                keeps  information about estimation   *
! *                                of the Harmonic variations in Earth's *
! *                                Orientation.                          *
! * MJD_EHEO_REF ( INTEGER*4 ) -- MJD part of the reference epoch for    *
! *                               estimation of harmonic Earth           *
! *                               orientation parameters.                *
! * TAI_EHEO_REF ( REAL*8    ) -- TAI part of the reference epoch for    *
! *                               estimation of harmonic Earth           *
! *                               orientation parameters.                *
! *        L_PAR ( INTEGER*4 ) -- The total number of global parameter.  *
! *        C_PAR ( INTEGER*4 ) -- The list of global parameters.         *
! * EST_VEC ( REAL*8    ) -- Vector of estimates. Dimension: L_PAR.      *
! * COV_MAT ( REAL*8    ) -- Unscaled covariance matrix in packed upper  *
! *                          triangular representation. Dimension: L_PAR.*
! *                                                                      *
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
! *  ###  06-JUN-2006    EHEO_RES   v2.5 (c) L. Petrov  29-DEC-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'heo.i'
      TYPE      ( EHEO__TYPE ) EHEO(L_EHEO)
      INTEGER*4  L_EHEO, L_PAR, MJD_EHEO_REF, IUER
      CHARACTER  C_PAR(L_PAR)*(*)
      REAL*8     TAI_EHEO_REF, EST_VEC(*), COV_MAT(*)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 512 )
      CHARACTER  HEO_FORMAT_FILE*128, BUF(MBUF)*128, OUT*128
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, &
     &           SYSNAME*128, NODENAME*128, HARDWARE*128, UNIT*6, STR*128
      LOGICAL*4  LEX, FL_EST, FL_ERR, FL_EST_AMP_FIRST, FL_EST_VEL_FIRST, &
     &           FL_ERR_AMP_FIRST, FL_ERR_VEL_FIRST
      INTEGER*8  IND8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, NBUF, IER
      REAL*8     HEO_AMP_EST(2,2), HEO_AMP_ERR(2,2), &
     &           HEO_VEL_EST(2,2), HEO_VEL_ERR(2,2)
      CHARACTER, EXTERNAL :: GET_CDATE*19,  MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
! --- First put adjustments of parameters and their formal uncertainties
! --- into the listing in spool format
!
      DO 410 J1=1,L_PAR
         IF ( C_PAR(J1)(1:4) == 'HEO ' ) THEN
              IF ( C_PAR(J1)(6:6) == ' ' ) THEN
                   UNIT = 'rad   '
                 ELSE 
                   UNIT = 'rad/s '
              END IF
!
              IND8 = (INT8(J1)*INT8(J1+1))/INT8(2) ! NB: 46341*46342 > 2^31
              WRITE ( UNIT=23, FMT=1720 ) J1, C_PAR(J1), &
     &                          EST_VEC(J1), UNIT, &
     &                          DSQRT ( COV_MAT(IND8) ), UNIT, &
     &                          DSQRT ( COV_MAT(IND8) ), UNIT  
 1720         FORMAT ( I6,'.', 2X, A, 2X, 1PD22.15, 1X, A, &
     &                                2X, 1PD22.15, 1X, A, &
     &                                2X, 1PD22.15, 1X, A  )
         END IF
 410  CONTINUE 
!
      WRITE ( 23, '(A)' ) 'HEO  Output Begin: ==>'
      WRITE ( 23, '(A)' ) HEO__LABEL
!
! --- Check whether the file with documentation exists
!
      CALL GETENVAR ( 'PSOLVE_HELP_DIR', STR )
      IF ( ILEN(STR) == 0 ) THEN
           HEO_FORMAT_FILE = SOLVE_HELP_DIR//'/heo_format.txt'
         ELSE 
           HEO_FORMAT_FILE = STR(1:I_LEN(STR))//'/heo_format.txt'
      END IF 
      INQUIRE ( FILE=HEO_FORMAT_FILE, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3781, IUER, 'EHEO_RES', 'Cannot find the file '//&
     &         'with documentation '//HEO_FORMAT_FILE )
           RETURN 
      END IF
!
! --- Read the file with documentation into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( HEO_FORMAT_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3782, IUER, 'EHEO_RES', 'Failure in an attempt '// &
     &         'to read the file with documentation '//HEO_FORMAT_FILE )
           RETURN 
      END IF
!
! --- Get system information: user name, user real name, email address,
! --- system name, node name and hardware name
!
      CALL GETINFO_USER   ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
! --- External comments
!
      WRITE ( 23, '(A)' ) '#'
!@      WRITE ( 23, '(A)' ) '# Created by Calc/Solve'
      WRITE ( 23, '(A)' ) '# Created by VTD/pSolve'
      WRITE ( 23, '(A)' ) '#       run by '//USER_REALNAME(1:I_LEN(USER_REALNAME))// &
     &                    ' ( '//USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
      WRITE ( 23, '(A)' ) '#           on '//NODENAME(1:I_LEN(NODENAME))// &
     &                    ' at '//GET_CDATE()//' local time'
      WRITE ( 23, '(A)' ) '#'
      WRITE ( 23, '(A)' ) '# Result of VLBI solution '// &
     &                     SOLUID_CHR(1:I_LEN(SOLUID_CHR))
      WRITE ( 23, '(A)' ) '#'
      WRITE ( 23, '(A)' ) '#============================ Beginning of format description: ================='
      WRITE ( 23, '(A)' ) '#'
!
! --- Contents of the format specifications
!
      DO 420 J2=1,NBUF
         WRITE ( 23, '(A)' ) '# '//BUF(J2)(1:I_LEN(BUF(J2)))
 420  CONTINUE 
      WRITE ( 23, '(A)' ) '#'
      WRITE ( 23, '(A)' ) '#============================ End of format description: ======================='
      WRITE ( 23, '(A)' ) '#'
      WRITE ( 23, '(A)' ) '#  Name of the model'
      WRITE ( 23, '(A)' ) '#'
!
! --- Solution ID
!
      WRITE ( 23, '(A)' ) 'N  VLBI solution '// &
     &                     SOLUID_CHR(1:I_LEN(SOLUID_CHR))// &
     &                    ' produced at '//GET_CDATE()
      WRITE ( 23, '(A)' ) '#'
      WRITE ( 23, '(A)' ) '# Epoch for the model (TAI)'
      WRITE ( 23, '(A)' ) '#'
!
! --- Reference epoch of HEO model
!
      CALL ERR_PASS ( IUER, IER )
      OUT = MJDSEC_TO_DATE ( MJD_EHEO_REF, TAI_EHEO_REF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3783, IUER, 'EHEO_RES', &
     &                    'Wrong MJD_EHEO_REF, TAI_EHEO_REF' )
           RETURN
      END IF
      WRITE ( 23, '(A)' ) 'E  '//OUT(1:19)
!
      WRITE ( 23, '(A)' ) '#'
      WRITE ( 23, '(A)' ) '#'
      WRITE ( 23, '(A)' ) '#  Harmonic   Phase        Frequency             Acceleration  Comment          '
      WRITE ( 23, '(A)' ) '#             rad          rad/sec               rad/sec^2                      '
      WRITE ( 23, '(A)' ) '#'
!
! --- List of phases, frequencies and acceleration
!
      DO 430 J3=1,L_EHEO
         CALL CLRCH ( OUT ) 
         IF ( L_EHEO < 1000 ) THEN
              OUT(1:8) = 'H  Term_'
              WRITE ( UNIT=OUT(9:11), FMT='(I3)' ) J3
              CALL CHASHR ( OUT(9:11) )
              CALL BLANK_TO_ZERO ( OUT(9:11) )
           ELSE 
              OUT(1:7) = 'H  Trm_'
              WRITE ( UNIT=OUT(8:11), FMT='(I4)' ) J3
              CALL CHASHR ( OUT(8:11) )
              CALL BLANK_TO_ZERO ( OUT(8:11) )
         END IF
         WRITE ( UNIT=OUT(14:25), FMT='(F11.9)'    ) EHEO(J3)%PHAS
         WRITE ( UNIT=OUT(28:46), FMT='(1PD19.12)' ) EHEO(J3)%FREQ
         WRITE ( UNIT=OUT(49:59), FMT='(1PD11.4)'  ) EHEO(J3)%ACCL
         WRITE ( UNIT=23, FMT='(A)' ) OUT(1:80)
 430  CONTINUE 
!
! --- Now we need to print estimates of cosine and sine amplitudes
!
      FL_EST_AMP_FIRST = .FALSE.
      DO 440 J4=1,L_EHEO
!
! ------ Build the record title. 
!
         CALL CLRCH ( OUT ) 
         IF ( L_EHEO < 1000 ) THEN
              OUT(1:8) = 'A  Term_'
              WRITE ( UNIT=OUT(9:11), FMT='(I3)' ) J4
              CALL CHASHR ( OUT(9:11) )
              CALL BLANK_TO_ZERO ( OUT(9:11) )
            ELSE 
              OUT(1:7) = 'A  Trm_'
              WRITE ( UNIT=OUT(8:11), FMT='(I4)' ) J4
              CALL CHASHR ( OUT(8:11) )
              CALL BLANK_TO_ZERO ( OUT(8:11) )
         END IF
         CALL NOUT_R8 ( 4, HEO_AMP_EST )
         CALL NOUT_R8 ( 4, HEO_AMP_ERR )
!
! ------ Look through the list of parameters and pick all parameters which 
! ------ correspond to the J4-th constituent
!
         FL_EST = .FALSE.
         DO 450 J5=1,L_PAR
            IF ( C_PAR(J5)(1:4) == 'HEO ' ) THEN
                 IF ( C_PAR(J5)(11:20) == EHEO(J4)%NAME(1:10) .AND. &
     &                ( C_PAR(J5)(5:6) == 'C '  .OR. &
     &                  C_PAR(J5)(5:6) == 'S '       )  ) THEN
!
! ----------------- Get the estimate and the error and put them in 
! ----------------- the appropriate slot
!
                    IF ( C_PAR(J5)(5:9) == 'C  EP' ) THEN
                         HEO_AMP_EST(HEO__E1E2,HEO__COS) = EST_VEC(J5)
                         FL_EST = .TRUE.
                       ELSE IF ( C_PAR(J5)(5:9) == 'C  E3' ) THEN
                         HEO_AMP_EST(HEO__E3,HEO__COS) = EST_VEC(J5)
                         FL_EST = .TRUE.
                       ELSE IF ( C_PAR(J5)(5:9) == 'S  EP' ) THEN
                         HEO_AMP_EST(HEO__E1E2,HEO__SIN) = EST_VEC(J5)
                         FL_EST = .TRUE.
                       ELSE IF ( C_PAR(J5)(5:9) == 'S  E3'           .AND. &
     &                           DABS(EHEO(J4)%FREQ) > EHEO__FRQ_MIN       ) THEN
                         HEO_AMP_EST(HEO__E3,HEO__SIN) = EST_VEC(J5)
                         FL_EST = .TRUE.
                    END IF
                 END IF
            END IF
 450     CONTINUE 
!
         IF ( FL_EST  .AND.  .NOT. FL_EST_AMP_FIRST ) THEN
!
! ----------- If this is hte first estimate of amplitude, let us 
! ----------- printe the header comment
!
              WRITE ( 23, '(A)' ) '#'
              WRITE ( 23, '(A)' ) '# Amplitudes of harmonic variations in the Earth rotation'
              WRITE ( 23, '(A)' ) '# '
              WRITE ( 23, '(A)' ) '# Harmonic        PM_cos       PM_sin        E3_cos        E3_sin'
              WRITE ( 23, '(A)' ) '#                   prad         prad          prad          prad'
              WRITE ( 23, '(A)' ) '#'
              FL_EST_AMP_FIRST = .TRUE.
         END IF 
!
         IF ( FL_EST ) THEN
!
! ----------- Well, time came to print estimates of the amplitude for the
! ----------- J4-th component
!
              WRITE ( UNIT=OUT(14:25), FMT='(F12.0)' ) HEO_AMP_EST(HEO__E1E2,HEO__COS)*1.D12
              WRITE ( UNIT=OUT(27:38), FMT='(F12.0)' ) HEO_AMP_EST(HEO__E1E2,HEO__SIN)*1.D12
              WRITE ( UNIT=OUT(41:52), FMT='(F12.0)' ) HEO_AMP_EST(HEO__E3,HEO__COS)*1.D12
              WRITE ( UNIT=OUT(54:65), FMT='(F12.0)' ) HEO_AMP_EST(HEO__E3,HEO__SIN)*1.D12
              WRITE ( UNIT=23, FMT='(A)' ) OUT(1:80)
         END IF
 440  CONTINUE 
!
! --- Now process estimates of rate of change of sine and cosine amplitudes
!
      FL_EST_VEL_FIRST = .FALSE.
      DO 460 J6=1,L_EHEO
         CALL CLRCH ( OUT ) 
         IF ( L_EHEO < 1000 ) THEN
              OUT(1:8) = 'V  Term_'
              WRITE ( UNIT=OUT(9:11), FMT='(I3)' ) J6
              CALL CHASHR ( OUT(9:11) )
              CALL BLANK_TO_ZERO ( OUT(9:11) )
            ELSE 
              OUT(1:7) = 'V  Trm_'
              WRITE ( UNIT=OUT(8:11), FMT='(I4)' ) J6
              CALL CHASHR ( OUT(8:11) )
              CALL BLANK_TO_ZERO ( OUT(8:11) )
         END IF
         CALL NOUT_R8 ( 4, HEO_VEL_EST )
         CALL NOUT_R8 ( 4, HEO_VEL_ERR )
         FL_EST = .FALSE.
!
! ------ Look through the list of parameters and pick all parameters which 
! ------ correspond to the J6-th constituent
!
         DO 470 J7=1,L_PAR
            IND8 = (INT8(J7)*INT8(J7+1))/INT8(2)
            IF ( C_PAR(J7)(1:4) == 'HEO ' ) THEN
                 IF ( C_PAR(J7)(11:20) == EHEO(J6)%NAME(1:10) .AND. &
     &                ( C_PAR(J7)(5:6) == 'CV'  .OR. &
     &                  C_PAR(J7)(5:6) == 'SV'       )  ) THEN
                    IF ( C_PAR(J7)(5:9) == 'CV EP' ) THEN
                         HEO_VEL_EST(HEO__E1E2,HEO__COS) = EST_VEC(J7)
                         FL_EST = .TRUE.
                       ELSE IF ( C_PAR(J7)(5:9) == 'CV E3' ) THEN
                         HEO_VEL_EST(HEO__E3,HEO__COS) = EST_VEC(J7)
                         FL_EST = .TRUE.
                       ELSE IF ( C_PAR(J7)(5:9) == 'SV EP' ) THEN
                         HEO_VEL_EST(HEO__E1E2,HEO__SIN) = EST_VEC(J7)
                         FL_EST = .TRUE.
                       ELSE IF ( C_PAR(J7)(5:9) == 'SV E3' ) THEN
                         HEO_VEL_EST(HEO__E3,HEO__SIN) = EST_VEC(J7)
                         FL_EST = .TRUE.
                    END IF
                 END IF
!
                 IF ( DABS(EHEO(J6)%FREQ) < EHEO__FRQ_MIN ) THEN
!
! ------------------- Special case of Sin E3 amplitude for zero frequency. 
! ------------------- This is rate of change of E3
!
                      IF ( C_PAR(J7)(11:20) == EHEO(J6)%NAME(1:10) .AND. &
     &                     C_PAR(J7)(5:9) == 'S  E3'                     ) THEN
!
                           HEO_VEL_EST(HEO__E3,HEO__COS) = EST_VEC(J7)
                           HEO_VEL_ERR(HEO__E3,HEO__COS) = DSQRT ( COV_MAT(IND8) )
                           FL_EST = .TRUE.
                      END IF
                 END IF
            END IF
 470     CONTINUE 
!
         IF ( FL_EST  .AND.  .NOT. FL_EST_VEL_FIRST ) THEN
              WRITE ( 23, '(A)' ) '#'
              WRITE ( 23, '(A)' ) '# Rate of change of the amplitudes of harmonic variations in the Earth rotation'
              WRITE ( 23, '(A)' ) '# '
              WRITE ( 23, '(A)' ) '# Harmonic   PM_rate_cos  PM_rate_sin   E3_rate_cos  E3_rate_sin'
              WRITE ( 23, '(A)' ) '#            10^{-21} rad/s             10^{-21} rad/s'
              WRITE ( 23, '(A)' ) '#'
              FL_EST_VEL_FIRST = .TRUE.
         END IF 
!
         IF ( FL_EST ) THEN
!
! ----------- Now print estimates of rate of change of the consituents
!
              WRITE ( UNIT=OUT(14:25), FMT='(F12.0)' ) HEO_VEL_EST(HEO__E1E2,HEO__COS)*1.D21
              WRITE ( UNIT=OUT(27:38), FMT='(F12.0)' ) HEO_VEL_EST(HEO__E1E2,HEO__SIN)*1.D21
              WRITE ( UNIT=OUT(41:52), FMT='(F12.0)' ) HEO_VEL_EST(HEO__E3,HEO__COS)*1.D21
              WRITE ( UNIT=OUT(54:65), FMT='(F12.0)' ) HEO_VEL_EST(HEO__E3,HEO__SIN)*1.D21
              WRITE ( UNIT=23, FMT='(A)' ) OUT(1:80)
         END IF
 460  CONTINUE 
!
! --- Now we need to print estimates of cosine and sine amplitude errors
! --- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      FL_ERR = .FALSE.
      FL_ERR_AMP_FIRST = .FALSE.
      DO 480 J8=1,L_EHEO
!
! ------ Build the record title. 
!
         CALL CLRCH ( OUT ) 
         IF ( L_EHEO < 1000 ) THEN
              OUT(1:8) = 'S  Term_'
              WRITE ( UNIT=OUT(9:11), FMT='(I3)' ) J8
              CALL CHASHR ( OUT(9:11) )
              CALL BLANK_TO_ZERO ( OUT(9:11) )
            ELSE
              OUT(1:7) = 'S  Trm_'
              WRITE ( UNIT=OUT(8:11), FMT='(I4)' ) J8
              CALL CHASHR ( OUT(8:11) )
              CALL BLANK_TO_ZERO ( OUT(8:11) )
         END IF
         CALL NOUT_R8 ( 4, HEO_AMP_EST )
         CALL NOUT_R8 ( 4, HEO_AMP_ERR )
         FL_ERR = .FALSE.
!
         DO 490 J9=1,L_PAR
            IND8 = (INT8(J9)*INT8(J9+1))/INT8(2)
            IF ( C_PAR(J9)(1:4) == 'HEO ' ) THEN
                 IF ( C_PAR(J9)(11:20) == EHEO(J8)%NAME(1:10) .AND. &
     &                ( C_PAR(J9)(5:6) == 'C '  .OR. &
     &                  C_PAR(J9)(5:6) == 'S '       )  ) THEN
!
! ----------------- Get the estimate and the error and put them in 
! ----------------- the appropriate slot
!
                    IF ( C_PAR(J9)(5:9) == 'C  EP' ) THEN
                         HEO_AMP_ERR(HEO__E1E2,HEO__COS) = DSQRT ( COV_MAT(IND8) )
                         FL_ERR = .TRUE.
                       ELSE IF ( C_PAR(J9)(5:9) == 'C  E3' ) THEN
                         HEO_AMP_ERR(HEO__E3,HEO__COS) = DSQRT ( COV_MAT(IND8) )
                         FL_ERR = .TRUE.
                       ELSE IF ( C_PAR(J9)(5:9) == 'S  EP' ) THEN
                         HEO_AMP_ERR(HEO__E1E2,HEO__SIN) = DSQRT ( COV_MAT(IND8) )
                         FL_ERR = .TRUE.
                       ELSE IF ( C_PAR(J9)(5:9) == 'S  E3'           .AND. &
     &                           DABS(EHEO(J8)%FREQ) > EHEO__FRQ_MIN       ) THEN
                         HEO_AMP_ERR(HEO__E3,HEO__SIN) = DSQRT ( COV_MAT(IND8) )
                         FL_ERR = .TRUE.
                    END IF
                 END IF
            END IF
 490     CONTINUE 
!
! ------ Look through the list of parameters and pick all parameters which 
! ------ correspond to the J8-th constituent
!
         IF ( FL_ERR  .AND.  .NOT. FL_ERR_AMP_FIRST ) THEN
!
! ----------- If this is hte first estimate of amplitude, let us 
! ----------- printe the header comment
!
              WRITE ( 23, '(A)' ) '#'
              WRITE ( 23, '(A)' ) '# Errors od Amplitudes of harmonic variations in the Earth rotation'
              WRITE ( 23, '(A)' ) '# '
              WRITE ( 23, '(A)' ) '# Harmonic        PM_cos       PM_sin        E3_cos        E3_sin'
              WRITE ( 23, '(A)' ) '#                   prad         prad          prad          prad'
              WRITE ( 23, '(A)' ) '#'
              FL_ERR_AMP_FIRST = .TRUE.
         END IF 
!
         IF ( FL_ERR  .AND.  .NOT. FL_ERR_VEL_FIRST ) THEN
!
! ----------- Well, time came to print errors of the amplitude for the
! ----------- J8-th component
!
              WRITE ( UNIT=OUT(15:26), FMT='(F12.1)' ) HEO_AMP_ERR(HEO__E1E2,HEO__COS)*1.D12
              WRITE ( UNIT=OUT(28:39), FMT='(F12.1)' ) HEO_AMP_ERR(HEO__E1E2,HEO__SIN)*1.D12
              WRITE ( UNIT=OUT(42:53), FMT='(F12.1)' ) HEO_AMP_ERR(HEO__E3,HEO__COS)*1.D12
              WRITE ( UNIT=OUT(55:66), FMT='(F12.1)' ) HEO_AMP_ERR(HEO__E3,HEO__SIN)*1.D12
              WRITE ( UNIT=23, FMT='(A)' ) OUT(1:80)
         END IF
 480  CONTINUE 
!
! --- Now process errors of rate of change of sine and cosine amplitudes
!
      FL_ERR = .FALSE.
      FL_ERR_VEL_FIRST = .FALSE.
      DO 4100 J10=1,L_EHEO
         CALL CLRCH ( OUT ) 
         IF ( L_EHEO < 1000 ) THEN
              OUT(1:8) = 'R  Term_'
              WRITE ( UNIT=OUT(9:11), FMT='(I3)' ) J10
              CALL CHASHR ( OUT(9:11) )
              CALL BLANK_TO_ZERO ( OUT(9:11) )
            ELSE 
              OUT(1:7) = 'R  Trm_'
              WRITE ( UNIT=OUT(8:11), FMT='(I4)' ) J10
              CALL CHASHR ( OUT(8:11) )
              CALL BLANK_TO_ZERO ( OUT(8:11) )
         END IF
         CALL NOUT_R8 ( 4, HEO_VEL_EST )
         CALL NOUT_R8 ( 4, HEO_VEL_ERR )
         FL_ERR = .FALSE.
!
! ------ Look through the list of parameters and pick all parameters which 
! ------ correspond to the J10-th constituent
!
         DO 4110 J11=1,L_PAR
            IND8 = (INT8(J11)*INT8(J11+1))/INT8(2)
            IF ( C_PAR(J11)(1:4) == 'HEO ' ) THEN
                 IF ( C_PAR(J11)(11:20) == EHEO(J10)%NAME(1:10) .AND. &
     &                ( C_PAR(J11)(5:6) == 'CV'  .OR. &
     &                  C_PAR(J11)(5:6) == 'SV'       )  ) THEN
                    IF ( C_PAR(J11)(5:9) == 'CV EP' ) THEN
                         HEO_VEL_ERR(HEO__E1E2,HEO__COS) = DSQRT( COV_MAT(IND8) )
                         FL_ERR = .TRUE.
                       ELSE IF ( C_PAR(J11)(5:9) == 'CV E3' ) THEN
                         HEO_VEL_ERR(HEO__E3,HEO__COS) = DSQRT( COV_MAT(IND8) )
                         FL_ERR = .TRUE.
                       ELSE IF ( C_PAR(J11)(5:9) == 'SV EP' ) THEN
                         HEO_VEL_ERR(HEO__E1E2,HEO__SIN) = DSQRT( COV_MAT(IND8) )
                         FL_ERR = .TRUE.
                       ELSE IF ( C_PAR(J11)(5:9) == 'SV E3' ) THEN
                         HEO_VEL_ERR(HEO__E3,HEO__SIN) = DSQRT( COV_MAT(IND8) )
                         FL_ERR = .TRUE.
                    END IF
                 END IF
!
                 IF ( DABS(EHEO(J10)%FREQ) < EHEO__FRQ_MIN ) THEN
!
! ------------------- Special case of Sin E3 amplitude for zero frequency. 
! ------------------- This is rate of change of E3
!
                      IF ( C_PAR(J11)(11:20) == EHEO(J10)%NAME(1:10) .AND. &
     &                     C_PAR(J11)(5:9) == 'S  E3'                     ) THEN
!
                           HEO_VEL_EST(HEO__E3,HEO__COS) = EST_VEC(J11)
                           HEO_VEL_ERR(HEO__E3,HEO__COS) = DSQRT( COV_MAT(IND8) )
                           FL_ERR = .TRUE.
                      END IF
                 END IF
            END IF
 4110    CONTINUE 
!
         IF ( FL_ERR  .AND.  .NOT. FL_ERR_VEL_FIRST ) THEN
              WRITE ( 23, '(A)' ) '#'
              WRITE ( 23, '(A)' ) '# Errors of rate of change of the amplitudes of harmonic variations'
              WRITE ( 23, '(A)' ) '# in the Earth rotation'
              WRITE ( 23, '(A)' ) '# '
              WRITE ( 23, '(A)' ) '# Harmonic   PM_rate_cos  PM_rate_sin   E3_rate_cos  E3_rate_sin'
              WRITE ( 23, '(A)' ) '#            10^{-21} rad/s             10^{-21} rad/s'
              WRITE ( 23, '(A)' ) '#'
              FL_ERR_VEL_FIRST = .TRUE.
         END IF 
!
         IF ( FL_ERR ) THEN
!
! ----------- Now print estimates of rate of change of the consituents
!
              WRITE ( UNIT=OUT(14:25), FMT='(F12.0)' ) HEO_VEL_ERR(HEO__E1E2,HEO__COS)*1.D21
              WRITE ( UNIT=OUT(27:38), FMT='(F12.0)' ) HEO_VEL_ERR(HEO__E1E2,HEO__SIN)*1.D21
              WRITE ( UNIT=OUT(41:52), FMT='(F12.0)' ) HEO_VEL_ERR(HEO__E3,HEO__COS)*1.D21
              WRITE ( UNIT=OUT(54:65), FMT='(F12.0)' ) HEO_VEL_ERR(HEO__E3,HEO__SIN)*1.D21
              WRITE ( UNIT=23, FMT='(A)' ) OUT(1:80)
         END IF
 4100 CONTINUE 
!
! --- Printing trailing lines
!
      WRITE ( 23, '(A)' ) '#'
      WRITE ( 23, '(A)' ) HEO__LABEL
      WRITE ( 23, '(A)' ) 'HEO  Output End: <=='
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EHEO_RES  !#!#
