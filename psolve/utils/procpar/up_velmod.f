      PROGRAM    UP_VELMOD
! ************************************************************************
! *                                                                      *
! *   Program  UP_VELMOD  updates velocities mod-file.                   *
! *                                                                      *
! *   $Ex/up_velmod.e  \                                                 *
! *                    2009a_astro \                                     *
! *                    $Vs/2009a_astro/2009a_astro_apr.vel \             *
! *                    /tmp/2009a_astro_xs.vel \                         *
! *                    $Vs/2009a_astro/2009a_astro_vel.tie \             *
! *                    $Vs/2009a_astro/2009a_nnr.list \                  *
! *                    keepITRFdef                                       *
! *                    filout                                            *
! *                                                                      *
! *  ### 26-SEP-2001   UP_VELMOD   v2.4 (c)  L. Petrov  20-OCT-2023  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M_STA
      PARAMETER  ( M_STA = 1024 )
      CHARACTER  FIL_APR_VEL*128, FIL_EST_VEL*128, FIL_REF_VEL*128, &
     &           FIL_REP*128, FIL_TIE*128, FIL_OUT*128
      CHARACTER  BUF_APR_VEL(M_STA)*80,  BUF_EST_VEL(M_STA)*512, &
     &           BUF_VEL_REF(M_STA)*512, BUF_REP(M_STA)*80,  &
     &           BUF_TIE(M_STA)*80,  BUF_OUT(M_STA)*256
      INTEGER*4  N_APR_VEL, N_EST_VEL, N_REF, N_REP, N_TIE, NOUT
      CHARACTER  C_REP(M_STA)*8, STA_NAM*8, SOL_STR*16, REF_STR*16
      CHARACTER  SOL_NAME*32, STR*32, COMMAND_LINE*512
      LOGICAL*4  FL_OVR
      REAL*8     VEL_X, VEL_Y, VEL_Z
      INTEGER*4  K_TIE, K_STA, K_REF
      INTEGER*4  L_REP, IP, K_UPD, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, LTM_DIF
!
      FL_OVR  = .FALSE.
!
      IF ( IARGC() .LT. 4 ) THEN
           WRITE ( 6, * ) 'Usage: up_velmod apr_vel_file '// &
     &                   'est_vel_file vel_tie fil_out [keepITRFdef vel_repl_file ]'
           CALL EXIT ( 1 )
         ELSE
           CALL GET_COMMAND ( COMMAND_LINE )
           CALL GETARG ( 1, FIL_APR_VEL )
           CALL GETARG ( 2, FIL_EST_VEL )
           CALL GETARG ( 3, FIL_TIE )
           CALL GETARG ( 4, FIL_OUT )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                CALL TRAN ( 12, STR, STR )
                IF ( STR == 'keepitrfdef' ) THEN
                     FL_OVR = .TRUE.
                   ELSE IF ( STR == 'nokeepitrfdef' ) THEN
                     FL_OVR = .FALSE.
                   ELSE
                     CALL ERR_LOG ( 1501, -2, 'UP_VELMOD', 'Wrong 5th '// &
     &                   'qualifier: '//STR(1:I_LEN(STR))// &
     &                   ' -- only nokeepITRFdef or keepITRFdef are allowed' )
                     CALL EXIT ( 1 )
                END IF
           END IF
           IF ( FL_OVR .AND. IARGC() < 6 ) THEN
                CALL ERR_LOG ( 1502, -2, 'UP_VELMOD', 'Missing the 6th '// &
     &              'argument: station velocity replacement file from the '// &
     &              'reference catalogue' )
                CALL EXIT ( 1 )
           END IF
           IF ( FL_OVR .AND. IARGC() < 7 ) THEN
                CALL ERR_LOG ( 1503, -2, 'UP_VELMOD', 'Missing the 7th '// &
     &              'argument: station velocity reference catalogue' )
                CALL EXIT ( 1 )
           END IF
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, FIL_REP )
           END IF
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, FIL_REF_VEL )
           END IF
      END IF
!
      CALL CLRCH ( SOL_STR )
      CALL CLRCH ( REF_STR )
      SOL_STR = FIL_EST_VEL(LINDEX( FIL_EST_VEL, '/' )+1: )
      REF_STR = FIL_REF_VEL(LINDEX( FIL_REF_VEL, '/' )+1: )
!
      IUER = -1
      CALL RD_TEXT ( FIL_APR_VEL, M_STA, BUF_APR_VEL, N_APR_VEL, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( BUF_APR_VEL(1)(1:33) .NE. '$$  VEL-MODFILE Format 2001.09.26' ) THEN
           CALL ERR_LOG ( 1504, -2, 'UP_VELMOD', 'Unsupported format of '// &
     &         'the input file for a priori site velocities '//FIL_APR_VEL )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_EST_VEL, M_STA, BUF_EST_VEL, N_EST_VEL, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( BUF_EST_VEL(1)(1:46) .NE. '# GETPAR_VEL format version 1.1  of 2023.03.11' ) THEN
           CALL ERR_LOG ( 1505, -2, 'UP_VELMOD', 'Unsupported format of '// &
     &         'the input file for site velocity estimates '//FIL_EST_VEL )
           CALL EXIT ( 1 )
      END IF
!
      IF ( .NOT. ( FIL_TIE == 'no' .OR. FIL_TIE == 'none' ) ) THEN
           IUER = -1
           CALL RD_TEXT ( FIL_TIE, M_STA, BUF_TIE, N_TIE, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE
           N_TIE = 0
      END IF
!
      IF ( FL_OVR ) THEN
           IUER = -1
           CALL RD_TEXT ( FIL_REP, M_STA, BUF_REP, N_REP, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
           IUER = -1
           CALL RD_TEXT ( FIL_REF_VEL, M_STA, BUF_VEL_REF, N_REF, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE
           N_REP = 0
           N_REF = 0
      END IF
!
      NOUT = 1
      BUF_OUT(NOUT) = '$$  VEL-MODFILE Format 2001.09.26'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  Generated with '//TRIM(COMMAND_LINE)
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  File was generated by up_velmod at '//GET_CDATE()
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  VLBI station velocities'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  ~~~~~~~~~~~~~~~~~~~~~~~'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  Generated from solution uknown'
      IF ( FL_OVR ) THEN
           NOUT = NOUT + 1
           BUF_OUT(NOUT) = '$$  Velocitites of some stations a replaced by '// &
     &                     'values from '//FIL_REF_VEL(1:I_LEN(FIL_REF_VEL))
           NOUT = NOUT + 1
           BUF_OUT(NOUT) = '$$  They overwrote velocities from the solution.'
           NOUT = NOUT + 1
           BUF_OUT(NOUT) = '$$  Velocities which were not estimated are '// &
     &                     'taken from '//FIL_APR_VEL(1:I_LEN(FIL_APR_VEL))
      END IF
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  '
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  Modifications:'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  Who  When        What'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
!
      DO 410 J1=1,N_APR_VEL
         IF ( BUF_APR_VEL(J1)(1:1)  .NE. ' ' ) GOTO 410
         NOUT = NOUT + 1
         BUF_OUT(NOUT) = BUF_APR_VEL(J1)
         CALL CLRCH ( BUF_OUT(NOUT)(61:) )
         BUF_OUT(NOUT)(65:) = 'apriori'
 410  CONTINUE
!
      K_UPD = 0
      DO 420 J2=1,NOUT
         IF ( BUF_OUT(J2)(1:4) .EQ. '    ' ) THEN
              CALL VTD_NAME_REPAIR ( BUF_OUT(J2)(5:13) )
              STA_NAM = BUF_OUT(J2)(5:13)
         END IF
         DO 430 J3=1,N_EST_VEL
            IF ( BUF_OUT(J2)(1:27) == '$$  Generated from solution' ) THEN
                 IF ( BUF_EST_VEL(J3)(1:14) == '# Solution ID:' ) THEN
!
! ------------------- Copy the solution name
!
                      SOL_NAME = BUF_EST_VEL(J3)(21:)
                      BUF_OUT(J2)(29:) = SOL_NAME 
                 END IF
            END IF
            IF ( BUF_EST_VEL(J3)(1:8) .NE. 'STA_GVX:' ) GOTO 430
            IF ( BUF_EST_VEL(J3)(11:18) .EQ. STA_NAM ) THEN
                 READ ( UNIT=BUF_EST_VEL(J3)(24:32), FMT='(F9.3)' ) VEL_X
                 READ ( UNIT=BUF_EST_VEL(J3)(50:58), FMT='(F9.3)' ) VEL_Y
                 READ ( UNIT=BUF_EST_VEL(J3)(76:84), FMT='(F9.3)' ) VEL_Z
                 CALL CLRCH ( BUF_OUT(J2) )
                 WRITE ( BUF_OUT(J2), FMT='(4X, A8, 3(7X, F9.3) )' ) STA_NAM, &
     &                   VEL_X, VEL_Y, VEL_Z
                 BUF_OUT(J2)(65:) = SOL_NAME
                 K_UPD = K_UPD + 1
            END IF
 430     CONTINUE
 420  CONTINUE
!
      L_REP = 0
      IF ( N_REP > 0 ) THEN
           DO 440 J4=1,N_REP
              IF ( BUF_REP(J4)(1:1) .EQ. '*' ) GOTO 440
              IF ( BUF_REP(J4)(1:1) .EQ. '#' ) GOTO 440
              IF ( BUF_REP(J4)(1:1) .EQ. '$' ) GOTO 440
              IF ( BUF_REP(J4)(1:1) .EQ. '!' ) GOTO 440
              IF ( ILEN(BUF_REP(J4)) .LE. 0  ) GOTO 440
              CALL VTD_NAME_REPAIR ( BUF_REP(J4)(1:8) )
              L_REP = L_REP + 1
              C_REP(L_REP) = BUF_REP(J4)
 440       CONTINUE
      END IF
!
      K_STA = 0
      K_REF = 0
      DO 450 J5=1,NOUT
         IF ( BUF_OUT(J5)(1:1)  .NE. ' ' ) THEN
              BUF_OUT(J5) = '@         '//BUF_OUT(J5)
              CALL INCH   ( J5, BUF_OUT(J5)(3:10) )
              CALL CHASHR (     BUF_OUT(J5)(3:10) )
              GOTO 450
         END IF
!
         STA_NAM = BUF_OUT(J5)(5:12)
         K_STA = K_STA + 1
!
         IP = LTM_DIF ( 0, L_REP, C_REP, STA_NAM )
         IF ( IP .GT. 0 ) THEN
              IF ( N_REF > 0 ) THEN
                   DO 460 J6=1,N_REF
                      CALL VTD_NAME_REPAIR ( BUF_VEL_REF(J6)(5:12) )
                      IF ( BUF_VEL_REF(J6)(1:1)  .NE. ' ' ) GOTO 460
                      IF ( FL_OVR  .AND.  STA_NAM .EQ. BUF_VEL_REF(J6)(5:12) ) THEN
                           BUF_OUT(J5) = BUF_VEL_REF(J6)
                           CALL CLRCH ( BUF_OUT(J5)(61:) )
                           K_REF = K_REF + 1
                           BUF_OUT(J5)(65:) = REF_STR
                      END IF
 460               CONTINUE
              END IF
         END IF
         BUF_OUT(J5)(1:1) = '|'
         CALL INCH   ( J5, BUF_OUT(J5)(101:108) )
         CALL CHASHR (     BUF_OUT(J5)(101:108) )
 450  CONTINUE
!
! --- Check the list of tie stations for velocities
!
      K_TIE = 0
      DO 470 J7=1,NOUT
         IF ( BUF_OUT(J7)(1:1) .NE. '$' ) THEN
              STA_NAM = BUF_OUT(J7)(5:12)
              IF ( N_TIE > 0 ) THEN
                   DO 480 J8=1,N_TIE
                      IF ( BUF_TIE(J8)(1:1)  == '#' ) GOTO 480
                      IF ( ILEN(BUF_TIE(J8)) ==  0  ) GOTO 480
                      IF ( BUF_TIE(J8)(11:18) == STA_NAM ) THEN
!
! ------------------------ This station is considered as a weak site in the pair.
! ------------------------ Seach for the strong partner
!
                           DO 4100 J10=1,NOUT
                              IF ( BUF_OUT(J10)(1:1) .NE. '$' .AND. &
     &                             BUF_OUT(J10)(5:12) ==  BUF_TIE(J8)(1:8) ) THEN
!
! -------------------------------- And substitute them
!
                                   BUF_OUT(J7)(15:60) = BUF_OUT(J10)(15:60)
                                   BUF_OUT(J7)(65:) = 'as '//BUF_TIE(J8)(1:8) 
                                   K_TIE = K_TIE + 1
                              END IF
 4100                      CONTINUE 
                      END IF
 480               CONTINUE 
              END IF
         END IF
 470  CONTINUE 

!
! --- Sort the list
!
      CALL SORT_CH ( NOUT, BUF_OUT )
!
      OPEN ( UNIT=11, FILE=FIL_OUT, STATUS='UNKNOWN' )
!
! --- Print the catalogue header
!
      DO 4110 J11=1,NOUT
         IF ( ILEN(BUF_OUT(J11)) .LT. 11 ) GOTO 4110
         IF ( BUF_OUT(J11)(1:1) .EQ. '@' ) THEN
              WRITE ( 11, FMT='(A)' ) BUF_OUT(J11)(11:I_LEN(BUF_OUT(J11)))
           ELSE IF ( BUF_OUT(J11)(1:1) .EQ. '|' ) THEN
              WRITE ( 11, FMT='(A)' ) ' '// &
     &                BUF_OUT(J11)(2:I_LEN(BUF_OUT(J11)(1:100)))
           ELSE
              WRITE ( 11, FMT='(A)' ) 'mumu: '// &
     &                BUF_OUT(J11)(1:I_LEN(BUF_OUT(J11)))
         END IF
 4110  CONTINUE
      CLOSE ( UNIT=11 )
!
      WRITE ( 6, * ) 'Number of stations processed:           ', INT2(K_STA)
      WRITE ( 6, * ) 'Number of stations updated:             ', INT2(K_UPD-K_REF)
      WRITE ( 6, * ) 'Number of reference stations unchanged: ', INT2(K_REF)
      WRITE ( 6, * ) 'Number of stations with velocity ties:  ', INT2(K_TIE)
      WRITE ( 6, * ) 'Output file: ', FIL_OUT(1:I_LEN(FIL_OUT))
!
      END  !#!  UP_VELMOD  #!#
