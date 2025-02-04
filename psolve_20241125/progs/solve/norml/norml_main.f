      SUBROUTINE NORML_MAIN ( NPARIN, ARR, B3DOBJ, CNSTROBJ, CNAME, F_IO_NRM, &
     &                        IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'precm.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fbcom.i'
      INCLUDE   'plist.i'
!
      REAL*8      ARR(*)
      INTEGER*2   PARMS_SHORT
      PARAMETER ( PARMS_SHORT=10 )
!
      LOGICAL*2  F_IO_NRM
      INTEGER*4  IUER
      INTEGER*2  IPARMA(PARMS_SHORT,M_GPA), IPARMS(PARMS_SHORT,M_GPA)
      CHARACTER  LPARMA(M_GPA)*20 , LPARMS(M_GPA)*20
      EQUIVALENCE (IPARMA,LPARMA), (IPARMS,LPARMS)
!
      INTEGER*2 ISTS, ITIME(5), IYEAR
      INTEGER*2 ISTR_LEN / 20 /
      INTEGER*2 ISUPVL, J, KVELO(STA_BIT_WORDS)
      INTEGER*2 ISTAO, KSTAT(STA_BIT_WORDS), KSRCC(SRC_BIT_WORDS)
      INTEGER*2 INUT(2,2,116), INDX, NSITE
      INTEGER*2 NDXREF, NDX, NDXREFSO, NDXREF1, NDXREFV
      INTEGER*2 KSTATT(STA_BIT_WORDS)
      INTEGER*2 NSRC, NVEL, NLINES
      INTEGER*2 I, IPRES_KCOV, ICNS_VEL_TIE, ICNS_STA_TIE, ICNS_STA_ORG, &
     &          ICNS_VEL_SUP
      INTEGER*2 ITEMP(4), IWDS
!
      INTEGER*4 NPARIN, NPARMA, NPARMS, IXATS(M_GPA), IRA(MAX_SRC)
      CHARACTER SOLTYP*2
      CHARACTER WHO_STA(MAX_STA)*8, TEMP*8, CURNAM*8, TMPNAM*8
      CHARACTER CNAME*(*), LNAME1*63, CURPARM*20, SID*60
      CHARACTER  FNAME*128
      PARAMETER (IWDS=10)
      LOGICAL*2 KVEL(3), KCOV, EQUAL, KBIT, K_MN
      INTEGER*4 FIRST, LAST, K, N2, I4
      INTEGER*4  L_STA
      INTEGER*8 N8, NDX8
      INTEGER*4 JA, JB, JS
      REAL*8    DELTAPS(3,MAX_STA)
      REAL*8    DAPR(M_GPA), TVEC(M_GPA), DELTAPSO(2,MAX_SRC)
      REAL*8    DELTAPV(3,MAX_STA), TIME0X
      REAL*8    Z(M_GPA)
      INTEGER*1, ALLOCATABLE :: ARR_TMP1(:), ARR_TMP2(:)
      INTEGER*4  LEN_ARR_TMP1, LEN_ARR_TMP2
      EQUIVALENCE ( ITEMP(1), TEMP )
      LOGICAL*4    FL_UGL_OBSOLETE
      LOGICAL*4    TRUE_L4
      PARAMETER  ( TRUE_L4 = .TRUE. )
      CHARACTER  C_STA(MAX4_SIT)*8, OUT*4096
!
      DATA         ISTS / 1 /
!C
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
      CHARACTER  STR*54, STR1*128, FINAM_NRM*128, FINAM_FAST*128, &
     &           FINAM_COV*128, OBJ_SAVE*10
      CHARACTER  GET_VERSION*54
      INTEGER*8        LEN_WEI_CNS, MEM_LEN
      ADDRESS__TYPE :: ADR_WEI_CNS, MEM_ADR
      INTEGER*4  CNI_SAVE, J1, J2, J3, J4, ICODE, IER
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      LOGICAL*1  FL_DEBUG_CNS_MAT 
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*1  FL_NN_LISTING 
      LOGICAL*4  F_FD, FL_NOFD_IGNORE, FL_FULL_WEI 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*8, EXTERNAL :: INDX8
!CCCCC
!
!     HISTORY
!   JLR  921216  replaced nJ with I4Pn's
!   mwh  940201  implemented dynamic memory allocation for large matrices
!   kdb  950720  Add the ability to have horizontal-only or vertical-only
!                velocity_origin constraints.
!                Also, comment out obsolete code for applying a horizontal
!                and vertical velocity_origin constraint in a solution
!                estimating UEN sites.
!  jmg   950805  Modified user_constraint so will work correctly with
!                global runs. This is done by calling "do_user_constraint".
!  kdb   951108  Parameterize starting points in the normal matrix and
!                O-C (from 6000 and 9000 to 2* and 3*M_GPA)
!  kdb   951109  Distinguish between norml matrix zeroing messages
!                (helpful for testing).
!  jmg   960206  Call user constraint for interactive runs.
!  kdb   960422  Fix error: batch features such as zeroing out the
!                normal matrix weren't working because they were
!                passed through socom variable ipres, and
!                use_cgmf_com resets socom from the cgm.
!  kdb   960618  Fix error: weak site constraints weren't activated for
!                the interactive mode.
!  jmg   960610  Remove holleriths.
!  jmg   9611??  Modify to test five new constraint options.
!  kdb   961125  Modify to officially add the four new constraint options
!                       no net translation and rotation for positions and
!                       velocities)
!  kdb   970115  Modify to officially add a fifth new constraint option,
!                       no net rotation for sources.
!  pet   970117  Modify to implement B3D algorithm
!                       no net translation and rotation for positions and
!                       velocities)
!  pet   970130  Rewrote comments, cleaned text
!  jmg   970217  Modified so that User_constraint can have arbitrary linear
!                         linear constraints, not just constrain to 0.
!  pet   970307  Added FAST_BYPASS
!  pet   970523  Bug fixed with input CGM
!  pet   970613  Added possibility to look at normal matrix in MATView mode
!  pet   970712  Changed messages in the mode when NORML called by REWAY
!  pet   970922  Minor changes in the format of NOMAL_B3D
!  pet   980206  Changed interface almost all constrint routines in order to
!                allow to gather information about constraints applied
!  pet   980211  Changed the arguments list of apply_cnstr in order to
!                support update of the elements of normal vector
!  pet   980509  Added support of priting normal matrix in batch mode if
!                FAST_MODE is NONE.
!  pet   980721  Added openning and closing spool file in the middle of the text
!  pet   980722  Changed calls of various subroutines inmposing constraints
!                in order to pass values of sigmas as actual parameters
!  pet   990301  Added actual parameter IUER. Moved END_PROG from norml_main.f
!                to norml.f . Changed interface to gstap, gvelp, gsouc.
!                Add initialization of  ndxref, ndxref1, ndxrefv, ndxrefso
!  pet   990303  Added an actual parameter F_IO_NRM. If F_IO_NRM = .TRUE.
!                then CGM (or normal equations in independent mode) is read,
!                otherwise not and it is assumed that an array ARR already
!                contains it and SOCOM and PRFIL are already loaded
!  pet   990403  Added support of NO TRAIN mode for independent solution
!  pet   1999.05.31  Made CNSTROBJ a formal parameter
!  pet   1999.10.08  Added support of computation of correlations between
!                    adjustments in according with post OCT99 scheme
!  pet   2000.11.24  Removed calls do_velc and do_staw and replace them with
!                    do_stavel
!  pet   2000.11.28  Forced Solve to remove the scratch file with constraints
!                    before an attempt to apply constraints. Otherwise the
!                    stale constraint file may remain in disk in the case when
!                    no constraints are imposed.
!  pet   2001.03.09  Changed the number of parameters in the argument list for
!                    DO_USER_CONSTRAINT
!  pet   2001.04.13  changed DPPCO to DPPCO_SOLVE
!  pet   2001.05.25  Got rid of archaic code whcih tried to predict how long
!                    normal matrix will be inverted. Computers became 1000
!                    faster since the time when that code was written and now
!                    an attempt to computer the number of operations causes
!                    integer overflow :-)
!  pet   2001.10.16  Added warning which prohibts pringin normnal matrix in
!                    fast mode, since existing routine for that assumes that
!                    the full normal martix is kept.
!  pet   2002.09.25  Massive changes realted to changes in handling constraints.
!                    Changed calls to DO_USER_CONSTRAINT, DO_STAT, DO_STATT,
!                    DO_STAVELC, DO_VEL, DO_VELO, DO_VELH, DO_VELOH, DO_VELT,
!                    DO_Ra, DO_DC, DO_PWC
!  pet   2002.09.25  Added support of a kludge variable USER_GLOBAL_CONSTRAINT.
!                    If it is defined nad has a value "OBSOLETE", then
!                    alternative, obsolete format of user global constraints
!                    file will be used.
!  jwr   2003.05.15  TRUE__L2 and FALSE__L2 introduced in -i2 conversion.
!  pet   2003.08.12  Replaced .NOT. CRES_PRE98 with FL_NN_LISTING
!  pet   2005.03.04  Added support of harmonic site position variations 
!                    and spline parameterization of site positions 
!                    parameterized by a spline
!  pet   2006.01.31  Changed logic of NORML_MINA: ther previous version &
!                    allocated mamory for full wight matrix of constraints.
!                    The new version does it only if a) there are non-diagonal &
!                    elements of the weight matrix of constraints; &
!                    kludge environemetn variagle NOFD_IGNORE is not set
!  pet   2006.06.30  Added support EHEO_CLS_CNST, EHEO_ERM_CNST, ERM_CNST &
!                    constraints
!  pet   2007.08.09  Added support of source structure admittance constraints
!  pet   2015.12.26  Added support of the external matrix inverted in order &
!                    to invert large matrices with too few memory available
!  pet   2023.10.05  Fixed a bug related to interger*2/integer*4 in the argument 
!                    for update_spe
!
!  ARR(1) pointer to vectrs of scales
!
      JA = 1 + 3*M_GPA  !  pointer to normal matrix
      JB = 1 + 2*M_GPA  !  pointer to normal vector
      JS = 1 +   M_GPA  !  pointer to vectors of sigmas
!
! --- Setting flag of printing the ouput intformation at the screen usiing
! --- "_MN" interface (curses)
!
      K_MN = KSCREEN .AND.KBIT ( PRE_IP ( 2 ), INT2(6) )  ! Interactide mode
      IF ( KBIT ( PRE_IP ( 3 ), INT2(12) ) .AND. REWAY_VERBOSE ) THEN
           K_MN = .FALSE.  ! But supress printout
      END IF
!                                              !  in silent REWAY mode
      CALL CLRCH ( FINAM_NRM )
      FINAM_NRM  = PRE_SCR_DIR(1:PRE_SD_LEN)//'NRMF'//PRE_LETRS
!
      CALL CLRCH ( FINAM_FAST )
      FINAM_FAST = PRE_SCR_DIR(1:PRE_SD_LEN)//'FAST'//PRE_LETRS
! 
      IF ( CRES_STYLE == CRES__PRE98 ) THEN
           FL_NN_LISTING = .FALSE.
         ELSE 
           FL_NN_LISTING = .TRUE.
      END IF
!
      CALL GETENVAR ( 'DEBUG_CNS_MAT', STR )
      IF ( STR(1:1) == 'Y' .OR. STR(1:1) == 'y' ) THEN
           FL_DEBUG_CNS_MAT = .TRUE.
         ELSE
           FL_DEBUG_CNS_MAT = .FALSE.
      END IF
!
! --- Zeroing vectors DELTAPS, DELTPV, DELTAPSO, DAPR
!
      DO I=1,MAX_STA
         DO J=1,3
            DELTAPS(J,I) = 0.D0
            DELTAPV(J,I) = 0.D0
         ENDDO
      ENDDO
      DO I=1,MAX_SRC
         DO J=1,2
            DELTAPSO(J,I) = 0.D0
         ENDDO
      ENDDO
      DO J1=1,M_GPA
         DAPR(J1) = 0.0D0
      ENDDO
      NVEL = 0
      NDXREFV = 0
!
      CALL GETENVAR ( 'NOFD_IGNORE', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:3) == 'YES' .OR.  STR(1:2) == 'ON' ) THEN
           FL_NOFD_IGNORE = .TRUE.
         ELSE 
           FL_NOFD_IGNORE = .FALSE.
      END IF
!
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6) ) ) THEN
!
! -------- Printout on the screen version number
!
           CALL START_MN()
           STR = GET_VERSION()
           CALL SETCR_MN ( 79-I_LEN(STR), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
!
           if ( kbit ( pre_ip(3), INT2(12) ) ) then
                call clrch ( str )
                call inch  ( int4(reway_itcou), str )
                CALL SETCR_MN ( 0, 0 )
                call addstr_f ( '  REWAY --> NORML     Iteration '// &
     &                         str(1:i_len(str)) )
                call setcr_mn ( 50, 0 )
           end if
           call refresh_mn()
      ENDIF
!
      CALL USE_GLBFIL ( 'ORC' )
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Timer initialization
!
           CALL TIM_INIT ( )
      END IF
!
! --- Reading files which keeps common areas
!
      OLD_USER_PART = NUM_USER_PART
      IF ( F_IO_NRM ) THEN
           CALL USE_GLBFIL_4 ( 'ORC' )
           CALL USE_PARFIL   ( 'ORC' )
           CALL USE_COMMON   ( 'ORC' )
           CALL SOCOM_EXT()
      END IF
!
! --- In the case of modeling non-linear site position variations, 
! --- we need to expand socom_plus
!
      IF ( L_HPE > 0  .AND.  ADR_HPE .NE. 0 ) THEN
           CALL HPESOL_CREATE ( %VAL(ADR_HPE) )
           FL_HPESOL = .TRUE.
         ELSE
           FL_HPESOL = .FALSE.
      END IF
!
      IF ( L_SPE > 0  .AND.  ADR_SPE .NE. 0 ) THEN
           CALL SPESOL_CREATE ( %VAL(ADR_SPE) )
           FL_SPESOL = .TRUE.
         ELSE
           FL_SPESOL = .FALSE.
      END IF
!
      IF ( L_EERM > 0  .AND.  ADR_EERM .NE. 0 ) THEN
           CALL EERM_CREATE ( %VAL(ADR_EERM) )
           FL_EERM = .TRUE.
         ELSE
           FL_EERM = .FALSE.
      END IF
!
      IF ( L_EHEO > 0  .AND.  ADR_EHEO .NE. 0 ) THEN
           CALL HEOSOL_CREATE ( L_EHEO, %VAL(ADR_EHEO) )
         ELSE
           FL_EHEO = .FALSE.
      END IF
!
! --- Attempt to override FAST_MODE from environment variable and make test
! --- of eligibility FAST_MODE for this session
!
      IF ( ISLTY2(1:1) .EQ. 'I' ) THEN
!
! -------- Test will be done only for "Independent" solution
!
           CALL ERR_PASS    ( IUER, IER )
           CALL FAST_BYPASS ( IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8411, IUER, 'NORML_MAIN', 'Error in '// &
     &              'trying to bypass fast_mode' )
                RETURN
           END IF
      END IF
!
! --- If kcov is set and F_IO_NRM is TRUE then norml will read the cgm
! --- into socom and overwrite ipres, destroying the flag that tells
! --- whether or not to zero the normal matrix, etc.
!
      IPRES_KCOV = IPRES
      KCOV = ISOLU.EQ.1 .OR. (ICONT.EQ.0 .AND. IOCGM.NE.0 )
      IF ( FAST_DBG .EQ. F__APP ) THEN
           write  ( 6, 210 ) b3dobj%dbname_mes, fast_mode, fast_dbg, fast_cov
 210       format ( 1X,' NORML:  session ',a,' fast_mode=',i4, &
     &                 ' fast_dbg=',i4,' fast_cov=',i2 )
      END IF
!
      IF ( KCOV ) THEN
!
! ------ Initilaization
!
         NDXREF   = 0
         NDXREF1  = 0
         NDXREFV  = 0
         NDXREFSO = 0
!
! ------ Remove the previous file with global constraints
!
         CALL UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//'CSPG'//PRE_LETRS//CHAR(0) )
         CALL UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//'UGLC'//PRE_LETRS//CHAR(0) )
!
! ------ Reading 2-nd mapping files if it is necessary and making the lists
! ------ of parameters to be mapped
!
         IF ( STAMOD2 .NE. ' ' ) THEN
              LNAME1 = PRE_SAV_DIR(:PRE_SV_LEN)//STAMOD1
              CALL GSTAP ( LSINAM, NSITE, SUBXYZ, LNAME1, FALSE__L2 )
            ELSE
              NSITE = 0
         ENDIF
         IF ( SRCMOD2 .NE. ' ') THEN
              LNAME1 = PRE_SAV_DIR(:PRE_SV_LEN)//SRCMOD1
              CALL GSOUC ( LSONAM, NSRC, SUBRD, LNAME1, FALSE__L2 )
            ELSE
              NSRC = 0
         ENDIF
         IF ( VELMOD2 .NE. ' ' ) THEN
              LNAME1 = PRE_SAV_DIR(:PRE_SV_LEN)//VELMOD1
              CALL GVELP ( LVELNAM, NVEL, SUBVEL, LNAME1, FALSE__L2, TIME0X )
            ELSE
              NVEL = 0
         ENDIF
!
         IF ( F_IO_NRM ) THEN
!
! ----------- Reading CGM
!
              CALL ACS_CGMFIL   ( CNAME, 'O' )
              CALL USE_CGMF_COM ( 'R' )
              NPARIN = NPARAM
              IF ( L_SPE    > 0       .AND.  &
     &             PARM_NUM > 0       .AND.  &
     &             ADR_SPE .NE. 0            ) THEN
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL UPDATE_SPE ( PARM_NUM, CPARM_NAMES, L_SPE, &
     &                               %VAL(ADR_SPE), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8412, IUER, 'NORML_MAIN', 'Error in '// &
     &                      'an attempt to restore estimation status from '// &
     &                      'the parameter list' )
                        RETURN 
                  END IF
                  CALL SPESOL_CREATE ( %VAL(ADR_SPE) )
                  CALL HPESOL_CREATE ( %VAL(ADR_HPE) )
              END IF
         END IF
!
! ------ Reading primary mapping plate motion file
!
         IF ( PLTMOD2 .NE. ' ' ) THEN
              CALL SPLAT ( PLTMOD1, SUBVEL, NVEL, LVELNAM, PLATE_FACT )
         ENDIF
!
! ------ Building vector of correction to station coordinates due to
! ------ the secondary mapping file corrections
!
         DO I=1,NUMSTA
            CALL HOL2CHAR ( ISITN(1,I), INT2(1), INT2(8), TMPNAM )
            IF ( TMPNAM .EQ. STAREF ) THEN
                 NDXREF1 = I
            ENDIF
            DO J=1,NSITE
               CALL HOL2CHAR( LSINAM(1,J), INT2(1), INT2(8), CURNAM )
               IF ( CURNAM .EQ. STAREF ) THEN
                    NDXREF = J
               ENDIF
               IF ( TMPNAM .EQ. CURNAM ) THEN
                  DO K=1,3
                     DELTAPS(K,J)=SUBXYZ(K,J) - VSITEC(K,I)
                     VSITEC (K,I)=SUBXYZ(K,J)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
!
! ------ Building vector of correction to station velocities due to
! ------ the secondary mapping file corrections
!
         DO I=1,NUMSTA
            CALL HOL2CHAR( ISITN(1,I), INT2(1), INT2(8), TMPNAM )
            DO J=1,NVEL
               CALL HOL2CHAR( LVELNAM(1,J), INT2(1), INT2(8), CURNAM )
               IF ( TMPNAM .EQ. CURNAM ) THEN
                  DO K=1,3
                     IF ( .NOT.KBIT(LSITEV(1,K),I) ) NDXREFV=I
                        DELTAPV(K,J)=SUBVEL(K,J)/1000.D0 - VSITEV(K,I)
                        VSITEV(K,I) =SUBVEL(K,J)/1000.D0
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
!
! ------ Buildig vector of correction to source coordinates due to
! ------ the secondary mapping file corrections
!
         DO I=1,NUMSTR
            CALL HOL2CHAR( ISTRN(1,I), INT2(1), INT2(8), TMPNAM )
            DO J=1,NSRC
               CALL HOL2CHAR( LSONAM(1,J), INT2(1), INT2(8), CURNAM )
               IF ( CURNAM(:5) .EQ. SRCREF(:5) ) THEN
                    NDXREFSO = J
               ENDIF
               IF ( TMPNAM .EQ. CURNAM ) THEN
                  DO K=1,2
                     DELTAPSO(K,J)=SUBRD(K,J) - VSTARC(K,I)
                     VSTARC(K,I)  =SUBRD(K,J)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
!
         IF ( F_IO_NRM ) THEN
              CALL USE_CGMF_MAT ( ARR, NPARIN, 'R' )
         END IF
         IF ( KBIT( IPRES_KCOV, INT2(6) ) ) THEN
!
! ----------- Zeroing normal matrix and normal vector
!
              N8 = INT8(NPARIN)*(INT8(NPARIN)+1)/2
              WRITE ( *, * ) 'Zeroing out normal matrix,O-C'
              CALL NOUT8_R8 ( N8, ARR(3*M_GPA) )
              CALL NOUT_R8  ( NPARIN, ARR(1+2*M_GPA) )
         ENDIF
!
! ------ Print normal matrix if we have been requested to
!
         IF ( KBIT( IPRES_KCOV, INT2(5) ) ) THEN
              WRITE ( 23, '("NORMAL MATRIX:")' )
              FIRST=0
              LAST=0
              DO I=1,NPARIN
                 NLINES = (I+6)/7
                 DO J=1,NLINES
                    FIRST = 1+LAST
                    LAST = MIN(7,I-7*(J-1))+LAST
                    IF (J.EQ.1) THEN
                        WRITE(23,'(i6,7(1x,e15.9))')I, &
     &                       (ARR(JA-1+K),K=FIRST,LAST)
                      ELSE
                        WRITE(23, &
     &                       '(6x,7(1x,e15.9))')(ARR(JA-1+K),K=FIRST,LAST)
                    ENDIF
                 ENDDO
              ENDDO
         ENDIF
!
         IF ( F_IO_NRM ) THEN
              CALL ACS_CGMFIL ( CNAME, 'C' )
         END IF
      ELSE ! not kcov
        NPARIN=NPARAM
!
        IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD  .OR. &
     &       FAST_MODE .EQ. F__B1D ) THEN
!
! --------- Independent mode (Case when only one session treated, non CGM)
!
            IF ( F_IO_NRM ) THEN
                 CALL USE_NRMFIL ( ARR, NPARIN, 'OR' )
            END IF
            IF ( KBIT( IPRES, INT2(6) ) ) THEN
                 N2 = NPARIN * (NPARIN+1) / 2
                 WRITE(*,*) 'Zeroing the normal matrix,O-C'
                 DO I4=1,N2
                    ARR(I4+3*M_GPA) = 0.D0
                 ENDDO
                 DO I=1,NPARIN
                    ARR(I+2*M_GPA) = 0.D0
                 ENDDO
            ENDIF
!
            IF ( KBIT( IPRES, INT2(5) ) .AND. KBATCH ) THEN
!
! -------------- Printing normal matrix in batch mode
!
                 CALL PRINT_NORMAL ( INT2(23), ARR, JA, NPARIN )
               ELSE IF ( KBIT( IPRES, INT2(5) ) .AND. .NOT. KBATCH ) THEN
!
! -------------- Printing normal matrix in interactive mode
!
                 CALL END_MN()  ! postpone curser
                 CALL UN_CURSES ( )
!
! -------------- Request: how it would be better to print normal matrix?
!
                 IF ( FAST_MODE .EQ. F__NONE ) THEN
                      WRITE ( 6, FMT='(A)' ) ' '
                      WRITE ( 6, FMT='(A)' ) 'NORML: Where to print normal '// &
     &                       'matrix: in spool_file or at the screen '
                      WRITE ( 6, FMT='(A$)' ) '(MATView interface)   '// &
     &                                        ' File/(Screen) >> '
                      READ ( UNIT=5, FMT='(A)' ) STR
                      CALL CHASHL ( STR )
                      CALL TRAN ( 11, STR, STR )
                      IF ( STR(1:1) .EQ. 'F' ) THEN
                           CALL PRINT_NORMAL ( INT2(23), ARR, JA, NPARIN )
                        ELSE
!
! ------------------------ Print normal matrix on the screen in MATView mode
!
                           CALL MATVIEW ( 3, NPARIN, NPARIN, &
     &                          ARR(JA), 'NORMAL MATRIX', '(1PD15.7,1X)',1, 1, IER )
                           CALL START_MN()  ! start curser again
                      ENDIF
                    ELSE
                      WRITE ( 6, '(A)' ) 'You cannot use this option in '// &
     &                            'fast mode. Use tb3d or run solve in '// &
     &                            'non-fast mode'
                      CALL PAUSE ( 'Hit any key to proceed' )
                 ENDIF
            ENDIF
        END IF
      ENDIF
!
! === Set up the run code
!
      IF ( K_MN ) then
!
           call setcr_mn ( 1, 2 )
           if ( fast_mode .eq. f__none ) then
                call addstr_f ( "Solving normal system in FULL "// &
     &                          "mode is running now ... " )
             else if ( fast_mode .eq. f__prd   .or. &
     &                 fast_mode .eq. f__b1d   .or. &
     &                 fast_mode .eq. f__b1b3d      ) then
                call addstr_f ( "Solving normal system in fast DOT_PRODUCT "// &
     &                          "mode is running now ... " )
             else if ( fast_mode .eq. f__b3d ) then
                call addstr_f ( "Solving normal system in B3D mode "// &
     &                          " is running now ... " )
           end if
           call setcr_mn ( 78, 2 )
           call refresh_mn()
           call end_mn()
           if ( kbit( ipres, INT2(5) ) .and. fast_mode .eq. f__b3d ) then
                call hit_cont ( 'To print NORML matrix turn on FULL mode. '// &
     &              'You cannot do it in fast mode.', 1 )
           end if
      END IF
!
      CALL TIMEGET ( ITIME, IYEAR )
      IF ( IYEAR < 2000 ) THEN
           IRNCD(1) = (IYEAR - 1970)*1000 + ITIME(5)
           IRNCD(2) = ITIME(3) + ITIME(4)*100
         ELSE 
           IRNCD(1) = (IYEAR - 2000)*1000 + ITIME(5)
           IRNCD(2) = ITIME(3) + ITIME(4)*100
      END IF
!
! --- Calculation LPARMA -- list of parameters to be estimated;
! ---             NPARMA -- the number of paramaters to be estimated
!
      IF ( ISLTY2 .EQ. 'I' ) THEN
           KGLOBONLY = .FALSE.
           CALL GET_NAMES ( LPARMA, ISTR_LEN, M_GPA, NPARMA, TRUE__L2, &
     &          FALSE__L2 )
         ELSE
           CALL GET_NAMES ( LPARMA, ISTR_LEN, M_GPA, NPARMA, TRUE__L2, &
     &          TRUE__L2 )
      END IF
!
      IF ( FAST_MODE .EQ. F__B3D ) THEN
           IF ( F_IO_NRM ) THEN
!
! ------------- Reading fields to B3DOBJ objects
!
                CALL ERR_PASS  ( IUER, IER )
                CALL RDNOR_B3D ( FINAM_NRM, B3DOBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8413, IUER, 'NORML_MAIN', &
     &                   'Error during reading file '// &
     &                    FINAM_NRM(1:I_LEN(FINAM_NRM))//' with temporary '// &
     &                   'data structure for B3D algorithm while database '// &
     &                    B3DOBJ%DBNAME_MES//' was processing' )
                     RETURN
                ENDIF
           ENDIF
!
! -------- In the case of B3D algorithm we use such a trick: zeroing normal
! -------- matrix. So next several hundreds lines of code (imposing constraint)
! -------- will be modifiy "clear normal matrix". To the end of this process
! -------- arr(ja) will contain not a constrained normal matrix, but will be
! -------- the matrix of constraints. Note: arr(ja) will contain elements in
! -------- the order of FULL version of SOLVE, not in the order of B3D. So after
! -------- filling it by constraints it will be neccessary to reorder
! -------- in according to the order of parameters for B3D case and update
! -------- B3D normal matrix. THis is a depricated code whcih will be
! -------- eliminiated soon.
!
           IF ( NPARIN .LE. 0 ) THEN
                CALL ERR_LOG ( 8414, IUER, 'NORML_MAIN', 'Attempt to '// &
     &              'use B3D algorithm for making global estimation. '// &
     &              'Nparin=0. This error occured while database '// &
     &               B3DOBJ%DBNAME_MES//' was processing' )
                RETURN
           ENDIF
           CALL NOUT8_R8 ( (INT8(NPARIN)*(INT8(NPARIN)+1))/2, ARR(JA) )
           CALL NOUT_R8  (       NPARIN,                      ARR(JB) )
         ELSE IF ( FAST_MODE .EQ. F__NONE   .OR. &
     &             FAST_MODE .EQ. F__PRD    .OR. &
     &             FAST_MODE .EQ. F__B1D    .OR. &
     &             FAST_MODE .EQ. F__B1B3D        ) THEN
!
! -------- Test for validity FAST_MODE
!
           CONTINUE
         ELSE
           CALL CLRCH (            STR )
           CALL INCH  ( FAST_MODE, STR )
           CALL ERR_LOG ( 8415, IUER, 'NORML_MAIN', 'Internal '// &
     &         'error: unsupported value  FAST_MODE:  FAST_MODE='// &
     &          STR(1:I_LEN(STR))//'. Error occured while database '//B3DOBJ%DBNAME_MES// &
     &         ' was processing' )
           RETURN
      ENDIF
!
      IF ( KCOV ) THEN
!
! -------- Imposed constraints on global parameters
!
           WRITE ( 23, '(A)' ) 'Beginning of global constraint section'
           WRITE ( 23, '(A)' ) ' '
!
! -------- Initialization of the CNSTROBJ object
!
           CALL ERR_PASS ( IUER, IER )
           CALL INIT_CNS ( CNI__GLO, 'GLOBAL    ', CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8416, IUER, 'NORML_MAIN', 'Failure in '// &
     &              'an attempt to initialize CNSTROBJ' )
                RETURN
           END IF
!
! -------- Making corrections to the right parts of the normal equations due to
! -------- the secondary mapping file
!
           DO I=1,NPARAM
              INDX=0
              CALL HOL2CHAR( IPARMA(1,I), INT2(1), INT2(20), CURPARM )
              IF ( CURPARM(10:10).EQ.'X' .OR. CURPARM(10:14).EQ.'RIGHT' ) THEN
                   INDX=1
                ELSE IF ( CURPARM(10:10).EQ.'Y' .OR. CURPARM(10:11).EQ.'DE' )THEN
                   INDX=2
                ELSE IF ( CURPARM(10:10).eq.'Z' ) THEN
                   INDX=3
              ENDIF
!
              IF ( CURPARM(12:19).EQ.'VELOCITY' .AND. INDX.GT.0 ) THEN
!
! ---------------- Velocity...
!
                   DO J=1,NVEL
                      CALL HOL2CHAR ( LVELNAM(1,J), INT2(1), INT2(8), CURNAM )
                      IF ( CURNAM.EQ.CURPARM(1:8) ) THEN
                           DAPR(I)=DELTAPV(INDX,J)
                      ENDIF
                   ENDDO
                 ELSE IF ( (CURPARM(10:14).EQ.'RIGHT' .OR. CURPARM(10:11).EQ. &
     &                     'DE').AND. INDX.GT.0 ) THEN
!
! ---------------- Right ascension...
!
                   DO J=1,NSRC
                      CALL HOL2CHAR ( LSONAM(1,J), INT2(1), INT2(8), CURNAM )
                      IF ( CURNAM .EQ. CURPARM(1:8) ) THEN
                           DAPR(I)=DELTAPSO(INDX,J)
                           IF ( CURPARM(10:14).EQ.'RIGHT' .AND. NDXREFSO.GT.0) THEN
                                DAPR(I)=DAPR(I) - DELTAPSO(INDX,NDXREFSO)
                           ENDIF
                      ENDIF
                   ENDDO
                 ELSE IF (INDX.GT.0) THEN
!
! ---------------- Station coordinates
!
                   DO J=1,NSITE
                      CALL HOL2CHAR ( LSINAM(1,J), INT2(1), INT2(8), CURNAM )
                      IF ( CURNAM .EQ. CURPARM(1:8) ) THEN
                           DAPR(I)=DELTAPS(INDX,J)
                      ENDIF
                   ENDDO
                   IF ( NDXREF.GT.0 ) DAPR(I) = DAPR(I) - DELTAPS(INDX,NDXREF)
!
                   IF ( NDXREFSO.GT.0 ) THEN
                        DO J=1,NUMSTA
                           CALL HOL2CHAR ( ISITN(1,J), INT2(1), INT2(8), CURNAM )
                           IF ( CURNAM .EQ. CURPARM(1:8) ) THEN
                                NDX=J
                           ENDIF
                        ENDDO
                        IF ( INDX.EQ.1 ) DAPR(I) = DAPR(I) + DELTAPSO(1, NDXREFSO)* &
     &                                                      ( VSITEC(2,NDX) - VSITEC(2,NDXREF1) )
                        IF ( INDX.EQ.2 ) DAPR(I) = DAPR(I) - DELTAPSO(1, NDXREFSO)* &
     &                                                      ( VSITEC(1,NDX) - VSITEC(1,NDXREF1))
                   ENDIF
              ENDIF
           ENDDO ! NPARAM
!
! -------- This part of code update normal vector:
! -------- ARR(JB) = ARR(JB) - ARR(JA)*DAPR
!          ( Here ARR(JA) -- normal matrix, DAPR -- vector of corrections )
!
           DO J1=1,NPARAM
              TVEC(J1) = 0.d0
              DO J2=1,NPARAM
                 NDX8 = INDX8(J1,J2) + JA-1
                 TVEC(J1) = TVEC(J1) + ARR(NDX8)*DAPR(J2)
              ENDDO
              ARR(JB+J1-1) = ARR(JB+J1-1) - TVEC(J1)
           ENDDO
!
! -------- Open covfile:  create if not there:  write directory record
! -------- write commons
!
           CALL CREATE_COVF  ( 'U', NRMFL_PARMS )
           WRMS(3) = 1.0D0
           CALL USE_COVF_COM ( 'W' )
!
! -------- Handle constraints:
!
           CALL HOL2CHAR ( ISOLTYP, INT2(1), INT2(1), SOLTYP )
           IF ( SOLTYP.EQ.'S' ) THEN
!
                CALL HOL2CHAR ( SOLUID, INT2(1), INT2(60), SID )
                CNAME='SAVE'
                CALL CREATE_CGMF ( CNAME, SID, NPARIN, 'M', OUTCGM )
!
                CALL PARCNG()
                CALL USE_CGMF_COM ( 'W' )
           ENDIF
!
! -------- CONSTRAINT SECTION
!          ~~~~~~~~~~~~~~~~~~
!
! -------- Special trick whcih would allow to use obsolete form of user
! -------- constraints
!
           FL_UGL_OBSOLETE = .FALSE.
           CALL GETENVAR ( 'USER_GLOBAL_CONSTRAINT', STR )
           IF ( STR .EQ. 'OBSOLETE'  .OR.  STR .EQ. 'obsolete' ) THEN
                FL_UGL_OBSOLETE = .TRUE.
           END IF
!
! -------- Global solution
!
           IF ( KUSER_CONST  .AND.  .NOT. FL_UGL_OBSOLETE ) THEN
!
! ------------- A special trick: write down cgm-type socom/parfile in order 
! ------------- to pass it to user progam
!
                LEN_ARR_TMP1 = JSOCOM_BYTES 
                LEN_ARR_TMP2 = JPARFIL_BYTES  
                ALLOCATE ( ARR_TMP1(LEN_ARR_TMP1) )
                ALLOCATE ( ARR_TMP2(LEN_ARR_TMP2)  )
                CALL LIB$MOVC3 ( LEN_ARR_TMP1, PI_VAR, ARR_TMP1 )
                CALL LIB$MOVC3 ( LEN_ARR_TMP2, VAXOF,  ARR_TMP2 )
!
                FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'COMM'//PRE_LETRS
                CALL SYSTEM ( 'cp '//FNAME(1:I_LEN(FNAME))//' '// &
     &                               FNAME(1:I_LEN(FNAME))//'_TMP '//CHAR(0) ) 
                FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'PARF'//PRE_LETRS
                CALL SYSTEM ( 'cp '//FNAME(1:I_LEN(FNAME))//' '// &
     &                               FNAME(1:I_LEN(FNAME))//'_TMP '//CHAR(0) ) 
!
                CALL USE_COMMON  ( 'OWC' )
                CALL USE_PARFIL  ( 'OWC' )
!
! ------------- Run user constraint program.
!
                CALL RUN_PROG ( USER_CONST_PROG, 'WAIT', INT2(1) )
!
                CNI_SAVE = CNSTROBJ%CNS_TYP
                OBJ_SAVE = CNSTROBJ%OBJ_NAM
!
! ------------- Read the file with user constraints imposed on global parameters
!
                CALL ERR_PASS   ( IUER, IER )
                CALL READ_CNSTR ( CNSTROBJ, CNI__UGL, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8417, IUER, 'NORML_MAIN', 'Error '// &
     &                   'in an attempt to read user global constraints '// &
     &                   'in CNSTROBJ object from scratch file' )
                     RETURN
                END IF
!
! ------------- Restore socom/parfil
!
                CALL LIB$MOVC3 ( LEN_ARR_TMP1, ARR_TMP1, PI_VAR )
                CALL LIB$MOVC3 ( LEN_ARR_TMP2, ARR_TMP2, VAXOF  )
                DEALLOCATE ( ARR_TMP1 )
                DEALLOCATE ( ARR_TMP2 )
!
                FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'COMM'//PRE_LETRS
                CALL SYSTEM ( 'mv '//FNAME(1:I_LEN(FNAME))//'_TMP '// &
     &                               FNAME(1:I_LEN(FNAME))//' '//CHAR(0) ) 
                FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'PARF'//PRE_LETRS
                CALL SYSTEM ( 'mv '//FNAME(1:I_LEN(FNAME))//'_TMP '// &
     &                               FNAME(1:I_LEN(FNAME))//' '//CHAR(0) ) 
!
                CNSTROBJ%CNS_TYP = CNI_SAVE
                CNSTROBJ%OBJ_NAM = OBJ_SAVE
                IF ( FAST_DBG .EQ. F__PRI ) THEN
                     write ( 6, * ) ' norml_main: global_user: n_equat=', &
     &                                cnstrobj%n_equat
                ENDIF
              ELSE IF ( KUSER_CONST .AND. FL_UGL_OBSOLETE ) THEN
!
! ------------- Call user constraint program which would generate constraint
! ------------- file in obsolete, pre SEP-2002 format and apply these constraints
!
                CALL ERR_PASS ( IUER, IER )
                CALL DO_USER_CONSTRAINT ( TRUE_L4, LPARMA, NPARMA, CNSTROBJ, &
     &                                    ARR(JA), ARR(JB), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8418, IUER, 'NORML_MAIN', 'Errors in '// &
     &                   'imposing user constraints' )
                     RETURN
                ENDIF
           ENDIF
!
           CALL USE_GLBFIL_3 ( 'ORC' )
!
! -------- Fill the array 'WHO_STA' with station names for further processing
! -------- (the same as ISITN_CHR )
!
           IF ( NUMSTA.GT.MAX_STA ) THEN
                CALL FERR ( INT2(16), 'TOO MANY STATIONS IN NORML', INT2(0), &
     &                      INT2(0) )
           ENDIF
!
           DO I=1,NUMSTA
              DO J=1,4
                 ITEMP(J)=ISITN(J,I)
              ENDDO
              WHO_STA(I)=TEMP
           ENDDO
!
! -------- Apply weak constraint to source positions and source proper motions
!
           IF ( KSRC_CONST ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL DO_SRC ( F__NONE, FAST_DBG, TRUE_L4, TRUE_L4, IWDS, NPARMA, &
     &               IPARMA, NPARMA, IPARMA, SRC_COO_SIGMA, SRC_PRP_SIGMA, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8419, IUER, 'NORML_MAIN', 'Errors in '// &
     &                  'imposing constraints on source position' )
                     RETURN
                END IF
           ENDIF
!
! ------ 1. Velocity direction
!
         ISTAO=0
         IF ( .NOT. EQUAL ( '        ', INT2(1), DATSTA, INT2(1), INT2(8) )) &
     &        THEN
!
! ----------- Find the station and change the status of parameters
!
              CALL FIND_DIR  ( KVEL, ISTAO, ISUPVL )
!
! ----------- Get modified list of parameters
!
              CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                         TRUE__L2 )
!
! ----------- Find the correspondence between modified list and unmodified list
!
              CALL CXEPAR2 ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
!
! ----------- Apply constraint
!
              CALL ERR_PASS ( IUER, IER )
              CALL DO_DIR  ( KVEL, IXATS, NPARMA, ISTAO, ISUPVL, &
     &             VEL_DIR_SIGMA, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8420, IUER, 'NORML_MAIN', 'Errors in '// &
     &                 'imposing constraints on velocity direction' )
                   RETURN
              END IF
         ENDIF
         IF ( KBIT ( IPRES_KCOV, INT2(7) ) ) THEN
!
! ----------- Setting constraints on station positions components
!
              CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                         TRUE__L2 )
              CALL ERR_PASS   ( IUER, IER )
              CALL DO_STAVELC ( 'COMPONENT', NPARMS, FL_NN_LISTING, LPARMS, &
     &             STAXYZ_CNST, STAXYZ_CNSB, STAXYZ_CNFL, STAUEN_CNST, &
     &             STAUEN_CNSB, STAUEN_CNFL, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8421, IUER, 'NORML_MAIN', 'Errors in '// &
     &                 'imposing constraints on station coordinate components' )
                   RETURN
              END IF
         END IF
!
         IF ( KBIT ( IPRES_KCOV, INT2(8) ) ) THEN
!
! ----------- Setting constraints on velocity components
!
              CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                         TRUE__L2 )
              CALL ERR_PASS   ( IUER, IER )
              CALL DO_STAVELC ( 'VELOCITY ', NPARMS, FL_NN_LISTING, LPARMS, &
     &             VELXYZ_CNST, VELXYZ_CNSB, VELXYZ_CNFL, VELUEN_CNST, &
     &             VELUEN_CNSB, VELUEN_CNFL, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8422, IUER, 'NORML_MAIN', 'Errors in '// &
     &                 'imposing constraints on station velocity components' )
                   RETURN
              END IF
         END IF
!
         IF ( ISTASP .NE.0 .OR. KBIT( DEFCMP, INT2(4) ) ) THEN
              ICNS_STA_ORG = 0
!
! --------- 2. Station origin (very tie constraint on sum of coordinates of
! --------- selected stations)
!
            DO I=1,3
               CALL FIND_STAT ( KSTAT, I )
!
! ------------ Get modified list of parameters
!
               CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                         TRUE__L2 )
!
! ------------ Calculation IXATS -- vector of correspondence between LPARMA and
! ------------ LPARMS
!
               CALL CXEPAR2 ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
               CALL DO_STAT ( ICNS_STA_ORG, KSTAT, IXATS, NPARMA, I, &
     &              STA_ORG_SIGMA, CNSTROBJ, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8423, IUER, 'NORML_MAIN', 'Errors '// &
     &                  'in imposing constraints on station origin '// &
     &                  '(sum of adjustments of position of some stations)' )
                   RETURN
              END IF
            ENDDO
         ENDIF
!
! ------ No net translation for positions
!
         IF ( ISTASP .NE. 0 .OR. KBIT( DEFCMP, INT2(5) ) ) THEN
              CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                         TRUE__L2 )
              CALL ERR_PASS  ( IUER, IER )
              CALL NNT_POS   ( F__NONE, FAST_DBG, FL_NN_LISTING, TRUE_L4, &
     &             LPARMS, NPARMS, ARR(JA), NNT_POS_RTP, NNT_POS_SIGMA, B3DOBJ, &
     &             CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8424, IUER, 'NORML_MAIN', 'Error in '// &
     &                 'an attempt to impose global NNT_POS constraints' )
                   RETURN
              END IF
         ENDIF
!
! ------ No net rotation for positions
!
         IF ( ISTASP .NE. 0  .OR.  KBIT( DEFCMP, INT2(6) ) ) THEN
              CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                         TRUE__L2 )
              CALL ERR_PASS ( IUER, IER )
              CALL NNR_POS  ( F__NONE, FAST_DBG, FL_NN_LISTING, TRUE_L4, &
     &             LPARMS, NPARMS, ARR(JA), NNR_POS_RTP, NNR_POS_SIGMA, B3DOBJ, &
     &             CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8425, IUER, 'NORML_MAIN', 'Error in '// &
     &                 'an attempt to impose global NNR_POS constraints' )
                   RETURN
              END IF
         ENDIF
!
! ----- 2a. Velocity origin
!
        IF ( ( ISTASP .NE. 0 .OR. KBIT( DEFVEL, INT2(4) ) ) .AND. &
     &        VELOHORIZ .EQ. 0 ) THEN
!
! -------- Do simple XYZ constraint (no horizontal (rotation (EN)) or
! -------- vertical (translation (U))).
!
           DO I=1,3
              CALL FIND_VELO ( KVELO, I )
!
! ----------- Get modified list of parameters
!
              CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                         TRUE__L2 )
!
! ----------- Calculation IXATS -- vector of correspondence between LPARMA and
! ----------- LPARMS
!
              CALL CXEPAR2 ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
!
              CALL ERR_PASS ( IUER, IER )
              CALL DO_VELO  ( KVELO, IXATS, NPARMA, I, VEL_ORG_SIGMA, &
     &             CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8426, IUER, 'NORML_MAIN', 'Error in '// &
     &                 'an attempt to impose global VELO constraints' )
                   RETURN
              END IF
           ENDDO
        ENDIF
!
        IF ( ( ISTASP.NE.0 .OR. KBIT( DEFVEL, INT2(4) )) .AND. &
     &         VELOHORIZ .NE. 0 ) THEN
!
             CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                        TRUE__L2 )
!
             CALL ERR_PASS ( IUER, IER )
             CALL DO_VELOH ( NPARMS, IPARMS, VEL_SET_SIGMA, CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8427, IUER, 'NORML_MAIN', 'Error in '// &
     &                'an attempt to impose global VELOH constraints' )
                  RETURN
             END IF
        ENDIF
!
! ----- No net translation for velocities.
!
        IF ( ISTASP.NE.0 .OR. KBIT( DEFVEL, INT2(5) ) ) THEN
             CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                        TRUE__L2 )
             CALL ERR_PASS  ( IUER, IER )
             CALL NNT_VEL   ( FAST_DBG, FL_NN_LISTING, ARR(JA), LPARMS, &
     &            NPARMS, NNT_VEL_RTP, NNT_VEL_SIGMA, CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8428, IUER, 'NORML_MAIN', 'Error in '// &
     &                'an attempt to impose global NNT_VEL constraints' )
                  RETURN
             END IF
        ENDIF
!
! ----- No net rotation for velocities.
!
        If ( ISTASP.NE.0 .OR. KBIT( DEFVEL, INT2(6) ) ) THEN
             CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                        TRUE__L2 )
             CALL ERR_PASS  ( IUER, IER )
             CALL NNR_VEL   ( FAST_DBG, FL_NN_LISTING, ARR(JA), LPARMS, &
     &            NPARMS, NNR_VEL_RTP, NNR_VEL_SIGMA, CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8429, IUER, 'NORML_MAIN', 'Error in '// &
     &                'an attempt to impose global NNR_VEL constraints' )
                  RETURN
             END IF
        ENDIF
!
! ----- Constraint on non-linear site position motion
!
        IF ( FL_HPESOL ) THEN
             CALL ERR_PASS ( IUER, IER )
             CALL HPE_NN_CNST ( 'HPE_NNT', FL_NN_LISTING, NPARMA, &
     &                           LPARMA, %VAL(ADR_HPE), CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8430, IUER, 'NORML_MAIN', 'Error in '// &
     &                'an attempt to impose HPE_NNT constraints on '// &
     &                'harmonic site position variations' )
                  RETURN
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL HPE_NN_CNST ( 'HPE_NNR', FL_NN_LISTING, NPARMA, &
     &                           LPARMA, %VAL(ADR_HPE), CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8431, IUER, 'NORML_MAIN', 'Error in '// &
     &                'an attempt to impose HPE_NNR constraints on '// &
     &                'harmonic site position variations' )
                  RETURN
             END IF
        END IF
!
! ----- Decorrelation constraitns on spline coefficients modeling non-linear
! ----- station motion
!
        IF ( FL_SPESOL ) THEN
             CALL ERR_PASS ( IUER, IER )
             CALL SPESTA_CNST ( 'SPE_POS', NPARMA, LPARMA, %VAL(ADR_SPE), &
     &                          CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8432, IUER, 'NORML_MAIN', 'Error in '// &
     &                'an attempt to impose SPE_POS constraints on '// &
     &                'spline parameterization of site position variations' )
                  RETURN
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL SPESTA_CNST ( 'SPE_VEL', NPARMA, LPARMA, %VAL(ADR_SPE), &
     &                          CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8433, IUER, 'NORML_MAIN', 'Error in '// &
     &                'an attempt to impose SPE_VEL constraints on '// &
     &                'spline parameterization of site position variations' )
                  RETURN
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL SPE_CNST ( 'SPE_CNS', NPARMA, LPARMA, %VAL(ADR_SPE), &
     &                       B3DOBJ, CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8434, IUER, 'NORML_MAIN', 'Error in '// &
     &                'an attempt to impose SPE_CNS constraints on '// &
     &                'spline parameterization of site position variations' )
                  RETURN
             END IF
       END IF
! 
       IF ( FL_EHEO .AND. L_EHEC > 0 ) THEN
            CALL ERR_PASS ( IUER, IER )
            CALL EHEO_CLS_CNST ( NPARMS, LPARMS, L_EHEO, %VAL(ADR_EHEO), &
     &                           L_EHEC, %VAL(ADR_EHEC), CNSTROBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8435, IUER, 'NORML_MAIN', 'Error in '// &
     &               'an attempt to impose constraints between '// &
     &               'harmonic constituents of the Earth Rotation Model '// &
     &               'with close frequencies' )
                 RETURN
            END IF
       END IF   
!
       IF ( FL_EHEO .AND. FL_EERM ) THEN
            CALL ERR_PASS ( IUER, IER )
            CALL EHEO_ERM_CNST ( FAST_MODE, FAST_DBG, NPARMS, LPARMS, &
     &                           %VAL(ADR_EERM), L_EHEO, %VAL(ADR_EHEO), &
     &                           MJD_EHEO_REF, TAI_EHEO_REF, %VAL(ADR_EHES), &
     &                           CNSTROBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8436, IUER, 'NORML_MAIN', 'Error in '// &
     &               'an attempt to impose constraints on spline '// &
     &               'parameterization of the Earth Rotation Model '// &
     &               'in order to decorrelate them with harmonic '// &
     &               'variations in the Earth orientation' )
                 RETURN
            END IF
       END IF
!
! ---- Constraints on Empirical Earth rotation model and its derivatives
!
       IF ( FL_EERM ) THEN
            CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                       TRUE__L2 )
            CALL ERR_PASS ( IUER, IER )
            CALL ERM_CNST ( NPARMS, LPARMS, %VAL(ADR_EERM), CNSTROBJ, &
     &                      IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8437, IUER, 'NORML_MAIN', 'Error in '// &
     &               'an attempt to impose ERM_CNST constraints on '// &
     &               'spline parameterization of the Earth Rotation '// &
     &               'Model' )
                 RETURN
            END IF
       END IF
!
! ----- 2a. Velocity ties
!
       IF ( NUMVELGRP .GT. 0 ) THEN
            ICNS_VEL_TIE = 0
            DO J=1,NUMVELGRP
               DO I=1,3
                  CALL FIND_VELT ( KVELO, I, J, MAX4_SIT, L_STA, C_STA )
                  CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, &
     &                              TRUE__L2, TRUE__L2 )
!
! --------------- Calculation IXATS -- vector of correspondence between LPARMA
! --------------- and LPARMS
!
                  CALL CXEPAR2 ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
                  IF ( L_STA > 1 ) THEN
!
! -------------------- Now build equations of constraints
!
                       CALL ERR_PASS ( IUER,  IER )
                       CALL DO_VELT  ( ICNS_VEL_TIE, KVELO, IXATS, &
     &                                 NPARMA, I, VEL_TIE_SIGMA, CNSTROBJ, IER )
                       IF ( IER .NE. 0 ) THEN
                            CALL ERR_LOG ( 8438, IUER, 'NORML_MAIN', 'Error '// &
     &                          'in an attempt to impose global velocity ties '// &
     &                          'constraints' )
                            RETURN
                       END IF
                    ELSE
!
! -------------------- Fix LSITEV. The true is that, in order to apply velocity ties constraints,
! -------------------- the program FIND_VELT modified LSITEV. Now we have to play back and undo
! -------------------- these modifications.
!
                       DO K=1,NUMSTA
                          CALL KSBIT ( LSITEV(1,I), INT2(K), KBIT(KVELO,INT2(K)) )
                       ENDDO
                  END IF
               ENDDO
!
               IF ( L_STA > 0 ) THEN
                    CALL CLRCH ( OUT )
                    DO I=1,L_STA
                       OUT = TRIM(OUT)//' '//C_STA(I)
                    END DO
!                     WRITE ( 23, '(A,I4,A)' ) 'VEL_TIE: velocities of these stations '// &
!     &                                        'are constrained: '//TRIM(OUT)
               END IF
            ENDDO ! J
       ENDIF
!
! ---- 2b. Station ties
!
       IF ( NUMSTAGRP .GT. 0 ) THEN
            ICNS_STA_TIE = 0
            DO J=1,NUMSTAGRP
               DO I=1,3
                  CALL FIND_STATT ( KSTATT, I, J, MAX4_SIT, L_STA, C_STA  )
                  CALL GET_NAMES  ( LPARMS, ISTR_LEN, M_GPA, NPARMS, &
     &                              TRUE__L2, TRUE__L2 )
!
! --------------- Calculation IXATS -- vector of corresponence between LPARMA
! --------------- and LPARMS
!
                  CALL CXEPAR2 ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
!
                  CALL ERR_PASS ( IUER,  IER )
                  CALL DO_STATT ( ICNS_STA_TIE, KSTATT, IXATS, NPARMA, I, &
     &                            STA_TIE_SIGMA, CNSTROBJ, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 8439, IUER, 'NORML_MAIN', 'Error '// &
     &                     'in an attempt to impose global station position '// &
     &                     'ties constraints' )
                       RETURN
                  END IF
               ENDDO
!
                IF ( L_STA > 0 ) THEN
                     DO I=1,L_STA
                        OUT = TRIM(OUT)//' '//C_STA(I)
                     END DO
!
                     WRITE ( 23, '(A,I4,A)' ) 'STA_TIE: positions of these stations '// &
     &                                        'are constrained: '//TRIM(OUT)
                END IF
            ENDDO
       ENDIF
!
! ---- 3. Right ascension origin
!
       IF ( ISRCSP .NE. 0 .OR. KBIT( DEFSRC, INT2(3) ) ) THEN
            CALL FIND_RA   ( KSRCC, IRA )
            CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                       TRUE__L2 )
!
! --------- Calculation IXATS -- vector of corresponence between LPARMA
! --------- and LPARMS
!
            CALL CXEPAR2 ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
!
            CALL ERR_PASS ( IUER, IER )
            CALL DO_RA ( KSRCC, IRA, IXATS, NPARMA, RAS_ORG_SIGMA, CNSTROBJ, &
     &           IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8440, IUER, 'NORML_MAIN', 'Error '// &
     &               'in an attempt to impose global constraints on '// &
     &               'source right ascension ' )
                 RETURN
            END IF
       ENDIF
!
! ---- 3a. Declination origin
!
       IF ( ISRCSP .NE. 0  .OR.  KBIT( DEFSRC, INT2(6) ) ) THEN
            CALL FIND_DC   ( KSRCC, IRA )
            CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                       TRUE__L2 )
!
! --------- Calculation IXATS -- vector of corresponence between LPARMA
! --------- and LPARMS
!
            CALL CXEPAR2  ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
            CALL ERR_PASS ( IUER, IER )
            CALL DO_DC    ( KSRCC, IXATS, NPARMA, DCL_ORG_SIGMA, CNSTROBJ, &
     &                      IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8441, IUER, 'NORML_MAIN', 'Error '// &
     &               'in an attempt to impose global constraints on '// &
     &               'source declination ' )
                 RETURN
            END IF
       ENDIF
!
! ---- No net rotation for source positions
!
       IF ( ISRCSP.NE.0 .OR. KBIT( DEFSRC, INT2(7) ) ) THEN
            CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                       TRUE__L2 )
            CALL ERR_PASS ( IUER, IER )
            CALL NNR_SRC  ( F__NONE, FAST_DBG, FL_NN_LISTING, TRUE_L4, &
     &           LPARMS, NPARMS, ARR(JA), NNR_SRC_SIGMA, NNR_SRC_RTP, B3DOBJ, &
     &           CNSTROBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8442, IUER, 'NORML_MAIN', 'Error in '// &
     &               'an attempt to impose global NNR_SRC constraints' )
                 RETURN
            END IF
       ENDIF
!
! ---- No net rotation for source proper motion
!
       IF ( ISRCSP .NE. 0  .OR.  KBIT( DEFSRC, INT2(8) ) ) THEN
            CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                       TRUE__L2 )
            CALL ERR_PASS ( IUER, IER )
            CALL NNR_PRP  ( F__NONE, FAST_DBG, FL_NN_LISTING, LPARMS, NPARMS, &
     &           ARR(JA), NNR_PRP_SIGMA, NNR_SRC_RTP, B3DOBJ, CNSTROBJ, &
     &           IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8443, IUER, 'NORML_MAIN', 'Error in '// &
     &               'an attempt to impose global NNR_PRP constraints' )
                 RETURN
            END IF
       ENDIF
!
! ---- 3b. Continuous station positions constraints
!
       IF ( PWCNUM(1) .GT. 0 ) THEN
            CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, &
     &                       TRUE__L2 )
            CALL ERR_PASS ( IUER, IER )
            CALL DO_PWC   ( WHO_STA, IPARMS, NPARMS, IWDS, CNSTROBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8444, IUER, 'NORML_MAIN', &
     &               'Error in an attempt to impose global station '// &
     &               'velocity constraints between segments' )
                 RETURN
            END IF
       ENDIF
!
! ---- 5. Velocity UEN suppression with XYZ comp
!
       IF ( .NOT. KBIT( IUEN, INT2(1) ) ) THEN
            ICNS_VEL_SUP = 0
            DO I=1,NUMSTA
               CALL FIND_VEL ( KVEL, I , ISUPVL )
               IF ( ISUPVL.NE.0 .AND. (KVEL(1) .OR. KVEL(2) .OR. KVEL(3)) ) THEN
                    CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, &
     &                               TRUE__L2, TRUE__L2 )
!
! ----------------- Calculation IXATS -- vector of correspondence between LPARMA
! ----------------- and LPARMS
!
                    CALL CXEPAR2 ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL DO_VEL   ( ICNS_VEL_SUP, KVEL, IXATS, NPARMA, I, &
     &                   ISUPVL, VEL_VER_SIGMA, CNSTROBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8445, IUER, 'NORML_MAIN', &
     &                       'Error in an attempt to impose global station '// &
     &                       'velocity suppression constraints' )
                         RETURN
                    END IF
               ENDIF
            ENDDO
       ENDIF
!
! ----- Grab dynamic memory for weight matrix of constraints
!
       IF ( CNSTROBJ%N_OFD > 0  .AND.  .NOT. FL_NOFD_IGNORE ) THEN
            LEN_WEI_CNS = 8*(CNSTROBJ%N_EQUAT*(CNSTROBJ%N_EQUAT+1))/2
            CALL ERR_PASS ( IUER, IER )
            CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, &
     &                           LEN_WEI_CNS, ADR_WEI_CNS )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH  ( STR  ) 
                 CALL CLRCH  ( STR1 )
                 CALL IINCH8 ( INT8(8)*INT8(CNSTROBJ%N_EQUAT)*INT8(CNSTROBJ%N_EQUAT+1)/INT8(2), STR )
                 CALL INCH  ( CNSTROBJ%N_EQUAT, STR1 )
                 CALL ERR_LOG ( 8447, IUER, 'NORML_MAIN', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &               'memory for weight matrix of constraints needed for '// &
     &                STR1(1:I_LEN(STR1))//' constraint equations' )
                RETURN
            END IF
            FL_FULL_WEI = .TRUE.
          ELSE 
            FL_FULL_WEI = .FALSE.
       END IF
!
        IF ( SOU_ADM_FLAG .NE. SOUADM__NO  .AND.  SOU_ADM_CNS > 0.0D0 ) THEN
!
! ---------- Impose constrinat on source structure admittance
! 
             CALL ERR_PASS   ( IUER, IER ) 
             CALL SOUADM_CNS ( NPARMS, LPARMS, TRUE__L4, CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8448, IUER, 'NORML_MAIL', 'Error in '// &
     &                'an attempt to apply source structure admittance '// &
     &                'constraints' )
                  RETURN
             END IF
        END IF
!
! ----- Applying constraints
!
        IF ( FL_DEBUG_CNS_MAT ) THEN
!
! ---------- Zerroging the CGM in the constraint debugging mode
!
!!             ARR(JA:JA+INT8(NPARIN)*INT8(NPARIN+1)/INT8(2)) = 0.0D0
        END IF
        CALL ERR_PASS    ( IUER, IER )
        CALL APPLY_CNSTR ( F__NONE, FAST_DBG, CNSTROBJ, NPARIN, ARR(JA), &
     &                     ARR(JB), B3DOBJ, %VAL(0), FL_FULL_WEI, %VAL(ADR_WEI_CNS), &
     &                     IER )
        IF ( KCOV ) THEN
             WRITE ( 23, '(A)' ) 'End of global constraint section'
        END IF
        IF ( FL_DEBUG_CNS_MAT ) THEN
             CALL MATVIEW_2 ( NPARIN, ARR(JA) )
             CALL EXIT ( 0 )
        END IF
        IF ( FL_FULL_WEI ) THEN
             CALL FREE_MEM ( MEM_ADR ) ! Free dynamic memory
        END IF
        IF ( IER .NE. 0 ) THEN
             CALL ERR_LOG ( 8449, IUER, 'NORML_MAIL', 'Error in '// &
     &           'an attempt to apply constraints' )
             RETURN
        END IF
!
! ----- Writing them on disk
!
        CALL ERR_PASS    ( IUER, IER )
        CALL WRITE_CNSTR ( CNSTROBJ, CNI__GLO, IER )
        IF ( IER .NE. 0 ) THEN
             CALL ERR_LOG ( 8450, IUER, 'NORML_MAIN', 'Error in an '// &
     &           'attempt to write constraint equations in a sctrach file' )
             RETURN
        END IF
!
! ----- 6. Simple supression
!
        CALL FIND_SUPRS()
        CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, TRUE__L2 )
!
! ----- Calculation IXATS -- vector of corresponence between LPARMA
! ----- and LPARMS
!
        CALL CXEPAR2  ( LPARMA, IXATS, NPARMA, LPARMS, NPARMS )
!
! ----- Save number of suppressed global parameters for later (HAUSR)
!
        GLBSUP = 0
        DO I=1,NPARIN
           IF ( IXATS(I) .EQ. 0 ) GLBSUP = GLBSUP + 1
        ENDDO
        IF ( GLBSUP .GT. 0  .AND.  FL_SINEX_MAKE ) THEN
             CALL ERR_LOG  ( 8446, IUER, 'NORML_MAIN', 'SUPPRESSION '// &
     &           'consrtaints cannot be used togeather with a request '// &
     &           'to generate a listing of the solution in SINEX format' )
             RETURN
        END IF
        CALL DO_SUPRS ( FAST_DBG, ARR(JA), ARR(JB), IXATS, NPARIN, LPARMA )
!
        IF ( ILEN(SUPPRESS_FILE) > 0    .AND. &
     &       SUPPRESS_FILE .NE. 'NONE'        )  THEN
!
             CALL ERR_PASS ( IUER, IER )
             CALL DO_SUPR_FILE ( NPARMS, LPARMS, SUPPRESS_FILE, &
     &                           ARR(JA), ARR(JB), IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG  ( 8447, IUER, 'NORML_MAIN', 'SUPPRESSION '// &
     &                'consrtaints cannot be used togeather with a request '// &
     &                'to generate a listing of the solution in SINEX format' )
                  RETURN
             END IF
        END IF
!
! ----- Save suppressed CGM if it is a suppression run
!
        IF ( SOLTYP .EQ. 'S' ) THEN
             CALL USE_CGMF_MAT ( ARR, NPARIN, 'W' )
             CALL ACS_CGMFIL   ( CNAME, 'C' )
        ENDIF
      ENDIF  !!  if ( kcov )
!
! === Inversion and solution
!     ~~~~~~~~~~~~~~~~~~~~~~
!
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'NORML-02' )
           CALL TIM_INIT()
      END IF
!
      IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD    .OR. &
     &     FAST_MODE .EQ. F__B1D  .OR. FAST_MODE .EQ. F__B1B3D )    THEN
!
! -------- Variant with full matrix
!
           IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! ------------- DEBUG-mode: writing down normal matrix and normal vector
!
                call matview_w ( '/tmp/nor_full.mat', 3, nparam, &
     &               nparam, arr(ja),'Full normal matrix', '()', 1, 1, IER )
                call matview_w ( '/tmp/nor_full.vec', 1, nparam, 1, &
     &               arr(jb), 'Full normal vector', '()', 1, 1, IER )
           END IF
!
           IF ( NPARIN .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( NPARIN, STR ) 
                CALL ERR_LOG ( 8451, IUER, 'NORML_MAIN', 'Wrong dimension '// &
     &              'of CGM: '//STR )
                RETURN 
           END IF
!
! -------- Scaling normal system. Scaling vectors will be calculated and put
! -------- in arr(1). Normal system will be modified so that main diagonal will
! -------- contain only 1
!
           CALL SCALER ( ARR(JA), ARR(JB), ARR(1), NPARIN )
!
! -------- Invert normal matrix
!
#ifdef DEBUG
           IF ( KBATCH ) THEN
                WRITE ( 6, * ) 'NORML_MAIL 1522: inverting matrix of dimension ', NPARIN
           END IF
#endif
           CALL ERR_PASS ( IUER, IER )
           CALL INVS ( NPARIN, ARR(JA), RCOND, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                WRITE ( UNIT=STR(1:15), FMT='(1PG15.7)' ) RCOND
                CALL ERR_LOG ( 8452, IUER, 'NORML_MAIN', 'Failure to '// &
     &              'invert normal matrix: condition number: '//STR )
                RETURN 
           END IF
#ifdef DEBUG
           IF ( KBATCH ) THEN
                WRITE ( 6, * ) 'NORML_MAIL 1532: inverted  matrix of dimension ', NPARIN
           END IF
#endif
!
! -------- Compute the vector of the estimates (Z)
!
           CALL MUL_MV_SV_V ( NPARIN, ARR(JA), NPARIN, ARR(JB), &
     &                        NPARIN, Z, IER )
!
! -------- ... and copy it to ARR(JB)  thus, replacing normal vector
!
           CALL COPY_V ( NPARIN, Z, ARR(JB) )
!
! -------- Unscaling covariance matrix and vector estimates
!
           CALL UNSCALER ( ARR(JA), ARR(JB), ARR(1), NPARIN )
!
! -------- Writing down condition number
!
           RCOND_CGM = RCOND
           CALL USE_GLBFIL_4 ( 'OWC' )
!
           IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! ------------- DEBUG-mode: writing down covariance matrix and vectors of the
! ------------- estimatrs
!
                ier = -1
                call matview_w ( '/tmp/cov_full.mat', 3, nparam, &
     &               nparam, arr(ja), 'Full covariance matrix', '()', &
     &               1, 1, IER )
                if ( ier .ne. 0 ) then
                     call err_log ( 8453, iuer, 'norml_main', &
     &                             'failure in matview_w' ) 
                     return 
                end if
                ier = -1
                call matview_w ( '/tmp/cov_full.vec', 1, nparam, 1, &
     &               arr(jb), 'Full vector '//'of estimates', '()', 1, 1, IER )
                WRITE ( 6, 215 )  rcond
 215            FORMAT ( 1x,' NORML:  system has been solved.  rcond = ', &
     &                        1pe12.4 )
                if ( ier .ne. 0 ) then
                     call err_log ( 8454, iuer, 'norml_main', &
     &                             'failure in matview_w' ) 
                     return 
                end if
           END IF
!
! -------- Calculate sigmas, store in ARR(JS) vector(surprise, surprise . . .)
!
           CALL SIGMAS ( ARR(JA), ARR(JS), NPARIN )
!
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'NORML-03' )
                CALL TIM_INIT()
           END IF
        ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! -------- Variant with B3D
!
! -------- Real updating normal matrix for imposing constraints. Since matrix
! -------- ARR(JA) was zeroed before imposing constraints, now it contains
! -------- correction to the normal matrix due constraints. But the order
! -------- rows/columns in ARR is the order for full-variant SOLVE.
! -------- MAPCNSTR will rearrange rows and columns (virtually, of course)
! -------- and update normal matrix hidden in B3DOBJ
!
           CALL MAPCNST_B3D ( NPARIN, ARR(JA), DAPR, B3DOBJ )
!
! -------- Solving system of normal equations using B3D algorithm
!
           F_FD = .TRUE. ! Setting flag to make unscaling of the estiamtes and
!                        ! to calculate parameters covaraince
           CALL ERR_PASS  ( IUER, IER )
           CALL NORML_B3D ( FAST_COV, F_FD, FAST_DBG, B3DOBJ, RCOND, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8455, IUER, 'NORML_MAIN', &
     &              'Failure during solving normal system using B3D '// &
     &              'algorithm while database '//B3DOBJ%DBNAME_MES// &
     &              ' was processing' )
                RETURN
           END IF
           RCOND_CGM = RCOND
!
! -------- Writing down condition number
!
           CALL USE_GLBFIL_4 ( 'OWC' )
!
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'NORML-03' )
                CALL TIM_INIT()
           END IF
!
! -------- Expanding squeezed submatrices of B3D object to full matrix
!
           CALL NOUT8_R8 ( (INT8(NPARIN)*INT8(NPARIN+1))/2, ARR(JA) )
           CALL NOUT_R8  (       NPARIN,                    ARR(JB) )
           CALL NOUT_R8  (       NPARIN,                    ARR(JS) )
           CALL NOUT_R8  (       NPARIN,                    ARR(1 ) )
!
! -------- Putting expanded and rearranged covariance matrix to ARR(JA),
! -------- vector of estimates in ARR(JB), vector of variances of the estimates
! -------- in ARR(JS) and scaling vector in ARR(1) (Don't ask me why I move
! -------- scaling vector -- who knows, maybe it is used somewhere ;-)     )
! -------- Of course, ARR(JA) will have elements of covariance matrix only for
! -------- global paramaters, global-local,  main block diagonal and down
! -------- block-diagonal. Others elements will be zeroes.
!
           CALL EXPAND_B3D ( B3DOBJ, ARR(JA), ARR(JB), ARR(JS), ARR(1) )
           IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! ------------- DEBUG-mode: Writing object B3DOBJ on disk
!
                call clrch ( finam_cov )
                finam_nrm = '/tmp/cov_b3d.bin'
                call err_pass  ( iuer, ier )
                call wrnor_b3d ( finam_nrm, b3dobj, ier )
           END IF
      END IF
!
! --- Write matricies
!
      IF ( KCOV ) THEN
!
! -------- Case of global solution
!
           CALL USE_COVF_MAT ( ARR, NPARIN, 'W' )
           CALL ACS_COVFIL   ( 'C' )
           TGLBLS=NPARIN
           NUM_USER_PART = OLD_USER_PART
!
           IF ( KHFEOP.EQ.2.OR.KHFEOP.EQ.3) THEN
               NDX = NPARIN-2*(NUM_SDE_UT1+NUM_SDE_XY)
               DO I=1,NUM_SDE_UT1+NUM_SDE_XY
                   DO J=1,2
                      NDX = NDX+1
                      SDE_VAL(J,I) = ARR(JB+NDX-1)
                   ENDDO
               ENDDO
!
! ------------ Convert UT1 values (for high frequency EOP !) from usec to mas
!
               DO I=1,NUM_SDE_UT1
                  DO J=1,2
                     SDE_VAL(J,I) = SDE_VAL(J,I)*15.D0/1000.D0
                  ENDDO
               ENDDO
!
               CALL USE_GLBFIL_4 ( 'OWC' )
           ENDIF
           CALL USE_GLBFIL ( 'OWC' )
!
! -------- Create volatile objects for non-linear site position estimates
!
           IF ( L_HPE > 0 .AND. ADR_HPE .NE. 0 ) THEN
                CALL HPESOL_CREATE ( %VAL(ADR_HPE) )
             ELSE
                FL_HPESOL = .FALSE.
           END IF
           IF ( L_SPE > 0 .AND. ADR_SPE .NE. 0 ) THEN
                CALL SPESOL_CREATE ( %VAL(ADR_SPE) )
              ELSE
                FL_SPESOL = .FALSE.
           END IF
        ELSE ! not kcov
!
! ----- Write covariance matrice in the case of independent solution
!
        CALL USE_COMMON ( 'OWC' )
        IF ( F_IO_NRM ) THEN
             IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD    .OR. &
     &            FAST_MODE .EQ. F__B1D   )   THEN
!
! --------------- Write down ARR in NRMF file for CRES and ADJUST
!
                  CALL USE_NRMFIL ( ARR, NPARIN, 'WC' )
                ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
                  CALL USE_NRMFIL ( ARR, NPARIN, 'OWC' )
             END IF
        END IF
!
        IF ( TRAIN  .AND.  FAST_MODE .EQ. F__B3D  ) THEN
!
! ---------- Rewrite some fields of B3DOBJ in fast-file. It is done for CRES
! ---------- since B3DOBJ contains corrected theoreticals and they are
! ---------- necessary for calculations of residuals. It is done only in
! ---------- B3D - mode, since in B1B3D mode this information contains in
! ---------- NRMFxx file from ARCPE since BACK.
!
             CALL ERR_PASS ( IUER, IER )
             CALL WRNOR_B3D ( FINAM_FAST, B3DOBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8456, IUER, 'NORML_MAIN', &
     &                'Error during writing file '// &
     &                 FINAM_FAST(1:I_LEN(FINAM_FAST))//' while session '// &
     &                 B3DOBJ%DBNAME_MES//' was processing' )
                  RETURN
             END IF
        END IF
      END IF
!
      IF ( ISLTY2 .EQ. 'I'  .AND.  COR_LL_FLAG ) THEN
!
! -------- Computation of LOC_LOC correaltion between local parameters
! -------- in independent run.
!
           CALL ERR_PASS ( IUER, IER )
           IF ( FAST_MODE .EQ. F__B3D ) THEN
                CALL WRITE_DIACORR ( 'LOC_LOC', B3DOBJ, COR_LL_INCFIL, &
     &                 COR_LL_EXCFIL, NPARIN, %VAL(B3DOBJ%AD_B0), &
     &                 LPARMA, IER )
               ELSE IF ( FAST_MODE .EQ. F__NONE ) THEN
                  CALL WRITE_DIACORR ( 'LOC_LOC', B3DOBJ, COR_LL_INCFIL, &
     &                 COR_LL_EXCFIL, NPARIN, ARR(JA), LPARMA, IER )
             END IF
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8457, IUER, 'NORML_MAIN', &
     &                'Error in attempt to write LOC_LOC correlations' )
                  RETURN
             END IF
      END IF
!
      IF ( ISLTY2 .EQ. 'B'  .AND.  COR_GG_FLAG ) THEN
!
! ---------- Computation of GLO_GLO correlations between global paramters
! ---------- in global run.
!
             CALL ERR_PASS ( IUER, IER )
             CALL WRITE_DIACORR ( 'GLO_GLO', B3DOBJ, COR_GG_INCFIL, &
     &            COR_GG_EXCFIL, NPARIN, ARR(JA), LPARMA, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8458, IUER, 'NORML_MAIN', &
     &                'Error in an attempt to write GLO_GLO correlations' )
                  RETURN
             END IF
      END IF
!
! --- End of work at last!
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'NORML-04' )
      END IF
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, 260 )  nparin
  260      format ( 1x,' NORML:  finished.  nparin=',i5 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  NORML_MAIN  #!#
