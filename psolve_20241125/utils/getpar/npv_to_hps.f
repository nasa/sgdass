      SUBROUTINE NPV_TO_HPS ( L_NPV, C_NPV, SOL_ID, SOL_DATE, FILHPS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NPV_TO_HPS  parses the input text buffer with a file with *
! *   estimates of non-linear site position variations, extracts from    *
! *   there coefficients of expantion of non-lineat site position        *
! *   vartiations into the Fourier basis and writes then down in the     *
! *   output file in HARPOS format.                                      *
! *                                                                      *
! *  ### 29-OCT-2007   NPV_TO_HPS  v1.1 (c)  L. Petrov  01-JAN-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE      ( HPE__TYPE ) HPE(VTD__M_HPF)
      TYPE      ( HARPOS__H_RECORD ) :: H_REC
      TYPE      ( HARPOS__A_RECORD ) :: A_REC
      TYPE      ( HARPOS__S_RECORD ) :: S_REC
      TYPE      ( HARPOS__D_RECORD ) :: D_REC
      INTEGER*4  L_NPV, IUER
      CHARACTER  C_NPV(*)*(*), SOL_ID*(*), SOL_DATE*(*), FILHPS*(*)
      INTEGER*4  M_HLP
      PARAMETER  ( M_HLP = 16*1024 )
      CHARACTER  C_STA(VTD__M_HMD)*8, COO_STR(3,VTD__M_HMD)*14, &
     &           HAR_NAM(VTD__M_HPF)*8, STR*128, SOLVE_HELP_DIR_STR*128, HELP_FILE*128, &
     &           HELP_BUF(M_HLP)*128
      LOGICAL*4  FL_HPE 
      REAL*8     EST(3,VTD__M_HMD,VTD__M_HPF,2), COV(3,3,VTD__M_HMD,VTD__M_HPF,2)
      REAL*8     PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC, VAL, COO(3), &
     &           CF, SF, CL, SL, UEN(3), TRS_TO_UEN(3,3,VTD__M_HMD)
      INTEGER*4  L_STA, L_HPE, I_STA, I_CMP, I_AMP, LUN, J1, J2, J3, J4, &
     &           J5, J6, N_HLP, IND_EQU(3,VTD__M_HMD,VTD__M_HPF,2), IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ADD_CLIST, GET_UNIT, I_LEN, ILEN, LTM_DIF
!
! --- Open the output file with harmonic site position variations
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILHPS, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4571, IUER, 'NPV_TO_HPS', 'Error in opening '// &
     &         'output file with harmonic site position '// &
     &         'variations '//FILHPS )
           RETURN 
      END IF
!
! --- Parse the input buffer and gather information about variations
!
      L_STA = 0
      L_HPE = 0
      FL_HPE = .FALSE. 
      DO 410 J1=1,L_NPV
         IF ( C_NPV(J1)(1:5) == 'L_SPE' ) THEN
              FL_HPE = .FALSE.
              GOTO 410
         END IF
         IF ( C_NPV(J1)(1:8) == '   I_HPE' ) THEN
!
! ----------- Get phase and frequency
!
              FL_HPE = .TRUE.
              L_HPE = L_HPE + 1
              READ ( UNIT=C_NPV(J1)(23:32), FMT='(F10.5)'  ) HPE(L_HPE)%PHASE
              READ ( UNIT=C_NPV(J1)(46:64), FMT='(F19.12)' ) HPE(L_HPE)%FREQ
              GOTO 410
         END IF
         IF ( .NOT. FL_HPE ) GOTO 410
         IF ( C_NPV(J1)(1:8) == '   I_STA'  ) THEN
              CONTINUE 
           ELSE IF ( C_NPV(J1)(4:7) == 'APS:'  ) THEN
!
! ----------- Get apriori site coordiantes
!
              CALL VTD_NAME_REPAIR ( C_NPV(J1)(9:16) )
              CALL ERR_PASS ( IUER, IER ) 
              I_STA = ADD_CLIST ( M__SPE, L_STA, C_STA, C_NPV(J1)(9:16), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4573, IUER, 'NPV_TO_HPS', 'TRap of '// &
     &                 'internal control: too many stations. Please, '//&
     &                 'increase parameter M__SPE in solve.templ' )
                   RETURN 
              END IF
              I_CMP = INDEX ( 'XYZ', C_NPV(J1)(18:18) )
              COO_STR(I_CMP,I_STA) = C_NPV(J1)(24:37)
           ELSE IF ( C_NPV(J1)(4:7) == 'EST:'  ) THEN
!
! ----------- Get estimates of position variations
!
              CALL VTD_NAME_REPAIR ( C_NPV(J1)(9:16) )
              CALL ERR_PASS ( IUER, IER ) 
              I_STA = ADD_CLIST ( M__SPE, L_STA, C_STA, C_NPV(J1)(9:16), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4574, IUER, 'NPV_TO_HPS', 'TRap of '// &
     &                 'internal control: too many stations. Please, '//&
     &                 'increase parameter M__SPE in solve.templ' )
                   RETURN 
              END IF
              HAR_NAM(L_HPE) = C_NPV(J1)(21:28)
              I_CMP = INDEX ( 'XYZ', C_NPV(J1)(18:18) )
              I_AMP = INDEX ( 'CS',  C_NPV(J1)(19:19) )
              READ ( UNIT=C_NPV(J1)(78:92), FMT='(F15.5)' ) EST(I_CMP,I_STA,L_HPE,I_AMP)
         END IF
 410  CONTINUE 
!
      IF ( L_STA == 0 ) THEN
!
! -------- No station was found? Write an empty file and go home to dring hot tea
!
           WRITE ( LUN, FMT='(A)' ) HARPOS__LABEL 
           WRITE ( LUN, FMT='(A)' ) '# '
           WRITE ( LUN, FMT='(A)' ) '# No position variations available'
           WRITE ( LUN, FMT='(A)' ) '# '
           WRITE ( LUN, FMT='(A,I4)' ) 'L_STA: ', L_STA
           WRITE ( LUN, FMT='(A)' ) HARPOS__LABEL
           CLOSE ( UNIT=LUN )
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Write the header 
!
      WRITE ( LUN, FMT='(A)' ) HARPOS__LABEL
      WRITE ( LUN, FMT='(A)' ) '# '
      WRITE ( LUN, FMT='(A)' ) '# This file was generated on '//GET_CDATE()
      WRITE ( LUN, FMT='(A)' ) '# '
      WRITE ( LUN, FMT='(A)' ) '# Model of hamonic site positions variations'// &
     &                         ' from the VLBI solution'
      WRITE ( LUN, FMT='(A)' ) '# '
      WRITE ( LUN, FMT='(A)' ) '# SOL_ID:   '//SOL_ID(1:I_LEN(SOL_ID))
      WRITE ( LUN, FMT='(A)' ) '# SOL_DATE: '//SOL_DATE(1:I_LEN(SOL_DATE))
!
! --- Get directory with help files
!
      CALL GETENVAR ( 'PSOLVE_HELP_DIR', SOLVE_HELP_DIR_STR )
      IF ( ILEN(SOLVE_HELP_DIR_STR) .LE. 0 ) THEN
           SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR
      END IF
      IF ( SOLVE_HELP_DIR_STR(I_LEN(SOLVE_HELP_DIR_STR):I_LEN(SOLVE_HELP_DIR_STR)) .NE. '/' ) THEN
           SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR_STR(1:I_LEN(SOLVE_HELP_DIR_STR))//'/'
      END IF
!
      HELP_FILE = SOLVE_HELP_DIR_STR(1:I_LEN(SOLVE_HELP_DIR_STR))//HARPOS_FORMAT_HELP
      CALL RD_TEXT ( HELP_FILE, M_HLP, HELP_BUF, N_HLP, IER )
!
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#============================ Beginning of format description: ================='
      WRITE ( LUN, FMT='(A)' ) '#'
!
  write ( 6, * ) ' n_hlp = ', n_hlp ! %%%
      DO 420 J2=1,N_HLP
         WRITE ( LUN, FMT='(A)' ) '# '//HELP_BUF(J2)(1:I_LEN(HELP_BUF(J2)))
420  CONTINUE 
!
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#============================ End of format description: ======================='
      WRITE ( LUN, FMT='(A)' ) '#'
!
! --- Iniliatiaztin of output lines
!
      H_REC%REC_ID = 'H'
      CALL CLRCH ( H_REC%FILL_1 )
      CALL CLRCH ( H_REC%FILL_2 )
      CALL CLRCH ( H_REC%FILL_3 )
      CALL CLRCH ( H_REC%FILL_4 )
      CALL CLRCH ( H_REC%FILL_5 )
!
      A_REC%REC_ID = 'A'
      CALL CLRCH ( A_REC%FILL_1 )
      CALL CLRCH ( A_REC%FILL_2 )
!
      S_REC%REC_ID = 'S'
      CALL CLRCH ( S_REC%FILL_1 )
      CALL CLRCH ( S_REC%FILL_2 )
      CALL CLRCH ( S_REC%FILL_3 )
      CALL CLRCH ( S_REC%FILL_4 )
      CALL CLRCH ( S_REC%FILL_5 )
      CALL CLRCH ( S_REC%FILL_6 )
      CALL CLRCH ( S_REC%FILL_7 )
!
      D_REC%REC_ID = 'D'
      CALL CLRCH ( D_REC%FILL_1 )
      CALL CLRCH ( D_REC%FILL_2 )
      CALL CLRCH ( D_REC%FILL_3 )
      CALL CLRCH ( D_REC%FILL_4 )
      CALL CLRCH ( D_REC%FILL_5 )
      CALL CLRCH ( D_REC%FILL_6 )
      CALL CLRCH ( D_REC%FILL_7 )
      CALL CLRCH ( D_REC%FILL_8 )
      CALL CLRCH ( D_REC%FILL_9 )
!
! --- WRite thedelimited section
!
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#  Harmonic   Phase          Frequency           Acceleration'
      WRITE ( LUN, FMT='(A)' ) '#'
!
! --- Write ythe section with harmonic definitions
!
      DO 430 J3=1,L_HPE
         H_REC%WAVE_ID = HAR_NAM(J3) 
         WRITE ( UNIT=H_REC%PHASE, FMT='(1PD13.6)'  ) HPE(J3)%PHASE
         WRITE ( UNIT=H_REC%FREQ,  FMT='(1PD19.12)' ) HPE(J3)%FREQ
         WRITE ( UNIT=H_REC%ACCEL, FMT='(1PD10.3)'  ) 0.0D0
!
         CALL LIB$MOVC3 ( LEN__H_REC, H_REC, %REF(STR) )
         WRITE ( LUN, '(A)' ) STR(1:LEN__H_REC)
 430  CONTINUE 
!
! --- Write the section with the area definition
!
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#  Area of validty of harmonic variations estimates'
      WRITE ( LUN, FMT='(A)' ) '#'
!
      WRITE ( UNIT=A_REC%AREA_RD, FMT='(F14.6)' ) BSPPOS__RD_AREA
      CALL LIB$MOVC3 ( LEN__A_REC, A_REC, %REF(STR) )
      WRITE ( LUN, '(A)' ) STR(1:LEN__H_REC)
!
! --- Wriute the section with station definition
!
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#  Site ID    X-coord.      Y-coord.      Z-coord.     phi-geoc. longit.  height'
      WRITE ( LUN, FMT='(A)' ) '#'
      DO 440 J4=1,L_STA
!
! ------ Transform site coordinates
!
         READ ( UNIT=COO_STR(1,J4), FMT='(F14.4)' ) COO(1)
         READ ( UNIT=COO_STR(2,J4), FMT='(F14.4)' ) COO(2)
         READ ( UNIT=COO_STR(3,J4), FMT='(F14.4)' ) COO(3)
         CALL REF_ELL ( 0, COO, PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC )
!
! ------ Compute the transformation matrix formo the terrestrial coordiante
! ------ system to the local topocentric coordinates system
!
         CF = DCOS(PHI_GDT)
         SF = DSIN(PHI_GDT)
         CL = DCOS(LAMBDA)
         SL = DSIN(LAMBDA)
         TRS_TO_UEN(1,1,J4) = CF*CL
         TRS_TO_UEN(1,2,J4) = CF*SL
         TRS_TO_UEN(1,3,J4) = SF
!
         TRS_TO_UEN(2,1,J4) = -SL
         TRS_TO_UEN(2,2,J4) =  CL
         TRS_TO_UEN(2,3,J4) =  0.D0
!
         TRS_TO_UEN(3,1,J4) = -SF*CL
         TRS_TO_UEN(3,2,J4) = -SF*SL
         TRS_TO_UEN(3,3,J4) =  CF
!
! ------ Fill the records
!
         S_REC%SITE_ID = C_STA(J4)
         WRITE ( UNIT=S_REC%X_COORD,   FMT='(F13.4)' ) COO(1)
         WRITE ( UNIT=S_REC%Y_COORD,   FMT='(F13.4)' ) COO(2)
         WRITE ( UNIT=S_REC%Z_COORD,   FMT='(F13.4)' ) COO(3)
         WRITE ( UNIT=S_REC%GEOC_LAT,  FMT='(F8.4)'  ) PHI_GCN/DEG__TO__RAD
         WRITE ( UNIT=S_REC%LONGITUDE, FMT='(F8.4)'  ) LAMBDA/DEG__TO__RAD
         WRITE ( UNIT=S_REC%HEIGHT,    FMT='(F6.1)'  ) H_ELL
!
         CALL LIB$MOVC3 ( LEN__S_REC, S_REC, %REF(STR) )
         WRITE ( LUN, '(A)' ) STR(1:LEN__S_REC)
 440  CONTINUE 
!
! --- Write the section with amplitudes of harmonic variations
!
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#  Wave ID   Site ID     Up-cos  East-cos North-cos   Up-sin  East-sin North-sin'
      WRITE ( LUN, FMT='(A)' ) '#'
!
      DO 450 J5=1,L_STA
         DO 460 J6=1,L_HPE
            D_REC%WAVE_ID = HAR_NAM(J6) 
            D_REC%SITE_ID = C_STA(J5)
!
! --------- TRansforma amplitudes from the terrestrical coordinate system to 
! --------- the lcoal topocentric coordinate system
!
            CALL MUL_MV_IV_V ( 3, 3, TRS_TO_UEN(1,1,J5), 3, EST(1,J5,J6,1), &
     &                         3, UEN, -2 )
!
            WRITE ( UNIT=D_REC%UP_COS,    FMT='(F8.5)' ) UEN(1)
            WRITE ( UNIT=D_REC%EAST_COS,  FMT='(F8.5)' ) UEN(2)
            WRITE ( UNIT=D_REC%NORTH_COS, FMT='(F8.5)' ) UEN(3)
!
            CALL MUL_MV_IV_V ( 3, 3, TRS_TO_UEN(1,1,J5), 3, EST(1,J5,J6,2), &
     &                         3, UEN, -2 )
!
            WRITE ( UNIT=D_REC%UP_SIN,    FMT='(F8.5)' ) UEN(1)
            WRITE ( UNIT=D_REC%EAST_SIN,  FMT='(F8.5)' ) UEN(2)
            WRITE ( UNIT=D_REC%NORTH_SIN, FMT='(F8.5)' ) UEN(3)
!
            CALL LIB$MOVC3 ( LEN__D_REC, D_REC, %REF(STR) )
            WRITE ( LUN, '(A)' ) STR(1:LEN__S_REC)
 460     CONTINUE 
         WRITE ( LUN, FMT='(A)' ) '#'
 450  CONTINUE 
!
      WRITE ( LUN, FMT='(A)' ) HARPOS__LABEL
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NPV_TO_HPS  !#!#
