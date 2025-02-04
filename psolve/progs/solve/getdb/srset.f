      SUBROUTINE SRSET()
!CCCC
! "srset.f"
!
! Purpose           Routine to zero and blank out socom common block,
!                   i.e., re-initialize Solve system counters.
!
! Written           ?
!
! Notes             1. Based on ../include/socom.i.  Therefore SHOULD BE
!                   UPDATED WHENEVER socom.i IS CHANGED!
!                   2. Special handling for variables bround,
!                   flyby_warning, indl, isrsel, iuen, lnut(1),
!                   site_dep_el_cut, wvmask.  In effect, SOME PROGRAM
!                   OPTIONS ARE SET HERE.
!       NB: Some initialisaions here will be overlapped further by blkcl!!
!
! Subprograms used
!  Fortran          len
!
! Common            socom
!
! System            f77 under HP-UX 10.2
!
! Modifications
!  BA  92.01.28     "flyby_warning" parameter saved.
!  BA  93.05.18     Reinstalled above.
!  97.06.05 PET     Added INIT_INTERACTIVE variable setup
!  97.12.12 PET     Some changes: eliminated things which are doubled by blkcl
!  BA  98.08.24     Rewrote routine.  Variables now handled (generally)
!                   in order they appear in socom.i.  Double precision
!                   used throughout.  "MAX_DBS" used as necessary.
!                   Removed duplicate initializations.
!CCCC
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE 'socom.i'
!
      INTEGER*4  I, J, SAVEBG
      LOGICAL*2  SAVEFW
!
! --- Save necessary control variables.
!
! --- Flyby warning flag (set in OPTIN).
!
      SAVEFW = FLYBY_WARNING
      SAVEBG = BGROUND
!
! --- Zero out entire common block (just to be sure).
!
      DO I=1,JSOCOM_WORDS
         ISOCOM(I)=0
      ENDDO
!
! --- real*8 variables
!
      DO I = 1, MAX_CLK
        FJDCL(I) = 0.d0
      ENDDO
!
      DO I = 1, MAX_ATM
         TATM(I) = 0.D0
      ENDDO
      ELMIN = 0.d0
      DO I = 1, MAX_ROT
        TROT(I) = 0.D0
        DO J = 1, 4
          ROTAP(I,J) = 0.d0
        ENDDO
      ENDDO
!
      DO I = 1, 3
        WRMS(I) = 0.d0
        UT1INB(I) = 0.d0
        WOBINB(I) = 0.d0
      ENDDO
!
      CALCV = 0.D0
      ATMOS_INTERVAL = 0.D0
      CLOCK_INTERVAL = 0.D0
      NROT_A1(1) = 0.D0
      NROT_A1(2) = 0.D0
      TROT_A1 = 0.D0
      DO I = 1, MAX_EROT_VALUES
         UT1PTB(I) = 0.D0
         WOBXXB(I) = 0.D0
         WOBYYB(I) = 0.D0
      ENDDO
!
      FCNPER = 0.D0
!
! --- Zero out "elvcut" elevation cutoff site array.
!
      DO I = 1, MAX_ARC_STA
         SACNST(i) = 0.D0
         SCCNST(i) = 0.D0
         ELVCUT(i) = 0.D0
      ENDDO
!
      DO I = 1, 3
         EOPCONS(I)  = 0.D0
         EOPRCONS(I) = 0.D0
      ENDDO
!
      SEOCNST(2) = 0.D0
      PWCCNST    = 0.D0
      NUTCONS(1) = 0.D0
      NUTCONS(2) = 0.D0
      DO I = 1, MAX_GRAD
         TGRAD(I) = 0.D0
      ENDDO
      GRAD_INTERVAL = 0.D0
      GRADCONS(1)   = 0.D0
      GRADCONS(2)   = 0.D0
!
! --- Logical*2
!
!--- "logbcl", a baseline clock variable.
!
      LOGBCL       = .FALSE.
      BMODE_CL     = .FALSE.
      BMODE_AT     = .FALSE.
      CLK_BRK_STAT = .FALSE.
!
! --- Note proper reseting of "flyby_warning".
!
      FLYBY_WARNING   = SAVEFW
      SITE_DEP_CONST  = .FALSE.
      SIMULATION_TEST = .FALSE.
!
! --- Default site dependent elevation cutoff set to .FALSE..
!
      SITE_DEP_EL_CUT = .FALSE.
      SHORT_UT1_IN    = .FALSE.
      SOL_AVAIL       = .FALSE.
      OLD_CLOCKS      = .FALSE.
      OLD_ATMS        = .FALSE.
      SKIP_EOP_OFF    = .FALSE.
!
! --- Character
!
      DO I = 1, LEN(USER_PRO)
         USER_PRO(I:I) = ' '
      ENDDO
!
      DO I = 1, LEN(USER_BUF)
         USER_BUF(I:I) = ' '
      ENDDO
!
      DO I = 1, LEN(SCR_FIL_ORIGIN)
         SCR_FIL_ORIGIN = ' '
      ENDDO
      UT1_RS = ' '
      UT1_RS_FLYBY = ' '
!
! --- Integer*2
!
      NUMSTR = 0
      NUMSTA = 0
      NPOLD = 0
!
!--- "iclock", for baseline clocks.
!
      DO J = 1, MAX_ARC_STA
         DO I = 1, ARC_STA_BIT_WORDS
            ICLOCK(I,J) = 0
         ENDDO
      ENDDO
      IDNWT    = 0
      IPRES    = 0
      IRNCD(1) = 0
      IRNCD(2) = 0
      ITDGLB   = 0
      NPARAM   = 0
      IDATYP   = 0
      NROT     = 0
      NSOURC   = 0
      DO I = 1, MAX_ARC_STA
         NSPARM(I)  = 0
         NUMATM(I)  = 0
         NUMGRAD(I) = 0
         IATSTR(I)  = 0
      ENDDO
!
      ICLMAX = 0
      DO I = 1, MAX_ARC_STA
         NUMCLK(I) = 0
         ICLSTR(I) = 0
      ENDDO
      IPSTP = 0
      DO I = 1, 3
         LNUT(I) = 0
      ENDDO
!
      LPREC = 0
      DO J = 1, 3
         DO I = 1, STA_BIT_WORDS
            LTIDE(I,J) = 0
         ENDDO
      ENDDO
      LREL = 0
!
      DO J = 1, 3
         DO I = 1, ROT_BIT_WORDS
            LROT(I,J) = 0
         ENDDO
      ENDDO
      DO J = 1, 3
         DO I = 1, ATM_BIT_WORDS
            LATM(I,J) = 0
         ENDDO
      ENDDO
      DO I = 1, MAX_CLK
         LCLK(I) = 0
      ENDDO
      DO J = 1, 2
         DO I = 1, SRC_BIT_WORDS
            LSTAR(I,J) = 0
         ENDDO
      ENDDO
!
! --- Reset "lsitec", "inset" related (?).
!
      DO I = 1, STA_BIT_WORDS
         LAXOF(I) = 0
         DO J = 1, 3
            LSITEC(I,J) = 0
         ENDDO
      ENDDO
!
      DO I = 1, SRC_BIT_WORDS
         ISRSEL(I) = -1 ! all bits on
      ENDDO
!
! --- Default XYZ and no radial components.
!
      IUEN = 0
      DO I = 1, ARC_STA_BIT_WORDS
         DO J = 1, MAX_CLK
            ICLSTA(I,J) = 0
         ENDDO
      ENDDO
      NFLEPS = 0
      DO I = 1, 14
        FLEPS(I) = 0
        FLPSI(I) = 0
      ENDDO
!
      NFLPSI = 0
      DO I = 1, 7
        IDPNUT(I) = 0
      ENDDO
      NDPNUT = 0
!
! --- Reset "lsitev", "inset" related (?).
!
      DO I = 1, STA_BIT_WORDS
         DO J = 1, 3
            LSITEV(STA_BIT_WORDS,3) = 0
         ENDDO
      ENDDO
!
      IARCSOC = 0
      NSLAST  = 0
      IDBSEL  = 0
      NDB     = 0
      IDCSEL  = 0
      DO I = 1, ARC_STA_BIT_WORDS
         DO J = 1, MAX_ARC_STA
            IBLSEL_G(I,J) = 0
            IBLSEL_P(I,J) = 0
         ENDDO
      ENDDO
!
      CONSTRAINT_BITS = 0
!
! --- Set "indl" so site params menu comes up in "automatic" mode.
!
      indl = 2
!
!--- Set default to ignore WVR-based data edits.
!
      DO I = 1, MAX_ARC_STA
         WVMASK(I) = 0
      ENDDO
!
      BM_REF_CL         = 0
      CLOCK_REF_BITS(1) = 0
      CLOCK_REF_BITS(2) = 0
      NROT_A1(1)        = 0
      NROT_A1(2)        = 0
      EOP_STYLE(1)      = 0
      EOP_STYLE(2)      = 0
      EOPA1_CHOICE(1)   = 0
      EOPA1_CHOICE(2)   = 0
      IEOPL             = 0
      NUMSTAX           = 0
      INTERPOLATION_UT1 = 0
      INTERPOLATION_PM  = 0
!
! --- Restore "bground" to runtime value.
!
      BGROUND = SAVEBG
      DO I = 1, SRC_BIT_WORDS
        DO J = 1, 2
          LPROP(I,J) = 0
        ENDDO
      ENDDO
!
      TOTSTA   = 0
      LGRAD(1) = 0
      LGRAD(2) = 0
!
! --- Integer*4
!
      NUMOBS = 0
      DO I = 1, MAX_DBS
         IDBEND(MAX_DBS) = 0
      ENDDO
!
! --- Set flag of initialization
!
      INIT_INTERACTIVE =  1
!
      RETURN
      END  !#!  SRSET  #!#
