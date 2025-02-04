      SUBROUTINE A3JST ( KSRC, OVRTRA, OVRTRA_CLK, EOPTRACE, KCONS, &
     &                   SITES_ESTIMATED, OVRTRA_GRD, ARR, TOTAL_SITEC, &
     &                   LBUF_LEN, LBUF, IPTR, PAGEWID, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  A3JST PROGRAM SPECIFICATION
!
! 1.1 A3JST does the final clean up after the parameters have
!     been printed.
!
! 1.2 REFERENCES:
!
! 2.  A3JST INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2   KSRC, KCONS, SITES_ESTIMATED
      REAL*8      OVRTRA, OVRTRA_CLK, EOPTRACE, OVRTRA_GRD
      REAL*8      ARR(*), TOTAL_SITEC(3,MAX_STA)
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
! EOPTRACE - sum of earth orientation constraint shares
! KCONS - True if any constraints were applied
! KSRC - True if any source coordinate parameters were printed
! OVRTRA - Overall sum of atmosphere constraint shares
! OVRTRA_CLK - Overall sum of clock constraint shares
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'buff2.i'
      INCLUDE 'buff4.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
      INCLUDE 'cnstr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: adjst
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
!     ISCNT ( IN BUFF1 )will hold counters for BASFE indicating where
!     IN THE normal equations array the site coordinates are located.
!
      INTEGER*4  IUER
      INTEGER*4  J1, J2, ICOU
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*2   I, J, ICNT, IS, FIRST_LINE, LAST_LINE, LMINF, N
      INTEGER*2   TMPSCNT(3,MAX_STA_CMP), TMPCNT, IOS
      INTEGER*4   IX, IY, ICHR, PAGELEN, IER, IOS4
      CHARACTER*4 CCHAR
      EQUIVALENCE (ICHR,CCHAR)
      CHARACTER    BUFSTR*120, STR1*32, STR2*32
      LOGICAL*2    KBIT
      REAL*8       CHISQRG, VS2(3,MAX_STA), SCALE_CNS_SIG
      COMMON       / ADJ / VS2
      CHARACTER    FNAME*128
      INTEGER*4    PAGEWID_DUMMY 
!
! --- Arrays for monuments information
!
      REAL*8     XOFFST(3,MAX_STA)
      INTEGER*2  MOTYPE(MAX_STA), MONAM(5,MAX_STA), LSCRD(3,MAX_STA)
      COMMON    / BASEMA / XOFFST, MOTYPE, MONAM, LSCRD
      INTEGER*4, EXTERNAL ::  I_LEN
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   911219 changed calls to use_buffer to basfe_file when BUFF1_WORDS
!                and BUFF3_WORDS is passed because use_buffer used pipes
!                that can't handle that many words for read and write.
!                We now write to and read from a file instead of using pipes
!                for these cases.
!   PET   970717 Suppressed to print Chi-suqare in FAST mode without full
!                calcualation of covariance matrix
!   PET   971211 Added capacity to schedule MDLPL-extension
!   PET   980202 Corrected small bug: chi-sq/ndg was not printed in previuous
!                version when FAST_COV = F__FUL mode was in use. Time profiling
!                feature added.
!   PET   980209 Added printing information about general usage of constraints
!   PET   980330 Added printing information about type and sutypes of some
!                constraints
!   PET   990309 Eliminated a call of PROG_END and put it to ADJST
!   PET   990404 Rewrote end of the program for support of NO TRAIN mode for
!                computations of baseline length and/or baseline velocity
!                values
!   PET   990407 Fixed a bug: the previous versions wrote constraints
!                information in one file CSPRxx regardless of mode: global
!                or local. As a result constraints imposed on global paramters
!                may be overlapped by constraints imposed on local parameres
!                in the case when arc-file for the first database was not
!                saved on disk and PROC was called to create it.
!   pet   1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                     eliminated common block adj_buf
!   pet   2001.01.11  Removed phrase "Re-scale the scaled errors by ..." since
!                     at last now Solve computes sigmas of adjustments
!                     correctly. Instead of the routine writes whcih sigmas,
!                     a-sigma or m-sigma, are pregerable if a-sigma differs
!                     from m-sigma > 20%
!   pet   2002.05.31  Added parameter TOTAL_SITEC for carrying total site
!                     positions
!   pet   2003.08.07  Replaced CNPLT with REPA
!   pet   2003.12.09  Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!   pet   2004.10.26  Replaced hard-coded values for PgUp, PgDn keys with
!                     the values which are the same in BIG_ENDIAN and 
!                     LITTLE_ENDIAN environment
!   pet   2005.02.04  Made ADJST to call OPTIN adter REPA ends
!   pet   2020.12.30  Added support of apriori velocity for computation of &
!                     the total positions in the baseline solution
!
! 5.  A3JST PROGRAM STRUCTURE
!
! --- Read information about constraints
!
      CALL ERR_PASS ( IUER, IER )
      IF ( KGLOBALS ) THEN
           CALL READ_CNSTR ( CNSTROBJ, CNI__GLO, IER )
         ELSE
           CALL READ_CNSTR ( CNSTROBJ, CNI__LOC, IER )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3751, IUER, 'A3JST', 'Error in an attempt '// &
     &         'to read constriants equations' )
           RETURN
      END IF
!
! --- Printing information about general usage of constraints
!
      IF ( CNSTROBJ%N_EQUAT .EQ. 0  .AND.  CNSTROBJ%N_TYCNS .EQ. 0 ) THEN
           IPTR = IPTR + 2
           IF ( KSPOOL   ) WRITE ( 23, * ) ' '
           IF ( KGLOBALS ) THEN
                LBUF(IPTR) =    ' No constraints on CGM have been imposed '
              ELSE
                LBUF(IPTR) =    ' No constraints have been imposed '
           END IF
           IF ( KSCREEN  ) CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           IF ( KSPOOL   ) WRITE ( 23, * ) LBUF(IPTR)(:PAGEWID)
           IPTR = IPTR + 1
           IF ( KSPOOL   ) WRITE ( 23, * ) ' '
         ELSE
           IF ( KSPOOL   ) WRITE ( 23, '(A)' ) ' '
           IPTR = IPTR + 2
           IF ( KGLOBALS ) THEN
                LBUF(IPTR) =    ' CGM constraints usage information: '
              ELSE
                IF ( ISLTY2 .EQ. 'I' ) THEN
                     LBUF(IPTR) =    ' General constraints usage information: '
                   ELSE
!
! ------------------ If it was not indenedent solutiuon then nothing to do.
!
                     IPTR = IPTR - 2
                     GOTO 810
                END IF
           END IF
           IF ( KSCREEN  ) CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           IF ( KSPOOL   ) WRITE ( 23, '(A)' ) LBUF(IPTR)(:PAGEWID)
           IPTR = IPTR + 1
           IF ( KSPOOL   ) WRITE ( 23, '(A)' ) ' '
!
           ICOU = 0
           DO 410 J1=1,CNSTROBJ%N_EQUAT
              IF ( (       KGLOBALS  .AND.        CNSTROBJ%GLO_CNS(J1) ) .OR. &
     &             ( .NOT. KGLOBALS  .AND.  .NOT. CNSTROBJ%GLO_CNS(J1) )  ) THEN
!
                 IF ( CNSTROBJ%SBI_CNS(J1) .EQ. 1 ) THEN
                      SCALE_CNS_SIG = 1.0D0
                      IF ( CNSTROBJ%UNT_CNS(J1) .EQ. '10^-14 sec/sec' ) THEN
                           SCALE_CNS_SIG = 1.D14
                         ELSE IF ( CNSTROBJ%UNT_CNS(J1) .EQ. 'psec/hr' ) THEN
                           SCALE_CNS_SIG = 3600.0D0/1.D-12
                         ELSE IF ( CNSTROBJ%UNT_CNS(J1) .EQ. 'mas/day' ) THEN
                           SCALE_CNS_SIG = 1.D0/MAS__TO__RAD
                         ELSE IF ( CNSTROBJ%UNT_CNS(J1) .EQ. 'msec/day' ) THEN
                           SCALE_CNS_SIG = 1.0D3
                      END IF
                      ICOU = ICOU + 1
                      IPTR = IPTR+1
                      WRITE ( UNIT=LBUF(IPTR), FMT=110 ) ICOU, &
     &                             CNSTROBJ%ABB_CNS(J1), &
     &                             CNSTROBJ%DSC_CNS(J1), &
     &                             CNSTROBJ%SIG_CNS(J1)*SCALE_CNS_SIG, &
     &                             CNSTROBJ%UNT_CNS(J1)
 110                  FORMAT ( I2, ') ', A8, ' "', A32, '" sigma ',1PE10.3, &
     &                         ' ',A16 )
                      IF ( KSCREEN  ) CALL ADDSTR_F     ( LBUF(IPTR)(:PAGEWID) )
                      IF ( KSPOOL   ) WRITE ( 23, '(A)') &
     &                                        LBUF(IPTR)(1:I_LEN(LBUF(IPTR)))
                 END IF
              END IF
 410       CONTINUE
!
! -------- Obsolete constraints
!
           DO 420 J2=1,CNSTROBJ%N_TYCNS
              IF ( (       KGLOBALS  .AND.        CNSTROBJ%GLO(J2) ) .OR. &
     &             ( .NOT. KGLOBALS  .AND.  .NOT. CNSTROBJ%GLO(J2) )      ) THEN
!
                   ICOU = ICOU + 1
                   IPTR = IPTR+1
                   WRITE ( UNIT=LBUF(IPTR), FMT=110 ) ICOU, CNSTROBJ%ABBR(J2), &
     &                                                      CNSTROBJ%DESCR(J2), &
     &                                                      CNSTROBJ%SIGMA(J2), &
     &                                                      CNSTROBJ%UNITS(J2)
                   IF ( KSCREEN  ) CALL ADDSTR_F     ( LBUF(IPTR)(:PAGEWID) )
                   IF ( KSPOOL   ) WRITE ( 23, '(A)') &
     &                                           LBUF(IPTR)(1:I_LEN(LBUF(IPTR)))
!
! ---------------- Puting additional information about some constraint
!
                   IF ( CNSTROBJ%ABBR(J2) .EQ. 'LIN_STA' ) THEN
!
! --------------------- Putting additional information about LIN_STA constraint
!
                        IPTR = IPTR + 1
                        WRITE ( UNIT=LBUF(IPTR), FMT=120) &
     &                               STA_WT_FIL(1:I_LEN(STA_WT_FIL)), &
     &                               CNSTROBJ%ABBR(J2)
 120                    FORMAT ( 4X,'Station file "',A,'" was used for ', A )
                        IF ( KSCREEN ) CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                        IF ( KSPOOL  ) WRITE ( 23, '(A)') &
     &                                         LBUF(IPTR)(1:I_LEN(LBUF(IPTR)))
                   END IF
!
                   IF ( CNSTROBJ%ABBR(J2) .EQ. 'NNT_POS'  .OR. &
     &                  CNSTROBJ%ABBR(J2) .EQ. 'NNT_VEL'       ) THEN
!
! --------------------- Putting additional information about NNT_POS or
! --------------------- NNT_VEL constraint
!
                        IPTR = IPTR + 1
!
                        CALL CLRCH ( STR1 )
                        IF ( ( CNSTROBJ%ABBR(J2) .EQ. 'NNT_POS' .AND. &
     &                         KMATRIX_NNTP .EQ. 'AL'                 ) .OR. &
     &                       ( CNSTROBJ%ABBR(J2) .EQ. 'NNT_VEL' .AND. &
     &                         KMATRIX_NNTV .EQ. 'AL'                 ) ) THEN
                             STR1 = 'Total'
                           ELSE
                             STR1 = 'Horizontal'
                        END IF
!
                        CALL CLRCH ( STR2 )
!
                        CALL CLRCH ( STR2 )
                        IF ( ( CNSTROBJ%ABBR(J2) .EQ. 'NNT_POS'  .AND. &
     &                         KSIG_SCALE_NNTP   .EQ. 'WG'             ) .OR. &
     &                       ( CNSTROBJ%ABBR(J2) .EQ. 'NNT_VEL'  .AND. &
     &                         KSIG_SCALE_NNTV   .EQ. 'WG'             ) ) THEN
                             STR2 = 'Weighted'
                           ELSE
                             STR2 = 'Uniform'
                        END IF
!
                        WRITE ( UNIT=LBUF(IPTR), FMT=130 ) CNSTROBJ%ABBR(J2), &
     &                                                     STR1(1:I_LEN(STR1)), &
     &                                                     STR2(1:I_LEN(STR2))
 130                    FORMAT ( 4X,'Type of ',A,' constraint: "',A, &
     &                              '", subtype: "',A,'"' )
                        IF ( KSCREEN ) CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                        IF ( KSPOOL  ) WRITE ( 23, '(A)') &
     &                                         LBUF(IPTR)(1:I_LEN(LBUF(IPTR)))
                   END IF
!
                   IF ( CNSTROBJ%ABBR(J2) .EQ. 'NNR_POS'  .OR. &
     &                  CNSTROBJ%ABBR(J2) .EQ. 'NNR_VEL'  .OR. &
     &                  CNSTROBJ%ABBR(J2) .EQ. 'NNR_SRC'       ) THEN
!
! --------------------- Putting additional information about NNR constraints
!
                        IPTR = IPTR + 1
!
                        CALL CLRCH ( STR2 )
                        IF ( ( CNSTROBJ%ABBR(J2) .EQ. 'NNR_POS'  .AND. &
     &                         KSIG_SCALE_NNRP   .EQ. 'WG'             ) .OR. &
     &                       ( CNSTROBJ%ABBR(J2) .EQ. 'NNR_VEL'  .AND. &
     &                         KSIG_SCALE_NNRV   .EQ. 'WG'             ) .OR. &
     &                       ( CNSTROBJ%ABBR(J2) .EQ. 'NNR_SRC'  .AND. &
     &                         KSIG_SCALE_NNRS   .EQ. 'WG'             ) ) THEN
                             STR2 = 'Weighted'
                           ELSE
                             STR2 = 'Uniform'
                        END IF
!
                        WRITE ( UNIT=LBUF(IPTR), FMT=140 ) CNSTROBJ%ABBR(J2), &
     &                                                     STR2(1:I_LEN(STR2))
 140                    FORMAT ( 4X,'Subtype of ',A,' constraint: "',A,'" ' )
                        IF ( KSCREEN ) CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                        IF ( KSPOOL  ) WRITE ( 23, '(A)') &
     &                                         LBUF(IPTR)(1:I_LEN(LBUF(IPTR)))
                   END IF
              END IF
 420       CONTINUE
!
           IF ( ICOU .GT. 0 ) THEN
                IPTR = IPTR + 1
                IF ( KSPOOL   ) WRITE ( 23, '(A)' ) ' '
           END IF
 810       CONTINUE
      END IF
!
! --- Correct chi-square calculation
!
      ARC_SHARE=0.0
      IF ( KCONS .AND. .NOT.SIMULATION_TEST ) THEN
           CHISQRG   = CHINMRG/(CHIDNMG+OVRTRA_CLK+OVRTRA+EOPTRACE+OVRTRA_GRD)
           ARC_SHARE = OVRTRA_CLK+OVRTRA+EOPTRACE+OVRTRA_GRD
           CSHARE=CSHARE+ARC_SHARE
           IF ( KSCREEN ) THEN
              IPTR=IPTR+1
              IF ( FAST_MODE .EQ. F__NONE  .OR. &
     &             FAST_COV  .EQ. F__SEG   .OR. &
     &             FAST_COV  .EQ. F__FUL         ) THEN
!
                   WRITE  ( LBUF(IPTR), 9962 ) CHISQRG
 9962              FORMAT ( ' Corrected Reduced Chi-Square ',  F8.4 )
                   IF ( CHISQRG .LT. 0.64  .OR. CHISQRG .GT. 1.44  .AND. &
     &                  ISLTY2 .EQ. 'I' ) THEN
!
                        CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                        CALL NL_MN()
                        IPTR=IPTR+1
                        WRITE  ( LBUF(IPTR), '(A)' ) ' NB: m-sigmas are '// &
     &                          'preferable for this solution'
                   END IF
                ELSE
                   WRITE  ( 23, 9963   )
 9963              FORMAT ( ' Corrected Reduced Chi-Square    N/A  ' )
              END IF
              CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
              CALL NL_MN()
           ENDIF
!
           IF ( KSPOOL ) THEN
              IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_COV .EQ. F__SEG ) THEN
                   WRITE ( 23, 9962   ) CHISQRG
                   IF ( CHISQRG .LT. 0.64  .OR. CHISQRG .GT. 1.44   .AND. &
     &                  ISLTY2 .EQ. 'I' ) THEN
!
                        WRITE  ( 23, '(A)' ) ' NB: m-sigmas are '// &
     &                          'preferable for this solution'
                   END IF
                   WRITE ( 23, '(1X)' )
                ELSE
                   WRITE ( 23, 9963   )
                   WRITE ( 23, '(1X)' )
              ENDIF
           ENDIF
      ENDIF
!
      NUM_USER_PART = OLD_USER_PART
      CALL USE_GLBFIL ( 'OWC' )
!
! --- Optional listing of source-source arc distances
!
      IF ( NUMSTR.GE.2 .AND. KSRC .AND. (.NOT.KBATCH) .AND. &
     &     KBIT( IPRES, INT2(2)) ) THEN
           CALL RUN_PROG ( 'ALEN', 'WAIT', INT2(0) )
      END IF
!
! --- Handle batch mode flags
!
      CCHAR(4:4) = '?'
      IF ( KLCLBSL  .AND.  (.NOT.KGLOBALS)  .AND.  .NOT. KBIT( IUEN, INT2(1))) &
     &      GOTO 405
      IF ( KGLBBSL  .AND.        KGLOBALS   .AND.  .NOT. KBIT( IUEN, INT2(1))) &
     &      GOTO 405
!
      IF ( KBATCH ) THEN
!
! -------- Batch mode? Good bye, my love, good bye...
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
      IF ( ISCREEN == ISCREEN_NO ) THEN
!
! -------- Batch mode? Good bye, my love, good bye...
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
!
! --- Writing menu in interactive mode
!
      CALL GET_TERMSIZE ( PAGELEN, PAGEWID_DUMMY ) 
      IF ( PAGELEN .LE. 0 ) PAGELEN=24
      LMINF = PAGELEN - 6
      LAST_LINE=IPTR
      CALL BEEP_MN()
!
  401 CONTINUE
      IY=PAGELEN-1
      CALL SETSCRREG_MN ( 0, IY )
      CALL SETCR_MN ( 0, 0 )
      call addstr_f ( " Control: ' '-BASFE, 'P'-Plots" )
      call addstr_f ( ", 'O'-OPTIN, 'T'-Terminate, '*'Atm/Clock plots'/'" )
      call nl_mn()
      call addstr_f ( "  'E'-Sites 'S'-Sources 'L'-Last Page 'B'-Baselines" )
      call addstr_f ( " 'X'-Data bases             |" )
      call nl_mn()
      call reverse_on_mn()
      call addstr_f ( " '-','+':Line up/down  '<','>':Page up/down  "// &
     &                "'0'-'9' : Start n/10 of way through" )
      call nl_mn()
      call reverse_off_mn()
      CALL SETCR_MN ( 0, 3 )
      IY=PAGELEN-1
      CALL SETSCRREG_MN ( 4, IY )
      goto 403
!
  402 CONTINUE
      CALL SENKRS_MN ( IX, IY, ICHR )
!
! --- If no sites estimated don't try to call basfe.
!
      IF ( CCHAR(4:4) .EQ.' ' .AND. .NOT. SITES_ESTIMATED ) CCHAR(4:4) = 'O'
      IF ( CCHAR(4:4) .EQ. '+' ) THEN
           IF ( LAST_LINE .LT. IPTR ) THEN
                LAST_LINE=LAST_LINE+1
                CALL ADDSTR_F(LBUF(LAST_LINE)(:PAGEWID) )
                CALL NL_MN()
           ENDIF
           GOTO 402
         ELSE IF ( CCHAR(4:4) .EQ. '-' ) THEN
           IF ( FIRST_LINE .GT. 1 ) LAST_LINE=LAST_LINE-1
           GOTO 403
         ELSE IF ( CCHAR(4:4) .EQ. CHAR(19) .OR. CCHAR(4:4) .EQ. '>' ) THEN
           LAST_LINE=LAST_LINE+LMINF
           IF ( LAST_LINE .GT. IPTR ) LAST_LINE=IPTR
           GOTO 403
         ELSE IF ( CCHAR(4:4) .EQ. CHAR(18) .OR.  CCHAR(4:4) .EQ. '<' ) THEN
           LAST_LINE=LAST_LINE-LMINF
           IF ( LAST_LINE-LMINF .LT. 1 ) LAST_LINE=LMINF+1
           GOTO 403
         ELSE IF ( CCHAR(4:4).GE.'0' .AND. CCHAR(4:4).LE.'9' ) THEN
           READ(CCHAR(4:4),'(i1)') N
           LAST_LINE = 1 + LMINF + N*(IPTR/10) ! LMINF+(N*(IPTR-(LMINF+1)))/8+1
           IF ( CCHAR(4:4) == '9' ) LAST_LINE = IPTR
           GOTO 403
         ELSE
           GOTO 404
      ENDIF
403   CONTINUE
      FIRST_LINE=LAST_LINE-LMINF
      CALL SETCR_MN ( 0, 4 )
      CALL CLRTOBOT_MN()
      DO I=FIRST_LINE,LAST_LINE
         IF ( I > 0 ) THEN
              CALL ADDSTR_F(LBUF(I)(:PAGEWID) )
              CALL NL_MN()
         END IF
      ENDDO
      GOTO 402
404   continue
      IF  (CCHAR(4:4) .NE.' '  .AND. &
     &     CCHAR(4:4) .NE.'O'  .AND. &
     &     CCHAR(4:4) .NE.'P'  .AND. &
     &     CCHAR(4:4) .NE.'E'  .AND. &
     &     CCHAR(4:4) .NE.'S'  .AND. &
     &     CCHAR(4:4) .NE.'L'  .AND. &
     &     CCHAR(4:4) .NE.'B'  .AND. &
     &     CCHAR(4:4) .NE.'T'  .AND. &
     &     CCHAR(4:4) .NE.'X'  .AND. &
     &     CCHAR(4:4) .NE.'*'  .AND. &
     &     CCHAR(4:4) .NE.'/'  .AND. &
     &     CCHAR(4:4) .NE.'M'        )   GOTO 402
!
      IF ( CCHAR(4:4) .EQ. 'M' ) THEN
!
! -------- Write adjusted site positions to user's *MODFL
!
           WRITE(bufstr,2342) PRE_LETRS
 2342      FORMAT ( "Creating mod file *MOD", A, ".")
           CALL ADDSTR_F ( BUFSTR )
           CALL NL_MN()
           CALL NL_MN()
           CALL NL_MN()
           CALL MDFIL ( ISITN, VSITEC, NUMSTA, IX, IY, LBUF_LEN, LBUF, IPTR, &
     &                  PAGEWID )
           GOTO 401
      END IF ! Write adjusted site positions
!
! --- Schedule sites, sources, last page, baselines or data bases
!
      CALL END_MN()
      IF ( CCHAR(4:4).EQ.'E'  .OR. &
     &     CCHAR(4:4).EQ.'S'  .OR. &
     &     CCHAR(4:4).EQ.'L'  .OR. &
     &     CCHAR(4:4).EQ.'B'  .OR. &
     &     CCHAR(4:4).EQ.'X'        ) THEN
!
           IF ( CCHAR(4:4) .EQ. 'E' ) ICNT =  1
           IF ( CCHAR(4:4) .EQ. 'S' ) ICNT = -1
           IF ( CCHAR(4:4) .EQ. 'L' ) ICNT = -2
           IF ( CCHAR(4:4) .EQ. 'B' ) ICNT = -5
           IF ( CCHAR(4:4) .EQ. 'X' ) ICNT = -6
           CALL USE_BUFFER ( ICNT, INT2(1), 'OWC' )
           CALL RUN_PROG   ( 'SETFL', 'PASS', INT2(0) )
      END IF
!
      IF ( CCHAR(4:4) .EQ. 'O' ) CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
      IF ( CCHAR(4:4) .EQ. 'P' ) THEN
           CALL RUN_PROG ( 'REPA',  'WAIT', INT2(0) )
           CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
      END IF
!
      IF ( CCHAR(4:4) .EQ. '*' ) THEN
           CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC' )
           CALL RUN_PROG ( 'MDLPL', 'PASS', INT2(0) )
      ENDIF
      IF ( CCHAR(4:4) .EQ. '/' ) THEN
           CALL UN_CURSES()  ! Eliminate influence of the curses
           CALL USE_BUFFER ( INT2(1), INT2(1), 'OWC' )
           CALL RUN_PROG   ( 'MDLPL', 'PASS', INT2(0) )
      ENDIF
!
! --- Terminate by schedluing SLEND
!
      IF  ( CCHAR(4:4) .EQ. 'T' ) CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
!
! --- Basfe
!
405   CONTINUE
      IF ( KBIT( IUEN, INT2(1) ) ) THEN
           CALL NL_MN()
           CALL ADDSTR_F ( "   No Baseline information when using UEN" )
           CALL REFRESH_MN()
           GOTO 401
      ENDIF
!
! --- Write saved screen output to file to be used by adjust to scroll
! --- back display
!
      IF ( KSCREEN .AND. .NOT.KBATCH ) THEN
           FNAME=PRE_SCR_DIR(:PRE_SD_LEN)//'CRBF'//PRE_LETRS
           OPEN ( 34, FILE=FNAME, IOSTAT=IOS4 )
           IOS = IOS4
           CALL FERR ( IOS, "Opening CRES display buffer", INT2(0), INT2(0) )
           DO I=1,IPTR
              WRITE ( 34, '(A120)',IOSTAT=IOS4 ) LBUF(I)
              IOS = IOS4
              CALL FERR ( IOS, "Writing CRES display buffer", INT2(0), INT2(0) )
           ENDDO
           CLOSE ( 34, IOSTAT=IOS4 )
           IOS = IOS4
           CALL FERR ( IOS, "Closing CRES display buffer", INT2(0), INT2(0) )
      ENDIF
!
! --- Write some information to common area BUFF1 to transfer it after
! --- to program BASFE
!
      DO I=1,NUMSTA
         DO J=1,3
            VSITE1(J,I)  = TOTAL_SITEC(J,I)
            VSITE1V(J,I) = VS2(J,I)
         ENDDO
      ENDDO
!
      IF ( TRAIN ) THEN
           CALL BASFE_FILE ( BUFBUF4, BUFF4_WORDS, 'OWC' )
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'ADJST' )
           END IF
      END IF
!
! --- Turn on and schedule BASFE without wait
! --- NSCNT1  -- number of baselines with station poistions to have been
! ---            estimated
! --- NSCNT1V -- number of baselines with station velocities to have been
! ---            estimated
!
      IS=0
      IF ( NSCNT1 .NE. 0 ) IS=ISCNT(1,NSCNT1)
!
! --- Get flyby values & monument file name
!
      IF ( .NOT. KGLOBALS ) THEN
           CALL USE_GLBFIL   ( 'OR' )
           CALL USE_GLBFIL_4 ( 'RC' )
           CALL FLYBY_APRIOR()
         ELSE
!
! -------- Read global common blocks. The true is that we have to update
! -------- a priori station coordinates and velocities
!
           CALL ACS_COVFIL   ( 'O' )
           CALL USE_COVF_COM ( 'R' )
           CALL ACS_COVFIL   ( 'C' )
!
           CALL USE_GLBFIL   ( 'ORC' )
      ENDIF
!
      IF ( NSCNT1V .EQ. 0   .OR.  .NOT. KGLOBALS ) THEN
!
! -------- Compute informations about baselines, but not baseline velocities
!
           IF ( TRAIN ) THEN
                CALL USE_SPOOL ( 'C' )
                CALL RUN_PROG  ( 'BASFE', 'PASS', INT2(0) )
              ELSE
                IF ( NSCNT1 .NE. 0 ) THEN
!
! ------------------ Set up LSCRD for CVMML
!
                     CALL LVECT ( LSCRD, ISCNT, ABS(NSCNT1) )
!
! ------------------ Compute baselines
!
                     CALL BWORK ( LSCRD, XOFFST, MONAM, MOTYPE, ARR, LBUF, &
     &                            IPTR )
                END IF
           ENDIF
         ELSE
!
! -------- First compute baseline vectors for all baselines
!
           IF ( TRAIN ) THEN
                CALL USE_SPOOL ( 'C' )
                CALL RUN_PROG  ( 'BASFE', 'WAIT', INT2(0) )
              ELSE IF ( NSCNT1 .NE. 0 ) THEN
!
! ------------- Set up LSCRD for CVMML
!
                CALL LVECT ( LSCRD, ISCNT, ABS(NSCNT1) )
!
! ------------- Compute baselines
!
                CALL BWORK ( LSCRD, XOFFST, MONAM, MOTYPE, ARR, LBUF, IPTR )
           END IF
!
! -------- Put velocities of the stations on the place of station positions
!
           DO I=1,NUMSTA
              DO J=1,3
                 VSITE1V(J,I) = VSITEC(J,I)
                 VSITE1(J,I)  = VS2(J,I)
              ENDDO
           ENDDO
!
           DO I=1,MAX(NSCNT1V,NSCNT1)
              DO J=1,3
                 TMPSCNT(J,I) = ISCNT(J,I)
                 ISCNT(J,I)   = ISCNTV(J,I)
                 ISCNTV(J,I)  = TMPSCNT(J,I)
               ENDDO
           ENDDO
!
           IS=0
           IF ( NSCNT1V .NE. 0 ) IS = ISCNT(1,NSCNT1V)
           TMPCNT  =  NSCNT1
           NSCNT1  = -NSCNT1V
           NSCNT1V = TMPCNT
!
! -------- Then compute baseline velocity vectors
!
           IF ( TRAIN ) THEN
                CALL BASFE_FILE ( BUFBUF4, BUFF4_WORDS, 'OWC' )
                CALL RUN_PROG   ( 'BASFE', 'PASS', INT2(0) )
              ELSE IF ( NSCNT1 .NE. 0 ) THEN
!
! ------------- Set up LSCRD for CVMML
!
                CALL LVECT ( LSCRD, ISCNT, ABS(NSCNT1) )
!
! ------------- Compute baselines
!
                CALL BWORK ( LSCRD, XOFFST, MONAM, MOTYPE, ARR, LBUF, IPTR )
           END IF
      ENDIF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  A3JST  #!#
