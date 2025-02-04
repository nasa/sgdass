#define NO_DEBUG
      SUBROUTINE DO_SRC ( FAST_MODE, FAST_DBG, FGLOBAL_L4, FGLSRC_L4, &
     &                    IWDS, NPARM_ALL, IPARM_ALL, NPARM_GLO, IPARM_GLO, &
     &                    SRC_COO_SIGMA, SRC_PRP_SIGMA, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_SRC PROGRAM SPECIFICATION
!
! 1.1 Apply (weak) constraints on source positions. It may apply constraints
!     only on global parameters ( FGLSRC_L4 = .TRUE. ) or only on local (arc)
!     parameters ( FGLSRC_L4 = .FALSE. ). Constraints may be treated as
!     global or local ( FGLOBAL_L4 = .FALSE. ) constraints.
!     NB: all parameters are considered as global in Independent mode
!
! 1.2 REFERENCES:
!
! 2.  DO_SRC INTERFACE
!
! 2.1 Parameter File
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'cnstr.i'
      INCLUDE   'fast.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
!
! 2.2 INPUT Variables:
!
      INTEGER*4   FAST_MODE, FAST_DBG
      INTEGER*2   IWDS, IPARM_ALL(IWDS,M_GPA), IPARM_GLO(IWDS,M_GPA)
      INTEGER*4   NPARM_ALL, NPARM_GLO
      LOGICAL*4   FGLOBAL_L4, FGLSRC_L4
      INTEGER*4   IUER, IER
!
! IWDS          -- Length allowed for each parameter name
! NPARM_ALL     -- Number of all parameters (local and global)
! IPARM_ALL     -- Array of all parameter names (local and global)
! NPARM_ALL     -- Number of global parameters only
! IPARM_ALL     -- Array of global parameter names
! FGLOBAL_L4    -- .TRUE. if it is called in global mode
! FGLSRC_L4     -- .TRUE. if constraints should be imposed on sources treated
!                  as global parameters.
!                  Remind: all sources are treated as global in Independent mode
!
!
! 2.3 OUTPUT Variables:
!
! A - Modified normal equations matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4   ICNS
      INTEGER*4   I, J, END_APARM,     APARM(M_GPA), &
     &                  END_APARM_GLO, APARM_GLO(M_GPA)
      LOGICAL*2   LDUM
      CHARACTER   PARMTYP*3, DUMRA*20, DUM_STANAM*8, LPAR_ALL*20, LPAR_GLO*20
      REAL*8      SRC_COO_SIGMA, SRC_PRP_SIGMA
!
! 4.  HISTORY
!   MWH  941013  Created
!   PET  970927  Support of B3D mode added. Changed constraint value (10**10
!                instead of 10**6. Nb: usual values of the diagonal terms
!                of the normal matrix for source coordinates are 10**17--10**19)
!   PET  980119  Rewrote for support of CNSTROBJ data structure
!   pet  980205  Declared the sigma of the constrain in solve.i instead of
!                hard-coded value. Write down information about the type
!                of applied constraint.
!   pet  980722  Made SRC_COO_SIGMA formal parameter instead of named
!                constant in solve.i block.
!   pet  2000.01.18  Changed the list of formal parameters and internal logic
!                    in order to support applying constraints either on
!                    only global sources or only on local sources
!   pet  2000.01.26  Corrected an error made on 2000.01.18
!   pet  2002.09.17  Changed internal logic: the new version puts equations of
!                    constraitns in CNSTROBJ, while the old version put normal
!                    equations of constraints
!   pet  2018.03.10  Fixed regression bug related to type of IPARM_ALL, IPARM_GLO
!   pet  2023.03.13  Updated support of constraints on proper motion. Added &
!                    Printing information about constraints in the spool file
!
! 5.  DO_SRC PROGRAM STRUCTURE
!
!   Set up the constraint:  it is hard-wired here . . .
!
      PARMTYP    = 'SOU'
      LDUM       = .FALSE.
!
! --- Get the list of indices of all source parameters (local and global)
!
      CALL INDEX_PARM ( PARMTYP, APARM, IPARM_ALL, NPARM_ALL, IWDS, END_APARM, &
     &                  DUMRA, LDUM, DUM_STANAM )
!
      END_APARM_GLO  = 0
      IF ( .NOT. FGLSRC_L4 ) THEN
!
! -------- Get the list of indices of global source parameters
!
           CALL INDEX_PARM ( PARMTYP, APARM_GLO, IPARM_GLO, NPARM_GLO, IWDS, &
     &                       END_APARM_GLO, DUMRA, LDUM, DUM_STANAM )
      END IF
#ifdef DEBUG
   write ( 6, * ) 'do_src-107 nparm_all = ', nparm_all, nparm_glo, ' fglsrc_l4= ', fglsrc_l4, ' end_aparm_glo= ', end_aparm_glo ; call flush ( 6 ) ! %%%
#endif
!
! --- Add the constraint
!
      ICNS = 0
      DO 410 I=1,END_APARM
         IF ( .NOT. FGLSRC_L4  .AND.  END_APARM_GLO .GT. 0 ) THEN
!
! ----------- If we have to apply constraints only on non-global parameters
! ----------- then we check: whether this parameters is on the list of global
! ----------- parameters. If yes, then we don't apply constrain on this
! ----------- parameter.
!
              CALL LIB$MOVC3 ( 20, IPARM_ALL(1,APARM(I)), LPAR_ALL )
#ifdef DEBUG
   write ( 6, * ) 'do_src-124 i= ', i, ' aparm(i)= ', aparm(i), ' lpar_all = ', lpar_all
#endif
              DO 420 J=1,END_APARM_GLO
                 CALL LIB$MOVC3 ( 20, IPARM_GLO(1,APARM_GLO(J)), LPAR_GLO )
!
! -------------- Compare names of parameters: all (local+global) against global
!
                 IF ( LPAR_ALL .EQ. LPAR_GLO ) GOTO 410
 420          CONTINUE
#ifdef DEBUG
   write ( 6, * ) 'do_src-134 i= ', i, ' lpar_all = ', lpar_all
#endif
         END IF
         ICNS = ICNS + 1
!
! ------ Add information about the type of the constraint applied
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_NAM ( 'SRC_COO', ICNS, 'Source coordinates', 'rad', &
     &                      0.0D0, SRC_COO_SIGMA, FGLOBAL_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8441, IUER, 'DO_SRC', 'Error in an '// &
     &            'attempt to put information about source position '// &
     &            'constraint' )
              RETURN
         END IF
!
! ------ Add constraint equation
!
#ifdef DEBUG
         write ( 6, * ) 'do_src-145 i= ', i, ' aparm(i) = ', aparm(i) ! %%%%
#endif
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( 'SRC_COO', ICNS, APARM(I), 1.0D0, &
     &                      FGLOBAL_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8442, IUER, 'DO_SRC', 'Error in an '// &
     &            'attempt to put information about source position '// &
     &            'constraint equations' )
              RETURN
         END IF
 410  CONTINUE
!
      IF ( FGLOBAL_L4 .AND. SRC_COO_SIGMA > 0.0D0 ) THEN
           WRITE ( 23, 210 ) ICNS/2, SRC_COO_SIGMA
 210       FORMAT ( 'SRC_COO: ', I6, ' sources participarted in SRC_COO constraints'/ &
                    'SRC_COO sigma: ', 1PD13.6, ' rad'/ )
      END IF
#ifndef DEBUG
      IF ( FAST_DBG .EQ. F__PRI ) THEN
#endif
           WRITE ( 6, * ) ' do_src        fast_mode = ',fast_mode,' n_cnstr = ', &
     &                      cnstrobj%n_equat, ' sig= ', sngl(src_coo_sigma), &
     &                     ' fglobal_l4 = ', fglobal_l4
#ifndef DEBUG
      END IF
#endif
      PARMTYP    = 'PRP'
      LDUM       = .FALSE.
!
! --- Get the list of indices of all source parameters (local and global)
!
      IF ( SRC_PRP_SIGMA > 0.0D0 .AND. FGLOBAL_L4 .AND. NPARM_GLO > 0 ) THEN
           END_APARM_GLO  = 0
           CALL INDEX_PARM ( PARMTYP, APARM_GLO, IPARM_GLO, NPARM_GLO, IWDS, &
     &                       END_APARM_GLO, DUMRA, LDUM, DUM_STANAM )
!
! -------- Add the constraint
!
           ICNS = 0
           DO 430 I=1,END_APARM_GLO
              ICNS = ICNS + 1
!
! ----------- Add information about the type of the constraint applied
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( 'SRC_PRP', ICNS, 'Source proper motion', 'rad/s', &
      &                          0.0D0, SRC_PRP_SIGMA, FGLOBAL_L4, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8443, IUER, 'DO_SRC', 'Error in an '// &
      &                'attempt to put information about source position '// &
      &                'constraint' )
                   RETURN
              END IF
!
! ----------- Add constraint equation
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( 'SRC_PRP', ICNS, APARM_GLO(I), 1.0D0, &
     &                           FGLOBAL_L4, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8444, IUER, 'DO_SRC', 'Error in an '// &
     &                 'attempt to put information about source position '// &
     &                 'constraint equations' )
                   RETURN
              END IF
 430       CONTINUE
      END IF
!
      IF ( FGLOBAL_L4 .AND. SRC_PRP_SIGMA > 0.0D0 ) THEN
           WRITE ( 23, 220 ) ICNS/2, SRC_PRP_SIGMA
 220       FORMAT ( 'SRC_PRP: ', I6, ' sources participarted in SRC_PRP constraints'/ &
                    'SRC_PRP sigma: ', 1PD13.6, ' rad/s'/ )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_SRC  #!#
