      SUBROUTINE NAMES_CGM ( OUTNAM )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  NAMES_CGM PROGRAM SPECIFICATION
!
! 1.1 Generate and save the name of the CGM file in the forward
!     direction:  not necessary for back solutions.
!
! 1.2 REFERENCES:
!
! 2.  NAMES_CGM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'addcm.i'
      INCLUDE 'precm.i'
      CHARACTER*(NAME_SIZE) SAVNAM
      INTEGER*4 FILDES
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      COMMON   / SAVCGM /    FILDES, IDIRECT
      COMMON   / NAMCGM /    SAVNAM
      SAVE     / SAVCGM /, / NAMCGM /
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: adder
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER  CNAME*(NAME_SIZE), OUTNAM*(NAME_SIZE)
      CHARACTER  SID*60, BUFSTR*80, ERRSTR*255
      INTEGER*2  I, J, IL2, CREATE_CGMF
      LOGICAL*2  KBIT
!CCCCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb 10/02/95 Fix bug in which adder wouldn't write the axis offsets to
!                the cgm as global parameters unless at least one other
!                global site parameter was turned on.
!   pet 12/10/97 Added call LIST_DESELECT when the first database is migrating
!                to CGM
!   pet 12/16/97 Improved error message
!   pet 01/07/99 Set "USE_GLBFIL ( 'OWC' )" instead of "USE_GLBFIL ( 'WC' )"
!   pet 1999.06.07  Enhanced error message about failure to create CGM
!
! 5.  NAMES_CGM PROGRAM STRUCTURE
!
!CCCCC
      IF(STDALN) IOCGM=2
!
! --- Read in the comment field for the catalogue record.
!
      IF ( (PRE_IBATCH.EQ.0 .OR. STDALN ) .AND. ( IOCGM.EQ.2 ) ) THEN
           CALL START_MN()
           CALL &
     &          ADDSTR_F("COMMENT FOR THIS CGM ....."// &
     &         ".................................." )
           CALL NL_MN()
           CALL GETSTR_F ( BUFSTR )
           READ ( BUFSTR, 9001 ) SOLUID
 9001      FORMAT ( 30A2 )
           CALL END_MN()
      ENDIF
!
! --- Specify the CGM file name
!
      IF ( IOCGM .EQ. 2 ) then
           CALL HOL2CHAR( SOLUID, INT2(1), INT2(60), SID )
           CNAME = 'SAVE'
           IL2 = CREATE_CGMF ( CNAME, SID, NPARMF, 'M', OUTCGM )
           IF ( IL2 .NE. 0 ) THEN
                CALL FERR ( INT2(9001), 'NAMES_CGM: errors from CREATE_CGMF', &
     &               INT2(0), INT2(0) )
           END IF
           OUTNAM = SAVNAM
      ENDIF
      IF ( NPARMF .GT. NRMFL_PARMS ) THEN
           WRITE ( UNIT=ERRSTR, FMT='("NAMES_CGM: The size of  CGMFxx ", &
     &           "file appeared to be unsuffitient. CGM matrix with ", &
     &           "dimension ",I5, " is to be written, but the file was ", &
     &           "sized to keep the matrix with dimension no more than ",I5)' ) &
     &           NPARMF, NRMFL_PARMS
           CALL FERR ( INT2(9002), ERRSTR, INT2(0), INT2(0) )
      END IF
      CNAME = 'SCRATCH'
      IL2   = CREATE_CGMF ( CNAME, ' ', NRMFL_PARMS, 'U', ' ' )
      IF ( IL2 .NE. 0 ) THEN
           CALL FERR ( INT2(9003), 'NAMES_CGM: errors from CREATE_CGMF', &
     &          INT2(0), INT2(0) )
      END IF
      IONAM=CNAME
!
! --- Write out the common to the CGM file
!
      CALL PARCNG()
!
! --- If no station parameters are turned on, and this is the last arc,
! --- then get rid of station list   (mwh 930903)
!
      IF ( SLAST ) THEN
           DO I=1,NUMSTA
!
              DO J=1,3
                 IF (KBIT(LSITEC(1,J),I).OR.KBIT(LSITEV(1,J),I)) GOTO 100
              ENDDO
              IF (KBIT(LAXOF,I)) GOTO 100
           ENDDO
           NUMSTA = 0
           TOTSTA = 0
 810       CONTINUE
      ENDIF
!
100   CONTINUE
      IF ( CGMINN(1:1) .EQ. ' ' ) THEN
!
! -------- If the first session is migrating to the the CGM we should check:
! -------- does socom and prfile contains dseslected stations and source.
! -------- If so, LIST_DESLECT squeeze the list.
!
           CALL LIST_DESELECT()
      END IF
      CALL USE_CGMF_COM ( 'W' )
      CALL ACS_CGMFIL   ( CNAME, 'C' )
!
! --- Save the cgm name in GLBCM
!
      IF ( .NOT. STDALN ) THEN
           ONAMCG=IONAM
           TGLBLS=NPARMF
           CALL USE_GLBFIL ( 'OWC' )
      ENDIF
!
      RETURN
      END  !#!  NAMES_CGM  #!#
