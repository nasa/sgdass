      SUBROUTINE UPCAT(NAMCGM,dir,LETRS,SOLUID,TEST)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  UPCAT PROGRAM SPECIFICATION
!
! 1.1 Update the CGM catalog by adding the name of the CGM
!     stored in NAMCGM with the letters stored in PRE_ILETRS.
!
! 1.2 REFERENCES:
!
! 2.  UPCAT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TEST,NAMCGM,DIR,LETRS,SOLUID
!
! DIR - Directory where the CGM file is stored
! LETRS - Initials associated with the CGM
! NAMCGM - Name of the CGM file
! SOLUID - Solution identification string
! TEST - If 'TEST' or 'TSCO', write to TSTCAT instead of CGMCAT
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: opchk
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   IERR
      INTEGER*4   ISTAT
      CHARACTER   NAMCG*14, CATNAMR*128
!
! CATNAMR - Path/name of the catalog to be updated
! IERR - Error return from OPCHK
! ISTAT - IOSTAT return from READ
! NAMCG - CGM file names read from catalog
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!   KDB   991015 Add "TSCO" ("test catalog only") option
!                (That is, in this subroutine, access a test cgm
!                catalog on the solve scratch directory, but put the output
!                cgm itself on the standard cgm directory given by the cgm_dir
!                parameter, in the batch/ctrls and cutil/create_cgmf routines.)
!                The test catalog will be wiped out overnight in some
!                installations, so this potentially will create orphan cgms,
!                unless the user makes sure to manually delete these.
!
!                This option is being added for the support of automatic
!                eops extensions to ivs.
!
! 5.  UPCAT PROGRAM STRUCTURE
!
!     OPEN THE CGM CATALOGUE FILE.
!
      IF(TEST.NE.'TEST'.AND.TEST.NE.'TSCO') THEN
        CATNAMR=PRE_SAV_DIR(:PRE_SV_LEN)//'CGMCAT'
      ELSE
        CATNAMR=SCRATCH_DIR//'TSTCAT'
      ENDIF
      CALL OPCHK( INT2(65), CATNAMR, IERR )
      IF(IERR.NE.0) THEN
          call ferr( INT2(220), 'Unexpected IERR from OPCHK (CATNAMR)', &
     &         INT2(0), INT2(0) )
      END IF
!
!     READ THROUGH THE CGM CATALOGUE UNTIL AN EOF IS FOUND.
!
  110 CONTINUE
      READ(UNIT=65,   FMT=9002,IOSTAT=ISTAT,ERR=111,END=115) NAMCG
      GOTO 110
!
111   CONTINUE
      call ferr( INT2(221), 'Error reading the CGM CATALOUGE', INT2(0), &
     &     INT2(0) )
!
115   CONTINUE
!
!     WRITE OUT THE RECORD TO THE CATALOGUE FILE.
!
      WRITE(UNIT=65,   FMT=9002, &
     &   IOSTAT=istat)NAMCGM,LETRS,SOLUID,DIR
      call ferr( INT2(istat), "Writing CGM catalogue", INT2(0), INT2(0) )
 9002 FORMAT(A14,1X,A2,1X,A60,2X,A50)
      CLOSE(65,IOSTAT=istat)
      call ferr( istat, "Closing CGM catalogue", INT2(0), INT2(0) )
!
      RETURN
      END
