      SUBROUTINE SITPL(ISITN,NUMSTA,PLATES)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SITPL PROGRAM SPECIFICATION
!
! 1.1 Find tectonic plate for each site.
!
! 1.2 REFERENCES:
!
! 2.  SITPL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISITN(4,*),NUMSTA
!
! ISITN - Array containing site names
! NUMSTA - Number of sites to be looked up
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*4 PLATES(*)
!
! PLATES - Plate names corresponding to sites in ISITN
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  M__STA
      PARAMETER  ( M__STA = 4096 )
      CHARACTER*4 PLNM(M__STA)
      CHARACTER*8 KSTN_CHR(M__STA)
      INTEGER*2 KSTN(4,M__STA)
      INTEGER*2 I,J,ITOTAL
      INTEGER*4  IOS
      CHARACTER  ERRSTR*290, FNAME*128
      EQUIVALENCE (KSTN,KSTN_CHR)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! I,J - Loop indices
! ITOTAL - Number of sites in look-up table
! KSTN,KSTN_CHAR - Table of site names
! PLNM - Table of plate names corresponding to sites in KSTN
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MK   870427  Created
!   JWR  871006  Additional sites added
!   JWR  880211  MOJ 7287, OVR 7853, and DSS15 added.
!   CMA  881013  MOJ 7288; 7287 not actually occupied.
!   JWR  890221  Number of sites made a parameter and
!                sites 61-67 added.
!   JWR  890321  Error message corrected
!   MWH  900119  Added sites 75-81
!   MWH  900207  Added sites 82-83
!   MWH  900511  Added sites 84-90
!   MWH  901217  Added site 91
!   MWH  910111  Corrected "KASH 34" to "KASHIM34"
!   DSC  910301  Added site 95, MARCUS
!   jwr  910301  OVRO 90 added.
!   AEE  910515  Enhanced error messages written to the error file.
!   MWH  910709  Added sites 97-108
!   MWH  911113  Added sites 109-111
!   MWH  911223  Added site 112
!   AEE  920103  Added site 113
!   BAA  921104  Added "NRAO85 1" as site 122.
!   BAA  921119  Added "LEFT85 1" as site 123, "PARKES  " as site 124.
!                Changed dimensions from "1" to "*" for f77 -C use.
!   MWH  930513  Put site/plate list in a file
!   PET  1999.11.01  Improved error messages, replaced MAX_SITES with MAX_STA
!   PET  2007.11.15  Added support of the environment variable SOLVE_SITPL
!   PET  2021.07.30  replaced MAX_STA with M__STA
!
! 5.  SITPL PROGRAM STRUCTURE
!
!     Open and read list of sites/plates
!
      ITOTAL = 0
      CALL GETENVAR ( 'SOLVE_SITPL', FNAME )
      IF ( ILEN(FNAME) == 0 ) THEN
           FNAME = PRE_SAV_DIR(:PRE_SV_LEN)//SITPL_FIL
      END IF
      OPEN ( 67, FILE=FNAME, IOSTAT=IOS )
      CALL FERR( INT2(IOS), "Opening SITPL file "//FNAME, INT2(0), INT2(0) )
!
      do while (.TRUE.)
        itotal = itotal+1
        if (itotal.gt.M__STA) then
          call ferr( INT2(122), 'Too many sites in SITPL file '//FNAME, &
     &         INT2(0), INT2(0) )
        endif
        read(67,'(a8,3x,a4)',END=50, &
     &     IOSTAT=ios)kstn_chr(itotal),plnm(itotal)
        if (ios.ne.0) call ferr( INT2(ios), "reading SITPL list from file "//FNAME, &
     &   INT2(0), INT2(0) )
      enddo
50    continue
      close(67,IOSTAT=ios)
      call ferr( INT2(ios), "Closing "//FNAME, INT2(0), INT2(0) )
      itotal = itotal-1
!
!     Find plate name for each site
!
      DO 100 I=1,NUMSTA
         PLATES(I)='    '
         DO 200 J=1,ITOTAL
            IF ( ISITN(1,I) .EQ. KSTN(1,J) .AND. &
     &           ISITN(2,I) .EQ. KSTN(2,J) .AND. &
     &           ISITN(3,I) .EQ. KSTN(3,J) .AND. &
     &           ISITN(4,I) .EQ. KSTN(4,J)       ) THEN
                 PLATES(I) = PLNM(J)
            END IF
 200     CONTINUE
!
         IF ( PLATES(I) .EQ. '    ' ) THEN
              WRITE ( ERRSTR, '( '// &
     &           '" Site ",4A2," missing in list of sites and their tectonic", '//&
     &           '" plates. Add to list in ",A)' ) &
     &           (ISITN(J,I),J=1,4), FNAME
              CALL FERR (  INT2(216), ERRSTR, INT2(0), INT2(0) )
         ENDIF
  100 CONTINUE
!
      RETURN
      END
