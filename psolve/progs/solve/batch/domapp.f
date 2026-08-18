      SUBROUTINE DOMAPP ( EOPMOD )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DOMAPP PROGRAM SPECIFICATION
!
! 1.1 Set up mapping.
!
! 1.2 REFERENCES:
!
! 2.  DOMAPP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER EOPMOD*(*)
!
! eopmod - Y      = Eop mapping
!          others = Not eop mapping
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
      INCLUDE 'precm.i'
      INCLUDE 'dmapp.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
      LOGICAL*2  KBIT
      REAL*8     TIME0X
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910307 Added VELMAP (station velocities)
!   MWH  930222  Added STAMAP2 ('old' station positions)
!   PET  990301  Improved comments
!   PET  1999.10.15  Added ECCMAP     to the list of arguments for FLYBY_INIT
!   PET  2000.09.22  Added MGRMAP     to the list of arguments for FLYBY_INIT
!   PET  2001.01.12  Added METRIC_MAP to the list of arguments for FLYBY_INIT
!   PET  2002.10.02  Fixed a bug: the previous version overwrote variable
!                    TIME0. As a result station reference epoch for estimates
!                    was always set to the value from the reference epoch of
!                    the apriori mapping velocity catalgoue
!
! 5.  DOMAPP PROGRAM STRUCTURE
!
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) CALL START_MN
      TIME0X = TIME0
      IF ( EOPMOD .EQ. 'Y' ) THEN
           CALL FLYBY_INIT ( STAMAP, SRCMAP, NTMMAP, NTSMAP, EOPMAP, PLTMAP, &
     &          TIME0X, VELMAP, STAMAP2, SRCMAP2, VELMAP2, PLTMAP2, PLATE_SCALE, &
     &          AXOMAP, ECCMAP, MGRMAP, METRIC_MAP )
        ELSE
           CALL FLYBY_INIT ( STAMAP, SRCMAP, NTMMAP, NTSMAP, 'NONE', PLTMAP, &
     &          TIME0X, VELMAP, STAMAP2, SRCMAP2, VELMAP2, PLTMAP2, PLATE_SCALE, &
     &          AXOMAP, ECCMAP, MGRMAP, METRIC_MAP  )
      ENDIF
!
      STASUB_CHR  = STAMAP
      SRCSUB_CHR  = SRCMAP
      NUTSRS_CHR  = NTMMAP
      NUTDLY_CHR  = NTSMAP
      EOPDLY_CHR  = EOPMAP
      PLTMOD_CHR  = PLTMAP
      VELSUB_CHR  = VELMAP
      STASUB2_CHR = STAMAP2
      VELSUB2_CHR = VELMAP2
      AXOSUB_CHR  = AXOMAP
      ECCSUB_CHR  = ECCMAP
      MGRSUB_CHR  = MGRMAP
      METSUB_CHR  = METRIC_MAP
      PLTSUB2_CHR = PLTMAP2
      PLATE_SCL   = PLATE_SCALE
!
      TIME0 = TIME0X ! ???????????
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) CALL END_MN
      CALL USE_GLBFIL ( 'OWC' )
!
      RETURN
      END  !#!  DOMAPP  #!#
