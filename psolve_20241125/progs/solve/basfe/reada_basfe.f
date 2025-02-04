      SUBROUTINE READA_BASFE ( XOFFST, MONAM, MOTYPE, ADR_ARR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  READA_BASFE PROGRAM SPECIFICATION
!
! 1.1 Open the archive scratch file and read record counter.
!
! 1.2 REFERENCES:
!
! 2.  READA_BASFE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 MONAM(5,MAX_STA),MOTYPE(MAX_STA)
      REAL*8 XOFFST(3,MAX_STA)
!
! MONAM - Array of monument names for each station
! MOTYPE - Array of monument types for each station
! XOFFST - Array of axis offsets for each station
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'buff4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: basfe
!       CALLED SUBROUTINES: lvect,rnamf
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4        MAT_E
      INTEGER*8        MATSIZE
      ADDRESS__TYPE :: ADR_ARR
      INTEGER*4 I4P255, I4P256
      DATA      I4P255, I4P256 / 255, 256 /
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4, LOC
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   911219 changed call use_buffer to basfe_file, since BUFF1_WORDS
!                is much larger than use_buffer can handle. Instead of using
!                pipes, we write to and read from a file.
!   pet   980415 added expansion of socom to socom_plus
!
! 5.  READA_BASFE PROGRAM STRUCTURE
!
! --- READ THE BUFFER FROM ADJST
!
      CALL BASFE_FILE ( BUFBUF4, BUFF4_WORDS, 'ORC' )
      IF ( NSCNT1 .EQ. 0 ) RETURN
!
! --- Read the a matrix from NRMFIL
!
      IF ( .NOT. KGLOBALS ) THEN
           CALL USE_COMMON('ORC' )
!
! -------- Expansion socom to socom_plus
!
           CALL SOCOM_EXT()
           CALL USE_PARFIL('ORC' )
!!           matsize = i4p256* ( ( mat_e(max_par,nparam)+i4p255)/i4p256)
           MATSIZE = 8*MAT_E4 ( M_GPA, NPARAM )
           CALL GET_MEM ( MATSIZE, ADR_ARR )
           CALL USE_NRMFIL  ( %VAL(ADR_ARR), INT4(ISCNT(1,ABS(NSCNT1))), 'ORC' )
         ELSE
           CALL ACS_COVFIL('O' )
           CALL USE_COVF_COM('R' )
!!           matsize = i4p256* ( (mat_e(max_par,nparam)+i4p255)/i4p256)
           MATSIZE = 8*MAT_E4 ( M_GPA, NPARAM )
           CALL GET_MEM ( MATSIZE, ADR_ARR )
           CALL USE_COVF_MAT ( %VAL(ADR_ARR), INT4(ISCNT(1,ABS(NSCNT1))), 'R' )
           CALL ACS_COVFIL('C' )
      ENDIF
!
! --- Get apriori site location data
! --- Monument file is read to get monumnet data
! --- PARFIL is read to get apriori site data (and flyby values
! --- are applied to the apriori site data, if the values exist)
!
! --- Get flyby values & monument file name
!
      IF ( .NOT. KGLOBALS ) THEN
            CALL USE_GLBFIL   ( 'OR' )
            CALL USE_GLBFIL_4 ( 'RC' )
            CALL FLYBY_APRIOR()
         ELSE
            CALL USE_GLBFIL ( 'ORC' )
      ENDIF
!
      RETURN
      END  !#!  READA_BASFE  #!#
