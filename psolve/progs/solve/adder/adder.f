      PROGRAM ADDER
      IMPLICIT NONE
!
! 1.  ADDER PROGRAM SPECIFICATION
!
! 1.1 Add names to the parameter list
!
! 1.2 REFERENCES:
!
! 2.  ADDER INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'addcm.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
      CHARACTER SAVNAM*(NAME_SIZE)
      INTEGER*4 FILDES
      INTEGER*2 IDIRECT(BLOCK_WORDS), IOCGM_SAVE
      COMMON/SAVCGM/FILDES,IDIRECT
      COMMON/NAMCGM/SAVNAM
      SAVE /SAVCGM/,/NAMCGM/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: setup,lists,names
!
! 3.  LOCAL VARIABLES
      INTEGER*2   TRIMLEN
      CHARACTER   OUTNAM*(NAME_SIZE)
      INTEGER*4   JBLOCKS, JRND_BLOCKS, MAT_E
      INTEGER*8   NELEM, MATSIZE, LEN8_BYTES, LEN8_BLOCKS
      INTEGER*2   NP
      INTEGER*4   I4P1, I4P255, I4P256
      DATA        I4P1, I4P255, I4P256 / 1, 255, 256 /
!
!      REAL*8      ARR1, ARR2
!      POINTER ( PTR1, ARR1 )
!      POINTER ( PTR2, ARR2 )
      INTEGER*8    ADDR_ARR1, ADDR_ARR2
!CCCCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  951130  10/18/96  20:39:49 accidentally replaced with an actual sccs
!                version. Restore it.
!   pet  970228  Made bypass ADDER in the case of fast global algorithms.
!                Added timing profiling.
!   pet  980403  Corrected a bug: IFBE should be declared as INTEGER*4, not
!                INTEGER*2 !!
!   pet  980909  Corrected a bug: added setting ARCPE_WORKED flag.
!   pet  990406  Added a call of TIM_GET
!   pet  2001.05.10  pet  Increased amount of memory allocated for CGM
!                    by 256 bytes since it is read and written by
!                    256-bytes-long blocks.
!   pet  2001.05.10  pet  Added logic which prohibts writing the output CGM
!                         if its name is 'NONE'
!
! 5.  ADDER PROGRAM STRUCTURE
!
!CCCCC
!
      CALL PRE_PROG()
      INCLUDE 'adder_version.i' ! Set revision date of the current version
      CALL USE_GLBFIL_4 ( 'ORC' )
!
! --- Set timer
!
      CALL TIM_INIT()
!
! --- Set up guidelines for the task at hand
!
      CALL SETUP()
!
      IF (   ADORSB(1:3) .EQ. 'ADD'  .AND. &
     &     ( FAST_MODE .EQ. F__B1D   .OR.   FAST_MODE .EQ. F__B1B3D ) ) THEN
!
! ---------- In the case of F__B1D or F__B1B3D we have done all deal in ARCPE
! ---------- and therefore we don't need more ADDER
!
             CALL END_PROG()
             STOP
      END IF
!
! --- Build complete list of parameters, including additions
!
      CALL USE_GLBFIL ( 'ORC' )
      CALL LISTS_CGM()
!
! --- Generate and save name of output CGM file
!
      IOCGM_SAVE = IOCGM
      IF ( OUTCGM(1:4) .EQ. 'NONE'  .AND. &
     &     TRIMLEN(OUTCGM).EQ. 4    .AND. &
     &     IOCGM_SAVE .EQ. 2               ) IOCGM = 1
!
      CALL NAMES_CGM ( OUTNAM )
!
      IF ( OUTCGM(1:4) .EQ. 'NONE'  .AND. &
     &     TRIMLEN(OUTCGM).EQ. 4    .AND. &
     &     IOCGM_SAVE .EQ. 2               ) IOCGM = IOCGM_SAVE
!
! --- dimension arrays for doing the addition
!
      NP = MAX ( NPARMF, NPARMT, NPARMC )
!!      NELEM=I4P1+I4P256*((MAT_E(MAX_PAR,NP)+I4P255)/I4P256) + I4P256
!!       MATSIZE = NELEM*8
      LEN8_BYTES  = 8*(3*M_GPA + INT8(NP)*INT8(NP+1)/2)
      LEN8_BLOCKS = (LEN8_BYTES + 255)/256
!
! --- Convert to number of bytes (8 per real*8 element)
!
      MATSIZE = LEN8_BYTES
      CALL GRAB_MEM ( INT8(MATSIZE), ADDR_ARR1 )
      CALL GRAB_MEM ( INT8(MATSIZE), ADDR_ARR2 )
      CALL XDDER ( %VAL(ADDR_ARR1), %VAL(ADDR_ARR2) )
!
      IF ( IOCGM .EQ. 2  .AND. &
     &   .NOT. ( OUTCGM(1:4) .EQ. 'NONE'  .AND.  TRIMLEN(OUTCGM).EQ. 4 ) ) THEN
!!           JBLOCKS = JRND_BLOCKS(MAT_E(MAX_PAR,NPARMF)*REALL_WORDS)
!!           JBLOCKS = JBLOCKS+JSOCOM_BLOCKS+JPARFIL_BLOCKS+JPLIST_BLOCKS+1
           LEN8_BLOCKS = LEN8_BLOCKS + JBLOCKS + JSOCOM_BLOCKS + JPARFIL_BLOCKS + JPLIST_BLOCKS + 1
           CALL COPY_FILE ( SAVNAM(:TRIMLEN(SAVNAM)), &
     &                      OUTNAM(:TRIMLEN(OUTNAM)), 'Q', INT(LEN8_BLOCKS,KIND=4) )
           ONAMCG = OUTNAM
           INAMCG = OUTNAM
      ENDIF
!
! --- Set flag that ARCPE has processed at least one arc
!
      ARCPE_WORKED = .TRUE.
      CALL USE_GLBFIL ( 'OWC' )
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Timing printout
!
           CALL TIM_GET ( 'ADDER' )
      END IF
      CALL END_PROG()
!
      END !#!  ADDER  #!#
