      FUNCTION   FAST_BYPASS ( IUER )
! ************************************************************************
! *                                                                      *
! *     Hatch (kludge) for bypassing fast way. If the session marked as  *
! *   uneligible to fast algorithm then by default parameter FAST_MODE   *
! *   will be changed (only for this session!). If envoronment variable  *
! *   FAST_MODE__STRICTLY is specified and the first letter of their     *
! *   value is "y" or "Y", then such substitution will be supressed and  *
! *   the value of variable FAST_MODE will be conserved.                 *
! *                                                                      *
! *     FAST_BYPASS tests variables FAST_xxx_GLO before others affairs.  *
! *   If FAST_xxx are not equal FAST_xxx_GLO, then FAST_BYPASS firstly   *
! *   Assign FAST_xxx the values FAST_xxx_GLO. Such a trick allows to    *
! *   resurrect original values of FAST_xxx variables if they were       *
! *   changed for the previous session.                                  *
! *                                                                      *
! *     FAST_BYPASS also tests environment variable FAST_DBG. If it      *
! *  setup then it is read, parsed and FAST_BYPASS substitutes previous  *
! *  value   of FAST_DBG.                                                *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *     TEST ( LOGICAL*4 ) -- Will make tests of eligibility of this     *
! *                           session for FAST algorithms.               *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *      NB: It is assumed that commmon area glbc4 and socom have been   *
! *          read earlier. If not then FAST_BYPASS could give abnornal   *
! *          termination.                                                *
! *                                                                      *
! *  ###  29-JAN-97   FAST_BYPASS  v4.1  (c)  L. Petrov  13-MAY-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      INTEGER*4  FAST_BYPASS, IUER
      LOGICAL*4  TEST
      INTEGER*4  J1, J2, IER, I_LEN
      CHARACTER  STR*24
      LOGICAL    NEED_UPDATE
!
      NEED_UPDATE = .FALSE.
!
      IF ( SOCOM_PLUS_FIRST .NE. SPL__DONE ) THEN
           CALL ERR_LOG ( 8591, IUER, 'FAST_BYPASS', 'socom_plus has not '// &
     &         'initialized. It is fatal error' )
           RETURN
      END IF
!
! --- Test: is it necessary to resurrect FAST_xxx variables?
!
      IF ( FAST_MODE_GLO .NE. FAST_MODE ) THEN
           FAST_MODE = FAST_MODE_GLO
           NEED_UPDATE = .TRUE.
      END IF
      IF ( FAST_DBG_GLO .NE. FAST_DBG ) THEN
           FAST_DBG = FAST_DBG_GLO
           NEED_UPDATE = .TRUE.
      END IF
      IF ( FAST_COV_GLO .NE. FAST_COV ) THEN
           FAST_COV = FAST_COV_GLO
           NEED_UPDATE = .TRUE.
      END IF
!
! --- Getting value of FAST_DBG_BYPASS
!
      CALL CLRCH  (  STR )
      CALL GETENVAR ( 'FAST_DBG__BYPASS', STR )
      IF ( STR(1:1) .NE. ' ' ) THEN
           FAST_DBG = F__UND
           DO 410 J1=1,FD_VAR
              IF ( STR(1:3) .EQ. FD_ABR(J1) ) FAST_DBG = FD_VAL(J1)
 410       CONTINUE
!
           IF ( FAST_DBG .EQ. F__UND ) THEN
                CALL ERR_LOG ( 8592, IUER, 'FAST_BYPASS', 'Unrecognized '// &
     &              'value of environmnet variablre FAST_DBG__BYPASS: "'// &
     &               STR(1:I_LEN(STR))//'". See the tail of file '// &
     &              '$PSOLVE_ROOT/include/fast.i for list of acceptable values' )
                FAST_DBG = F__UND
                FAST_BYPASS = FAST_MODE
                RETURN
           END IF
           NEED_UPDATE = .TRUE.
      END IF
!
! --- Testing FAST_MODE__STRICTLY
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'FAST_MODE__STRICTLY', STR )
      IF ( STR(1:1) .NE. 'Y'  .AND.  STR(1:1) .NE. 'y' ) THEN
         IF ( .NOT. FAST_ELIG ) THEN
              FAST_MODE   = F__NONE
              NEED_UPDATE = .TRUE.
         END IF
      END IF
!
! --- Updating  glbc4 if needed
!
      IF ( NEED_UPDATE ) THEN
           CALL USE_GLBFIL_4 ( 'OWC' ) ! Writing glbc4.i
      END IF
!
      FAST_BYPASS = FAST_MODE
      CALL ERR_PASS ( 0, IUER )
!
      RETURN
      END  !#!  FAST_BYPASS  #!#
