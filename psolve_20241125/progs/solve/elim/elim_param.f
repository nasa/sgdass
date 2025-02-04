      SUBROUTINE ELIM_PARAM ( DBOBJ, ICMPL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ELIM_PARAM  makes test of parameterization used for       *
! *   ELIM/MILE. If some unsuprted parameters are to be estimated,       *
! *   Error message is generated and ICMPL=1.                            *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *   ICMPL ( INTEGER*4 ) -- Completion code.                            *
! *                          0 -- parameterization is supported by ELIM. *
! *                          1 -- parameterization is not supported by   *
! *                               ELIM/MILE.                             *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  12-JUL-98   ELIM_PARAM   v1.0  (c)  L. Petrov  12-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'obser.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*4  ICMPL, IUER
      INTEGER*2  ILPAR_LEN2
      INTEGER*4  NPARAM2
      PARAMETER ( ILPAR_LEN2 = 20 )
      CHARACTER  LPARM(M_GPA)*(ILPAR_LEN2)
      LOGICAL*2  KSHORT, KGLOBAL
      INTEGER*4  MPAR
      PARAMETER  ( MPAR = 3 )
      CHARACTER  PAR_LIST(MPAR)*8
      DATA PAR_LIST / &
     &                'X WOBBLE', &
     &                'Y WOBBLE', &
     &                'UT1-TAI ' &
     &              /
      INTEGER*4  J1
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      KSHORT  = .TRUE.
      KGLOBAL = .FALSE.
      CALL GET_NAMES ( LPARM, ILPAR_LEN2, M_GPA, NPARAM2, KSHORT, KGLOBAL )
!
      ICMPL = 0
      IF ( KUSER_PART ) THEN
           CALL ERR_LOG ( 6681, IUER, 'ELIM_PARAM', 'ELIM doesn''t support '// &
     &         'user defined partials. Please reparameterize your solution' )
           ICMPL=1
           RETURN
      END IF
!
      DO 410 J1=1,NPARAM2
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ELIM_PARAM  #!#
