      SUBROUTINE LISTS_UPDATE ( GLBMEM, NPARM_GLO, LPARM_GLO, NPARM_CGM, &
     &           LPARM_CGM, NPARM_NEW, LPARM_NEW, IXGTC, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine LISTS_UPDATE  update the list of global          *
! *   parameters transferred to CGM. It creates the list of new          *
! *   parameters LPARM_NEW. It creates a of cross reference table IXGTC  *
! *   between the list of global parameters and the updated list of new  *
! *   CGM parameters. It updates CGM-type common area socom and prfil    *
! *   for including new parameters into CGM.                             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    GLBMEM ( RECORD    ) -- Data structure which keeps addresses of   *
! *                            CGM, list of global parameters, global    *
! *                            socom, prfil and temporary normal         *
! *                            matrices. Defined in ../include/glbp.i    *
! * NPARM_GLO ( INTEGER*4 ) -- Number of global parameters.              *
! * LPARM_GLO ( INTEGER*4 ) -- The list of names of global parameters.   *
! *                            Dimension of the list is NPARM_GLO.       *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * NPARM_CGM ( INTEGER*4 ) -- Number of parameters in CGM after update. *
! * LPARM_CGM ( CHARACTER ) -- The list of names parameters in CGM after *
! *                            update. Dimension: NPARM_CGM.             *
! * NPARM_NEW ( INTEGER*4 ) -- Number of new parameters added to the     *
! *                            list of parameters in CGM after           *
! *                            processing this session. CGM didn't have  *
! *                            these parameters before this session.     *
! * LPARM_NEW ( CHARACTER ) -- The list of names of new parameters in    *
! *                            CGM after update. Dimension: NPARM_NEW.   *
! *     IXGTC ( INTEGER*4 ) -- Cross reference table from global         *
! *                            parameters to the CGM paramter. If        *
! *                            IXGTC(k) = l it means that k-th parameter *
! *                            in the lost of global parameters          *
! *                            corresponds to the l-th parameter in the  *
! *                            CGM.                                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  05-JAN-1999  LISTS_UPDATE  v1.2 (c) L. Petrov 10-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'q_socom.i'
      INCLUDE   'q_prfil.i'
      INCLUDE   'glbp.i'
!
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      INTEGER*4  IUER
      INTEGER*4  NPARM_CGM, NPARM_GLO, NPARM_NEW, IXGTC(M_GPA)
      CHARACTER  LPARM_CGM(M_GPA)*(*), LPARM_GLO(NPARM_GLO)*(*), &
     &           LPARM_NEW(M_GPA)*(*)
      CHARACTER  STR*32, STR1*32
      INTEGER*4  J1, J2, KP, IP, L_CGM
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF
!
      IF ( NPARM_GLO .EQ. 0 ) THEN
!
! -------- No global parameters in that session? Nothing to update. Come back!
!
           NPARM_NEW = 0
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      L_CGM = NPARM_CGM
!
! --- Keep on disk current arc-type socom and parfil
!
      CALL USE_COMMON ( 'OWC' )
      CALL USE_PARFIL ( 'OWC' )
!
! --- Set flags off for parameters which cannot potentially be moved to CGM
!
      CALL DEPAR()
!
! --- Modify the common block PARFL to agree with the current
! --- flyby a prioris. Care must be taken not to write PARFL
! --- out to disk after calling this routine.
!
      CALL FLYBY_APRIOR()
!
! --- Moving ARC-type to socom and parfil to q_ blocks
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  PI_VAR,   Q_PI_VAR   )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, VAXOF(1), Q_VAXOF(1) )
!
      IF ( L_CGM .GT. 0 ) THEN
!
! ------- Moving CGM-type socom and prfile from glbp to socom and parfil
!
          CALL LIB$MOVC3 ( JSOCOM_BYTES,  %VAL(GLBMEM%ADR_GLO_SOCOM), PI_VAR   )
          CALL LIB$MOVC3 ( JPARFIL_BYTES, %VAL(GLBMEM%ADR_GLO_PRFIL), VAXOF(1) )
      END IF
      CGM_TYPE = .TRUE.
!
! --- Turn off flags for arc parameters in preparation for building
! --- list of globals only
!
      CALL DEPAR()
!
! --- Add sources and stations to the q_ socom and q_ prfil
!
      CALL ADPAR()
      IF ( KUSER_PART ) CALL UPDATE_USERP()
!
! --- Moving CGM-type socom and prfile to glbp
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  PI_VAR,   %VAL(GLBMEM%ADR_GLO_SOCOM) )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, VAXOF(1), %VAL(GLBMEM%ADR_GLO_PRFIL) )
!
! --- Now restoring socom and parfil (for what?)
!
      CALL USE_COMMON ( 'ORC' )
      CALL USE_PARFIL ( 'ORC' )
!
! --- Now update the list of parameters of CGM
!
      IF ( L_CGM .EQ. 0 ) THEN
!
! -------- CGM was empty. Merely copy there the list of global parameters
!
           L_CGM = NPARM_GLO
           CALL LIB$MOVC3 ( L__GPA*L_CGM, LPARM_GLO, LPARM_CGM )
           CALL LIB$MOVC3 ( L__GPA*L_CGM, LPARM_GLO, LPARM_NEW )
           NPARM_NEW = NPARM_GLO
         ELSE
!
! -------- Now check the CGM list of parameters for each parameter from this
! -------- session which will be moved to CGM. If it is a new parameter then
! -------- we increase the list of CGM
!
           KP = 0
           DO 410 J1=1,NPARM_GLO
              IP = LTM_DIF ( 1, L_CGM, LPARM_CGM, LPARM_GLO(J1) )
              IF ( IP .LE. 0 ) THEN
!
! ---------------- J1-th parameter was not found in CGM
!
                   L_CGM = L_CGM + 1
                   IF ( L_CGM .GT. M_GPA ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( L_CGM, STR  )
                        CALL CLRCH ( STR1 )
                        CALL INCH  ( M_GPA, STR1 )
                        CALL ERR_LOG ( 4141, IUER, 'LISTS_UPDATE', 'The '// &
     &                      'number of parameters in CGM '//STR(1:I_LEN(STR))// &
     &                      ' exceeds the upper limit M_GPA = '//STR1(1:I_LEN(STR1))// &
     &                      ' from solve.i which was defined during compilation'// &
     &                      ' of SOLVE' )
                        RETURN
                   END IF
!
! ---------------- Add it to the CGM ...
!
                   LPARM_CGM(L_CGM) = LPARM_GLO(J1)
!
! ---------------- ... and to the list of new parameters
!
                   KP = KP + 1
                   LPARM_NEW(KP) = LPARM_GLO(J1)
              END IF
 410       CONTINUE
           NPARM_NEW = KP
      END IF
!
! --- Now we make the cross reference list from GLO to CGM:
! --- IXGTC(k) = l  where k is the k-th parameter in LPARM_GLO list
! --- (list of global parameters to be moved to CGM) and
! --- l is the index of that parameter in renewed CGM
!
      DO 420 J2=1,NPARM_GLO
         IP = LTM_DIF ( 1, L_CGM, LPARM_CGM, LPARM_GLO(J2) )
         IF ( IP .LE. 0 ) THEN
              CALL ERR_LOG ( 4142, IUER, 'LISTS_UPDATE', 'Trap of '// &
     &            'internal control: global parameter >>'//LPARM_GLO(J2)// &
     &            '<< was not found in the renewed CGM' )
              RETURN
         END IF
         IXGTC(J2) = IP
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         write ( 6, 110 ) j1, lparm_glo(j2)                  ! %%%%%%%
! 110     format ( 'lists_upate  j2=',i5,')  >>',a20,'<<  ' ) ! %%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 420  CONTINUE
!
      NPARM_CGM = L_CGM
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LISTS_UPDATE  #!#
