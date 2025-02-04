      SUBROUTINE EXPAND_CGM_EERM ( B3DOBJ, B1B3DOBJ, NPARM_ARCPE, &
     &                             DBNAME_MES, FINAM_NRM, ADR_AGG, ADR_BG, &
     &                             GLBMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EXPAND_CGM_EERM  expands the CGM in such a way, so it can  *
! *   accomodate all Empricial Earth Rotation Model parameters. It is    *
! *   supposed to run after processing the first experiment of the       *
! *   batch solution. This routine checks all ERM parameter srequested   *
! *   for the batch run. It adds to the parameter list those parameters  *
! *   names which are not there and extends the CGM and fills the new    *
! *   allocated space with zeroes.                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * DBNAME_MES ( CHARACTER ) -- Line with the database name and its      *
! *     B3DOBJ ( RECORD    ) -- Object with data structure for B3D       *
! *                             extension of SOLVE.                      *
! *   B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D     *
! *                             extension of SOLVE.                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * ADR_AGG ( INTEGER*8      ) -- Address of global-glboal block of      *
! *                               normal matrix for this session.        *
! *  ADR_BG ( INTEGER*8      ) -- Address of global vector of right-hand *
! *                               side of normal equations.              *
! *  GLBMEM ( RECORD         ) -- Data structure which keeps addresses   *
! *                               of CGM, list of global parameters,     *
! *                               global socom, prfil and temporary      *
! *                               normal matrices. Defined in            *
! *                               $PSOLVE_ROOT/include/glbp.i            *
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
! *                                                                      *
! * ###  16-MAY-2006 EXPAND_CGM_EERM v1.0 (c) L. Petrov  16-MAY-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'fast.i'
      INCLUDE   'glbp.i'
      TYPE     ( B3D__STRU     ) :: B3DOBJ
      TYPE     ( B1B3D__STRU   ) :: B1B3DOBJ
      TYPE     ( GLB_MEM__STRU ) :: GLBMEM    ! defined in glbp.i
      CHARACTER  DBNAME_MES*(*), FINAM_NRM*(*)
      INTEGER*4  NPARM_ARCPE
      ADDRESS__TYPE :: ADR_AGG, ADR_BG, IUER
      CHARACTER  C_GPA(M_GPA)*(L__GPA)
      CHARACTER  C_PAR_ERM(M_GPA)*20, C_PAR_STR*20
      INTEGER*4  J1, J2, J3, J4, IP, N_ERM, NPARM_CGM, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      CALL LIB$MOVC3 ( L__GPA*GLBMEM%L_GPA, %VAL(GLBMEM%ADR_C_GPA), C_GPA )
!
      N_ERM = 0
      DO 410 J1=1,3
         DO 420 J2=1-EERM%DEGREE(J1),EERM%NKNOTS(J1)-1
            CALL CLRCH ( C_PAR_STR )
            WRITE ( C_PAR_STR, '("ERM ",I1,4X,I5,5X)' ) J1, J2
!
            IP = LTM_DIF ( 0, GLBMEM%L_GPA, C_GPA, C_PAR_STR )
            IF ( IP .LE. 0 ) THEN
                 N_ERM = N_ERM + 1
                 C_PAR_ERM(N_ERM) = C_PAR_STR
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( N_ERM == 0 ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!                                !
      IF ( GLBMEM%L_GPA + N_ERM .GE. GLBMEM%NPAR_CGM ) THEN
           NPARM_CGM = GLBMEM%L_GPA + N_ERM 
           CALL ERR_PASS ( IUER, IER )
           CALL GLO_MEM_FAULT ( B3DOBJ, B1B3DOBJ, NPARM_CGM, NPARM_ARCPE, &
     &                          DBNAME_MES, FINAM_NRM, ADR_AGG, ADR_BG, &
     &                          GLBMEM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4141, IUER, 'EXPAND_CGM_EERM', 'Failure '// &
     &              'to recover from memory fault during processing '// &
     &              'experiment '//DBNAME_MES )
                RETURN 
           END IF 
      END IF
!
      DO 430 J3=1,N_ERM
         C_GPA(GLBMEM%L_GPA+J3) = C_PAR_ERM(J3)
 430  CONTINUE 
      GLBMEM%L_GPA = GLBMEM%L_GPA + N_ERM
      CALL LIB$MOVC3 ( L__GPA*GLBMEM%L_GPA, C_GPA, %VAL(GLBMEM%ADR_C_GPA) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EXPAND_CGM_EERM  !#!#
