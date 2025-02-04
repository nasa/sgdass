      SUBROUTINE MAPCNST_B3D ( N, MAT, DAPR_FULL, B3DOBJ )
! ************************************************************************
! *                                                                      *
! *   Routine  MAPCNSTR_B3D  makes the following things:                 *
! *   1) Finds correspondence for elements MAT oredered for FULL case of *
! *   SOLVE to the global submatrix B0 of B3D case. 2) Adds non-zero     *
! *   elements of the matrix  MAT  to corresponding elements gobal       *
! *   matrix B3D, 3) Finds correspondence for elements vectro of         *
! *   corrstions oredered for FULL case of SOLVE to the global normal    *
! *   vecotr Z0 of B3D case. 4) Puts non-zero elements of DAPR_FULL to   *
! *   corresponding  place COV_GLO, ordered for B3D case. 5) Multiply    *
! *   global-global submatrix B0 on COV_GLO and substracts results from  *
! *   global vector Z0.                                                  *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *         N ( INTEGER*4  ) -- Total number of prarameters.             *
! *       MAT ( REAL*8     ) -- Normal matrix of constraints. Dimension: *
! *                             (N*(N+1))/2 .                            *
! * DAPR_FULL ( REAL*8     ) -- Vector of corrections to global          *
! *                             parameters.                              *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! *  ###  17-JAN-97  MAPCNSTR_B3D  v1.0  (c)  L. Petrov  17-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  N
      REAL*8     MAT(*), DAPR_FULL(N), &
     &           DAPR_GLO(M_GPA), COV_GLO(M_GPA)
      INTEGER*4  J1, J2, LA, NBL, IR, IC
      CHARACTER  TYP*1
      LOGICAL    DAPR_DONE
      ADDRESS__TYPE           :: IAD
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D
!
      DAPR_DONE = .FALSE.
      LA = 0
      CALL NOUT_R8 ( B3DOBJ%N_GLO, COV_GLO )
      DO 410 J1=1,N       ! Cycle on
         DO 420 J2=1,J1   ! all elements of FULL matrix
            LA = LA + 1
!
! --------- Since matrix MAT was zeroed before imposing constraints we can
! --------- skip zero elements
!
            IF ( MAT(LA) .NE. 0.0D0 ) THEN
!
! -------------- Calculate the address of the element of apropriate submatrix
! -------------- in B3DOBJ which corresponds to the element MAT(J1,J2) of
! -------------- full matrix
!
                 IAD = FULL_B3D ( B3DOBJ, J1, J2, TYP, NBL, IR, IC )
                 IF ( IAD .EQ. -1 ) THEN
                      WRITE ( 6, * ) ' j1=',j1,' j2=',j2, &
     &                       ' nbl=',nbl,' ir=',ir,' ic=',ic, &
     &                       ' iad=',iad
                      WRITE ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                      CALL ERR_LOG ( 3451, -1, 'MAPCNST_B3D', &
     &                              'Internal error' )
                      RETURN
                 END IF
!
! -------------- Updating matrix hidden in B3DOBJ
!
                 CALL R8_UPDATE ( IAD, MAT(LA) )
            END IF
 420     CONTINUE
!
         IAD = FULL_B3D ( B3DOBJ, J1, J1, TYP, NBL, IR, IC )
         IF ( NBL .EQ. 0 ) THEN
              DAPR_GLO(IR) = DAPR_FULL(J1)
              IF ( DAPR_FULL(J1) .NE. 0.0D0 ) DAPR_DONE = .TRUE.
            ELSE
              IF ( DAPR_FULL(J1) .NE. 0.0D0 ) THEN
                 WRITE ( 6, * ) ' %% typ=',typ,' j1=',j1,' j2=',j2, &
     &                  ' nbl=',nbl,' ir=',ir,' ic=',ic, &
     &                  ' dapr_full(j1)=',dapr_full(j1)
                 CALL ERR_LOG ( 3452, -1, 'MAPCNST_B3D', 'Internal '// &
     &                                                   'error' )
              END IF
         END IF
 410  CONTINUE
!
! --- If at least one non-zero element of DAPR encountered ... Apliying
! --- correction to global parameters
!
      IF ( DAPR_DONE ) THEN
           CALL MUL_MV_SV_V ( B3DOBJ%N_GLO, %VAL(B3DOBJ%AD_B0), &
     &          B3DOBJ%N_GLO, DAPR_GLO, B3DOBJ%N_GLO, COV_GLO, %VAL(0) )
           CALL SUB_VV ( B3DOBJ%N_GLO, %VAL(B3DOBJ%AD_Z0), COV_GLO )
      END IF
!
      RETURN
      END  !#!  MAPCNST_B3D  #!#
