      SUBROUTINE HPE_RES ( L_HPE, HPE, L_PAR, C_PAR, L_STA, EST_VEC, &
     &                     COV_MAT, SCL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  HPE_RES  puts in the spool file opened at the logical     *
! *   unit 23 the HPE-section with information about results of          *
! *   estimation of harmonic site position varations.                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   L_HPE ( INTEGER*4 ) -- Number of harmonics.                        *
! *     HPE ( RECORD    ) -- Array of records defined in                 *
! *                          $PSOLVE_ROOT/include/solve.i which keeps    *
! *                          information about harmonic site position    *
! *                          variations estimation. Dimension: L_HPE.    *
! *   L_PAR ( INTEGER*4 ) -- The total number of estimated global        *
! *                          parameters.                                 *
! *   L_STA ( INTEGER*4 ) -- The total number of partcipated stations.   *
! *                          NB: stations with episodic are counted      *
! *                          twice.                                      *
! *   C_PAR ( CHARACTER ) -- Array of parameter names. Dimension: L_PAR. *
! * EST_VEC ( REAL*8    ) -- Vector of estimates. Dimension: L_PAR.      *
! * COV_MAT ( REAL*8    ) -- Unscaled covariance matrix in packed upper  *
! *                          triangular representation. Dimension: L_PAR.*
! *     SCL ( REAL*8    ) -- Covariance scaling factor.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
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
! *  ### 07-MAR-2005     HPE_RES   v1.3 (c)  L. Petrov  13-NOV-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'prfil.i'
      INTEGER*4  L_HPE, L_PAR, L_STA, IUER
      CHARACTER  C_PAR(L_PAR)*(*)
      CHARACTER  PAR_NAM*20, CMP(3)*1, COS_SIN(2)*1
      DATA       CMP     / 'X', 'Y', 'Z' /
      DATA       COS_SIN / 'C', 'S' /
      REAL*8     COV_MAT(*), EST_VEC(L_PAR), SCL
      TYPE       ( HPE__TYPE ) HPE(L_HPE)
      CHARACTER  STA_NAM*8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, ISTA, IND_PAR(M_GPA), &
     &           IPAR, K_PAR, K_STA
      INTEGER*4  I, J
      INTEGER*8  LOCS
      LOCS(I,J) = INT8(min(I,J)) +(INT8(max(I,J))*INT8(max(I,J)-1))/2
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( L_PAR .LT. 1  .OR.  L_PAR .GT. M_GPA ) THEN
           WRITE ( 6, * ) 'L_PAR  = ', L_PAR, ' M_GPA = ', M_GPA
           CALL ERR_LOG ( 7481, IUER, 'HPE_RES', 'Parameter L_PAR is '// &
     &         'out of range [1, M_GPA]' )
           RETURN 
      END IF
!
! --- Write the begining section marker 
!
      WRITE ( 23, '(A)' ) '1  HPE'
      WRITE ( 23, '(A, I4)' ) 'L_HPE ', L_HPE
!
      DO 410 J1=1,L_HPE
         K_STA = 0
         DO 420 J2=1,HPE(J1)%L_STA
            STA_NAM = HPE(J1)%C_STA(J2)
!@U            CALL UNDSCR ( STA_NAM )
            ISTA = LTM_DIF ( 1, L_STA, ISITN_CHR, STA_NAM )
            IF ( ISTA > 0 ) THEN
                 K_STA = K_STA + 1
            END IF
 420     CONTINUE 
         WRITE ( 23, 110 ) J1, HPE(J1)%PHASE, HPE(J1)%FREQ
 110     FORMAT ( '   I_HPE ', I4,'  Phase: ', F10.7,'  Frequency: ', 1PD19.12 )
     &                
         WRITE ( 23, '(A, I4)'  ) '   I_STA ', K_STA
         K_PAR = 0
!
! ------ Cycle over all stations for which a User ordered harmonic position 
! ------ variation estimation
!
         DO 430 J3=1,HPE(J1)%L_STA
            STA_NAM = HPE(J1)%C_STA(J3)
!@U            CALL UNDSCR ( STA_NAM )
            ISTA = LTM_DIF ( 1, L_STA, ISITN_CHR, STA_NAM )
            IF ( ISTA > 0 ) THEN
                 DO 440 J4=1,3 ! Cycle over components 
                    WRITE  ( 23, 210 ) HPE(J1)%C_STA(J3), CMP(J4), &
     &                                 VSITEC(J4,ISTA)
 210                FORMAT ( 3X,'APS: ',A, 1X, A, 2X, 'C: ', F14.5 )
                    DO 450 J5=1,2 ! Cycle over cosine and sine components
!
! -------------------- Build parameter name
! 
                       PAR_NAM = STA_NAM//' '//CMP(J4)//COS_SIN(J5)//' '// &
     &                           HPE(J1)%NAME
!
! -------------------- Search parameter name in the list
!
                       IPAR = LTM_DIF ( 1, L_PAR, C_PAR, PAR_NAM )
                       IF ( IPAR > 0 ) THEN
!
! ------------------------- Aga. We found it. Put the parameter ines into the list
!
                            K_PAR = K_PAR + 1
                            IND_PAR(K_PAR) = IPAR
!
! ------------------------- We do it the second time, because of in the prior time
! ------------------------- we replaced underscores with blanks
!
                            PAR_NAM = HPE(J1)%C_STA(J3)//' '//CMP(J4)// &
     &                                COS_SIN(J5)//' '//HPE(J1)%NAME
!
! ------------------------- ... and print the estimate
!
                            WRITE ( 23, 220 ) PAR_NAM, IPAR, HPE(J1)%PHASE, &
     &                                        HPE(J1)%FREQ, EST_VEC(IPAR)
 220                        FORMAT ( 3X,'EST: ', A, ' I: ',I5, ' P: ', F10.7, &
     &                                  ' F: ', 1PD18.12, ' V: ',1PD15.7 )
                       END IF
 450                CONTINUE 
 440             CONTINUE 
            END IF
 430     CONTINUE 
!
         IF ( K_PAR > 0 ) THEN
!
! ----------- Print elements of covariance matrix
!
              DO 460 J6=1,K_PAR
                 STA_NAM = C_PAR(J6)(1:8)
                 CALL VTD_NAME_REPAIR ( STA_NAM )
                 DO 470 J7=J6,K_PAR
                    IF ( C_PAR(IND_PAR(J6))(1:8) == C_PAR(IND_PAR(J7))(1:8) ) THEN
                         WRITE ( 23, 230 ) STA_NAM, IND_PAR(J6), IND_PAR(J7), &
     &                           COV_MAT(LOCS(IND_PAR(J6),IND_PAR(J7)))*SCL
 230                     FORMAT ( 3X, 'COV: ', A, ' IND1: ', I5, &
     &                                ' IND2: ', I5, ' C: ',1PD19.12 )
                    END IF
 470             CONTINUE 
 460         CONTINUE 
         END IF
 410  CONTINUE 
      WRITE ( 23, '(A)' ) '2  HPE'
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE HPE_RES
