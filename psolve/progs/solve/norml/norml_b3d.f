      SUBROUTINE NORML_B3D ( FAST_COV, F_UD, FAST_DBG, B3DOBJ, GCOND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NORML_B3D  finds estimates of the parameteres, estimates  *
! *   of their variancres and covariance matrices in LSQ problem for the *
! *   case of B3D parameterization.                                      *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! * FAST_COV ( INTEGER*4 ) -- Mode switch for calculation covariance     *
! *                           matrices.                                  *
! *             FAST_COV = F__GLO -- Only covariance matrix for global   *
! *                                  parameters will be calculated.      *
! *             FAST_COV = F__SEG -- All blocks of covariance matrix     *
! *                                  which correspond to non-zero blocks *
! *                                  of normal matrix will be calculated *
! *             FAST_COV = F__LOC -- the same as F__SEG.                 *
! *             FAST_COV = F__FUL -- All blocks of the covariance matrix *
! *                                  will be calculated.                 *
! *     F_UD ( LOGICAL*4 ) -- Flag: is it necessary to do unscaling and  *
! *                           calculation of square root from the        *
! *                           dispersion of the parameters. If .FALSE.   *
! *                           then covariance matrix remain unscaled and *
! *                           dispersion of the parameters will not be   *
! *                           calculated.                                *
! * FAST_DBG ( INTEGER*4 ) -- Verbosity mode switch for debug. Should be *
! *                           zero for normal work.                      *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    GCOND ( REAL*8    ) -- condition number for combined              *
! *                           global-global matrix.                      *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *            Input: mode swicth:                                       *
! *                   IUER=0 -- no error messages  will be generated     *
! *                             even in the case of error.               *
! *                   IUER=-1 - in the case of error the message will    *
! *                             be put in stdout.                        *
! *            Output: 0 in the case of successful completion and        *
! *                    non-zero in the case of error.                    *
! *                                                                      *
! *  History:                                                            *
! *                                                                      *
! *  22-SEP-97 pet  Added a formal parameter F_FD. Added support of      *
! *                 calculateion full covariance matrix.                 *
! *                                                                      *
! *  ###  10-JAN-1997  NORML_B3D   v3.1  (c)  L. Petrov 17-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  FAST_COV, FAST_DBG, IUER
      LOGICAL*4  F_UD, F_MAT, F_VEC
      INTEGER*4  N, G, SB, SX, NK, IER
      INTEGER*8  GA, SBA, SXA
      ADDRESS__TYPE :: AD_WGG, AD_W1SG, AD_W2SG, AD_W3SSS, AD_W4SS, AD_W5SSS, &
     &                 AD_W6SS, AD_WS, AD_WG
      REAL*8     SIG_B3D, GCOND
      CHARACTER  STR*20
!
! --- Setting address-synonims for working matrices/vectors
!
      AD_WGG   = B3DOBJ%AD_N00
      AD_W1SG  = B3DOBJ%AD_N10
      AD_W2SG  = B3DOBJ%AD_N20
      AD_W3SSS = B3DOBJ%AD_N11
      AD_W4SS  = B3DOBJ%AD_N21
      AD_W5SSS = B3DOBJ%AD_N22
      AD_W6SS  = B3DOBJ%AD_N12
      AD_WS    = B3DOBJ%AD_VS1
      AD_WG    = B3DOBJ%AD_VG0
!
!
! --- Extracting some fields from B3DOBJ for faciliating programming
!
      N   = B3DOBJ%NBS
      NK  = ((N-1)*(N-2))/2
      G   = B3DOBJ%N_GLO
      GA  = (INT8(G)*INT8(G+1))/2
      SB  = B3DOBJ%SB
      SBA = (INT8(SB)*INT8(SB+1))/2
      SX  = B3DOBJ%SX
      SXA = (INT8(SX)*INT8(SX+1))/2
!
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
         WRITE ( 6, * ) '  ##  norml_b3d  1  (before  b3d_scl_x) '
         WRITE ( 6, * ) '  n=',n,' g=',g,' ga=',ga,' sb=',sb,' sx=',sx
      END IF
!
! --- Scaling normal matrix and normal vector. All matrices and vecotrs are
! --- scaled so that the main diagonal of the normal matrix become to be unit
! --- (even in the case if the ingenious element were zero -- that is dirty
! --- trick used in SOLVE )
!
      CALL ERR_PASS ( IUER, IER )
      CALL B3D_SCL_X ( N, G, GA, SB, SBA, SX, SXA, &
     &     %VAL(B3DOBJ%AD_B0),    %VAL(B3DOBJ%AD_Z0),    %VAL(B3DOBJ%AD_U0), &
     &     %VAL(B3DOBJ%AD_B(1)),  %VAL(B3DOBJ%AD_C(1)),  %VAL(B3DOBJ%AD_D(1)), &
     &     %VAL(B3DOBJ%AD_ZS(1)), %VAL(B3DOBJ%AD_US(1)), &
     &     %VAL(B3DOBJ%AD_BX),    %VAL(B3DOBJ%AD_CX),    %VAL(B3DOBJ%AD_DX), &
     &     %VAL(B3DOBJ%AD_ZSX),   %VAL(B3DOBJ%AD_USX), &
     &     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6501, IUER, 'NORML_B3D', 'Failure in '// &
     &         'scaling normal equations' )
           RETURN
      END IF
!
! --- Decomposition of normal matrix Finding the estimates of the parameters
!
      CALL ERR_PASS  ( IUER, IER )
      CALL B3D_SOL_X ( N, G, SB, SBA, SX, SXA, &
     &     %VAL(B3DOBJ%AD_B0),    %VAL(B3DOBJ%AD_Z0),    %VAL(B3DOBJ%AD_E0), &
     &     %VAL(B3DOBJ%AD_B(1)),  %VAL(B3DOBJ%AD_C(1)),  %VAL(B3DOBJ%AD_D(1)), &
     &     %VAL(B3DOBJ%AD_ZS(1)), %VAL(B3DOBJ%AD_ES(1)), &
     &     %VAL(B3DOBJ%AD_BX),    %VAL(B3DOBJ%AD_CX),    %VAL(B3DOBJ%AD_DX), &
     &     %VAL(B3DOBJ%AD_ZSX),   %VAL(B3DOBJ%AD_ESX), &
     &     %VAL(AD_WGG),          %VAL(AD_W1SG),         %VAL(AD_W2SG), &
     &     %VAL(AD_W3SSS),        %VAL(AD_W4SS),         %VAL(AD_WS), &
     &     %VAL(AD_WG),           GCOND,                 IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6502, IUER, 'NORML_B3D', 'Failure in '// &
     &         'solving system of normal equations by B3D '// &
     &         'algorithm' )
           RETURN
      END IF
!
!
! --- Finding blocks of covaraince matrices and putting them on the place of
! --- normal matrix
!
      SIG_B3D = 1.D0
      IF ( FAST_COV .EQ. F__SEG  .OR.  FAST_COV .EQ. F__LOC ) THEN
           CALL B3D_COV_X ( N, G, SB, SBA, SX, SXA, &
     &          %VAL(B3DOBJ%AD_B0), &
     &          %VAL(B3DOBJ%AD_B(1)), %VAL(B3DOBJ%AD_C(1)), %VAL(B3DOBJ%AD_D(1)), &
     &          %VAL(B3DOBJ%AD_BX),   %VAL(B3DOBJ%AD_CX),   %VAL(B3DOBJ%AD_DX), &
     &          %VAL(AD_W1SG),        %VAL(AD_W2SG),        %VAL(AD_W3SSS), &
     &          %VAL(AD_W4SS),        %VAL(AD_W5SSS),       %VAL(AD_W6SS) )
         ELSE IF ( FAST_COV .EQ. F__FUL ) THEN
           CALL B3D_COV_F ( N, G, SB, SBA, SX, SXA, NK, &
     &          %VAL(B3DOBJ%AD_B0), &
     &          %VAL(B3DOBJ%AD_B(1)), %VAL(B3DOBJ%AD_C(1)), %VAL(B3DOBJ%AD_D(1)), &
     &          %VAL(B3DOBJ%AD_BX),   %VAL(B3DOBJ%AD_CX),   %VAL(B3DOBJ%AD_DX), &
     &          %VAL(B3DOBJ%AD_CVF), &
     &          %VAL(AD_W1SG),        %VAL(AD_W2SG),        %VAL(AD_W3SSS), &
     &          %VAL(AD_W4SS),        %VAL(AD_W5SSS),       %VAL(AD_W6SS) )
         ELSE IF ( FAST_COV .EQ. F__GLO ) THEN
           CONTINUE
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( FAST_COV, STR )
           CALL ERR_LOG ( 6503, IUER, 'NORML_B3D', 'Wrong value of FAST_COV '// &
     &       ' FAST_COV = '//STR )
      END IF!
!
      IF ( F_UD ) THEN
           F_MAT = .TRUE.
           F_VEC = .TRUE.
         ELSE
           F_MAT = .FALSE.
           F_VEC = .FALSE.
      END IF
!
!
! --- Unscaling vector of the esimates (and covariance matrix if F_UD = .TRUE. )
!
      CALL ERR_PASS  ( IUER, IER )
      IF ( FAST_COV .NE. F__FUL ) THEN
!
! ------ Case when covarianve matrix has been calculated only partly
!
         CALL B3D_USC_X ( F_MAT, F_VEC, N, G, GA, SB, SBA, SX, SXA, &
     &        %VAL(B3DOBJ%AD_B0),    %VAL(B3DOBJ%AD_E0),   %VAL(B3DOBJ%AD_U0), &
     &        %VAL(B3DOBJ%AD_B(1)),  %VAL(B3DOBJ%AD_C(1)), %VAL(B3DOBJ%AD_D(1)), &
     &        %VAL(B3DOBJ%AD_ES(1)), %VAL(B3DOBJ%AD_US(1)), &
     &        %VAL(B3DOBJ%AD_BX),    %VAL(B3DOBJ%AD_CX),   %VAL(B3DOBJ%AD_DX), &
     &        %VAL(B3DOBJ%AD_ESX),   %VAL(B3DOBJ%AD_USX)   )
       ELSE IF ( FAST_COV .EQ. F__FUL ) THEN
!
! ------ Case when covariance matrix has been calculated completely
!
         CALL B3D_USC_F ( F_MAT, F_VEC, N, G, GA, SB, SBA, SX, SXA, NK, &
     &        %VAL(B3DOBJ%AD_B0),    %VAL(B3DOBJ%AD_E0),   %VAL(B3DOBJ%AD_U0), &
     &        %VAL(B3DOBJ%AD_B(1)),  %VAL(B3DOBJ%AD_C(1)), %VAL(B3DOBJ%AD_D(1)), &
     &        %VAL(B3DOBJ%AD_ES(1)), %VAL(B3DOBJ%AD_US(1)), &
     &        %VAL(B3DOBJ%AD_BX),    %VAL(B3DOBJ%AD_CX),   %VAL(B3DOBJ%AD_DX), &
     &        %VAL(B3DOBJ%AD_ESX),   %VAL(B3DOBJ%AD_USX),  %VAL(B3DOBJ%AD_CVF) )
      END IF
!
!
      IF ( F_UD  ) THEN
!
! -------- Finding estimates of the variances of the parameters
!
           CALL ERR_PASS  ( IUER, IER )
           CALL B3D_DSP_X ( FAST_COV, N, G, GA, SB, SBA, SX, SXA, &
     &          %VAL(B3DOBJ%AD_B0),   %VAL(B3DOBJ%AD_Z0), &
     &          %VAL(B3DOBJ%AD_C(1)), %VAL(B3DOBJ%AD_ZS(1)), &
     &          %VAL(B3DOBJ%AD_CX),   %VAL(B3DOBJ%AD_ZSX), SIG_B3D, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6504, IUER, 'NORML_B3D', 'Failure in '// &
     &              'finding estimates of variances of the parameter estimates' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  NORML_B3D  #!#
