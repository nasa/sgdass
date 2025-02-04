      SUBROUTINE BACK_B1B3D ( FAST_COV, FAST_DBG, B3DOBJ, B1B3DOBJ, ARR_MAT, &
     &                        ARR_VEC, ARR_DSP, ARR_SCL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BACK_B1B3D  finds estimates of local and segmented        *
! *   parameters for this arc and their covariance matrices wfor back    *
! *   run of the algorithm B1B3D. Then it extract covariance martices    *
! *   and vectors of adjustments from fileds of object B1B3D and puts    *
! *   them in arrays ARR_MAT, ARR_VEC.                                   *
! *                                                                      *
! * ___________________________ INPUT PARAMETERS: ______________________ *
! *                                                                      *
! * FAST_DBG ( INTEGER*4 ) -- Debugging mode swithcer.                   *
! * FAST_COV ( INTEGER*4 ) -- Mode swithcer.                             *
! *            FAST_COV =1 -- only covariance matrix of global-global    *
! *                           parameters will be calculated.             *
! *            FAST_COV =2 -- covariance matrices of global-global       *
! *                           parameters, local-global and local-local   *
! *                           will be calculated.                        *
! *            FAST_COV =3 -- all covariance matrices which corresponds  *
! *                           non-zero blocks of normal matrices will    *
! *                           calculated.                                *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *  ARR_MAT ( REAL*8    ) -- array with  covariance matrices in PROC    *
! *                           order of parameters.                       *
! *  ARR_VEC ( REAL*8    ) -- array with  adjustments of parameters in   *
! *                           PROC  order of parameters.                 *
! *  ARR_DSP ( REAL*8    ) -- array with  variances of the estiamtes.    *
! *  ARR_SCL ( REAL*8    ) -- array with  scalses.                       *
! *                                                                      *
! *  ###  27-FEB-97   BACK_B1B3D   v1.4  (c)  L. Petrov  13-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      INCLUDE   'solve.i'
      INCLUDE   'baccm.i'
      INCLUDE   'fast.i'
!
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  IUER, G, L, S, SX, NS, FAST_COV, FAST_DBG, IER
      INTEGER*8  GA, LA, SA
      REAL*8     ARR_MAT(*), ARR_VEC(*), ARR_DSP(*), ARR_SCL(*)
      CHARACTER  FINAM_COV*32
!
      G  = B3DOBJ%N_GLO
      L  = B3DOBJ%N_LOC
      S  = B3DOBJ%SB
      SX = B3DOBJ%SX
      GA = (INT8(G)*INT8(G+1))/2
      LA = (INT8(L)*INT8(L+1))/2
      SA = (INT8(S)*INT8(S+1))/2
      NS = B3DOBJ%NBS
!
! --- Obtaining adustemnts of local parameters and adjustments of segmemted
! --- parameters
!
      CALL B1B3D_BCK ( G, L, S, SX, GA, LA, SA, NS, &
     &     %VAL(B1B3DOBJ%AD_W00),    %VAL(B1B3DOBJ%AD_WI0), &
     &     %VAL(B1B3DOBJ%AD_WIJ(1)), %VAL(B1B3DOBJ%AD_BI0), &
     &     %VAL(B1B3DOBJ%AD_BIJ(1)), %VAL(B1B3DOBJ%AD_CIJ(1)), &
     &     %VAL(B1B3DOBJ%AD_DIJ(1)), %VAL(B1B3DOBJ%AD_ZI0), &
     &     %VAL(B1B3DOBJ%AD_ZIJ(1)), %VAL(B1B3DOBJ%AD_E00), &
     &     %VAL(B1B3DOBJ%AD_EI0),    %VAL(B1B3DOBJ%AD_EIJ(1)), &
     &     %VAL(B1B3DOBJ%AD_VG),     %VAL(B1B3DOBJ%AD_VL), &
     &     %VAL(B1B3DOBJ%AD_VS1)     )
!
      IF ( FAST_COV .EQ. F__LOC ) THEN
!
! -------- Obtaining covariance matrices for local paramters and local-global
! -------- parameters. Calculation variances of the local parameters.
! -------- Covariance matrices of segmented parameters set to zero, except the
! -------- main diagonal (set to unity)
!
           CALL ERR_PASS ( IUER, IER )
           CALL B1B3D_COV1 ( G, L, S, SX, GA, LA, SA, NS, &
     &          %VAL(B1B3DOBJ%AD_W00),    %VAL(B1B3DOBJ%AD_WI0), &
     &          %VAL(B1B3DOBJ%AD_BI0),    %VAL(B1B3DOBJ%AD_ZI0), &
     &          %VAL(B1B3DOBJ%AD_WIJ(1)), %VAL(B1B3DOBJ%AD_BIJ(1)), &
     &          %VAL(B1B3DOBJ%AD_CIJ(1)), %VAL(B1B3DOBJ%AD_DIJ(1)), &
     &          %VAL(B1B3DOBJ%AD_ZIJ(1)), %VAL(B1B3DOBJ%AD_NGG), &
     &          %VAL(B1B3DOBJ%AD_NLG),    %VAL(B1B3DOBJ%AD_NLL),   IER )
!
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8521, -1, 'BACK_B1B3D', 'Error during '// &
     &              'calculation variances of local parameters' )
                RETURN
           END IF
        ELSE IF ( FAST_COV .EQ. F__SEG ) THEN
!
! -------- Obtaining covariance matrices for ALL blocks which correspond to
! -------- non-zero elements of normal matrix. Calculation variances of the
! -------- local and segmented parameters.
!
           CALL ERR_PASS ( IUER, IER )
           CALL B1B3D_COV2 ( G, L, S, SX, GA, LA, SA, NS, &
     &          %VAL(B1B3DOBJ%AD_W00),    %VAL(B1B3DOBJ%AD_WI0), &
     &          %VAL(B1B3DOBJ%AD_BI0),    %VAL(B1B3DOBJ%AD_ZI0), &
     &          %VAL(B1B3DOBJ%AD_WIJ(1)), %VAL(B1B3DOBJ%AD_BIJ(1)), &
     &          %VAL(B1B3DOBJ%AD_CIJ(1)), %VAL(B1B3DOBJ%AD_DIJ(1)), &
     &          %VAL(B1B3DOBJ%AD_ZIJ(1)), %VAL(B1B3DOBJ%AD_NGG), &
     &          %VAL(B1B3DOBJ%AD_NLG),    %VAL(B1B3DOBJ%AD_NLL), &
     &          %VAL(B1B3DOBJ%AD_NS1G),   %VAL(B1B3DOBJ%AD_NS2G), &
     &          %VAL(B1B3DOBJ%AD_NS1L),   %VAL(B1B3DOBJ%AD_NS2L), &
     &          %VAL(B1B3DOBJ%AD_NS1S1),  %VAL(B1B3DOBJ%AD_NS2S2), &
     &          %VAL(B1B3DOBJ%AD_NS2S1),  %VAL(B1B3DOBJ%AD_NS1S2),  IER )
!
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8522, -1, 'BACK_B1B3D', 'Error during '// &
     &              'calculation variances of local or segmented parameters' )
                RETURN
           END IF
      END IF
!
! --- Unscaling local and segmented parameters and their covariance matrices
!
      CALL B1B3D_USC ( G, L, S, SX, GA, LA, SA, NS, &
     &     %VAL(B1B3DOBJ%AD_WI0),    %VAL(B1B3DOBJ%AD_WIJ(1)), &
     &     %VAL(B1B3DOBJ%AD_BI0),    %VAL(B1B3DOBJ%AD_BIJ(1)), &
     &     %VAL(B1B3DOBJ%AD_CIJ(1)), %VAL(B1B3DOBJ%AD_DIJ(1)), &
     &     %VAL(B1B3DOBJ%AD_UI0),    %VAL(B1B3DOBJ%AD_UIJ(1)), &
     &     %VAL(B1B3DOBJ%AD_EI0),    %VAL(B1B3DOBJ%AD_EIJ(1)), &
     &     %VAL(B1B3DOBJ%AD_ZI0),    %VAL(B1B3DOBJ%AD_ZIJ(1))  )
!
! --- Extracting from fields of the object B1B3DOBJ covariance matrix
! --- for local and segmented parameters, vector of adustments and their
! --- variances, vector of scales. Then putting them to fields of arrays
! --- ARR2_MAT, ARR2_VEC in PROC order
!
      CALL EXPAND_B1B3D ( FAST_COV, B3DOBJ, B1B3DOBJ, ARR_MAT, ARR_VEC, &
     &                    ARR_DSP, ARR_SCL )
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! -------- DEBUG-mode: Writing object B3DOBJ on disk
!
           call clrch ( finam_cov )
           finam_cov = '/tmp/cov_b1b3d.bin'
           call err_pass ( iuer, ier )
           call wrnor_b1b3d ( finam_cov, b3dobj, b1b3dobj, ier )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  BACK_B1B3D  #!#
