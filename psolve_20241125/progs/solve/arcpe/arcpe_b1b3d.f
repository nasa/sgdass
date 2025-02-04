#define NO_DEBUG
      SUBROUTINE ARCPE_B1B3D ( B3DOBJ, B1B3DOBJ, AD_AGG, AD_BG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ARCPE_B1B3D  makes calculation for forward run of global  *
! *   solution for this session using B1B3D algorithm. It is assumed     *
! *   that blocks of normal matrix and normal vector were counted        *
! *   properly and saved  the object B1B3DOBJ. Results of the work of    *
! *   ARCPE_B1B3D are updated submatrices kept in B1B3DOBJ object.       *
! *                                                                      *
! *   Addresses of updated global-global normal matrix and global-global *
! *   normal vector will be transfered to AD_AGG and AD_BG.              *
! *                                                                      *
! * ___________________________ INPUT PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *     IUER ( INTEGER*4, OPT ) -- Universal error habdler.              *
! *            Input: swicth IUER=0 -- no error messages will be         *
! *                                 generated even in the case of error. *
! *                          IUER=-1 -- in the case of error the message *
! *                                  will pe put on stdout.              *
! *            Default input value = -1                                  *
! *            Output: 0 in the case of successful completion and error  *
! *                    code in the case of error.                        *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *   AD_AGG ( INTEGER*4  ) -- Address of the global-global normal       *
! *                            matrix (CGM).                             *
! *    AD_BG ( INTEGER*4  ) -- Address of the global-global normal       *
! *                            vector.                                   *
! *                                                                      *
! *  ###  27-FEB-97  ARCPE_B1B3D   v1.3  (c)  L. Petrov  10-MAR-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
!
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      ADDRESS__TYPE :: AD_AGG, AD_BG
      INTEGER*4  IUER
      INTEGER*8  GA, LA, SA
      INTEGER*4  G, L, S, SX, NS, IER
! 
      G  = B3DOBJ%N_GLO
      L  = B3DOBJ%N_LOC
      S  = B3DOBJ%SB
      SX = B3DOBJ%SX
      GA = (INT8(G)*INT8(G+1))/2
      LA = (INT8(L)*INT8(L+1))/2
      SA = (INT8(S)*INT8(S+1))/2
      NS = B3DOBJ%NBS
      IF ( G .LE. 0 ) THEN
           CALL ERR_LOG ( 8511, IUER, 'ARCPE_B1B3D', 'There are no any '// &
     &         'global parameters. Session '//B3DOBJ%DBNAME_MES// &
     &         ' was being processed' )
           RETURN
      END IF
#ifdef DEBUG
   write ( 6, * ) 'arcpe_b1b3d: g,l,s,xs= ', g, l, s, sx, ' ns= ', ns 
#endif
!
! --- Scaling normal equations. Only local and segmented parameters are being
! --- scaled. Global one remain untouched
!
      CALL ERR_PASS ( IUER, IER )
      CALL B1B3D_SCL ( G, L, S, SX, GA, LA, SA, NS, &
     &                 %VAL(B1B3DOBJ%AD_WI0),    %VAL(B1B3DOBJ%AD_WIJ(1)), &
     &                 %VAL(B1B3DOBJ%AD_BI0),    %VAL(B1B3DOBJ%AD_BIJ(1)), &
     &                 %VAL(B1B3DOBJ%AD_CIJ(1)), %VAL(B1B3DOBJ%AD_DIJ(1)), &
     &                 %VAL(B1B3DOBJ%AD_ZI0),    %VAL(B1B3DOBJ%AD_ZIJ(1)), &
     &                 %VAL(B1B3DOBJ%AD_UI0),    %VAL(B1B3DOBJ%AD_UIJ(1)), &
     &                 IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8511, IUER, 'ARCPE_B1B3D', 'Scaling error during '// &
     &         'forward run of global solution made by B1B3D algorithm '// &
     &         'while session '//B3DOBJ%DBNAME_MES//' was being processed' )
           RETURN
      END IF
!
! --- Eliminating influence of local and segmented parameters on global ones
!
      CALL ERR_PASS ( IUER, IER )
      CALL B1B3D_FRW ( G, L, S, SX, GA, LA, SA, NS, &
     &                 %VAL(B1B3DOBJ%AD_W00),    %VAL(B1B3DOBJ%AD_WI0), &
     &                 %VAL(B1B3DOBJ%AD_WIJ(1)), %VAL(B1B3DOBJ%AD_BI0), &
     &                 %VAL(B1B3DOBJ%AD_BIJ(1)), %VAL(B1B3DOBJ%AD_CIJ(1)), &
     &                 %VAL(B1B3DOBJ%AD_DIJ(1)), %VAL(B1B3DOBJ%AD_Z00), &
     &                 %VAL(B1B3DOBJ%AD_ZI0),    %VAL(B1B3DOBJ%AD_ZIJ(1)), &
     &                 %VAL(B1B3DOBJ%AD_NGG),    %VAL(B1B3DOBJ%AD_NLG), &
     &                 %VAL(B1B3DOBJ%AD_NLL),    %VAL(B1B3DOBJ%AD_NS1G), &
     &                 %VAL(B1B3DOBJ%AD_NS2G),   %VAL(B1B3DOBJ%AD_NS1L), &
     &                 %VAL(B1B3DOBJ%AD_NS2L),   %VAL(B1B3DOBJ%AD_NS2S1), &
     &                 %VAL(B1B3DOBJ%AD_NS1S1),  %VAL(B1B3DOBJ%AD_VG), &
     &                 %VAL(B1B3DOBJ%AD_VL),     %VAL(B1B3DOBJ%AD_VS1), &
     &                 B3DOBJ%RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8512, IUER, 'ARCPE_B1B3D', 'Elimination error '// &
     &         'during forward run of global solution made by B1B3D '// &
     &         'algorithm while session '//B3DOBJ%DBNAME_MES// &
     &         ' was being processed' )
           RETURN
      END IF
!
! --- Copying addresses of the combined global-global normal matrix and
! ---                                   global-global normal vector
!
      AD_AGG = B1B3DOBJ%AD_W00
      AD_BG  = B1B3DOBJ%AD_Z00
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ARCPE_B1B3D  #!#
