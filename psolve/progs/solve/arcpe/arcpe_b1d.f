      SUBROUTINE ARCPE_B1D ( L, G, JA, JB, ARR1, ARR2, AD_AGG, AD_BG, IUER )
! ************************************************************************
! *                                                                      *
! *      Routine  ARCPE_B1D  firstly rearranges normal matrix and normal *
! *   vector localted in array ARR1 in ARCPE into B1D order for          *
! *   implementing elimination of the local parameters for the arc under *
! *   consideration using B1D approach.                                  *
! *                                                                      *
! *      Then ARPCPE_B1D makes elimnation influence of the local         *
! *   parameters on glolbal one. It makes calculation for forward run of *
! *   global solution for this session.                                  *
! *                                                                      *
! *   ARCPE-order:                                                       *
! *                   LLLLLLmmmm                                         *
! *                    LLLLLmmmm                                         *
! *                     LLLLmmmm     L -- local-local submatrix          *
! *                      LLLmmmm     m -- mixed local-global submatrix   *
! *                       LLmmmm     G -- global-global submatrix        *
! *                        Lmmmm                                         *
! *                         GGGG                                         *
! *                          GGG                                         *
! *                           GG                                         *
! *                            G                                         *
! *                                                                      *
! *   B1D-order:                                                         *
! *                   LLLLLL     A_LL                                    *
! *                    LLLLL                                             *
! *                     LLLL                                             *
! *                      LLL                                             *
! *                       LL                                             *
! *                        L                                             *
! *                                                                      *
! *                   mmmm       A_LG                                    *
! *                   mmmm                                               *
! *                   mmmm                                               *
! *                   mmmm                                               *
! *                   mmmm                                               *
! *                                                                      *
! *                   GGGG       A_GG                                    *
! *                    GGG                                               *
! *                     GG                                               *
! *                      G                                               *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *        L ( INTEGER*4  ) -- the number of local parameters.           *
! *        G ( INTEGER*4  ) -- the number of global parameters.          *
! *       JA ( INTEGER*4  ) -- index of the element in ARR1 or ARR2      *
! *                            which is the first element of the normal  *
! *                            matrix.                                   *
! *       JB ( INTEGER*4  ) -- index of the element in ARR1 or ARR2      *
! *                            which is the first element of the normal  *
! *                            vector.                                   *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD     ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
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
! *  ###  11-FEB-97    ARCPE_B1D   v1.2  (c)  L. Petrov  18-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
!
      INTEGER*4  L, G, JA, JB, IUER
      REAL*8     ARR1(*), ARR2(*)
      INTEGER*8  AD_AGG, AD_ALG, AD_ALL, AD_BG, AD_BL, AD_SC, &
     &           AD_WGG, AD_WLG, AD_WLL, AD_WG, AD_WL
!
! --- Calculation addresses for different submatrices
!
      AD_ALL = LOC( ARR1(JA) )                    ! local-local   normal martix
      AD_ALG = AD_ALL + 8* (INT8(L)*INT8(L+1))/2  ! local-global  normal martix
      AD_AGG = AD_ALG + 8*  INT8(L)*INT8(G)       ! global-global normal martix
      AD_SC  = LOC( ARR1(1)  )                    ! vector of scales
      AD_BL  = LOC( ARR1(JB) )                    ! local normal vector
      AD_BG  = AD_BL  + 8* L                      ! global normal vector
!
      AD_WL  = LOC( ARR2(JB) )                    ! working vector. Dimension L
      AD_WG  = AD_WL  + 8* L                      ! working vector. Dimension G
      AD_WLL = LOC( ARR2(JA) )                    ! working matrix L*L/2
      AD_WLG = LOC( ARR2(JA) ) + 8* (INT8(L)*INT8(L+1))/2                      ! working matrix L*G
      AD_WGG = LOC( ARR2(JA) ) + 8* (INT8(L)*INT8(L+1))/2 + 8* INT8(L)*INT8(G) ! working matrix G*G/2
!
! --- Parameter elimination
!
      CALL B1D_FORWARD ( G, L, &
     &     %VAL(AD_AGG), %VAL(AD_ALG), %VAL(AD_ALL), &
     &     %VAL(AD_BG),  %VAL(AD_BL), &
     &     %VAL(AD_WGG), %VAL(AD_WLG), %VAL(AD_WLL), &
     &     %VAL(AD_WG),  %VAL(AD_WL),  %VAL(AD_SC),  RCOND, IUER )
!
! --- Putting in AD_ALG  -AD_WLG  -- other SOLVE routines expect negative sign
!
      CALL COPY_V8   ( INT8(L)*INT8(G), %VAL(AD_WLG), %VAL(AD_ALG) )
      CALL MUL_VC_V8 ( INT8(L)*INT8(G), %VAL(AD_ALG), -1.0D0 )
!
! --- Saving RCOND in glbfil
!
      CALL USE_GLBFIL_4 ( 'OWC' )
      RETURN
      END  !#!  ARCPE_B1D  #!#
