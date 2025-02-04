      SUBROUTINE BACK_B1D ( L, G, JA, JB, M, ARR1, ARR2 )
! ************************************************************************
! *                                                                      *
! *   Routine  BACK_B1D  rearranges matrix MAT1 for implementing         *
! *   calculcation vector of adjustments and the estimates of the        *
! *   covariance matrix of local parameters for the arc under            *
! *   consideration.                                                     *
! *                                                                      *
! *  ###  12-FEB-97    BACK_B1D   v1.2  (c)  L. Petrov  09-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  L, G, JA, JB, M
      REAL*8     ARR1(M), ARR2(M)
!
      INTEGER*4  J2
      ADDRESS__TYPE :: ISH, &
     &           AD_AGG, AD_ALG, AD_ALL, AD_BG, AD_BL, AD_SC, &
     &           AD_WLG, AD_WLL, AD_WL
      INTEGER*4  I, J
      INTEGER*8  LOCS
      LOCS(I,J) = INT8(MIN(I,J)) + (INT8(MAX(I,J))*INT8(MAX(I,J)-1))/2
!
! --- Calculation addresses for different submatrices
!
      AD_ALL = LOC( ARR1(JA) )                    ! local-local nor. mat^-1
      AD_ALG = AD_ALL + 8* (INT8(L)*INT8(L+1))/2  ! local-global  normal martix
      AD_AGG = AD_ALG + 8* INT8(L)*INT8(G)        ! global-global normal martix
      AD_SC  = LOC( ARR1(1)  )                    ! vector of scales for local parameters
      AD_BL  = LOC( ARR1(JB) )                    ! local normal vector
      AD_BG  = AD_BL  + 8* L                      ! global normal vector
!
      AD_WL  = LOC( ARR2(JA) )                    ! working vector. Dimension L
      AD_WLL = AD_WL  + 8* L                      ! working matrix L*L/2
      AD_WLG = AD_WLL + 8* (INT8(L)*INT8(L+1))/2  ! working matrix L*G
      IF ( ( LOC(AD_BG) + 8*L - LOC(AD_ALL) )/8  .GT.  M ) THEN
            CALL ERR_LOG ( 8391, -1, 'BACK_B1D', 'Fatal error of '// &
     &          'address calculations' )
            STOP ' BACK: abnormal termination'
      END IF
      IF ( ( LOC(AD_WLG) + 8*INT8(L)*INT8(G) - LOC(AD_WL) )/8  .GT.  M ) THEN
            CALL ERR_LOG ( 8392, -1, 'BACK_B1D', 'Fatal error of '// &
     &          'address calculations' )
            STOP ' BACK: abnormal termination'
      END IF
!
! --- Obtaining local estimates and their covariance matrix
!
      CALL B1D_COV ( G, L, &
     &     %VAL(AD_AGG), %VAL(AD_ALG), %VAL(AD_ALL), &
     &     %VAL(AD_BG),  %VAL(AD_BL), &
     &     %VAL(AD_WLG), %VAL(AD_WLL), %VAL(AD_WL)   )
      ISH=1
      DO 420 J2=1,G
!
! ------ Back copying J2 -th column of     - A(-1){ll} * A{lg} to ARR   and
! ------ putting it in the ORDER which other SOLVE routnes expects to see it
!
         CALL COPY_V ( L, %VAL(AD_WLG + 8*INT8(L)*INT8(J2-1) ), &
     &                     ARR1(JA-1 + LOCS(1,L+J2)) )
!
! ------ Back copying J2-th column of global-global normal matrix
!
         CALL COPY_V ( J2, %VAL(AD_AGG + 8*(ISH-1)), &
     &                      ARR1(JA-1 + LOCS(L+1,L+J2)) )
         ISH = ISH+J2
 420  CONTINUE
!
      RETURN
      END  !#!  BACK_B1D  #!#
