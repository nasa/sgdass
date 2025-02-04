      SUBROUTINE DPPFA_JMG ( A, N, SCAL, B )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 N
      REAL*8 A(* ),SCAL(*),B(*)
!
! CHOLESKY FACTORIZATION OF A SOLVE FORMAT MATRIX
!
! DOUBLE PRECESION VERSION OF LINPACK SPPFA ROUTINE WITH
! DIRECT CALLS TO THE HP VECTOR INSTRUCTION SET AND I*4 INDEXING
!
      INTEGER*2 J,K,I
      INTEGER*4 JJ,KJ,KK,II,iblas0,iblas1,nblas
      REAL*8    S, T, DDOT
!
      iblas0=0
      iblas1=1
!
      JJ=0
      DO J=1,N
        S=0.0D0
        KJ=JJ
        KK=0
        DO K=1,J-1
          KJ=KJ+1
          nblas=k-1
          T=DDOT ( NBLAS, A(KK+1), IBLAS1, A(JJ+1), IBLAS1 )
          T=A(KJ)-T
          KK=KK+K
          T=T/A(KK)
          A(KJ)=T
          S=S+T*T
        ENDDO
        JJ=JJ+J
        S=A(JJ)-S
        IF(S.LE.1.0D-12) THEN
          WRITE(*,*) "Parameter ", J, " failed"
          SCAL(J)=0.0D0
          B(J)=0.0D0
          nblas=j-1
          CALL DCOPY ( NBLAS, 0.0D0, IBLAS0, A(JJ-J+1), IBLAS1 )
          II=JJ
          DO I=J+1,N
            II=II+I-1
            A(II)=0.0D0
          ENDDO
          S=1.0D0
        ENDIF
        A(JJ)=DSQRT(S)
      ENDDO
!
      RETURN
      END  !#!  DPPFA_JMG  #!#
      SUBROUTINE MAT_MUL2 ( A_out,B_in,C_in)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      REAL*8     A_out(2,2),B_in(2,2), C_in(2,2)
      A_out(1,1)=B_in(1,1)*C_in(1,1)+B_in(1,2)*C_in(2,1)
      A_out(1,2)=B_in(1,1)*C_in(1,2)+B_in(1,2)*C_in(2,2)
      A_out(2,2)=B_in(2,1)*C_in(1,2)+B_in(2,2)*C_in(2,2)
      A_out(2,1)=B_in(2,1)*C_in(1,1)+B_in(2,2)*C_in(2,1)
      RETURN
      END   !#!  MAT_MUL2  #!#
      SUBROUTINE CMPLX_MAT_MUL2 ( A_out,B_in,C_in)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      COMPLEX*16 A_out(2,2),B_in(2,2), C_in(2,2)
      A_out(1,1)=B_in(1,1)*C_in(1,1)+B_in(1,2)*C_in(2,1)
      A_out(1,2)=B_in(1,1)*C_in(1,2)+B_in(1,2)*C_in(2,2)
      A_out(2,2)=B_in(2,1)*C_in(1,2)+B_in(2,2)*C_in(2,2)
      A_out(2,1)=B_in(2,1)*C_in(1,1)+B_in(2,2)*C_in(2,1)
      RETURN
      END  !#!  CMPLX_MAT_MUL2  #!#
      SUBROUTINE ADD_USING_COVAR ( z1,cov1,z2,cov2,z_out,cov_out,num_parm)
! add two "measurements" and associated covariances.
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 num_parm                !Number of parameters
      REAL*8     z1(num_parm),cov1(num_parm*(num_parm+1)/2)
      REAL*8     z2(num_parm),cov2(num_parm*(num_parm+1)/2)
      REAL*8     z_out(num_parm),cov_out(num_parm*(num_parm+1)/2)
! this uses the following formula:
!   COV_out=[cov1^-1 + cov2^-1]^-1
!   z_out = [cov1^-1 + cov2^-1]^-1*[cov1^-1*x1+cov2^-1*x2]
!
      REAL*8     z_tmp(num_parm),cov_tmp(num_parm*(num_parm+1)/2)
      z_tmp=z1
      cov_tmp=cov1
      call invert_norm_tri(cov_tmp,z_tmp,num_parm)
!
!
      z_out=z2
      cov_out=cov2
      call invert_norm_tri(cov_out,z_out,num_parm)
!
!
      z_out=z_out+z_tmp
      cov_out=cov_out+cov_tmp
      call invert_norm_tri(cov_out,z_out,num_parm)
      RETURN
      END  !#!  ADD_USING_COVAR  #!#
      SUBROUTINE DET8 ( RMAT, IFIZ_DIM, ILOG_DIM, DET )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      integer*2 ifiz_dim
      integer*2 ilog_dim
      REAL*8      rmat(ifiz_dim,ifiz_dim),det
!
      det=rmat(1,1)*(rmat(2,2)*rmat(3,3)-rmat(2,3)*rmat(3,2)) &
     &       -rmat(1,2)*(rmat(2,1)*rmat(3,3)-rmat(2,3)*rmat(3,1)) &
     &       +rmat(1,3)*(rmat(1,2)*rmat(2,3)-rmat(1,3)*rmat(2,2))
!
      RETURN
      END  !#!  DET8  #!#
      SUBROUTINE MAKE_COV_POSITIVE ( COV_MEAS, IDIM )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 IDIM
      REAL*8      cov_meas(*)
!
      RETURN
      END
      SUBROUTINE MAKE_EIGENVALUES_POSITIVE ( R, KDIM )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
! r must be symmetric!
      integer*2 kdim
      REAL*8      r(kdim,kdim)
      INTEGER*2 i
      integer*2 nrot
      REAL*8      tmp_mat1(kdim,kdim),tmp_vec(kdim)
      REAL*8      tmp_mat2(kdim,kdim)
      REAL*8      rmin, bt(kdim), z(kdim)
!
      tmp_mat1=r
! find eigenvalues of symmetric matrix.
!@      call old_jacobi(tmp_mat1,kdim,kdim,tmp_vec,tmp_mat2,nrot,bt,z)
      CALL LAPACK_JACOBI ( TMP_MAT1, KDIM, KDIM, TMP_VEC, TMP_MAT2, NROT, BT, Z )
!
!
      rmin = tmp_vec(1)
      do i=2,kdim
        if(rmin .gt. tmp_Vec(i))  rmin = tmp_vec(i)
      end do
      if(rmin .lt. 0) then
        write(*,*) "Fixing negative eigenvalues: ",rmin
        do i=1,kdim
           r(i,i) = r(i,i) - 3*rmin
        end do
      endif
!
      RETURN
      END  !#!  MAKE_EIGENVALUES_POSITIVE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INVERT_NORM_TRI ( A, B, NDIMS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      REAL*8     A(*), B(*)
      INTEGER*2  NDIMS
      INTEGER*4  NDIMS_I4
      INTEGER*4    MAX_PAR
      PARAMETER  ( MAX_PAR = 32768 )
      REAL*8     SCALE(MAX_PAR)
      NDIMS_I4 = NDIMS
!
      CALL SCALER    ( A, B, SCALE, NDIMS_I4 )
      CALL DPPFA_JMG ( A, NDIMS_I4, SCALE, B )
      CALL DPPSL     ( A, B, NDIMS_I4 )
      CALL DPPIN     ( A, NDIMS_I4 )
      CALL UNSCALER  ( A, B, SCALE, NDIMS_I4 )
!
      RETURN
      END  !#!  INVERT_NORM_TRI  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INVERT_TRI ( A, NPARA )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! invert square matrix given in triangular form.
!
      REAL*8   A(*)
      INTEGER*2  NPARA
      INTEGER*4  MAX_PAR
      PARAMETER  ( MAX_PAR = 32768 )
      REAL*8     SCALE(MAX_PAR), TEMP(MAX_PAR)
!
      TEMP = 0.0D0
      CALL SCALER ( A, TEMP, SCALE, NPARA )
      CALL DPPFA ( A, NPARA )
      CALL DPPIN ( A, NPARA )
      CALL UNSCALER ( A, TEMP, SCALE, NPARA )
!
      RETURN
      END  !#!  INVERT_TRI  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE JACOBI ( A, N, NP, D, V, NROT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 nmax, ip, iq, i, j
      REAL*8 sm, tresh, g, h, t, theta, c, s, tau
!-----END of imp added lines.
!
      PARAMETER (NMAX=100)
        INTEGER*2 n,np,nrot
      real*8 A(NP,NP),D(NP),V(NP,NP),B(NMAX),Z(NMAX)
      DO 12 IP=1,N
        DO 11 IQ=1,N
          V(IP,IQ)=0.
11      CONTINUE
        V(IP,IP)=1.
12    CONTINUE
      DO 13 IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=0.
13    CONTINUE
      NROT=0
      DO 24 I=1,50
        SM=0.
        DO 15 IP=1,N-1
          DO 14 IQ=IP+1,N
                    SM=SM+ABS(A(IP,IQ))
14        CONTINUE
15      CONTINUE
        IF(SM.EQ.0.)RETURN
        IF(I.LT.4)THEN
          TRESH=0.2*SM/N**2
        ELSE
          TRESH=0.
        ENDIF
        DO 22 IP=1,N-1
          DO 21 IQ=IP+1,N
                    G=100.*ABS(A(IP,IQ))
                    IF((I.GT.4).AND.(ABS(D(IP))+G.EQ. &
     &                 ABS(D(IP))).AND.(ABS(D(IQ))+G.EQ.ABS(D(IQ))))THEN
                      A(IP,IQ)=0.
                    ELSE IF(ABS(A(IP,IQ)).GT.TRESH)THEN
                      H=D(IQ)-D(IP)
                      IF(ABS(H)+G.EQ.ABS(H))THEN
                        T=A(IP,IQ)/H
                      ELSE
                        THETA=0.5*H/A(IP,IQ)
                        T=1./(ABS(THETA)+SQRT(1.+THETA**2))
                        IF(THETA.LT.0.)T=-T
                      ENDIF
                      C=1./SQRT(1+T**2)
                      S=T*C
                      TAU=S/(1.+C)
                      H=T*A(IP,IQ)
                      Z(IP)=Z(IP)-H
                      Z(IQ)=Z(IQ)+H
                      D(IP)=D(IP)-H
                      D(IQ)=D(IQ)+H
                      A(IP,IQ)=0.
                      DO 16 J=1,IP-1
                        G=A(J,IP)
                        H=A(J,IQ)
                        A(J,IP)=G-S*(H+G*TAU)
                        A(J,IQ)=H+S*(G-H*TAU)
16            CONTINUE
                      DO 17 J=IP+1,IQ-1
                        G=A(IP,J)
                        H=A(J,IQ)
                        A(IP,J)=G-S*(H+G*TAU)
                        A(J,IQ)=H+S*(G-H*TAU)
17            CONTINUE
                      DO 18 J=IQ+1,N
                        G=A(IP,J)
                        H=A(IQ,J)
                        A(IP,J)=G-S*(H+G*TAU)
                        A(IQ,J)=H+S*(G-H*TAU)
18            CONTINUE
                      DO 19 J=1,N
                        G=V(J,IP)
                        H=V(J,IQ)
                        V(J,IP)=G-S*(H+G*TAU)
                        V(J,IQ)=H+S*(G-H*TAU)
19            CONTINUE
                      NROT=NROT+1
                    ENDIF
21        CONTINUE
22      CONTINUE
        DO 23 IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=0.
23      CONTINUE
24    CONTINUE
      CALL PAUSE  ( 'JACOBI-294: 50 iterations should never happen' )
      RETURN
      END  !#!  JACOBI  #!#
