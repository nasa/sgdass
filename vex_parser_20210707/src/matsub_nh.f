! ***************************************************************
! * Matrix Manipulation Library					*
! *								*
! * Version ??	   C. Jekeli & N. Habana    since before 2015	*
! *								*		
! ***************************************************************

	SUBROUTINE MATID(A,N)
! C
! C	A is NxN identity matrix
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,N)
	CALL MATZR(A,N,N)
	DO 10 I=1,N
	  A(I,I)=1.D0
   10	CONTINUE
	RETURN
	END
!
!---------------------------------------------------
!
      SUBROUTINE MATAD(A,B,C,N,M)
! C
! C	 C=A+B
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(N,M),C(N,M)
	DO 10 I=1,N
           DO 20 J=1,M
	      C(I,J)=A(I,J)+B(I,J)
  20	   CONTINUE
  10	CONTINUE
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATSB(A,B,C,N,M)
! C
! C	C=A-B
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(N,M),C(N,M)
	DO 10 I=1,N
	   DO 20 J=1,M
	      C(I,J)=A(I,J)-B(I,J)
  20	   CONTINUE  
  10    CONTINUE
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATZR(A,N,M)
! C
! C     A is NxM zero matrix
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M)
        DO 10 I=1,N
 	   DO 20 J=1,M
	      A(I,J)=0.D0
  20	   CONTINUE
  10    CONTINUE	   
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATEQ(A,B,N,M)
! C
! C	 B=A
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(N,M)
	DO 10 I=1,N
	   DO 20 J=1,M
	      B(I,J)=A(I,J)
  20	   CONTINUE
  10    CONTINUE	   
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE CMATEQ(A,B,N,M)
! C
! C	 B=A (all complex matrices)
! C
	IMPLICIT COMPLEX*16 (A-H,O-Z)
	DIMENSION A(N,M),B(N,M)
	DO 10 I=1,N
	   DO 20 J=1,M
	      B(I,J)=A(I,J)
  20	   CONTINUE
  10    CONTINUE	   
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATSC(A,X,B,N,M)
! C
! C B=X*A  (X is a scalar)
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(N,M)
	DO 10 I=1,N
 	   DO 20 J=1,M
	      B(I,J)=X*A(I,J)
   20	   CONTINUE
   10   CONTINUE	   
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATPD(A,B,C,N,M,L)
! C
! C	 C=A*B
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(M,L),C(N,L)
	DO 10 I=1,N
	   DO 20 J=1,L
	      D=0.D0
	      DO 30 K=1,M
	         D=D+A(I,K)*B(K,J)
  30          CONTINUE
	      C(I,J)=D
  20	   CONTINUE
  10    CONTINUE	   
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE CMATPD(A,B,C,N,M,L)
! C
! C	 C=A*B (all complex matrices)
! C
	IMPLICIT COMPLEX*16 (A-H,O-Z)
	DIMENSION A(N,M),B(M,L),C(N,L)
	DO 10 I=1,N
	   DO 20 J=1,L
	      D=(0.D0,0.D0)     
	      DO 30 K=1,M
	         D=D+A(I,K)*B(K,J)
  30          CONTINUE
	      C(I,J)=D
  20	   CONTINUE
  10    CONTINUE	     
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATTP(A,B,N,M)
! C
! C	 B=A-transpose
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(M,N)
	DO 10 I=1,N
	   DO 20 J=1,M
	      B(J,I)=A(I,J)
  20 	   CONTINUE
  10    CONTINUE	   
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATNG(A,B,N,M)
! C
! C B=-A
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(N,M)
	DO 10 I=1,N
	   DO 20 J=1,M
	      B(I,J)=-A(I,J)
  20 	   CONTINUE
  10    CONTINUE	   
	RETURN
	END	
!
!---------------------------------------------------
!	         				
	SUBROUTINE MATINV(A,N,P,B,IJOB)
! C
! C	B=(A-inverse)*B  (A is NxN, B is NxP)
! C	IJOB=0 on first call, =1 on subsequent calls (same A)
! C	A is destroyed.
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER P
	DIMENSION A(N,1),B(N,1)
	IF (N .EQ. 1) GO TO 220
	IF (IJOB .EQ. 1) GO TO 80
	N1=N-1
	DO 60 I=1,N1
	   NI=N-I
	   I1=I+1
	   DO 40 M=1,NI
	      MI=M+I
	      A(MI,I)=-A(MI,I)/A(I,I)
	      DO 20 L=I1,N
	         A(MI,L)=A(MI,L)+A(I,L)*A(MI,I)
  20	      CONTINUE
  40	   CONTINUE	
  60	CONTINUE
!  
  80	CONTINUE
!
	DO 120 I=2,N
	   I1=I-1
	   DO 105 K=I,N
	      DO 100 J=1,P
	         B(K,J)=B(K,J)+B(I1,J)*A(K,I1)
 100	      CONTINUE
 105	   CONTINUE	   
 120	CONTINUE
	DO 200 J=1,P
	   B(N,J)=B(N,J)/A(N,N)
	   DO 160 K=1,N1
              NK=N-K
              SUM=B(NK,J)
	      DO 140 M=1,K
	         NKM=NK+M
	         SUM=SUM-A(NK,NKM)*B(NKM,J)
 140	      CONTINUE
	      B(NK,J)=SUM/A(NK,NK)
 160	   CONTINUE
 200	CONTINUE
	RETURN
!
 220	CONTINUE
!
	DO 240 J=1,P
	  B(N,J)=B(N,J)/A(N,N)
 240	CONTINUE
!
	RETURN
	END
!
!---------------------------------------------------
!
! CCCCCCCCCCCC WHAT DOES THIS DO	CCCCCCCC
	SUBROUTINE MATQTP(A,B,Q,P,M)
	IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER P,Q
	DIMENSION A(M,M),B(M,M)
	IQ=-Q
	DO 60 I=1,P
	   JQ=IQ
	   IQ=IQ+Q
	   DO 50 J=I,P
	      JQ=JQ+Q
	      DO 40 K=1,Q
	         K1=IQ+K
	         K2=JQ+K
	         DO 30 L=1,Q
	            L1=JQ+L
	            L2=IQ+L
	            B(K2,L2)=A(K1,L1)
	            B(K1,L1)=A(K2,L2)
  30	        CONTINUE
  40	      CONTINUE
  50       CONTINUE	   
  60    CONTINUE	   
	RETURN
	END
!
!---------------------------------------------------
!
! CCCCCCCCCCC  WHAT DOES THIS DO	CCCCCCC
	SUBROUTINE MATEXR(A,B,Q,P,M)

	IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER P,Q
	DIMENSION A(M,M),B(M,M)
	I1=-Q
	I2=M
	DO 50 I=1,P
	   I1=I1+Q
	   I2=I2-Q
	   DO 40 J=1,M
	      DO 30 K=1,Q
	         K1=I1+K
	         K2=I2+K
	         B(K2,J)=A(K1,J)
  30	      CONTINUE
  40	   CONTINUE
  50    CONTINUE	   
	RETURN
	END
! CCCCCCCCCCCCC
!
!---------------------------------------------------
!
	SUBROUTINE MATTPD(A,B,C,N,M,L)
! C
! C	 C=(A-transpose)*B
! C	
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(M,N),B(M,L),C(N,L)
	DO 30 I=1,N
	   DO 20 J=1,L
	      D=0.D0
	      DO 10 K=1,M
	         D=D+A(K,I)*B(K,J)
  10	      CONTINUE
	      C(I,J)=D
  20       CONTINUE	   
  30	CONTINUE
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATPDT(A,B,C,N,M,L)
! C
! C	C=A*(B-transpose)
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(L,M),C(N,L)
	DO 30 I=1,N
	   DO 20 J=1,L
	      D=0.D0
	      DO 10 K=1,M
	         D=D+A(I,K)*B(J,K)
  10	      CONTINUE
	      C(I,J)=D
  20       CONTINUE	   
  30	CONTINUE
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE MATADT(A,B,C,N,M)
! C
! C	C=A+(B-transpose)
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,M),B(M,N),C(N,M)
	DO 20 I=1,N
	   DO 10 J=1,M
	      C(I,J)=A(I,J)+B(J,I)
  10	   CONTINUE
  20    CONTINUE	     
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE CMATINV(A,N,P,B,IJOB)
! C
! C	Same as MATINV, but for complex arrays
! C
	IMPLICIT COMPLEX*16 (A-H,O-Z)
        INTEGER P
	DIMENSION A(N,N),B(N,P)
	IF (N .EQ. 1) GO TO 220
	IF (IJOB .EQ. 1) GO TO 80
	N1=N-1
	DO 60 I=1,N1
	   NI=N-I
	   I1=I+1
	   DO 40 M=1,NI
	      MI=M+I
	      A(MI,I)=-A(MI,I)/A(I,I)
	      DO 20 L=I1,N
	         A(MI,L)=A(MI,L)+A(I,L)*A(MI,I)
  20	      CONTINUE
  40	   CONTINUE	
  60	CONTINUE
!
  80	CONTINUE
!
	DO 120 I=2,N
	   I1=I-1
	   DO 105 K=I,N
	      DO 100 J=1,P
	         B(K,J)=B(K,J)+B(I1,J)*A(K,I1)
 100	      CONTINUE
 105	   CONTINUE	   
 120	CONTINUE
	DO 200 J=1,P
	   B(N,J)=B(N,J)/A(N,N)
	   DO 160 K=1,N1
              NK=N-K
              SUM=B(NK,J)
	      DO 140 M=1,K
	         NKM=NK+M
	         SUM=SUM-A(NK,NKM)*B(NKM,J)
 140	      CONTINUE
	      B(NK,J)=SUM/A(NK,NK)
 160	   CONTINUE
 200	CONTINUE
	RETURN
!
 220	CONTINUE
!
	DO 240 J=1,P
	   B(N,J)=B(N,J)/A(N,N)
 240	CONTINUE
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE AINVRS(A,N)
! C
! C	Subroutine to invert a general non-singular matrix in place.  All
! C	pivot elements are assumed to be non-zero.  The matrix is A and has
! C	dimension N.  The inverse is returned in A.  It is computed by Gaussian
! C	elimination.
! C
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION A(N,N)
! C
	IF (N .LT. 3) THEN
	    PRINT*,'Subroutine AINVRS is not coded for matrices'
	    PRINT*,'with dimension less than 3x3.'
	    STOP
	END IF
! C
! C	Compute elements of transformation matrices that transform A to be upper
! C	triangular - store these in lower triangle of A - and compute upper
! C	triangular part of the transformed A.
	N1=N-1
	DO 100 M=1,N1
	   M1=M+1
	   DO 80 J=M1,N
	      A(J,M)=-A(J,M)/A(M,M)
	      DO 60 K=M1,N
	         A(J,K)=A(J,K)+A(J,M)*A(M,K)
   60	      CONTINUE
   80	   CONTINUE
  100	CONTINUE
! C
! C Multiply the transformation matrices.
	DO 200 M=2,N1
	   MP1=M+1
	   MM1=M-1
	   DO 185 J=MP1,N
	      DO 180 K=1,MM1 
	         A(J,K)=A(J,K)+A(J,M)*A(M,K)
  180	      CONTINUE
  185	   CONTINUE
  200	CONTINUE
! C
! C	Compute elements of transformation matrices that transform the upper-
! C	triangularized A into the identity matrix and store them in the upper
! C	triangle of A.
	A(1,1)=1.D0/A(1,1)
	DO 240 K=2,N
	   A(1,K)=A(1,K)*A(1,1)
  240	CONTINUE
! C
	DO 300 M=2,N1
	   MP1=M+1
	   MM1=M-1
	   A(M,M)=1.D0/A(M,M)
	   DO 280 K=MP1,N
	      A(M,K)=A(M,K)*A(M,M)
	      DO 260 J=1,MM1
	         A(J,K)=A(J,K)-A(J,M)*A(M,K)
  260	      CONTINUE
  280	   CONTINUE
  300	CONTINUE
! C
	A(N,N)=1.D0/A(N,N)
! C
	DO 400 M=2,N
	   M1=M-1
	   DO 380 J=1,M1
	      A(J,M)=-A(J,M)*A(M,M)
  380	   CONTINUE
  400	CONTINUE
! C
! C	Multiply transformation matrices with previous lower triangular transformation.
	DO 500 M=2,N
	   M1=M-1
	   DO 465 J=1,M1
	      DO 460 K=1,M1
	         A(J,K)=A(J,K)+A(J,M)*A(M,K)
  460	      CONTINUE
  465	    CONTINUE
	   DO 480 K=1,M1
	      A(M,K)=A(M,M)*A(M,K)
  480	   CONTINUE
  500	CONTINUE
! C
	RETURN
	END
!
!---------------------------------------------------
!
	SUBROUTINE CAINVRS(A,N)
! C
! C	Subroutine to invert a general non-singular complex matrix in place.  All
! C	pivot elements are assumed to be non-zero.  The matrix is A and has
! C	dimension N.  The inverse is returned in A.  It is computed by Gaussian
! C	elimination.
! C
	IMPLICIT COMPLEX*16 (A-H,O-Z)
	DIMENSION A(N,N)
! C
        ONE=(1.D0,0.D0)
! C
	IF (N .LT. 3) THEN
	    PRINT*,'Subroutine AINVRS is not coded for matrices'
	    PRINT*,'with dimension less than 3x3.'
	    STOP
	END IF
! C
! C	Compute elements of transformation matrices that transform A to be upper
! C	triangular - store these in lower triangle of A - and compute upper
! C	triangular part of the transformed A.
	N1=N-1
	DO 100 M=1,N1
	   M1=M+1
	   DO 80 J=M1,N
	      A(J,M)=-A(J,M)/A(M,M)
	      DO 60 K=M1,N
	         A(J,K)=A(J,K)+A(J,M)*A(M,K)
   60	      CONTINUE
   80	   CONTINUE
  100	CONTINUE
! C
! C	Multiply the transformation matrices.
	DO 200 M=2,N1
	   MP1=M+1
	   MM1=M-1
	   DO 185 J=MP1,N
	      DO 180 K=1,MM1 
	         A(J,K)=A(J,K)+A(J,M)*A(M,K)
  180	      CONTINUE
  185	   CONTINUE   
  200	CONTINUE
! C
! C	Compute elements of transformation matrices that transform the upper-
! C	triangularized A into the identity matrix and store them in the upper
! C	triangle of A.
	A(1,1)=ONE/A(1,1)
	DO 240 K=2,N
	   A(1,K)=A(1,K)*A(1,1)
  240	CONTINUE
! C
	DO 300 M=2,N1
	   MP1=M+1
	   MM1=M-1
	   A(M,M)=ONE/A(M,M)
	   DO 280 K=MP1,N
	      A(M,K)=A(M,K)*A(M,M)
	      DO 260 J=1,MM1
	         A(J,K)=A(J,K)-A(J,M)*A(M,K)
  260	      CONTINUE
  280	   CONTINUE
  300	CONTINUE
! C
	A(N,N)=ONE/A(N,N)
! C
	DO 400 M=2,N
	   M1=M-1
	   DO 380 J=1,M1
	      A(J,M)=-A(J,M)*A(M,M)
  380	   CONTINUE
  400	CONTINUE
! C
! C Multiply transformation matrices with previous lower triangular transformation.
	DO 500 M=2,N
	   M1=M-1
	   DO 465 J=1,M1
	      DO 460 K=1,M1
	         A(J,K)=A(J,K)+A(J,M)*A(M,K)
  460	      CONTINUE
  465	   CONTINUE   
	   DO 480 K=1,M1
	      A(M,K)=A(M,M)*A(M,K)
  480	   CONTINUE
  500	CONTINUE
! C
	RETURN
	END
