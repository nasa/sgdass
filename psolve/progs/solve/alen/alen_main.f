      SUBROUTINE ALEN_MAIN(mat)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ALEN PROGRAM SPECIFICATION
!
! 1.1 Calculate and display arc distances between source pairs,
!     and their error (based on errors in the coordinates and the
!     correlations among the errors in the coordinates).
!
! 1.2 REFERENCES:
!
! 2.  ALEN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: none
!       CALLED SUBROUTINES: arcst
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IFLGA (2,MAX_SRC),IFRACT(21)
      INTEGER*4 INDX4,JK
      REAL*8 STRFE(2,MAX_SRC),OLDSC(2,MAX_SRC)
      REAL*8 NEWSC(2,MAX_SRC)
      LOGICAL*2 KBIT
      INTEGER*2 I,ICT,IFR,IST,IST1,IST2,IST3,IST4,J,K,L
      real*8 ja,jb,js
      real*8 mat(*)
      REAL*8 ARCDST,ARCERR,CHANG,FRACT,OLDARC,OLDERR
      COMMON/ALNEMA/IFLGA,STRFE,OLDSC,NEWSC
      integer*4 I4P255,I4P256
      DATA I4P255,I4P256 /255,256/
!
!  IFLGA: ARRAY TO HOLD THE PARAMETER NUMBER OF ADJUSTED SOURCE
!         PARAMETERS, OR 0 IF THE PARAMETER IS NOT ADJUSTED
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!
!   CAK  820512  Created
!   mwh  940201  Implement dynamic memory allocation for large matrices
!
! 5.  ALEN PROGRAM STRUCTURE
!
      JA = 3*M_GPA
      JB = 2*M_GPA
      JS = M_GPA
      IF ( KSPOOL ) CALL USE_SPOOL('O')
!
      IF(NUMSTR.GT.MAX_SRC)call ferr( INT2(242), 'TOO MANY SOURCES FOR ALEN', &
     &   INT2(0), INT2(0) )
!
!   OPEN AND READ NORMAL EQUATIONS FILE
!
      CALL USE_NRMFIL(mat,NPARAM,'ORC' )
!
!   OPEN AND READ PARFIL
!
      CALL USE_PARFIL('ORC' )
      IF ( .NOT. KGLOBALS ) THEN
           CALL USE_GLBFIL_4('ORC' )
           CALL FLYBY_APRIOR()
      ENDIF
!
!     RECALCULATE THE CORRELATION MATRIX FROM THE COVARIANCE MATRIX
!     ASSUMING THAT THE SIGMAS HAVE ALREADY BEEN EXTRACTED AND
!     STORED IN 'SIG' (I.E., THAT 'COVP' HAS ALREADY RUN.)
!
      DO I=1,NPARAM
        DO J=1,I
          JK=INDX4(I,J)
          mat(ja+JK)=mat(ja+JK)/(mat(js+I)*mat(js+J))
        ENDDO
      ENDDO
!
!     CLEAR ADJUSTED FLAG ARRAY
!
      DO I=1,2
        DO J=1,MAX_SRC
          IFLGA(I,J)=0
        ENDDO
      ENDDO
!
!     CONVERT SIGMAS TO 'SCALED SIGMAS'
!
      DO I=1,NPARAM
        mat(js+I)=mat(js+I)*WRMS(3)
      ENDDO
!
!  THE LAST STATION PARAMETER IS:
!
      ICT=NSLAST
!
!     COUNT SOURCES AND EXTRACT SOURCE INFORMATION
!
      DO IST=1,NUMSTR
        DO K=1,2
          OLDSC(K,IST)=VSTARC(K,IST)
          IF (KBIT(LSTAR(1,K),IST)) THEN
            ICT=ICT+1
            IFLGA(K,IST)=ICT
            NEWSC(K,IST)=VSTARC(K,IST)+mat(jb+ICT)
            STRFE (K,IST)=mat(js+ICT)
          ELSE
            NEWSC(K,IST)=VSTARC(K,IST)
            IFLGA(K,IST)=0
            STRFE(K,IST)=0.D0
          ENDIF
        ENDDO
      ENDDO
!
!     CALCULATE ARCLENGTHS AND STATISTICS
!
      IF(KSPOOL) WRITE (23,107)
107   FORMAT("1....Source Pair....  .Arc (sec)..  .Change.  Sc. Sig.  ", &
     &'Adj',/)
!
!     COUNTER FOR DISTRIBUTION FUNCTION
!
      DO I=1,21
        IFRACT(I)=0
      ENDDO
!
      DO 50 I=1,NUMSTR
        if (iflga(1,i).eq.0.or.iflga(2,i).eq.0) goto 50
      DO 40 J=1,I
        if (iflga(1,j).eq.0.or.iflga(2,j).eq.0) goto 40
      IF (I.EQ.J) GO TO 40
!
!     IT IS NOT NECESSARY TO TAKE SPECIAL CARE NOT TO EXTRACT
!     CORRELATION MATRIX ELEMENTS FOR PARAMETERS WHICH ARE NOT
!     ADJUSTED, SINCE WHATEVER NUMBER IS EXTRACTED ERRONEOUSLY
!     WILL BE MULTIPLIED BY ZERO (FORMAL ERROR) IN ARCST.
!
!     GET ARC DIST BEFORE ADJUSTMENT
!
      CALL ARCST ((OLDSC(1,I)),(OLDSC(2,I)),(OLDSC(1,J)),(OLDSC(2,J)), &
     &            0.D0,0.D0,0.D0,0.D0, &
     &            0.D0,0.D0,0.D0,0.D0,0.D0,0.D0, &
     &            OLDARC,OLDERR )
!
!     GET ADJUSTED ARC LENGTH AND STATISTICS
!
      CALL ARCST ((NEWSC(1,I)),(NEWSC(2,I)),(NEWSC(1,J)),(NEWSC(2,J)), &
     &     (STRFE(1,I)),(STRFE(2,I)),(STRFE(1,J)),(STRFE(2,J)),(mat(ja+ &
     &     INDX4( INT2((IFLGA(1,I))), INT2((IFLGA(2,I)))))),(mat(ja+ &
     &     INDX4( INT2((IFLGA(1,I))), INT2((IFLGA(1,J))) ))),(mat(ja+ &
     &     INDX4( INT2((IFLGA(1,I))), INT2((IFLGA(2,J))) ))),(mat(ja+ &
     &     INDX4( INT2((IFLGA(2,I))), INT2((IFLGA(1,J))) ))),(mat(ja+INDX4(INT2((IFLGA(2,I))), INT2((IFLGA(2,J))) ))),(mat(ja+INDX4( INT2((IFLGA(1, &
     &     J))), INT2((IFLGA(2,J)))))),ARCDST,ARCERR )
      CHANG=ARCDST-OLDARC
      ARCDST=ARCDST*1.296D6/6.2831853072D0
      CHANG =CHANG *1.296D6/6.2831853072D0
!
!     GET SIGMA FROM SIGMA**2
!
      ARCERR=DSQRT(ARCERR)
      ARCERR=ARCERR*1.296D6/6.2831853072D0
!
!     FOUR CHARACTERS TO INDICATE IF THE SOURCE COORDINATE WAS
!     ADJUSTED
!
      IST1=2H**
      IST2=2H**
      IST3=2H**
      IST4=2H**
      IF (IFLGA(1,I).EQ.0) IST1=2H              !
      IF (IFLGA(2,I).EQ.0) IST2=2H              !
      IF (IFLGA(1,J).EQ.0) IST3=2H              !
      IF (IFLGA(2,J).EQ.0) IST4=2H              !
!
      IF(KSPOOL) WRITE (23,106) (ISTRN(L,I),L=1,4),(ISTRN(L,J),L=1,4), &
     &                 ARCDST,CHANG*1.D3,ARCERR*1.D3, &
     &                 IST1,IST2,IST3,IST4
106   FORMAT(1X,4A2," - ",4A2,2X,F12.4,2X,F8.1,2X,F8.1,1X,4A1)
!
!     KEEP TRACK OF DISTRIBUTION FUNCTION
!
      FRACT=CHANG/ARCERR
      IFR=FRACT+11.5D0
      IF (IFR.LT.1) IFR=1
      IF (IFR.GT.21) IFR=21
      IFRACT(IFR)=IFRACT(IFR)+1
40    CONTINUE
50    CONTINUE
!
!     WRITE DENSITY FUNCTION
!
      IF(KSPOOL) WRITE (23,51) IFRACT
51    FORMAT(//," Density function, half sigma per bin :",/, &
     &          '                               **',/, &
     &21I3)
400   CONTINUE
!
!     CLOSE FILES
!
      IF(KSPOOL) CALL USE_SPOOL('C')
      CALL USE_COMMON('WC' )
!
!     TERMINATE
!
      CALL END_PROG()
      END
