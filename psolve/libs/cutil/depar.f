      SUBROUTINE DEPAR()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DEPAR PROGRAM SPECIFICATION
!
! 1.1 Turn off flags for arc parameters in preparation for building
!      list of globals only.
!
! 1.2 REFERENCES:
!
! 2.  DEPAR INTERFACE
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
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: equal,kbit,sbit
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,IB,IDEFAL,ICHCM,K,IBD,ii
      LOGICAL*2 KBIT,EQUAL
!
! I,J - Loop indices
! IB,IBD - Bit values for sbit calls
! IDEFAL -
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2022.08.23  Adder support of Ionosphere scale estimation
!
! 5.  DEPAR PROGRAM STRUCTURE
!
! Set number of clocks and atmospheres to zero for all stations
!
      DO I=1,MAX_ARC_STA
         NUMCLK(I)=0
         NUMATM(I)=0
         NUMGRAD(I)=0
      ENDDO
!
      CALL SBIT( IUEN, INT2(2), INT2(0) )
!
      LNUT(1)=0
      NROT_A1(1)=0
      NROT_A1(2)=0
      NROT=0
      LOGBCL=.FALSE.
!
! Turn off station position arc parameters
!
      IDEFAL=0
      IF(KCSTA) IDEFAL=1
      DO I=1,NUMSTA
        IBD=IDEFAL
        DO J=1,NACSTA
          IF(EQUAL( ISELAR(IACSTA+(J-1)*4), INT2(1), ISITN(1,I), INT2(1), &
     &       INT2(8))) THEN
            IBD=1-IDEFAL
            GO TO 20
          ENDIF
        ENDDO
20      CONTINUE
        DO J=1,3
          IB=IBD
          IF(CARCMP(J:J).EQ.'-') IB=0
          IF(KBIT(LSITEC(1,J),I)) CALL SBIT(LSITEC(1,J),I,IB)
        ENDDO
      ENDDO
!
! Turn off source position arc parameters
!
      IDEFAL=0
      IF(KCSRC) IDEFAL=1
      DO I=1,NUMSTR
        IB=IDEFAL
        DO J=1,NACSRC
          IF(EQUAL( ISELAR(IACSRC+(J-1)*4), INT2(1), ISTRN(1,I), INT2(1), &
     &       INT2(8))) THEN
            IB=1-IDEFAL
            GO TO 30
          ENDIF
        ENDDO
30      CONTINUE
        DO J=1,2
          IF(KBIT(LSTAR(1,J),I)) CALL SBIT(LSTAR(1,J),I,IB)
          IF(KBIT(LPROP(1,J),I)) CALL SBIT(LPROP(1,J),I,IB)
        ENDDO
      ENDDO
!
! Turn off axis offset arc parameters
!
      idefal=0
      if(kcaxis) idefal=1
      do i=1,numsta
        if(kbit(laxof(1),i)) call sbit(laxof(1),i,idefal)
      enddo
!
! Turn off earth tides arc parameters
!
      idefal=0
      if(kctide) idefal=1
      do i=1,numsta
        do j=1,3
          if(kbit(ltide(1,j),i)) call sbit(ltide(1,j),i,idefal)
        enddo
      enddo
      ios_est = ios__undf
!
!  turn off baseline-dependent clocks
!
!       logbcl = .FALSE.
!       do i=1,numsta
!         ii = i+1
!         do j = ii,numsta
!           call sbit(iclock(1,i),j,0)
!         enddo
!       enddo
!
! Turn off user_defined arc parameters
!
      kglobonly = .TRUE.
!
      RETURN
      END
