      SUBROUTINE DO_CLK_9612 ( FAST_MODE, FAST_DBG, WHO_STA, LPARM, IPARMS, A, &
     &                         B3DOBJ, B1B3DOBJ, CNSTROBJ  )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! ************************************************************************
! *                                                                      *
! *     Archaic version of SOLVE at 01-DEC-96. Used with pre DEC-96      *
! *   SOLVE compatibility mode or for clocks with non-uniform segments.  *
! *                                                                      *
! ************************************************************************
!
! 1.  DO_CLK PROGRAM SPECIFICATION
!
! 1.1 Apply clock constraints as requested.
!
! 1.2 REFERENCES:
!
! 2.  DO_CLK INTERFACE
!
! 2.1 Parameter File
      INCLUDE  'solve.i'
      INCLUDE  'fast.i'
      INCLUDE  'cnstr.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  FAST_MODE, FAST_DBG
      ADDRESS__TYPE :: IAD
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D, FULL_B1B3D
      REAL*8     APOS1
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IPARMS
      REAL*8 A(*)
      character*(*) LPARM(M_GPA)
      CHARACTER*8   WHO_STA(*)
!
! A - Normal equation matrix
! IPARM - Array of parameter names
! IPARMS - Number of parameters
! WHO_STA - Array of station names
!
! 2.3 OUTPUT Variables:
!
! A - Normal equation matrix with constraints added
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: add_clk
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*4    FALSE_L4
      PARAMETER  ( FALSE_L4 = .FALSE. )
      INTEGER*2  IERR, HR, MIN
      INTEGER*4  CPARM(M_GPA), END_CPARM, I, J, K
      INTEGER*4  CHK_STRT, CHK_END, ICHK, INIT_CLK(MAX_ARC_STA,3)
      INTEGER*4  N4
      INTEGER*8  POS1, INDX8
      CHARACTER*20 CLK_PARM*13, DUMRA
      REAL*8     CONSTRAINT(MAX_CLK), SIGMA, TIME_INTERVAL, TIM8(MAX_CLK)
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
!
!     Transformation INT2 --> INT4
!
! 4.  HISTORY
!   WHO  WHEN    WHAT
!   MWH  910524  Modify to accomodate new parameterization scheme
!   jmg  960610  Remove holleriths.
!   pet  971024  Add support of B3D and B1B3D
!   pet  971203  Added logic for bypassing deselected stations
!   pet  980109  Fixed bug connected with bypassing deselected stations
!   pet  980119  Rewrote to support data structure CNSTR
!   pet  980205  Added writing information about the type of applied constraint.
!
! 5.  DO_CLK_9612 PROGRAM STRUCTURE
!
      DO I=1,NUMSTA
!
! ------ Check: was the I-th station in solution?
!
         IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 810
         SIGMA=DBLE(SCCNST(I))
         SIGMA=(SIGMA*1.d-14)*3600.d0
         CALL CINDEX_PARM( 'CL1', CPARM, LPARM, IPARMS, END_CPARM, DUMRA, &
     &        FALSE__L2, WHO_STA(I) )
        do j=1,3
          init_clk(i,j) = cparm(1)+j-1
          if (end_cparm.eq.0) init_clk(i,j) = 0
        enddo
        do j=1,end_cparm
          read(lparm(cparm(j))(17:18),'(i2)') hr
          read(lparm(cparm(j))(19:20),'(i2)') min
          tim8(j) = hr + min/60.d0
        enddo
        constraint(1) = 0.d0
        chk_strt = 1
        do j=2,end_cparm
          time_interval = tim8(j)-tim8(j-1)
          if (time_interval.le.0) time_interval = time_interval+24.d0
          constraint(j) = 1/((sigma*time_interval)**2)
!         if(clock_interval.eq.0) constraint(j) = 0.d0
          if ( lparm(cparm(j))(9:10).eq.'C0' ) then
!           write(*,*) "C0", j
            constraint(j) = 0.d0
            chk_end = j-1
            do ichk=chk_strt,chk_end
                pos1=indx8(cparm(ichk),cparm(ichk))
                IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD   .OR. &
     &               FAST_MODE .EQ. F__B1D     ) THEN
                     APOS1 = A(POS1)
                  ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
                     IAD = FULL_B3D ( B3DOBJ, CPARM(ICHK), &
     &                            CPARM(ICHK), %VAL(0), %VAL(0), &
     &                            %VAL(0), %VAL(0) )
                     CALL COPY_V ( 1, %VAL(IAD), APOS1 )
                  ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                     IAD = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, CPARM(ICHK), &
     &                                  CPARM(ICHK), %VAL(0), %VAL(0), &
     &                                  %VAL(0), %VAL(0) )
                     CALL COPY_V ( 1, %VAL(IAD), APOS1 )
                END IF
                if ( apos1 .ne. 0 ) goto 100
            enddo
            do ichk=chk_strt,chk_end
                constraint(ichk) = 0.d0
            enddo
100         chk_strt=j
          endif
        enddo
        do j=chk_strt,end_cparm
           pos1 = indx8(cparm(j),cparm(j))
           IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD   .OR. &
     &          FAST_MODE .EQ. F__B1D     ) THEN
                APOS1 = A(POS1)
             ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
                IAD = FULL_B3D ( B3DOBJ, CPARM(J), &
     &                           CPARM(J), %VAL(0), %VAL(0), %VAL(0), &
     &                           %VAL(0) )
                CALL COPY_V ( 1, %VAL(IAD), APOS1 )
             ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                IAD = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, CPARM(J), &
     &                             CPARM(J), %VAL(0), %VAL(0), %VAL(0), &
     &                             %VAL(0))
                CALL COPY_V ( 1, %VAL(IAD), APOS1 )
           END IF
           if ( apos1 .ne. 0 ) goto 200
        enddo
        do j=chk_strt,end_cparm
          constraint(j) = 0.d0
        enddo
200     continue
!
! ----- Add the constraint
!
        DO J=2,END_CPARM
           IF ( DABS(CONSTRAINT(J)) .GT. 1.D-30 ) THEN
!
! ------------- Add information about the type of the constraint applied
!
                CALL ADD_TYCNS ( 'CLL_RATE', 'Clock rate between '// &
     &                           'segments', '10**-14sec/sec', DBLE(SCCNST(I)), &
     &                           FALSE_L4, CNSTROBJ )
           END IF
        END DO
!
        CALL ADD_CLK ( END_CPARM, CPARM, CONSTRAINT, CNSTROBJ )
 810    CONTINUE
      ENDDO
!
      RETURN
      END  !#!  DO_CLK_9612  #!#
