      SUBROUTINE OUT_CGM ( CGMNM, A2, BVECT, TVCT, CPARM, CPAR1, CPAR2, &
     &                     ELEMENTS, MPAR, IWDS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OUT_CGM PROGRAM SPECIFICATION
!
! 1.1 Print out a list of parameters in matrix form.
!
! 1.2 REFERENCES:
!
! 2.  OUT_CGM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 ELEMENTS
      INTEGER*2 MPAR, IWDS
      INTEGER*2 CPARM(IWDS,MPAR)
      REAL*8 A2(ELEMENTS),BVECT(MPAR),TVCT(MPAR)
      CHARACTER*(*) CGMNM
      INTEGER*2 CPAR1,CPAR2
!
! A2 - Subset of CGM matrix
! BVECT - Subset of CGM matrix
! CGMNM - Name of input CGM file
! CPARM - Parameter list
! ELEMENTS - Number of elements in A2
! IWDS - Length in words of parameter names
! MPAR - Maximum number of parameters
! TVCT - Vector consisting of diagonal of A2
! CPAR1 - Lower index
! CPAR2 - Higher index
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'baccm.i'
      INCLUDE 'glbc4.i'
      integer*2 numst,isit(4,MAX_STA),numsr,isrc(4,MAX_SRC)
      real*8 apriorp(3,MAX_STA),apriorv(3,MAX_STA),rtime0
      real*8 apriors(3,MAX_SRC)
      COMMON /cgmout/isit,numst,apriorp,apriorv,rtime0, &
     &    apriors,numsr,isrc
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: covmm
!       CALLED SUBROUTINES: prnt_vect
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  INT2_X, INT2_Y, INT2_Z, INT2_A, INT2_V, INT2_R, INT2_D
      PARAMETER ( INT2_X = 2H X ) 
      PARAMETER ( INT2_Y = 2H Y ) 
      PARAMETER ( INT2_Z = 2H Z ) 
      PARAMETER ( INT2_A = 2H A ) 
      PARAMETER ( INT2_V = 2H V ) 
      PARAMETER ( INT2_R = 2H R ) 
      PARAMETER ( INT2_D = 2H D ) 
      INTEGER*8 INDX8
      INTEGER*2 I, I1, J, K, CPARX, ITIM, MON, DAY, YR
      INTEGER*4 IOS, J1, J2
      LOGICAL*2 EQUAL
      REAL*8    APRIOR, FJD
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  900418  Modify to allow STA or SOU covariance option
!   BA   1999.08.05  Allowing use of axis offset parameters.  Also
!                    added error message if unknown station parameter
!                    type used in future.
!
! 5.  OUT_CGM PROGRAM STRUCTURE
!
!   first write header
!
      cparx = cpar2-cpar1+1
      WRITE ( 88, 1101, IOSTAT=IOS ) 0, CPARX, CGMNM, 0, NPARM2, CGMNM
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1101 FORMAT('1',1X,I5,1X,I5,1X,A/ &
     &       ' ',1X,I5,1X,I5,1X,A)
!
      fjd=rtime0*JYEAR__DAYS
      call mdyjl(mon,day,yr,itim,fjd )
      write (88, 1103, IOSTAT=IOS ) YR, MON, DAY
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
1103  format("Ref Date = ",3i2.2/)
!
! If we're doing stations, write the apriori position of the
!   reference station and the date as found in TROT
!
       if (apriorp(1,1).ne.0) then
         call use_glbfil_4('ORC' )
         do i=1,numst
           if (equal( isitn(1,i), INT2(1), fixed_sta(1), INT2(1), INT2(8))) &
     &      then
             write ( 88, 1104, IOSTAT=IOS ) (FIXED_STA(J),J=1,4),VSITEC(1,I)
             CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
             WRITE ( 88, 1105, IOSTAT=IOS ) (FIXED_STA(J),J=1,4),VSITEC(2,I)
             CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
             WRITE ( 88, 1106, IOSTAT=IOS ) (FIXED_STA(J),J=1,4),VSITEC(3,I)
             CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
1104         format(5X,'0 ',4A2,' X COMPONENT',47X,D23.16)
1105         format(5X,'0 ',4A2,' Y COMPONENT',47X,D23.16)
1106         format(5X,'0 ',4A2,' Z COMPONENT',47X,D23.16)
           endif
         enddo
       endif
!
!    Handle station related parameters here.  k of 1, 2, 3, 4
!    corresponds to station X, Y, Z, and axis offset respectively.
!    If "V" appears, for k of 1, 2, 3, than parameter is station
!    velocity in X, Y, or Z direction respectively.  Any other station
!    parameter now causes a terminal error.  BA, 99.08.05.
!
       DO I=cpar1,cpar2
        do j=1,numst
          if (equal( isit(1,j), INT2(1), cparm(1,i), INT2(1), INT2(8) )) then
                IF ( CPARM(5,I) .EQ. INT2_X ) K=1
                IF ( CPARM(5,I) .EQ. INT2_Y ) K=2
                IF ( CPARM(5,I) .EQ. INT2_Z ) K=3
                IF ( CPARM(5,I) .EQ. INT2_A ) K=4
                IF ( CPARM(6,I) .EQ. INT2_V ) THEN
!--- Get station velocity apriori value.
              aprior=apriorv(k,j)
            else
              if(k.lt.1.or.k.gt.4) then
                write(6,"('*** ERROR *** In BACK/outcgm.f.  Undefined' &
     &            ,' station parameter: ',16A2)") &
     &            (cparm(i1,i),i1=1,iwds)
                write(6,"('BACK program execution terminating.!!!')")
                stop
              endif
              if(k.ne.4) then
!--- Get station position apriori value.
                aprior=apriorp(k,j)
              else
!--- Get station axis offset apriori value.
                aprior=vaxof(j)
              endif
            endif
          endif
        enddo
!    (End of handling of station related apriori values.)
!
        do j=1,numsr
          if (equal( isrc(1,j), INT2(1), cparm(1,i), INT2(1), INT2(8) )) then
            if (cparm(5,i) .eq. INT2_R ) then
              if ( cparm(10,i) .eq. INT2_V ) then
                aprior = 0.d0
              else
                aprior = apriors(1,j)
              endif
            else if (cparm(5,i).eq.INT2_D) then
              if (cparm(7,i).eq.INT2_V) then
                aprior = 0.d0
              else
                aprior = apriors(2,j)
              endif
            endif
          endif
        enddo
        WRITE ( 88, 1102, IOSTAT=IOS ) I, (CPARM(J,I),J=1,IWDS), &
     &                                 TVCT(I), BVECT(I), BVECT(I)+APRIOR
        CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1102   FORMAT(1X,I5,1X,10A2,1X,3D23.16)
       ENDDO
!
! --- now write out the matrix of parameters
!
      DO J1=CPAR1+1,CPAR2
         DO J2=CPAR1,J1-1
            VECTR(J)=A2(INDX8(J1,J2))
         ENDDO
         CALL PRNT_VECT ( VECTR(CPAR1), INT2(J1-CPAR1), J2 )
      ENDDO
!
      RETURN
      END  !#!  OUT_CGM  #!#
