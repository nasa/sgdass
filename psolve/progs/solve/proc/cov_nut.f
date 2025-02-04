      SUBROUTINE COV_NUT(THISJD,FSTJD,INC,NUM,PI,CVINF,WHO,NUTCONS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COV_NUT PROGRAM SPECIFICATION
!
! 1.1 Get the constraint matrix
!
! 1.2 REFERENCES:
!
! 2.  COV_NUT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NUM,INC
      REAL*8 FSTJD,THISJD,PI
      REAL*8 NUTCONS(2)
      CHARACTER*(*) WHO(2)
!
! NUTCONS - Nutation parameter constraint
! FSTJD - First J.D.
! INC - Interval between epochs
! NUM - Number of epochs
! PI - Pi in 11-digit precision
! THISJD - Epoch for which shorter position and parameter names lists
!          should be generated
! WHO - List of nutation offset parameter names for this epoch
!
! 2.3 OUTPUT Variables:
!
      REAL*8 CVINF(3)
!
! CVINF - THe solve format matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: /adjst/eop_share
!       CALLED SUBROUTINES: dppfa,dppin
!
! 3.  LOCAL VARIABLES
!
      REAL*8 CVDUM(3),COV1(3),COV2(3),RSSl,RSSo
      REAL*8 lnSG,obSG,loCR,DAY1,DIFJD,loCV
      REAL*8 lnCV,obCV,DAY2,SIGl,SIGo,T,DT
      REAL*8 A,B,C,Rad_per_Masec,Sec_per_Masec,D
      INTEGER*4 INT
      INTEGER*2 I,J,K,M,N,CNSTR_REC,ERR_NUM(3)
      CHARACTER*150 errstr
      character*80 bufstr
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  940614  Created, based on cov_eop
!
! 5.  COV_NUT PROGRAM STRUCTURE
!
!   conversion constants
!
      Rad_per_Masec=PI/(180.D0*3600.D0*1000.D0)
      Sec_per_Masec=1.D0/15000.D0
!
!   within an interval(or on an endpoint), get the left endpoint
!
      DIFJD=THISJD-FSTJD
      CNSTR_REC=INT(DIFJD/INC)+1 !get left endpoint
      IF(CNSTR_REC.LT.1.OR.CNSTR_REC.GT.NUM) THEN
        WRITE(errstr,*) "LEFT POINT - NUT EPOCH NOT WITHIN &
     &                   CONSTRAINTTABLE",CNSTR_REC
        call ferr( INT2(151), errstr, INT2(0), INT2(0) )
      ENDIF
      DAY1=FSTJD+(CNSTR_REC-1)*INC
      lnSG=lnSIG(CNSTR_REC)
      obSG=obSIG(CNSTR_REC)
      loCR=lnobCOR(CNSTR_REC)
!
!   how far into the interval is the point of interpolation?
!
      DT=THISJD-DAY1
!
!   first convert sigmas
!
      SIGl=lnSG
      SIGo=obSG
!
!  GET VALUES TO RSS TO DIAGONAL
!
      RSSl=NUTCONS(1)*Rad_per_Masec
      RSSo=NUTCONS(2)*Rad_per_Masec
!
!   form the covariance matrix for the left endpoint in all of its glory
!
      COV1(1)=(SIGl*SIGl)+RSSl*RSSl
      COV1(2)=loCR*(SIGl*SIGo)
      COV1(3)=(SIGo*SIGo)+RSSo*RSSo
!
!   now obtain the right endpoint
!
      CNSTR_REC=CNSTR_REC+1 !get right endpoint
      IF(CNSTR_REC.LT.1.OR.CNSTR_REC.GT.NUM) THEN
        WRITE(errstr,*) "RIGHT POINT - EOP EPOCH NOT WITHIN &
     &                   CONSTRAINTTABLE",CNSTR_REC
        call ferr( INT2(152), errstr, INT2(0), INT2(0) )
      ENDIF
      DAY2=FSTJD+(CNSTR_REC-1)*INC
      lnSG=lnSIG(CNSTR_REC)
      obSG=obSIG(CNSTR_REC)
      loCR=lnobCOR(CNSTR_REC)
!
!   what is the length(in days) of this interval?
!
      T=DAY2-DAY1
!
!   first convert sigmas
!
      SIGl=lnSG
      SIGo=obSG
!
!   form the covariance matrix for the right endpoint in all of its glory
!
      COV2(1)=(SIGl*SIGl)+RSSl*RSSl
      COV2(2)=loCR*(SIGl*SIGo)
      COV2(3)=(SIGo*SIGo)+RSSo*RSSo
!
!   now perform straight-line interpolation between the two days by
!
      CVDUM(1)=(COV1(1)*(1-DT/T))+(COV2(1)*(DT/T))
      CVDUM(2)=(COV1(2)*(1-DT/T))+(COV2(2)*(DT/T))
      CVDUM(3)=(COV1(3)*(1-DT/T))+(COV2(3)*(DT/T))
!
!   look at the diagonal elements and replace if necessary
!
!   to grow linearly
      D=2.D0/5.0D0 !2 masec error/5 days if diagonals less than this
      A=MIN(ABS(T-DT),DT)*D
      B=(A*Rad_per_Masec)**2
!
      IF(CVDUM(1).LT.B) CVDUM(1)=B
      IF(CVDUM(3).LT.B) CVDUM(3)=B
!
!   scale for inverting
!
      SIGl=DSQRT(CVDUM(1))
      SIGo=DSQRT(CVDUM(3))
!
      CVDUM(1)=1.D0
      CVDUM(2)=CVDUM(2)/(SIGl*SIGo)
      CVDUM(3)=1.D0
!
!   look at who is not present, zero out rows/cols accordingly
!
      DO I=1,3
       CVINF(I)=0.D0
      ENDDO
      IF(INDEX(WHO(1),'LONGITUD').NE.0) THEN
       CVINF(1)=CVDUM(1)
       IF(INDEX(WHO(2),'OBLIQUIT').NE.0) THEN
        CVINF(2)=CVDUM(2)
        CVINF(3)=CVDUM(3)
       endif
      ELSE IF(INDEX(WHO(1),'OBLIQUIT').NE.0) THEN
       CVINF(3)=CVDUM(3)
      ENDIF
!
!   Cholesky decomposition routines:  see LINPAK for details
!
      CALL DPPFA( CVINF, INT2(2) )
      CALL DPPIN( CVINF, INT2(2) )
!
      CVINF(1)=CVINF(1)/(SIGl*SIGl)
      CVINF(2)=CVINF(2)/(SIGl*SIGo)
      CVINF(3)=CVINF(3)/(SIGo*SIGo)
!
      RETURN
      END
