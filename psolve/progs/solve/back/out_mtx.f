      SUBROUTINE OUT_MTX(JPARM,INAME,JNAME,A2,BVECT,TVCT, &
     &                   I_BEG,I_END,J_BEG,J_END,TR, &
     &                   PLIST,ELEMENTS,MPAR,IWDS,I_ARC,IARCNM)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OUT_MTX PROGRAM SPECIFICATION
!
! 1.1 Print out a list of parameters in matrix form, and associated
!     matrix elements, without using the index lists used in OUT_LST.
!
! 1.2 REFERENCES:
!
! 2.  OUT_MTX INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 ELEMENTS
      INTEGER*2 MPAR,IWDS
      INTEGER*2 JPARM(IWDS,MPAR)
      INTEGER*2 IARCNM,I_BEG,I_END,J_BEG,J_END
      INTEGER*2 I_ARC
      REAL*8 A2(ELEMENTS),TVCT(MPAR),BVECT(MPAR)
      CHARACTER*(*) INAME,JNAME
      LOGICAL*2 TR,PLIST
!
! A2,BVECT - Subsets of ARC matrix
! MPAR - Maximum number of parameters
! INAME,JNAME - Names of input CGM/ARC or ARC/ARC files
! I_ARC - Cuurent arc number if ARC/ARC input files, 0 if CGM/ARC
! IARCNM - Current arc number
! IWDS - Length in words of parameter names
! I_BEG,J_BEG - Begining points of input files
! I_END,J_END - Ending points of input files
! JPARM - Parameter list
! ELEMENTS - Number of elements in A2
! TR,PLIST - True if input files are ARC and ARC
!            False if CGM and ARC
! TVCT - Vector consisting of diagonal elements of A2
!
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'baccm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: covmm
!       CALLED SUBROUTINES: prnt_vect
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 INDX4,POS
      INTEGER*2 I,II,J,JJ,K,L,RMOD,IDUM(10),ROW,COL
      INTEGER*2 JNUM,IOUT(10)
      INTEGER*2 JOUT(10),IARC,ITOT,JTOT
      INTEGER*2 MOD
      INTEGER*4 IOS
      CHARACTER DUM*20,XS1*1,XS2*1
      character*3 ptype(mpar)
      REAL*8 CNVRT(MPAR),apriore(mpar)
      INTEGER*2 IROW,ROWI,ROWJ,IPT
      LOGICAL*2 IDONE
      REAL*8    APRIOR(M_GPA)
      COMMON / APRIORI / APRIOR
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  960626  Fix units on wobble, ut1-tai and nutation parameters.
!                Also print totals for wobble and ut1-tai old-style offset and
!                rate parameters.
!                Also fix incompatibilities between the format of the CVRFxx
!                file and the format expected by corel.
!   kdb  960703  Print totals for site parameters.
!   kdb  960820  Fix arc_j range error by changing aprior declaration from
!                max_sta to max_par.
!
! 5.  OUT_MTX PROGRAM STRUCTURE
!
!   first write header
!
      ITOT=I_END-I_BEG+1
      JTOT=J_END-J_BEG+1
      WRITE ( 88, 1101, IOSTAT=IOS ) I_ARC,ITOT,INAME,IARCNM,JTOT,JNAME
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1101 FORMAT('1',1X,I5,1X,I5,1X,A/ &
     &       ' ',1X,I5,1X,I5,1X,A,1X,"Date = 00/00/00",/)
!
!   process parameters and matricies:  if the index lists are to be used,
!   then LISTS is true.
!
!     Get aprioris for the desired eop parameters.  Also get the parameter types
!     and conversion factors for all parameters (although only the eop
!     conversion factors will be meaningful; all others will be 1).
!     Although it make look as if some parameters' values are being set
!     multiple times, parminfoe will only set uninitialized parameters.
!
      CALL USE_GLBFIL_4('ORC' )
      DO IPT = 1,MPAR
        PTYPE(IPT) = '   '
      ENDDO
      IF (PLIST) THEN
        DO IPT = I_BEG,I_END
          IROW = IXC2J(IPT)
          CALL PARMINFOE(IROW,JPARM,MPAR,IWDS,PTYPE,CNVRT,APRIORE )
        ENDDO
      ENDIF
      IF (TR) THEN
        DO IPT = 1,I_END
          IROW = IXC2J(IPT)
          CALL PARMINFOE(IROW,JPARM,MPAR,IWDS,PTYPE,CNVRT,APRIORE )
        ENDDO
      ELSE
        IDONE = .FALSE.
        DO IPT=I_BEG,I_END
          IDONE = .TRUE.
          IROW = IXC2J(IPT)
          CALL PARMINFOE(IROW,JPARM,MPAR,IWDS,PTYPE,CNVRT,APRIORE )
        ENDDO
        IF (IDONE) THEN
          DO IPT=J_BEG,J_END
            IROW = IXC2J(IPT)
            CALL PARMINFOE(IROW,JPARM,MPAR,IWDS,PTYPE,CNVRT,APRIORE )
          ENDDO
        ENDIF
      ENDIF
!
      IF(PLIST) THEN
       DO I=I_BEG,I_END
        ROW=IXC2J(I)
        if (ptype(row).eq."WOB".or.ptype(row).eq."UT1") then
          WRITE ( 88, 1103, IOSTAT=IOS ) I-I_BEG+1,(JPARM(J,ROW),J=1,IWDS), &
     &         TVCT(ROW)*CNVRT(ROW)*CNVRT(ROW), &
     &         BVECT(ROW)*CNVRT(ROW), &
     &         (BVECT(ROW)+APRIORE(ROW))*CNVRT(ROW)
          CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1103     FORMAT(1X,I5,1X,10A2,1X,3D23.16)
        else if (ptype(row).eq."NUT") then
          WRITE ( 88, 1102, IOSTAT=IOS ) I-I_BEG+1,(JPARM(J,ROW),J=1,IWDS), &
     &         TVCT(ROW)*CNVRT(ROW)*CNVRT(ROW), &
     &         BVECT(ROW)*CNVRT(ROW)
          CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
        else if (ptype(row).eq."STA") then !site position parameter
          WRITE ( 88, 1103, IOSTAT=IOS ) I-I_BEG+1,(JPARM(J,ROW),J=1,IWDS), &
     &         TVCT(ROW),BVECT(ROW), &
     &         BVECT(ROW) + APRIOR(ROW)
          CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
        else
          WRITE ( 88, 1102, IOSTAT=IOS ) I-I_BEG+1,(JPARM(J,ROW),J=1,IWDS), &
     &         TVCT(ROW),BVECT(ROW)
          CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1102     FORMAT(1X,I5,1X,10A2,1X,2D23.16)
        ENDIF
        ENDDO
      ENDIF
!
!   now write out the matrix of parameters
!
      IF(TR) THEN
       DO I=I_BEG,I_END
        ROWI = IXC2J(I)
        DO J=1,I-1
         ROWJ = IXC2J(J)
         VECTR(J)=A2(INDX4(IXC2J(I), &
     &      IXC2J(J)))*CNVRT(ROWI)*CNVRT(ROWJ)
        ENDDO
        CALL PRNT_VECT( VECTR, INT2(I-1), INT2(I-I_BEG+1) )
       ENDDO
      ELSE
       DO I=I_BEG,I_END
        ROWI = IXC2J(I)
        DO J=J_BEG,J_END
         ROWJ = IXC2J(J)
         VECTR(J)=A2(INDX4(IXC2J(I), &
     &      IXC2J(J)))*CNVRT(ROWI)*CNVRT(ROWJ)
        ENDDO
        CALL PRNT_VECT( VECTR, JTOT, INT2(I-I_BEG+1) )
       ENDDO
      ENDIF
      RETURN
      END
