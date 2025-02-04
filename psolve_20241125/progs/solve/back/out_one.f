      SUBROUTINE OUT_ONE(IND_ICOV,IND_JCOV,IPARM,JPARM,IPRMS,JPRMS, &
     &                   INAME,JNAME,A2,ROW_OFST,COL_OFST,TVECT, &
     &                   ELEMENTS,MPAR,IWDS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OUT_ONE PROGRAM SPECIFICATION
!
! 1.1 Print out one parameter's information.
!
! 1.2 REFERENCES:
!
! 2.  OUT_ONE INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 ELEMENTS, IOS
      INTEGER*2 MPAR,IWDS,IND_ICOV(MPAR)
      INTEGER*2 IND_JCOV(MPAR),IPARM(IWDS,MPAR),JPARM(IWDS,MPAR)
      INTEGER*2 ROW_OFST,COL_OFST,IPRMS,JPRMS
      REAL*8 A2(ELEMENTS),TVECT(MPAR)
      CHARACTER*12 INAME,JNAME
!
! A2 - Sub-set of ARC matrix
! ELEMENTS - Number elements in A2
! COL_OFST - Column offset - Zero
! ROW_OFST - Row offset - Zero
! IND_ICOV,IND_JCOV - Covariance index lists(same)
! INAME,JNAME - Names input files (same)
! IPARM,JPARM - Parameter list(same)
! IPRMS,JPRMS - Number of arc parameters in an individual solution(same)
! IWDS - Length in words of parameter names
! MPAR - Maximum number of parameters
! TVECT - Vector consisting of diagonal elements of A2
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: covmm
!       CALLED SUBROUTINES: prnt_vect
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 INDX4
      INTEGER*2 I,II,J,JJ,R
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  OUT_ONE PROGRAM STRUCTURE
!
!   first write header
!
      WRITE ( 88, 1101, IOSTAT=IOS ) INAME(1:10),INAME(11:12),JNAME(1:10), &
     &        JNAME(11:12)
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1101 FORMAT('1',/,' Covariances between parameters of ',A10,' ',A2, &
     &       ' and ',A10,' ',A2,//)
      WRITE ( 88, 1102, IOSTAT=IOS )
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1102 FORMAT(9X,'I-Parm',17X,'J-Parm',9x,'Element Indices',10X, &
     &       'Covariance',10x,'Diagonal Element, This Col',/)
!
!   now write the parm's information
!
      I=IND_ICOV(1)
      II=IND_ICOV(1)+ROW_OFST
      J=IND_JCOV(1)
      JJ=IND_JCOV(1)+COL_OFST
      WRITE ( 88, 1103, IOSTAT=IOS ) (IPARM(R,I),R=1,IWDS), &
     &        (JPARM(R,J),R=1,IWDS),I,J,A2(Indx4(II,JJ)),TVECT(J)
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1103 FORMAT(2X,10A2,2X,10A2,7X,'(',I3,',',I3,')',7X,D20.13,5X,D20.13,/)
      WRITE ( 88, '(/," Diagonal Elements ",/)',IOSTAT=ios)
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
      WRITE ( 88, 3334, IOSTAT=IOS ) (TVECT(J),J=1,JPRMS)
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 3334 FORMAT("0",5D20.13/)
!
      RETURN
      END
