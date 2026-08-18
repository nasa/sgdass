      SUBROUTINE PRNT_VECT(VECTR,VLEN,VNUM)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PRNT_VECT PROGRAM SPECIFICATION
!
! 1.1 Print out a vector
!
! 1.2 REFERENCES:
!
! 2.  PRNT_VECT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 VLEN,VNUM
      REAL*8 VECTR(*)
!
! VECTR - Vector to be printed
! VLEN - Vector length
! VNUM - Number of corrent vector
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: out_cgm,out_lst,out_mtx,arc_arc
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 L,K,RMOD
      INTEGER*4 IOS
      INTEGER*4 R
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  PRNT_VECT PROGRAM STRUCTURE
!
      IF(VLEN.GT.0.AND.VLEN.LE.5) THEN
       WRITE ( 88, 3333, IOSTAT=IOS ) VNUM, (VECTR(R),R=1,VLEN)
       call ferr ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 3333  FORMAT(1X,I5,1X,5D23.16)
      ELSE IF(VLEN.GT.0) THEN
       WRITE(88,3333,IOSTAT=IOS ) VNUM,(VECTR(R),R=1,5)
       CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
       K=MOD(VLEN,INT2(5))
       RMOD=(VLEN)/5
       DO L=1,RMOD-1
        WRITE(88,3334,IOSTAT=IOS ) (VECTR(R),R=(5*L)+1,(L+1)*5)
        CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 3334   FORMAT(7X,5D23.16)
       ENDDO
       IF(K.GT.0) THEN
        WRITE ( 88, 3334, IOSTAT=IOS ) (VECTR(R),R=(RMOD*5)+1,(RMOD*5)+K)
        CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
       ENDIF
      ENDIF
      RETURN
      END
