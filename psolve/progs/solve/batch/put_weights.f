      SUBROUTINE put_weights(constant,weight_type_ur)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  PUT_WEIGHTS PROGRAM SPECIFICATION
!
! 1.1 Put re-weight constants in NAMFIL.
!
! 1.2 REFERENCES:
!
! 2.  PUT_WEIGHTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      real*8 constant(4,MAX_ARC_BSL)
      character*1 weight_type_ur
!
! CONSTANT - Re-weight constants to be placed in NAMFIL
! weight_type_ur - A for by Arc weight file , S for by Site
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      character*72 jbuf
      character*8  jbasl(2)
      integer*2    idb, ierr
      INTEGER*4    IOS
      LOGICAL*2 kfirst
      real*8 ee(4)
      integer*2 ibline
      data idb/1/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  970204  New site weighting feature.
!
! 5.  PUT_WEIGHTS PROGRAM STRUCTURE
!
      ierr = 0
      ibline = 0
      call getcard( idb, 'REWT', INT2(1), jbuf, ierr )
      do while (ierr .eq. 0)
!
         IF ( INDEX ( JBUF(23:32), '*' ) > 0 ) JBUF(23:32) = '      0.00'
         IF ( INDEX ( JBUF(33:42), '*' ) > 0 ) JBUF(33:42) = '      0.00'
         IF ( INDEX ( JBUF(43:52), '*' ) > 0 ) JBUF(43:52) = '      0.00'
         IF ( INDEX ( JBUF(53:62), '*' ) > 0 ) JBUF(53:62) = '      0.00'
         READ ( JBUF, '(5X,A8,1X,A8,4F10.2,8X)', IOSTAT=IOS ) JBASL, EE
         CALL FERR ( INT2(IOS), "Decoding REWT card", INT2(0), INT2(0) )
         IBLINE = IBLINE + 1
         IF ( WEIGHT_TYPE_UR .EQ. 'A' ) THEN
              EE(1)=CONSTANT(1,1)
              EE(2)=CONSTANT(2,1)
              EE(3)=CONSTANT(3,1)
              EE(4)=CONSTANT(4,1)
           ELSE
              EE(1)=CONSTANT(1,IBLINE)
              EE(2)=CONSTANT(2,IBLINE)
              EE(3)=CONSTANT(3,IBLINE)
              EE(4)=CONSTANT(4,IBLINE)
         ENDIF
         WRITE ( JBUF, '("REWT ",a8,"-",a8,4f10.2)' ) JBASL, EE
         IF ( INDEX ( JBUF, '*' ) .NE. 0 ) THEN
              WRITE ( 6, FMT='("IBL=",i4,"card: ",a)' ) IBLINE, JBUF
              WRITE ( 6, * ) ' EE=',EE
              CALL FERR ( INT2(444), "put_weights: writing REWT card", &
     &             INT2(0), INT2(0) )
        END IF
        call putcard( idb, 'REWT', INT2(4), jbuf, ierr )
!       if(ierr.ne.0) pause 'error writing NAMFIL'
        if(ierr.ne.0) call ferr( INT2(125), 'writing NAMFIL in put_weight', &
     &     INT2(0), INT2(0) )
        call getcard( idb, 'REWT', INT2(0), jbuf, ierr )
      enddo
!
      if(ierr.ne.1) then
!       pause 'error reading NAMFIL in put_weight'
        call ferr( INT2(126), 'error reading NAMFIL in put_weight', INT2(0), &
     &       INT2(0) )
      endif
!
      return
      end
