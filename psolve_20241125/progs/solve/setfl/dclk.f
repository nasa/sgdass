      SUBROUTINE DCLK ( ISITE, IPAGEC, LISTLEN )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DCLK PROGRAM SPECIFICATION
!
! 1.1 Delete a clock breaks epoch in interactive mode
!
! 1.2 REFERENCES:
!
! 2.  DCLK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISITE,ipagec,listlen
!
! ISITE - Site number of station being processed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: stflg
!       CALLED SUBROUTINES: posn
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   KBR, ICHR, K1, J1, J2, J3, ISTART, ICOU_BR, IDE
      CHARACTER*2 ICH
      EQUIVALENCE ( ICH, ICHR )
      LOGICAL*2   KBIT
      INTEGER*2   INT2_ARG
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jwr  851112  Structures inserted and mask logic removed
!   mwh  910404  Fixed bug (closing gap in ICLSTA)
!   pet  970717  Virtually rewrote routine
!   pet  970821  Corrected bug for the case of batch mode clocks
!   pet  20-JAN-2000  Changed the logic: the previous version removed clock
!                     breakes in non "batch clock mode" incorrectly
!
! 5.  DCLK PROGRAM STRUCTURE
!
!
! --- Check whether this is the last clock epoch - don't delete it
!
      IF ( NUMCLK(ISITE) .LE. 1 ) RETURN
!
! --- Check the presence of clock breaks -- DCLK has a sence only if
! --- there are real breaks (CLK_BRK_STAT is updated each time when stflg is
! --- called)
!
      IF ( BMODE_CL  .AND.  .NOT. CLK_BRK_STAT ) RETURN
!
! --- Putting short message with instructions to user what's to do. Is is not
! --- amiss since even users who spent substantial part of their life with
! --- SOLVE used to forget it...
!
      CALL SETCR_MN      ( 1, 2 )
      CALL ADDSTR_F      ( "                                                " )
      CALL SETCR_MN      ( 1, 2 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F      ( "Delete clocks" )
      CALL REVERSE_OFF_MN()
      CALL SETCR_MN      ( 1, 4 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F      ( "To delete clock break put cursor at the line "// &
     &                     "with break epoch and hit <blank>" )
      CALL REVERSE_OFF_MN()
!
! --- Positioning cursor and reading the input
!
      CALL POSN ( 6, ICLSTR(ISITE), NUMCLK(ISITE), KBR, ICHR )
!
! --- Correction an error of pos. Now KBR will point at the clock break which
! --- we are goiung to delete
!
      IF ( BMODE_CL ) THEN
           KBR = KBR + (IPAGEC-1)*LISTLEN
           IF ( KBR .GT. 0     ) KBR = KBR - 1
         ELSE
           KBR = KBR - ICLSTR(ISITE)
           IF ( KBR .EQ. 1 ) RETURN
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         call end_mn                                                    ! %%%%%
!         CALL SYSTEM ( 'reset' )  ! Elimination of the influence of     ! %%%%%
!         CALL PRCH ( CHAR(27)//'g'//CHAR(27)//'E'//CHAR(27)//'&k1L'//   ! %%%%%
!     #               CHAR(27)//'&s1A' )                                 ! %%%%%
!          TYPE *,' ich(1:1) >>',ich(1:1),'<<  kbr = ',kbr               ! %%%%%
!          call pause ( 'dlck 1' )                                       ! %%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- We will not do anything if not-blank key was hit or we positioned out of
! --- clock breaks range...
!
      IF ( ICH(1:1) .EQ. ' '  .AND.  KBR .GT. 0 ) THEN
!
! -------- What we going to do is to scan the array LCLK. We will dumbly count
! -------- the clock epock with discontinuity (leared 13-th bit), but not
! -------- taking into account the first epoch (which is always had this bit
! -------- cleared). The aim is clear, let's work, comrades!
!
           ICOU_BR = 0
           IF ( BMODE_CL ) THEN
                ISTART = 2
              ELSE
                ISTART = 1
           END IF
           DO 410 J1=ISTART,NUMCLK(ISITE)
!
! ----------- Some socery with ICLSTR. I heared a tale that sometimes this
! ----------- array had non-zero elements...
!
              K1 = J1 + ICLSTR(ISITE)
!
              IF ( KBIT(ICLSTA(1,K1), ISITE) ) THEN
                 IF ( .NOT. KBIT( LCLK(K1), INT2(13)) ) THEN
!
! ----------------- Oh! This clock epoch has a discontinuity. We are always
! ----------------- waited for something like that.
!
                    ICOU_BR = ICOU_BR + 1 ! Increment break counters
                    IF ( ICOU_BR   .EQ. KBR ) THEN
!
! -------------------- Urah! we reached at last the clock epoch with the break
! -------------------- which we are going to delete
!
                       IF ( BMODE_CL ) THEN
!
! ------------------------- By the way: in batch mode we should delete two
! ------------------------- epochs: one with real clock break and one in one
! ------------------------- "centimunite" before it.
!
                            IDE = 2
                          ELSE
                            IDE = 1
                       END IF
!
                       IF ( J1 .LT. NUMCLK(ISITE) ) THEN
!
! ----------------------- If it is not such a rare events that hatred clock
! ----------------------- break is just the last one, we squeeze down arrays
! ----------------------- associated with clock break epochs and new elements
! ----------------------- will swallow hatred clock break
!
                          DO 420 J2=J1,NUMCLK(ISITE)-IDE
                             FJDCL(J2+ICLSTR(ISITE))=FJDCL(J2+IDE+ICLSTR(ISITE))
                             LCLK (J2+ICLSTR(ISITE))=LCLK(J2+IDE+ICLSTR(ISITE))
                             ICLSTA(1,J2+ICLSTR(ISITE)) = &
     &                                          ICLSTA(1,J2+IDE+ICLSTR(ISITE))
                             ICLSTA(2,J2+ICLSTR(ISITE)) = &
     &                                          ICLSTA(2,J2+IDE+ICLSTR(ISITE))
 420                      CONTINUE
                       END IF
!
! -------------------- To be far from a sin we zero the last elements of the
! -------------------- arrays associated with clock epochs, not to leave the
! -------------------- traces of existence of the previous clock break
!
                       IF ( BMODE_CL ) THEN
                            FJDCL(NUMCLK(ISITE))     = 0.D0
                            LCLK(NUMCLK(ISITE))      = 0
                            ICLSTA(1,NUMCLK(ISITE))  = 0
                            ICLSTA(2,NUMCLK(ISITE))  = 0
!
! ------------------------- Don't forget about one centiminute before!
!
                            FJDCL(NUMCLK(ISITE)-1)     = 0.D0
                            LCLK(NUMCLK(ISITE)-1)      = 0
                            ICLSTA(1,NUMCLK(ISITE)-1)  = 0
                            ICLSTA(2,NUMCLK(ISITE)-1)  = 0
                          ELSE
                            FJDCL(ICLSTR(ISITE)+NUMCLK(ISITE))     = 0.D0
                            LCLK(ICLSTR(ISITE)+NUMCLK(ISITE))      = 0
                            ICLSTA(1,ICLSTR(ISITE)+NUMCLK(ISITE))  = 0
                            ICLSTA(2,ICLSTR(ISITE)+NUMCLK(ISITE))  = 0
                       END IF
!
! -------------------- The last thing what remained to be done is reducing the
! -------------------- number of clock epochs for this site. We do it with
! -------------------- pleasure...
!
                       NUMCLK(ISITE) = NUMCLK(ISITE) - IDE
                       IF ( BMODE_CL ) THEN
!
! ------------------------- Ah! For batch-clock we should adjust number of
! ------------------------- clocks for all stations
!
                            DO 430 J3=1,NUMSTA
                               IF ( NUMCLK(J3) .NE. 0 ) NUMCLK(J3)=NUMCLK(ISITE)
 430                        CONTINUE
                       END IF
!
! -------------------- ... and jump to the end.
!
                       GOTO 810
                    END IF
                 END IF
              END IF
 410       CONTINUE
 810       CONTINUE
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           type *,' numclk(isite) =',numclk(isite)                ! %%%%
!           do 510 j1=1,numclk(isite)                              ! %%%%
!              k1 = j1 + iclstr(j1)                                ! %%%%
!              type 129, j1,fjdcl(j1), kbit(iclsta(1,k1),1),       ! %%%%
!     #                              kbit(lclk(k1),13)             ! %%%%
! 129          FORMAT ( 1x,' i =',i3,' fjdcl=',1pg22.15,' kb =',I7, ! %%%
!     #                    ' kbl_13 = ',I7 )                       ! %%%%
! 510       continue                                               ! %%%%
!           call pause ( 'dlck 3' )                                ! %%%%
!         call start_mn                                            ! %%%%
!! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      END IF
!
! --- Now we should delete from the screen instructions how to use dclk.
! --- This job has been done, the time to sing another song came...
!
      CALL SETCR_MN      ( 1, 2 )
      CALL ADDSTR_F      ( "             " )
      CALL SETCR_MN      ( 0, 4 )
      CALL ADDSTR_F      ( "                                              "// &
     &                     "                                " )
      RETURN
!
! ---- Good Bye! This part of code I left as an monument to the old dclk.
! ---- Could anybody tell, how could has it worked???
!
!
!      DO WHILE (IQUIT .AND. NUMCLK(ISITE).GT.1)
!       DO BEGIN looking
!
!         Determine the position of the epoch to be deleted
!         Make certain the user hasn't requested an immediate out ("R")
!         and that the requested epoch is legal.
!
!
!           THEN BEGIN deleting an epoch
!             Delete the line the epoch is on,i.e., move up the screen
!             one line.
!
!             K is the location in the LCLK array of the clock epoch
!             to be deleted.
!             Move down the epochs and the flags for the polys succeeding
!
!
!
!              DO J=K,NUMCLK(NUMSTA)+ICLSTR(NUMSTA)-1
!
!               BEGIN moving them up
!
!                  FJDCL(J) = FJDCL(J + 1)
!                  LCLK(J)  = LCLK(J + 1)
!                 do i=1,2
!                   iclsta(i,j) = iclsta(i,j + 1)
!                 enddo
!                 ENDF moving them up
!              END DO
!
!             Decrement the start counters for the succeeding stations,
!             if this is not the last station.
!
!              IF (ISITE.LT.NUMSTA)  THEN
!               THEN BEGIN decrement
!                  KSTA = ISITE + 1
!                  DO JSTA = KSTA,NUMSTA
!                    ICLSTR(JSTA) = ICLSTR(JSTA) - 1
!                    IF ( ICLSTR(JSTA) .GT. 0 ) ICLSTR(JSTA) = ICLSTR(JSTA) - 1
!                  END DO
!C                 ENDT decrement
!C
!             Decrement the clock total counter for this station
!
!              END IF
!              NUMCLK(ISITE) = NUMCLK(ISITE) - 1
!             ENDT deleting an epoch
!          ELSE
!            IQUIT = .FALSE.
!          END IF
!         ENDW looking
!      END DO
!
!      CALL SETCR_MN ( INT4(50), INT4(2) )
!      CALL ADDSTR_F ( "                    " )
!      RETURN
      END  !#!  DCLK  #!#
