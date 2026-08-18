      SUBROUTINE DIAGHELP(ibuf,ilen,iconvert,de_found,dr_found, &
     &       iafter_de,irec,max_len2,irtype,kerr)
!
!     DIAGHELP
!
      implicit none
! 1.  DIAGHELP PROGRAM SPECIFICATION
!
! 1.1.   DIAGHELP identifies data base data which needs to be converted
!        from a non-UNIX data format to a UNIX data format, or vice versa,
!        and does it.
!
! 1.2.   RESTRICTIONS - results not guaranteed for non-835 UNIX machines
!
! 1.3.   REFERENCES - none
!
! 2.  DIAGHELP INTERFACE
!
! 2.1.   CALLING SEQUENCE: CALL DIAGHELP(ibuf,ilen,iconvert,
!                             de_found,dr_found,iafter_de,irec,max_len2,kerr)
!
!     INPUT VARIABLES:
!
      integer*2 ibuf(*),ilen,iconvert,iafter_de,max_len2,irtype
      integer*4 irec
      logical*2 de_found,dr_found
!
!     IBUF - data base record to convert
!     ILEN - no of words of data in buffer
!     ICONVERT - whether converting from non-UNIX to UNIX (835) format or
!                vice versa
!     DE_FOUND, DR_FOUND, IAFTER_DE - variables for finding the data base
!         records whose data needs conversion
!     IREC - current record number
!     MAX_LEN2 -  buffer size (number of 16-bit words)
!
!     OUTPUT VARIABLES:
!
      integer*2 kerr
!
!     KERR - error return (0 = okay, -1 = data to be converted had
!                          wrong number of words of data,
!                          -2 = conversion error,
!                          -3 = bad iconvert value)
!
!     irtype - identifies the record as data type 1-5 or other (=0)
!
! 2.2.   COMMON BLOCKS USED: none
!
! 2.3.   DATA BASE ACCESSES: none
!
! 2.4.   EXTERNAL INPUT/OUTPUT
!
!     INPUT VARIABLES: none
!
!     OUTPUT VARIABLES:
!
!
! 2.5.   SUBROUTINE INTERFACE:
!
!     CALLING SUBROUTINES:
!
!     CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 IERR
      CHARACTER*2 REC_ID
      INTEGER*2 HST_TRACKER
      INTEGER*4 LAST_HIST
      LOGICAL*2 HST_LEFT
      SAVE HST_TRACKER,HST_LEFT,LAST_HIST
      DATA HST_TRACKER /0/, LAST_HIST /0/
      DATA HST_LEFT /.TRUE./
!
! 4.  CONSTANTS USED: none
!
! 5.  INITIALIZED VARIABLES
!
!
! 6.  PROGRAMMER: K. Baver 11/8/88
!     :93.06.23:jwr: Typing and dimensioning change to convert (1)'s
!                    in (*) so -C option of compile will work.
!
!     PROGRAM STRUCTURE
!
!      Identify data which takes a different format on UNIX and non-UNIX
!        machines.  Convert to the format for the machine for which the
!        data base is destined.
!
       write (rec_id,"(A2)") ibuf(1)
       IF (HST_LEFT) THEN
         IRTYPE = -1
       ELSE
         IRTYPE = 0
       END IF
!
!      do a loose check for history record
!
       IF (HST_LEFT) THEN
         IF (IREC .GE. 8) THEN
           IF (HST_TRACKER .EQ. 2) THEN
             HST_TRACKER = 0
             IRTYPE = 6
             LAST_HIST = IREC
           ELSE IF (REC_ID .EQ. 'HS') THEN
             HST_TRACKER = HST_TRACKER + 1
           END IF
           IF (LAST_HIST .NE. 0) THEN
             IF (IREC .GT. LAST_HIST .AND. REC_ID.NE.'HS') THEN
               HST_LEFT = .FALSE.
               IRTYPE = 0
             END IF
           END IF
         END IF
       END IF
!
!
       if(rec_id.eq.'DR'.and.ilen.eq.12) then
         dr_found = .true.
       else if(rec_id.eq.'DE'.and.dr_found.and.ilen.eq.12.and. &
     &          ((iafter_de.eq.5.and.de_found).or..not.de_found)) then
         de_found = .true.
         iafter_de = 0
       else if(rec_id.eq.'ZZ'.and.dr_found.and. &
     &          iafter_de.eq.5.and.de_found) then
         dr_found = .false.
         de_found = .false.
         iafter_de = 0
       else if(de_found) then
         iafter_de = iafter_de+1
!
!        Certain types of data are represented differently on UNIX and
!         non-UNIX machines.  Convert these types to the appropriate
!         representation for the machine for which the data base is headed.
!
         if(iafter_de.eq.1) then
!
!          Data type which is real*6 on a non-UNIX machine.  There is no
!            real*6 data on a UNIX machine, so if this data base is being
!            transferred to the UNIX, convert the data to real*8.
!            Otherwise, the data base is headed for a non-UNIX machine,
!            and the data is real*8 and must be converted back to real*6.
!
           if (iconvert .eq. 1) then
!            headed for UNIX -- convert from REAL*6 to REAL*8
             irtype = 1
             if(mod(ilen,int2(3)).ne.0) then
               write(6,"('DIAGHELP: wrong number of words for ' &
     &                  'non-UNIX REAL*6 record, on record=',i11)")irec
               kerr = -1
               return
             endif
             call r6recex(ibuf,ilen,max_len2,irec,ierr)
             if(ierr.lt.0) then
               write(6,"('DIAGHELP: Error ',i5, &
     &                  ' encountered in REAL*6 -> ' &
     &                  'REAL*8 transformation on record ',i11)") &
     &                  ierr,irec
               kerr = -2
               return
             endif
           else if (iconvert .eq. -1) then
!            headed for non-UNIX -- convert from REAL*8 to REAL*6
             irtype = 1
             if(mod(ilen,int2(4)).ne.0) then
               write(6,"('DIAGHELP: wrong number of words for ' &
     &                  'UNIX REAL*8 record, on record=',i11)")irec
               kerr = -1
               return
             endif
             call r8reccon(ibuf,ilen,ierr)
             if(ierr.lt.0) then
               write(6,"('DIAGHELP: Error encountered in REAL*8 -> ' &
     &                  'REAL*6 transformation on record ',i11)")irec
               kerr = -2
               return
             endif
           else
!            bad iconvert value - recode the calling sub
             write(6,"('DIAGHELP: bad iconvert value: ',i5)") &
     &         iconvert
             kerr = -3
             return
           endif
         else if(iafter_de.eq.4) then
!
!          Data which is real*8 on a non-UNIX machine. The UNIX and
!            non-UNIX representations of real*8 data differ, so convert
!            to the appropriate representation for the destination machine.
!
           if (iconvert .eq. 1) then
!            Convert from non-UNIX to UNIX real*8 representation
             irtype = 4
             if(mod(ilen,int2(4)).ne.0) then
               write(6,"('DIAGHELP: wrong number of words for ' &
     &                  'non-UNIX REAL*8 record, on record=',i11)")irec
               kerr = -1
               return
             endif
             call r8n2u(ibuf,ilen,irec,ierr)
             if(ierr.lt.0) then
               write(6,"('DIAGHELP: Error ',i5, &
     &                  ' encountered in non-UNIX REAL*8 -> ',/, &
     &                  'UNIX REAL*8 transformation on record ',i11)") &
     &                  ierr,irec
               kerr = -2
               return
             endif
           else if (iconvert.eq.-1) then
!            convert from UNIX REAL*8 to non-UNIX REAL*8
             irtype = 4
             if(mod(ilen,int2(4)).ne.0) then
               write(6,"('DIAGHELP: wrong number of words for ' &
     &                  'UNIX REAL*8 record, on record=',i11)")irec
               kerr = -1
               return
             endif
             call r8u2n(ibuf,ilen,ierr)
             if(ierr.lt.0) then
               write(6,"('DIAGHELP: Error encountered in ', &
     &                  'UNIX REAL*8 -> non-UNIX REAL*8 ' &
     &                  'transformation ',/,'on record ',i11)")irec
               kerr = -2
               return
             endif
           else
!            bad iconvert value - recode the calling sub
             write(6,"('DIAGHELP: bad iconvert value: ',i5)") &
     &         iconvert
             kerr = -3
             return
           endif !end of real*8 case (where headed bad iconvert value)
         else if (iafter_de .eq. 2) then
           irtype = 2
         else if (iafter_de .eq. 3) then
           irtype = 3
         else if (iafter_de .eq. 5) then
           irtype = 5
         end if !real*6 data vs real*8 data vs other data cases
! (end of real*8 case handling)
       endif !found one of the 5 data types
      kerr = 0
      RETURN
      END
