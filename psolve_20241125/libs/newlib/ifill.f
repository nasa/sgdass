        SUBROUTINE IFILL(IOUT,IC,NC,JC)
        IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
        INTEGER*2 IOUT(*),IC,NC,JC
!
! IFILL: fill array IOUT with from character IC through IC+NC-1 inclusive
!        with the character in th lower byte of JC
!
! Input:
!       IOUT: hollerith array to be filled
!       IC:   first character in IOUT to fill
!       NC:   number of characters in IOUT to fill
!       JC:   lower byte contains fill character
!
! Output:
!        IOUT: characters IC...IC+NC-1 filled with lower byte of JC
!              NC .eq. 0 then no-op
!
! Warning:
!         Negative and zero values of IC are not support
!         NCHAR must be non-negative
!         IC+NCHAR.LE.32767
!
      INTEGER*2 I,IEND
!
      IF(IC.LE.0.OR.NC.LT.0) THEN
        WRITE(7,*) 'IFILL: Illegal arguments',IC,NC
        STOP
      ENDIF
!
      IF(NC.EQ.0) RETURN
!
      IEND=32767-NC+1
!
      IF(IC.GT.IEND) THEN
        WRITE(7,*) 'IFILL: Illegal combination',IC,NC
        STOP
      ENDIF
!
      DO I=0,NC-1
        CALL PCHAR( IOUT, INT2(IC+I), JC )
      ENDDO
!
      RETURN
      END
