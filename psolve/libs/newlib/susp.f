      Subroutine Susp(irsl,imult)
      Implicit none
      INCLUDE 'fclib.i'
      Integer*2 irsl, imult
      Integer*4 ierr, tsleep
!
      IF(IRSL.LT.2.OR.IRSL.GT.4) THEN
        WRITE(7,*) 'SUSP: Unsupported arg',IRSL
      ENDIF
!
      TSLEEP=IMULT
      IF(IRSL.GT.2) TSLEEP=TSLEEP*60
      IF(IRSL.GT.3) TSLEEP=TSLEEP*60
      ierr = fc_sleep(tsleep)
!
      return
      end
