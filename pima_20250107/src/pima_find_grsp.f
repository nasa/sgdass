      SUBROUTINE PIMA_FIND_GRSP ( PIM, FRQ_MULT, GRSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FIND_GRSP 
! *                                                                      *
! * ### 27-AUG-2006  PIMA_FIND_GRSP  v1.0 (c) L. Petrov  27-AUG-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE        ) :: PIM
      REAL*8     FRQ_MULT, GRSP
      INTEGER*4  IUER
      LOGICAL*4  FL_MULT
      INTEGER*8  I8_FRQ(PIM__MFRQ), J1, J2, J3, J4, IFRQ
!
      IFRQ = 0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         I8_FRQ(IFRQ) = IDNINT ( (PIM%FRQ(J1)%FREQ - PIM%FRQ(PIM%CONF%BEG_FRQ)%FREQ)/ &
     &                            FRQ_MULT )
 410  CONTINUE 
      IF ( IFRQ .EQ. 0 ) THEN
           CALL ERR_LOG ( 5581, IUER, 'PIMA_FIND_GRSP', 'Cannot find group'// &
     &         'delay spacing, since no frequency was found' )
           RETURN 
         ELSE IF ( IFRQ == 1 ) THEN
           CALL ERR_LOG ( 5582, IUER, 'PIMA_FIND_GRSP', 'Cannot find group'// &
     &         'delay spacing, since only one frequency was found' )
           RETURN 
         ELSE IF ( IFRQ == 2 ) THEN
            GRSP = (I8_FRQ(IFRQ) - I8_FRQ(IFRQ))*FRQ_MULT
         ELSE IF ( IFRQ .GE. 3 ) THEN
            GRSP = 0.0D0
!
! --------- Search for the maximum integer which will be a devider for
! --------- all frequencies with respect to the reference frequency
!
            DO 420 J2=1,I8_FRQ(IFRQ)
               FL_MULT = .TRUE.
               DO 430 J3=2,IFRQ
                  IF ( MOD(I8_FRQ(J3),J2) .NE. 0 ) FL_MULT = .FALSE.
 430           CONTINUE 
               IF ( FL_MULT ) GRSP = 1.0D0/(J2*FRQ_MULT)
 420        CONTINUE 
            IF ( GRSP == 0.0D0 ) THEN
                 CALL ERR_LOG ( 5583, IUER, 'PIMA_FIND_GRSP', 'Cannot '// &
     &               'find group delay spacing. They are not commensurate' )
                 RETURN 
            END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FIND_GRSP  !#!#
