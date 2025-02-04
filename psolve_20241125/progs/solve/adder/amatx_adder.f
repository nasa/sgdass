      SUBROUTINE AMATX_ADDER (A1,A2,B1,B2, IXREF, NPARM,ADORSB, &
     &  remnum,remparm)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      REAL*8    A1(*),A2(*),B1(*),B2(*)
      INTEGER*4 IXREF(*),NPARM, REMNUM, REMPARM(*)
      logical*2 kpos
      CHARACTER*(*) ADORSB
!
      INTEGER*8 INDX8, IJX, OLDIND, NEWIND
      INTEGER*4 I, J, IX, K
!
      remnum=0
      IF ( ADORSB .EQ. 'ADD' ) THEN
           DO I = 1, NPARM
              IF ( IXREF(I) .NE. 0) THEN
                   IX = IXREF(I)
                   B1(IX) = B1(IX) + B2(I)
                   DO J = 1, I
                      IF ( IXREF(J) .NE. 0 ) THEN
                           IJX = INDX8(IX,IXREF(J))
                           A1(IJX) = A1(IJX) + A2(INDX8(I,J))
                      END IF
                   END DO
              END IF
           END DO
      ELSE
        DO I = 1, NPARM
           IF ( IXREF(I) .NE. 0) THEN
                KPOS = .FALSE.
                IX = IXREF(I)
                B1(IX) = B1(IX) - B2(I)
                DO J = 1, I
                   IF ( IXREF(J) .NE. 0 ) THEN
                        IJX = INDX8 ( IX, IXREF(J) )
                        A1(IJX) = A1(IJX) - A2 ( INDX8(I,J) )
                        IF (A1(IJX) .NE. 0.D0 ) KPOS = .true.
                   END IF
               END DO
               IF ( .NOT. KPOS ) THEN
                    REMNUM=REMNUM+1
                    REMPARM(REMNUM) = IXREF(I)
               ENDIF
          END IF
        END DO
!
! If any parameters have disappeared through SUBTRACTion,
!   squeeze them out of the normal matrix
!
        do i=remnum,1,-1
           nparm=nparm-1
           do j=remparm(i),nparm
              do k=1,remparm(i)-1
                 newind = indx8(j,k)
                 oldind = indx8( j+1, k)
                 a1(newind) = a1(oldind)
              enddo
              do k=remparm(i),j
                 newind = indx8(j,k)
                 oldind = indx8( j+1, k+1 )
                 a1(newind) = a1(oldind)
              enddo
              b1(j) = b1(j+1)
           enddo
        enddo
      ENDIF
!
      RETURN
      END
