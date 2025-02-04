      FUNCTION   LIB$CALLG ( ARG_LIST, FUNC )
! ************************************************************************
! *                                                                      *
! *   Routine  LIB$CALLG  calls external procedure FUNC with the         *
! *   argument list supplied.                                            *
! *                                                                      *
! * ______________________ Input parameters: ___________________________ *
! *                                                                      *
! *  ARG_LIST ( INTEGER*4 ) -- Argument list to be passed to             *
! *                            user-procedure. The argument-list         *
! *                            argument is the address of an array of    *
! *                            longwords that is the argument list. The  *
! *                            first longword contains the count of the  *
! *                            remaining longwords, to a maximum of 64.  *
! *      FUNC ( INTEGER*4 ) -- Address of the external user procedure.   *
! *                                                                      *
! * ______________________ Ouput parameters: ___________________________ *
! *                                                                      *
! * LIB$CALLG ( INTEGER*4 ) -- Value returned by the procedure FUNC.     *
! *                                                                      *
! *  ### 04-AUG-2002   LIB$CALLG   v1.1 (c)  L. Petrov  16-APR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      ADDRESS__TYPE :: ARG_LIST(0:255)
      ADDRESS__TYPE :: LIB$CALLG
      ADDRESS__TYPE, EXTERNAL  :: FUNC
!
      IF ( ARG_LIST(0) .EQ. 0 ) THEN
           LIB$CALLG = FUNC ( )
         ELSE IF ( ARG_LIST(0) .EQ. 1 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)) )
         ELSE IF ( ARG_LIST(0) .EQ. 2 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)) )
         ELSE IF ( ARG_LIST(0) .EQ. 3 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)) )
         ELSE IF ( ARG_LIST(0) .EQ. 4 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)), %VAL(ARG_LIST(4))  )
         ELSE IF ( ARG_LIST(0) .EQ. 5 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)), %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)) )
         ELSE IF ( ARG_LIST(0) .EQ. 6 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)), %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)), %VAL(ARG_LIST(6))  )
         ELSE IF ( ARG_LIST(0) .EQ. 7 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)), %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)), %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)) )
         ELSE IF ( ARG_LIST(0) .EQ. 8 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)), %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)), %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)), %VAL(ARG_LIST(8))  )
         ELSE IF ( ARG_LIST(0) .EQ. 9 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)), %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)), %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)), %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)) )
         ELSE IF ( ARG_LIST(0) .EQ. 10 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)), %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)), %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)), %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)), %VAL(ARG_LIST(10)) )
         ELSE IF ( ARG_LIST(0) .EQ. 11 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)), %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)), %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)), %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)), %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)), %VAL(ARG_LIST(10)), &
     &                        %VAL(ARG_LIST(11)) )
         ELSE IF ( ARG_LIST(0) .EQ. 12 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)),  %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)),  %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)),  %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)),  %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)),  %VAL(ARG_LIST(10)), &
     &                        %VAL(ARG_LIST(11)), %VAL(ARG_LIST(12)) )
         ELSE IF ( ARG_LIST(0) .EQ. 13 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)),  %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)),  %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)),  %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)),  %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)),  %VAL(ARG_LIST(10)), &
     &                        %VAL(ARG_LIST(11)), %VAL(ARG_LIST(12)), &
     &                        %VAL(ARG_LIST(13))                     )
         ELSE IF ( ARG_LIST(0) .EQ. 14 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)),  %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)),  %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)),  %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)),  %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)),  %VAL(ARG_LIST(10)), &
     &                        %VAL(ARG_LIST(11)), %VAL(ARG_LIST(12)), &
     &                        %VAL(ARG_LIST(13)), %VAL(ARG_LIST(14)) )
         ELSE IF ( ARG_LIST(0) .EQ. 15 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)),  %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)),  %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)),  %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)),  %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)),  %VAL(ARG_LIST(10)), &
     &                        %VAL(ARG_LIST(11)), %VAL(ARG_LIST(12)), &
     &                        %VAL(ARG_LIST(13)), %VAL(ARG_LIST(14)), &
     &                        %VAL(ARG_LIST(15))                     )
         ELSE IF ( ARG_LIST(0) .EQ. 16 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)),  %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)),  %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)),  %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)),  %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)),  %VAL(ARG_LIST(10)), &
     &                        %VAL(ARG_LIST(11)), %VAL(ARG_LIST(12)), &
     &                        %VAL(ARG_LIST(13)), %VAL(ARG_LIST(14)), &
     &                        %VAL(ARG_LIST(15)), %VAL(ARG_LIST(16))  )
         ELSE IF ( ARG_LIST(0) .GE. 16 ) THEN
           LIB$CALLG = FUNC ( %VAL(ARG_LIST(1)),  %VAL(ARG_LIST(2)), &
     &                        %VAL(ARG_LIST(3)),  %VAL(ARG_LIST(4)), &
     &                        %VAL(ARG_LIST(5)),  %VAL(ARG_LIST(6)), &
     &                        %VAL(ARG_LIST(7)),  %VAL(ARG_LIST(8)), &
     &                        %VAL(ARG_LIST(9)),  %VAL(ARG_LIST(10)), &
     &                        %VAL(ARG_LIST(11)), %VAL(ARG_LIST(12)), &
     &                        %VAL(ARG_LIST(13)), %VAL(ARG_LIST(14)), &
     &                        %VAL(ARG_LIST(15)), %VAL(ARG_LIST(16)), &
     &                        %VAL(ARG_LIST(17)), %VAL(ARG_LIST(18)), &
     &                        %VAL(ARG_LIST(19)), %VAL(ARG_LIST(20)), &
     &                        %VAL(ARG_LIST(21)), %VAL(ARG_LIST(22)), &
     &                        %VAL(ARG_LIST(23)), %VAL(ARG_LIST(24)), &
     &                        %VAL(ARG_LIST(25)), %VAL(ARG_LIST(26)), &
     &                        %VAL(ARG_LIST(27)), %VAL(ARG_LIST(28)), &
     &                        %VAL(ARG_LIST(29)), %VAL(ARG_LIST(30)), &
     &                        %VAL(ARG_LIST(31)), %VAL(ARG_LIST(32)), &
     &                        %VAL(ARG_LIST(33)), %VAL(ARG_LIST(34)), &
     &                        %VAL(ARG_LIST(35)), %VAL(ARG_LIST(36)), &
     &                        %VAL(ARG_LIST(37)), %VAL(ARG_LIST(38)), &
     &                        %VAL(ARG_LIST(39)), %VAL(ARG_LIST(40)), &
     &                        %VAL(ARG_LIST(41)), %VAL(ARG_LIST(42)), &
     &                        %VAL(ARG_LIST(43)), %VAL(ARG_LIST(44)), &
     &                        %VAL(ARG_LIST(45)), %VAL(ARG_LIST(46)), &
     &                        %VAL(ARG_LIST(47)), %VAL(ARG_LIST(48)), &
     &                        %VAL(ARG_LIST(49)), %VAL(ARG_LIST(50)), &
     &                        %VAL(ARG_LIST(51)), %VAL(ARG_LIST(52)), &
     &                        %VAL(ARG_LIST(53)), %VAL(ARG_LIST(54)), &
     &                        %VAL(ARG_LIST(55)), %VAL(ARG_LIST(56)), &
     &                        %VAL(ARG_LIST(57)), %VAL(ARG_LIST(58)), &
     &                        %VAL(ARG_LIST(59)), %VAL(ARG_LIST(60)), &
     &                        %VAL(ARG_LIST(61)), %VAL(ARG_LIST(62)), &
     &                        %VAL(ARG_LIST(63)), %VAL(ARG_LIST(64))  )
      END IF
      RETURN
      END  !#!  LIB$CALLG  #!#
