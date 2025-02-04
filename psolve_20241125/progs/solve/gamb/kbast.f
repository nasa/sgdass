        FUNCTION KBAST ( OBS, NB )
! ************************************************************************
! *                                                                      *
! *     Auxilary function  KBAST  returns the 7-character string of kind *
! *     iii/jjj, where iii -- is the number of the 1-st station of the   *
! *     basline with number NB, and jjj -- is the number of the 2-nd     *
! *     station of this basline. If the number of the baseline is not    *
! *     set up correctly then  KBAST  returns the value "Error! ". It is *
! *     assumed that the number of the basline NB has been calculated by *
! *     NSTBA.                                                           *
! *                                                                      *
! *       It is assumed that the list of stations has been created and   *
! *     kept in data structure obs.                                      *
! *                                                                      *
! * ________________________ Intput parameters: ________________________ *
! *                                                                      *
! *      OBS ( RECORD    ) -- Data structure which contains              *
! *                           band-independent informatiuon: time of     *
! *                           observaion, baseline, lists of objects,    *
! *                           status flags etc.                          *
! *       NB ( INTEGER*4 ) -- Baseline code.                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  <KBAST> ( INTEGER*4 ) -- Test string of baseline content.           *
! *                                                                      *
! *  ###  28-DEC-94       KBAST     V2.1  (c) L. Petrov  02-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INCLUDE    'solve.i'
        INCLUDE    'gamb.i'
        INTEGER*4  NB, NST1, NST2, IST1, IST2, IL, ILEN, IFIND_PL
        CHARACTER  KBAST*7
        TYPE ( OBS__STRU ) ::  OBS
!C
        IF ( OBS%FIRST_FIELD .NE. OBS__IDE ) THEN
             KBAST = 'Not OBS'
             RETURN
        END IF
!
        CALL NBAST ( NB, NST1, NST2 )
        IF ( NST1.GT.900 .OR. NST1.LT.1 .OR. NST2.GT.900 .OR. &
     &       NST2.LT.1 ) THEN
           KBAST = 'Error! '
         ELSE
           CALL CLRCH ( KBAST )
!
           IST1 = IFIND_PL ( OBS%L_STA, OBS%LIS_STA, NST1 )
           IF ( IST1 .GT. 0 ) THEN
                CALL INCH ( IST1, KBAST )
             ELSE
                KBAST(1:3) = '???'
           END IF
           IL= ILEN(KBAST)
!
           KBAST(IL+1:)='/'
!
           IST2 = IFIND_PL ( OBS%L_STA, OBS%LIS_STA, NST2 )
           IF ( IST1 .GT. 0 ) THEN
                CALL INCH ( IST2, KBAST(IL+2:) )
             ELSE
                KBAST(IL+2:IL+4) = '???'
           END IF
           CALL CHASHL ( KBAST(IL+2:) )
        END IF
!
        RETURN
        END  !#!  KBAST  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION CBAST ( OBS, NB )
! ************************************************************************
! *                                                                      *
! *       Auxilary function  CBAST  returns the 17-character string of   *
! *     kind aaaaaaaa/bbbbbbbb, where aaaaaaaa -- is 8-cahracter name of *
! *     the 1-st station of the basline with number NB, and bbbbbbbb --  *
! *     is the number of the 2-st station of this basline. If the number *
! *     of the baseline is not set up correctly then  CBAST  returns the *
! *     value "NB is Erroneous! ".                                       *
! *                                                                      *
! *       It is assumed that the number of the basline NB has been       *
! *     calculated by NSTBA.                                             *
! *                                                                      *
! *       It is assumed that the list of stations has been created and   *
! *     kept in data structure OBS.                                      *
! *                                                                      *
! * ________________________ Intput parameters: ________________________ *
! *                                                                      *
! *      OBS ( RECORD    ) -- Data structure which contains              *
! *                           band-independent informatiuon: time of     *
! *                           observaion, baseline, lists of objects,    *
! *                           status flags etc.                          *
! *       NB ( INTEGER*4 ) -- Baseline code.                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  <CBAST> ( CHARACTER ) -- Test string of baseline content.           *
! *                                                                      *
! *                                                                      *
! *  ###  28-DEC-94       CBAST     V2.2  (c) L. Petrov  02-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INCLUDE    'solve.i'
        INCLUDE    'gamb.i'
        INTEGER*4  NB, NST1, NST2, IST1, IST2, IFIND_PL
        CHARACTER  CBAST*17
        TYPE ( OBS__STRU ) ::  OBS
!C
        IF ( OBS%FIRST_FIELD .NE. OBS__IDE ) THEN
             CBAST = 'CBAST: wrong OBS!'
             RETURN
        END IF
        CALL NBAST ( NB, NST1, NST2 )
        IF ( NST1.GT.900 .OR. NST1.LT.1 .OR. NST2.GT.900 .OR. &
     &       NST2.LT.1 ) THEN
           CBAST = 'NB is Erroneous! '
         ELSE
           CALL CLRCH ( CBAST )
           IST1 = IFIND_PL ( OBS%L_STA, OBS%LIS_STA, NST1 )
           IF ( IST1 .GT. 0 ) THEN
                CBAST(1:8) = OBS%C_STA(IST1)
              ELSE
                CALL INCH ( NST1, CBAST(1:8) )
                CBAST(8:8) = '?'
           END IF
!
           CBAST(9:9)='/'
           IST2 = IFIND_PL ( OBS%L_STA, OBS%LIS_STA, NST2 )
           IF ( IST2 .GT. 0 ) THEN
                CBAST(10:17) = OBS%C_STA(IST2)
              ELSE
                CALL INCH ( NST2, CBAST(10:17) )
                CBAST(17:17) = '?'
           END IF
        END IF
!
        RETURN
        END  !#!  CBAST  #!#
