        FUNCTION ADD_LIS1 ( M, L, LIS, IPAR, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  ADD_LIS  scans the integer list and searches there      *
! *   the element IPAR. If it does not find IPAR in the list, ADD_LIS    *
! *   adds to the end of the list:                                       *
! *        L := L+1                                                      *
! *        LIS(L) := IPAR                                                *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *        M ( INTEGER*4 )  --  maximal length of the list LIS.          *
! *     IPAR ( INTEGER*1 )  --  the element under investigation.         *
! *                                                                      *
! * _________________________ Modified paramters: ______________________ *
! *                                                                      *
! *        L ( INTEGER*4 )  --  The number of elements in the list LIS.  *
! *      LIS ( INTEGER*1 )  --  The list under investigation.            *
! *                             Dimension: M.                            *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  31-JAN-1992    ADD_LIS   v2.1  (c)  L. Petrov 14-MAY-1998  ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  M, L, I_LEN, J1, IUER 
        INTEGER*1  ADD_LIS1, LIS(M), IPAR
        CHARACTER  STR*20, STR1*20
!     
        IF ( L .GT. M ) THEN
           CALL CLRCH (    STR  )
           CALL INCH  ( L, STR  )
           CALL CLRCH (    STR1 )
           CALL INCH  ( M, STR1 )
           CALL ERR_LOG ( 1, IUER, 'ADD_LIS1',                        &
     &             'List overflow: specified length of the list '//   &
     &             '('//STR(1:I_LEN(STR))//') appeared larger '//     &
     &             'than max length ('//STR1(1:I_LEN(STR1))//') ' )
           RETURN
        END IF
        IF ( L.EQ.0 ) THEN
             L=1
        ELSE
           DO 410 J1=1,L
!
! ----------- Scan the list
!
              IF ( LIS(J1).EQ.IPAR ) THEN
                 ADD_LIS1 = INT(J1,1)
                 CALL ERR_LOG ( 0, IUER )
                 RETURN
              END IF
  410      CONTINUE
           L=L+1
        END IF
!
        IF ( L.GT.M ) THEN
           ADD_LIS1 = INT(-1, 1)
           CALL CLRCH ( STR )
           CALL INCH  ( M, STR )
           CALL ERR_LOG ( 2, IUER, 'ADD_LIS1',                          &
     &             'List overflow. Length of the list ('//              &
     &             STR(1:I_LEN(STR))//' appeared too short' )
             RETURN
        END IF
        ADD_LIS1 = INT(L,1)
        LIS(L)=IPAR
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  ADD_LIS  #!#
      
