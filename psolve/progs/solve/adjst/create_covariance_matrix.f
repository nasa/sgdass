       SUBROUTINE CREATE_COVARIANCE_MATRIX ( NUM_PARMS, PARM_LIST, MAT, &
     &                                       COV_MAT, IERR4 )
       IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!      This routine will create a full (square) covariance matrix.
!
!      Input
       integer*4  num_parms                    ! The number of the parameters in parm_list.
       integer*4  parm_list(num_parms)         ! The parameter numbers for the covariance matrix.
       real*8 mat(*)                           ! where SOLVE stores covariance
!
!      Output:
       real*8     cov_mat(num_parms,num_parms) ! The full covariance matrix in SOLVE form.
       integer*4 ierr4 !error flag
!
       integer*4 i,j
       real*8 return_covariance
      INTEGER*2  INT2_ARG
!
!      :96.05.21:jwr: Created.
!
       DO I=1,NUM_PARMS
         DO J = I,NUM_PARMS
            COV_MAT(I,J) = RETURN_COVARIANCE ( PARM_LIST(I), PARM_LIST(J), MAT )
            IF ( I .NE. J ) COV_MAT(J,I) = COV_MAT(I,J)
         ENDDO
       ENDDO
       CALL ERR_LOG ( 0, IERR4 )
!
       RETURN
       END  !#!  CREATE_COVARIANCE_MATRIX  #!#
