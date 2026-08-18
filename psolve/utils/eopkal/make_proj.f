      SUBROUTINE MAKE_PROJ ( NUM_EOP, KUSE_RATE, PROJ, IXREF )
      IMPLICIT NONE
      INCLUDE 'eopkal_ut.i'
      INCLUDE 'eopkal_pm.i'
!
      INTEGER*2 num_eop                !number of eop estimated
      LOGICAL kuse_rate(3)             ! Do we use rate info?
      REAL*8     proj(num_pm+num_ut,num_eop)    !Projection from measurement space to filter space.
      INTEGER*2 ixref(3)               !Correspondence between EOP and filter
      INTEGER*2 i_out,i_in             !projection matrix indices
!
      WRITE ( 6, * ) ' num_pm = ',num_pm,' num_ut=',num_ut,' num_eop=',num_eop ! %%%%
      proj=0
      proj(1,1)=1                     !Polar motion and UT values project directly.
      proj(2,2)=1
      proj(num_pm+1,3)=1
      ixref(1)=1
      ixref(2)=2
      ixref(3)=num_pm+1
!
      IF(num_eop .EQ. 3) return
      i_in=3
      IF(kuse_rate(1)) then
        i_in=i_in+1
        proj(1,i_in)=-gamma
        proj(2,i_in)=sigma
        proj(3,i_in)=gamma
        proj(4,i_in)=sigma
        i_out=5
        IF(kpm_annual) then
          proj(i_out,1)=sigma
          i_out=i_out+2
        endif
        IF(kpm_linear) then
          proj(i_out,1)=1.
        endif
      endif
      IF(kuse_rate(2)) then
        i_in=i_in+1
        proj(1,i_in)=-sigma
        proj(2,i_in)=-gamma
        proj(3,i_in)=sigma
        proj(4,i_in)=-gamma
        i_out=5
        IF(kpm_annual) then
          proj(i_out,1)=-gamma
          i_out=i_out+2
        endif
        IF(kpm_linear) then
          proj(i_out+1,1)=1.
        endif
      endif
      IF(kuse_rate(3)) then
        i_in=i_in+1
        i_out=num_pm+2
        proj(i_out,i_in)=1.
        IF(kut_annual) then
           proj(i_out,i_in)=1
           i_out=i_out+2
        endif
        IF(kut_semi) then
           proj(i_out,i_in)=1
           i_out=i_out+2
        endif
      endif
      RETURN
      END  !#!  MAKE_PROJ  #!#
