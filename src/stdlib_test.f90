module stdlib_test
  use stdlib_kinds, only: dp,int8,int32
  implicit none
  private
!
  public :: test_stdlib_kind
!
contains
!
  subroutine test_stdlib_kind
!
    real(kind=dp) :: value 
    integer(int8) :: value1
    integer(int32) :: value2
    write(*,*) "Hello, stdlib_test!"
    value=1.24335788_dp;
    write(*,*) 'value= ',value;
    value1=9_int8
    write(*,*) 'value1= ',value1;
    value2=19356777_int32;
    write(*,*) 'value2= ',value2;
!
  end subroutine test_stdlib_kind
!
end module stdlib_test
