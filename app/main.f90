program main
  use stdlib_test, only: test_stdlib_kind
  use stdlib_logger_test,only: test_logger;
  implicit none
!
  call test_stdlib_kind();
  call test_logger();
!  
end program main
