!fpm build --profile release --link-flag "-LC:\Users\khaor\AppData\Roaming\local\lib\libstdlib.a"
! fpm run test_logger --profile release --flag "-I/ucrt64/include/fortran_stdlib/GNU-14.1.0/"
program test_stlib
    use stdlib_kinds, only: wp=>dp;
    use stdlib_error, only: error_stop,check;
    use stdlib_logger; !, only: global_logger;
    implicit none;
    !
    character(len=256) :: iomsg
    real(kind=wp) :: val
    integer :: iostat, unit1, unit2, unit3, unit4, unit5, stat , nunita, max_width
    logical :: add_blank_line, exist, indent, time_stamp, opened
    integer,allocatable :: log_units(:)
    type(logger_type) :: my_logger
    character(10) str
    integer :: nums(10) = (/ 1,4,0,2,3,5,6,7,8,9 /)
    
    val=12.0_wp;
    write(*,*) global_logger%log_units_assigned()
    if(global_logger%log_units_assigned() == 0) then
       write(*,*) 'No units assigned';
    else
        call error_stop("Logger has units");
    endif
    !
    nunita=global_logger%log_units_assigned();
    call check(nunita==0);
    !
    call global_logger%configuration(add_blank_line=add_blank_line, &
            indent=indent, max_width=max_width, time_stamp=time_stamp, &
            log_units=log_units );
    if(.not. add_blank_line) then
        write(*,*) 'add_blank_line is false';
    endif
    if(.not. indent) then
        write(*,*) 'indent is false';
    else
        write(*,*) 'indent is true';
    endif
    if(max_width == 0) then
        write(*,*) 'max_width is 0';
    endif
    if(time_stamp) then
        write(*,*) 'time_stamp is true';
    endif
    if(size(log_units) == 0) then
        write(*,*) 'not log_units assigned';
    endif
    !
    call global_logger%log_information('This message should be output to OUTOUT_UNIT',&
        module = 'N/A', procedure = 'test_stlib');
    call global_logger%log_information('This message should be output to OUTOUT_UNIT' &
        // new_line('a') // 'This is a new line', module = 'N/A', procedure='test_stlib');
    call global_logger%log_debug('This message should be output to OUTOUT_UNIT', &
        module = 'N/A', procedure = 'test_stlib');
     call global_logger%log_debug( 'This message should be output ' //       &
            'to OUTPUT_UNIT, unlimited in width, not preceded by ' //      &
            'a blank line, then by a time stamp, then by MODULE % ' //     &
            'PROCEDURE, be prefixed by DEBUG. ' // new_line('a') //        &
            'This is a new line of the same log message.',                 &
            module = 'N/A',                                                &
            procedure = 'TEST_STDLIB_LOGGER' );
    call global_logger%configure(add_blank_line=.true.,indent=.false.,max_width=80,time_stamp=.false.);
    call global_logger%log_information('This message should be output to OUTOUT_UNIT',&
        module = 'N/A', procedure = 'test_stlib');
    call global_logger%log_information('This message should be output to OUTOUT_UNIT' &
        // new_line('a') // 'This is a new line', module = 'N/A', procedure='test_stlib');
    !
    ! Test open log files
    !
    call global_logger%add_log_file('first_log_file.log',unit=unit1,stat=stat);
    if(stat == success) then
        write(*,*) 'Log file opened';
    else
        write(*,*) 'unable to open the log file';
    endif
    if(global_logger%log_units_assigned() == 1) then
        write(*,*) 'Unit assigned';
    endif
    call global_logger%add_log_file('second_log_file.log',unit=unit2,stat=stat);
    if(stat == success) then
        write(*,*) 'Second log file opened';
    else
        write(*,*) 'unable to open the second log file';
    endif
    call global_logger%add_log_file('third_log_file.log',unit=unit3,stat=stat);
    if(stat == success) then
        write(*,*) 'Third log file opened';
    else
        write(*,*) 'unable to open the third log file';
    endif
    call global_logger%add_log_file('fourth_log_file.log',unit=unit4,stat=stat);
    if(stat == success) then
        write(*,*) 'Fourth log file opened';
    else
        write(*,*) 'unable to open the fourth log file';
    endif
    call global_logger%add_log_file('fifth_log_file.log',unit=unit5,stat=stat);
    if(stat == success) then
        write(*,*) 'Fifth log file opened';
    else
        write(*,*) 'unable to open the fifth log file';
    endif
    !
    !
    !
    write(str,'(10i1)') nums;    
    !
    ! Test levels
    !
    call global_logger%configure(level=all_level);
    call global_logger%log_message('All level');
    call global_logger%log_message('This message should be always printed');
    call global_logger%log_message(str);
    call global_logger%log_debug('Debug level message');
    call global_logger%log_information('Information level message');
    call global_logger%log_warning('Warning level message');
    call global_logger%log_error('Error level message');
    call global_logger%log_io_error('IO Error level message');
    !
    call global_logger%configure(level=information_level);
    call global_logger%log_message('Information level');
    call global_logger%log_debug('Debug message should not be printed');
    call global_logger%log_information('Information message should be printed');
    call global_logger%log_warning('Warning level message should be printed');
    call global_logger%log_error('Error level message should be printed');
    call global_logger%log_io_error('IO Error level message should be printed');
    !
    call global_logger%configure(level=warning_level);
    call global_logger%log_message('Warning level');
    call global_logger%log_debug('Debug message should not be printed');
    call global_logger%log_information('Information message should be not printed');
    call global_logger%log_warning('Warning level message should be printed');
    call global_logger%log_error('Error level message should be printed');
    call global_logger%log_io_error('IO Error level message should be printed');
    !
    call global_logger%configure(level=error_level);
    call global_logger%log_message('Error level');
    call global_logger%log_debug('Debug message should not be printed');
    call global_logger%log_information('Information message should be not printed');
    call global_logger%log_warning('Warning level message should be printed');
    call global_logger%log_error('Error level message should be printed');
    call global_logger%log_io_error('IO Error level message should be printed');
    !
    call global_logger%configure(level=none_level);
    call global_logger%log_message('None level');
    call global_logger%log_debug('Debug message should not be printed');
    call global_logger%log_information('Information message should not be printed');
    call global_logger%log_warning('Warning level message should not be printed');
    call global_logger%log_error('Error level message should not be printed');
    call global_logger%log_io_error('IO Error level message should not be printed');
    !
    ! Test remove log files
    !
    write(*,*) 'Removing log files';
    call global_logger%remove_log_unit(unit5,close_unit=.true.,stat=stat);
    inquire(unit=unit5,opened=opened);
    if(opened) then
        write(*,*) 'unit5 is opened';
    else
        write(*,*) 'unit5 is closed'; 
    endif
    if(global_logger%log_units_assigned() == 4) then
        write(*,*) 'Log files = 4';
    endif
    call global_logger%remove_log_unit(unit4,close_unit=.true.,stat=stat);
    if(global_logger%log_units_assigned() == 3) then
        write(*,*) 'Log files = 3';
    endif
    call global_logger%remove_log_unit(unit3,close_unit=.true.,stat=stat);
    if(global_logger%log_units_assigned() == 2) then
        write(*,*) 'Log files = 2';
    endif
    call global_logger%remove_log_unit(unit2,close_unit=.true.,stat=stat);
    if(global_logger%log_units_assigned() == 1) then
        write(*,*) 'Log files = 1';
    endif
    call global_logger%remove_log_unit(unit1,close_unit=.true.,stat=stat);
    if(global_logger%log_units_assigned() == 0) then
        write(*,*) 'Log files = 0';
    endif
    stop
!
end program test_stlib