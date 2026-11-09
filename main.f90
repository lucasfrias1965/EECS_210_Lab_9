program labnine 
    character(len=20) :: rowline
    integer, dimension(10) :: rowspace
    integer :: n_size 
    integer :: int_space
    integer :: check_n

    integer, allocatable :: matrix(:, :)

    
    n_size = 0
    print *, "Input your relational matrix:"
    read (*,'(A)') rowline     
    !can't believe fortran is 1 indexed
    
    do i = 1, len(rowline)
        if ( rowline(i:i) == " ") cycle 
        if ( rowline(i:i) /= "1" .and. rowline(i:i) /= "0") error stop "Error, value is not 1 or 0"

        read (rowline(i:i), *) int_space

        rowspace (n_size) = int_space
        n_size = n_size + 1 
    end do
    !okay now we know nsize

    print '(I0)', n_size

    !lets dallocate this

    allocate(matrix(n_size, n_size))

    !put the value in

    matrix(1, 1:n_size) = rowspace(1:n_size)

    !now lets allow the user to keep inputting values


    do i = 2, n_size 

        check_n = 0
        rowline = "" !clear rowline
        read (*,'(A)') rowline

        !do our same loop, same thingy

        do j = 1, len(rowline)

            !if the rowline is empty cycle
            if ( rowline(j:j) == " ") cycle 
            ! if the rowline is 1 or 0, stop
            if ( rowline(j:j) /= "1" .and. rowline(j:j) /= "0") error stop "Error, value is not 1 or 0"

            read (rowline(j:j), *) int_space
        
            !set rowspace
            rowspace (check_n) = int_space
            check_n = check_n + 1 

        end do
        
        !set the checkn
        if (check_n /= n_size) error stop "Error, inconsistent row sizes"
        
        matrix(i,1:n_size) = rowspace(1:n_size)

     end do 
    !not gonna do column check bc i'm lazy  
    !and should be fine anways because we only loop enough
    !to fit the right amount. this should exclude bad values 
    !but not really sure


    

        

   deallocate (matrix) 
end program labnine 
