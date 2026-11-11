program labnine 
    implicit none
    ! LAB 09
    ! by lucas frias
    ! <@
    ! (KU//
    !  "
    ! rock chalk!
    ! pardas wednesday 8:00 AM
    ! eecs 210
    ! description: using the DFS
    ! algorithm, will calculate whether 
    ! a graph is connected or not connected


    character(len=20) :: rowline
    integer, dimension(10) :: rowspace
    integer :: n_size 
    integer :: int_space
    integer :: check_n
    integer :: i, j

    !declare our heap allocated matrix

    integer, allocatable :: matrix(:, :)
    integer, allocatable :: identity(:, :)
    integer, allocatable :: result(:, :)
 

    !get the user input
    !TODO: get the actual lab code

     

    n_size = 0
    print *, "Enter your matrix:"
    read (*,'(A)') rowline     

    !can't believe fortran is 1 indexed
    !anyways lets just got and get what the first
    !user input is for the matrix, then size it accordingly


    !here's the iterator for getting the user input    
    do i = 1, len(rowline)
        !remove whitespace
        if ( rowline(i:i) == " ") cycle 
        if ( rowline(i:i) == ",") cycle 
        !check to see if it's not a 0, 1 value
        if ( rowline(i:i) /= "1" .and. rowline(i:i) /= "0") error stop "Error, value is not 1 or 0"
        !now we're gonna process the
        !integer
        read (rowline(i:i), *) int_space
    
        !increment the first row size
        n_size = n_size + 1 
        rowspace (n_size) = int_space

    end do
    !okay now we know nsize

    !lets dallocate this

    allocate(matrix(n_size, n_size))
    allocate(identity(n_size, n_size))
    allocate(result(n_size, n_size))


    !put the value in

    matrix(1, 1:n_size) = rowspace(1:n_size)

    !put the value in
    matrix(1, 1:n_size) = rowspace(1:n_size)

    !now lets allow the user to keep inputting values

    !now we're iterating through the "2nd" index (gross)
    ! and all the way to the size
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
            check_n = check_n + 1 
            rowspace (check_n) = int_space

        end do
        
        !set the checkn
        if (check_n /= n_size) error stop "Error, inconsistent row sizes"
        
        matrix(i,1:n_size) = rowspace(1:n_size)

     end do 
    !not gonna do column check bc i'm lazy  
    !and should be fine anways because we only loop enough
    !to fit the right amount. this should exclude bad values 
    !but not really sure


    !https://thequantasticjournal.com/how-a-matrix-can-tell-if-a-graph-is-connected-d14702bad69e 

    !we're gonna do the (A+I)^n-1        
   
    !first lets generate i 
   
    !just iterate through i j and if they're equal its the diagonal
    do i = 1, n_size

        do j = 1, n_size

            if (j == i) then
                identity(i, j) = 1
            else 
                identity(i, j) = 0
            end if

        end do

    end do


    ! look at the link i included
    ! this is the matrix + its identity
    matrix = matrix + identity
    ! the result is the thing we multiply with again and again
    ! first time is going to be multiplied by the identity
    result = identity


    do i = 1, n_size - 1
        ! yes, fortran has built in matmul
        ! since fotran was intended for scientific and numerical 
        ! stuff this isn't shocking. but its not a library, its built 
        ! into like fortran 90 so i think its fair.
        ! so take mercy on me 
        result = matmul(result, matrix)
    end do


    !https://gcc.gnu.org/onlinedocs/gfortran/ALL.html
    ! pretty neat keyword
    if (all(result > 0)) then
        print *, "Graph is connected"
    else
        print *, "Graph is not connected"
    end if
        

   ! deallocate all of our stock stuff
   deallocate (matrix) 
   deallocate (identity)
   deallocate (result)

end program labnine 
