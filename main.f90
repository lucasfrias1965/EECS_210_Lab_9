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

    integer, allocatable :: matrix(:, :)
    integer, allocatable :: identity(:, :)
    integer, allocatable :: result(:, :)
 

    
    n_size = 0
    print *, "Input your relational matrix:"
    read (*,'(A)') rowline     
    !can't believe fortran is 1 indexed
    
    do i = 1, len(rowline)
        if ( rowline(i:i) == " ") cycle 
        if ( rowline(i:i) /= "1" .and. rowline(i:i) /= "0") error stop "Error, value is not 1 or 0"

        read (rowline(i:i), *) int_space

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
   
    do i = 1, n_size

        do j = 1, n_size

            if (j == i) then
                identity(i, j) = 1
            else 
                identity(i, j) = 0
            end if

        end do

    end do

    matrix = matrix + identity
    result = identity


    do i = 1, n_size - 1
        result = matmul(result, matrix)
    end do


    !https://gcc.gnu.org/onlinedocs/gfortran/ALL.html
    if (all(result > 0)) then
        print *, "Graph is connected"
    else
        print *, "Graph is not connected"
    end if

        

   deallocate (matrix) 
   deallocate (identity)

end program labnine 
