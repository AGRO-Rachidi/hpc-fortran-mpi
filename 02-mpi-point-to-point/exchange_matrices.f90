program MPI_communicate
    use iso_fortran_env, only: real64
    use mpi
    implicit none

    integer, parameter :: matrix_size = 1000000
    integer :: ierr, rank, num_procs
    real(real64), allocatable :: send_matrix(:), recv_matrix(:)

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)

    if (num_procs < 2) then
        if (rank == 0) print *, 'Run this program with at least 2 MPI processes.'
        call MPI_Finalize(ierr)
        stop
    end if

    allocate(send_matrix(matrix_size), recv_matrix(matrix_size))

    if (rank == 0) then
        send_matrix = 1.0_real64
        call MPI_Send(send_matrix, matrix_size, MPI_DOUBLE_PRECISION, 1, 10, &
                      MPI_COMM_WORLD, ierr)
        call MPI_Recv(recv_matrix, matrix_size, MPI_DOUBLE_PRECISION, 1, 20, &
                      MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
        print *, 'Rank 0 received:', recv_matrix(1)
    else if (rank == 1) then
        call MPI_Recv(recv_matrix, matrix_size, MPI_DOUBLE_PRECISION, 0, 10, &
                      MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
        send_matrix = 2.0_real64 * recv_matrix
        call MPI_Send(send_matrix, matrix_size, MPI_DOUBLE_PRECISION, 0, 20, &
                      MPI_COMM_WORLD, ierr)
    end if

    call MPI_Finalize(ierr)
end program MPI_communicate