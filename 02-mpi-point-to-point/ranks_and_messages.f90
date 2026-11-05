!Write a Fortran MPI program where:

!If rank == 0, you set x = 42 and send it to rank 1.

!If rank == 1, you receive x from rank 0 and print it.

!All other ranks do nothing (but still call MPI_INIT and MPI_FINALIZE)

program mpi_learning
    use mpi_f08
    implicit none
    integer:: rank, size, ierr, x

    call MPI_Init(ierr)
    call MPI_Comm_Rank(MPI_COMM_WORLD, rank,ierr)
    if(rank ==0)then
        !x = 42
        call MPI_Send(x, 42, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierr )
    else if (rank ==1) then
        call MPI_Recv(x, 42, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, ierr,status(MPI_STATUS_SIZE) )
    call MPI_Finalize(ierr)
end program mpi_learning