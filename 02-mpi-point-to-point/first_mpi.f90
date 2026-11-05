program first_mpi
    use mpi
    implicit none
    integer:: ierr
    integer:: rank, nprocs
    call MPI_Init(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
    print *, 'Rank:', rank, 'Size:', size
    call MPI_Finalize(ierr)

    
end program first_mpi
