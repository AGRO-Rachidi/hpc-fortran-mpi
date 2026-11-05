program hello_mpi
  implicit none
  include 'mpif.h'
  integer :: rank, size, ierror

  ! 1. Initialiser l'environnement MPI
  call MPI_Init(ierror)

  ! 2. Gérer les propriétés du communicateur global
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
  print *, "Processus ", rank, " sur un total de ", size, " processus."

  ! 3. Clôturer l'environnement MPI
  call MPI_Finalize(ierror)
end program hello_mpi