program carre_matrices
  use mpi_f08
  implicit none
  integer, parameter :: n = 5, nblocs = 4
  integer :: rank, P, i, j, k
  real(8):: temps_debut, temps_fin, temps_total
  real(8) :: toutes(n, n, nblocs)      ! classeur : feuille 1 vide (root), feuilles 2,3,4 = matrices
  real(8) :: resultats(n, n, nblocs)   ! classeur des carrés, rempli par Gather
  real(8) :: A(n, n), C(n, n)          ! A = ma matrice reçue, C = son carré

  call MPI_Init()
  temps_debut = MPI_Wtime()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, P)

  ! (1) Garde : il faut exactement 1 root + 3 esclaves
  if (P /= nblocs) then
     if (rank == 0) then
        print *, "Erreur : ce programme demande exactement", nblocs, "processus."
        print *, "Nombre de processus fourni :", P
        print *, "Relancez avec : mpirun -np 4 ./carre_matrices"
     end if
     call MPI_Abort(MPI_COMM_WORLD, 1)
  end if

  ! (2) Le root prépare les matrices
  if (rank == 0) then
     toutes = 0.0d0                    ! tout le classeur à zéro
     do i = 1, n
        toutes(i, i, 2) = 1.0d0        ! matrice 1 : identité (diagonale à 1)
        toutes(i, i, 4) = 2.0d0        ! matrice 3 : 2 x identité (diagonale à 2)
     end do
     toutes(:, :, 3) = 1.0d0           ! matrice 2 : toute la feuille à 1
  end if

  ! (3) Distribution : chacun reçoit n*n réels (une feuille) dans A
  call MPI_Scatter(toutes, n*n, MPI_DOUBLE_PRECISION, &   ! quoi envoyer, combien PAR processus, type
                   A,      n*n, MPI_DOUBLE_PRECISION, &   ! où ranger, combien, type
                   0, MPI_COMM_WORLD)                     ! root, groupe

  ! (4) Calcul : les esclaves font le produit matriciel, le root ne fait rien
  if (rank /= 0) then
    print*, "  Ici, c'est le processus", rank, "mon bloc est:", A
     C = matmul(A, A)
     print*, " Ici, c'est le processus", rank, "mon carré est:", C
  else
     C = 0.0d0
  end if

  ! (5) Ramassage : chacun renvoie C, le root range la part du rang k dans la feuille k+1
  call MPI_Gather(C,         n*n, MPI_DOUBLE_PRECISION, &
                  resultats, n*n, MPI_DOUBLE_PRECISION, &
                  0, MPI_COMM_WORLD)

  ! (6) Affichage par le root
  if (rank == 0) then
     do k = 2, nblocs
        print *, "Carre de la matrice", k - 1, "(calcule par le rang", k - 1, ")"
        do i = 1, n
           print '(5F8.2)', (resultats(i, j, k), j = 1, n)
        end do
        print *
     end do
  end if
  temps_fin = MPI_Wtime()
  temps_total = temps_fin - temps_debut
  print *, "Temps total d'exécution :", temps_total,"s"
  call MPI_Finalize()
end program carre_matrices

