program newton_dynamique
  use mpi_f08
  implicit none
  integer, parameter :: NSYS = 150                  ! nombre de systemes a resoudre
  integer, parameter :: TAG_TRAVAIL = 1, TAG_STOP = 2
  integer :: rank, nproc

  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, nproc)

  ! Garde : sans esclave, le maitre attendrait pour toujours
  if (nproc < 2) then
     print *, "Erreur : il faut au moins 2 processus (1 maitre + 1 esclave)."
     call MPI_Abort(MPI_COMM_WORLD, 1)
  end if

  if (rank == 0) then
     call maitre()
  else
     call esclave()
  end if

  call MPI_Finalize()

contains

  !------------------------------------------------------------------
  ! MAITRE : distribue les systemes au premier esclave libre
  !------------------------------------------------------------------
  subroutine maitre()
    integer :: prochain, e, i, qui, pilule
    integer :: tache_de(nproc - 1)        ! tache_de(e) = systeme en cours chez l'esclave e
    integer :: traite_par(NSYS)           ! quel esclave a resolu chaque systeme
    real(8) :: racines(NSYS), x
    type(MPI_Status) :: status

    prochain = 1
    pilule   = -1
    tache_de = 0

    ! Distribution initiale : un systeme par esclave, pilule pour les esclaves en trop
    do e = 1, nproc - 1
       if (prochain <= NSYS) then
          call MPI_Send(prochain, 1, MPI_INTEGER, e, TAG_TRAVAIL, MPI_COMM_WORLD)
          tache_de(e) = prochain
          prochain = prochain + 1
       else
          call MPI_Send(pilule, 1, MPI_INTEGER, e, TAG_STOP, MPI_COMM_WORLD)
       end if
    end do

    ! Boucle principale : une reception par systeme
    do i = 1, NSYS
       call MPI_Recv(x, 1, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, TAG_TRAVAIL, &
                     MPI_COMM_WORLD, status)
       qui = status%MPI_SOURCE
       racines(tache_de(qui))    = x       ! on range la racine au bon endroit
       traite_par(tache_de(qui)) = qui

       if (prochain <= NSYS) then          ! travail suivant au premier libre
          call MPI_Send(prochain, 1, MPI_INTEGER, qui, TAG_TRAVAIL, MPI_COMM_WORLD)
          tache_de(qui) = prochain
          prochain = prochain + 1
       else                                ! plus de travail : pilule empoisonnee
          call MPI_Send(pilule, 1, MPI_INTEGER, qui, TAG_STOP, MPI_COMM_WORLD)
       end if
    end do

    ! Affichage des resultats
    do i = 1, NSYS
       print '(A,I4,A,F12.9,A,I3)', "Systeme ", i, " : racine = ", racines(i), &
                                    "   (esclave ", traite_par(i)
    end do
    print *, "Erreur maximale par rapport a sqrt(2) :", &
             maxval(abs(racines - sqrt(2.0d0)))
  end subroutine maitre

  !------------------------------------------------------------------
  ! ESCLAVE : resout les systemes recus jusqu'a la pilule
  !------------------------------------------------------------------
  subroutine esclave()
    integer :: k, n, niter, compteur
    real(8) :: x
    type(MPI_Status) :: status

    compteur = 0
    do
       call MPI_Recv(k, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD, status)
       if (status%MPI_TAG == TAG_STOP) exit          ! pilule recue : on s'arrete

       ! Hypothese : 1 systeme sur 5 est une zone de front de flamme (lourde)
       if (mod(k, 5) == 0) then
          niter = 5000                               ! front de flamme
       else
          niter = 10                                 ! zone froide et homogene
       end if

       ! Newton-Raphson pour f(x) = x^2 - 2, f'(x) = 2x, x0 = 1
       x = 1.0d0
       do n = 1, niter
          x = x - (x*x - 2.0d0) / (2.0d0*x)
       end do

       compteur = compteur + 1
       call MPI_Send(x, 1, MPI_DOUBLE_PRECISION, 0, TAG_TRAVAIL, MPI_COMM_WORLD)
    end do

    print *, "Esclave", rank, ":", compteur, "systemes resolus"
  end subroutine esclave

end program newton_dynamique