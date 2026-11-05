!prochain ← 1

!// Démarrage : un bloc à chaque esclave
!pour e allant de 1 à nproc − 1 :
  !  envoyer prochain à l'esclave e (tag TRAVAIL)
 !   prochain ← prochain + 1

!// Boucle principale : une réception par bloc
!répéter Nbloc fois :
    !recevoir un message de N'IMPORTE QUI (MPI_ANY_SOURCE), avec status
    !qui ← status%MPI_SOURCE                  // qui vient de finir ?
    !si prochain ≤ Nbloc alors
     !   envoyer prochain à qui (tag TRAVAIL)  // client suivant !
    !    prochain ← prochain + 1
   ! sinon
  !      envoyer −1 à qui (tag STOP)           // la pilule
 !   fin si
!fin répéter

!répéter indéfiniment :
 !   recevoir un message du maître (n'importe quel tag), avec status
  !  si status%MPI_TAG = STOP alors
   !     sortir de la boucle                   // pilule reçue : on s'arrête
    !fin si
    !traiter le bloc reçu
    !envoyer au maître le numéro du bloc traité (tag TRAVAIL)   // "j'ai fini"
!fin répéter

program repartition_dynamique
  use mpi_f08
  implicit none
  integer, parameter :: NBLOC = 120
  integer, parameter :: TAG_TRAVAIL = 1, TAG_STOP = 2
  integer :: rank, nproc

  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, nproc)

  ! (1) Garde : sans esclave, le maître attendrait pour toujours
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

  subroutine maitre()
    integer :: prochain, e, i, qui, bloc_fini, pilule
    type(MPI_Status) :: status

    prochain = 1                         ! prochain ← 1
    pilule = -1

    ! (2) Distribution initiale
    do e = 1, nproc - 1                  ! pour e allant de 1 à nproc − 1
       if (prochain <= NBLOC) then       !   si prochain ≤ Nbloc
          call MPI_Send(prochain, 1, MPI_INTEGER, e, TAG_TRAVAIL, MPI_COMM_WORLD)
          prochain = prochain + 1
       else                              !   sinon : pas de travail pour toi
          call MPI_Send(pilule, 1, MPI_INTEGER, e, TAG_STOP, MPI_COMM_WORLD)
       end if
    end do

    ! (3) Boucle principale : une réception par bloc
    do i = 1, NBLOC                      ! répéter Nbloc fois
       call MPI_Recv(bloc_fini, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, &
                     MPI_COMM_WORLD, status)
       qui = status%MPI_SOURCE           ! qui vient de finir ?
       if (prochain <= NBLOC) then       ! client suivant !
          call MPI_Send(prochain, 1, MPI_INTEGER, qui, TAG_TRAVAIL, MPI_COMM_WORLD)
          prochain = prochain + 1
       else                              ! plus de travail : la pilule
          call MPI_Send(pilule, 1, MPI_INTEGER, qui, TAG_STOP, MPI_COMM_WORLD)
       end if
    end do
  end subroutine maitre

  subroutine esclave()
    integer :: bloc
    type(MPI_Status) :: status

    do                                   ! répéter indéfiniment
       ! (4) recevoir du maître, n'importe quel tag
       call MPI_Recv(bloc, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD, status)
       ! (5) pilule reçue : on sort
       if (status%MPI_TAG == TAG_STOP) exit
       ! (6) traiter le bloc
       call traiter_bloc(bloc)
       print *, "Esclave", rank, "a traite le bloc", bloc
       ! (7) dire "j'ai fini" au maître
       call MPI_Send(bloc, 1, MPI_INTEGER, 0, TAG_TRAVAIL, MPI_COMM_WORLD)
    end do
  end subroutine esclave

  subroutine traiter_bloc(bloc)          ! fournie : simule le travail
    integer, intent(in) :: bloc
    integer :: nops, k
    real(8) :: x
    if (mod(bloc, 4) == 0) then
       nops = 20000                      ! forêt dense
    else
       nops = 500                        ! savane ou sol nu
    end if
    x = 0.0d0
    do k = 1, nops
       x = x + sin(dble(k))
    end do
  end subroutine traiter_bloc

end program repartition_dynamique