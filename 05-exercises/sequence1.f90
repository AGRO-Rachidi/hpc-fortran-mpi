!program hi_daniel
 !   use  mpi_f08
  !  Implicit none
   !! integer:: Rank, Size, ierror
    !call MPI_INIT(ierror)
    !call MPI_COMM_RANK(MPI_COMM_WORLD, Rank, ierror)
    !call MPI_COMM_SIZE(MPI_COMM_WORLD, Size, ierror)
    !print *, "Hello from process ", Rank, " of ", Size
    !call MPI_FINALIZE(ierror)
!end program hi_daniel

!Consigne : En exploitant le principe SPMD, modifer le programme précédent afin d'ajouter
!un filtre logique conditionnel (aiguillage). Seul le processus de Rang 0 doit agir en tant que
!processus maître pour aficher une bannière d'initialisation. Les processus de rang impair doivent
!extraire et aficher leur identifiant ainsi que le nom de la machine physique sur laquelle ils
!s'exécutent.

!Donc si je comprends bien, on me demande d'extraire les rangs, puis si la variable rang = 0,
!j'affiche, bienvenue dans cet environnemnt de calcul parallèle, dans cet environnement, nous 
!avons x processeurs qui travaillent en parallèle. Sinon, si mod(rang, 2)/=0, j'extrait et j'affiche rang et le nom d'hôte
!Sinon, je ne fais rien.


!program hi_daniel
    !use mpi_f08
    !implicit none
    !integer:: rank, size, ierr, result_len
    !character(len=MPI_MAX_PROCESSOR_NAME):: name
    !call MPI_INIT(ierr)
    !call MPI_COMM_RANK(MPI_COMM_WORLD, rank,ierr)
    !call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
    !if ( rank == 0 ) then
      !  print*, "[Master] Salut, soyez les bienvenues dans cet environnement de calcul prallèle"
     !   print*, "[Master] Ce programme utilise",size, "processus pour le calcul en parallèle"
    !else if (mod(rank, 2)/=0) then
       ! call MPI_GET_PROCESSOR_NAME(name, result_len, ierr) ! extraction du nom de la machine physique sur lequel tourne le processus
   !     print*, "[Worker]le  processus",rank,"tourne sur : ", trim(name)
    
  !  end if
 !   call MPI_FINALIZE(ierr)
!end program hi_daniel

!Environnement mpi+openmpi

program hybrid_init_mpi
    use mpi_f08
    implicit none
    integer:: rank, fourni!(Stocke le niveau de support de threads réellement accordé par la bibliothèque MPI.)
    integer:: ierr
    call MPI_init_thread(MPI_THREAD_MULTIPLE, fourni, ierr)!Au lieu d'un MPI_Init classique, cette fonction demande un niveau spécifique de support multithread à la bibliothèque MPI.
    !MPI_THREAD_MULTIPLE : C'est le niveau le plus élevé de parallélisme hybride. Il demande que plusieurs threads puissent appeler simultanément des fonctions MPI sans verrouillage externe.
    !La variable fourni reçoit la réponse de la bibliothèque MPI indiquant le niveau qu'elle peut réellement offrir.
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    if (rank == 0) then
        print*, "[DIAGNOSTIC] le niveau requis:", MPI_THREAD_MULTIPLE
    if ( fourni == MPI_THREAD_MULTIPLE ) then
        print * , " [ SUCCESS ] Le cluster supporte le parallelisme hybride avance . "
    else
        print * , " [ WARNING ] Niveau restreint fourni : " , fourni
    end if
    end if
    call MPI_Finalize (ierr)
end program hybrid_init_mpi

