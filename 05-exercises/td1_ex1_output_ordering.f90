!1.1 Exercice 1.1 : Le non-déterminisme des flux d’affichage
!Soit le code séquentiel classique suivant traduit en parallélisme MPI brut où P processus
!exécutent le même bloc de code :

!// Execute par tous les processus en parallele ( SPMD )
!std :: cout << " Debut du calcul sur le processus " << rank << std :: endl ;
!// ... Zone de calcul intense fictive ...
!std :: cout << " Fin du calcul sur le processus " << rank << std :: endl ;

!a) Si ce programme est lancé sur P = 4 processus, l’affichage sur la console de la frontale de
!calcul suivra-t-il systématiquement l’ordre des rangs (0, 1, 2, 3) ? Justifiez votre réponse
!en détaillant le comportement de l’ordonnanceur du système d’exploitation et l’impact du
!réseau.
!b) Proposez une solution algorithmique simple (sans utiliser de primitives complexes ou collectives) pour forcer le processus de rang 0 à afficher son message de fin strictement après
!que tous les autres processus ont affiché leur message de début.

!Résultat
! Non, l'affichage ne suivra pas forcément cet ordre parce que le fonctionnement de l'ordonnanceur du sytème qui est celui
! de  réguler l'usage du processeur par les programmes ne dépend pas du rang du processus.Il n'autorise un programme à utiliser 
! le processeur que lorsque le processeur est disponible , et ce en fonction de la charge de la machine à l'instant, les coeurs libres, les processus en cours, etc
! pas en fonction de son numero d'ordre. Il faut aussi noter que les 4 processus ne se lancent pas au même instant

! En ce qui concerne l'impact du réseau, lorsque plusieurs machines travaillent ensemble sur un même programme, et donc plusieurs processus sont en jeu, pour centraliser les résultats,
! il faut que ces machines puissent envoyer leur texte affiché au lanceur, ici mpirun qui collecte les textes affichés par chacun de ces processus dans l'ordre d'arriver. Et pour effectuer cet envoi, il faut passer par un réseau
! Et c'est là qu'intervient le partage par réseau. Lors de ce partage, on peut être confronté à des problèmes de latence et autres.

!Q2:« Fais en sorte que le processus 0 affiche sa ligne “Fin” seulement quand tous les autres ont affiché leur ligne “Début”. »
!Avec une contrainte : pas de primitives collectives.
!Une primitive, c’est simplement une fonction de base fournie par MPI.
!Une primitive collective fait participer tout le monde d’un coup (par exemple MPI_Barrier, « tout le monde attend tout le monde »). Elle est interdite ici 

program td
    use mpi_f08
    implicit none
    integer:: rank, P, jeton,i
    call MPI_Init()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, P)
    
    jeton = 0
    if (rank == 0) then
        print*, "Debut du calcul des processus"
        do i = 1, P-1
            call MPI_Recv(jeton, 1, MPI_INTEGER, MPI_ANY_SOURCE, 10, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
        end do
        print*, "Fin du calcul des processus"
    else 
        call MPI_Send(jeton, 1, MPI_INTEGER, 0, 10, MPI_COMM_WORLD)
    end if
    call MPI_Finalize()
    !✅ Exercice 1.1 terminé : ce que tu dois retenir
    !Plusieurs processus = aucun ordre garanti entre eux. L’ordre est garanti seulement à l’intérieur d’un processus.
    !Pour imposer un ordre, on construit une chaîne de messages. Un message va de celui qui sait vers celui qui attend.
    !Un Recv = un message. Autant de Send que de Recv.
    !L’affichage à l’écran ne prouve rien sur l’ordre d’exécution.
    
end program td