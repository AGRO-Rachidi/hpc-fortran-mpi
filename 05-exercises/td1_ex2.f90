!On souhaite concevoir une application distribuée où les processus effectuent des tâches radicalement hétérogènes en fonction de leur identifiant unique au sein du communicateur global
!MPI_COMM_WORLD.
!a) Écrire la structure logique d’aiguillage (en pseudo-code ou syntaxe C++/Fortran) permettant de distribuer les rôles suivants sur un cluster de P processeurs :
!— Le processus de rang 0 agit comme coordinateur central.
!Les processus dont le rang est pair (et supérieur à 0) exécutent une routine de
!simulation physique locale.
!— Les processus dont le rang est impair exécutent un algorithme d’optimisation de
!contraintes logistiques.
!b) Si ce programme est exécuté par erreur sur un seul et unique cœur (P = 1), décrivez le
!comportement de votre architecture logique. Quels sont les risques potentiels en termes de
!gestion mémoire ou d’échec d’exécution ?

!a) Structure logique d’aiguillage en pseudo-code :
program td_aiguillage
    use mpi_f08
    implicit none
    integer:: rank, P
    call MPI_Init()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, P)
    if(P < 3) then
        if (rank ==0) then
            print*, "Erreur : le programme nécessite au moins 3 processus pour fonctionner correctement."
            call MPI_Abort(MPI_COMM_WORLD, 1)
        end if
    end if
    if (rank == 0) then
        ! coordianteur_central()
    else if (mod(rank, 2) == 0) then
        ! simulation_physique_local()
    else
        ! optimisation_logistique()
    end if
    call MPI_Finalize()
end program td_aiguillage

!b-)
!si ce programme est exécuté sur un seul coeur, cela voudra dire que  seul le processus 0
!aura la chance de s'exécuter et pas les autres , et dans ce cas, on peut assister à un blocage du programme 
!du fait que le processus attendrait des messages en provenance des autres processus pour annoncer
!la fin de l'exécution dans un scénario o`u il fait du recv .
! il y a aussi le risque de depassement de tampon mémoire si le processus 0 envoie des messages 
!à des processus qui n'existent pas, ce qui pourrait provoquer des erreurs de segmentation ou des fuites de mémoire.

