program exo1

    use iso_fortran_env, only: real64 ! usage du module standard pour definir le type de variable reelle double precision

    implicit none

    real(kind=real64), allocatable :: tableau(:,:) ! cette ligne declare un tableau dynamique de type reel double precision

    integer :: N
    integer :: M
    integer :: statut ! pour la verification de l'allocation de memoire
    integer :: i, j
    integer :: exercise ! pour l'affichage du tableau dans un fichier

     ! ---------- NOUVELLES variables pour l'inversion ----------
    ! On va avoir besoin d'une DEUXIEME matrice pour l'inverse.
    ! Pourquoi ? Parce que LAPACK VA MODIFIER la matrice qu'on lui donne.
    ! Donc on garde 'tableau' intact, et on travaille sur 'inverse'.
    real(kind=real64), allocatable :: inverse(:,:)

    ! ipiv : tableau d'entiers que LAPACK utilise en interne (on ne le lit pas)
    integer, allocatable :: ipiv(:)

    ! work : tableau de brouillon pour LAPACK (on ne le lit pas non plus)
    real(kind=real64), allocatable :: work(:)

    ! info : code de retour de LAPACK (0 = tout va bien, sinon erreur)
    integer :: info

    ! lwork : taille du tableau work que LAPACK veut
    integer :: lwork


    print *, "Entrez le nombre de lignes du tableau :"

    read *, N ! lecture du nombre de lignes du tableau entre par l'utilisateur

    print *, "Entrez le nombre de colonnes du tableau :"

    read *, M ! lecture du nombre de colonnes du tableau entre par l'utilisateur

    allocate(tableau(N,M), stat=statut) ! allocation dynamique de memoire pour le tableau avec les dimensions N et M

    if (statut /= 0) then ! si statut est different de 0, il y a eu une erreur lors de l'allocation

        print *, "Erreur d'allocation de memoire"

        stop

    end if

    ! Remplissage du tableau avec des valeurs fictives

    ! Matrice identité * 2 : elle est carrée et inversible.
! Son inverse vaut 0.5 sur la diagonale.
do i = 1, N
    do j = 1, M
        if (i == j) then
            tableau(i,j) = 2.0_real64
        else
            tableau(i,j) = 0.0_real64
        end if
    end do
end do

    !do i = 1, N

      !  do j = 1, M

        !    tableau(i,j) = real(i, kind=real64) / real(j, kind=real64) ! division des indices i et j convertie en reel

      !  end do

    !end do
    !affichage du tableau pour verification
    open(newunit=exercise, file="exercise.txt", status="replace")
    write(exercise,*) "Bonjour les amis!"
    write(exercise,*)  "Le tableau est :"

    do i = 1, N
        write(exercise,*) tableau(i,:)
    end do

    ! ============  COMPLEMENT : CALCUL DE L'INVERSE  ============
    ! ============================================================

    ! ---------- Vérification importante ----------
    ! Une matrice n'est inversible QUE si elle est carrée.
    ! Carrée = autant de lignes que de colonnes (N = M).
    if (N /= M) then
        write(exercise,*) " "
        write(exercise,*) "Impossible d'inverser : la matrice n'est pas carree."
        close(exercise)
        deallocate(tableau)
        stop
    end if

    ! ---------- Etape 1 : on copie tableau dans inverse ----------
    ! Pourquoi copier ? Parce que LAPACK va DETRUIRE le contenu de 'inverse'
    ! (il le remplace par sa décomposition interne). On veut donc garder
    ! 'tableau' intact pour l'écrire dans le fichier.
    allocate(inverse(N,N), stat=statut)
    if (statut /= 0) then
        print *, "Erreur d'allocation de memoire pour inverse"
        stop
    end if

    do i = 1, N
        do j = 1, N
            inverse(i,j) = tableau(i,j)
        end do
    end do

       ! ---------- Etape 2 : on prépare ipiv ----------
    ! ipiv doit avoir N cases. On l'alloue.
    allocate(ipiv(N), stat=statut)
    if (statut /= 0) then
        print *, "Erreur d'allocation de ipiv"
        stop
    end if
     ! ---------- Etape 3 : appel de dgetrf (préparation / LU) ----------
    ! Signature : dgetrf(N, N, matrice, N, ipiv, info)
    !   - les deux premiers N : nombre de lignes et de colonnes
    !   - 'inverse' : la matrice à préparer (SERA MODIFIEE)
    !   - le 3ème N : la taille d'une ligne (appelé "leading dimension")
    !   - ipiv : tableau que LAPACK remplit
    !   - info : résultat (0 = OK)
    call dgetrf(N, N, inverse, N, ipiv, info)

    if (info /= 0) then
        write(exercise,*) " "
        write(exercise,*) "Erreur dgetrf, info =", info
        write(exercise,*) "La matrice est probablement singuliere (non inversible)."
        close(exercise)
        deallocate(tableau, inverse, ipiv)
        stop
    end if

    ! ---------- Etape 4 : on demande à LAPACK la taille de work ----------
    ! Astuce classique de LAPACK : on appelle dgetri avec lwork = -1,
    ! et LAPACK écrit dans work(1) la taille optimale qu'il veut.
    ! C'est une "question", pas un vrai calcul.
    lwork = -1
    allocate(work(1), stat=statut)
    if (statut /= 0) then
        print *, "Erreur d'allocation de work (taille 1)"
        stop
    end if

    call dgetri(N, inverse, N, ipiv, work, lwork, info)

    ! Maintenant work(1) contient la taille souhaitée. On la récupère.
    lwork = int(work(1))

    ! On réalloue work avec la bonne taille.
    deallocate(work)
    allocate(work(lwork), stat=statut)
    if (statut /= 0) then
        print *, "Erreur d'allocation de work (taille lwork)"
        stop
    end if

      ! ---------- Etape 5 : appel de dgetri (le vrai calcul) ----------
    ! Signature : dgetri(N, matrice, N, ipiv, work, lwork, info)
    call dgetri(N, inverse, N, ipiv, work, lwork, info)

       if (info /= 0) then
        write(exercise,*) " "
        write(exercise,*) "Erreur dgetri, info =", info
        close(exercise)
        deallocate(tableau, inverse, ipiv, work)
        stop
    end if
     ! ---------- Etape 6 : on écrit l'inverse dans exercise.txt ----------
    ! (le fichier est resté ouvert depuis tout à l'heure)
    write(exercise,*) " "
    write(exercise,*) "L'inverse de la matrice est :"
    do i = 1, N
        write(exercise,*) inverse(i,:)
    end do

    close(exercise)
    
    deallocate(tableau) ! on libere la memoire allouee pour le tableau
    
end program exo1