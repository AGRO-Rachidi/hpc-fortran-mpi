program matrice
    use iso_fortran_env, only: real64
    implicit none
    !include 'mpif.h'
    !integer :: ierror
    integer:: statut
    integer:: N, i, j
    integer, allocatable:: matrix(:,:)
    integer, allocatable:: matrix_sum(:,:)
    real(kind=real64), allocatable:: matrix_inverse(:,:)
    real(kind=real64):: det
    real(kind=real64), allocatable:: matrix_augmente(:,:)
    integer:: matrix_file
    print *, "entrez la taille de la matrice:\n"
    read *, N
    if (N<=0) then
        print *, "Erreur: la taille de la matrice doit être positive."
        stop
    end if
    allocate(matrix(N,N), stat=statut)
    if (statut/=0) then
        print *, "Erreur d'allocation de mémoire"
        stop
    end if
    do i = 1, N
        print*, "Entrer les éléments de la ligne ", i
        read *, matrix(i,:)
    end do
    open(newunit=matrix_file, file="matrix.txt", status="replace")
    print *, "La matrice est  dans le fichier matrix.txt"
    write(matrix_file,*) N 
    do i = 1, N
        write(matrix_file,*) matrix(i,:)
    end do
    close(matrix_file)
    open(newunit=matrix_file, file="matrix.txt", status="old", action="read")
    read(matrix_file, *) N
    do i = 1, N
        read(matrix_file, *) matrix(i,:)
    end do
    close(matrix_file)
    print *, "la matrice lue à partir du fichier matrix.txt est: " 
    do j = 1, N
        print *,matrix(j,:)
    end do 
    
    
    allocate(matrix_sum(N,N), stat=statut)
    if (statut/=0) then
        print *, "Erreur d'allocation de mémoire"
        stop
    end if
    matrix_sum = 0
    do i = 1, N
        do j = 1, N
            matrix_sum(i,j) = matrix(i,j) + matrix(i,j)
        end do
    end do
    print *, "la somme de la matrice avec elle-même est: "
    do j = 1, N
        print *,matrix_sum(j,:)
    end do
    allocate(matrix_inverse(N,N), stat=statut)
    if (statut/=0) then
        print *, "Erreur d'allocation de mémoire"
        stop
    end if
    allocate(matrix_augmente(N,2*N), stat=statut)
    if (statut/=0) then
        print *, "Erreur d'allocation de mémoire"
        stop
    end if
    if (N==2) then
        det = matrix_sum(1,1)*matrix_sum(2,2) - matrix_sum(1,2)*matrix_sum(2,1)
        if (det == 0.0_real64) then
            print *, "La matrice n'est pas inversible"
        else
            matrix_inverse(1,1) = matrix_sum(2,2)/det
            matrix_inverse(1,2) = -matrix_sum(1,2)/det
            matrix_inverse(2,1) = -matrix_sum(2,1)/det
            matrix_inverse(2,2) = matrix_sum(1,1)/det
            print *, "l'inverse de la matrice est: "
            do j = 1, N
                print *,matrix_inverse(j,:)
            end do
        end if
    else
        do i = 1, N
            do j = 1, N
                matrix_augmente(i,j) = matrix_sum(i,j)
                if (i == j) then
                    matrix_augmente(i,N+j) = 1.0_real64
                else
                    matrix_augmente(i,N+j) = 0.0_real64
                end if
            end do
        end do
        ! Application de l'algorithme de Gauss-Jordan pour inverser la matrice
        do i = 1, N
            ! Normalisation de la ligne i
            det = matrix_augmente(i,i)
            if (det == 0.0_real64) then
                print *, "La matrice n'est pas inversible"
                stop
            end if
            matrix_augmente(i,:) = matrix_augmente(i,:)/det
            ! Elimination des autres lignes
            do j = 1, N
                if (j /= i) then
                    matrix_augmente(j,:) = matrix_augmente(j,:) - matrix_augmente(j,i)*matrix_augmente(i,:)
                end if
            end do
        end do
        ! Extraction de l'inverse de la matrice augmentée
        do i = 1, N
            matrix_inverse(i,:) = matrix_augmente(i,N+1:2*N)
        end do
        print *, "l'inverse de la matrice est: "
        do j = 1, N
            print *,matrix_inverse(j,:)
        end do
    end if 




end program matrice