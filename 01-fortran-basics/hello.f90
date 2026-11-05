program test
    implicit none
    integer::fichier
    open(newunit=fichier, file="fichier.txt", status="new")
    write(fichier,*) "Bonjour les amis!"
    write(fichier,*) "Bienvenue a ce cours d'inititation"
    write(fichier,*) "au fortran!"
    close(fichier)
end program  test