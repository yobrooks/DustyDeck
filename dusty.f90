program dusty
        USE ExtraFunc
        IMPLICIT NONE
        !variable declarations/initializations
        !INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(17, 307)
        INTEGER, PARAMETER :: MAXDIM = 5 !parameter==constant, real value is 100
        INTEGER, DIMENSION(MAXDIM) :: IA
        INTEGER :: N, i, j, k, ival
        REAL(KIND = DP), DIMENSION(MAXDIM) :: AV, BV, CV
        REAL(KIND = DP), DIMENSION(MAXDIM, MAXDIM) :: OP, ID, AM, BM, CM, DM
        REAL(KIND = DP) :: check, BOT, TOP, HOLDA, HOLDB, TRACE3

        REAL(KIND = DP) :: seed
        !REAL(KIND = DP) :: seed, wall, cpu, walltime, cputime
        !EXTERNAL :: walltime, cputime
    
        N = MAXDIM

        !wall = walltime()
        !cpu = cputime()
        !seed = 1.0D0

    !Loop 10 Series -- Filling Arrays
    ! do loops like for loops
        write(*,*) "Fill AV"
        do 10 i = 1, N
                AV(i) = bessel_jn(0, dble((conrand(seed) *&
                        (-1)**(mod(int(10*conrand(seed)), N)))))
                write(*,*) "AV at ",i,": ",AV(i)
10      continue

        do 11 i = 1, N
                BV(i) = bessel_jn(1, dble((conrand(seed) *&
                        (-1)**(mod(int(10*conrand(seed)), N)))))
                write(*,*) "BV at ",i,": ",BV(i)
11      continue

        check = 0.0
        do 12 i = 1, N
                ival = N
                check = check + AV(i) * BV(i)
                call idcheck(ival,check,AV,BV,ID)
                write(*,*) "check at ",i,": ",check
12      continue

    write(*,*) "DONE!"
end program dusty





