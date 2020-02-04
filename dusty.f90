program dusty
    IMPLICIT NONE !might have to remove this statement; typecasting
        !variable declarations/initializations
    INTEGER, PARAMETER :: MAXDIM = 5 !parameter==constant, real value is 100
    INTEGER, DIMENSION(MAXDIM) :: IA
    INTEGER :: N
    INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(14)
    REAL(KIND = DP), DIMENSION(MAXDIM) :: AV, BV, CV
    REAL(KIND = DP), DIMENSION(MAXDIM, MAXDIM) :: OP, ID, AM, BM, CM, DM
    REAL(KIND = DP) :: check, BOT, TOP, HOLDA, HOLDB, TRACE3

    REAL(KIND = DP) :: seed, conrand, trig
    EXTERNAL :: trig, conrand
    
    N = MAXDIM

    !wall = walltime()
    !cpu = cputime()
    seed = 1.0D0

    !Loop 10 Series -- Filling Arrays
    ! do loops like for loops
    do 10 i = 1, N
        AV(i) = bessel_jn(0, dble((conrand(seed) *&
               (-1)**(mod(int(10*conrand(seed)), N)))))
10  continue

    do 11 i = 1, N
        BV(i) = bessel_jn(1, dble((conrand(seed) *&
                (-1)**(mod(int(10*conrand(seed)), N)))))
11  continue

    check = 0.0
    do 12 i = 1, N
        ival = N
        check = check + AV(i) * BV(i)
        call idcheck(ival,check,AV,BV,ID)
12  continue

!Computer |AV><BV|
    do 13 i = 1, N
        do 14 j=1, N
            call idcheck(N,check,AV,BV,ID)
            if(check .gt. 0.5) then !.gt. means greater than
                OP(i,j) = AV(i) * BV(j) / BV(i)
            else
                OP(i,j) = AV(j) * BV(i) / BV(j)
            endif
14      continue
        IA(I) = i
13  continue

    do 15 i = 1, N
        do 16 j = 0, i, 8
            IA(I) = mod(mod(i+j,N),N)+1 !mod=modulus
16      continue
15  continue

!Loop 20
    do 20 i = 1, N
        call idcheck(N,check,AV,BV,ID)
        CV(IA(I)) = (AV(IA(I)) + BV(IA(I))) / check
20  continue

!Loop 30
    do 30 i = 2, N
        call idcheck(N,check,AV,BV,ID)
        AV(i) = AV(i-1) * BV(i) + CV(i)
30  continue

!Loop 40
    do 40 i = 1, N
        call idcheck(N,check,AV,BV,ID)
        do 45 j = 1, N
            if(check .gt. 0.5) then
                BOT = OP(i,j)
                TOP = AV(j) * BV(j)
                HOLDA = AV(j)
                AV(j) = BV(j) + CV(j) / (TOP-BOT) * ID(i,i)
                BV(j) = HOLDA + CV(j) / (TOP-BOT) * ID(j,j)
                AM(i,j) = AV(j) * trig(IA(i), IA(j))
                BM(i,j) = BV(j) * trig(IA(j), IA(i))
            else
                BOT = OP(i,j)
                TOP = AV(j) * BV(j)
                HOLDA = AV(j)
                AV(j) = BV(j) - CV(j) / (TOP-BOT) * ID(j,j)
                BV(j) = HOLDA - CV(j) / (TOP-BOT) * ID(i,i)
                AM(i,j) = AV(j) / trig(IA(i), IA(j))
                BM(i,j) = BV(j) / trig(IA(j), IA(i))
            end if
45      continue
40  continue
        
end program dusty
