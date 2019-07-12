PROGRAM SAMPLER_UNI

REAL   :: vector(50)
Real :: LOWER,UPPER
INTEGER :: seed(100),list(6)
character(20):: filename

vector(1:50)=0
Seed(1)=123456
CALL random_seed(put=Seed(1:100))

m=1
list=(/5,10,20,30,50,51/)
k=5

Lower=0
Upper=20
do while (k<51)

j=1
    Do while (j<1001)
    i=1
        DO while (i<k+1)
        CALL UNIFORM_DRAW(VALU,LOWER,UPPER)
        vector(i)=VALU
        i=i+1
        END Do
    
    write(filename,20) "UniformSample",k,".out"
 
    20 format (A13,i2,a4)
    open(10,File=filename)
    write(10,15)SUM(Vector)/(i-1)
    15 format (100f12.6)
    j=j+1
    End Do
m=m+1
k=list(m)
End do

END PROGRAM
!*****************************************************************
SUBROUTINE UNIFORM_DRAW(VALU,LOWER,UPPER)
REAL::VALU,LOWER,UPPER,X
CALL RANDOM_NUMBER(X)
!"RANDOM_NUMBER is an intrinsic routine that will generate a random
!number in between 0 and 1, i.e., 0 =< x < 1
VALU = X*(UPPER-LOWER) + LOWER
!this step changes the boundaries from (0,1) to (lower, upper)
END SUBROUTINE
!*****************************************************************

