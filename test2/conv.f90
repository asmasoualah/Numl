module m_lu
contains
  subroutine lu_solve_lapack(A,X,B,N)
    implicit none
    integer :: n
    real(kind=8),dimension(n,n) :: A
    real(kind=8),dimension(n) :: X,B
    
    real(kind=8),dimension(n,n) :: LU
    integer,    dimension(n)    :: ipiv
    integer                :: info
    real(kind=8):: t3,t2,t1
    
    X = B
    LU = A
    call cpu_time(t1)
    call dgetrf(N,N,LU,N,IPIV,INFO)
    call cpu_time(t2)    
    
    call dgetrs('N',N,1,LU,N,IPIV,X,N,INFO)
    call cpu_time(t3)
  end subroutine lu_solve_lapack
  
end module m_lu

program vf1d
  use m_lu
  implicit none

  real(kind=8),dimension(:),allocatable :: xf
  real(kind=8),dimension(:),allocatable :: xc
  real(kind=8),dimension(:),allocatable :: FI,B,MUf,Uf,rhof
  real(kind=8),dimension(:,:),allocatable :: A
  integer :: n
  real(kind=8) :: xmin,xmax,h,r
  real(kind=8) :: aw,ae,ap,su,sp,fi_a,fi_b
  real(kind=8) :: pi = acos(-1.d0)
  integer :: i

  n=50
  xmin=0
  xmax=1.
 
  allocate(xf(0:n))
  allocate(muf(0:n))
  allocate(rhof(0:n))
  allocate(uf(0:n))
  allocate(xc(1:n))
  allocate(b(1:n))
  allocate(fi(1:n))
  allocate(A(1:n,1:n))
  
  xf(0)=xmin
  xf(n)=xmax
  h=(xmax-xmin)/real(n)

  !> maillage des noeuds de face
  do i=1,n
     xf(i) = xf(i-1) + h 
  end do
  
  !> perturbation des mailles faces
  do i=1,n-1
     
     call random_number(r)
     r = (r-0.5)*2*0.5*0.0 ! -1<r<1
     xf(i) =  xf(i)*(1+r*h)
  end do
  
!!$  do i=0,n
!!$     print*,i,xf(i)
!!$  end do
  !> maillage des noeuds de pression
  do i=1,n
     xc(i) = (xf(i-1)+xf(i))*.5
  end do

  call set_diffusion_term(xf,muf,n)
  !> set the properties

  !> cas 1
  uf = 0.1
  rhof = 1
  muf = 0.1
  fi_a = 1
  fi_b = 0

  !> cas 1
  uf = 2.5
  rhof = 1
  muf = 0.1
  fi_a = 1
  fi_b = 0

  
  b=0

  
  A = 0
  do i=1,n

     if (i==1) then
        Aw = 0
        Ae = muf(i)/(xc(i+1)-xc(i)) - rhof(i)*uf(i)*0.5
        Sp = -(muf(i-1)/(xc(i)-xf(i-1)) + rhof(i-1)*uf(i-1))
        Su = -Sp*fi_a
     else if (i==N) then
        
        Aw = muf(i-1)/(xc(i)-xc(i-1)) + rhof(i-1)*uf(i-1)*0.5
        Ae = 0
        Sp =-(muf(i  )/(xf(i)-xc(i)) - rhof(i  )*uf(i  ))
        Su = -Sp*fi_b
     else
        Aw = muf(i-1)/(xc(i)-xc(i-1)) + rhof(i-1)*uf(i-1)*0.5
        Ae = muf(i  )/(xc(i+1)-xc(i)) - rhof(i  )*uf(i  )*0.5
        Sp = 0
        su = 0
     end if
     Ap = aw+ae-sp

     if (i==1) then
        A(i,[i,i+1]) = [Ap,-Ae]
     else if (i==n) then
        A(i,[i,i-1]) = [Ap,-Aw]
     else
        A(i,[i,i-1,i+1]) = [Ap,-Aw,-Ae]
     end if
     B(i) = B(i) + su
     
     print*,i,aw,ae,su,sp
  end do

  

  call lu_solve_lapack(A,FI,B,N)

  open(unit=20,file="out.dat")
  do i=1,n
     write(20,*)xc(i),FI(i),fi_a+(fi_b-fi_a)*(exp(rhof(0)*uf()))
  end do
  
contains

  subroutine set_source_term(xf,b,n)
    implicit none
    integer :: n
    real(kind=8),dimension(0:n) :: xf
    real(kind=8),dimension(1:n) :: b
    integer :: i

    do i=1,n
       ! primitive de cos entre xf()
       b(i) = cos(xf(i))-cos(xf(i-1))
       b(i) = -sin((xf(i)+xf(i-1))*0.5)*(xf(i)-xf(i-1))
    end do
    
    
  end subroutine set_source_term
  
  
  subroutine set_diffusion_term(xf,mu_face,n)
    implicit none
    integer :: n
    real(kind=8),dimension(0:n) :: xf
    real(kind=8),dimension(0:n) :: mu_face
    integer :: i

    do i=0,n
       ! primitive de cos entre xf()
       mu_face(i) = 1
    end do

    
  end subroutine set_diffusion_term



  
  


end program vf1d
