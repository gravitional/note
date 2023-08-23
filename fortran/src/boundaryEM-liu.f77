c----------------------------------------------------------------------------
c Program: 2D_Potential - A boundary element method (BEM) code in Fortran
c for analyzing general 2D potential problems (governed by
c Laplace equation) using constant elements.
c
c Developer: Dr. Yijun Liu at the University of Cincinnati, Cincinnati, OH.
c
c Version: V.1.20.
c Released: October 1, 2008.
c
c Copyright(c)2004--2008 By University of Cincinnati.
c This code is intended for educational use only. No part of
c the code can be used for any commercial applications/
c distributions without prior written permission of the
c original developer.
c
c----------------------------------------------------------------------------

      implicit real*8(a-h, o-z)
      character*80 Prob_Title
      allocatable :: a(:,:),u(:),x(:,:),y(:,:),node(:,:),bc(:,:),
      & dnorm(:,:),xfield(:,:),f(:),atu(:),itemp(:)

      open (5, file='input.dat', status='old')
      open (6, file='output.dat', status='unknown')
      open (7, file='phi_boundary.plt',status='unknown')
      open (8, file='xy.plt', status='unknown')
      open (9, file='phi_domain.plt', status='unknown')
      call CPU_Time(time0)

c Read in initial data
     read(5,1) Prob_Title
     read(5,*) n, nfield
1    format(A80)
     write(6,1) Prob_Title
     write(*,1) Prob_Title
     write(6,*) ' Total number of elements =', n
     write(*,*) ' Total number of elements =', n
     write(6,*)
     write(*,*)
c Allocate the arrays
     allocate (a(n,n),u(n),x(2,n),y(2,n),node(2,n),bc(2,n),dnorm(2,n),
     &  xfield(2,nfield),f(nfield),atu(n),itemp(n))

c Input and prepare the BEM model
      call prep_model(n,x,y,bc,dnorm,node,xfield,nfield)
c Compute the right-hand-side vector b
        call bvector(u,x,y,bc,node,dnorm,n)
c Computer the coefficient matrix A
        call coefficient(a,n,x,y,bc,node,dnorm)
c Solve the system of equations Ax = b
c Use LAPACK direct solver (double precision, available at www.netlib.org)
        write(6,*) 'LAPACK direct solver is called ......'
        write(*,*) '
        LAPACK direct solver is called ......'
        call dgesv(n,1,a,n,itemp,u,n,info)
        write(6,*) 'LAPACK solver info =', info
        write(*,*) 'LAPACK solver info =', info

c Output the boundary solution
        write(6,*)
        write(6,*) ' Boundary Solution:'
        do i=1,n
        write(6,*) i, u(i)
        write(7,*) i, u(i)
        enddo
c Evaluate the potential field inside the domain and output the results
        call domain_field(nfield,xfield,f,n,x,y,bc,node,dnorm,u)
c Estimate the total CPU time
        call CPU_Time(time)
        write(*,*)
        write(*,*) ' Total CPU time used =', time-time0, '(sec)'
        write(6,*)
        write(6,*) ' Total CPU time used =', time-time0, '(sec)'
        stop
        end

c----------------------------------------------------------------------------
c Definition of the Variables:
c
c n = total number of (middle) nodes (elements)
c x(2,n) = coordinates of the nodes
c y(2,n) = coordinates of the end points defining the elements
c node(2,n) = element connectivity
c bc(2,n) = bc(1,i) contains BC type, bc(2,i) BC value, for element i
c dnorm(2,n) = normal of the elements
c a(n,n) = matrix A
c u(n) = first stores b vector; then solution vector of Ax = b
c nfield = total number of the field points inside the domain
c xfield(2,nfield) = coordinates of the field points inside the domain
c f(nfield)= values of potential at field points inside the domain
c atu(n)= temp array for the solver
c itemp(n)= temp array for the solver
c
c----------------------------------------------------------------------------

      subroutine prep_model(n,x,y,bc,dnorm,node,xfield,nfield)
      implicit real*8(a--h,o--z)
      dimension x(2,*),y(2,*),bc(2,*),xfield(2,*),dnorm(2,*),node(2,*)
c Input the mesh data
      read(5,*)
      do i=1,n
      read(5,*) itemp, y(1,i), y(2,i)
      enddo
      read(5,*)
      do i=1,n
      read(5,*) itemp, node(1,i), node(2,i), bc(1,i), bc(2,i)
      enddo
c Input the field points inside the domain
      if (nfield .gt. 0) then
      read(5,*)
      do i=1,nfield
      read(5,*) itemp, xfield(1,i), xfield(2,i)
      enddo
      endif
c Compute mid-nodes and normals of the elements
      do i=1,n
      x(1,i) = (y(1,node(1,i)) + y(1,node(2,i)))*0.5d0
      x(2,i) = (y(2,node(1,i)) + y(2,node(2,i)))*0.5d0
      h1 =       y(2,node(2,i)) - y(2,node(1,i))
      h2 = -y(1,node(2,i)) + y(1,node(1,i))
      el = sqrt(h1**2 + h2**2)
      dnorm(1,i) = h1/el
      dnorm(2,i) = h2/el
      enddo
c Output nodal coordinates for plotting/checking
      do i = 1,n
      write(8,*) x(1,i), x(2,i)
      enddo
      return
      end

c----------------------------------------------------------------------------
      subroutine bvector(u,x,y,bc,node,dnorm,n)
      implicit real*8(a--h,o--z)
      dimension u(*),x(2,*),y(2,*),bc(2,*),node(2,*),dnorm(2,*)
      data pi/3.141592653589793D0/
      pi2 = pi*2
      do i=1,n
      u(i) = 0.d0
      enddo
      do j=1,n  ! Loop on field points (Column)
      al = sqrt((y(1,node(2,j)) - y(1,node(1,j)))**2 +
      (y(2,node(2,j))- y(2,node(1,j)))**2) ! Element length
      do i=1,n ! Loop on source points (Row)
c Compute parameters used in the formulas for the two intergals
      x11 = y(1,node(1,j)) - x(1,i)
      x21 = y(2,node(1,j)) - x(2,i)
      x12 = y(1,node(2,j)) - x(1,i)
      x22 = y(2,node(2,j)) - x(2,i)
      r1 = sqrt(x11**2 + x21**2)
      r2 = sqrt(x12**2 + x22**2)
      d=   x11*dnorm(1,j) + x21*dnorm(2,j)

      t1 = -x11*dnorm(2,j) + x21*dnorm(1,j)
      t2 = -x12*dnorm(2,j) + x22*dnorm(1,j)
      ds = abs(d)
      theta1 = datan2(t1,ds)
      theta2 = datan2(t2,ds)
      dtheta = theta2 - theta1
      aa = (-dtheta*ds + al + t1*log(r1) - t2*log(r2))/pi2
      if(d .lt. 0.d0) dtheta = -dtheta
      bb = -dtheta/pi2
      if(i .eq. j) bb = 0.5
      if(bc(1,j).eq.1.) u(i) = u(i) - bb*bc(2,j)
      ! Potential is given
      if(bc(1,j).eq.2.) u(i) = u(i) + aa*bc(2,j)
      ! Derivative is given
      enddo
      enddo
      return
      end

c----------------------------------------------------------------------------
      subroutine coefficient(a,n,x,y,bc,node,dnorm)
      implicit real*8(a--h,o--z)
      dimension a(n,n),x(2,*),y(2,*),bc(2,*),node(2,*),dnorm(2,*)
      data pi/3.141592653589793D0/
      pi2 = pi*2
      do j=1,n
      do i=1,n
      a(i,j) = 0.d0
      enddo
      enddo
      do j=1,n       ! Loop on field points (Column)
      al = sqrt((y(1,node(2,j)) - y(1,node(1,j)))**2 +
      (y(2,node(2,j)) - y(2,node(1,j)))**2) ! Element length

      do i=1,n
        ! Loop on source points (Row)
        x11 = y(1,node(1,j)) - x(1,i)
        x21 = y(2,node(1,j)) - x(2,i)
        x12 = y(1,node(2,j)) - x(1,i)
        x22 = y(2,node(2,j)) - x(2,i)
        r1 =
        sqrt(x11**2 + x21**2)
        r2 =
        sqrt(x12**2 + x22**2)
        d
        =
        x11*dnorm(1,j) + x21*dnorm(2,j)
        t1 = -x11*dnorm(2,j) + x21*dnorm(1,j)
        t2 = -x12*dnorm(2,j) + x22*dnorm(1,j)
        ds = abs(d)
        theta1 = datan2(t1,ds)
        theta2 = datan2(t2,ds)
        dtheta = theta2 - theta1
        aa = (-dtheta*ds + al + t1*log(r1) - t2*log(r2))/pi2
        if(d .lt. 0.d0) dtheta = -dtheta
        bb = -dtheta/pi2
        if(i.ne.j)
        then
        if(bc(1,j).eq.1.) a(i,j) = a(i,j) - aa
        if(bc(1,j).eq.2.) a(i,j) = a(i,j) + bb
        endif
        if(i.eq.j)
        then
        if(bc(1,j).eq.1.) a(i,j) = a(i,j) - aa
        if(bc(1,j).eq.2.) a(i,j) = a(i,j) + 0.5d0
        endif
        enddo
        enddo
        return
        end

c----------------------------------------------------------------------------
      subroutine domain_field(nfield,xfield,f,n,x,y,bc,node,dnorm,u)
      implicit real*8(a--h,o--z)
      dimension xfield(2,*), f(*), x(2,*),y(2,*),bc(2,*),node(2,*),
      &       dnorm(2,*),u(*)
      data
      pi/3.141592653589793D0/
      pi2 = pi*2.d0
      do i=1,nfield
      f(i) = 0.d0
      enddo
      do j=1,n       ! Loop over all elements
      if(bc(1,j).eq.1) then
      f0       = bc(2,j)
      df0 = u(j)
      else if(bc(1,j).eq.2) then
      f0       = u(j)
      df0 = bc(2,j)
      endif
      al       = sqrt((y(1,node(2,j)) - y(1,node(1,j)))**2 +
      (y(2,node(2,j)) - y(2,node(1,j)))**2)       ! Element length
      do i=1,nfield       ! Loop over all field points inside the domain
      x11 = y(1,node(1,j)) - xfield(1,i)
      x21 = y(2,node(1,j)) - xfield(2,i)
      x12 = y(1,node(2,j)) - xfield(1,i)
      x22 = y(2,node(2,j)) - xfield(2,i)
      r1 =       sqrt(x11**2 + x21**2)
      r2 =       sqrt(x12**2 + x22**2)
      d       =       x11*dnorm(1,j) + x21*dnorm(2,j)
      t1 = -x11*dnorm(2,j) + x21*dnorm(1,j)
      t2 = -x12*dnorm(2,j) + x22*dnorm(1,j)
      ds = abs(d)
      theta1 = datan2(t1,ds)
      theta2 = datan2(t2,ds)
      dtheta = theta2 - theta1
      aa = (-dtheta*ds + al + t1*log(r1) - t2*log(r2))/pi2
      if(d .lt. 0.d0) dtheta = -dtheta

          bb = -dtheta/pi2
      f(i) = f(i) + aa*df0 - bb*f0
      enddo
      enddo
c Output results
      do i=1,nfield
      write(9,20) xfield(1,i), f(i)
      enddo
      20
      format(1x, 4E18.8)
      return
      end
c----------------------------------------------------------------------------