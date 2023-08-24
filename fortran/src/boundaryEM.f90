!// ForQuill v1.01 Beta www.fcode.cn
!----------------------------------------------------------------------------
! Program: 2D_Potential - A boundary element method (BEM) code in Fortran
!     for analyzing general 2D potential problems (governed by
!     Laplace equation) using constant elements.
!
! Developer: Dr. Yijun Liu at the University of Cincinnati, Cincinnati, OH.
!
! Version: V.1.20.
! Released: October 1, 2008.
!
! Copyright(c)2004--2008 By University of Cincinnati.
! This code is intended for educational use only. No part of
! the code can be used for any commercial applications/
! distributions without prior written permission of the
! original developer.
!
!----------------------------------------------------------------------------

  Implicit Real *8(A-H, O-Z)

  Character *80 prob_title

  Allocatable :: a(:, :), u(:), x(:, :), y(:, :), node(:, :), bc(:, :), dnorm(:, :), xfield(:, :), f(:), atu(:), itemp(:)

  Open (5, File='input.dat', Status='old')
  Open (6, File='output.dat', Status='unknown')
  Open (7, File='phi_boundary.plt', Status='unknown')
  Open (8, File='xy.plt', Status='unknown')
  Open (9, File='phi_domain.plt', Status='unknown')

  Call cpu_time(time0)

! Read in initial data
  Read (5, 1) prob_title
  Read (5, *) n, nfieldormat(a80)
  Write (6, 1) prob_title
  Write (*, 1) prob_title
  Write (6, *) ' Total number of elements =', n
  Write (*, *) ' Total number of elements =', n
  Write (6, *)
  Write (*, *)

! Allocate the arrays
  allocate(a(n,n), u(n), x(2,n), y(2,n), node(2,n), bc(2,n), dnorm(2,n),  xfield(2,nfield), f(nfield), atu(n), itemp(n))

! Input and prepare the BEM model
  Call prep_model(n, x, y, bc, dnorm, node, xfield, nfield)

! Compute the right-hand-side vector b
  Call bvector(u, x, y, bc, node, dnorm, n)

! Computer the coefficient matrix A
  Call coefficient(a, n, x, y, bc, node, dnorm)

! Solve the system of equations Ax = b
! Use LAPACK direct solver (double precision, available at www.netlib.org)
  Write (6, *) 'LAPACK direct solver is called ......'
  Write (*, *) 'LAPACK direct solver is called ......'
  Call dgesv(n, 1, a, n, itemp, u, n, info)
  Write (6, *) 'LAPACK solver info =', info
  Write (*, *) 'LAPACK solver info =', info

! Output the boundary solution
  Write (6, *)
  Write (6, *) ' Boundary Solution:'
  Do i = 1, n
    Write (6, *) i, u(i)
    Write (7, *) i, u(i)
  End Do

! Evaluate the potential field inside the domain and output the results
  Call domain_field(nfield, xfield, f, n, x, y, bc, node, dnorm, u)

! Estimate the total CPU time
  Call cpu_time(time)
  Write (*, *)
  Write (*, *) ' Total CPU time used =', time - time0, '(sec)'
  Write (6, *)
  Write (6, *) ' Total CPU time used =', time - time0, '(sec)'
  Stop
End Program

!----------------------------------------------------------------------------
! Definition of the Variables:
!
! n = total number of (middle) nodes (elements)
! x(2,n) = coordinates of the nodes
! y(2,n) = coordinates of the end points defining the elements
! node(2,n) = element connectivity
! bc(2,n) = bc(1,i) contains BC type, bc(2,i) BC value, for element i

! dnorm(2,n) = normal of the elements
! a(n,n) = matrix A
! u(n) = first stores b vector; then solution vector of Ax = b
! nfield = total number of the field points inside the domain
! xfield(2,nfield) = coordinates of the field points inside the domain
! f(nfield)= values of potential at field points inside the domain
! atu(n)= temp array for the solver
! itemp(n)= temp array for the solver
!
!----------------------------------------------------------------------------

Subroutine prep_model(n, x, y, bc, dnorm, node, xfield, nfield)
  Implicit Real *8(A-H, O-Z)
  Dimension x(2, *), y(2, *), bc(2, *), xfield(2, *), dnorm(2, *), node(2, *)

! Input the mesh data
  Read (5, *)
  Do i = 1, n
    Read (5, *) itemp, y(1, i), y(2, i)
  End Do

  Read (5, *)
  Do i = 1, n
    Read (5, *) itemp, node(1, i), node(2, i), bc(1, i), bc(2, i)
  End Do

! Input the field points inside the domain
  If (nfield>0) Then
    Read (5, *)
    Do i = 1, nfield
      Read (5, *) itemp, xfield(1, i), xfield(2, i)
    End Do
  End If

! Compute mid-nodes and normals of the elements
  Do i = 1, n
    x(1, i) = (y(1,node(1,i))+y(1,node(2,i)))*0.5D0
    x(2, i) = (y(2,node(1,i))+y(2,node(2,i)))*0.5D0
    h1 = y(2, node(2,i)) - y(2, node(1,i))
    h2 = -y(1, node(2,i)) + y(1, node(1,i))
    el = sqrt(h1**2+h2**2)
    dnorm(1, i) = h1/el
    dnorm(2, i) = h2/el
  End Do

! Output nodal coordinates for plotting/checking
  Do i = 1, n
    Write (8, *) x(1, i), x(2, i)
  End Do
  Return
End Subroutine prep_model

!----------------------------------------------------------------------------
Subroutine bvector(u, x, y, bc, node, dnorm, n)
  Implicit Real *8(A-H, O-Z)
  Dimension u(*), x(2, *), y(2, *), bc(2, *), node(2, *), dnorm(2, *)
  Data pi/3.141592653589793D0/
  pi2 = pi*2
  Do i = 1, n
    u(i) = 0.D0
  End Do

  Do j = 1, & ! Loop on field points (Column)                         
    n
    al = sqrt((y(1,node(2,j))-y(1,node(1,j)))**2+ (y(2,node(2,j))-y(2,node(1,j)))**2) ! Element length       
    Do i = 1, & ! Compute parameters used in the formulas for the two intergals
      n
! Loop on source points (Row)                            
      x11 = y(1, node(1,j)) - x(1, i)
      x21 = y(2, node(1,j)) - x(2, i)
      x12 = y(1, node(2,j)) - x(1, i)
      x22 = y(2, node(2,j)) - x(2, i)
      r1 = sqrt(x11**2+x21**2)
      r2 = sqrt(x12**2+x22**2)
      d = x11*dnorm(1, j) + x21*dnorm(2, j)

      t1 = -x11*dnorm(2, j) + x21*dnorm(1, j)
      t2 = -x12*dnorm(2, j) + x22*dnorm(1, j)
      ds = abs(d)
      theta1 = datan2(t1, ds)
      theta2 = datan2(t2, ds)
      dtheta = theta2 - theta1

      aa = (-dtheta*ds+al+t1*log(r1)-t2*log(r2))/pi2
      If (d<0.D0) dtheta = -dtheta
      bb = -dtheta/pi2
      If (i==j) bb = 0.5
      If (bc(1,j)==1.) u(i) = u(i) - bb*bc(2, j) ! Potential 
      If (bc(1,j)==2.) u(i) = u(i) + aa*bc(2, j) ! Derivative
    End Do
  End Do
  Return
End Subroutine bvector

!----------------------------------------------------------------------------
Subroutine coefficient(a, n, x, y, bc, node, dnorm)
  Implicit Real *8(A-H, O-Z)
  Dimension a(n, n), x(2, *), y(2, *), bc(2, *), node(2, *), dnorm(2, *)

  Data pi/3.141592653589793D0/
  pi2 = pi*2
  Do j = 1, n
    Do i = 1, n
      a(i, j) = 0.D0
    End Do
  End Do

  Do j = 1, & ! Loop on field points (Column)                    
    n
    al = sqrt((y(1,node(2,j))-y(1,node(1,j)))**2+  (y(2,node(2,j))-y(2,node(1,j)))**2) ! Element length            
    Do i = 1, & ! Loop on source points (Row)                            
      n
      x11 = y(1, node(1,j)) - x(1, i)
      x21 = y(2, node(1,j)) - x(2, i)
      x12 = y(1, node(2,j)) - x(1, i)
      x22 = y(2, node(2,j)) - x(2, i)

      r1 = sqrt(x11**2+x21**2)
      r2 = sqrt(x12**2+x22**2)
      d = x11*dnorm(1, j) + x21*dnorm(2, j)
      t1 = -x11*dnorm(2, j) + x21*dnorm(1, j)
      t2 = -x12*dnorm(2, j) + x22*dnorm(1, j)

      ds = abs(d)
      theta1 = datan2(t1, ds)
      theta2 = datan2(t2, ds)
      dtheta = theta2 - theta1

      aa = (-dtheta*ds+al+t1*log(r1)-t2*log(r2))/pi2
      If (d<0.D0) dtheta = -dtheta
      bb = -dtheta/pi2
      If (i/=j) Then
        If (bc(1,j)==1.) a(i, j) = a(i, j) - aa
        If (bc(1,j)==2.) a(i, j) = a(i, j) + bb
      End If

      If (i==j) Then
        If (bc(1,j)==1.) a(i, j) = a(i, j) - aa
        If (bc(1,j)==2.) a(i, j) = a(i, j) + 0.5D0
      End If

    End Do
  End Do
  Return
End Subroutine coefficient

!----------------------------------------------------------------------------
Subroutine domain_field(nfield, xfield, f, n, x, y, bc, node, dnorm, u)
  Implicit Real *8(A-H, O-Z)
  Dimension xfield(2, *), f(*), x(2, *), y(2, *), bc(2, *), node(2, *),  dnorm(2, *), u(*)

  Data pi/3.141592653589793D0/
  pi2 = pi*2.D0

  Do i = 1, nfield
    f(i) = 0.D0
  End Do

  Do j = 1, & 
    n
! Loop over all elements                           
    If (bc(1,j)==1) Then
      f0 = bc(2, j)
      df0 = u(j)
    Else If (bc(1,j)==2) Then
      f0 = u(j)
      df0 = bc(2, j)
    End If

    al = sqrt((y(1,node(2,j))-y(1,node(1,j)))**2+ (y(2,node(2,j))-y(2,node(1,j)))**2) ! Element length      
    Do i = 1, & ! Loop over all field points inside the domain
      nfield
      x11 = y(1, node(1,j)) - xfield(1, i)
      x21 = y(2, node(1,j)) - xfield(2, i)
      x12 = y(1, node(2,j)) - xfield(1, i)
      x22 = y(2, node(2,j)) - xfield(2, i)

      r1 = sqrt(x11**2+x21**2)
      r2 = sqrt(x12**2+x22**2)
      d = x11*dnorm(1, j) + x21*dnorm(2, j)
      t1 = -x11*dnorm(2, j) + x21*dnorm(1, j)
      t2 = -x12*dnorm(2, j) + x22*dnorm(1, j)

      ds = abs(d)
      theta1 = datan2(t1, ds)
      theta2 = datan2(t2, ds)
      dtheta = theta2 - theta1

      aa = (-dtheta*ds+al+t1*log(r1)-t2*log(r2))/pi2
      If (d<0.D0) dtheta = -dtheta
      bb = -dtheta/pi2

      f(i) = f(i) + aa*df0 - bb*f0
    End Do
  End Do

! Output results
  Do i = 1, nfield
    Write (9, 20) xfield(1, i), f(i)
  End Do
  Return

  20 Format (1X, 4E18.8)
End Subroutine domain_field
!----------------------------------------------------------------------------
