
(*****************************************************************************)
(*****************************************************************************)

INTERFACE ode;

(*<T>Library of numerical integrators to ordinary differential equations
  of arbitrary order, linear or nonlinear.  Both initial value problems and
  boundary value problems are catered for:

  \MNI{1.}{%
      IVPs:  at present only one routine is supplied;  this is based on a 
      Runge-Kutta Order Four scheme.  This is usually good enough.  I will
      be adding a Runge-Kutta Order Six scheme, simply because I have an
      implementation I did years ago knocking around, though I don't expect
      it to be significantly better very often.  More significant might be
      the addition of a predictor-corrector scheme or something specifically
      for ``stiff'' differential equations.
    }


  \MNI{2.}{%
      BVPs:  two routines are/will be supplied:  
      a ``relaxation/finite-difference''
      routine and a second based on a ``shooting'' algorithm.  The latter
      is not yet implemented --- though it solves, in principle at least, 
      the same problems as the former.  In practice, one routine may converge
      when the other does not;  furthermore, one can provide an 
      independent check of the other (assuming both converge!);  finally,
      rest assured that the shooting algorithm {\it will\/} be implemented
      in the not too distant future as I need it for my research!
    }

  *)

(*<T>\STitle{History}

     \twocoltable{4truecm}{1truecm}{9truecm}{%
         31 May 96, Simon Hood,\vskip0pt
         Oceanography Labs,\vskip0pt
         University of Liverpool, U.K.\vskip0pt
         \smallskip
         simonh@liv.ac.uk
       & Initial release.  Remains to implement the BVP shooting routine.\cr
       }
  *)

IMPORT
  SimonH_Log AS Log,
  Lin_Alg AS LA;

TYPE

  (*<T>Used to define given ode when calling RK4 : *)
  ivp_given_ode_proc_type 
              = PROCEDURE(         x : LONGREAL;
                          READONLY y : REF ARRAY OF LONGREAL) : LONGREAL
                    RAISES {};


  (*<T>Used to define given ode and boundary conditions when calling 
    BVP\_OrdN\_FD : *)
  F_proc_type = PROCEDURE (READONLY k : INTEGER;
                           READONLY x : REF ARRAY OF LONGREAL;
                           READONLY Y : REF ARRAY OF ARRAY OF LONGREAL;
                           VAR      F : REF ARRAY OF LONGREAL) 
                    RAISES {};


  (*<T>Used to define partial derivatives, wrt each dependent variable ($y$,
    $y'$ etc) at both $x_{k-1}$ and $x_k$, of given ode and boundary 
    conditions when calling BVP\_OrdN\_FD : *)
  S_proc_type = PROCEDURE (READONLY k : INTEGER;
                           READONLY x : REF ARRAY OF LONGREAL;
                           READONLY Y : REF ARRAY OF ARRAY OF LONGREAL;
                           VAR      S : REF ARRAY OF ARRAY OF LONGREAL) 
                    RAISES {};


  (*<T> *)
  LA_solve_type = PROCEDURE (READONLY n     : INTEGER;
                             READONLY A     : REF ARRAY OF ARRAY OF LONGREAL;
                             READONLY b     : REF ARRAY OF LONGREAL;
                             READONLY debug : BOOLEAN;
                             VAR      x     : REF ARRAY OF LONGREAL) 
                      RAISES{LA.No_Unique_Soln, 
                             Log.Cannot_Create, Log.Name_Is_Nil, 
                             Log.Cannot_Put, Log.Cannot_Close};

EXCEPTION
    Too_Many_Iterations;
    F_Sys_Error;
    Not_Yet_Implemented;


(*****************************************************************************)
(*****************************************************************************)


PROCEDURE RK4(         order      : INTEGER;
                       a,b        : LONGREAL;
                       J          : INTEGER;
                       F          : ivp_given_ode_proc_type;
              READONLY init_conds : REF ARRAY OF LONGREAL;     (* [0..order] *)
              VAR      x          : REF ARRAY OF LONGREAL;     (* [0..J] *)
              VAR      y          : REF ARRAY OF LONGREAL;     (* [0..J] *)
            ) RAISES {F_Sys_Error};

(*<T>
   \STitle{Purpose}

   Solves initial value problems for ODEs, 
   $$ y^{[N]} = f(x,y,y_x,...y^{[N-1]}) $$, of
   arbitrary order.

 
   \STitle{Argument}

   \twocoltable{2cm}{0.5cm}{11cm}{%
     x          & independent variable at ``J'' points $\in [a, b]$ including
                  end points, i.e., the grid on which the integration is
                  performed; \cr
     order      & order of ODE to be solved; \cr
     a,b        & $x \in [a,b], \quad a < b$; \cr
     J          & number of intervals to divide $[a,b]$ into; \cr
     init\_conds & array of initial conditions of form \cr
                & ic[m] = value m'th derivative at $x = a$; \cr
     F          & supplied function representing the ODE, e.g., write
                  $$\Delta(x,y,y_x,y_{xx},y_{xxx}) = 0$$ 
                  as 
                  $$y[0] = y$$ 
                  with,
                  $$\eqalign{
                      y[1] &= y_x;\cr
                      y[2] &= y_{xx};\cr
                      y[3] &= F(x,y[\,]);\cr
                    }$$\cr
     y         & dependent variable and its derivatives, as found by the
                 numerical integration at each point on the grid, 
                 $x \in [a, b]$, including end points; \cr
   }


   \STitle{Exceptions Raised}
   
   \twocoltable{3cm}{1cm}{10cm}{%
       F\_Sys\_Error:  &  should never be raised;  from the user's point of 
                          view this means a bug has been discovered in the 
                          implementation!
     }


   \STitle{Special Notes/Requirements}
   
   \ANI{The size of the arrays ``init\_conds'', ``x'' and ``y'' must be
        of the correct size!}


   \STitle{References}
   
   \MNI{1.}{
       Burden, Faires and Reynolds.
     }


   \STitle{To Do!}
   
   \MNI{1.}{
       Rather than have and array-bound error if ``init\_conds'', ``x'' or
       ``y'' are to small, an exception should be raised.
     }
  *)

(*****************************************************************************)


PROCEDURE Shoot() RAISES {Not_Yet_Implemented};


(*****************************************************************************)


PROCEDURE BVP_OrdN_FD(          max_iters, 
                                num_eqs, num_pts, num_bca, 
                                k_a, k_b      : INTEGER;
                                tolerance     : LONGREAL;
                                debug         : BOOLEAN;
                       READONLY x             : REF ARRAY OF LONGREAL;
                       READONLY y_scale       : REF ARRAY OF LONGREAL;
                                Compute_F     : F_proc_type;
                                Compute_S     : S_proc_type;
                                LA_Solve      : LA_solve_type;
                       VAR      y             : REF ARRAY OF ARRAY OF LONGREAL)
          RAISES {Too_Many_Iterations};

(*<T>Boundary Value Problem, Order N (arbitrary), Finite-Difference method : 

   \STitle{Purpose}
   
   To approximate the solution of the (nonlinear) boundary value problem
       $${{{\rm d}y}\over{{\rm d}x}}= y^{[N]} 
            = f(x,y,y',y'',...,y^{[N-1]}),
         $$
   with boundary conditions
       $$\eqalign{
           &\A(x_a, \y_a) =  F_{a,n} = 0, 
                \qquad n = 0,\ldots, N_a - 1 \cr
           &\B(x_b, \y_b) = F_{b,n} = 0, 
                \qquad n = 0, \ldots, N - N_a - 1, \cr
         }$$
   i.e., an $N$'th order ode, with $N$ boundary conditions, $N_a$ of which
   are at the first boundary and $N_b = N - N_a$ of which are at the second
   boundary.


   \STitle{Overview}
   
   This routine uses an algorithm based on a finite-difference/relaxation 
   approach.  The theory is of this approach is widely known;  in particular
   it is described by Press {\it et al.}$^{[1]}$.  (Note that this 
   implementation is not based on Press';  rather it was written from 
   scratch in order to take advantage of the features of a modern 
   language --- Modula-3 --- such as dynamic allocation of memory, 
   exceptions and procedure types/variables.)

   The emphasis of this release (version 1.0) is on {\it maintainability},
   i.e., ease of understanding, debugging and modification, rather than
   efficiency.  

   The essence of the finite-difference/relaxation technique is to reduce
   the solution of a boundary value problem to an exactly determined system 
   of simultaneous,
   linear, algebraic equations.  As one might expect, therefore, the
   ``rate determing step'' of the integration process is the solution of the
   system of algebraic equations.  With maintainability in mind, therefore,
   the implementation separates out this part of the integration process so
   that the user may try different routines.

   In contrast, Press {\it et al.\/} tightly integrate the solution of the
   algebraic system with the rest of the integration process making the
   algorithm rather complicated and difficult to understand and maintain;  
   that said their algorithm is very efficient --- advantage is taken on the 
   block-diagonal structure of the system.  (Of course users can supply
   such a routine.)

   Two routines for solving the linear system are supplied with this module;
   neither takes advantage of the block-diagonal nature of the system.  
   However, the numerical integration of an ode, even in this slightly 
   inefficient way takes only seconds on a standard U{\sc nix} box 
   or 586\thinspace PC.  
   

 
   \STitle{Arguments}

   \twocoltable{3truecm}{1truecm}{10truecm}{%
       max\_iters & maximum number of iterations tried before non-convergence
                    is assumed;\cr
       num\_eqs   & an ode of order $N$ is written as $N = num_eq$ first 
                    order odes;\cr
       num\_pts   & the number of points in the grid on which the solution
                    is found;\cr
       num\_bca   & number of boundary conditions imposed at $x_a$;\cr
       k\_a, k\_b & the grid is $x_k$, $k = k_a\dots k_b$;\cr
       tolerance  & sets the convergence target;\cr
       debug      & if true the routine outputs several diagnostic files
                    --- see below;\cr
       x          & the grid;\cr
       y\_scale   & y\_scale[i] should be the typical size of y[i] --- used
                    to check out the convergence;\cr
       Compute\_F & defines given ode and boundary conditions;\cr
       Compute\_S & defines partial derivatives, wrt each dependent variable 
                    ($y$, $y'$ etc) at both $x_{k-1}$ and $x_k$, of given ode 
                    and boundary conditions;\cr
       LA\_Solve  & the routine used to solve the linear system;\cr
       y          & $y$\dots$y^{[N]}$ at each of $x_k$;\cr
     }


   \STitle{References}
   
   \MNI{[1]}{Numerical Recipes:  The Art of Scientific Computing (F{\sc ortran}
             version), Press W.H., Flannery B.P., Teukolsky S.A.\ and
             Vetterling W.T., Cambridge University Press.
     }


   \centerline{\vbox{\hrule width2.5truein}}

  
   In the following the details of the theory of the method are outlined.
   It is not necessary to study these, but some understanding may well
   be useful --- e.g., if an integration fails to converge.


   \STitle{Notation}
   
   The following notation is used:
   \BUL{
       we have an $N$th order ode, represented as $N$ first order odes, and
       therefore a vector, $\y_k$, of these variables:
     }
   \BUL{
       $$\eqalign{
           \y_k = (y^{[0]}_k, y^{[1]}_k, \ldots, y^{[N-1]}_k)
         }$$
       where $y^{[n]}_k$ is the $n$'th derivative of $y$ at the $k$'th 
       grid-point.
     }
   \BUL{
       There are $K$ grid-points, $x_k$, $k = 1, \ldots, K$.
       (There are therefore $K-1$ intervals.)  
     }
   \BUL{
       We associate `a' and `A' with the first boundary, and `b' and `B' with 
       the second;  hence $x_1 = x_a$ and $x_K = x_b$.
     }


   \STitle{Defining the Problem}
   
   We represent an $N$th order ode as a system of $N$ first order odes.
   Therefore, the equations to be solved, together with their boundary 
   conditions are, in vector/matrix notation :
   $$\eqalignno{
       &{\d\y \over {\d x}} - \f(x,y,y^{[1]},\ldots,y^{[N-1]}) = E_k = 0, 
           \qquad k = 2\ldots K &(1)\cr
       &\A(x_1,\y_1) = \E_a = E_1 = 0, &(2)\cr
       &\B(x_k, \y_k) = \E_b = E_{K+1} = 0, &(3)\cr
     }$$  


   \STitle{The Finite-Difference Approximation Used}
   
   Choose a simple two-point finite-difference scheme;  then (1) becomes
   $$ 0 = \E_k = \y_k - \y_{k-1}  
               - (x_k - x_{k-1})\f_k(x_k,x_{k-1},\y_k,\y_{k-1}), \qquad
      k = 2, \ldots, K.
     \eqno{(4)}$$
   This approximation will be a good one provided $\Delta x$ is ``small''
   and the solution to our equation is ``well behaved''.

   The vector/matrix equation (4), together with finite-difference 
   approximations to (2) and (3) are an $NK \times NK$ system
   of linear equations, i.e, we have $NK$ equations for the $N$ unknowns,
   $y^{[n]}$ at the $K$ positions, $x_k$.


   \STitle{The Fundamental Idea}
   
   How does this help us find the solution?  Given an initial guess we can
   ``relax'' towards a better approximation by determining corrections to 
   $\y$ such that $\y + \Delta\y$ is a better approximation to the solution.


   \STitle{Relaxation}
   
   Expanding the \fdes, (4), in a Taylor series about $\y$, we have
   $$\halign to\displaywidth{%
         \tabskip=\centering%
         #&\tabskip=\centering#\tabskip=0pt\cr
         %
         $E_{nk}(\y_k + \Delta\y_k,\y_{k-1}+\Delta\y_{k-1}; 
                 x_k, x_{k-1})$\hfil&\cr 
         \noalign{\smallskip}
         $\qquad= E_{n,k}(x_k,\y_k; x_k, x_{k-1}) 
          + \displaystyle{\sum^N_{n=1}{      {\partial E_{n,k}} 
                                       \over {\partial y_{n,k-1}}}
                        \Delta y_{n,k-1}   
          + \sum^N_{n=1}{{\partial E_{n,k}} \over {\partial y_{n,k}}}
                        \Delta y_{n,k},}$\hfil&\hfil{\rm(5)}\cr
         \noalign{\smallskip}
         \hfil$n = 1,\ldots,N, \qquad k = 2,\ldots,K$&\cr
     }$$

   We want $\E(\y+\Delta\y)$ to be a better approximation than $\E(\y)$;
   assuming perfection then $\E(\y+\Delta\y) = 0$, so
   $$\eqalign{
        &\sum^N_{\hat n=1}{       {\partial E_{n,k}} 
                            \over {\partial y^{[\hat n]}(k-1)}}
                          \Delta y^{[\hat n]}(k-1)   
       + \sum^N_{\hat n=1}{       {\partial E_{n,k}} 
                            \over {\partial y^{[\hat n]}(k)}}
                          \Delta y^{[\hat n]}(k)\cr
       &\qquad
       = - E_{n,k}(x_k,\y_k; x_k, x_{k-1}),    
       \qquad n = 1,\ldots,N
       \qquad k = 2,\ldots,K.
     }\eqno{(6)}$$
   Expanding the boundary conditions in a similar way we obtain
   $$\eqalignno{
     \sum^N_{\hat n=1}{      {\partial E_{n,k=a=1}} 
                       \over {\partial y^{[\hat n]}(k=a)}}
                 \Delta y^{[\hat n]}(k_a) &= - E_{n,a}, 
         \qquad n = 1,\ldots,N_a, &(7)\cr
     \sum^N_{\hat n=1}{      {\partial E_{n,k=b=K+1}} 
                       \over {\partial y^{[\hat n]}(k=b)}}
                 \Delta y^{[\hat n]}(k_b) &= - E_{n,b},
         \qquad n = 1,\ldots,N_b. &(8)\cr
     }$$


   \STitle{Usage}
   
   Given an initial guess $\y_k$, $k=1,\ldots,K$, i.e., $Y_{n,k}$, we need
   to solve the system (6), (7) and (8), which we write as 
   $$ E_{i,j}\Delta y_j = e_j, \eqno{(9)} $$
   i.e.,

   $$
       \left(\matrix{{{\partial E_{1,a}}\over{\partial y^{[0]}_{k_a}}} & 
                     \ldots &
                     {{\partial E_{1,a}}\over{\partial y^{[N]}_{k_a}}} & \cr
                     \vdots & & \vdots \cr
                     {{\partial E_{N_a,a}}\over{\partial y^{[0]}_{k_a}}} & 
                     \ldots &
                     {{\partial E_{N_a,a}}\over{\partial y^{[N]}_{k_a}}} & \cr
                     {{\partial E_{1,2}}\over{\partial y^{[0]}_{k_a}}} & 
                     \ldots & \ldots & \ldots &
                     {{\partial E_{1,2}}\over{\partial y^{[N]}_{k=2}}} & \cr
                     \vdots & & & & \vdots \cr
                     {{\partial E_{N,2}}\over{\partial y^{[0]}_{k_a}}} & 
                     \ldots & \ldots & \ldots &
                     {{\partial E_{N,2}}\over{\partial y^{[N]}_{k=2}}} & \cr
                     & & & & & \ddots\cr
                     & & & & & & 
                     {{\partial E_{1,K}}\over{\partial y^{[0]}_{k=K}}} & 
                     \ldots & \ldots & \ldots &
                     {{\partial E_{1,K}}\over{\partial y^{[N]}_{k=b}}} & \cr 
                     & & & & & & 
                     \vdots & & & & \vdots \cr
                     & & & & & & 
                     {{\partial E_{N,K}}\over{\partial y^{[0]}_{k=K}}} & 
                     \ldots & \ldots & \ldots &
                     {{\partial E_{N,K}}\over{\partial y^{[N]}_{k=b}}} & \cr
                      & & & & & & & &
                              {{\partial E_{1_b,b}}\over{\partial y^{[0]}_{k_b}}} & 
                              \ldots &
                              {{\partial E_{1_b,b}}\over{\partial y^{[N]}_{k_b}}} & \cr 
                      & & & & & & & &
                      \vdots & & \vdots \cr
                      & & & & & & & & 
                              {{\partial E_{N_b,b}}\over{\partial y^{[0]}_{k_b}}} & 
                              \ldots &
                              {{\partial E_{N_b,b}}\over{\partial y^{[N]}_{k_b}}} & \cr 
          }\right)
       \left(\matrix{\Delta y^{[0]}_{a}\cr
                     \vdots\cr
                     \Delta y^{[N]}_{a}\cr
                     \Delta y^{[0]}_{2}\cr
                     \vdots\cr
                     \Delta y^{[N]}_{2}\cr
                     \vdots\cr
                     \vdots\cr
                     \Delta y^{[0]}_{K}\cr
                     \vdots\cr
                     \Delta y^{[N]}_{K}\cr
                     \Delta y^{[0]}_{b}\cr
                     \vdots\cr
                     \Delta y^{[N]}_{b}\cr
               }\right) 
     = \left(\matrix{-E_{1,a}\cr
                     \vdots\cr
                     -E_{N_a,a}\cr
                     -E_{1,2}\cr
                     \vdots\cr
                     -E_{N,2}\cr
                     \vdots\cr
                     \vdots\cr
                     -E_{1,K}\cr
                     \vdots\cr
                     -E_{N,K}\cr
                     -E_{1,b}\cr
                     \vdots\cr
                     -E_{N_b,b}\cr
               }\right)
     $$
   
   Given the solution, $\Delta\y$, to this system we compute the improved
   approximate solution to the system of odes $\y := \y + \Delta\y$, 
   and repeat the procedure until $\vert\Delta\y\vert$ becomes smaller than 
   some acceptable value.  

   
   \STitle{Algorithm to construct $\E$ and $\e$}

   The question remains, how does the algorithm know the values of the various
   partial derivatives and the \fdes\ themselves (at $k = 2,\ldots,K$) and also
   the boundary conditions (at $k = 1$ and $k = K+1$)?  These are supplied 
   through two user-implemented routines:

   It is convenient to introduce two matrices, $S$ and $F$;  these are 
   defined by
   $$\eqalign{
      &S_{n,\hat n}(k) = {      {\partial E_{n,k}} 
                          \over {\partial y^{[\hat n]}(k-1)}}, 
           \qquad
       S_{n,\hat n + N}(k) = {      {\partial E_{n,k}} 
                              \over {\partial y^{[\hat n]}(k)}}, 
           \qquad n = 1,\ldots,N, \qquad k = 2,\ldots,K\cr
           \noalign{\smallskip}
      &S_{n,\hat n} = {{\partial E_{n,1}} \over {\partial y_{\hat n,1}}}, 
           \qquad k = a = 1 \cr
           \noalign{\smallskip}
      &S_{n,\hat n} = {{\partial E_{n,K+1}} \over {\partial y_{\hat n,K}}}, 
           \qquad k = b = K+1 \cr
           \noalign{\smallskip}
      &F_n = A_n, \qquad k = a = 1, \qquad n = 0\ldots N_a - 1\cr
           \noalign{\smallskip}
      &F_n = B_n, \qquad k = b = K + 1, \qquad n = 0\ldots N - N_a - 1\cr
           \noalign{\smallskip}
      &F_n = E_n, \qquad n = 0\ldots N - 1.\cr
     }$$
   
   The tests which come with {\code BVP\char'137OrdN\char'137 FD} provide 
   examples.


   \STitle{What can go wrong}

   \BUL{The algorithm relies on initial guess.  Experience suggests that
        this need not be particularly close to the actual solution, but of
        course if convergence problems are found one should try to determine
        a better initial guess.
     }
   \BUL{Experience gained so far with this algorithm suggests that 
        oscillation of the maximum error (as displayed after each iteration)
        means the differential equation has not been coded correctly.}


   \STitle{Diagnostics --- {\code debug}}

   If ``debug'' is set equal to ``true'' then several debug files are
   written (<...>.dlog).  These should help to determine what went wrong.
   The files are:\bigskip
 
   \twocoltable{3truecm}{1truecm}{10truecm}{%
       ode-E.dlog     & contains $\E$ at each iteration;\cr
       ode-Y.dlog     & \dots $\Y$ \dots;\cr
       ode-corr.dlog  & \dots $\Delta\y$ \dots;\cr
       ode-e.dlog     & \dots $\e$ \dots;\cr
       ode-x.dlog     & \dots $\x$ \dots;\cr
       Lin\_Alg.dlog  & contains the linear system solved in the form of an
                        augmented matrix.
     }

  *)


(*****************************************************************************)
(*****************************************************************************)

END ode.

(*****************************************************************************)
(*****************************************************************************)


