function [x, fx, exitflag, output] = newuoas(fun, x0, options)
%
% NEWUOAs v0.3
%
% NEWUOAs seeks the solution of an unconstrained optimization problems
% without using derivatives. It is designed for solving relatively large
% problems. In my test, It generally outperformed NEWUOA for problems
% with more than 20 variables. Moreover, it succeeded in solving some problems
% with more than 10,000 variables, including ARWHEAD, CHROSEN, and SPARSQUR.  

% The algorithm NEWUOAs was described in the PhD thesis 
%
% Z. Zhang, On Derivative-free Optimization Methods (in Chinese), PhD thesis, Institute of Computational Mathematics and Scientific/Engineering Computing, Chinese Academy of Sciences, Beijing, China, April 2012
%
% An English paper is being written. 
%
% This version is not intended to be released. It is only for test. 
%
% All rights reserved. 
%
% ZHANG Zaikun, 08/08/2016
% Department of Applied Mathematics, The Hong Kong Polytechnic University
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if (nargin == 1)
    x0 = 0;
end
x0 = x0(:); % Work with column vectors. 
n = length(x0);

% Default options:
rhobeg = 1;
rhoend = 1e-6;
maxfun = 100*n;
maxiter = 50;
ftarget = -Inf;
maxsubspacedim = min(3, n); 
modeltype = 'quadratic';
debug = true;
chkfunval = false;
reproduce = false;
warndim = true;
subspace = true;


if (~isnumeric(ftarget) || ftarget ~= ftarget)
    error('NEWUOAs:InvalidFtarget', 'ftarget should be a number.')
end

if (debug == true && chkfunval == true)
    warning('NEWUOAs:Chkfunval', 'NEWUOAs is in debug mode and checks whether fx=fun(x) at exit, \nwhich will cost an extra function evaluation. Set options.debug=false \nor options.chkfunval=false to disable the check.');
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Real NEWUOAs begins.
output.algorithm = 'NEWUOAs';
exitflag = NaN;
x = x0;
fx = feval(fun, x);
nf = 1;
fhist = fx;

if (abs(fx) == Inf || fx ~= fx) 
    exitflag = -1;
    warning('NEWUOAs:InvalidFunctionValue', 'The objective function returns an NaN or inifnite value at the starting point. NEWUOAs has to terminate.');
    output.message = 'The objective function returns an NaN or infnite value at the starting point.';
    output.iterations = 0;
    output.funcCount = nf;
    output.fhist = fhist;
    return;
end
if (fx <= ftarget)
    exitflag = 1;
    output.message = 'NEWUOAs terminates because the target function value is achieved.'; 
    output.iterations = 0;
    maxiter = 0;
end

h = rhobeg;
if (exist('newuoa_options', 'var') == 1)
    clear newuoa_options;
end
newuoa_options.rhobeg = rhobeg;
normd = rhobeg;
D = [];
smalld = 0;
halt = false;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for iter = 1:maxiter % Begin main loop of NEWUOAs. 
    while(1)
        dim = min(size(D,2)+2, maxsubspacedim);
        if (dim >= 5)
            npt = 2*dim + 1;
        else
            npt = (dim+1)*(dim+2)/2;
        end
        if (nf >= maxfun - npt - 6 || nf >= maxfun - 2*n - npt -4 && n >= 5000) % Has to be improved. 
            exitflag = 3; 
            output.message = 'NEWUOAs terminates because the maximal number of function evaluation is (nearly) reached.';
            halt = true;
            break;
        end
        submaxfun = min(2*n, maxfun - nf - (npt+5));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if (iter <= 1 && n >= 16) 
            [B, xnew, fnew, g, subnf, subfhist] = def_subspace(fun, x, fx, min(h^2, h), rhoend, n+1, 'lq', D, reproduce); % Note that D=[] when iter=1.
        else
            [B, xnew, fnew, g, subnf, subfhist] = def_subspace(fun, x, fx, h, rhoend, submaxfun, 'quadratic', D, reproduce);
        end

        if (isnan(sum(sum(B,2)+g+xnew)+fnew))
            exitflag = -2; 
            output.message = 'def_subspace returns NaN values. This implies a bug in the code.';
            error('NEWUOAs:def_subspace:NaN', 'def_subspace returns NaN values.');
        end

        dim = size(B, 2);
        if ((dim > 0 && nnz(B) == 0) || (dim == 0 && norm(g) > 1e-3*h) || dim > maxsubspacedim)
            exitflag = -3; 
            output.message = 'def_subspace returns a wrong dimension of the subspace. This implies a bug in the code.';
            error('NEWUOAs:def_subspace:WrongDim', 'def_subspace returns a wrong dimension of the subspace.');
        end

        if (dim == 0 && debug == true)
            warning('NEWUOAs:def_subspace:EmptyBasis', 'def_subspace returns an empty basis.');
        end
        
        x = xnew;
        fx = fnew;
        nf = nf + subnf;
        fhist = [fhist, subfhist];

        if (fx <= ftarget)
            exitflag = 1;
            output.message = 'NEWUOAs terminates because the target function value is achieved.'; 
            halt = true;
            break;
        end
        if (dim > 0 && norm(g)*sqrt(double(2*n)/double(min(2*n, submaxfun))) > rhoend)
            break;
        elseif (h <= rhoend)
            exitflag = 0; 
            output.message = 'NEWUOAs terminates because the first-order optimality is approximately achieved.';
            output.approx_1stopt = norm(g)*sqrt(double(2*n)/double(min(2*n, submaxfun)))+h;
            output.approx_1stopt = round(output.approx_1stopt, 1, 'significant');
            halt = true;
            break;
        else
            h = h/2;
        end
    end

    if (halt == true) 
        break;
    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Call NEWUOA in the subspace. 
    if (dim <= 5)
        newuoa_options.npt = (dim+1)*(dim+2)/2;
    else
        newuoa_options.npt = 2*dim+1;
    end
    newuoa_options.maxfun = min(500*dim, maxfun - nf);
    newuoa_options.rhoend = max([min(rhoend, 1.0/double(2^iter)), rhoend/max(n, 50), eps]);
    newuoa_options.rhobeg = max([newuoa_options.rhoend, h, normd, 0.5*newuoa_options.rhobeg]);
    [dopt, f, newuoa_exitflag, newuoa_output] = newuoa(@(d)fun(x+B*d), zeros(dim, 1), newuoa_options); 

    normd = norm(dopt);
    if (normd ~= normd || f ~= f || f > fx || (normd > 0 && f == fx))
        exitflag = -4; 
        output.message = 'NEWUOA outputs some unexpected value. This implies a bug in NEWUOA.';
        error('NEWUOAs:NEWUOAFailed', 'NEWUOA failed to solve the subproblem.\nNEWUOA outputs a step with norm %.4E and a function value %.4E', normd, f);
    end

    if (normd > 0)
        dx = B*dopt;
        x = x + dx;
        fx = f;
    end
    nf = nf + newuoa_output.funcCount; 
    fhist = [fhist, newuoa_output.fhist];

    if (fx <= ftarget)
        exitflag = 1;
        output.message = 'NEWUOAs terminates because the target function value is achieved.'; 
        break;
    end

    if (normd <= 0.1*rhoend)
        smalld = smalld + 1;
    else
        smalld = 0;
    end
    if (smalld >= 3)
        exitflag = 2; 
        output.message = 'NEWUOAs terminates because the stepsize is small for 3 consecutive iterations.';
        break;
    end

    h = max([h/2, rhoend/max(n, 50), sqrt(eps)]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Up date D.
    if (normd <= 0.1*rhoend)
        dx = []; 
    end

    if (maxsubspacedim == 3)
        D = dx;
    else
        D = [dx, -g, D(:, 1:min(size(D,2), maxsubspacedim-4))];
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end % End main loop of NEWUOAs.

if (debug == true)
    if (nf ~= size(fhist, 2) || (fx ~= min([fhist,fx]) && (fx == fx || min([fhist,fx]) == min([fhist,fx]))))
        error('NEWUOAs:InvalidFhist', 'NEWUOAs returns an fhist that does not match nf or fx.');
    end
    if (chkfunval == true)
        funx = feval(fun, x);
        if (funx ~= fx && (fx == fx || funx == funx))
            error('NEWUOAs:InvalidFx', 'NEWUOAs returns x and fx that do not not match.');
        end
    end
end

if (exitflag ~= exitflag)
    exitflag = 4; 
    output.message = 'NEWUOAs terminates because the maximal numer of iterations is reached.'; 
end

output.funcCount = nf;
if (~isfield(output, 'iterations'))
    output.iterations = iter;
end
output.fhist = fhist;

return;


function [B, xnew, fnew, g, nf, fhist] = def_subspace(f, x, fx, h, rhoend, maxfun, modeltype, D, reproduce)
% DEF_SUBSPACE v0.1
%
% subspace is a function to generate a subspace to be used in the
% minimization of the objective function f. 
%
% Inputs:
% f: objective function handle
% x: the base point 
% fx:
% h: 
% maxfun:
% modeltype:
% D:


% Only the cases with (maxfun == 2*n && modeltype = 'quadratic') or
% (maxfun == n && modeltype = 'linear') are implemented (with some
% simple randomization, parfois). In these cases, interpolation coincide with
% finite difference, yet with an adaptively chosen stepsize, which
% stabilizes the process and makes it much more robust than fminunc of
% MATLAB (finite difference+quasi-Newton when no gradient is available).
% Finite difference is not a sin as long as it is done properly. 
% Interpolation is no more than generalized and advanced finite difference.
%
% TO DO: non-finite-difference interpolation models and randomization.  

% NOTE: The function evaluations in this subroutine are completely parallelable. 

if (reproduce == true) % reproduce = true means to reproduce the result of the last run. This should be used only for testing algorithms. 
    load('seed.newuoas.mat', 'seed'); 
else
    rng('shuffle'); 
    seed = int32(abs(1e8*(2*rand(1,1)-1))); save('seed.newuoas.mat', 'seed'); 
end
rng(seed);

n = length(x);

        fp = NaN(n, 1);
        fn = NaN(n, 1);
        for i = 1:n
            xtmp = x;
            xtmp(i) = x(i) + h;
            fp(i) = feval(f, xtmp);
            if (abs(fp(i)) == Inf || fp(i) ~= fp(i)) 
                warning('NEWUOAs:def_subspace:InvalidFunctionValue', 'The objective function returns an NaN or infinite value at a point with norm %.4E', norm(xtmp));
            end
            xtmp(i) = x(i) - h;
            fn(i) = feval(f, xtmp);
            if (abs(fn(i)) == Inf || fn(i) ~= fn(i)) 
                warning('NEWUOAs:def_subspace:InvalidFunctionValue', 'The objective function returns an NaN or infinite or value at a point with norm %.4E', norm(xtmp));
            end
        end
        nf = 2*n;
        g = (fp-fn)/(2*h);
        H = (fp+fn-2*fx)/(h^2);
        g(g~=g) = 0;
        if(nnz(abs(g) == Inf) > 0)
            g(g == Inf) = 1;
            g(g == -Inf) = -1;
            g(abs(g) < Inf) = 0;
        end
        H(H~=H) = 0;
        if(nnz(abs(H) == Inf) > 0)
            H(H == Inf) = 1;
            H(H == -Inf) = -1;
            H(abs(H) < Inf) = 0;
        end

        [fnew, ind] = min([fp', fn']); % find least coordinate, ind is its index
        if (fnew < fx)
            if (ind <= n)
                xnew = x;
                xnew(ind) = x(ind) + h;
                g(ind) = g(ind) + h*H(ind);
                if (nnz(D) > 0) 
                    D(ind, :) = D(ind, :) + h;
                end
            else
                xnew = x;
                xnew(ind-n) = x(ind-n) - h;
                g(ind-n) = g(ind-n) - h*H(ind-n);
                if (nnz(D) > 0) 
                    D(ind-n, :) = D(ind-n, :) - h;
                end
            end
        else
            fnew = fx;
            xnew = x;
        end
        pg = NaN(size(g));
        threshold = max(eps, 1e-6*min(1, max(abs(H))));
        ind = find(H >= threshold);
        pg(ind) = g(ind)./H(ind);
        ind = find(H < threshold);
        pg(ind) = g(ind).*(-H(ind)/(threshold^2)+2/threshold);
    fhist = [fp', fn'];
    X = [D, -g -pg]; % The order of the directions matter. It seems working better to put D before -g.
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dimtmp = size(X, 2);
dim = 0;
for i = 1:dimtmp
    d = X(:, i);
    if (norm(d) > 1e-3*h) 
        dim = dim + 1;
        d = d/norm(d, Inf);
        X(:, dim) = d;
    end 
end
X = X(:, 1:dim);
dimtmp = dim;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dim = 0;
B = NaN(size(X));
for i = 1:dimtmp
    d = X(:, i);
    normd = norm(d);
    if (normd > 1e-8)
        dim = dim + 1;
        d = d/normd;
        B(:, dim) = d;
        X(:, i+1:dimtmp) = X(:, i+1:dimtmp) - d*(d'*X(:, i+1:dimtmp)); 
        %!!! DO NOT use d*d'*X(:, i+1:dimtmp) to replace the last term;
        %!!! Otherwise, we would have a n*n >> n*timtmp dimensional matrix to deal with! 
    end
end
B = B(:, 1:dim);

return;
