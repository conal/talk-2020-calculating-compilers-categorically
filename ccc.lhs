%% -*- latex -*-

% Presentation
%\documentclass[aspectratio=1610]{beamer} % Macbook Pro screen 16:10
%% \documentclass{beamer} % default aspect ratio 4:3
\documentclass[handout]{beamer}

% \setbeameroption{show notes} % un-comment to see the notes

\input{macros}

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include formatting.fmt

% \title{Calculating compilers categorically}
\title{Cheap \& cheerful compiler calculation}
\date{August 2020}
% \date{\today{} (draft)}
% \institute[]{Target}

\setlength{\itemsep}{2ex}
\setlength{\parskip}{1ex}
\setlength{\blanklineskip}{1.5ex}
\setlength\mathindent{4ex}

\nc\wow\emph

%% \usepackage{scalerel}

\begin{document}

\frame{\titlepage}

\framet{Goals}{
\begin{itemize}\itemsep2ex \parskip2ex
\item
  Compiler from functional language to stack machine
\item
  Specification: simple essence of stack computation
\item
  Correct implementation as solution to standard algebra problems
\end{itemize}
}

\out{
\framet{Example}{
\begin{code}
\ (x,y) -> 2 * x + 3 * y
\end{code}
}
\note{Give translation.}
}

\framet{The essence of stack computation}{
For a function |f :: a -> b|,
\begin{itemize}
\item
  Start with a stack containing |a| on top.
\item
  Replace |x :: a| by |f x :: b|, leaving the rest.
\end{itemize}

\vspace{5ex}
Formally,
\begin{code}
first :: (a -> b) -> forall z. (a :* z -> b :* z)
first f (a,z) = (f a, z)
\end{code}
}

\framet{Strategy}{
\begin{enumerate}\itemsep2ex \parskip2ex
\item Specify by precise analogy.
\item Solve for correct implementation.
\item Profit!
\end{enumerate}
}

\framet{Package as new type}{
\begin{code}
newtype StackFun a b = SF (forall z. a :* z -> b :* z)

NOP

stackFun :: (a -> b) -> StackFun a b
stackFun f = SF (first f)
\end{code}
}

\framet{Analogy (homomorphism)}{\parskip2ex
Specification: |stackFun| defines a \emph{precise analogy}.

\begin{itemize}\itemsep3ex
\item Identify a standard vocabulary (algebraic abstraction).
\item \emph{Analogous} meaning, related by |stackFun|.
\item Formalize algebraically: homomorphism.
%% \item Solutions are correct compilers.
\end{itemize}
}

%% Domain-independent translation to \& from standard vocabulary.

\framet{Identify a standard vocabulary}{
For functions and other function-like things:
\begin{code}
class Category k where
  id   :: a `k` a
  (.)  :: (b `k` c) -> (a `k` b) -> (a `k` c)
\end{code}

Analogy/homomorphism properties (``functor''):
\begin{code}
id = stackFun id
stackFun g . stackFun f = stackFun (g . f)
\end{code}

Solve each equation for its one unknown (LHS operation).
}

\framet{Sequential composition}{
\begin{code}
id = stackFun id
\end{code}

Trivial (already solved), but we can simplify/optimize:
\begin{code}
   stackFun id
=  {- definition of |stackFun| -}
   SF (first id)
=  {- property of |first| and |id| -}
   SF id
\end{code}
}


\framet{Sequential composition}{
\begin{code}
stackFun g . stackFun f = stackFun (g . f)
\end{code}

Simplify LHS:
\begin{code}
   stackFun g . stackFun f
=  {- definition of |stackFun| -}
   SF (first g) . SF (first f)
\end{code}
Then RHS:
\begin{code}
   stackFun (g . f)
=  {- definition of |stackFun| -}
   SF (first (g . f))
=  {- property of |first| and |(.)| -}
   SF (first g . first f)
\end{code}
The simplified specification:
\begin{code}
SF (first g) . SF (first f) == SF (first g . first f)
\end{code}
}

\framet{Sequential composition}{
\begin{code}
SF (first g) . SF (first f) == SF (first g . first f)
\end{code}

\vspace{1ex}

Strengthen by generalizing from |first g| and |first f|:
\begin{code}
SF g . SF f == SF (g . f)  -- Now in solved form.
\end{code}

\vspace{4ex}

Sufficient definitions:
\begin{code}
instance Category StackFun where
  id = SF id
  SF g . SF f = SF (g . f)
\end{code}
}

%format lassocP = lassoc
%format rassocP = rassoc
%format swapP = swap
%format AssociativePCat = AssociativeCat
%format BraidedPCat = BraidedCat

\framet{Easy interfaces}{
\begin{code}
class AssociativePCat k where
  rassocP :: ((a :* b) :* c) `k` (a :* (b :* c))
  lassocP :: (a :* (b :* c)) `k` ((a :* b) :* c)

class BraidedPCat k where
  swapP :: (a :*  b) `k` (b :* a)
\end{code}

Homomorphisms already in solved form.
Satisfy by definition:
\begin{code}
instance AssociativePCat StackFun where
  rassocP = stackFun rassocP
  lassocP = stackFun lassocP

instance BraidedPCat StackFun where
  swapP = stackFun swapP
\end{code}
}

\framet{Parallel composition}{
\begin{code}
class MonoidalPCat k where
  (***) :: (a `k` c) -> (b `k` d) -> ((a :* b) `k` (c :* d))
\end{code}

Convenient specializations:
\begin{code}
first :: MonoidalPCat k => (a `k` c) -> ((a :* b) `k` (c :* b))
first f = f *** id

second :: MonoidalPCat k => (b `k` d) -> ((a :* b) `k` (a :* d))
second g = id *** g
\end{code}

Focus on |first|, since
\begin{code}
f *** g  = first f . second g

second g = swap . first g . swap
\end{code}

%% (f *** g) . (p *** q) == (f . p) *** (g . q)
%% first f . second g == f *** g
%% second g . first f == f *** g
%% f *** g  = first f . second g
%%          = first f . swap . first g . swap


}

\framet{Homomorphism property for |first|}{
\begin{code}
first (stackFun f) == stackFun (first f)
\end{code}
Simplifying,
\begin{code}
first (SF (first f)) == SF (first (first f))
\end{code}
Types:
\begin{code}
                f   :: a -> c
         first  f   :: a :* b -> c :* b
first (  first  f)  :: forall z. (a :* b) :* z -> (c :* b) :* z
\end{code}
For stack computation, temporarily move |b| aside:
\begin{code}
   first (first f)
=  {- definition of |first| on |(->)| -}
   \ ((a,b),z) -> ((f a,b),z)
=  {- definition of |lassocP|, |rassocP|, and |first| on |(->)| -}
   lassocP . first f . rassocP
\end{code}
}

\framet{Homomorphism property for |first|}{
Homomorphism equation now:
\begin{code}
first (SF (first f)) == SF (lassocP . first f . rassoc)
\end{code}
Strengthen/generalize:
\begin{code}
first (SF f) == SF (lassocP . f . rassoc)  -- Now in solved form.
\end{code}

Sufficient definition:
\begin{code}
instance MonoidalPCat StackFun where
  first (SF f) = SF (lassocP . f . rassocP)
  second g = swap . first g . swap
  f *** g = first f . second g
\end{code}
Note right-to-left argument evaluation.
For left-to-right, |f *** g = second g . first f|.
}

\framet{Parallel composition}{
\begin{code}
    stackFun f *** stackFun g
==  {- |(***)| for |StackFun| -}
    first (stackFun f) . second (stackFun g)
==  {- definition of |stackFun| -}
    SF (first f) *** SF (first g)
==  {- ... -}
    SF (  lassocP . first f . rassocP . first swap .
          lassocP . first g . rassocP . first swap)
\end{code}
Step-by-step:
%format --> = "\ \longmapsto\ "
%format -*> = "\ \longmapsto\!\!\!^\ast\ "
\begin{code}
                 ((a,b)          ,z)
first swap  -->  ((b,a)          ,z)
rassocP     -->  (b              ,(a,z))
first g     -*>  (g b            ,(a,z))    -- steps for |g|
lassocP     -->  ((g b,a)        ,z)
first swap  -->  ((a, g b)       ,z)
rassocP     -->  (a              ,(g b,z))
first f     -*>  (f a            ,(g b,z))  -- steps for |f|
lassocP     -->  ((f a, g b)     ,z)
\end{code}

\out{
Operationally, |first g| and |first f| stand for stack-transformation sub-sequences.
Note that this final stack state is equal to |first (f *** g) ((a,b),z)| as needed.
We have, however, flattened (under the |SF| constructor) into \emph{purely sequential} compositions of functions of three forms:
\begin{itemize}\itemsep0ex
\item |first p| for simple functions |p|, 
\item |rassocP|, and
\item |lassocP|.
\end{itemize}
Moreover, the latter two always come in balanced pairs.
}

}

\end{document}
