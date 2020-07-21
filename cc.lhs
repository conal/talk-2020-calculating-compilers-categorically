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
\title[]{Cheap \& cheerful compiler calculation}
\date[]{Haskell Love 2020}
% \date{\today{} (draft)}
% \institute[]{Target}

\setlength{\itemsep}{2ex}
\setlength{\parskip}{3ex}
\setlength{\blanklineskip}{1.5ex}
\setlength\mathindent{4ex}

\nc\wow\emph

%% \usepackage{scalerel}

\begin{document}

\frame{\titlepage}

\title{Compiler calculation}
\title{Cheap \& cheerful compiler calculation}
\date{Haskell Love 2020}

\framet{Goals}{
\begin{itemize}\itemsep6ex
\item
  Compiler from functional language to stack machine
\item
  Specification: simple essence of stack computation
\item
  Correct implementation as solution to standard algebra problems
%% \item Statically, polymorphically typed
%% \item  Total
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
\parskip4ex
For a function |f :: a -> b|,
\begin{itemize}
\itemsep4ex
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
\begin{itemize}\itemsep8ex
\item Specify by precise analogy.
\item Solve for correct implementation.
% \item Profit!
\item Apply to more interesting machines.
\end{itemize}
}

\framet{Package as new type}{
\begin{code}
newtype StackFun a b = SF (forall z. a :* z -> b :* z)

NOP

stackFun :: (a -> b) -> StackFun a b
stackFun f = SF (first f)
\end{code}
}

\framet{Analogy (homomorphism)}{
\parskip5ex

Specification: |stackFun| defines a \emph{precise (non-leaky) analogy}

%format L = "\mathop{\mathbb{L}}"
\begin{itemize}\itemsep5ex
\item for a useful algebraic abstraction/vocabulary |L|, i.e.,
\item |stackFun| is an |L|-homomorphism/analogy.
%% \item Solutions are correct compilers.
\end{itemize}
}

%% Domain-independent translation to \& from standard vocabulary.

\framet{Identify a useful vocabulary}{
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
\vspace{4ex}
\begin{code}
stackFun g . stackFun f = stackFun (g . f)
\end{code}

\vspace{-2ex}
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
\vspace{2ex}
\begin{code}
SF (first g) . SF (first f) == SF (first g . first f)
\end{code}

\vspace{1ex}

Generalize/strengthen:
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

%format subx = "\!_\times"
%format subx = "{}"

%format lassocP = lassoc subx
%format rassocP = rassoc subx
%format swapP = swap subx
%format AssociativePCat = AssociativeCat subx
%format BraidedPCat = BraidedCat subx

\framet{Easy operations}{
\vspace{2ex}
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

%format MonoidalPCat = MonoidalP
%format MonoidalPCat = Monoidal "\!_" :*
%format ProductCat = Cartesian
\framet{Easy operations}{
\begin{code}
class MonoidalPCat k => ProductCat k where
  exl  :: (a :* b) `k` a
  exr  :: (a :* b) `k` b
  dup  :: a `k` (a :* a)
\end{code} 
Homomorphisms are again in solved form.
\begin{code}
instance ProductCat StackFun where
  exl  = stackFun exl
  exr  = stackFun exr
  dup  = stackFun dup
\end{code}
}

\framet{Parallel composition}{
\vspace{6ex}
\begin{code}
class MonoidalPCat k where
  (***) :: (a `k` c) -> (b `k` d) -> ((a :* b) `k` (c :* d))
\end{code}
\vspace{-3ex}

Convenient specializations:
\begin{code}
first :: MonoidalPCat k => (a `k` c) -> ((a :* b) `k` (c :* b))
first f = f *** id

second :: MonoidalPCat k => (b `k` d) -> ((a :* b) `k` (a :* d))
second g = id *** g
\end{code}
Focus on |first|, since
\begin{code}
f *** g = first f . second g = second g . first f

second g = swapP . first g . swapP
\end{code}

%% (f *** g) . (p *** q) == (f . p) *** (g . q)
%% first f . second g == f *** g
%% second g . first f == f *** g
%% f *** g  = first f . second g
%%          = first f . swapP . first g . swapP


}

\framet{Homomorphism property for |first|}{
\vspace{3ex}
\begin{code}
first (stackFun f) == stackFun (first f)
\end{code}
\vspace{-4.5ex}

Simplifying,
\begin{code}
first (SF (first f)) == SF (first (first f))
\end{code}
\vspace{-5ex}

Types:

\vspace{-2ex}
\begin{code}
                f   :: a -> c
         first  f   :: a :* b -> c :* b
first (  first  f)  :: forall z. (a :* b) :* z -> (c :* b) :* z
\end{code}
\vspace{-5ex}

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
Now
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
  second g = swapP . first g . swapP
  f *** g = first f . second g
\end{code}
Note right-to-left argument evaluation.
For left-to-right, define |f *** g = second g . first f|.
}

\framet{Parallel composition}{
\vspace{2ex}
\begin{code}
    stackFun f *** stackFun g
==  {- definitions above -}
    SF (  lassocP . first f . rassocP . first swapP .
          lassocP . first g . rassocP . first swapP)
==  {- below -}
    stackFun (f *** g)
\end{code}
%% ==  {- |(***)| for |StackFun| -}
%%     first (stackFun f) . second (stackFun g)
%% ==  {- definition of |stackFun| -}
%%     SF (first f) *** SF (first g)

\vspace{-2ex}
Step by step\out{ (right-to-left)}:
%format --> = "\ \longmapsto\ "
%format -*> = "\ \longmapsto\!\!\!^\ast\ "

\vspace{-2ex}
\begin{code}
                  ((a,b)          ,z)
first swapP  -->  ((b,a)          ,z)
rassocP      -->  (b              ,(a,z))
first g      -*>  (g b            ,(a,z))    -- steps for |g|
lassocP      -->  ((g b,a)        ,z)
first swapP  -->  ((a, g b)       ,z)
rassocP      -->  (a              ,(g b,z))
first f      -*>  (f a            ,(g b,z))  -- steps for |f|
lassocP      -->  ((f a, g b)     ,z)        -- |== first (f *** g) ((a,b),z)|
\end{code}
}

\framet{Parallel composition}{
\begin{code}
    stackFun f *** stackFun g
==  SF (  lassocP . first f . rassocP . first swapP .
          lassocP . first g . rassocP . first swapP)
\end{code}

We've recursively flattened to \emph{purely sequential} compositions of:
\begin{itemize}\itemsep2ex
\item |first p| for a few primitive functions |p|, 
\item |rassocP| and |lassocP| in balanced pairs (``push'' and ``pop'').
\end{itemize}
}

%if False

%format MonoidalSCat = MonoidalS
%format CoproductCat = Cocartesian

\framet{Conditional composition (coproducts)}{

\begin{code}
class MonoidalSCat k where
  (+++) :: (a `k` c) -> (b `k` d) -> ((a :+ b) `k` (c :+ d))

left :: MonoidalSCat k => (a `k` c) -> ((a :+ b) `k` (c :+ b))
left f = f +++ id

right :: MonoidalSCat k => (b `k` d) -> ((a :+ b) `k` (a :+ d))
right g = id +++ g

class CoproductCat k where
  inl  :: a `k` (a :+ b)
  inr  :: b `k` (a :+ b)
  jam  :: (a :+ a) `k` a
\end{code}

Works out as well.
}

%endif

\framet{From stack functions to stack programs}{
\begin{itemize}\itemsep6ex
\item Code generation and optimization need inspection.
\item Introduce \emph{data} representation denoting stack functions.
\item Specify by homomorphism, and calculate implementation.
\end{itemize}
}

\framet{Primitive operations}{
\vspace{2ex}
\begin{code}
data Prim :: * -> * -> * NOP where  -- Notation
  Exl  :: Prim (a :* b) a
  Exr  :: Prim (a :* b) b
  Dup  :: Prim a (a :* a)
  ...
  Negate :: Num a => Prim a a
  Add, Sub, Mul :: Num a => Prim (a :* a) a
  ...

evalPrim :: Prim a b -> (a -> b)    -- Denotation
evalPrim Exl     = exl      -- |fst|
evalPrim Exr     = exr      -- |snd|
evalPrim Dup     = dup      -- |dup|
                 ...
evalPrim Negate  = negateC  -- |negate|
evalPrim Add     = addC     -- |uncurry (+)|
                 ...
\end{code}
}

%format Pure = Prim
\framet{Stack operations}{
\begin{code}
data StackOp :: * -> * -> * NOP where
  Pure  :: Prim a b -> StackOp (a :* z) (b :* z)
  Push  :: StackOp ((a :* b) :* z) (a :* (b :* z))
  Pop   :: StackOp (a :* (b :* z)) ((a :* b) :* z)

NOP

evalStackOp :: StackOp u v -> (u -> v)
evalStackOp (Pure f)  = first (evalPrim f)
evalStackOp Push      = rassocP
evalStackOp Pop       = lassocP
\end{code}
}

\framet{Stack programs}{
%format ++* = ++
%format :< = "\triangleleft"
%% %format :< = ::
%format :< = "\mathbin{:\hspace{-0.4ex}<}"
%% Linear chains of stack operations:
\begin{code}
infixr 5 :<
data StackOps :: * -> * -> * NOP where
  Nil   :: StackOps a a
  (:<)  :: StackOp a b -> StackOps b c -> StackOps a c
NOP
evalStackOps :: StackOps u v -> (u -> v)
evalStackOps Nil          = id
evalStackOps (op :< ops)  = evalStackOps ops . evalStackOp op

NOP
data StackProg a b = SP { unSP :: forall z. StackOps (a :* z) (b :* z) }
NOP
progFun :: StackProg a b -> StackFun a b
progFun (SP ops) = SF (evalStackOps ops)
\end{code}
Compiler specification: |progFun| is homomorphic.
}

\framet{Compiler solution}{
\vspace{3ex}
\begin{code}
instance Category StackProg where
  id = SP Nil
  SP g . SP f = SP (f ++* g)

instance MonoidalPCat StackFun where
  first (SP ops) = SP (Push :< ops ++* Pop :< Nil)
  second g = swapP . first g . swapP
  f *** g = first f . second g

primProg :: Prim a b -> StackProg a b
primProg p = SP (Pure p :< Nil)

instance ProductCat StackProg where
  exl  = primProg Exl
  exr  = primProg Exr
  dup  = primProg Dup

...
\end{code}
}

\framet{Example}{
Haskell:
\begin{code}
\ (x,y) -> 2 * x + 3 * y
\end{code}
Algebraic translation:
\begin{code}
addC .
(mulC . (const 2 *** exl) . dup *** mulC . (const 3 *** exr) . dup) .
dup
\end{code}
Stack program:
\begin{code}
[  Dup,Push,Dup,Push,Const 2,Pop,Swap,Push,Exl,Pop,
   Swap,Mul,Pop,Swap,Push,Dup,Push,Const 3,Pop,Swap,
   Push,Exr,Pop,Swap,Mul,Pop,Swap,Add ]
\end{code}
%% To do: better optimization
}

\framet{To do}{
\begin{itemize}\itemsep4ex
\item Coproducts and closure
\item Better optimization
\item More realistic machine model
\end{itemize}
}

\end{document}
