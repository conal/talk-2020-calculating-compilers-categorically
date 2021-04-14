%% -*- latex -*-

% Presentation
%\documentclass[aspectratio=1610]{beamer} % Macbook Pro screen 16:10
%% \documentclass{beamer} % default aspect ratio 4:3
\documentclass[aspectratio=169]{beamer}  % handout,,10pt

% \setbeameroption{show notes} % un-comment to see the notes

\usepackage[many]{tcolorbox}

\input{macros}

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include formatting.fmt

% \nc\tit{Calculating compilers categorically}
% \nc\tit{Cheap \& cheerful compiler calculation}
\nc\tit{Compiling gracefully}

% \title[]\tit
\title[]\tit
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
\title\tit
\date{Haskell Love 2020}

\framet{Example}{
Source code:
\begin{code}
\ (x,y) -> 2 * x + 3 * y
\end{code}
\vspace{2ex}

Object code:
\begin{code}
[  Dup,Push,Dup,Push,Const 2,Pop,Swap,Push,Exl,Pop
,  Swap,Mul,Pop,Swap,Push,Dup,Push,Const 3,Pop,Swap
,  Push,Exr,Pop,Swap,Mul,Pop,Swap,Add ]
\end{code}
%% To do: better optimization
}

%% Apply denotational design

\framet{Compiling gracefully}{
\pause
\begin{enumerate}\itemsep8ex
\item Identify essence of stack computation.
\item Specify by precise analogy.
\item Solve for correct implementation.
% \item Profit!
% \item Apply to more interesting machines.
\end{enumerate}
}

\framet{The essence of stack computation}{
\parskip1ex
\vspace{6ex}
\begin{code}
forall z. (a :* z -> b :* z)  -- ``accumulator'' and ``stack''
\end{code}
\pause
\begin{code}
first :: (a -> b) -> forall z. (a :* z -> b :* z)

first f (a,z) = (f a, z)
\end{code}
\pause

Encapsulate in a new type:
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

%format ~> = "\leadsto"

%format `k` = ~>
%format k = (~>)

%format kk = "\Varid{k}"

\framet{Identify a useful vocabulary}{
For functions and other function-like things:
\begin{code}
class Category k where
  id   :: a `k` a
  (.)  :: (b `k` c) -> (a `k` b) -> (a `k` c)
\end{code}
\emph{Bonus:} automatically translate from Haskell!
(See \href{http://conal.net/papers/compiling-to-categories}{\emph{Compiling
to categories}}.)

Analogy/homomorphism properties (``functor''):
\begin{code}
id = stackFun id

stackFun g . stackFun f = stackFun (g . f)
\end{code}
Solve each equation for its one unknown (LHS operation).
}

%% Sadly, GHC no longer supports infix operators for type variables, so we
%% have to write something uglier.

\framet{Identify a useful vocabulary}{
For functions and other function-like things:
\begin{code}
class Category kk where
  id   :: a `kk` a
  (.)  :: (b `kk` c) -> (a `kk` b) -> (a `kk` c)
\end{code}
\emph{Bonus:} automatically translate from Haskell!
(See \href{http://conal.net/papers/compiling-to-categories}{\emph{Compiling
to categories}}.)

Analogy/homomorphism properties (``functor''):
\begin{code}
id = stackFun id

stackFun g . stackFun f = stackFun (g . f)
\end{code}
Solve each equation for its one unknown (LHS operation).
}

\framet{Identity}{
Homomorphism equation:
\vspace{3ex}
\begin{code}
id = stackFun id
\end{code}

%if True
\vspace{5ex}
Already in solved form.
%else
Trivial (already solved), but we can simplify/optimize:
\begin{code}
   stackFun id
=  {- definition of |stackFun| -}
   SF (first id)
=  {- property of |first| and |id| -}
   SF id
\end{code}
%endif
}

\framet{Sequential composition}{
\vspace{4ex}
\begin{code}
stackFun g . stackFun f = stackFun (g . f)
\end{code}

Simplify each side:\\
\hspace{2ex}
\begin{minipage}[c]{0.4\textwidth}
\begin{tcolorbox}
\mathindent0ex
\begin{code}
   stackFun g . stackFun f
=  {- definition of |stackFun| -}
   SF (first g) . SF (first f)
\end{code}
\end{tcolorbox}
\end{minipage}
%\begin{minipage}[b]{0ex}{\rule[1ex]{0.5pt}{1.05in}}\end{minipage}
\hspace{2ex}
\begin{minipage}[c]{0.4\textwidth}\setlength\mathindent{2ex}
\begin{tcolorbox}
\mathindent0ex
\begin{code}
   stackFun (g . f)
=  {- definition of |stackFun| -}
   SF (first (g . f))
=  {- property of |first| and |(.)| -}
   SF (first g . first f)
\end{code}
\end{tcolorbox}
\end{minipage}

Equivalent specification:
\begin{code}
SF (first g) . SF (first f) == SF (first g . first f)
\end{code}
}

\framet{Sequential composition}{
\vspace{3ex}
\begin{code}
SF (first g) . SF (first f) == SF (first g . first f)
\end{code}

\vspace{1ex}

Generalize/strengthen:
\vspace{2ex}
\begin{code}
SF g' . SF f' == SF (g' . f')  -- Now in solved form.
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

%if True
\framet{More easy operations}{
\vspace{3ex}
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
%endif

%format MonoidalPCat = MonoidalP
%format MonoidalPCat = Monoidal "\!_" :*

\framet{Parallel composition}{
\vspace{4ex}
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

Inlining,
\begin{code}
first (SF (first f)) == SF (first (first f))
\end{code}
\vspace{-5ex}

%if False
Types:

\vspace{-2ex}
\begin{code}
                f   :: a -> c
         first  f   :: a :* b -> c :* b
first (  first  f)  :: forall z. (a :* b) :* z -> (c :* b) :* z
\end{code}
\vspace{-5ex}

%endif
%% For stack computation, temporarily move |b| aside:
Can we eliminate a |first|?
\pause
\begin{code}
   first (first f)
=  {- definition of |first| on |(->)| -}
   \ ((a,b),z) -> ((f a,b),z)
=  {- definition of |lassocP|, |rassocP|, and |first| on |(->)| -}
   lassocP . first f . rassocP
\end{code}
}

%format --> = "\ \longmapsto\ "
%format -*> = "\ \longmapsto\!\!\!^\ast\ "

\framet{Homomorphism property for |first|}{
\parskip3ex
\vspace{2ex}
\begin{code}
first (first f) == lassocP . first f . rassocP
\end{code}

Accumulator/stack evolution:

\vspace{1ex}
\begin{code}
              ((a,b)          ,z)
rassocP  -->  (a              ,(b,z))    -- \emph{push}
first f  -*>  (f a            ,(b,z))    -- steps for |f|
lassocP  -->  ((f a,b)        ,z)        -- \emph{pop}
\end{code}
\vspace{-3ex}

%% Now we know what the stack is for!
The stack's purpose revealed!
}

\framet{Homomorphism property for |first|}{
Now
\begin{code}
first (SF (first f)) == SF (lassocP . first f . rassoc)
\end{code}
Strengthen/generalize:
\begin{code}
first (SF f') == SF (lassocP . f' . rassoc)  -- Now in solved form.
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
%if False
\small
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
%else
Inlining,
\begin{code}
stackFun f *** stackFun g == SF (  lassocP . first f . rassocP . first swapP .
                                   lassocP . first g . rassocP . first swapP)
\end{code}
%endif
Accumulator/stack evolution\out{ (right-to-left)}:
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
stackFun f *** stackFun g == SF (  lassocP . first f . rassocP . first swapP .
                                   lassocP . first g . rassocP . first swapP)
\end{code}

We've recursively flattened to \emph{purely sequential} compositions of:
\begin{itemize}\itemsep2ex
\item |first p| for a few primitive functions |p|, 
\item |rassocP| and |lassocP| in balanced pairs (``push'' and ``pop'').
\end{itemize}
}

%if True
%format ProductCat = Cartesian
\framet{Still more easy operations}{
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
%endif

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

\framet{Reifying stack computation}{
% From stack functions to stack programs
\begin{itemize}\itemsep6ex
\item Code generation needs inspection.
\item Introduce \emph{data} representation denoting stack functions.
\item Specify by homomorphism; solve for implementation.
\end{itemize}
}

\framet{Primitive operations}{
\vspace{3ex}
\small
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

instance MonoidalPCat StackProg where
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
Standard algebraic translation:
\begin{code}
   addC
.  (mulC . (const 2 *** exl) . dup *** mulC . (const 3 *** exr) . dup)
.  dup
\end{code}
Stack program:
\begin{code}
[  Dup,Push,Dup,Push,Const 2,Pop,Swap,Push,Exl,Pop
,  Swap,Mul,Pop,Swap,Push,Dup,Push,Const 3,Pop,Swap
,  Push,Exr,Pop,Swap,Mul,Pop,Swap,Add ]
\end{code}
%% To do: better optimization
}

%format +++ = "\!+\!"
%format :+++ = ":\!\!+"

\framet{Sum types and higher-order functions}{
\begin{itemize}\itemsep3ex
\item Sums (``coproducts'') are dual to products: |(+++), inl, inr, jam|.
\item Higher-order functions (``exponentials''): |curry, uncurry, apply|.
\end{itemize}

Additional ``primitives'':
\begin{code}
data Prim :: * -> * -> * NOP where  -- Notation
  ...
  (:+++) :: StackOps a c -> StackOps b d -> Prim (a :+ b) (c :+ d)
  Apply :: Prim ((a -> b) :* a) b
  Curry :: StackProg (a :* b) c -> Prim a (b -> c)
\end{code}
Or relax the representation.
}

\framet{Compiling gracefully}{
\begin{itemize}\itemsep4ex
\item Identify simple essence of stack computation\out{ (|StackFun|)}.
\item Relate to regular functions\out{ (|stackFun|)}.
\item Identify common algebraic vocabulary\out{ (|Category| etc)}.
\item Reify stack computation\out{ (|StackProg|)}.
\item Solve standard homomorphism equations $\Rightarrow$ compiler.
\item Standard translation from Haskell to algebraic form.
\end{itemize}
}

\framet{To do}{
\begin{itemize}\itemsep6ex
\item Better generic optimization
\item More ambitious machine models
\end{itemize}
}

\end{document}
