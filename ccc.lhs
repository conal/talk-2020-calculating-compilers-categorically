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

\framet{Example}{
\begin{code}
\ (x,y) -> 2 * x + 3 * y
\end{code}
}

\note{Give translation.}

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

\framet{Analogy}{

Specification: |stackFun| defines a \emph{precise analogy}.

\begin{itemize}\itemsep2ex \parskip2ex
\item Identify a standard vocabulary (algebraic abstraction).
\item \emph{Analogous} meaning, related by |stackFun|.
\item Formalize algebraically: homomorphism.
%% \item Solutions are correct compilers.
\end{itemize}

%% Domain-independent translation to \& from standard vocabulary.

}

\end{document}
