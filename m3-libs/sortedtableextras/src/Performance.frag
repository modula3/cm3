(* Created on Sun Nov 23 08:56:00 PST 1997 by heydon *)
(* Last modified on Thu Nov 27 11:52:34 PST 1997 by heydon *)

(*
This section describes the performance of the "Table.Default",
"SortedTable.Default", "RedBlackTbl.T", and "SkipListTbl.T"
implementations. All performance experiments were made on a Digital
AlphaStation 400 workstation equipped with a 233 megahertz DECchip
21064 processor running Digital Unix version 4.0. The tests were
performed on an unloaded machine, and all code was compiled with
optimization.

We instantiated each kind of table with integer keys and integer
values. Table~\ref{tbl:elapsed} shows the average elapsed times in
microseconds of various operations performed on these tables. Each
experiment was performed 10 times; the table shows the mean elapsed
time for each operation.

Three kinds of experiments were performed on each type of table,
denoted by the labels {\it Random}, {\it Increasing}, and {\it
Decreasing}. As described in more detail below, the only difference
between the three kinds of experiments is the keys that were used, and
the order in which elements were inserted into and deleted from the
tables.

\begin{table}[htb]
\begin{center}
\begin{tabular}{r|ccccccc}
           &        &        & Iter & Iter & Seek & Seek & \\
Experiment & Insert & Search &  Up  & Down &  Up  & Down & Delete \\
\hline
\multicolumn{1}{l|}{{\tt Table.Default}} & & & & & & & \\
Random     & 12 & 2 & \multicolumn{2}{c}{2} & N/A & N/A & 3 \\
Increasing & 20 & 6 & \multicolumn{2}{c}{1} & N/A & N/A & 2 \\
Decreasing & 21 & 6 & \multicolumn{2}{c}{1} & N/A & N/A & 2 \\
\hline
\multicolumn{1}{l|}{{\tt SortedTable.Default}} & & & & & & & \\
Random     & 40 & 15 & 1 & 1 & 10 & 10 & 14 \\
Increasing & 22 & 18 & 1 & 1 & 10 & 10 &  6 \\
Decreasing & 24 & 24 & 1 & 1 & 11 & 11 &  6 \\
\hline
\multicolumn{1}{l|}{{\tt RedBlackTbl.T}} & & & & & & & \\
Random     & 30 &  5 & 1 & 1 & 8 &  9 & 10 \\
Increasing & 34 & 11 & 1 & 1 & 8 &  9 &  6 \\
Decreasing & 35 & 11 & 1 & 1 & 8 &  9 &  6 \\
\hline
\multicolumn{1}{l|}{{\tt SkipListTbl.T}} & & & & & & & \\
Random     & 74 & 14 & 1 & 1 & 23 & 23 & 24 \\
Increasing & 58 & 42 & 1 & 1 & 23 & 24 &  6 \\
Decreasing & 54 & 24 & 1 & 1 & 22 & 23 & 15 
\end{tabular}
\end{center}
\caption{Average elapsed times in microseconds of various operations on
each of the four table implementations.}
\label{tbl:elapsed}
\end{table}

In the Random experiment, 100,000 keys were chosen at random
from the interval [1, 100000] and inserted into the table. Due
to collisions, the resulting table almost certainly contained
fewer than 100,000 elements. In the Increasing experiment, the
keys 1 through 100,000 were inserted into the table in
increasing order; in the Decreasing experiment, the same set of
keys were inserted in decreasing order.

After the keys were inserted, several more tests were performed
on each table:

\begin{itemize}
\item A {\it search} was done on the keys 1 through 100,000 in
order.

\item Upward and downward {\it iterations} were done on all of
the elements of each table. The single time shown for the
"Table.Default" implementation is the time required for
unordered iteration, since the type "Table.T" does not support
ordered iteration.

\item 100,000 upward and downward {\it seeks} were done on each
table. The "i"th key to seek for was calculated as "(i * 23) MOD
100000". This test has the effect of striding over the keys of
the table in order, but wrapping back to the start (or end if
seeking downward) of the table roughly 23 times. These times are
not reported for the "Table.Default" implementation because the
type "Table.Iterator" does not provide a "seek" method.

\item 100,000 {\it deletions} were performed using the same keys and
key order as were used in the insertion test. In the case of the
Random experiment, some of those deletions were undoubtably no-ops,
but 100,000 deletion operations were performed nonetheless.
\end{itemize}

The Random test is probably the most important of the three. The
Random results show that, as expected, the simple hash tables are
significantly faster than the tables supporting ordered iteration. The
Random results also show that the red-black tree implementation is
somewhat faster than the treap implementation, and significantly
faster than the skip list implementation.

We ran the Increasing and Decreasing tests to measure the performance
of the red-black implementation on its worst-case input. Even when
given its worst-case input, the red-black implementation does not
perform much worse than the treap implementation, and it is still
substantially faster than the skip list implementation.

\begin{table}[htb]
\begin{center}
\begin{tabular}{c|cc}
Table Type & Interface & Implementation \\
\hline
Table.Default       & 143 & 287 \\
SortedTable.Default & 109 & 433 \\
RedBlackTbl.T 	    & 118 & 487 \\
SkipListTbl.T 	    & 172 & 311
\end{tabular}
\end{center}
\caption{The size of each generic interface and implementation
module in lines of code.}
\label{tbl:codesize}
\end{table}

What price is there to be paid in code complexity for the increased
performance offered by red-black trees? As shown in
Table~\ref{tbl:codesize}, red-black trees do have the longest
implementation when measured in lines of code. However, the
implementation of red-black trees is only 12\% longer than that
of treaps and 57\% longer than that of skip lists. The red-black
implementation is indeed a bit more subtle than the skip list
implementation, but the increased code size is due partly to the
replicated code necessary to handle left and right cases. We spent
only an hour or so longer debugging the red-black code than we did
debugging the skip list code.
*)
