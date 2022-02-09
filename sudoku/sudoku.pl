/* Lab 3  : Deadline 13th November 2021 (Sat) 11pm

   Generalizing Sudoku Puzzle in Prolog
   
   Below is a solution to Sudoku Puzzle
   can be found in SWI-Prolog web site. It is fun
   to see how it works for a 9 x 9 sudoku puzzle
   with 3 x 3 mini-blocks. If you have not played
   Sudoku before, please try mini-sudoku first:
      https://www.mathinenglish.com/puzzlessudoku.php

   There are variations of the suduko puzzle
   with different main grid sizes and mini-block sizes.

   For example, junior-sudoku is based on
       4 x 4 grid with 2 x 2 mini-blocks

   Another example is mini-sudoku that is based on
       6 x 6 grid with 3 x 2 mini-blocks

   Task 1 (85%)
   ======
   Generalize your sudoku solution generator using
   a new predicate gen_suduko below which supports
   different variations of sudoku puzzles, based on
   grid and mini-block sizes.

   gen_sudoku(Rows,N,B_R,B_C)
      N - size of entire block of N x N
      B_R - mini-block row size
      B_C - mini-block column size

   We can add the following constraints:
         N #>3, B_R >1, B_C>1, N #= B_R * B_C
   To restrict ourselves to regular-shaped sudokus that
   that are easier for humans to follow.
   
   The output for gen_sudoku will be made using maplist(portray_clause, Rows)
   in the query predicate.

   Prolog Task 2 (15%)
   ======
   Design a problem suduko generator, called
           find_puzzle_sudoku(Rows,S,N,M,B_R,B_C)
   that would generate a random sudoku puzzle of grid size S x S,
   mini-blocks B_R x B_C and which has from N to M known number of values. 

   Your solution may make use of random generator predicate
      random(+L:int, +U:int, -R:int)

   It should start with a random puzzle (whose numbers
   are well-spaced out) with N known numbers.
   If this did not return a unique solution, you could
   add one more (random) number to this incomplete puzzle,
   You can progressively do that until it hits M known numbers.

   If no unique puzzle is found with M known numbers, you can
   exit with a false outcome.

   PS You may use a predicate  aggregate_all(count, pred(X), Count). to count number of solutions.
   See https://stackoverflow.com/questions/6060268/prolog-count-the-number-of-times-a-predicate-is-true

   Due to the use of randomization, kindly note that the solution you get
   from this predicate is non-deterministic. You should try to
   think of some solutions that would give you the best possible outcome
   with smallest number of random values used,
   but without sacrificing too much on the time taken for your puzzle
   generator to terminate. If appropriate, you may repeat some of the 
   randomization processes but bearing in mind a trade-off between a 
   better solution versus time-out.
   
   We shall have a mini-competition to see who has the best find_puzzle_sudoku code. 
   For this mini-competition, the winner is one who can use the smallest number of
   known values used, followed by time taken. The competition is 
   just for fun and to encourage you to try your best.
   
Below are some test cases for mini and junior 
sudoku test cases.

mini_suduko(1,[[_,_,6,_,4,_],
               [_,_,_,_,6,_],
               [_,_,_,5,_,3],
               [3,_,_,_,_,_],
               [_,1,_,_,_,_],
               [_,5,_,_,4,_]]).

junior_suduko(1,[[_,4,_,1],
                 [3,_,4,_],
                 [1,_,_,4],
                 [_,2,1,_]]).

mini_suduko 1: 

No result. 

junior_suduko 1 :

[2, 4, 3, 1].
[3, 1, 4, 2].
[1, 3, 2, 4].
[4, 2, 1, 3].

   
*/

:- use_module(library(clpfd)).

/*
  Doc on maplist (similar to Haskell's map)
   https://www.swi-prolog.org/pldoc/man?predicate=maplist/2
  Doc on transpose/2 (for list of lists of same length)
   https://www.swi-prolog.org/pldoc/doc_for?object=clpfd%3Atranspose/2
  Doc on append/2 (to concatenate a list of lists)
   https://www.swi-prolog.org/pldoc/doc_for?object=append/2
  Doc on same_length/2 (to concatenate a list of lists)
   https://www.swi-prolog.org/pldoc/doc_for?object=same_length/2
  
*/


sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).

problem(2, [[3,_,_,8,_,1,_,_,2],
            [2,_,1,_,3,_,6,_,4],
            [_,_,_,2,_,4,_,_,_],
            [8,_,9,_,_,_,1,_,6],
            [_,6,_,_,_,_,_,5,_],
            [7,_,2,_,_,_,4,_,9],
            [_,_,_,5,_,9,_,_,_],
            [9,_,4,_,8,_,7,_,5],
            [6,_,_,1,_,7,_,_,3]]).

problem(3,     [[_,_,6,_,4,_],
               [_,_,_,_,6,_],
               [_,_,_,5,_,3],
               [3,_,_,_,_,_],
               [_,1,_,_,_,_],
               [_,5,_,_,4,_]]).

problem(4,       [[_,4,_,1],
                 [3,_,4,_],
                 [1,_,_,4],
                 [_,2,1,_]]).

problem(5, [[_,2,_,3,6,_],
            [_,_,_,5,_,2],
            [1,5,_,_,_,4],
            [2,_,3,1,5,6],
            [4,3,_,6,1,_],
            [6,1,_,4,2,_]]).

% multiple solutions         
problem(6, [[_,5,_,_,_,_],
            [_,_,2,5,_,_],
            [_,_,_,4,_,_],
            [4,_,_,_,5,6],
            [2,_,5,_,_,1],
            [_,6,1,2,4,_]]).

% multiple solutions
problem(7, [[2,9,5,7,4,3,8,6,1],
            [4,3,1,8,6,5,9,_,_],
            [8,7,6,1,9,2,5,4,3],
            [3,8,7,4,5,9,2,1,6],
            [6,1,2,3,8,7,4,9,5],
            [5,4,9,2,1,6,7,3,8],
            [7,6,3,5,2,4,1,8,9],
            [9,2,8,6,7,1,3,5,4],
            [1,5,4,9,3,8,6,_,_]]).

% side case that requires guessing but only 1 solution
problem(8, [
            [_,_,_,_,_,5,_,_,_], 
            [_,2,_,_,9,_,_,1,_], 
            [_,4,7,1,_,_,_,9,_], 
            [_,_,_,_,_,2,_,5,_], 
            [_,_,8,6,4,_,_,_,_], 
            [1,_,_,_,_,_,4,7,_], 
            [_,8,_,_,6,4,_,2,_], 
            [_,_,_,7,_,8,_,6,_], 
            [3,_,_,2,1,9,8,_,_]
        ]).

% query for 9x9 sudoku
query(Prob,Rows) :- problem(Prob, Rows), sudoku(Rows), maplist(portray_clause, Rows).

% query for N*N sudoku with blocks B_R * B_C
gen_query(Prob, Rows, N, B_R, B_C) :- problem(Prob, Rows), gen_sudoku(Rows, N, B_R, B_C), maplist(portray_clause, Rows).


/*
  N - size of entire block of N x N
  B_R - mini-block column size
  B_C - mini-block column size
*/      
gen_sudoku(Rows, N, B_R, B_C):- 
         N #>1, B_R >1, B_C>1, N #= B_R * B_C,
         length(Rows, N), maplist(same_length(Rows), Rows),
         append(Rows, Vs), Vs ins 1..N,
         maplist(all_distinct, Rows),
         transpose(Rows, Columns),
         maplist(all_distinct, Columns),
         split_at(B_R, Rows, Sections), % split into row sections
         test_sections(Sections, B_R, B_C).

/*
Splits a list into N sublists of equal length.
*/
split_at(_, [], []).
split_at(N, Xs, [H|R]) :- append(H, T, Xs), length(H, N), split_at(N, T, R).
      
% checks that all row sections are valid
test_sections([], _, _).        
test_sections([X|Rest], B_R, B_C):- test_row_section(X,B_R,B_C), test_sections(Rest,B_R,B_C). 

% ensure all blocks in each row section is valid
test_row_section(Rows, B_R, B_C) :-
         transpose(Rows, Cols), 
         flatten(Cols, F),
         I is B_C*B_R,
         split_at(I, F, Blocks),
         maplist(all_distinct, Blocks).


mini_sudoku(Rows) :- gen_sudoku(Rows,6,2,3).
junior_sudoku(Rows) :- gen_sudoku(Rows,4,2,2).
new_sudoku(Rows) :- gen_sudoku(Rows,9,3,3).

/*
  puzzle generated with grid size S x S.
  the puzzle contain from N to M known values.

  Rows - unsolved puzzle
  S - size of entire block of S x S
  N - minimum known values
  M - maximum known values
  B_R - mini-block column size
  B_C - mini-block column size
*/ 

find_puzzle_sudoku(Rows, S, N, M, B_R, B_C):- 
      N #< M, M #=< S*S,
      length(Rows, S), maplist(same_length(Rows), Rows),
      gen_sudoku(P, S, B_R, B_C), % P - solved puzzle
      add_n_vals(Rows,P,N), % add N values
      D is M-N,
      add_more_vals(Rows,P,D), % add till max M values
      unique_sol(P), % if there's no unique sol, exit with false outcome
      write("Puzzle:\n"),
      maplist(portray_clause, Rows),
      write("Solution:\n"),
      maplist(portray_clause, P).

% adds first N random values
add_n_vals(_,_,0):- !.
add_n_vals(Rows,P,N):- unique_sol(P), add_exist_val(Rows,P), M is N-1, add_n_vals(Rows,P,M), !.
add_n_vals(Rows,P,N):- add_val(Rows,P), M is N-1, add_n_vals(Rows,P,M).

% adds more random values until puzzle is solvable or maxed values
add_more_vals(_,_,0):- !.
add_more_vals(_,P,_):- unique_sol(P), !.
add_more_vals(Rows,P,N):- add_val(Rows,P), M is N-1, add_more_vals(Rows,P,M).

% add a value to an unsolved cell in P
add_val(Rows,P):-
      get_coord(P,R,C), % get unsolved cell from P
      set_rand(Rows,P,R,C). 

% add a solved value in P to Rows
add_exist_val(Rows,P):-
      get_coord(Rows,R,C), % get unchosen cell from Rows
      set_exist_rand(Rows,P, R, C).

% gets list of row ids that have at least one unsolved cell
get_avail_rows([],_,[]).
get_avail_rows([R|Xs],I,A):- maplist(nonvar, R), J is I+1, get_avail_rows(Xs,J, A), !.
get_avail_rows([_|Xs],I, [I|A]):- J is I+1, get_avail_rows(Xs, J, A).

% gets list of cell col ids that are unsolved
get_avail_cells([],_,[]).
get_avail_cells([R|Xs],I,A):- nonvar(R), J is I+1, get_avail_cells(Xs,J, A), !.
get_avail_cells([_|Xs],I,[I|A]):- J is I+1, get_avail_cells(Xs, J, A).

% get random row and col numbers
get_coord(Rows,R,C):-
      get_avail_rows(Rows,0,A_R),
      random_member(R, A_R), % choose random row number from available rows
      nth0(R, Rows, Row),
      get_avail_cells(Row,0,A_C),
      random_member(C, A_C). % choose random col number from available cells


% set random number at cell in P and Rows
set_rand(Rows,P, R, C):-
      find_values(P,R,C,List), % finds available values of cell
      random_member(Val, List), % choose random cell val
      cell(Rows, Val, R, C), 
      cell(P, Val, R, C).

% get number at cell in P and set in Rows
set_exist_rand(Rows,P, R, C):-
      cell(P, Val, R, C), % obtain bounded value at cell in P
      cell(Rows, Val, R, C). % set value at cell in Rows

% finds available values a cell can be bound to
find_values(P,R,C,List):-
   cell(P, Val, R, C),
   findall(Val, indomain(Val), List).

% value of a cell
cell(Rows,Val,R,C):-
   nth0(R, Rows, Row),
   nth0(C, Row, Val).

% determines if sudoku has unique solution
unique_sol([]).
unique_sol([R|Xs]):- maplist(nonvar, R), unique_sol(Xs).

% unique_sol(Rows):- aggregate_all(count, maplist(label,Rows), Count), Count #= 1.


   
