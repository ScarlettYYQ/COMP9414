% Yaqi YANG
%z5143675
%CSE9414 Assignment_1-prolog programming

% 1
% Write a predicate sumsq_neg(Numbers, Sum) that sums the squares of only the negative numbers in a list of numbers.

sumsq_neg([], 0).

sumsq_neg([Number|Rastlist], Sum):-
Number < 0,
sumsq_neg(Rastlist, NewSum),
Sum is NewSum + Number * Number.

sumsq_neg([Number|Rastlist], Sum):-
not(Number < 0),
sumsq_neg(Rastlist,Sum).

% 2
% Write a predicate all_like_all(Who_List, What_List) that takes a list of people Who_List and a list of items What_List and succeeds if every person in Who_List likes every item in What_List, according to the predicate likes(Who, What). Your predicate should also succeed if either Who_List or What_List is empty.

all_like_all(_,[]).
all_like_all([],_).

all_like_all(WhoList, [Item|RestWhat]):-
[Person|RestWho] = WhoList,
personLike(Person,[Item|RestWhat]),
all_like_all(RestWho, [Item|RestWhat]).

personLike(_,[]).
personLike(Person,[Item|RestWhat]):-
likes(Person,Item),
personLike(Person,RestWhat).

% 3
% Write a predicate sqrt_table(N, M, Result) that binds Result to the list of pairs consisting of a number and its square root, from N down to M, where N and M are non-negative integers, and N >= M. For example:

sqrt_number(N, M, Sqrt_Result):-
N>M,
New_N is N-1,
Sqrt_N is sqrt(N),
Sqrt_Result = [[N,Sqrt_N]|New_Result],
sqrt_number(New_N, M, New_Result).

sqrt_number(N, M, New_Result):-
N=M,
Sqrt_N is sqrt(N),
New_Result = [[N,Sqrt_N]].

sqrt_table(N, M, Result):-
M>0,
N>=M,
sqrt_number(N, M, Sqrt_Result),
Result = Sqrt_Result.

% 4
% Write a predicate chop_up(List, NewList) that takes List and binds NewList to List with all sequences of successive increasing whole numbers replaced by a two-item list containing only the first and last number in the sequence. An example of successive increasing whole numbers is: 19,20,21,22. (Note that the numbers have to be successive in the sense of increasing by exactly 1 at each step.) For example:

chop_up([FirstNumber|RestList], NewList):-
test(FirstNumber,RestList,IfResult),
NewList = IfResult.

test_1(C,[],[C]).
test_1(A,[Y|X],[A|Z]):-
Q is A+1,
Y= Q,
test_1(Y,X,Z).

test_1(A,[Y|_X],Z):-
Q is A+1,
Y\= Q,
Z=[A].

test([],[],[]).
test(U,[],I):-
U\=[],
I=[U].

test(A,[Y|X],C):-
Q is A+1,
Y= Q,
test_1(A,[Y|X],Z),
reverse(Z,Z_R),
firstandlast(Z,Z_R,Z_FINAL),
C=[Z_FINAL|New_C],
try(Z,[A,Y|X],[R|Tail_Rest]),
test(R,Tail_Rest,New_C).

test(A,[Y|X],C):-
Q is A+1,
Y \= Q,
C=[A|New_C],
test(Y,X,New_C).

try([],[],[[]]).
try([],U,U).
try([Z_1|Z_2],[Z_1|Q],R):-
try(Z_2,Q,R).

firstandlast([],_,[]).
firstandlast([Z_1|_],[Z_R_1|_],R):-
R=[Z_1,Z_R_1].

% 5
% For this question we consider binary expression-trees whose leaves are either of the form tree(empty, Num, empty) where Num is a number, or tree(empty, z, empty) in which case we will think of the letter z as a kind of "variable". Every tree is either a leaf or of the form tree(L, Op, R) where L and R are the left and right subtrees, and Op is one of the arithmetic operators '+', '-', '*', '/' (signifying addition, subtraction, multiplication and division).

tree_eval(Value, tree(L, Op , R), Eval):-
tree_eval(Value,L,Eval_L),
tree_eval(Value,R,Eval_R),
op(Eval_L,Eval_R,Op,Eval).

tree_eval(_Value,tree(empty, Num, empty),Eval):-
number(Num),
Eval = Num.

tree_eval(Value,tree(empty, Num, empty),Eval):-
atom(Num),
Eval=Value.

op(Eval_L,Eval_R,Op,OpResult):-
Op = +,
OpResult is Eval_L + Eval_R.

op(Eval_L,Eval_R,Op,OpResult):-
Op = -,
OpResult is Eval_L - Eval_R.

op(Eval_L,Eval_R,Op,OpResult):-
Op = /,
OpResult is Eval_L / Eval_R.

op(Eval_L,Eval_R,Op,OpResult):-
Op = *,
OpResult is Eval_L * Eval_R.


