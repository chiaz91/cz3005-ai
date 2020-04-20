% facts 
prince(charles).
prince(andrew).
prince(edward).
princess(ann).
older(charles, ann).
older(ann, andrew).
older(andrew, edward).

% Age rules
% recursion is used to find out the older relation with transitivity characteristic 
older_than(X, Y):- older(X,Y);older(X,Z), older_than(Z,Y).

% prior/2, predicate defines if X has higher precedence than Y
% In new rule, only birth order is being considered.
prior(X, Y):- older_than(X, Y).

% prior/3, predicate that finds the higher precedence variable in 3rd parameter
prior(X, Y, X) :- prior(X, Y).
prior(X, Y, Y) :- prior(Y, X).

% [List Operation] find top precedence item from list
% base case (fact): if list only have one item, the item has top precedence
% however if list has more than 1 item, post order recursion is used to find out which item has top precedence in the list
% each recursion, we let first item as X, then rest of the item will be tail T, we find a top precedence item TempTop from T, 
% TempTop is then compare with X to get acutal top precedence item Top of each recursion.
top_of_list([X], X).
top_of_list([X|T], Top) :-
	top_of_list(T, TempTop),
	prior(X, TempTop, Top).

% [List Operation] del an item from list
% 1st parameter: itme to be deleted
% 2nd parameter: list that item is deleting from
% 3rd parameter: result after operation
% base case 1 (fact 1): list only has one item X, then reutn empty list as result
% base case 2 (fact 2): let first item be X, rest of items be Tail, T. After deleting item X, result will be T.
% if deleting item X is not in first item, then then deleting item should appear in Tail, and recursion starts   
del(X, [X], []).
del(X, [X|T], T).
del(X, [Y|T],[Y|T2]) :- del(X, T, T2).
	
% print out precedence order in sequence
print_top([]).
print_top(X) :-
	top_of_list(X, T),
	print(T),
	nl,
	del(T, X, R),
	print_top(R).
	
%print_top([ann,edward,charles,andrew]).