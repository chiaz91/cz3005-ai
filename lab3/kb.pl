:- dynamic like/1.
:- dynamic dislike/1.
:- dynamic exclude/1.

% facts (menu)
exclude([]).
meal([vegan, value, healthy]).
sandwich([wheat, oat, italian, wrap]).
meat([fish, chicken, beef, pork]).
top_up([tuna, cheese, egg_mayo, bacon]).
salad([lettuce, tomato,cucumbers,olive,onion]).
sauce([chilli, sweet_onion, mustard, mayonnaise, bbq, ketchup]).
sides([soup, cookie]).

/*
	veggie meal -> no meat options, no cheese top-up
	healthy meal -> no fatty sauces
	value meal -> no top-up
*/
should_exclude(vegan, [fish, chicken, beef, pork, onion, tuna, cheese, bacon, sweet_onion]).
should_exclude(healthy, [mayonnaise, bbq, ketchup]).
should_exclude(value, [tuna, cheese, egg_mayo, bacon]).
% meat and sandwish should only allow one selection
should_exclude(X, L):- 
	sandwich(L), member(X,L);
	meat(L), member(X,L).

ask:-
	gen_options(L), validate_and_query_options(L).

% generate valid options by subtracting exclusion list from menu list
gen_options(L):-
	menu_list(M),
	%exclude_list(E),
	exclude(E),
	subtract(M, E, L).
	
validate_and_query_options(L):-
	% get an option to ask from list
	member(X,L), 
	write('Do you like '),	
	write(X), 
	write('? y/n/q: '), 
	read(Like), (
		Like==q -> abort;
		Like==y -> (write('Great..'), assert(like(X)), add_exclude([X]), check_exclude(X));
		write('Huh..'), assert(dislike(X)), add_exclude([X])
	), 
	ask;
	% when an item is failed to get -> L=[] -> order is completed
	write('Order completed'),
	nl,
	print_order.

% combine all menu into a list
menu_list(L):-
	meal(M), sandwich(S), meat(M2), top_up(T), salad(S2), sauce(S3), sides(S4),
	append(M,S,V1), 
	append(V1,M2,V2), 
	append(V2,T,V3), 
	append(V3,S2,V4),
	append(V4,S3,V5),
	append(V5,S4,L).

check_exclude(X):-
	like(X),
	should_exclude(X, L),
	add_exclude(L).

add_exclude(L):-
	exclude(E),
	append(E, L, NewE),
	retractall(exclude(_)),
	assert(exclude(NewE)).
	
print_order:-
	meal(M),
	findall(X, like(X), Likelist),
	subtract(Likelist, M, L),
	writeln(L).
	
clean:-
	retractall(like(_)),
	retractall(dislike(_)),
	retractall(exclude(_)),
	assert(exclude([])).
	
log_debug:-
	findall(X, like(X), L),
	findall(X, dislike(X), D),
	exclude(E),
	write('likes: '),writeln(L),
	write('dislikes: '),writeln(D),
	write('excludes: '),writeln(E).