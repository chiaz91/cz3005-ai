:- dynamic like/1.
:- dynamic dislike/1.

% facts (menu)
meal([vegan, value, healthy]).
sandwich([wheat, oat, italian, wrap]).
meat([fish, chicken, beef, pork]).
top_up([tuna, cheese, egg_mayo, bacon]).
salad([lettuce,tomato,cucumbers,olive,onion]).
sauce([chilli,sweet_onion, mustard, mayonnaise, bbq, ketchup]).
sides([soup, cookie]).

/*
	veggie meal -> no meat options, no cheese top-up
	healthy meal -> no fatty sauces
	value meal -> no top-up
*/
non_vegan([fish, chicken, beef, pork, onion, tuna, cheese, bacon, sweet_onion]).
non_healthy([mayonnaise, bbq, ketchup]).
non_value([tuna, cheese, egg_mayo, bacon]).


ask:-
	gen_options(L), validate_and_query_options(L).

% generate valid options by subtracting exclusion list from menu list
gen_options(L):-
	menu_list(M),
	exclude_list(E),
	remaining(M, E, L).
	
validate_and_query_options(L):-
	% get an option to ask from list
	member(X,L), 
	print('Do you like '),	
	print(X), 
	print('? y/n/q: '), 
	read(Like), (
		Like==q -> abort;
		Like==y -> print('Great..'), assert(like(X));
		print('Huh..'), assert(dislike(X))
	), 
	ask;
	% when an item is failed to get -> L=[] -> order is completed
	print('Order completed'),
	nl,
	print_order.

% find remaining element of L1 after subtracting common elements from L2
remaining(L1, L2, Result):-
	intersection(L1, L2, Temp), 
	subtract(L1, Temp, Result).

% construct a exclusion list (history options + item to be excluded based on preferrence)
exclude_list(L):-
	% construct history list
	findall(X, like(X), Likelist),
	findall(X, dislike(X), Dislikelist),
	append(Likelist, Dislikelist, History),
	% adding exclude list
	(like(vegan), non_vegan(NV); NV=[]), append(History, NV, E1),
	(like(healthy), non_healthy(NH); NH=[]), append(E1, NH, E2),
	(like(value), non_value(NV2); NV2=[]), append(E2, NV2, E3),
	(sandwich(S), member(B,S), like(B), append(E3, S, L); L=E3).

% combine all menu into a list
menu_list(L):-
	meal(M), sandwich(S), meat(M2), top_up(T), salad(S2), sauce(S3), sides(S4),
	append(M,S,V1), 
	append(V1,M2,V2), 
	append(V2,T,V3), 
	append(V3,S2,V4),
	append(V4,S3,V5),
	append(V5,S4,L).
	
print_order:-
	meal(M),
	findall(X, like(X), Likelist),
	remaining(Likelist, M, L),
	print(L).
	
clean:-
	retractall(like(_)),
	retractall(dislike(_)).
	
	