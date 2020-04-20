/* 	declaring predicates as dynamic, to allow runtime update on database
	ref: https://www.swi-prolog.org/FAQ/DynamicCode.html
*/
:- dynamic like/1.
:- dynamic dislike/1.
:- dynamic exclude/1.

/*	adding all menu as facts
	each menu contain list of items
*/
exclude([]).
meal([vegan, value, healthy]).
sandwich([wheat, oat, italian, wrap]).
meat([fish, chicken, beef, pork]).
top_up([tuna, cheese, egg_mayo, bacon]).
salad([lettuce, tomato,cucumbers,olive,onion]).
sauce([chilli, sweet_onion, mustard, mayonnaise, bbq, ketchup]).
sides([soup, cookie]).

/*	should_exclude is to set related exclusion to allow following rules to be added in the program
	vegan meal -> no meat options, no cheese top-up
	healthy meal -> no fatty sauces
	value meal -> no top-up
*/
should_exclude(vegan, [fish, chicken, beef, pork, onion, tuna, cheese, bacon, sweet_onion]).
should_exclude(healthy, [mayonnaise, bbq, ketchup]).
should_exclude(value, [tuna, cheese, egg_mayo, bacon]).

should_exclude(X, L):- 
	/*	As it is not making sense to select more than one bread or meat,
		hence the following predicate defines any bread or meat selected should exclude the other choices 
		to allow single selection
	*/
	sandwich(L), member(X,L);
	meat(L), member(X,L).


gen_options(L):-
	/*	this predicate generate list of valid items to be asked by subtracting exclusion list from menu list
	*/
	menu_list(M),
	exclude(E),
	subtract(M, E, L).
	


menu_list(L):-
	/*	this predicate merge all menu items into a single list L
	*/
	meal(M), sandwich(S), meat(M2), top_up(T), salad(S2), sauce(S3), sides(S4),
	append(M,S,V1), 
	append(V1,M2,V2), 
	append(V2,T,V3), 
	append(V3,S2,V4),
	append(V4,S3,V5),
	append(V5,S4,L).

check_exclude(X):-
	/*	this predicate validate if user like item X, and collect the related exclusion list based on item X,
		if related exclusion list is found, add it to current existing exclusion list
	*/
	like(X),
	should_exclude(X, L),
	add_exclude(L).

add_exclude(L):-
	/*	this predicate add list of item to existing exclusion list, and update knowledge base database.
	*/
	exclude(E),
	append(E, L, NewE),
	retractall(exclude(_)),
	assert(exclude(NewE)).
	

	
clean:-
	/*	this predicate removes all the facts that is added during past attempt of order collection to allow program start taking new order
	*/
	retractall(like(_)),
	retractall(dislike(_)),
	retractall(exclude(_)),
	assert(exclude([])).
	


/************** PROLOG ONLY COMMAND LINE INTERFACE **************/
ask:- 
	/*	this predicate starts order taking for custom subway by calling gen_options/1 to retrieve a list of all valid menu items,
		from the valid items, choose the first menu item to ask user whether they like the item X.
	*/
	gen_options(L), ask_or_display_result(L).


ask_or_display_result(L):-
	/*	this predicate attempt to choose the first item from the list of valid items L,
		after choosing, display 'Do you like X? y/n/q: ' to allow user input whether they want item X.
		once user responsed, program add their preferrence into database using assert/1, check for related exclusion with should_exclude/2
		and finally recursively ask for next question.
	*/
	member(X,L), 
	write('Do you like '), write(X), write('? y/n/q: '), 
	read(Like), (
		Like==q -> abort;
		Like==y -> (write('Great..'), assert(like(X)), add_exclude([X]), check_exclude(X));
		write('Huh..'), assert(dislike(X)), add_exclude([X])
	), 
	ask;
	/*	however if the program failed to get first item from valid list L, it means L is empty list. 
		which leads to end of program and user's order will be displayed on screen.
	*/
	writeln('Order completed'),
	print_order.
	
print_order:-
	/*	this predicate collect and print out all items liked by user
	*/
	meal(M),
	findall(X, like(X), Likelist),
	subtract(Likelist, M, L),
	writeln(L).


/************** DEBUGGING ONLY COMMAND **************/
log_debug:-
	findall(X, like(X), L),
	findall(X, dislike(X), D),
	exclude(E),
	write('likes: '),writeln(L),
	write('dislikes: '),writeln(D),
	write('excludes: '),writeln(E).
