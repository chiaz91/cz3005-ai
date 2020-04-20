/*	adding necessary modules
*/
:- debug.
:- style_check(-singleton).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
/* read database */
:- ['kb.pl'].

/* rout and bind predicate to default URL (eg localhost:port/) */
:- http_handler(/, home_page, []).
/* local host the server at given port */
server(Port):- http_server(http_dispatch, [port(Port)]).
/* stop hosting the server */
server_kill:-  http_stop_server(8000,[]),abort.


home_page(_Request) :-
	/* predicate that binded with root URL, if receive post request, it process the request else it show a simple form to ask for user input*/
   	member(method(post), _Request)->response_request(_Request);display_html_form.

response_request(_Request):-
	/* read data from the post request received */
	http_parameters(
		_Request, 
		[
			item(X,[optional(true), default(none)]), 
			like(Like,[optional(true), default(none)])
		]
	),
	(
		/* after data read from post request, update database based on user input */
		(Like=='y'->( assert(like(X)), add_exclude([X]), check_exclude(X);current_predicate(exclude/1)));
		assert(dislike(X)), add_exclude([X])
	),
	/* after database is updated based on user input, construct a html page to ask for next menu item  */
	gen_options(L),
	member(Y,L),
	html_form(Y);
	/* however if the program failed to get first item from valid list L, it means L is empty list. 
		which leads to end of program and user's order will be displayed on screen. */
	meal(M),
	findall(X, like(X), Likelist),
	subtract(Likelist, M, L),
	clean,
	html_order_completed(L).


display_html_form:-
	/* collect list of valid items, choose the first item X and construct a html page to ask whether user like the item X */
	gen_options(L),
	member(X,L),
	html_form(X).	
	

html_form(X):-
	/*	this predicate take in item X to be asked, and construct a html form page to ask whether user like item X*/
	reply_html_page(
		[title('Sandwich interactor')],
		[
			center(h1(['Subway sandwich interactor'])),
			center([
				form(
					[action='/', method='post'],
					[
						h2(['Do you like ', X, ' ?']),
						input([type='hidden',id='item',name='item',value=X],[]),
						input([type='radio',id='radio_like',name='like',value='y', checked]),
						label([for='radio_like'],'yes'),
						input([type='radio',id='radio_dislike',name='like',value='n']),
						label([for='radio_dislike'],'no'),
						br([]),br([]),
						button([type='submit'],['Submit'])
					]
				) 
			]),
			hr([]),
			center(p(['This program is developed with SWI Prolog to assist order taking on custom subway']))
		]
	).
	
html_order_completed(L):-
	/*	this predicate takes in list of user likded items, and construct a html page to present it to user */
	atomic_list_concat(L, ', ', Str),
	reply_html_page(
		[title('Sandwich interactor')],
		[
			center(h1(['Subway sandwich interactor'])),
			center(h2(['Order taken: '])),
			center(p([Str]) ),
			center([
				form(
					[action='/', method='post'],
					[button([type='submit'],['try again'])]
				)
			])
		]
	).
	

/* when program is loaded, it automatically host the server on port 8000 */
:-server(8000).







