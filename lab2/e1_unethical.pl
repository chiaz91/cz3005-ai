% facts 
company(sumsum).
company(appy).
competitor(sumsum,appy).
phone_technology(galatica_s3).
develop(sumsum,galatica_s3).
steal(stevey,galatica_s3).
boss(stevey).

% rules 
unethical(X) :-
	boss(X),
	steal(X, B),
	business(B),
	develop(C,B),
	rival(C).
rival(X) :- competitor(X,appy).
business(X) :- phone_technology(X).
