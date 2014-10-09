/* ----------------------------- TESTS: ----------------------------*/
tests :- myGraph(G),
         clique(G,[jen,susan]),
         ispath(G,ken,jessica,[ken,<-,andrzej,->,susan,->,reed,->,tony,<-,jen,->,jessica]),
         goodfriends(G,jen,susan),
         wannabe(G,reed),
         idol(G,tony),
         \+ idol(G,reed),
         \+ wannabe(G,tony),
         \+ goodfriends(G,susan,jessica),
         \+ ispath(G,ken,jessica,[ken,<-,andrzej,->,susan,->,reed,->,tony,->,jen,->,jessica]),
         \+ clique(G,[jessica,susan]).

myGraph([
person(susan, [reed, jen, andrzej, jessica]),
person(reed, [tony, jessica]),
person(jessica, [jen]),
person(tony, []),
person(ken, [andrzej]),
person(jen, [tony, susan, jessica]),
person(andrzej, [susan, ken])]).

/* --------------------------- SOLUTION: ---------------------------*/

/* Defines select */
select_(X,[X|L],L).
select_(X,[Y|L],[Y|R]) :- select_(X,L,R).

/* Defines member */
member_(X,[X | _]).
member_(X,[_ | T]) :- member_(X,T).

/* Does $1 point to $2? */
friends(G,X,Y) :- member_(person(X,Xfriends),G),
                  member_(Y,Xfriends).

/* Does $1 point to all the elements of on $2? */
friends_with_list(_, _, []).
friends_with_list(G, X, [ H | L ]) :- friends(G,X,H),
                                      friends_with_list(G,X,L).

/* Does $1 and $2 point to each other? */
goodfriends(G, X, Y) :- friends(G,X,Y),
                        friends(G,Y,X).

/* Does $1 and all the elements of $2 point to each other? */
goodfriend_with_all(_, _, []).
goodfriend_with_all(G, X, [ H | L ]) :- goodfriends(G,X,H),
                                        goodfriend_with_all(G,X,L).

/* Does all the elements of $1 point the each other? */
clique(_,[_|[]]).
clique(G,[A|T ]) :- goodfriend_with_all(G,A,T),
                    select_(person(A,_),G,G_),
                    clique(G_, T).

/* Is there a chain of pointers from $1 to $2? */
chain(_,X,X).
chain(G,X,Y) :- select_(person(X,XfriendList),G,Gnew),
                friends(G,X,Z),

% OLEKS -1: Why don't you just check that Z occurs among the friends of X? This
% is somewhat inefficient.
%
% JENS: That is what I do. An alternative would be: ...
                member_(Z,XfriendList), 
% ... which is just an uglier way of expressing the same thing.

                member_(person(Z,_),Gnew),
                chain(Gnew,Z,Y).

/* Is there a chain from $1 to all the elements of $2? */
chain_to_all(_,_,[]).
chain_to_all(G,X,[person(Y,_)|L]) :- chain(G,X,Y),
                                     chain_to_all(G,X,L).

/* Is there a chain to $1 from all the elements of $2? */
chain_from_all(_,_,[]).
chain_from_all(G,X,[person(Y,_)|L]) :- chain(G,Y,X),
                                       chain_from_all(G,X,L).

/* Is there a chain from $1 to all the people in the graph? */
wannabe(G,X) :- chain_to_all(G,X,G).

/* Is there a chain to $1 from people in the graph? */
idol(G,X)    :- chain_from_all(G,X,G).

/* Is there a path as described in $3 from $1 to $2?
   (ispath strips off the first person, and makes sure the it is the first
   element in the path. `path` traverses the path.) */

ispath(G_, X, X, [X])        :- member_(person(X,_),G_).
ispath(G_, X, Y, [X,'->'|P]) :- select_(person(X,XfriendList),G_,G__),
                                member(Z,XfriendList),
                                ispath(G__,Z,Y,P).

ispath(G_, X, Y, [X,'<-'|P]) :- select_(person(X,_),G_,G__),
                                member(person(Z,ZfriendList),G_),
                                member(X,ZfriendList),
                                ispath(G__,Z,Y,P).
