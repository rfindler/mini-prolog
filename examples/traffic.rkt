#lang mini-prolog

% from the plzoo https://github.com/andrejbauer/plzoo/

train(koper,ljubljana).
bus(koper,ljubljana).
bus(ljubljana,maribor).
bus(ljubljana,kocevje).
train(kranj,bled).
bus(bled,bohinj).

connection1(X,Y) :- train(X,Y).
connection1(X,Y) :- bus(X,Y).
connection(X,Y) :- connection1(X,Y).
connection(X,Y) :- connection1(X,Z), connection(Z,Y).
