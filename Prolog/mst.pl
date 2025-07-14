% Afifa Saeed 835367
% ho collaborato Jhordan Steve Rodríguez Rojas 832966
% e con Sharmin Anthuane Camacho Rojas 793661

:-dynamic graph/1.
:-dynamic vertex/2.
:-dynamic arc/4.
:-dynamic heap/2.
:-dynamic heap_entry/4.
:-dynamic vertex_key /3.
:-dynamic vertex_previous /3.

%%-- INTERFACCIA PER LA MANIPOLAZIONE DI GRAFI --%%

%new_graph
new_graph(G) :- graph(G), !.

new_graph(G) :- assert(graph(G)), !.

%delete_graph
delete_graph(G) :-
    retractall(graph(G)),
    retractall(arc(G, _, _, _)),
    retractall(vertex(G, _)),
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)).

%new_vertex
new_vertex(G, V) :- vertex(G, V), !.

new_vertex(G, V) :- graph(G),
	            assert(vertex(G, V)), !.

%graph_vertices
graph_vertices(G, Vs) :-
    setof(vertex(G, C), vertex(G, C), Vs).

%list_vertices
list_vertices(G) :- graph(G), !,
	            listing(vertex(G, _)).

%new_arc
new_arc(G, U, V) :- new_arc(G, U, V, 1), !.

new_arc(G, U, V, W) :-
    W > 0,
    retract(arc(G, U, V, _)),
    assert(arc(G, U, V, W)), !.

new_arc(G, U, V, W) :-
    W > 0,
    retract(arc(G, V, U, _)),
    assert(arc(G, V, U, W)), !.

new_arc(G, U, V, W) :-
    U \= V,
    graph(G),
    vertex(G, U),
    vertex(G, V),
    W > 0,
    assert(arc(G, U, V, W)), !.

new_arc(G, _, _, W) :-
    graph(G),
    W =< 0, !.

new_arc(G, U, V, _) :-
    graph(G),
    U == V, !.

%graph_arcs
graph_arcs(G, Es) :-
    findall(arc(G, V1, V2, D), arc(G, V1, V2, D), Es).

%vertex_neighbors
vertex_neighbors(G, V, Ns) :-
    find_right(G, V, X), !,
    find_left(G, V, X1), !,
    append(X, X1, Ns).

find_right(G, V, X) :-
    vertex(G, V),
    findall(arc(G, V, R, W), arc(G, V, R, W), X).

find_left(G, V, X):-
    vertex(G, V),
    findall(arc(G, V, L, W), arc(G, L, V, W), X).

%adjs
adjs(G, V, Vs) :-
    vertexz_right(G, V, X),
    vertexz_left(G, V, X1),
    append(X, X1, Vs).

vertexz_right(G, V, X) :-
    vertex(G, V),
    findall(vertex(G, R), arc(G, V, R, _), X).

vertexz_left(G, V, X) :-
    vertex(G, V),
    findall(vertex(G, L), arc(G, L, V, _), X).

%list_arcs
list_arcs(G) :- graph(G),
    listing(arc(G, _, _, _)).

%list_graph
list_graph(G) :- graph(G),
	         list_vertices(G),
		 list_arcs(G).

%read_graph
read_graph(G, FileName) :-
    graph(G),
    delete_graph(G),
    read_graph_from_file(G, FileName), !.


read_graph(G, FileName) :-
    new_graph(G),
    read_graph_from_file(G, FileName), !.

read_graph_from_file(G, FileName) :-
    csv_read_file(FileName,
                  Righe,
                  [functor(table),
                   arity(3),
                   separator(0'\t)]),
                   generate_graph(G, Righe).

generate_graph(_, []):- !.

generate_graph(G, [X | Xs]) :-
    arg(1, X, A),
    arg(2, X, B),
    arg(3, X, C),
    new_vertex(G, A),
    new_vertex(G, B),
    new_arc(G, A, B, C),
    generate_graph(G, Xs), !.

%write_graph
write_graph(G, Filename) :-
    write_graph(G, Filename, graph).

write_graph(G, Filename, graph) :-
    graph(G),
    findall(row(A, B, C), arc(G, A, B, C), Rows),
    csv_write_file(Filename,
                   Rows,
                   [functor(table),
                    arity(3),
                    separator(0'\t)]), !.

write_graph(G, FileName, edge) :-
    generate_data(G, Data),
    csv_write_file(FileName, Data,
                   [separator(0'\t)]), !.

generate_data([], []) :- !.

generate_data([X | Arcs], [Y | Data]) :-
    arg(2, X, U),
    arg(3, X, V),
    arg(4, X, W),
    Y =.. [arc, U, V, W],
    generate_data(Arcs, Data).


%%------ IMPLEMENTAZIONE MINHEAP ------%%

%new_heap
new_heap(H) :- heap(H, _S), !.

new_heap(H) :- assert(heap(H, 0)), !.

%delete_heap
delete_heap(H) :-
    retractall(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).

%heap_has_size
heap_has_size(H, S) :- heap(H, S).

%heap_empty
heap_empty(H) :- heap_has_size(H, 0).

%heap_not_empty
heap_not_empty(H) :- heap(H, S),
	             S > 0.

%heap_head
heap_head(H, K, V) :- heap_entry(H, 1, K, V), !.

%heap_insert
heap_insert(H, K, V) :-
    heap_has_size(H, S),
    retract(heap(H, _)),
    S1 is S + 1,
    assert(heap(H, S1)),
    assert(heap_entry(H, S1, K, V)),
    ordinamento_P(H, S1).

ordinamento_P(_H, P) :-
    P = 1, !.

ordinamento_P(H, P) :-
    heap_entry(H, P, K1,_V1),
    NP is floor(P/2),
    heap_entry(H, NP, K2, _V2),
    K1 >= K2, !.

ordinamento_P(H, P) :-
    heap_entry(H, P, K1, V1),
    NP is floor(P/2),
    heap_entry(H, NP, K2, V2),
    K1 < K2,
    exchange(H, P, K1, V1, NP, K2, V2),
    ordinamento_P(H, NP), !.

exchange(H, P, K1, V1, NP, K2, V2) :-
    retract(heap_entry(H, P, K1, V1)),
    retract(heap_entry(H, NP, K2, V2)),
    assert(heap_entry(H, P, K2, V2)),
    assert(heap_entry(H, NP, K1, V1)),!.


%heap_extract /3
heap_extract(H, K, V) :-
    heap_head(H, K, V),
    heap_has_size(H, S),
    S == 1,
    retract(heap(H, _)),
    retract(heap_entry(H, 1, K, V)),
    assert(heap(H, 0)), !.

heap_extract(H, K, V) :-
    heap_head(H, K, V),
    heap_has_size(H, S),
    S > 1,
    retract(heap(H, _)),
    S1 is S - 1,
    assert(heap(H, S1)),
    retract(heap_entry(H, 1, K, V)),
    retract(heap_entry(H, S, K2, V2)),
    assert(heap_entry(H, 1, K2, V2)),
    heapify(H, 1), !.

heapify(H, P) :-
    heap_has_size(H, S),
    S == P, !.

heapify(H, P) :-
    heap_has_size(H, S),
    S == 2,
    PS is P * 2,
    heap_entry(H, P, K1,_),
    heap_entry(H, PS, K2,_),
    K1 =< K2 , !.

heapify(H, P):-
    heap_has_size(H, S),
    S == 2,
    PS is P * 2,
    heap_entry(H, P, K1, V1),
    heap_entry(H, PS, K2, V2),
    K1 > K2,
    exchange(H, P, K1, V1, PS, K2, V2), !.

heapify(H, P) :-
    heap_has_size(H, S),
    S > 2,
    PS is P * 2,
    PD is PS + 1,
    PD > S,
    PS > S, !.

heapify(H, P) :-
    heap_has_size(H, S),
    S > 2,
    PS is P * 2,
    PD is PS + 1,
    PD > S,
    PS =< S,
    heap_entry(H, P, K1, V1),
    heap_entry(H, PS, K2, V2),
    K2 < K1,
    exchange(H, P, K1, V1, PS, K2, V2), !.

heapify(H, P) :-
    heap_has_size(H, S),
    S > 2,
    PS is P * 2,
    PD is PS + 1,
    PD > S,
    PS =< S,
    heap_entry(H, P, K1, _),
    heap_entry(H, PS, K2, _),
    K2 >= K1, !.

heapify(H, P) :-
    heap_has_size(H, S),
    S > 2,
    PS is P * 2,
    PD is PS + 1,
    PD =< S,
    heap_entry(H, P, K1, V1),
    PS is P * 2,
    PD is PS + 1,
    heap_entry(H, PS, K2, V2),
    heap_entry(H, PD, K3, V3),
    ordina_figli(H, P, K1, V1, PS, K2, V2, PD, K3, V3).

ordina_figli(H, _, K1, _, PS, K2, _, PD, K3, _):-
    heap_has_size(H, S),
    PS =< S,
    PD =< S,
    K1 =< K2,
    K1 =< K3, !.

ordina_figli(H, P, K1, V1, PS, K2, V2, PD, K3, _):-
    heap_has_size(H, S),
    PS =< S,
    PD =< S,
    K1 > K2,
    K2 =< K3,
    exchange(H,P,K1,V1,PS,K2,V2),
    heapify(H,PS).

ordina_figli(H, P, K1, V1, PS, K2, _, PD, K3, V3):-
    heap_has_size(H, S),
    PS =< S,
    PD =< S,
    K1 > K3,
    K3 < K2,
    exchange(H, P, K1, V1, PD, K3, V3),
    heapify(H, PD).

%modify_key
modify_key(H, NewKey, OldKey, V) :-
    heap_has_size(H, S),
    S == 1,
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)), !.

modify_key(H, NewKey, OldKey, V) :-
    heap_has_size(H, S),
    S > 1,
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    ordinamento_modify(H, P), !.

ordinamento_modify(H, P) :-
    heap_entry(H, P, _, _),
    NP is floor(P/2),
    NP < 1, !.

ordinamento_modify(H, P) :-
    heap_entry(H, P, K1, _),
    NP is floor(P/2),
    NP >= 1,
    heap_entry(H, NP, K2, _),
    K1 >= K2,
    heapify(H,P), !.

ordinamento_modify(H, P) :-
    heap_entry(H, P, K1, _),
    NP is floor(P/2),
    NP >= 1,
    heap_entry(H, NP, K2, _),
    K1 < K2 ,
    ordinamento_P(H, P), !.

%list_heap
list_heap(H) :-
    heap(H, _),
    listing(heap(H, _)), !,
    listing(heap_entry(H, _, _, _)).


%%---- IMPLEMENTAZIONE MST ----%%

%--- mst_prim
mst_prim(G, Source):-
    cancella_vk_vp(G),
    vertex(G, Source),
    new_heap(h1),
    create_vertex_key(G, h1),
    assert(vertex_previous(G, Source, null)),
    mod_vertex_key(G, h1, 0, Source),
    while_h(G, h1),
    delete_heap(h1), !.

%create_vertex_key
create_vertex_key(G, H) :-
    asseg_inf(G, H).

asseg_inf(G, H1):-
    graph_vertices(G, Vs),
    put_inf(G, H1, Vs).

put_inf(_G,_H1, []) :- !.

put_inf(G, H1, [vertex(G, V1) | Vs]) :-
     Infty is inf,
     heap_insert(H1, Infty, V1),
     assert(vertex_key(G, V1, Infty)),
     put_inf(G, H1, Vs).

%mod_vertex_key
mod_vertex_key(G, H, NK, Vertex) :-
    heap_entry(H, _, OK, Vertex),
    vertex_key(G, Vertex, OK),
    modify_key(H, NK, OK, Vertex),
    retract(vertex_key(G, Vertex, OK)),
    assert(vertex_key(G, Vertex, NK)).

%while_h
while_h(_G, H):-
    heap_empty(H), !.

while_h(G, H):-
    heap_extract(H, _, V),
    min_vertex_adj(H, V),
    while_h(G, H).

%min_vertex_adj
min_vertex_adj(H1, V):-
    vertex_key(G, V, _),
    vertex_neighbors(G, V, Ns),
    assign_min_key(H1, Ns).

assign_min_key(_H1, []) :- !.

assign_min_key(H1, [arc(G, V, VN, P) | Ls]) :-
    vertex_key(G, VN, PN),
    heap_entry(H1, _, _, VN),
    P < PN,
    mod_vertex_key(G, H1, P, VN),
    mod_vertex_prev(G, VN, V),
    assign_min_key(H1, Ls), ! .

assign_min_key(H1, [arc(G, _, VN, _) | Ls]) :-
    vertex_key(G, VN, _),
    assign_min_key(H1, Ls), ! .

assign_min_key(H1, [arc(G, _V, VN, P)|Ls]) :-
    vertex_key(G, VN, PN),
    P >= PN,
    assign_min_key(H1, Ls), !.

%mod_vertex_prev
mod_vertex_prev(G, Vertex, NV) :-
    vertex_previous(G, Vertex, OV),
    assert(vertex_previous(G, Vertex, NV)),
    retract(vertex_previous(G, Vertex, OV)), !.

mod_vertex_prev(G, Vertex, NV) :-
    assert(vertex_previous(G, Vertex, NV)).

%cancella_vk_vp
cancella_vk_vp(G) :-
    graph(G),
    retractall( vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)), !.

cancella_vk_vp(G) :-
    graph(G), !.

%---- mst_get
mst_get(G, Source, []):-
    vertex(G, Source),
    findall(V, vertex_previous(G, V, Source), []), !.

mst_get(G, Source, [arc(G, V1, V2, W) | PreorderTree]):-
    vertex(G, Source),
    findall(V, vertex_previous(G, V, Source), Vs),
    find_branches(G, Source, Vs, [arc(G, V1, V2, W)]),
    mst_get(G, V2, PreorderTree), !.

mst_get(G, Source, Preorder):-
    vertex(G, Source),
    findall(V, vertex_previous(G, V, Source), Vs),
    find_branches(G, Source, Vs, Arcs),
    sort(3, @<, Arcs, SortedVertexArcs),
    sort(4, @<, SortedVertexArcs, SortedWeightArcs),
    recursive_get(G, SortedWeightArcs, Preorder), !.

%find_branches
find_branches(_, _, [], []).

find_branches(G, Source, [V | Vs], [arc(G, Source, V, VK) | Arcs]) :-
    vertex_key(G, V, VK),
    find_branches(G, Source, Vs, Arcs), !.

%recursive_get
recursive_get(_G, [], []) :- !.

recursive_get(G, [arc(G, V1, V2, W) | Arcs], PreOrder) :-
    mst_get(G, V2, P1),
    recursive_get(G, Arcs, P2),
    append([arc(G, V1, V2, W) | P1], P2, PreOrder).










