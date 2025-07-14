Afifa Saeed 835367


Questa libreria è stata implementata per la versione Swipl 8.0.3 e successive.

“Ghaph Algorithms: Minimum Spanning Trees”
La libreria comprende le implementazioni delle seguenti interfacce:
- creazione e manipolazione di grafi non orientati
- creazione e manipolazione di un MinHeap
- algoritmo PRIM per generare il Minimum Spanning Tree (MST) di un grafo non orientato.

Per testare il progetto il primo passo che bisogna fare è la creazione di un grafo, dopodiché
si può procedere con il predicato mst_prim, al quale si dovrà assegnare il nome del grafo 
che abbiamo precedutamene creato, e un vertice da cui si vuole partire per calcolare il 
percorso con peso minimo. Se il grafo non esiste o il vertice non esiste o  quest’ultimo non 
appartiene al grafo questo fallirà. 
Per ultimo si può procedere con mst_get, al quale viene assegnato il nome del grafo, e il 
vertice da cui si è partiti in mst_prim per ottenere la lista di archi ordinata.


** Interfaccia Grafi **
1) new_graph(G): aggiunge un nuovo grafo alla base di dati; se esiste già, risponde true.

2) delete_graph(G): rimuove il grafo G (se esiste) e tutti i suoi archi e vertici.

3) new_vertex(G,V): aggiunge un nuovo vertice; se già esiste, risponde true.

4) graph_vertices(G, Vs): vero quando Vs è una lista contenente tutti i vertici di G.

5) list_vertices(G): stampa alla console di Prolog una lista dei vertici del grafo G.

6) new_arc(G,U,V,W):  se il grafo esiste allora ci permette la creazione degli archi tra due vertici con peso maggiore di zero. Se un arco (U,V) esiste allora aggiornerà il peso dell’arco 
con quello appena inserito. In tal caso che non sia inserito un peso che colleghi due vertici 
il peso del arco sarà 1 per default garantendo così che sia un grafo connesso.

7) graph_arcs(G, Es): è vero se Es è una lista di tutti gli archi di G.

8) vertex_neightbors(G,V,Ns): restituisce True se Ns è una lista di tutti gli archi che portano 
a un vertice N da V. Per la sua implementazione abbiamo utilizzato due predicati di 
appoggio find_right e find_left. 

9) list_arcs(G): stampa una lista degli archi del grafo G.

10) list_graph(G): stampa una lista degli archi e vertici del grafo G.

11) read_graph(G,FileName) : legge un grafo G, da un file FileName e lo inserisce nel data 
base di Prolog. Per realizzare ciò abbiamo usato la libreria CSV, per specificare meglio, il 
predicato csv_read_file.

12) write_graph(G, FileName): questo predicato è vero quando G  viene scritto  sul file 
FileName secondo il valore dell'argomento Type. 
Se Type è graph, allora G è un grafo esistente. In FileName saranno iscritti gli archi del grafo.
Se Type è edges, allora G è una lista di archi, ognuno dei quali viene stampato su FileName.


** Interfaccia Heap **

1) new_heap(H): crea un nuovo heap se non è già presente nella base di dati con dimensione 
iniziale pari a 0.

2) delete_heap(H): elimina tutto il heap comprese tutte i heap_entry.

3) heap_has_size(H, S): è vero se S è la dimensione attuale dello heap.

4) heap_empty(H): è vero se heap non contiene elementi.

5) heap_not_empty(H): è vero se heap H contiene almeno un elemento.

6) heap_head(H, K, V): è vero quando l'elemento dello heap H con chiave minima K è V. 

7) heap_insert(H,K,V): è vero quando l'elemento V è inserito nello heap H con chiave K.
Quando viene inserito ogni elemento, questo viene ordinato con il predicato di appoggio 
ordinamento_P(H, P) e exchange(H, P, K1, V1, NP, K2, V2). 

8) heap_extract(H,K,V): è vero quando la coppia K,V con K minimo, è rimossa dallo heap H.
Quando viene rimosso la chiave K, allora il heap viene ordinato rispettando il heap-property
con il predicato heapify(H,P) e ordina_figli(H, P, K1, V1, PS, K2, V2, PD, K3, V3).

9) modify_key(H, NewKey, OldKey, V): è vero quando la chiave OldKey associata al valore V
è sostituita da NewKey. Dopo aver modificato il heap dobbiamo fare un riordinamento della
chiave usando il predicato di appoggio ordinamento_modify(H, P, K, V).

10) list_heap(H): stampa lo stato interno dello Heap.


 ** Implementazione MST PRIM **

1) vertex_key(G,V,K): è vero quando V è un vertice di G e contiene il peso minimo di un arco
che connette  a V. Se questo arco non esiste allora k è inf.

2) vertex_previous(G,V,U): è vero quando V ed U sono vertici di G e il vertice U è il "genitore"
di V. 

3) mst-prim(G,Source): Questa funzione trova il Minimum Spanning Tree per il grafo G in 
input e a partire dal vertice source.  L’algoritmo termina restituendo true. 
Usa predicati di appoggio come
- create_vertex_key(G, H): chiama il predicato asseg_inf.
- asseg_inf(G,H): cerca la lista di vertici e chiama il predicato put_inf.
- put_inf(G,H, L): inserisce una heap_entry per ogni vertice con chiave al infinito e aggiunge 
  una vertex_key ad ogni vertice. 
- min_vertex_adj(H1, V): cerca i vertici che raggiungono V e chiama assign_min_key.
- assign_min_key(H1, Ns): aggiornerà il vertex-key dei vertici che raggiungono V e generarà
 una lista di questi.
- mod_vertex_key(H, NK, Vertex): ha il compito di aggiornare il vertex_key del vertice 
Vertex e di cambiare la chiave di quest'ultima nel Heap.
- mod_vertex_prev(Vertex, NV): ha il compito di aggiornare il vertex_previous del vertice 
Vertex.
- while_h(G, H): richiama i predicati heap_extract(H,K,V) e min_vertex_adj(H, V) fino ad avere il 
 heap H vuoto. 
- cancella_vk_vp(G): cancella tutti i vertex_key e vertex_previous del grafo G.

4) mst_get(G,Source,PreorderTree):  
Ha come input il nome del grafo e un source su cui è stato precedentemente eseguito 
l'algoritmo di Prim. Ritorna la lista degli archi del MST ordinata seguendo una visita preorder 
con precedenza all'arco con minor peso. In caso di più archi con peso uguale, si procede con 
l'ordine lessicografico del vertice target.
Inoltre abbiamo usato dei predicati di appoggio
- find_branches(G,Source,Vs,Arcs): genera una lista di archi. Questi archi sono generati usando
l'informazione del vertex_key e vertex_previous di Source. 
- recursive_get(G, SortedWeightArcs, Preorder): Genera una lista di archi ordinati.
