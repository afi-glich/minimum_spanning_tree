Afifa Saeed 835367

Questa libreria è stata implementata per la versione LispWorks 7.1.2 e successive.

La libreria comprende le implementazioni delle seguenti interfacce:
- creazione e manipolazione di grafi non orientati
- creazione e manipolazione di un MinHeap
- algoritmo PRIM per generare il Minimum Spanning Tree (MST) di un grafo non orientato.

** Interfaccia Grafi **

1) is-graph(graph-id) -> boolean
Ha come input un nome di grafo valido: una s-expr non nulla, e ritorna graph-id se grafo esiste. NIL nel caso non esista il grafo. Genera un errore se graph-id è null.

2) new-graph(graph-id) -> graph-id
Prende in input un graph-id non nullo e genera errore se questa condizione non è soddisfatta. Ritorna graph-id, se ha successo. NIL altrimenti.

3) delete-graph(graph-id) -> NIL
Ha come input un graph-id non nullo.
Rimuove il grafo, e relativi vertici, archi, eventuali vertex-key, vertex-previous calcolati da Prim e ritorna NIL. 
Genera un errore se grafo non esiste, oppure graph-id non è valido.

4) new-vertex(graph-id vertex-id) -> vertex-rep
Ha come input un graph-id non nullo e un vertex-id che sia un simbolo o un numero.
Aggiunge, se non esiste, il vertice vertex-id al grafo graph-id e lo ritorna.
Genera errore se vertex-id non è un simbolo o non numero oppure se graph-id non è valido oppure non è un grafo esistente.

5) graph-vertices(graph-id) -> vertex-rep-list
Ha come input un graph-id non nullo. 
Ritorna una lista di tutti i vertici del grafo graph-id. 
Genera errore se graph-id non valido oppure non esiste quel grafo oppure graph-id non è un nome valido.

6) new-arc (graph-id vertex-id1 vertex-id2 &optional weight) -> arc-rep
Ha come input un graph-id non nullo, vertex-id1 e vertex-id2 entrambi simboli o numeri e un peso > 0 (opzionale; 1 di defualt).
Se graph-id, vertex-id1 e vertex-id2 non sono già definiti, li crea prima di creare l'arco tra i due vertici. 
Genera errore se graph-id, vertex-id1 e vertex-id2 non sono nomi validi. Genera errore se peso <= 0.

6) graph-arcs(graph-id) -> arc-rep-list
Ha come input un graph-id non nullo.
Ritorna una lista di tutti gli archi del grafo graph-id.
Genera errore se graph-id non è valido. Genera errore se il grafo non esiste.

7) graph-vertex-neighbors (graph-id vertex-id) -> arc-rep-list
Ha come input un graph-id non nullo e un vertex-id simbolo o un numero. 
Ritorna una lista con tutti gli archi che partano dal vertice vertex-id nel grafo graph-id. Ritorna NIL se non vi sono archi connessi a vertex-id oppure se graph-id o vertex-id non esistono. 
Genera errore se graph-id e/o vertex-id sono non validi.

8) graph-vertex-adjacent (graph-id vertex-id): 
Ha come input un graph-id non nullo e un vertex-id simbolo o un numero. 
Ritorna una lista con tutti i vertici adiacenti a vertex-id nel grafo graph-id. Ritorna NIL se graph-id o vertex-id non esistono. 
Genera errore se graph-id e/o vertex-id sono non validi.

9) graph-print (graph-id)
Ha come input un graph-id non nullo. 
Stampa tutti i vertici e tutti gli archi del grafo graph-id. Ritorna NIL se graph-id non appartiene a un grafo esistente.
Genera errore se graph-id non è valido, oppure se grafo non esiste.

** Interfaccia Heap **
1) new-heap (heap-id &optional (capacity 42)) -> heap-rep 
Ha come input un heap-id che è un simbolo e capacity che rappresenta la capacità dello heap.
Genera errore se heap-id non è un valore valido.

2) heap-id(heap-rep) -> heap-id
Ritorna l’id del heap heap-rep passato come argomento. 
Genera errore se l'argomento è nil.

3) Heap-size(heap-rep) -> heap-size
Ritorna il numero di elementi dello heap-rep passato come argomento.
Genera errore se l'argomento è nil.

4) heap-actual-heap(heap-rep) -> heap-actual-heap
Ritorna l’array contenente le coppie (Key Value) del heap.
Genera errore se l'argomento è nil.

5) heap-delete(heap-id) -> boolean
Rimuove lo heap indicato con il nome heap-id e ritorna true. Restitusce NIL se heap-id non esiste.
Genera un errore se heap-id non è un simbolo. 

6) heap-empty(heap-id) -> boolean
Ritorna T se heap ha 0 elementi. NIL altrimenti. 
Genera errore se heap-id non è un heap già definito.

7) heap-not-empty(heap-id) -> boolean
Come heap-empty, genera errore se heap-id non è valido. Ritorna T se heap non è vuoto; NIL altrimenti. 

8) heap-head(heap-id) -> (K V)
Restituisce la coppia (K V), associato alla chiave mminima.
Genera errore se heap-id non è un simbolo.

9) heap-insert (heap-id K V) -> boolean
Aggiungere la coppia (K V) allo heap e ritorna T se la coppia (K V) viene aggiunta; NIL altrimenti.
Genera errore se heap-id non è un simbolo. 

10) heap-extract (heap-id) -> (K V)
Rimuove l’elemento in testa allo heap (heap-head) e lo ritorna. Heap-extract genera errore se heap-id non è un simbolo.

11) heap-print (heap-id)
Stampa lo stato interno dello heap. Stampa il numero di elementi nello heap e tutte le coppie (K V).
Genera errore se heap-id non è un simbolo oppure se non esiste alcun heap con quel nome.


** Implementazione MST PRIM **

1) mst-vertex-key (graph-id vertex-id) -> k
Ritorna il peso minimo associato al vertex-id per il grafo graph-id. 
Ritorna most-positive-double-float (infinito) se il vertice non è presente nel MST. 
Ritorna invece NIL se graph-id e/o vertex-id non sono validi.
Genera errore se graph-id è nullo e/o vertex-id non è un simbolo o un numero.

2) mst-previous(graph-id vertex-id) -> u
Ritorna il vertice che è il genitore del vertice vertex-id nel MST. Ritorna NIL se vertex-id non è presente nel MST.
Genera errore se graph-id è nullo e/o vertex-id non è un simbolo o un numero.

3) mst-prim (graph-id source) -> NIL
Questa funzione trova il Minimum Spanning Tree per il grafo graph-id in input e a partire dal vertice soource. L’algoritmo termina restituendo NIL. 
Genera errore se graph-id o vertex-id non sono validi. 

4) mst-get(graph-id source) -> preorder-mst 
Ha come input un graph-id e un source su cui è stato precedemente eseguito l'algoritmo di Prim.
Ritorna la lista degli archi del MST ordinata seguendo una visita preorder con precedenza all'arco con minor peso. In caso di più archi con peso uguale, si procede con l'orgine lessicografico del vertice target.
Genera errore se graph-id è null e se source non è un simbolo o un numero.








