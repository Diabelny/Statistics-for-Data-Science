# Definizione struttura di un potenziale nodo generico
Node <- function() {
  return(list(isLeaf = TRUE, feature = NULL, threshold = NULL, leftChild = NULL, rightChild = NULL, classCounts = numeric()))
}

#isLeaf: Campo booleano che se impostato a TRUE indica che questo nodo è inizialmente considerato una foglia.
#feature: Caratteristica (feature) utilizzata per dividere i dati. 
#threshold: Valore soglia utilizzato con la caratteristica per la divisione.
#leftChild e rightChild: Campi che rappresentano i figli sinistro e destro del nodo. Sono inizialmente impostati su NULL.
#classCounts: Un vettore numerico vuoto (numeric()) che conterrà le informazioni sulle conte del numero di campioni per ciascuna classe all'interno dei dati associati a questo nodo. (Nel nostro studio si assume che la variabile target sia binaria)

# Calcolo Hoeffding Bound(bound generale)
hoeffdingBound <- function(R, delta, n) {
  return(sqrt((R^2 * ln(1/delta)) / (2 * n)))
}

# Calcolo dell' Hoeffding Bound sfruttando le informazioni del Teorema 4 basandoci sulla metrica Gini Index(Teorema 2) usata negli esperimenti del paper
gini_hoeffdingBound <- function(n,delta){
  return(sqrt(8/n*ln(2/delta))+4*sqrt(1/n))  
}

# Calcolo Gini Index
giniIndex <- function(classCounts) {
  probs <- classCounts / sum(classCounts)
  giniIndex <- 1 - sum(probs^2)
  return(giniIndex)
}

# Funzione per determinare il miglior split basandosi sul Gini Index come metrica di impurità. Secondo lo studio del paper la funzione verrà chiamata due volte,per ogni tentativo di split, per calcolare i due attributi col maggior gain.
bestSplit <- function(node, x, y) {
  minGini <- Inf #+infinito
  bestFeature <- NULL
  bestThreshold <- NULL
  bestGain <- 0
  
  # Calcolo Gini Index nodo genitore
  parentGini <- calculateGiniIndex(node$classCounts)
  
  #Ciclo su tutti gli attributi e i relativi valori ordinandoli
 {
  leftCounts=0
  rightCounts=0
  # logica non implementata
    
  # alla fine del ciclo leftCounts conterrà il conteggio delle classi per i campioni che finiscono nel figlio sinistro dopo il potenziale split, stessa cosa per rightCounts.
  # questi conteggi verranno poi utilizzati per calcolare indice Gini nei figli sx e dx determinado quale split offre la MIGLIORE SEPARAZIONE DELLE CLASSI.
           
  # Calcolo Gini Index per figlio sinistro e destro e poi li devo pesare
  leftGini <- calculateGiniIndex(leftCounts)
  rightGini <- calculateGiniIndex(rightCounts)
      
  # Media pesata dei figli
  childrenGini <- (sum(leftCounts) / sum(node$classCounts)) * leftGini + (sum(rightCounts) / sum(node$classCounts)) * rightGini
      
  # Aggiornamento dei valori se l'indice di Gini pesato dei figli è minore del minimo calcolato fino ad ora
  if (childrenGini < minGini) {
    minGini <- childrenGini
    bestFeature <- feature
    bestThreshold <- threshold
    bestGain <- parentGini - childrenGini
    }
 } 
  # ritorno una lista contenenti importanti informazioni per determinare  nella funzione updateTree se effettuare lo split.
  return(list(feature = bestFeature, threshold = bestThreshold, gain=bestGain))
}


# Funzione che calcola l'altezza di un determinato nodo
calculateTreeHeight <- function(node) {
  if (node$isLeaf) {
    return(1)
  } else {
    return(1 + max(c(calculateTreeHeight(node$leftChild), calculateTreeHeight(node$rightChild))))
  }
}


# Funzione che aggiorna la struttura dell' albero
updateTree <- function(node, x, y, delta,tau,t) {
  
  # Calcolo Hoeffding bound generale
  R <- log(length(node$classCounts)) # calcola il logaritmo del numero di classi per determinare il range di entropia
  n <- sum(node$classCounts) # numero totale di campioni nel nodo
  altezza<- calculateTreeHeight(node) # calcolo altezza nodo corrente
  delta<-delta/((altezza+1)(altezza+2)*t*d*n)#t tempo, d=numero di attributi utilizzati per comparare i vari split
  bound <- hoeffdingBound(R, delta, n)
  
  # Calcolo Hoeffding bound secondo il Teorema 4 e 2
  gini_bound <- gini_hoeffdingBound(n,delta)
  
  # Ricerca dei due migliori split su cui poi effettuare la condizione
  first_best_split <- bestSplit(node, x, y)
  first_feature <- first_best_split$feature # miglior attributo 
  first_threshold <- first_best_split$threshold # valore relativo
  first_gain <- first_best_split$gain # gain relativo
  index_col_bestsplit=which(names(x)==first_feature) # indice della colonna risultata migliore dalla funzione bestSplit
  x_new=x[-index_col_bestsplit] # x_new in questo caso è il record senza la colonna restituita dalla prima chiamata della funzione bestSplit
  second_best_split <- bestSplit(node, x_new , y) 
  second_feature <- second_best_split$feature # secondo miglior attributo 
  second_threshold <- second_best_split$threshold # valore relativo
  second_gain <- second_best_split$gain # gain relativo
  
  
  # Se la seguente condizione è verificata deve essere eseguita la logica di split per "first_best_split" e relativa "threshold"
  if(first_gain > second_gain2 + 2*gini_bound ||gini_bound <= tau ){
    # per il TEOREMA 4, se epsilon(m,delta) è stato calcolato secondo le direttive dei Teoremi 1||2||3, nel nostro caso 2(Gini Index),
    # si ha la sicurezza che  la probabilità che un esempio casuale X 
    #sia instradato (routed) attraverso una suddivisione tau-subottimale sia al massimo  "delta".
  }
  
}

# Esempio di potenziale utilizzo dell'algoritmo
root <- Node() # creazione nodo foglia iniziale 
delta <- 0.05 

# Costruzione sequenziale dell'albero
col_target=ncol(datset)-1
for(i in 1:nrow(dataset)) {
  # la funzione è parametrizzata anche con tau che all'occorrenza può essere instanziato con un valore > 0
  # tau=viene utilizzato nella condizione della bestSplit per rappresentare la situazione in cui la differenza tra i due gain migliori
  # è talmente piccola che aspettare di processare nuovi esempi non portebbe alcun vantaggio.
  updateTree(root, dataset[i, -col_target], as.numeric(dataset[i, col_target]), delta,tau,i)
  # i=tempo=numero istanze che vengono processate=tempo t
}
