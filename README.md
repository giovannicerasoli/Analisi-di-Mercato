# Progettazione di un nuovo servizio di Fidelity Card per profumerie

## Descrizione del progetto

Questo progetto è stato realizzato per il corso di **Analisi di Mercato Quantitative** dell’Università degli Studi di Milano-Bicocca, anno accademico 2025/2026.

L’obiettivo principale dello studio è progettare un nuovo servizio di **Fidelity Card per profumerie**, utilizzando tecniche quantitative di analisi di mercato per comprendere le preferenze dei consumatori, segmentare la clientela e proporre azioni commerciali mirate.

## Obiettivo

Il progetto mira a individuare quali caratteristiche di una Fidelity Card risultano maggiormente apprezzate dai consumatori e a definire proposte differenziate per specifici segmenti di clientela.

Lo studio si sviluppa attraverso quattro fasi principali:

1. Analisi descrittiva del campione
2. Segmentazione della clientela
3. Progettazione di Fidelity Card per i segmenti individuati
4. Definizione di potenziali azioni commerciali

## Dataset

Il dataset utilizzato è composto da:

- **123 osservazioni**
- **36 variabili**

Le variabili considerate riguardano diverse dimensioni del consumatore:

- Variabili demografiche
- Variabili psicografiche
- Comportamenti d’acquisto
- Spese recenti
- Valutazione di diverse proposte di Fidelity Card
- Importanza attribuita alle caratteristiche di una profumeria

## Variabili analizzate

Le principali variabili considerate nello studio includono:

### Variabili demografiche

- Età
- Genere

### Variabili psicografiche

- Praticità negli acquisti
- Importanza della tradizione
- Propensione a distinguersi dagli altri
- Dinamicità
- Socievolezza
- Sensibilità alla qualità
- Influenza della pubblicità
- Reazione ai cambiamenti

### Comportamenti d’acquisto

- Luogo abituale di acquisto
- Frequenza di acquisto
- Spesa media
- Tendenza ad acquistare prodotti non preventivati

### Acquisti recenti

- Profumi
- Cura corpo e viso
- Trucco
- Prodotti per abbronzatura

## Metodologia

L’analisi è stata condotta attraverso due principali tecniche quantitative:

### Analisi Conjoint

L’analisi conjoint è stata utilizzata per stimare le utilità individuali associate alle diverse caratteristiche delle Fidelity Card.

Le caratteristiche considerate sono:

- Premio
- Animazione gratuita
- Convenzione
- Sconto

Il metodo adottato è di tipo **Full Profile**, in cui ogni proposta di Fidelity Card presenta una combinazione completa degli attributi.

La stima delle utilità è stata effettuata tramite un modello additivo **Part-Worth**, utilizzando una regressione multipla di tipo **OLS**.

### Analisi Cluster

L’analisi cluster è stata utilizzata per segmentare il campione sulla base delle utilità individuali ottenute tramite l’analisi conjoint.

La segmentazione è stata effettuata utilizzando:

- Distanza euclidea
- Metodo di aggregazione Ward
- Numero finale di cluster: 4

## Segmenti individuati

Dall’analisi sono stati individuati quattro principali segmenti di clientela.

### Cluster 1: Tradizionali e cauti

Segmento composto da consumatori più tradizionali, con una frequenza di acquisto moderata e una preferenza per soluzioni semplici e rassicuranti.

Fidelity Card ideale:

- Premio: Beauty farm
- Animazione: no feste a tema
- Convenzione: nessuna preferenza marcata
- Sconto: fisso

Azioni commerciali suggerite:

- Sconti fissi per i possessori della Fidelity Card
- Buoni da utilizzare entro 30 giorni
- Giornate benessere in profumeria
- Partnership con beauty farm locali

### Cluster 2: Donne beauty fedeli ed impulsive

Segmento composto prevalentemente da donne, con maggiore coinvolgimento nel mondo beauty e tendenza ad acquistare anche prodotti non preventivati.

Fidelity Card ideale:

- Premio: Beauty farm
- Animazione: rivista
- Convenzione: centro estetico
- Sconto: promozionale

Azioni commerciali suggerite:

- Sconto all’attivazione della Fidelity Card
- Promozioni valide in giornate specifiche
- Promo combinate tra prodotto abituale e nuovo prodotto
- Newsletter beauty
- Partnership con beauty farm e centri estetici
- Sponsorizzazioni tramite influencer femminili credibili nel mondo skincare e make-up

### Cluster 3: Uomini pragmatici orientati alla qualità

Segmento composto prevalentemente da uomini, orientati alla qualità e con comportamenti d’acquisto più pragmatici.

Fidelity Card ideale:

- Premio: nessuna preferenza marcata
- Animazione: feste a tema
- Convenzione: palestra
- Sconto: nessuna preferenza marcata

Azioni commerciali suggerite:

- Sconto all’attivazione della Fidelity Card
- Bundle promozionali, ad esempio kit per la barba
- Reparto uomo più ampio
- Prodotti specifici per sportivi
- Fidelity Card attivabile online
- Partnership con palestre e locali in voga

### Cluster 4: Dinamici occasionali

Segmento composto da consumatori dinamici, occasionali e maggiormente attratti da esperienze avventurose.

Fidelity Card ideale:

- Premio: avventura
- Animazione: no rivista
- Convenzione: nessuna preferenza marcata
- Sconto: nessuna preferenza marcata

Azioni commerciali suggerite:

- Sconto all’attivazione della Fidelity Card
- Maggiori probabilità di vincita all’aumentare degli acquisti
- Eventi outdoor
- Partnership con negozi di abbigliamento tecnico
- Premi esperienziali, come snorkeling o lancio con il paracadute

## Segmento di maggiore interesse

Il segmento ritenuto più interessante è il **Cluster 2: Donne beauty fedeli ed impulsive**.

Questo segmento presenta caratteristiche particolarmente rilevanti per una profumeria:

- Acquista con maggiore frequenza
- Frequenta poche profumerie fisse
- È più propenso ad acquistare prodotti non preventivati
- Mostra interesse per servizi vicini al mondo beauty, come beauty farm e centri estetici

Per questi motivi, rappresenta un target commerciale potenzialmente molto attrattivo.

## Risultati principali

L’analisi ha evidenziato che, a livello complessivo, gli elementi più importanti nella valutazione di una Fidelity Card sono:

1. Premio
2. Convenzione
3. Animazione
4. Sconto

La Fidelity Card ideale per il campione complessivo include:

- Vacanza avventurosa
- Feste a tema
- Palestra
- Sconto di qualsiasi tipo

Tuttavia, l’analisi cluster mostra che le preferenze cambiano in modo significativo tra i diversi segmenti, rendendo utile una strategia commerciale differenziata.

## Conclusioni

Il progetto mostra come l’utilizzo combinato di analisi conjoint e analisi cluster possa supportare la progettazione di un servizio di Fidelity Card più coerente con le preferenze dei consumatori.

La segmentazione consente di passare da una proposta generica a una strategia mirata, costruendo offerte diverse in base ai bisogni, ai comportamenti e alle preferenze dei clienti.

In particolare, il Cluster 2 rappresenta il segmento più promettente per lo sviluppo commerciale, grazie al maggiore coinvolgimento nel mondo beauty, alla fedeltà verso specifiche profumerie e alla maggiore propensione agli acquisti impulsivi.

## Autori

- Giovanni Giacomo Cerasoli
- Federico Perina

## Corso

Analisi di Mercato Quantitative  
Università degli Studi di Milano-Bicocca  
Anno Accademico 2025/2026

