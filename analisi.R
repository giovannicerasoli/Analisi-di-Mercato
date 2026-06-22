# -----------------------------------------------------------------------------#
# Librerie
# -----------------------------------------------------------------------------#
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(haven)
library(psych)
# library(writexl)


# -----------------------------------------------------------------------------#
# Importazione dati
# -----------------------------------------------------------------------------#
dati = read.xlsx("Dati_Fidelity2026_pulito.xlsx")


# -----------------------------------------------------------------------------#
# Pulizia dataset
# -----------------------------------------------------------------------------#
# trasformo la data di nascita in formato date
dati$Data_di_nascita = as.Date(dati$Data_di_nascita, origin = "1899-12-30")

# creo una colonna con l'età
oggi = Sys.Date()
dati$eta = as.integer(format(oggi, "%Y")) - as.integer(format(dati$Data_di_nascita, "%Y"))
# Correzione per chi non ha ancora fatto il compleanno quest’anno
dati$eta = dati$eta - ifelse(format(oggi, "%m-%d") < format(dati$Data_di_nascita, "%m-%d"), 1, 0)

# controllo tutte le variabili: ingressi ed eventuali NA
summary(dati$eta)
hist(dati$eta, breaks = 5, main = "Distribuzione dell'età", xlab = "Età", ylab = "Frequenza")

table(dati$Luogo_di_acquisto, useNA = "always")
dati$Luogo_di_acquisto[dati$Luogo_di_acquisto == "li acquisto online"] = "online"
dati$Luogo_di_acquisto[dati$Luogo_di_acquisto == "li acquista dove capita, ipermercati, ingrosso, etc, online "] = "online"
dati$Luogo_di_acquisto[dati$Luogo_di_acquisto == "li acquista in alcune profumerie fisse, li acquista dove capita, ipermercati, ingrosso, etc, Farmacia"] = "li acquista dove capita, ipermercati, ingrosso, etc"

table(dati$Frequenza_di_acquisto, useNA = "always")
dati$Frequenza_di_acquisto[dati$Frequenza_di_acquisto == "ancora pi√π raramente"] = "ancora più raramente"
dati$Frequenza_di_acquisto[dati$Frequenza_di_acquisto == "ancora più raramente, mai"] = "ancora più raramente"

table(dati$Spesa_media, useNA = "always")
dati$Spesa_media[dati$Spesa_media == "10 euro "] = "10 euro"
dati$Spesa_media[dati$Spesa_media == "Meno di 20 euro"] = "10 euro"
dati$Spesa_media[dati$Spesa_media == "20 euro, 40 euro"] = "30 euro"
dati$Spesa_media[dati$Spesa_media == "20 euro, meno di 20 euro"] = "20 euro"
dati$Spesa_media[dati$Spesa_media == "40 euro, Oltre i 50 euro"] = "Oltre i 50 euro"

table(dati$Acquisto_recente_trucco)
table(dati$Acquisto_recente_cura_corpo_viso)
table(dati$Acquisto_recente_profumi)
table(dati$Acquisto_recente_abbronzatura)

prop.table(table(dati$Acquisto_recente_trucco)) * 100
prop.table(table(dati$Acquisto_recente_cura_corpo_viso)) * 100
prop.table(table(dati$Acquisto_recente_profumi)) * 100
prop.table(table(dati$Acquisto_recente_abbronzatura)) * 100

table(dati$In_genere_compra, useNA = "always")
dati$In_genere_compra[dati$In_genere_compra == "altri prodotti in pi√π, non preventivati"] = "altri prodotti in più, non preventivati"

library(ggplot2)
library(patchwork)

crea_df = function(x) {
  df = as.data.frame(prop.table(table(x)))
  names(df) = c("categoria", "freq")
  df
}

df1 = crea_df(dati$Luogo_di_acquisto)
df2 = crea_df(dati$Frequenza_di_acquisto)
df3 = crea_df(dati$Spesa_media)
df4 = crea_df(dati$In_genere_compra)

limite = max(df1$freq, df2$freq, df3$freq, df4$freq)

grafico_barre = function(df, titolo, colori) {
  ggplot(df, aes(x = reorder(categoria, freq), y = freq, fill = categoria)) +
    geom_col(show.legend = FALSE, width = 0.5) +
    geom_text(aes(label = paste0(round(freq * 100, 1), "%")), hjust = -0.1, size = 4.5) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, limite),
      labels = function(x) paste0(x * 100, "%"),
      expand = expansion(mult = c(0, 0.12))
    ) +
    scale_fill_manual(values = colori) +
    labs(title = titolo, x = NULL, y = NULL) +
    theme_minimal(base_size = 14)
}

g1 = grafico_barre(df1, "Distribuzione del luogo di acquisto",
                   c("#08306b", "#08519c", "#4292c6", "#6baed6", "#9ecae1"))

g2 = grafico_barre(df2, "Distribuzione della frequenza di acquisto",
                   c("darkgreen", "darkolivegreen3", "chartreuse2", "darkolivegreen1"))

g3 = grafico_barre(df3, "Distribuzione della spesa media",
                   c("coral", "deeppink", "deeppink3", "deeppink4", "darkmagenta", "darkorchid4"))

g4 = grafico_barre(df4, "Distribuzione 'in genere compra'",
                   c("darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4"))

(g1 + g2) / (g4 + g3)

table(dati$Cortesia_del_personale, useNA = "always")
table(dati$Interpretazione_delle_esigenze_del_cliente, useNA = "always")
table(dati$Segnalazione_delle_promozioni, useNA = "always")
table(dati$Consigli_di_utilizzo_dei_prodotti, useNA = "always")
table(dati$Prezzi_praticati, useNA = "always")
table(dati$Sconto_praticato, useNA = "always")
table(dati$Campioncini_di_prova_regalati, useNA = "always")

mean(dati$Cortesia_del_personale, useNA = "always")
mean(dati$Interpretazione_delle_esigenze_del_cliente, useNA = "always")
mean(dati$Segnalazione_delle_promozioni, useNA = "always")
mean(dati$Consigli_di_utilizzo_dei_prodotti, useNA = "always")
mean(dati$Prezzi_praticati, useNA = "always")
mean(dati$Sconto_praticato, useNA = "always")
mean(dati$Campioncini_di_prova_regalati, useNA = "always")

median(dati$Cortesia_del_personale, useNA = "always")
median(dati$Interpretazione_delle_esigenze_del_cliente, useNA = "always")
median(dati$Segnalazione_delle_promozioni, useNA = "always")
median(dati$Consigli_di_utilizzo_dei_prodotti, useNA = "always")
median(dati$Prezzi_praticati, useNA = "always")
median(dati$Sconto_praticato, useNA = "always")
median(dati$Campioncini_di_prova_regalati, useNA = "always")

table(dati$Sono_una_persona_dinamica_con_tanti_interessi, useNA = "always")
table(dati$`La_pubblicità_che_vedo_influenza_le_mie_scelte_d'acquisto`, useNA = "always")
table(dati$I_cambiamenti_nella_vita_quotidiana_mi_innervosiscono, useNA = "always")
table(dati$Sono_socievole_mi_piace_conoscere_sempre_nuove_persone, useNA = "always")
table(dati$Per_i_miei_acquisti_bado_più_alla_praticità_che_a_tutto_il_resto, useNA = "always")
table(dati$I_valori_della_tradizione_sono_molto_importanti_per_me, useNA = "always")
table(dati$Mi_piace_invitare_gli_amici_a_casa, useNA = "always")
table(dati$`Amo_stare_all'aria_aperta`, useNA = "always")
table(dati$Sono_pronto_a_spendere_di_più_per_avere_la_qualità_elevata, useNA = "always")
table(dati$Mi_piace_distinguermi_dagli_altri, useNA = "always")

prop.table(table(dati$Sono_una_persona_dinamica_con_tanti_interessi, useNA = "always")) * 100
prop.table(table(dati$`La_pubblicità_che_vedo_influenza_le_mie_scelte_d'acquisto`, useNA = "always")) * 100
prop.table(table(dati$I_cambiamenti_nella_vita_quotidiana_mi_innervosiscono, useNA = "always")) * 100
prop.table(table(dati$Sono_socievole_mi_piace_conoscere_sempre_nuove_persone, useNA = "always")) * 100
prop.table(table(dati$Per_i_miei_acquisti_bado_più_alla_praticità_che_a_tutto_il_resto, useNA = "always")) * 100
prop.table(table(dati$I_valori_della_tradizione_sono_molto_importanti_per_me, useNA = "always")) * 100
prop.table(table(dati$Mi_piace_invitare_gli_amici_a_casa, useNA = "always")) * 100
prop.table(table(dati$`Amo_stare_all'aria_aperta`, useNA = "always")) * 100
prop.table(table(dati$Sono_pronto_a_spendere_di_più_per_avere_la_qualità_elevata, useNA = "always")) * 100
prop.table(table(dati$Mi_piace_distinguermi_dagli_altri, useNA = "always")) * 100

table(dati$Fidelity_Card_1, useNA = "always")
table(dati$Fidelity_Card_2, useNA = "always")
table(dati$Fidelity_Card_3, useNA = "always")
table(dati$Fidelity_Card_4, useNA = "always")
table(dati$Fidelity_Card_5, useNA = "always")
table(dati$Fidelity_Card_6, useNA = "always")
table(dati$Fidelity_Card_7, useNA = "always")
table(dati$Fidelity_Card_8, useNA = "always")
table(dati$Fidelity_Card_9, useNA = "always")

mean(dati$Fidelity_Card_1)
mean(dati$Fidelity_Card_2)
mean(dati$Fidelity_Card_3)
mean(dati$Fidelity_Card_4)
mean(dati$Fidelity_Card_5)
mean(dati$Fidelity_Card_6)
mean(dati$Fidelity_Card_7)
mean(dati$Fidelity_Card_8)
mean(dati$Fidelity_Card_9)

table(dati$Genere, useNA = "always")
# calcolami il genere in percentuale
prop.table(table(dati$Genere, useNA = "always")) * 100


# -----------------------------------------------------------------------------#
# Segmentazione preferenze dei consumatori
# -----------------------------------------------------------------------------#
Subset = dati[, 10:16]

# matrice di correlazione
correlation_matrix = cor(Subset, method = "pearson", use = "complete.obs")
corrplot::corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.6,
  tl.cex = 0.8,
  diag = FALSE
)

# Test di significatività delle correlazioni
# 1. Bartlett test sulla matrice di correlazione
n_obs = nrow(Subset)
bartlett_test = cortest.bartlett(correlation_matrix, n = n_obs)
print(bartlett_test)

# 2. KMO test
kmo_result = KMO(Subset)
print(kmo_result$MSA)
# per maggiore interpretabilità non si effettua PCA

# PCA
X = scale(Subset)
pca_result = princomp(Subset, cor = TRUE)
# Estraggo gli autovalori
eigenvalues = pca_result$sdev^2  # gli autovalori sono la varianza delle componenti
# Tabella di varianza spiegata
# Supponiamo che 'eigenvalues' sia un vettore contenente gli autovalori
eigenvalue_table = data.frame(
  Component = paste0("PC", 1:length(eigenvalues)), # Nome delle componenti (PC1, PC2, ...)
  Eigenvalue = eigenvalues, # Autovalori
  Variance_Explained = eigenvalues / sum(eigenvalues) * 100, # % varianza spiegata
  Cumulative_Variance = cumsum(eigenvalues / sum(eigenvalues)) * 100 # % cumulata
)
# Visualizza la tabella
print(eigenvalue_table)

# Come interpretare gli assi?
# Calcolo le correlazioni tra le variabili originali e le componenti principali
pc_score = as.data.frame(pca_result$scores)
# Matrice di correlazione tra le variabili
cor_var_pcs = cor(Subset, pc_score[,1:5])
cor_var_pcs = as.data.frame(cor_var_pcs)
# Visualizzare la tabella
print(cor_var_pcs)

### evidenzio il valore massimo per ogni variabile -----------------------------
mat = cor_var_pcs

highlight = t(apply(mat, 1, function(x){
  max_idx = which.max(abs(x))
  out = sprintf("%.3f", x)
  out[max_idx] = paste0("**", out[max_idx], "**")
  return(out)
}))

highlight = as.data.frame(highlight)
colnames(highlight) = colnames(mat)

highlight

#Rotazione degli assi  
# Pesi della PCA
loadings = pca_result$loadings

# Applico la rotazione (es.varimax) ai pesi
rotation_result = varimax(loadings[,1:5],normalize = TRUE)
# la rotazione e' calcolata sul numero di assi scelti 

# Trovo gli autovettori ruotati
rotated_loadings = rotation_result$loadings

# Calcolo i nuovi score ruotati
df2 = scale(Subset, center = TRUE, scale = TRUE)
rotated_scores = as.data.frame(as.matrix(df2) %*% rotated_loadings)

# Matrice di correlazione tra le componenti ruotate e le variabili originali
cor_var_pcs2 = cor(Subset, rotated_scores[,1:5])
cor_var_pcs2 = as.data.frame(cor_var_pcs2)
# Visualizzare la tabella
print(cor_var_pcs2)

### evidenzio il valore massimo per ogni variabile -----------------------------
mat = cor_var_pcs2

highlight2 = t(apply(mat, 1, function(x){
  max_idx = which.max(abs(x))
  out = sprintf("%.3f", x)
  out[max_idx] = paste0("**", out[max_idx], "**")
  return(out)
}))

highlight2 = as.data.frame(highlight2)
colnames(highlight2) = colnames(mat)

highlight2

# Dati con fattori ruotati
data_final = cbind(dati, rotated_scores)

# -----------------------------------------------------------------------------#
# Cluster analysis sulle preferenze
# -----------------------------------------------------------------------------#
# Clustering: Metodo di Ward e distanza euclidea
data_clu = data_final[, 39:43] 
d = dist(data_clu, method ="euclidean")
# Link di Ward
hc = hclust(d, method = "ward.D2")

# Dendrogramma
plot(hc, main = "Dendrogramma con link di Ward (dist. Euclidea)",
     xlab = "Osservazioni", ylab = "Distanza")
rect.hclust(hc, k = 6, border = "red")

library("NbClust")
best_ch=NbClust(data_clu, distance = "euclidean", min.nc=2, max.nc=8, method = "ward.D2", index = "ch")
best_d=NbClust(data_clu, distance = "euclidean", min.nc=2, max.nc=8, method = "ward.D2", index = "db")
best_s=NbClust(data_clu, distance = "euclidean", min.nc=2, max.nc=8, method = "ward.D2", index = "silhouette")
# Taglio il dendrogramma per ottenere 4 cluster
clusters = cutree(hc, k = 6)

# aggiungo i cluster al dataset
data_final$Clu = clusters

table(data_final$Clu)


# -----------------------------------------------------------------------------#
# Caratterizzazione 
# -----------------------------------------------------------------------------#
# variabili usate per i cluster
set_var = c(names(data_final)[10:16])

# Calcola statistiche globali 
media_totale = colMeans(data_final[, 10:16], na.rm = TRUE)
varianza_totale = apply(data_final[, 10:16], 2, var, na.rm = TRUE)
numerosita_totale = nrow(data_final)

# Funzione per calcolare le statistiche t
calcola_statistiche_t = function(data, var_name, media_tot, var_tot, n_tot) {
  media_cluster = mean(data[[var_name]], na.rm = TRUE)
  var_cluster = var(data[[var_name]], na.rm = TRUE)
  n_cluster = sum(!is.na(data[[var_name]]))
  
  sp = ((n_cluster - 1) * var_cluster + (n_tot - 1) * var_tot) / (n_cluster + n_tot - 2)
  t_value = (media_cluster - media_tot) / sqrt(sp * (1/n_cluster + 1/n_tot))
  
  return(list(media = media_cluster, sp = sp, t = t_value))
}

# Calcolo media, varianza e t-statistic di ogni variabile per cluster
statistiche_cluster_t = data_final %>%
  group_by(Clu) %>%
  group_modify(~ {
    res = data.frame(n_cluster = nrow(.x))
    for (var in set_var) { # calcola per ogni variabile
      stats = calcola_statistiche_t(.x, var, media_totale[var], varianza_totale[var], numerosita_totale)
      res[paste0("media_", var, "_cluster")] = stats$media
      res[paste0("sp_", var)] = stats$sp
      res[paste0("t_", var)] = stats$t
    }
    
    return(res)
  }) %>%
  ungroup()


# Visualizza i risultati
# Seleziona le colonne delle medie dalle statistiche
tabella_medie = statistiche_cluster_t %>%
  select(Clu, n_cluster, starts_with("media_"))
# Rinomina le colonne per chiarezza
nomi_media = names(tabella_medie)[startsWith(names(tabella_medie), "media_")]
nomi_variabili_media = gsub("media_", "", nomi_media)
names(tabella_medie)[startsWith(names(tabella_medie), "media_")] = paste0("media_", nomi_variabili_media)

print(t(tabella_medie))

# write.xlsx(tabella_medie, "C:/Users/fedep/OneDrive/Documenti/Unimib/analisiDiMercatoQuant/progettoEsame/Tabella_medie.xlsx")


# Seleziona le colonne delle statistiche t dalle statistiche
tabella_t = statistiche_cluster_t %>%
  select(Clu, n_cluster, starts_with("t_"))
# Rinomina le colonne per chiarezza
nomi_t = names(tabella_t)[startsWith(names(tabella_t), "t_")]
nomi_variabili_t = gsub("t_", "", nomi_t)
names(tabella_t)[startsWith(names(tabella_t), "t_")] = paste0("t_", nomi_variabili_t)

print(t(tabella_t))

# write.xlsx(tabella_t, "C:/Users/fedep/OneDrive/Documenti/Unimib/analisiDiMercatoQuant/progettoEsame/Tabella_t.xlsx")

# -----------------------------------------------------------------------------#
# eta
# Calcola le statistiche globali
media_eta_totale = mean(data_final$eta, na.rm = TRUE)
varianza_eta_totale = var(data_final$eta, na.rm = TRUE)
numerosita_eta_totale = length(na.omit(data_final$eta))

# Calcola sp e t per ciascun cluster
statistiche_eta_cluster = data_final %>%
  group_by(Clu) %>%
  summarize(
    n_cluster = n(),
    
    media_eta_cluster = mean(eta, na.rm = TRUE),
    
    varianza_eta_cluster = var(eta, na.rm = TRUE),
    
    sp_eta = ((n_cluster - 1) * varianza_eta_cluster + 
                (numerosita_eta_totale - 1) * varianza_eta_totale) / 
      (n_cluster + numerosita_eta_totale - 2),
    
    t_eta = (media_eta_cluster - media_eta_totale) / 
      sqrt(sp_eta * (1/n_cluster + 1/numerosita_eta_totale))
  )

print(statistiche_eta_cluster)

# -----------------------------------------------------------------------------#
# Genere
# Distribuzione 
# Crea la tabella di contingenza
tabella_genere_cluster = table(data_final$Genere, data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_genere_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_genere_cluster)/totale_campione * 100

# Aggiungo colonna con percentuali riga
tabella_percentuale_totale= cbind(percentuali_colonna, 
                                  'Percentuale Genere'= percentuali_riga)
print(tabella_percentuale_totale)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato = chisq.test(tabella_genere_cluster)
print(test_chi_quadrato)

# -----------------------------------------------------------------------------#
# Luogo_di_acquisto
# Distribuzione 
# Crea la tabella di contingenza
tabella_luogo_acquisto_cluster = table(as_factor(data_final$Luogo_di_acquisto), 
                                       data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_luogo_acquisto_cluster, 
                                 margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_luogo_acquisto_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_luogo_acquisto_cluster = cbind(percentuali_colonna, 
                                       'Percentuale_luogo_acquisto_campione' 
                                       = percentuali_riga)
print(tabella_luogo_acquisto_cluster)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_luogo_acquisto = chisq.test(tabella_luogo_acquisto_cluster)
print(test_chi_quadrato_luogo_acquisto)

# -----------------------------------------------------------------------------#
# Frequenza_di_acquisto
# Distribuzione 
# Crea la tabella di contingenza
tabella_frequenza_acquisto_cluster = table(as_factor(
  data_final$Frequenza_di_acquisto), data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_frequenza_acquisto_cluster, 
                                 margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_frequenza_acquisto_cluster)/
  totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_frequenza_acquisto = cbind(percentuali_colonna, 
                                          'Percentuale_frequenza_acquisto_campione'= 
                                            percentuali_riga)
print(tabella_finale_frequenza_acquisto)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_frequenza_acquisto = chisq.test(
  tabella_finale_frequenza_acquisto)
print(test_chi_quadrato_frequenza_acquisto)

# -----------------------------------------------------------------------------#
# Spesa_media
# Distribuzione 
# Crea la tabella di contingenza
tabella_Spesa_media_cluster = table(as_factor(data_final$Spesa_media), 
                                    data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_Spesa_media_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_Spesa_media_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_Spesa_media = cbind(percentuali_colonna, 
                                   'Percentuale_Spesa_media_campione' = 
                                     percentuali_riga)
print(tabella_finale_Spesa_media)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_Spesa_media = chisq.test(tabella_finale_Spesa_media)
print(test_chi_quadrato_Spesa_media)

# -----------------------------------------------------------------------------#
# Acquisto_recente_trucco
# Distribuzione 
# Crea la tabella di contingenza
tabella_trucco_cluster = table(as_factor(data_final$Acquisto_recente_trucco), 
                               data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_trucco_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_trucco_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_trucco = cbind(percentuali_colonna, 'Percentuale_trucco_campione' 
                              = percentuali_riga)
print(tabella_finale_trucco)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_trucco = chisq.test(tabella_trucco_cluster)
print(test_chi_quadrato_trucco)

# -----------------------------------------------------------------------------#
# Acquisto_recente_cura_corpo_viso
# Distribuzione 
# Crea la tabella di contingenza
tabella_cura_corpo_viso_cluster = table(as_factor(
  data_final$Acquisto_recente_cura_corpo_viso), data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_cura_corpo_viso_cluster, 
                                 margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_cura_corpo_viso_cluster)/
  totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_cura_corpo_viso = cbind(percentuali_colonna, 
                                       'Percentuale_cura_corpo_viso_campione' = 
                                         percentuali_riga)
print(tabella_finale_cura_corpo_viso)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_cura_corpo_viso = chisq.test(tabella_cura_corpo_viso_cluster)
print(test_chi_quadrato_cura_corpo_viso)

# -----------------------------------------------------------------------------#
# Acquisto_recente_profumi
# Distribuzione 
# Crea la tabella di contingenza
tabella_profumi_cluster = table(as_factor(data_final$Acquisto_recente_profumi), 
                                data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_profumi_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_profumi_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_profumi = cbind(percentuali_colonna, 
                               'Percentuale_profumi_campione' = 
                                 percentuali_riga)
print(tabella_finale_profumi)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_profumi = chisq.test(tabella_profumi_cluster)
print(test_chi_quadrato_profumi)

# -----------------------------------------------------------------------------#
# Acquisto_recente_abbronzatura
# Distribuzione 
# Crea la tabella di contingenza
tabella_abbronzatura_cluster = table(as_factor(
  data_final$Acquisto_recente_abbronzatura), data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_abbronzatura_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_abbronzatura_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_abbronzatura = cbind(percentuali_colonna, 
                                    'Percentuale_abbronzatura_campione' = 
                                      percentuali_riga)
print(tabella_finale_abbronzatura)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_abbronzatura = chisq.test(tabella_abbronzatura_cluster)
print(test_chi_quadrato_abbronzatura)

# -----------------------------------------------------------------------------#
# In_genere_compra
# Distribuzione 
# Crea la tabella di contingenza
tabella_genere_compra_cluster = table(as_factor(data_final$In_genere_compra), 
                                      data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_genere_compra_cluster, 
                                 margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_genere_compra_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_genere_compra = cbind(percentuali_colonna, 
                                     'Percentuale_genere_compra_campione' = 
                                       percentuali_riga)
print(tabella_finale_genere_compra)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_genere_compra = chisq.test(tabella_genere_compra_cluster)
print(test_chi_quadrato_genere_compra)

# -----------------------------------------------------------------------------#
# Sono_una_persona_dinamica_con_tanti_interessi
# Distribuzione 
# Crea la tabella di contingenza
tabella_interessi_cluster = table(as_factor(
  data_final$Sono_una_persona_dinamica_con_tanti_interessi), data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_interessi_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_interessi_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_interessi = cbind(percentuali_colonna, 
                                 'Percentuale_interessi_campione' = 
                                   percentuali_riga)
print(tabella_finale_interessi)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_interessi = chisq.test(tabella_interessi_cluster)
print(test_chi_quadrato_interessi)

# -----------------------------------------------------------------------------#
# La_pubblicità_che_vedo_influenza_le_mie_scelte_d'acquisto
# Distribuzione 
# Crea la tabella di contingenza
tabella_pubblicita_cluster = table(as_factor(
  data_final$`La_pubblicità_che_vedo_influenza_le_mie_scelte_d'acquisto`), 
  data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_pubblicita_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_pubblicita_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga.
tabella_finale_pubblicita = cbind(percentuali_colonna, 
                                  'Percentuale_pubblicita_campione' = 
                                    percentuali_riga)
print(tabella_finale_pubblicita)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_pubblicita = chisq.test(tabella_pubblicita_cluster)
print(test_chi_quadrato_pubblicita)

# -----------------------------------------------------------------------------#
# I_cambiamenti_nella_vita_quotidiana_mi_innervosiscono
# Distribuzione 
# Crea la tabella di contingenza
tabella_nervosi_cluster = table(as_factor(
  data_final$I_cambiamenti_nella_vita_quotidiana_mi_innervosiscono), 
  data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_nervosi_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_nervosi_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_nervosi = cbind(percentuali_colonna, 
                               'Percentuale_nervosi_campione' = 
                                 percentuali_riga)
print(tabella_finale_nervosi)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_nervosi = chisq.test(tabella_nervosi_cluster)
print(test_chi_quadrato_nervosi)

# -----------------------------------------------------------------------------#
# Sono_socievole_mi_piace_conoscere_sempre_nuove_persone
# Distribuzione 
# Crea la tabella di contingenza
tabella_socievole_cluster = table(as_factor(
  data_final$Sono_socievole_mi_piace_conoscere_sempre_nuove_persone), 
  data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_socievole_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_socievole_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_socievole = cbind(percentuali_colonna, 
                                 'Percentuale_socievole_campione' = 
                                   percentuali_riga)
print(tabella_finale_socievole)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_socievole = chisq.test(tabella_socievole_cluster)
print(test_chi_quadrato_socievole)

# -----------------------------------------------------------------------------#
# Per_i_miei_acquisti_bado_più_alla_praticità_che_a_tutto_il_resto
# Distribuzione 
# Crea la tabella di contingenza
tabella_praticita_cluster = table(as_factor(
  data_final$Per_i_miei_acquisti_bado_più_alla_praticità_che_a_tutto_il_resto), 
  data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_praticita_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_praticita_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga.
tabella_finale_praticita = cbind(percentuali_colonna, 
                                 'Percentuale_praticita_campione' = 
                                   percentuali_riga)
print(tabella_finale_praticita)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_praticita = chisq.test(tabella_praticita_cluster)
print(test_chi_quadrato_praticita)

# -----------------------------------------------------------------------------#
# I_valori_della_tradizione_sono_molto_importanti_per_me
# Distribuzione 
# Crea la tabella di contingenza
tabella_tradizione_cluster = table(as_factor(
  data_final$I_valori_della_tradizione_sono_molto_importanti_per_me), 
  data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_tradizione_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_tradizione_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga.
tabella_finale_tradizione = cbind(percentuali_colonna, 
                                  'Percentuale_acquisto_campione' = 
                                    percentuali_riga)
print(tabella_finale_tradizione)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_tradizione = chisq.test(tabella_tradizione_cluster)
print(test_chi_quadrato_tradizione)

# -----------------------------------------------------------------------------#
# Mi_piace_invitare_gli_amici_a_casa
# Distribuzione 
# Crea la tabella di contingenza
tabella_casa_cluster = table(as_factor(
  data_final$Mi_piace_invitare_gli_amici_a_casa), data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_casa_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_casa_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga.
tabella_finale_casa = cbind(percentuali_colonna, 
                            'Percentuale_casa_campione' = percentuali_riga)
print(tabella_finale_casa)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_casa = chisq.test(tabella_casa_cluster)
print(test_chi_quadrato_casa)

# -----------------------------------------------------------------------------#
# Amo_stare_all'aria_aperta
# Distribuzione 
# Crea la tabella di contingenza
tabella_aria_aperta_cluster = table(
  as_factor(data_final$`Amo_stare_all'aria_aperta`), data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_aria_aperta_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_aria_aperta_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_aria_aperta = cbind(percentuali_colonna, 
                                   'Percentuale_aria_aperta_campione' = 
                                     percentuali_riga)
print(tabella_finale_aria_aperta)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_aria_aperta = chisq.test(tabella_aria_aperta_cluster)
print(test_chi_quadrato_aria_aperta)

# -----------------------------------------------------------------------------#
# Sono_pronto_a_spendere_di_più_per_avere_la_qualità_elevata
# Distribuzione 
# Crea la tabella di contingenza
tabella_spendaccioni_cluster = table(as_factor(
  data_final$Sono_pronto_a_spendere_di_più_per_avere_la_qualità_elevata), 
  data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_spendaccioni_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_spendaccioni_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_spendaccioni = cbind(percentuali_colonna, 
                                    'Percentuale_spendaccioni_campione' = 
                                      percentuali_riga)
print(tabella_finale_spendaccioni)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_spendaccioni = chisq.test(tabella_spendaccioni_cluster)
print(test_chi_quadrato_spendaccioni)

# -----------------------------------------------------------------------------#
# Mi_piace_distinguermi_dagli_altri
# Distribuzione 
# Crea la tabella di contingenza
tabella_distinti_cluster = table(as_factor(
  data_final$Mi_piace_distinguermi_dagli_altri), data_final$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_distinti_cluster, margin = 2) * 100

# Calcola le percentuali di riga 
percentuali_riga = rowSums(tabella_distinti_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_distinti = cbind(percentuali_colonna, 
                                'Percentuale_distinti_campione' = 
                                  percentuali_riga)
print(tabella_finale_distinti)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_distinti = chisq.test(tabella_distinti_cluster)
print(test_chi_quadrato_distinti)



# -----------------------------------------------------------------------------#
# Importazione dati delle utilities derivanti dalla conjoint analysis
# -----------------------------------------------------------------------------#
utility = read_sav("Fidelity_conjoint.sav")

# selezione le variabili con i punteggi delle utility, escludendo le righe
# 41, 64, 83 e 99 che non sono state usate nella conjoint (sd = 0)
Subset2 = utility[-c(41, 64, 83, 99), 3:13]

# matrice di correlazione
correlation_matrix = cor(Subset2, method = "pearson", use = "complete.obs")
corrplot::corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.6,
  tl.cex = 0.8,
  diag = FALSE
)

# Test di significatività delle correlazioni
# 1. Bartlett test sulla matrice di correlazione
n_obs = nrow(Subset2)
bartlett_test = cortest.bartlett(correlation_matrix, n = n_obs)
print(bartlett_test)

# 2. KMO test
kmo_result = KMO(Subset2)
print(kmo_result$MSA)

# per maggiore interpretabilità non si effettua PCA


# -----------------------------------------------------------------------------#
# CLuster analysis sulle utilities
# -----------------------------------------------------------------------------#
# Clustering: Metodo di Ward e distanza euclidea
data_clu2 = Subset2 
d2 = dist(data_clu2, method ="euclidean")
# Link di Ward
hc2 = hclust(d2, method = "ward.D2")

# Dendrogramma
plot(hc2, main = "Dendrogramma con link di Ward (dist. Euclidea)",
     xlab = "Osservazioni", ylab = "Distanza", labels=F)
rect.hclust(hc2, k = 4, border = "red")

library("NbClust")
best_ch=NbClust(data_clu2, distance = "euclidean", min.nc=2, max.nc=8, method = "ward.D2", index = "ch")
best_d=NbClust(data_clu2, distance = "euclidean", min.nc=2, max.nc=8, method = "ward.D2", index = "db")
best_s=NbClust(data_clu2, distance = "euclidean", min.nc=2, max.nc=8, method = "ward.D2", index = "silhouette")
# Taglio il dendrogramma per ottenere 4 cluster
clusters2 = cutree(hc2, k = 4)

# Aggiungo i cluster al dataset originale, assegnando cluster 5
# alle righe 41, 64, 83 e 99
dati$Clu2 = NA
dati$Clu2[-c(41, 64, 83, 99)] = clusters2
dati$Clu2[c(41, 64, 83, 99)] = 5
table(dati$Clu2, useNA = "always")

# join tra dati e utility per ID
dati$ID = as.numeric(dati$ID)
utility$ID = as.numeric(utility$ID)
data_final2 = merge(dati, utility[, c(1, 3:13)], by.x = "ID", by.y = "ID", all.x = TRUE)
data_final2 = data_final2[-c(41, 64, 83, 99), ]

# -----------------------------------------------------------------------------#
# Caratterizzazione 
# -----------------------------------------------------------------------------#
# variabili usate per i cluster
set_var2 = c(names(data_final2)[40:50])

# Calcola statistiche globali 
media_totale2 = colMeans(data_final2[, 40:50], na.rm = TRUE)
varianza_totale2 = apply(data_final2[, 40:50], 2, var, na.rm = TRUE)
numerosita_totale2 = nrow(data_final2)

# Calcolo media, varianza e t-statistic di ogni variabile per cluster
statistiche_cluster_t2 = data_final2 %>%
  group_by(Clu2) %>%
  group_modify(~ {
    res = data.frame(n_cluster = nrow(.x))
    for (var in set_var2) { # calcola per ogni variabile
      stats = calcola_statistiche_t(.x, var, media_totale2[var], varianza_totale2[var], numerosita_totale2)
      res[paste0("media_", var, "_cluster")] = stats$media
      res[paste0("sp_", var)] = stats$sp
      res[paste0("t_", var)] = stats$t
    }
    
    return(res)
  }) %>%
  ungroup()


# Visualizza i risultati
# Seleziona le colonne delle medie dalle statistiche
tabella_medie2 = statistiche_cluster_t2 %>%
  select(Clu2, n_cluster, starts_with("media_"))
# Rinomina le colonne per chiarezza
nomi_media2 = names(tabella_medie2)[startsWith(names(tabella_medie2), "media_")]
nomi_variabili_media2 = gsub("media_", "", nomi_media2)
names(tabella_medie2)[startsWith(names(tabella_medie2), "media_")] = paste0("media_", nomi_variabili_media2)

print(t(tabella_medie2))

# write.xlsx(tabella_medie, "C:/Users/fedep/OneDrive/Documenti/Unimib/analisiDiMercatoQuant/progettoEsame/Tabella_medie_conjoint.xlsx")


# Seleziona le colonne delle statistiche t dalle statistiche
tabella_t2 = statistiche_cluster_t2 %>%
  select(Clu2, n_cluster, starts_with("t_"))
# Rinomina le colonne per chiarezza
nomi_t2 = names(tabella_t2)[startsWith(names(tabella_t2), "t_")]
nomi_variabili_t2 = gsub("t_", "", nomi_t2)
names(tabella_t2)[startsWith(names(tabella_t2), "t_")] = paste0("t_", nomi_variabili_t2)

print(t(tabella_t2))

# write.xlsx(tabella_t, "C:/Users/fedep/OneDrive/Documenti/Unimib/analisiDiMercatoQuant/progettoEsame/Tabella_t_conjoint.xlsx")

# -----------------------------------------------------------------------------#
# eta
# Calcola le statistiche globali
media_eta_totale = mean(data_final2$eta, na.rm = TRUE)
varianza_eta_totale = var(data_final2$eta, na.rm = TRUE)
numerosita_eta_totale = length(na.omit(data_final2$eta))

# Calcola sp e t per ciascun cluster
statistiche_eta_cluster = data_final2 %>%
  group_by(Clu2) %>%
  summarize(
    n_cluster = n(),

    media_eta_cluster = mean(eta, na.rm = TRUE),

    varianza_eta_cluster = var(eta, na.rm = TRUE),

    sp_eta = ((n_cluster - 1) * varianza_eta_cluster +
                (numerosita_eta_totale - 1) * varianza_eta_totale) /
      (n_cluster + numerosita_eta_totale - 2),

    t_eta = (media_eta_cluster - media_eta_totale) /
      sqrt(sp_eta * (1/n_cluster + 1/numerosita_eta_totale))
  )

print(statistiche_eta_cluster)

# -----------------------------------------------------------------------------#
# Genere
# Distribuzione
# Crea la tabella di contingenza
tabella_genere_cluster = table(data_final2$Genere, data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_genere_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_genere_cluster)/totale_campione * 100

# Aggiungo colonna con percentuali riga
tabella_percentuale_totale= cbind(percentuali_colonna,
                                  'Percentuale Genere'= percentuali_riga)
print(tabella_percentuale_totale)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato = chisq.test(tabella_genere_cluster)
print(test_chi_quadrato)

# -----------------------------------------------------------------------------#
# Luogo_di_acquisto
# Distribuzione
# Crea la tabella di contingenza
tabella_luogo_acquisto_cluster = table(as_factor(data_final2$Luogo_di_acquisto),
                                       data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_luogo_acquisto_cluster,
                                 margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_luogo_acquisto_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_luogo_acquisto_cluster = cbind(percentuali_colonna,
                                     'Percentuale_luogo_acquisto_campione'
                                     = percentuali_riga)
print(tabella_luogo_acquisto_cluster)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_luogo_acquisto = chisq.test(tabella_luogo_acquisto_cluster)
print(test_chi_quadrato_luogo_acquisto)

# -----------------------------------------------------------------------------#
# Frequenza_di_acquisto
# Distribuzione
# Crea la tabella di contingenza
tabella_frequenza_acquisto_cluster = table(as_factor(
  data_final2$Frequenza_di_acquisto), data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_frequenza_acquisto_cluster,
                                 margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_frequenza_acquisto_cluster)/
  totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_frequenza_acquisto = cbind(percentuali_colonna,
                                   'Percentuale_frequenza_acquisto_campione'=
                                     percentuali_riga)
print(tabella_finale_frequenza_acquisto)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_frequenza_acquisto = chisq.test(
  tabella_finale_frequenza_acquisto)
print(test_chi_quadrato_frequenza_acquisto)

# -----------------------------------------------------------------------------#
# Spesa_media
# Distribuzione
# Crea la tabella di contingenza
tabella_Spesa_media_cluster = table(as_factor(data_final2$Spesa_media),
                                    data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_Spesa_media_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_Spesa_media_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_Spesa_media = cbind(percentuali_colonna,
                                   'Percentuale_Spesa_media_campione' =
                                     percentuali_riga)
print(tabella_finale_Spesa_media)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_Spesa_media = chisq.test(tabella_finale_Spesa_media)
print(test_chi_quadrato_Spesa_media)

# -----------------------------------------------------------------------------#
# Acquisto_recente_trucco
# Distribuzione
# Crea la tabella di contingenza
tabella_trucco_cluster = table(as_factor(data_final2$Acquisto_recente_trucco),
                               data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_trucco_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_trucco_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_trucco = cbind(percentuali_colonna, 'Percentuale_trucco_campione'
                             = percentuali_riga)
print(tabella_finale_trucco)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_trucco = chisq.test(tabella_trucco_cluster)
print(test_chi_quadrato_trucco)

# -----------------------------------------------------------------------------#
# Acquisto_recente_cura_corpo_viso
# Distribuzione
# Crea la tabella di contingenza
tabella_cura_corpo_viso_cluster = table(as_factor(
  data_final2$Acquisto_recente_cura_corpo_viso), data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_cura_corpo_viso_cluster,
                                 margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_cura_corpo_viso_cluster)/
  totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_cura_corpo_viso = cbind(percentuali_colonna,
                                       'Percentuale_cura_corpo_viso_campione' =
                                         percentuali_riga)
print(tabella_finale_cura_corpo_viso)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_cura_corpo_viso = chisq.test(tabella_cura_corpo_viso_cluster)
print(test_chi_quadrato_cura_corpo_viso)

# -----------------------------------------------------------------------------#
# Acquisto_recente_profumi
# Distribuzione
# Crea la tabella di contingenza
tabella_profumi_cluster = table(as_factor(data_final2$Acquisto_recente_profumi),
                                data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_profumi_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_profumi_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_profumi = cbind(percentuali_colonna,
                               'Percentuale_profumi_campione' =
                                 percentuali_riga)
print(tabella_finale_profumi)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_profumi = chisq.test(tabella_profumi_cluster)
print(test_chi_quadrato_profumi)

# -----------------------------------------------------------------------------#
# Acquisto_recente_abbronzatura
# Distribuzione
# Crea la tabella di contingenza
tabella_abbronzatura_cluster = table(as_factor(
  data_final2$Acquisto_recente_abbronzatura), data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_abbronzatura_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_abbronzatura_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_abbronzatura = cbind(percentuali_colonna,
                                    'Percentuale_abbronzatura_campione' =
                                      percentuali_riga)
print(tabella_finale_abbronzatura)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_abbronzatura = chisq.test(tabella_abbronzatura_cluster)
print(test_chi_quadrato_abbronzatura)

# -----------------------------------------------------------------------------#
# In_genere_compra
# Distribuzione
# Crea la tabella di contingenza
tabella_genere_compra_cluster = table(as_factor(data_final2$In_genere_compra),
                                      data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_genere_compra_cluster,
                                 margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_genere_compra_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_genere_compra = cbind(percentuali_colonna,
                                     'Percentuale_genere_compra_campione' =
                                       percentuali_riga)
print(tabella_finale_genere_compra)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_genere_compra = chisq.test(tabella_genere_compra_cluster)
print(test_chi_quadrato_genere_compra)

# -----------------------------------------------------------------------------#
# Sono_una_persona_dinamica_con_tanti_interessi
# Distribuzione
# Crea la tabella di contingenza
tabella_interessi_cluster = table(as_factor(
  data_final2$Sono_una_persona_dinamica_con_tanti_interessi), data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_interessi_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_interessi_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_interessi = cbind(percentuali_colonna,
                                 'Percentuale_interessi_campione' =
                                   percentuali_riga)
print(tabella_finale_interessi)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_interessi = chisq.test(tabella_interessi_cluster)
print(test_chi_quadrato_interessi)

# -----------------------------------------------------------------------------#
# La_pubblicità_che_vedo_influenza_le_mie_scelte_d'acquisto
# Distribuzione
# Crea la tabella di contingenza
tabella_pubblicita_cluster = table(as_factor(
  data_final2$`La_pubblicità_che_vedo_influenza_le_mie_scelte_d'acquisto`),
  data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_pubblicita_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_pubblicita_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga.
tabella_finale_pubblicita = cbind(percentuali_colonna,
                                  'Percentuale_pubblicita_campione' =
                                    percentuali_riga)
print(tabella_finale_pubblicita)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_pubblicita = chisq.test(tabella_pubblicita_cluster)
print(test_chi_quadrato_pubblicita)

# -----------------------------------------------------------------------------#
# I_cambiamenti_nella_vita_quotidiana_mi_innervosiscono
# Distribuzione
# Crea la tabella di contingenza
tabella_nervosi_cluster = table(as_factor(
  data_final2$I_cambiamenti_nella_vita_quotidiana_mi_innervosiscono),
  data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_nervosi_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_nervosi_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_nervosi = cbind(percentuali_colonna,
                               'Percentuale_nervosi_campione' =
                                 percentuali_riga)
print(tabella_finale_nervosi)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_nervosi = chisq.test(tabella_nervosi_cluster)
print(test_chi_quadrato_nervosi)

# -----------------------------------------------------------------------------#
# Sono_socievole_mi_piace_conoscere_sempre_nuove_persone
# Distribuzione
# Crea la tabella di contingenza
tabella_socievole_cluster = table(as_factor(
  data_final2$Sono_socievole_mi_piace_conoscere_sempre_nuove_persone),
  data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_socievole_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_socievole_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_socievole = cbind(percentuali_colonna,
                                 'Percentuale_socievole_campione' =
                                   percentuali_riga)
print(tabella_finale_socievole)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_socievole = chisq.test(tabella_socievole_cluster)
print(test_chi_quadrato_socievole)

# -----------------------------------------------------------------------------#
# Per_i_miei_acquisti_bado_più_alla_praticità_che_a_tutto_il_resto
# Distribuzione
# Crea la tabella di contingenza
tabella_praticita_cluster = table(as_factor(
  data_final2$Per_i_miei_acquisti_bado_più_alla_praticità_che_a_tutto_il_resto),
  data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_praticita_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_praticita_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga.
tabella_finale_praticita = cbind(percentuali_colonna,
                                 'Percentuale_praticita_campione' =
                                   percentuali_riga)
print(tabella_finale_praticita)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_praticita = chisq.test(tabella_praticita_cluster)
print(test_chi_quadrato_praticita)

# -----------------------------------------------------------------------------#
# I_valori_della_tradizione_sono_molto_importanti_per_me
# Distribuzione
# Crea la tabella di contingenza
tabella_tradizione_cluster = table(as_factor(
  data_final2$I_valori_della_tradizione_sono_molto_importanti_per_me),
  data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_tradizione_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_tradizione_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga.
tabella_finale_tradizione = cbind(percentuali_colonna,
                                  'Percentuale_acquisto_campione' =
                                    percentuali_riga)
print(tabella_finale_tradizione)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_tradizione = chisq.test(tabella_tradizione_cluster)
print(test_chi_quadrato_tradizione)

# -----------------------------------------------------------------------------#
# Mi_piace_invitare_gli_amici_a_casa
# Distribuzione
# Crea la tabella di contingenza
tabella_casa_cluster = table(as_factor(
  data_final2$Mi_piace_invitare_gli_amici_a_casa), data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_casa_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_casa_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga.
tabella_finale_casa = cbind(percentuali_colonna,
                                'Percentuale_casa_campione' = percentuali_riga)
print(tabella_finale_casa)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_casa = chisq.test(tabella_casa_cluster)
print(test_chi_quadrato_casa)

# -----------------------------------------------------------------------------#
# Amo_stare_all'aria_aperta
# Distribuzione
# Crea la tabella di contingenza
tabella_aria_aperta_cluster = table(
  as_factor(data_final2$`Amo_stare_all'aria_aperta`), data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_aria_aperta_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_aria_aperta_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_aria_aperta = cbind(percentuali_colonna,
                                   'Percentuale_aria_aperta_campione' =
                                     percentuali_riga)
print(tabella_finale_aria_aperta)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_aria_aperta = chisq.test(tabella_aria_aperta_cluster)
print(test_chi_quadrato_aria_aperta)

# -----------------------------------------------------------------------------#
# Sono_pronto_a_spendere_di_più_per_avere_la_qualità_elevata
# Distribuzione
# Crea la tabella di contingenza
tabella_spendaccioni_cluster = table(as_factor(
  data_final2$Sono_pronto_a_spendere_di_più_per_avere_la_qualità_elevata),
  data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_spendaccioni_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_spendaccioni_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_spendaccioni = cbind(percentuali_colonna,
                                    'Percentuale_spendaccioni_campione' =
                                      percentuali_riga)
print(tabella_finale_spendaccioni)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_spendaccioni = chisq.test(tabella_spendaccioni_cluster)
print(test_chi_quadrato_spendaccioni)

# -----------------------------------------------------------------------------#
# Mi_piace_distinguermi_dagli_altri
# Distribuzione
# Crea la tabella di contingenza
tabella_distinti_cluster = table(as_factor(
  data_final2$Mi_piace_distinguermi_dagli_altri), data_final2$Clu)

# Calcola il totale dell'intero campione
totale_campione = nrow(data_final2)

# Calcola le percentuali di colonna rispetto al totale del campione
percentuali_colonna = prop.table(tabella_distinti_cluster, margin = 2) * 100

# Calcola le percentuali di riga
percentuali_riga = rowSums(tabella_distinti_cluster)/totale_campione * 100

#Aggiungo colonna con percentuali riga
tabella_finale_distinti = cbind(percentuali_colonna,
                                'Percentuale_distinti_campione' =
                                  percentuali_riga)
print(tabella_finale_distinti)

# Calcola il chi-quadrato di Pearson
test_chi_quadrato_distinti = chisq.test(tabella_distinti_cluster)
print(test_chi_quadrato_distinti)


# Campioncini_di_prova_regalati
# Calcola le statistiche globali
media_campioncini_totale = mean(data_final2$Campioncini_di_prova_regalati, na.rm = TRUE)
varianza_campioncini_totale = var(data_final2$Campioncini_di_prova_regalati, na.rm = TRUE)
numerosita_campioncini_totale = length(na.omit(data_final2$Campioncini_di_prova_regalati))

# Calcola sp e t per ciascun cluster
statistiche_campioncini_cluster = data_final2 %>%
  group_by(Clu2) %>%
  summarize(
    n_cluster = n(),
    
    media_campioncini_cluster = mean(Campioncini_di_prova_regalati, na.rm = TRUE),
    
    varianza_campioncini_cluster = var(Campioncini_di_prova_regalati, na.rm = TRUE),
    
    sp_campioncini = ((n_cluster - 1) * varianza_campioncini_cluster +
                (numerosita_campioncini_totale - 1) * varianza_campioncini_totale) /
      (n_cluster + numerosita_campioncini_totale - 2),
    
    t_campioncini = (media_campioncini_cluster - media_campioncini_totale) /
      sqrt(sp_campioncini * (1/n_cluster + 1/numerosita_campioncini_totale))
  )

print(statistiche_campioncini_cluster)

# Consigli_di_utilizzo_dei_prodotti
# Calcola le statistiche globali
media_consigli_totale = mean(data_final2$Consigli_di_utilizzo_dei_prodotti, na.rm = TRUE)
varianza_consigli_totale = var(data_final2$Consigli_di_utilizzo_dei_prodotti, na.rm = TRUE)
numerosita_consigli_totale = length(na.omit(data_final2$Consigli_di_utilizzo_dei_prodotti))

# Calcola sp e t per ciascun cluster
statistiche_consigli_cluster = data_final2 %>%
  group_by(Clu2) %>%
  summarize(
    n_cluster = n(),
    
    media_consigli_cluster = mean(Consigli_di_utilizzo_dei_prodotti, na.rm = TRUE),
    
    varianza_consigli_cluster = var(Consigli_di_utilizzo_dei_prodotti, na.rm = TRUE),
    
    sp_consigli = ((n_cluster - 1) * varianza_consigli_cluster +
                        (numerosita_consigli_totale - 1) * varianza_consigli_totale) /
      (n_cluster + numerosita_consigli_totale - 2),
    
    t_consigli = (media_consigli_cluster - media_consigli_totale) /
      sqrt(sp_consigli * (1/n_cluster + 1/numerosita_consigli_totale))
  )

print(statistiche_consigli_cluster)

# Prezzi_praticati
# Calcola le statistiche globali
media_prezzi_totale = mean(data_final2$Prezzi_praticati, na.rm = TRUE)
varianza_prezzi_totale = var(data_final2$Prezzi_praticati, na.rm = TRUE)
numerosita_prezzi_totale = length(na.omit(data_final2$Prezzi_praticati))

# Calcola sp e t per ciascun cluster
statistiche_prezzi_cluster = data_final2 %>%
  group_by(Clu2) %>%
  summarize(
    n_cluster = n(),
    
    media_prezzi_cluster = mean(Prezzi_praticati, na.rm = TRUE),
    
    varianza_prezzi_cluster = var(Prezzi_praticati, na.rm = TRUE),
    
    sp_prezzi = ((n_cluster - 1) * varianza_prezzi_cluster +
                     (numerosita_prezzi_totale - 1) * varianza_prezzi_totale) /
      (n_cluster + numerosita_prezzi_totale - 2),
    
    t_prezzi = (media_prezzi_cluster - media_prezzi_totale) /
      sqrt(sp_prezzi * (1/n_cluster + 1/numerosita_prezzi_totale))
  )

print(statistiche_prezzi_cluster)

# Sconto_praticato
# Calcola le statistiche globali
media_sconto_totale = mean(data_final2$Sconto_praticato, na.rm = TRUE)
varianza_sconto_totale = var(data_final2$Sconto_praticato, na.rm = TRUE)
numerosita_sconto_totale = length(na.omit(data_final2$Sconto_praticato))

# Calcola sp e t per ciascun cluster
statistiche_sconto_cluster = data_final2 %>%
  group_by(Clu2) %>%
  summarize(
    n_cluster = n(),
    
    media_sconto_cluster = mean(Sconto_praticato, na.rm = TRUE),
    
    varianza_sconto_cluster = var(Sconto_praticato, na.rm = TRUE),
    
    sp_sconto = ((n_cluster - 1) * varianza_sconto_cluster +
                   (numerosita_sconto_totale - 1) * varianza_sconto_totale) /
      (n_cluster + numerosita_sconto_totale - 2),
    
    t_sconto = (media_sconto_cluster - media_sconto_totale) /
      sqrt(sp_sconto * (1/n_cluster + 1/numerosita_sconto_totale))
  )

print(statistiche_sconto_cluster)

# Cortesia_del_personale
# Calcola le statistiche globali
media_cortesia_totale = mean(data_final2$Cortesia_del_personale, na.rm = TRUE)
varianza_cortesia_totale = var(data_final2$Cortesia_del_personale, na.rm = TRUE)
numerosita_cortesia_totale = length(na.omit(data_final2$Cortesia_del_personale))

# Calcola sp e t per ciascun cluster
statistiche_cortesia_cluster = data_final2 %>%
  group_by(Clu2) %>%
  summarize(
    n_cluster = n(),
    
    media_cortesia_cluster = mean(Cortesia_del_personale, na.rm = TRUE),
    
    varianza_cortesia_cluster = var(Cortesia_del_personale, na.rm = TRUE),
    
    sp_cortesia = ((n_cluster - 1) * varianza_cortesia_cluster +
                   (numerosita_cortesia_totale - 1) * varianza_cortesia_totale) /
      (n_cluster + numerosita_cortesia_totale - 2),
    
    t_cortesia = (media_cortesia_cluster - media_cortesia_totale) /
      sqrt(sp_cortesia * (1/n_cluster + 1/numerosita_cortesia_totale))
  )

print(statistiche_cortesia_cluster)

# Interpretazione_delle_esigenze_del_cliente
# Calcola le statistiche globali
media_esigenze_totale = mean(data_final2$Interpretazione_delle_esigenze_del_cliente, na.rm = TRUE)
varianza_esigenze_totale = var(data_final2$Interpretazione_delle_esigenze_del_cliente, na.rm = TRUE)
numerosita_esigenze_totale = length(na.omit(data_final2$Interpretazione_delle_esigenze_del_cliente))

# Calcola sp e t per ciascun cluster
statistiche_esigenze_cluster = data_final2 %>%
  group_by(Clu2) %>%
  summarize(
    n_cluster = n(),
    
    media_esigenze_cluster = mean(Interpretazione_delle_esigenze_del_cliente, na.rm = TRUE),
    
    varianza_esigenze_cluster = var(Interpretazione_delle_esigenze_del_cliente, na.rm = TRUE),
    
    sp_esigenze = ((n_cluster - 1) * varianza_esigenze_cluster +
                     (numerosita_esigenze_totale - 1) * varianza_esigenze_totale) /
      (n_cluster + numerosita_esigenze_totale - 2),
    
    t_esigenze = (media_esigenze_cluster - media_esigenze_totale) /
      sqrt(sp_esigenze * (1/n_cluster + 1/numerosita_esigenze_totale))
  )

print(statistiche_esigenze_cluster)

# Segnalazione_delle_promozioni
# Calcola le statistiche globali
media_promozioni_totale = mean(data_final2$Segnalazione_delle_promozioni, na.rm = TRUE)
varianza_promozioni_totale = var(data_final2$Segnalazione_delle_promozioni, na.rm = TRUE)
numerosita_promozioni_totale = length(na.omit(data_final2$Segnalazione_delle_promozioni))

# Calcola sp e t per ciascun cluster
statistiche_promozioni_cluster = data_final2 %>%
  group_by(Clu2) %>%
  summarize(
    n_cluster = n(),
    
    media_promozioni_cluster = mean(Segnalazione_delle_promozioni, na.rm = TRUE),
    
    varianza_promozioni_cluster = var(Segnalazione_delle_promozioni, na.rm = TRUE),
    
    sp_promozioni = ((n_cluster - 1) * varianza_promozioni_cluster +
                     (numerosita_promozioni_totale - 1) * varianza_promozioni_totale) /
      (n_cluster + numerosita_promozioni_totale - 2),
    
    t_promozioni = (media_promozioni_cluster - media_promozioni_totale) /
      sqrt(sp_promozioni * (1/n_cluster + 1/numerosita_promozioni_totale))
  )

print(statistiche_promozioni_cluster)

### calcolo le importanze per ogni individuo
# Calcola i range di ogni attributo e poi le importanze percentuali
importanze = utility %>%
  mutate(
    range_premio = pmax(Premio1, Premio2, Premio3, na.rm = TRUE) - 
      pmin(Premio1, Premio2, Premio3, na.rm = TRUE),
    
    range_animazione = pmax(Animazione1, Animazione2, Animazione3, na.rm = TRUE) - 
      pmin(Animazione1, Animazione2, Animazione3, na.rm = TRUE),
    
    range_convenzione = pmax(Convenzione1, Convenzione2, Convenzione3, na.rm = TRUE) - 
      pmin(Convenzione1, Convenzione2, Convenzione3, na.rm = TRUE),
    
    range_sconto = pmax(Sconto1, Sconto2, na.rm = TRUE) - 
      pmin(Sconto1, Sconto2, na.rm = TRUE),
    
    somma_range = range_premio + range_animazione + range_convenzione + range_sconto,
    
    imp_premio = 100 * range_premio / somma_range,
    imp_animazione = 100 * range_animazione / somma_range,
    imp_convenzione = 100 * range_convenzione / somma_range,
    imp_sconto = 100 * range_sconto / somma_range
  )

# Tieni solo le colonne utili
importanze_finali = importanze %>%
  select(
    ID,
    imp_premio,
    imp_animazione,
    imp_convenzione,
    imp_sconto
  )

head(importanze_finali)

# write.csv(importanze_finali, "C:/Users/fedep/OneDrive/Documenti/Unimib/analisiDiMercatoQuant/progettoEsame/importanze_individuali.csv", row.names = FALSE)

importanze_finali = importanze_finali %>%
  mutate(somma_importanze = imp_premio + imp_animazione + imp_convenzione + imp_sconto)

summary(importanze_finali$somma_importanze)

# aggiungo i cluster alla tabella delle importanze
importanze_finali$Clu2 = NA
importanze_finali$Clu2[-c(41, 64, 83, 99)] = clusters2
importanze_finali$Clu2[c(41, 64, 83, 99)] = 5
table(importanze_finali$Clu2, useNA = "always")

colMeans(importanze_finali[which(importanze_finali$Clu2 == 1), 2:5])
colMeans(importanze_finali[which(importanze_finali$Clu2 == 2), 2:5])
colMeans(importanze_finali[which(importanze_finali$Clu2 == 3), 2:5])
colMeans(importanze_finali[which(importanze_finali$Clu2 == 4), 2:5])


# -----------------------------------------------------------------------------#
# doppia segmentazione
# -----------------------------------------------------------------------------#
dati$Clu = clusters
table(dati$Clu)
table(dati$Clu2)

dati$Clu = factor(dati$Clu,
                  levels = 1:6,
                  labels = c("Disinteressati ed abituali", "Donne coinvolte ed esigenti", 
                             "Coinvolti ma economici ed occasionali", "Grandi spender impulsivi ed instabili", 
                             "Occasionali tradizionalisti", "Uomini opportunisti e disaffezionati"))

dati$Clu2 = factor(dati$Clu2,
                  levels = 1:5,
                  labels = c("Beauty farm - festa a tema + sconto fisso", "Beauty farm + rivista + centro estetico + sconto promozionale", 
                             "Festa a tema + palestra", "Adventure - rivista", 
                             "Totalmente indifferenti"))

# Tabella dati iniziali
Tab = table(dati$Clu, dati$Clu2)

#profili riga
profili_riga = as.data.frame.matrix(prop.table(Tab, 1) * 100)
round(profili_riga, 2)

#profili colonna
profili_colonna = as.data.frame.matrix(prop.table(Tab, 2) * 100)
round(profili_colonna, 2)

library(FactoMineR)
library(factoextra)

# Esegui il test del chi-quadrato
risultato_chi_quadrato = chisq.test(Tab)
#valori della tabella con troppi zeri usiamo la simulazione Monte Carlo
# risultato_chi_quadrato = chisq.test(Tab, simulate.p.value = TRUE, B = 2000)

# Visualizza il risultato
print(risultato_chi_quadrato)
# Esegui l'Analisi delle Corrispondenze (CA)
res.ca = CA(Tab, graph = FALSE) # graph = FALSE per non mostrare i grafici automaticamente

# Visualizzazione dei risultati

# Valori Singolari (radici quadrate degli autovalori) e percentuale di inerzia spiegata
print("Valori Singolari e Inerzia:")
print(res.ca$eig)

#Coordinate di riga teniamo solo le prime due colonne (Dim 1 e Dim 2)
coord = as.data.frame(res.ca$row$coord[, 1:2])
colnames(coord) = c("Coord_Dim1", "Coord_Dim2")

#Contributo all'asse fattoriale
contrib = as.data.frame(res.ca$row$contrib[, 1:2])
colnames(contrib) = c("Contrib_Dim1", "Contrib_Dim2")

#Qualità di rappresentazione dei punti (cos2)
cos2 = as.data.frame(res.ca$row$cos2[, 1:2])
colnames(cos2) = c("Cos2_Dim1", "Cos2_Dim2")

#Tutto in un unico dataframe
tabella_finale = cbind(coord, contrib, cos2)
#Aggiungi il nome delle modalità come colonna 
tabella_righe = tabella_finale %>%
  mutate(Profilo = rownames(tabella_finale)) %>%
  select(Profilo, everything())
rownames(tabella_righe) = NULL
# Visualizza il risultato
print(tabella_righe)
#Visualizza la tabella finale
print(tabella_finale)


#Coordinate di colonna teniamo solo le prime due colonne (Dim 1 e Dim 2)
coord_col = as.data.frame(res.ca$col$coord[, 1:2])
colnames(coord_col) = c("Coord_Dim1", "Coord_Dim2")

contrib_col = as.data.frame(res.ca$col$contrib[, 1:2])
colnames(contrib_col) = c("Contrib_Dim1", "Contrib_Dim2")

cos2_col = as.data.frame(res.ca$col$cos2[, 1:2])
colnames(cos2_col) = c("Cos2_Dim1", "Cos2_Dim2")

#Tutto in un unico dataframe
tabella_colonne = cbind(coord_col, contrib_col, cos2_col)

#Aggiungi il nome delle modalità come colonna 
tabella_colonne = tabella_colonne %>%
  mutate(Modalita = rownames(tabella_colonne)) %>%
  select(Modalita, everything())
rownames(tabella_colonne) = NULL
# 4. Visualizza il risultato
print(tabella_colonne)


# Visualizzare i grafici 
plot(res.ca, axes = c(1, 2))             # Grafico congiunto di righe e colonne





















