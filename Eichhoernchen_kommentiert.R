#Daten herunterladen und als csv-Datei einlesen


nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")


# Installieren der Packages (falls noch nicht geschehen)

#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("wordcloud2")

#Aufrufen der benötigten Packages:

library(tidyverse)
library(wordcloud2)
library(tidytext)

#Laden der Stopwörter

data(stop_words)

# NAs aus dem sq löschen

sq <- drop_na(nyc_squirrels,other_activities)

# Auswählen der Spalte "Other activities"
# (mutate) - Anlegen einer neuen Spalte "all_text" 
# Separieren der Wortreihen in der Spalte "Other activities" in Wörter getrennt durch Leerzeichen
# (unnest) - Rausnehmen der Wörter aus "all-text" und verlagern in die Spalte "word"
# anti_join - Entfernen der stop words (View(stop_words))
# filter - Entfernen aller unerwünschten Wörter/Zahlen

sq <- sq %>% select(other_activities) %>% 
  mutate(all_text = stringr::str_c(other_activities, sep = " ")) %>% 
  tidytext::unnest_tokens(word, all_text) %>% 
  anti_join(stop_words) %>% 
  filter(!word %in% c(("unknown"), c("1":"20")))

#Jetzt müssen wir die Wörter noch zusammen zählen
#Zunächst werden Wörter in der Spalte "Word" gruppiert (group_by), 
# neue Spalte anlegen, die neue Spalte heißt "n_activities", 
# darin sind die einzelnen Wörter aus komplexen Aktivitäten 
#(select) Auswählen von Spalten "Word" und "n-activities", other_activities fällt weg
#Aufheben der Gruppierung
#Neue Gruppierung nach "word" und "n_activities"
#Zusammenzählen mit (summarize)
#im Ergebnis sehen wir, wie häufig jedes Wort vorkommt
sq_sum <- sq %>% 
  group_by(word) %>% 
  mutate(n_activities = n_distinct(other_activities)) %>% 
  select(word,n_activities) %>% 
  ungroup() %>% 
  group_by(word,n_activities) %>% 
  summarize()


# Erstellen der Wordcloud
# (arrange) ordnet die Daten in absteigender Reihenfolge
# umändern in einen data frame
# (slice) Auswählen der Zeilen von 1-100
# Erstellen eines tibbles (eine spezielle Art von data frame)
# Wordcloud erstellen aus der Spalte "n_acitivities", (size) - die Bestimmung der Cloudgröße; (shape) Bestimmen der Form
sq_sum %>% 
  arrange(desc(n_activities)) %>% 
  as_data_frame() %>% 
  slice(1:100) %>% 
  as_tibble() %>% 
  wordcloud2(sq_sum$n_activities, size = .55, shape = 'cardioid')


# gleicher Code mit Grafik als Muster
sq_sum %>% 
  arrange(desc(n_activities)) %>% 
  as_data_frame() %>% 
  slice(1:100) %>% 
  as_tibble() %>% 
  wordcloud2(sq_sum$n_activities, size = .8, figPath = "squirrel.png", color = "brown")
