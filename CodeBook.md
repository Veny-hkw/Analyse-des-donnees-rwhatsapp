CodeBook : data analysis

1) importation and data cleaning

chat <- rwa_read("raw_data.txt")

f_chat <- chat %>% 
  filter(!is.na(author)) %>% 
  filter(!text %in% "<Médias omis>") %>% 
  mutate(temps = date(time),
         saison = case_when(
           temps >= dmy(02032022) & temps <= dmy(01082022) ~ "1ère période",
           temps >= dmy(02082022) & temps <= dmy(03012023) ~ "2nde période",
           temps >= dmy(04012023) & temps <= dmy(02062023) ~ "3ème période",
           temps >= dmy(03062024) & temps <= dmy(09012024) ~ "4ème période",
           temps >= dmy(10012024) & temps <= dmy(11052024) ~ "5ème période"
         )) %>% 
  select(-c(time, source))

## remove links
f_chat$text <- f_chat$text %>% 
  str_remove("http[[:alnum:]]*") %>% 
  str_remove("www[[:alnum:]]*") %>% 
  str_remove("youtube[[:alnum:]]*") %>% 
  str_remove("Ce message a été supprimé") %>% 
  str_remove("null")

## remove numbers
f_chat$text <- f_chat$text %>% 
  removeNumbers()

## remove punctuation
f_chat$text <- f_chat$text %>% 
  removePunctuation()

## remove whitespace
f_chat$text <- f_chat$text %>% 
  stripWhitespace()

## change the name of some authors
f_chat$author <- f_chat$author %>% 
  str_replace_all("Deus E Capaz De Fazer", "Veny") %>% 
  str_replace_all("Elbert A", "Elbert") %>% 
  str_replace("Mum Of Love", "Autres") %>%  
  str_replace("Monsieur EKIRI Julien", "Autres") %>% 
  str_replace("Denis", "Denson")

#2) create table of raw data

f_chat %>% 
  select(-c(temps, saison, emoji_name)) %>% 
  datatable(
    rownames = F,
    class = "cell-border stripe compact hover",
    colnames = c( "Utilisateurs" = 1, 'Conversations' = 2),
    options = list(lengthChange = F,
                   ColumDefs = list( 
                     list( ClassName = "dt-center", targets = "_all")),
                   autoWidth = T, autFill = T,
                   lengthMenu = list(c(5, 8, 1), c("5", "8", "1") ),
                   language = list(
                     paginate = list(previous = "précédent", 'next' = "suivant"),
                     info = ""),
                   order = list(list(2, "asc")),    
                   searching = FALSE
                  ),
    caption = htmltools::tags$caption(
      style = "text-align : center; caption-side: top; color: orange;", "Table 1 : ", 
      htmltools::tags$strong("Les données whatsapp")) ) %>% 
  formatStyle(columns = c("Utilisateurs"), target = c("cell"),
              color = "navy", fontWeight = "bold")

#3) Fréquence des chats entre 2023-2024

f_chat %>% 
  group_by(saison) %>% 
  count(temps) %>% 
          filter(!is.na(saison)) %>% 
  ggplot(aes(x = temps, y = n, fill = saison))+
  geom_bar(stat = "identity", show.legend = T)+
  theme_bw()+
  labs(title = "Fréquence des chats sur la période 2023-2024",
       subtitle = "La période 2022-2024 est répartie en 5 périodes",
       x = "", y = "Nombre de messages")+
  theme(plot.title = element_text(color = "blue", size = 12,
                                  face = "bold", vjust = 0.9),
        plot.subtitle = element_text(color = "deeppink", size = 9,
                                     face = "bold", vjust = 0.5),
  axis.title.y = element_text(size = 10, vjust = 2.2),
  axis.ticks.y = element_blank(),
  axis.text.x = element_text(color = "black", size = 10),
  panel.border = element_rect(linetype = 1, linewidth = 0.9,
                              colour = 'black'),
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(linewidth = 0.9),
  legend.position = 'bottom',
  legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_date( date_labels = "%b %y", date_breaks = "3 months")


#4) Fréquence des chats hebdomadaire

Days <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi","samedi", "dimanche")

f_chat %>%
  group_by(jour_sem, nbr_jour) %>%
  count(sort = T) %>%
  ggplot(aes(nbr_jour , y = n, fill = n))+
  geom_bar(stat = 'identity', show.legend = F, width = 0.7)+
  labs(title = "Fréquence des chats hebdomadaire",
       x = "", y = "Nombre de conversations")+
  scale_fill_gradient2( low = "green", mid = "red",
                        high = "steelblue")+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(vjust = 2.2, size = 10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "black", size = 10),
        plot.background = element_rect(fill = "beige"),
        panel.border = element_rect(linetype = 1, linewidth = 0.9,
                                    colour = 'red'),
        panel.background = element_rect(linetype = 9,
                                        color = "red"))+
  scale_x_continuous(breaks = 1:7, labels = Days) 


#5) NOMBRE DE CHAT PAR UTILISATEUR

Count <- f_chat %>% 
  filter(!author == 234) %>% 
  group_by(author) %>%
  count(name = "Nombre de messages par personne", sort = T)

Count %>% 
  datatable(
    class = "compact hover cell-border stripe",
    rownames = F, 
    colnames = c("Utilisateurs" = 1),
    options = list(lengthMenu = list(c(4)), dom = "t", columnDefs = list(
      list( className = "dt-center", targets = "_all")
      )
    ),
    caption = htmltools::tags$caption(style = "caption-side: top; 
    text-align: center; color: orange", "Table 3 : ", 
    htmltools::tags$strong("Nombre de messages par personne"))
  ) %>% 
  formatStyle(names(Count[,2]), 
              background = styleColorBar(Count[, 2], rgb(0.20, 0.6, 0.50, 0.35)),
              backgroundSize = "98% 88%",
              backgroundRepeat = "no-repeat",
              # backgroundPosition = "center", 
              color = "red")

#6) Part des messages par utilisateur

f_chat %>% 
  group_by(author) %>% 
  filter(author >= 205) %>% 
  count() %>% 
  plot_ly(values = ~n, labels = ~factor(author), type = "pie", 
          textinfo = "label+percent", textposition = "outside",
          line = list(color = "white", width = 2), # hoverinfo = "label",
          outsidetextfont = list(color = "red"),
          marker = list(colors = c("gold", "cornflowerblue")), hole = 0.5)


#7) Fréquence de messages par heure

f_chat <- chat %>% 
  filter(!is.na(author)) 
  
f_chat %>%
  mutate(heure = hour(time)) %>% 
  count(heure) %>% 
  ggplot(aes(heure, y = n, fill = heure))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_x_continuous(breaks = 0:23)+
  labs(title = "Fréquence de messages par heure",
       x = "Heures", y = "Nombre de messages")+
  scale_fill_gradient2(low = "lightgreen", mid = "orange",
                       high = "darkblue")+
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 9, vjust = 2.2, 
                                    color = "black"),
        axis.title.x = element_text(size = 9, color = "black",
                                    hjust = 3.5),
        plot.background = element_rect(fill = "beige"),
        panel.background = element_rect(linetype = 9))+
  theme_bw()

#8) Emojis préférés par chaque utilisateur


f_chat$author <- f_chat$author %>% 
  str_replace_all("Deus E Capaz De Fazer", "Veny") %>% 
  str_replace_all("Elbert A", "Elbert") %>% 
  str_replace("Mum Of Love", "Autres") %>%  
  str_replace("Monsieur EKIRI Julien", "Autres") %>% 
  str_replace("Denis", "Denson")

f_chat %>% 
  unnest(emoji) %>% 
  group_by(author) %>%
  count(emoji, author, sort = T) %>% 
  slice_max(emoji, n = 6) %>% 
  ggplot(aes(reorder_within(emoji, n, author), y = n, fill = author))+
  geom_bar(stat = "identity", show.legend = F, width = 0.4)+
  coord_flip()+
  facet_wrap(~author, ncol = 3, scales = "free")+
  theme_bw()+
  labs(y = "", x = "")+
  scale_x_reordered()+
  theme(
    axis.text.x = element_text(color = "blue"),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "beige", linetype = 2),
  )

#9) Noms des émojis préférés par chaque utilisateur

f_chat %>% 
  unnest(emoji_name) %>% 
  group_by(author) %>% 
  count(emoji_name, sort = T) %>% 
  top_n(n = 10) %>% 
  ggplot(aes(reorder_within(emoji_name, n, author), y = n))+ 
  aes(fill = author) + 
  geom_col(width = 0.4, show.legend = F)+
  facet_wrap(facets = ~author, ncol = 2, scales = "free")+
  coord_flip()+
  scale_x_reordered()+
  theme_bw()+
  labs(x = "", y = "")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.ticks.y = element_blank())+
  scale_fill_manual(values = c( "#AAA009", "#900AAA", "#0FE999", "#1AE432", "#eda813"))


#10) Graphique 8** : Mots les plus utilisés

word_to_remove <- c("tu",'toi', 'te','ton', "n'","es",
                    'que','pas','omis', 'il', 'ta', 'oui', 'non',
                    'ok','médias', 'elle', 'as', 'des', 'de',"le",
                    'la', 'je','et', "en", 'ah', 'est', "ne", 
                    'du',"on", "sur","mais","où","là","rien",
                    "au","vas",'de', 'ce', "ça", "c'est", "à",
                    "qui","me", 'un', "une", "va", "sis", "les",
                    "j'ai", 'vais', "suis","si", "a","bon", "oh",
                    "quoi", "pour", "supprimé", "dans", "tout", "sa", "https", "vous", "nous", "tous", "ou", "été","vos", "avec", "par", "message", "plus", "ca", "dit", "cest")

word.f_chat <- f_chat %>% 
  unnest_tokens(output = word, input = text, to_lower = TRUE) %>% 
  filter(!word %in% word_to_remove)

word.f_chat %>% 
  count(word, sort = T) %>% 
  top_n(n = 15) %>% 
  ggplot(aes(reorder(word, n), y = n, fill = n))+
  geom_bar(stat = "identity", show.legend = F, width = 0.11) +
  geom_point(show.legend = F, size = 1.3, color = "black")+
  coord_flip() +
  theme_bw()+
  labs(x = "", y = "")+
  theme(axis.text.x = element_text(color = "blue"),
        axis.text.y = element_text(colour = "black"),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "beige"),
        panel.border = element_rect(linewidth = 1,
                                    linetype = 1))+
  scale_fill_gradient2(low = "lightgreen", 
                       mid = "orange", high = "blue")


#11) Nuages de mots

wordcloud::wordcloud(word.f_chat$word,
                     scale = c(5, 0.3), 
                     min.freq = 5, 
                     rot.per = 0.4,
                     max.words = 100,
                     colors = brewer.pal(11, name = "Set1"), 
                     random.order = F)


#12) Graphique 9** : Les 5 motsles plus utilisés par chaque utilisateur

word.f_chat %>% 
  filter(!word %in% word_to_remove) %>% 
  group_by(author) %>%
  count(word) %>% 
  top_n(5, n) %>%
  ggplot(aes(reorder_within(word, n, author), y = n))+ 
  aes(fill = author)+
  geom_bar(stat = "identity", width = 0.2, show.legend = F) +
  geom_point(show.legend = F,size = 1.3, color = "black")+
  facet_wrap(~author, ncol = 2, scales = "free")+
  coord_flip() +
  scale_x_reordered()+
    scale_fill_manual(values = c( "#AAA009", "#900AAA", 
                                  "#0FE999", "#1AE432", "#eda813"))+
  theme_bw()+
  labs(x = "", y = "")+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "beige"),
        panel.border = element_rect(color = "red", linewidth = 1,
                                    linetype = 1))

#13) Sentiment Table 1

Graphique 9** : Les 5 motsles plus utilisés par chaque utilisateur"}

word.f_chat %>% 
  filter(!word %in% word_to_remove) %>% 
  group_by(author) %>%
  count(word) %>% 
  top_n(5, n) %>%
  ggplot(aes(reorder_within(word, n, author), y = n))+ 
  aes(fill = author)+
  geom_bar(stat = "identity", width = 0.2, show.legend = F) +
  geom_point(show.legend = F,size = 1.3, color = "black")+
  facet_wrap(~author, ncol = 2, scales = "free")+
  coord_flip() +
  scale_x_reordered()+
    scale_fill_manual(values = c( "#AAA009", "#900AAA", 
                                  "#0FE999", "#1AE432", "#eda813"))+
  theme_bw()+
  labs(x = "", y = "")+
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "beige"),
        panel.border = element_rect(color = "red", linewidth = 1,
                                    linetype = 1))

#14) Sentiment Table 2

df_sent %>% 
  datatable( rownames = F,
             extensions = c("KeyTable", "ColReorder", "Buttons"),
             class = 'cell-border strip hover compact',
             options = list( dom = "t",
               keys = TRUE
               # colReorder = list(realtime = FALSE),
               # buttons = I("colvis")
               ),
             caption = htmltools::tags$caption(
               style = 'caption-side : top; color:orange; text-align: left; 
               font-stretch:2.5; line-height: 2.5;', 'Table 5 : ', 
               htmltools::tags$strong("les sentiments liés à nos conversations (10 ières ligne du tableau)")
        )
  ) %>% formatStyle(columns = names(df_sent), 
                    # target = c("cell", "row"),
                    background = styleColorBar(df_sent[, 2], "lightgreen"), 
                    backgroundSize = "100% 82%",
                    backgoundRepeat = "no-repeat",
                    backgroundPosition = "center",
                    color = "red"
  )

#15) Les types de sentiments les plus fréquents

df_sent %>% 
  ggplot(aes(sentiment , sentiment_count, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = F) + 
  labs(y = "Nombres de messages", x = "Les sentiments")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9))+
  theme_bw()+ 
  ggtitle("Les sentiments les plus saillant issus de nos conversations")+
  theme(plot.title = element_text(color = "navy", size = 10, face = "bold"),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(color = "red", vjust = 3),
        panel.background = element_rect(colour = "beige"))


#END

