library(ggplot2)
library(palmerpenguins)
library(dplyr)

pinguins <- penguins
str(pinguins)
summary(pinguins)
pinguins <- na.omit(pinguins)

ggplot(data = pinguins, mapping = aes(x = island, fill = species)) + 
  geom_bar()+ 
  theme_minimal()+
  labs(x = "Ilha",
       y = "Frequencia",
       title = "Especies em cada ilha",
       fill = "Especies")+
  scale_fill_manual(values =
  c("Adelie" = "#fbb4ae",
    "Chinstrap" = "#b3cde3",
    "Gentoo" = "#ccebc5")) + 
  facet_wrap(~sex)

ggplot(pinguins, aes(x = sex, y = body_mass_g, fill = sex)) +
  geom_boxplot()+
  facet_wrap(~species)+
  #scale_x_discrete(labels = c("female" = "", "male" = ""))+
  labs( y = "peso (g)",
        x = "",
        fill = "sexo")+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_manual(values =
                        c("female" = "#fbb4ae",
                          "male" = "#b3cde3"),
                    labels = c("female" = "fêmea", "male" = "macho"))#+
  #scale_fill_discrete(labels = c("female" = "fêmea", "male" = "macho")) #Retorna values para o padrão

ggplot(pinguins, aes(x = sex, y = bill_length_mm)) +
  geom_boxplot()+
  facet_wrap(~species)+
  scale_x_discrete(labels = c("female" = "fêmea", "male" = "macho"))

ggplot(pinguins, aes(x = sex, y = bill_depth_mm)) +
  geom_boxplot()+
  facet_wrap(~species)+
  scale_x_discrete(labels = c("female" = "fêmea", "male" = "macho"))

ggplot(pinguins, aes(x = body_mass_g, y = flipper_length_mm, color = species, size = bill_length_mm ))+
  #geom_point(alpha = 0.5)
  geom_jitter() #Pertuba um pouco a localização da variavel (diminuir sobreposições)
