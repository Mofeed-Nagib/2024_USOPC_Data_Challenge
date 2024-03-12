### Full name data cleaning
later_scores[later_scores$fullname == "Miguel Aquino",]$country <- "PUR"
later_scores[later_scores$fullname == "Naveen Daries",]$country <- "RSA"
later_scores[later_scores$fullname == "Nikita Simonov",]$country <- "AZE"

# Cleaning by Country
# ALB
  # N/A
# ALG
  # N/A
# ARG
  # Daniel Angel Villafane
  later_scores[later_scores$fullname == "Daniel Villafane" |
               later_scores$fullname == "Daniel Villafañe",]$fullname <- "Daniel Angel Villafane"
  # Julian Ezequiel Jato
  later_scores[later_scores$fullname == "Julian Jato",]$fullname <- "Julian Ezequiel Jato"
  # Luca Valentino Alfieri
  later_scores[later_scores$fullname == "Luca Alfieri",]$fullname <- "Luca Valentino Alfieri"
  # Nicole Iribarne Aparicio
  later_scores[later_scores$fullname == "Nicole Iribarne",]$fullname <- "Nicole Iribarne Aparicio"
  # Rocio Selene Saucedo
  later_scores[later_scores$fullname == "Rocio Saucedo",]$fullname <- "Rocio Selene Saucedo"
# ARM
  # N/A
# ARU
  # N/A
# AUS
  # Aiden Michael Frick
  later_scores[later_scores$fullname == "Aiden Frick",]$fullname <- "Aiden Michael Frick"
  # Clay Mason Stephens
  later_scores[later_scores$fullname == "Clay Masonstephens",]$fullname <- "Clay Mason Stephens"
  # Emily Whitehead
  later_scores[later_scores$fullname == "Emily Whitehead",]$gender <- "w"
  # Georgia Godwin
  later_scores[later_scores$fullname == "Georgia Godwin",]$gender <- "w"
  # Kiplin Morrish Smith
  later_scores[later_scores$fullname == "Kiplin Smith",]$fullname <- "Kiplin Morrish Smith"
# AUT
  # Alissa Moerz
  later_scores[later_scores$fullname == "Alissa Mörz",]$fullname <- "Alissa Moerz"
  # Carina Kroell
  later_scores[later_scores$fullname == "Carina Kröll",]$fullname <- "Carina Kroell"
  # Charlize Moerz
  later_scores[later_scores$fullname == "Charlize Mörz",]$fullname <- "Charlize Moerz"
  # Severin Kranzlmueller
  later_scores[later_scores$fullname == "Severin Kranzlmüller",]$fullname <- "Severin Kranzlmueller"
  # Vinzenz Johann Hock
  later_scores[later_scores$fullname == "Vinzenz Höck" |
               later_scores$fullname == "Vinzenz Hoeck",]$fullname <- "Vinzenz Johann Hock"
# AZE
  # N/A
# BAN
  # N/A
# BAR
  # Anya Kaelin Pilgrim
  later_scores[later_scores$fullname == "Anya Pilgrim",]$fullname <- "Anya Kaelin Pilgrim"
  # Erin Gianna Pinder
  later_scores[later_scores$fullname == "Erin Pinder",]$fullname <- "Erin Gianna Pinder"
  # Olivia Reagan Mafes Kelly
  later_scores[later_scores$fullname == "Olivia Kelly",]$fullname <- "Olivia Reagan Mafes Kelly"
# BEL
  # Aberdeen O'Driscoll
  later_scores[later_scores$fullname == "Aberdeen O Driscoll" |
               later_scores$fullname == "Aberdeen O'driscol",]$fullname <- "Aberdeen O'Driscoll"
  # Luka Van Den Keybus
  later_scores[later_scores$fullname == "Luka Keybus",]$fullname <- "Luka Van Den Keybus"
  # Noemie Louon
  later_scores[later_scores$fullname == "Noémie Louon",]$fullname <- "Noemie Louon"
# BLR
  # N/A
# BOL
  later_scores[later_scores$fullname == "Diana Vasquez",]$fullname <- "Diana Stephany Vasquez Seoane"
# BRA
  # Bernardo Actos Miranda
  later_scores[later_scores$fullname == "Bernardo Actos Andrade De Souza Miranda" |
               later_scores$fullname == "Bernardo Miranda",]$fullname <- "Bernardo Actos Miranda"
  # Carolyne Mercer Winche Pedro
  later_scores[later_scores$fullname == "Carolyne Pedro",]$fullname <- "Carolyne Mercer Winche Pedro"
  # Julia Das Neves Botega Soares
  later_scores[later_scores$fullname == "Julia Soares",]$fullname <- "Julia Das Neves Botega Soares"
  # Lucas De Souza Bitencourt
  later_scores[later_scores$fullname == "Lucas Souza Bitencourt" |
                 later_scores$fullname == "Lucas Bitencourt",]$fullname <- "Lucas De Souza Bitencourt"
  # Luisa Gomes Maia
  later_scores[later_scores$fullname == "Luisa Maia",]$fullname <- "Luisa Gomes Maia"
  # Patrick Sampaio Correa
  later_scores[later_scores$fullname == "Patrick Correa" |
               later_scores$fullname == "Patrick Sampaio",]$fullname <- "Patrick Sampaio Correa"
  # Tomas Rodrigues Florencio
  later_scores[later_scores$fullname == "Tomas Florencio",]$fullname <- "Tomas Rodrigues Florencio"
  # Yuri Guimaraes
  later_scores[later_scores$fullname == "Yuri Guimarães",]$fullname <- "Yuri Guimaraes"
# BUL
  # N/A
# CAN
  # Cassandra Paige Lee
  later_scores[later_scores$fullname == "Cassandra Lee",]$fullname <- "Cassandra Paige Lee"
  # Elsabeth Black
  later_scores[later_scores$fullname == "Ellie Black",]$fullname <- "Elsabeth Black"
  # Emma Spence
  later_scores[later_scores$fullname == "Emma Spence",]$gender <- "w"
  # Jenna Lalonde
  later_scores[later_scores$fullname == "Jenna Lalonde",]$gender <- "w"
  # Laurie Denommee
  later_scores[later_scores$fullname == "Laurie Denommée",]$fullname <- "Laurie Denommee"
  later_scores[later_scores$fullname == "Laurie Denommee",]$gender <- "w"
  # Samuel Zakutney
  later_scores[later_scores$fullname == "Sam Zakutney",]$fullname <- "Samuel Zakutney"
  # Sydney Leslie Turner
  later_scores[later_scores$fullname == "Sydney Turner",]$fullname <- "Sydney Leslie Turner"
  # Zachary Nathaniel Clay
  later_scores[later_scores$fullname == "Zachary Clay",]$fullname <- "Zachary Nathaniel Clay"
# CAY
  # N/A
# CCS
  # N/A
# CHI
  # Antonia Marihuan Rubio
  later_scores[later_scores$fullname == "Antonia Marihuan",]$fullname <- "Antonia Marihuan Rubio"
  # Franchesca Antonella Santi
  later_scores[later_scores$fullname == "Franchesca Santi",]$fullname <- "Franchesca Antonella Santi"
  # Ignacio Javier Varas
  later_scores[later_scores$fullname == "Ignacio Varas",]$fullname <- "Ignacio Javier Varas"
  # Joel Alvarez Vergara
  later_scores[later_scores$fullname == "Joel Gonzalo Vergara" |
               later_scores$fullname == "Joel Alvarez",]$fullname <- "Joel Alvarez Vergara"
  # Luciano Mauricio Letelier
  later_scores[later_scores$fullname == "Luciano Letelier",]$fullname <- "Luciano Mauricio Letelier"
  # Makarena Daisy Pinto Adasme
  later_scores[later_scores$fullname == "Makarena Pinto Adasme",]$fullname <- "Makarena Daisy Pinto Adasme"
# CHN
  # N/A
# COL
  # Andres Martinez Moreno
  later_scores[later_scores$fullname == "Andres Martinez",]$fullname <- "Andres Martinez Moreno"
  # Angelica Mesa Vergara
  later_scores[later_scores$fullname == "Angelica Mesa",]$fullname <- "Angelica Mesa Vergara"
  # Daira Gisell Lamadrid
  later_scores[later_scores$fullname == "Daira Lamadrid",]$fullname <- "Daira Gisell Lamadrid"
  # Dilan Andres Jimenez Giraldo
  later_scores[later_scores$fullname == "Dilan Jimenez Giraldo" |
               later_scores$fullname == "Dilan Jimenez",]$fullname <- "Dilan Andres Jimenez Giraldo"
  # Ginna Escobar Betancur
  later_scores[later_scores$fullname == "Ginna Escobar",]$fullname <- "Ginna Escobar Betancur"
  # Jose Manuel Martinez Moreno
  later_scores[later_scores$fullname == "Jose Martinez",]$fullname <- "Jose Manuel Martinez Moreno"
  # Jossimar Orlando Calvo Moreno
  later_scores[later_scores$fullname == "Jossimar Orlando Calvo Moreno Jo",]$fullname <- "Jossimar Orlando Calvo Moreno"
  # Kristopher Steven Bohorquez Cantor
  later_scores[later_scores$fullname == "Kristopher Bohorquez",]$fullname <- "Kristopher Steven Bohorquez Cantor"
  # Maria Jose Villegas Jimenez
  later_scores[later_scores$fullname == "Maria Villegas",]$fullname <- "Maria Jose Villegas Jimenez"
  # Sergio Andres Vargas Rincon
  later_scores[later_scores$fullname == "Sergio Vargas",]$fullname <- "Sergio Andres Vargas Rincon"
  # Yiseth Eliana Valenzuela Astudillo
  later_scores[later_scores$fullname == "Yiseth Valenzuela",]$fullname <- "Yiseth Eliana Valenzuela Astudillo"
# CRC
  # Alberto Perez Fernandez
  later_scores[later_scores$fullname == "Alberto Pérez Fernández" |
               later_scores$fullname == "Alberto Perez",]$fullname <- "Alberto Perez Fernandez"
  # Anelena Rodriguez Johanning
  later_scores[later_scores$fullname == "Anelena Rodriguez",]$fullname <- "Anelena Rodriguez Johanning"
  # Franciny Morales Barquero
  later_scores[later_scores$fullname == "Franciny Morales",]$fullname <- "Franciny Morales Barquero"
  # Jhossua Ariel Corrales Castro
  later_scores[later_scores$fullname == "Jhossua Corrales",]$fullname <- "Jhossua Ariel Corrales Castro"
  # Rachel Rodriguez Miranda
  later_scores[later_scores$fullname == "Rachel Rodriguez",]$fullname <- "Rachel Rodriguez Miranda"
# CRO
  # N/A
# CUB
  # Alejandro De La Cruz Gato
  later_scores[later_scores$fullname == "Alejandro De La Cruz",]$fullname <- "Alejandro De La Cruz Gato"
  # Angelissa Ponce Villalpando
  later_scores[later_scores$fullname == "Angelissa Ponce",]$fullname <- "Angelissa Ponce Villalpando"
  # Diorges Adriano Escobar Olmo
  later_scores[later_scores$fullname == "Diorges Escobar",]$fullname <- "Diorges Adriano Escobar Olmo"
  # Jose Carlos Escandon Marin
  later_scores[later_scores$fullname == "Jose Carlos Escandon" |
               later_scores$fullname == "Jose Carlos Escandón Marín" |
               later_scores$fullname == "Jose Escandon" ,]$fullname <- "Jose Carlos Escandon Marin"
  # Pablo Harold Pozo Decos
  later_scores[later_scores$fullname == "Pablo Pozo",]$fullname <- "Pablo Harold Pozo Decos"
  # Yohendry Villaverde Mederos
  later_scores[later_scores$fullname == "Yohendry Villaverde",]$fullname <- "Yohendry Villaverde Mederos"
# CYP
  # Tatiana Bachurina
  later_scores[later_scores$fullname == "Tatiana Bachurina",]$gender <- "w"
# CZE
  # N/A
# DEN
  # N/A
# DOM
  # Ana Patricia Fuentes De La Cruz
  later_scores[later_scores$fullname == "Ana Fuentes",]$fullname <- "Ana Patricia Fuentes De La Cruz"
  # Audrys Nin Reyes
  later_scores[later_scores$fullname == "Audrys Nin",]$fullname <- "Audrys Nin Reyes"
  # Camil Betances Reyes
  later_scores[later_scores$fullname == "Camil Betances",]$fullname <- "Camil Betances Reyes"
  # Jabiel De Jesus Polanco Acosta
  later_scores[later_scores$fullname == "Jabiel Polanco",]$fullname <- "Jabiel De Jesus Polanco Acosta"
  # Jeordy Ramirez Castro
  later_scores[later_scores$fullname == "Jeordy Ramirez",]$fullname <- "Jeordy Ramirez Castro"
  # Leandro Geronimo Pena Santana
  later_scores[later_scores$fullname == "Leandro Geronimo Santana" |
               later_scores$fullname == "Leandro Pena" ,]$fullname <- "Leandro Geronimo Pena Santana"
  # Wilfry Manuel Contreras
  later_scores[later_scores$fullname == "Wilfry Contreras",]$fullname <- "Wilfry Manuel Contreras"
  # Yamilet Pena Abreu
  later_scores[later_scores$fullname == "Yamilet Abreu" |
               later_scores$fullname == "Yamilet Pena",]$fullname <- "Yamilet Pena Abreu"
# EAI
  # N/A
# ECU
  # Alais Natasha Perea Ponce
  later_scores[later_scores$fullname == "Alais Perea",]$fullname <- "Alais Natasha Perea Ponce"
  # Ashley Nayerly Bohorquez Romero
  later_scores[later_scores$fullname == "Ashley Bohorquez",]$fullname <- "Ashley Nayerly Bohorquez Romero"
  # Cesar Armando Lopez Chavez
  later_scores[later_scores$fullname == "Cesar Lopez",]$fullname <- "Cesar Armando Lopez Chavez"
  # Johnny Adrian Valencia Zambrano
  later_scores[later_scores$fullname == "Johnny Adrian Valencia",]$fullname <- "Johnny Adrian Valencia Zambrano"
# EGY
  # Ahmed El Maraghy
  later_scores[later_scores$fullname == "Ahmed Elmaraghy",]$fullname <- "Ahmed El Maraghy"
# ESA
  # Alexa Gabriela Grande Franco
  later_scores[later_scores$fullname == "Alexa Grande",]$fullname <- "Alexa Gabriela Grande Franco"
  # Carmina Isabella Chavez Martinez
  later_scores[later_scores$fullname == "Carmina Chavez",]$fullname <- "Carmina Isabella Chavez Martinez"
  # Pablo Natanael Velasquez Candray
  later_scores[later_scores$fullname == "Pablo Velasquez",]$fullname <- "Pablo Natanael Velasquez Candray"
  # Paola Massiel Ruano Barahona
  later_scores[later_scores$fullname == "Paola Ruano",]$fullname <- "Paola Massiel Ruano Barahona"
# ESP
  # Adria Vera Mora
  later_scores[later_scores$fullname == "Adria Vera",]$fullname <- "Adria Vera Mora"
  # Dietmar Reinhardt Codina
  later_scores[later_scores$fullname == "Dietmar Reinhardt"|
               later_scores$fullname == "Dietmar V. Reinhardt Codina",]$fullname <- "Dietmar Reinhardt Codina"
  # Jorge Rubio Cerro
  later_scores[later_scores$fullname == "Jorge Rubio",]$fullname <- "Jorge Rubio Cerro"
  # Joshua Jack Williams Meehan
  later_scores[later_scores$fullname == "Joshua Jack Williams",]$fullname <- "Joshua Jack Williams Meehan"
  # Lorena Medina Cobos
  later_scores[later_scores$fullname == "Lorena Medina",]$fullname <- "Lorena Medina Cobos"
  # Maia Llacer Sirera
  later_scores[later_scores$fullname == "Maia Llacer",]$fullname <- "Maia Llacer Sirera"
  # Nicolau Mir Rossello
  later_scores[later_scores$fullname == "Nicolau Mir",]$fullname <- "Nicolau Mir Rossello"
  # Oriol Rifa Pedreno
  later_scores[later_scores$fullname == "Oriol Rifa",]$fullname <- "Oriol Rifa Pedreno"
  # Pau Jimenez Fernandez
  later_scores[later_scores$fullname == "Pau Jimenez I Fernandez"|
               later_scores$fullname == "Pau Jimenez",]$fullname <- "Pau Jimenez Fernandez"
  # Paula Raya Artigas
  later_scores[later_scores$fullname == "Paula Raya I Artigas"|
               later_scores$fullname == "Paula Raya",]$fullname <- "Paula Raya Artigas"
# FIN
  # Jimi Päivänen
  later_scores[later_scores$fullname == "Jimi Pävänen"|
               later_scores$fullname == "Jimi Päivänen",]$fullname <- "Jimi Paivanen"
  # Malla Alexandra Montell
  later_scores[later_scores$fullname == "Malla Montell",]$fullname <- "Malla Alexandra Montell"
  # Saara Katariina Kokko
  later_scores[later_scores$fullname == "Saara Kokko",]$fullname <- "Saara Katariina Kokko"
  # Sani Maekelae
  later_scores[later_scores$fullname == "Sani Mäkelä",]$fullname <- "Sani Maekelae"
  # Sara Sofia Loikas
  later_scores[later_scores$fullname == "Sara Loikas",]$fullname <- "Sara Sofia Loikas"
  # Tarmo Tuomas Kanerva
  later_scores[later_scores$fullname == "Tarmo Kanerva",]$fullname <- "Tarmo Tuomas Kanerva"
# FRA
  # Lea Franceries
  later_scores[later_scores$fullname == "Léa Franceries",]$fullname <- "Lea Franceries"
  # Leo Saladino
  later_scores[later_scores$fullname == "Léo Saladino",]$fullname <- "Leo Saladino"
  # Melanie De Jesus Dos Santos
  later_scores[later_scores$fullname == "Mélanie De Jesus Dos Santos"|
               later_scores$fullname == "Melanie Jesus Santos",]$fullname <- "Melanie De Jesus Dos Santos"
  # Morgane Osyssek Reimer
  later_scores[later_scores$fullname == "Morgane Osyssek",]$fullname <- "Morgane Osyssek Reimer"
# GBR
  # Max Whitlock Obe
  later_scores[later_scores$fullname == "Max Whitlock",]$fullname <- "Max Whitlock Obe"
  # Pavel Karnejenko
  later_scores[later_scores$fullname == "Pavel Karenejenko",]$fullname <- "Pavel Karnejenko"
  # Poppy Grace Stickler
  later_scores[later_scores$fullname == "Poppy Stickler",]$fullname <- "Poppy Grace Stickler"
  # Ruby Evans
  later_scores[later_scores$fullname == "Ruby Evan",]$fullname <- "Ruby Evans"
# GEO
  # N/A
# GER
  # Anna Lena Koenig
  later_scores[later_scores$fullname == "Anna-Lena König",]$fullname <- "Anna Lena Koenig"
  # Carlo Hoerr
  later_scores[later_scores$fullname == "Carlo Horr"|
               later_scores$fullname == "Carlo Hörr",]$fullname <- "Carlo Hoerr"
  # Chiara Summer Moiszi
  later_scores[later_scores$fullname == "Chiara Moiszi",]$fullname <- "Chiara Summer Moiszi"
  # Emma Leonie Malewski
  later_scores[later_scores$fullname == "Emma Malewski",]$fullname <- "Emma Leonie Malewski"
  # Karina Schoenmaier
  later_scores[later_scores$fullname == "Karina Schönmaier",]$fullname <- "Karina Schoenmaier"
  # Lea Marie Quaas
  later_scores[later_scores$fullname == "Lea Quaas",]$fullname <- "Lea Marie Quaas"
  # Pauline Schaefer Betz
  later_scores[later_scores$fullname == "Pauline Schäfer",]$fullname <- "Pauline Schaefer Betz"
# GBR
  # Alice Kinsella
  later_scores[later_scores$fullname == "Alice Kinsella",]$gender <- "w"
  # Cara Kennedy
  later_scores[later_scores$fullname == "Cara Kennedy",]$gender <- "w"
  # Eilidh Gorrell
  later_scores[later_scores$fullname == "Eilidh Gorrell",]$gender <- "w"
  # Emily Bremner
  later_scores[later_scores$fullname == "Emily Bremner",]$gender <- "w"
  # Georgia Mae Fenton
  later_scores[later_scores$fullname == "Georgia Mae Fenton",]$gender <- "w"
  # Jea Maracha
  later_scores[later_scores$fullname == "Jea Maracha",]$gender <- "w"
  # Mia Evans
  later_scores[later_scores$fullname == "Mia Evans",]$gender <- "w"
  # Ondine Achampong
  later_scores[later_scores$fullname == "Ondine Achampong",]$gender <- "w"
  # Poppy Grace Stickler
  later_scores[later_scores$fullname == "Poppy Grace Stickler",]$gender <- "w"
  # Shannon Archer
  later_scores[later_scores$fullname == "Shannon Archer",]$gender <- "w"
  # Tara Donnelly
  later_scores[later_scores$fullname == "Tara Donnelly",]$gender <- "w"
# GRE
  # Areti Paraskevi Pagoni
  later_scores[later_scores$fullname == "Areti Pagoni",]$fullname <- "Areti Paraskevi Pagoni"
# GUA
  # N/A
# HAI
  # N/A
# HKG
  # Cheuk Lam Charlie Chan
  later_scores[later_scores$fullname == "Cheuk Lam Chan",]$fullname <- "Cheuk Lam Charlie Chan"
  # Hiu Ying Angel Wong
  later_scores[later_scores$fullname == "Hiu Ying Wong",]$fullname <- "Hiu Ying Angel Wong"
  # Man Hin Frankie Lee
  later_scores[later_scores$fullname == "Man Hin Lee",]$fullname <- "Man Hin Frankie Lee"
# HUN
  # N/A
# INA
  # Joseph Judah Hatoguan
  later_scores[later_scores$fullname == "Joseph Hatoguan",]$fullname <- "Joseph Judah Hatoguan"
  # Larasati Rengganis
  later_scores[later_scores$fullname == "Larasati Regganis",]$fullname <- "Larasati Rengganis"
# IND
  # Pranati Nayak
  later_scores[later_scores$fullname == "Pranati Nayak",]$gender <- "w"
  # Ruthuja Nataraj
  later_scores[later_scores$fullname == "Ruthuja Nataraj",]$gender <- "w"
# IRI
  # N/A
# IRL
  # Ewan McAteer
  later_scores[later_scores$fullname == "Ewan Mc Ateer"|
               later_scores$fullname == "Ewan Mcateer"|
               later_scores$fullname == "Mc Ewan Ateer",]$fullname <- "Ewan McAteer"
  # Rhys McClenaghan
  later_scores[later_scores$fullname == "Mc Rhys Clenaghan"|
               later_scores$fullname == "Rhys Mc Clenaghan"|
               later_scores$fullname == "Rhys Mcclenaghan",]$fullname <- "Rhys McClenaghan"
# ISL
  # Agust Ingi Davidsson
  later_scores[later_scores$fullname == "Agust Davidsson",]$fullname <- "Agust Ingi Davidsson"
  # Dagur Kari Olafsson
  later_scores[later_scores$fullname == "Dagur Olafsson",]$fullname <- "Dagur Kari Olafsson"
  # Hildur Maja Gudmundsdottir
  later_scores[later_scores$fullname == "Hildur Gudmundsdottir",]$fullname <- "Hildur Maja Gudmundsdottir"
  # Jonas Ingi Thorisson
  later_scores[later_scores$fullname == "Jonas Thorisson",]$fullname <- "Jonas Ingi Thorisson"
  # Margret Lea Kristinsdottir
  later_scores[later_scores$fullname == "Margret Kristinsdottir",]$fullname <- "Margret Lea Kristinsdottir"
# ISR
  # N/A
# ITA
  # N/A
# JAM
  # Caleb Fischle Faulk
  later_scores[later_scores$fullname == "Caleb Faulk",]$fullname <- "Caleb Fischle Faulk"
  # Danyella Richards
  later_scores[later_scores$fullname == "Danyella Richards",]$gender <- "w"
  # Elel Diliza Wahrmann Baker
  later_scores[later_scores$fullname == "Elel Wahrmann Baker",]$fullname <- "Elel Diliza Wahrmann Baker"
  # Michael James Reid
  later_scores[later_scores$fullname == "Michael Reid",]$fullname <- "Michael James Reid"
  # Tyesha Tinekai Mattis
  later_scores[later_scores$fullname == "Tyesha Mattis",]$fullname <- "Tyesha Tinekai Mattis"
# JEY
  # N/A
# JOR
  # N/A
# JPN
  # N/A
# KAZ
  # Dmitriy Patanin
  later_scores[later_scores$fullname == "Dmitry Patanin",]$fullname <- "Dmitriy Patanin"
# KGZ
  # N/A
# KOR
  # Ga-Ram Bae
  later_scores[later_scores$fullname == "Garam Bae",]$fullname <- "Ga-Ram Bae"
  # Jin-Seong Yun
  later_scores[later_scores$fullname == "Jinseong Yun",]$fullname <- "Jin-Seong Yun"
  # Yo-Seop Jeon
  later_scores[later_scores$fullname == "Yoseop Jeon",]$fullname <- "Yo-Seop Jeon"
# KSA
  # N/A
# LAT
  # Dmitrijs Mickevics
  later_scores[later_scores$fullname == "Dimitrijs Mickevics",]$fullname <- "Dmitrijs Mickevics"
# LTU
  # N/A
# LUX
  # N/A
# MAR
  # N/A
# MAS
  # Li Wen Rachel Yeoh
  later_scores[later_scores$fullname == "Li Wen Yeoh",]$fullname <- "Li Wen Rachel Yeoh"
# MEX
  # Ahtziri Viridiana Sandoval
  later_scores[later_scores$fullname == "Ahtziri Sandoval",]$fullname <- "Ahtziri Viridiana Sandoval"
  # Alexa Citlali Moreno Medina
  later_scores[later_scores$fullname == "Alexa Moreno Medina"|
               later_scores$fullname == "Alexa Moreno",]$fullname <- "Alexa Citlali Moreno Medina"
  # Alonso Perez Torres
  later_scores[later_scores$fullname == "Alonso Perez",]$fullname <- "Alonso Perez Torres"
  # Fabian De Luna Hernandez
  later_scores[later_scores$fullname == "Fabian De Luna"|
               later_scores$fullname == "Fabián De Luna Hernández"|
               later_scores$fullname == "Fabian Luna",]$fullname <- "Fabian De Luna Hernandez"
  # Isaac Nunez Farfan
  later_scores[later_scores$fullname == "Isaac Farfa"|
               later_scores$fullname == "Isaac Nunez"|
               later_scores$fullname == "Isaac Nuñez",]$fullname <- "Isaac Nunez Farfan"
  # Josue Juarez
  later_scores[later_scores$fullname == "Josue Juarez Juarez",]$fullname <- "Josue Juarez"
  # Maximiliano Galicia Flores
  later_scores[later_scores$fullname == "Maximiliano Galicia",]$fullname <- "Maximiliano Galicia Flores"
  # Natalia Isabel Escalera
  later_scores[later_scores$fullname == "Natalia Escalera",]$fullname <- "Natalia Isabel Escalera"
  # Rodrigo Gomez Grosso
  later_scores[later_scores$fullname == "Rodrigo Gomez"|
               later_scores$fullname == "Rodrigo Grosso",]$fullname <- "Rodrigo Gomez Grosso"
# MGL
  # N/A
# MLT
  # N/A
# MON
  # N/A
# NED
  # Jermain Gruenberg
  later_scores[later_scores$fullname == "Jermain Grünberg",]$fullname <- "Jermain Gruenberg"
  # Loran De Munck
  later_scores[later_scores$fullname == "Loran Munck",]$fullname <- "Loran De Munck"
  # Martijn De Veer
  later_scores[later_scores$fullname == "Martijn Veer",]$fullname <- "Martijn De Veer"
  # Tisha Manouk Gijs Volleman
  later_scores[later_scores$fullname == "Tisha Volleman",]$fullname <- "Tisha Manouk Gijs Volleman"
  # Vera Van Pol
  later_scores[later_scores$fullname == "Vera Pol",]$fullname <- "Vera Van Pol"
  # Wout Johan Alexander Teillers
  later_scores[later_scores$fullname == "Wout Teillers",]$fullname <- "Wout Johan Alexander Teillers"
# NOR
  # Juliane Toessebro
  later_scores[later_scores$fullname == "Juliane Tøssebro",]$fullname <- "Juliane Toessebro"
  # Julie Madsoe
  later_scores[later_scores$fullname == "Julie Madsø",]$fullname <- "Julie Madsoe"
  # Peder Funderud Skogvang
  later_scores[later_scores$fullname == "Peder Skogvang",]$fullname <- "Peder Funderud Skogvang"
# NZL
  # Jorden O’Connell-Inns
  later_scores[later_scores$fullname == "Jorden Oconnell-Inns",]$fullname <- "Jorden O’Connell-Inns"
  # Keira Rolston-Larking
  later_scores[later_scores$fullname == "Keira Rolston Larking",]$fullname <- "Keira Rolston-Larking"
  # Samuel Dick
  later_scores[later_scores$fullname == "Sam Dick"|
               later_scores$fullname == "Samual Dick",]$fullname <- "Samuel Dick"
  # William Fu-Allen
  later_scores[later_scores$fullname == "William Fuallen"|
               later_scores$fullname == "William Fu Allen",]$fullname <- "William Fu-Allen"
# PAK
  # N/A
# PAN
  # Hillary Alexandra Heron Soto
  later_scores[later_scores$fullname == "Hillary Heron Soto"|
               later_scores$fullname == "Hillary Heron",]$fullname <- "Hillary Alexandra Heron Soto"
  # Karla Andrea Navas Boyd
  later_scores[later_scores$fullname == "Karla Navas Boyd"|
               later_scores$fullname == "Karla Navas",]$fullname <- "Karla Andrea Navas Boyd"
  # Kevin Espinosa Castillo
  later_scores[later_scores$fullname == "Kevin Espinosa",]$fullname <- "Kevin Espinosa Castillo"
  # Lana Raquel Herrera Rodriguez
  later_scores[later_scores$fullname == "Lana Herrera Rodriguez"|
               later_scores$fullname == "Lana Raquel Herrera"|
               later_scores$fullname == "Lana Herrera",]$fullname <- "Lana Raquel Herrera Rodriguez"
  # Lucia Carolina Paulino Lopez
  later_scores[later_scores$fullname == "Lucia Paulino Lopez",]$fullname <- "Lucia Carolina Paulino Lopez"
  # Richard Ameth Atencio Higinio
  later_scores[later_scores$fullname == "Richard Atencio",]$fullname <- "Richard Ameth Atencio Higinio"
  # Sebastian Andres Sue Dominguez
  later_scores[later_scores$fullname == "Sebastián Andrés Sue Domínguez"|
               later_scores$fullname == "Sebastian Sue",]$fullname <- "Sebastian Andres Sue Dominguez"
  # Valentina Brostella Arias
  later_scores[later_scores$fullname == "Valentin Brostella"|
               later_scores$fullname == "Valentina Brostella",]$fullname <- "Valentina Brostella Arias"
# PER
  # Edward Daniel Alarcon Luque
  later_scores[later_scores$fullname == "Daniel Alarcon",]$fullname <- "Edward Daniel Alarcon Luque"
  # Edward Andre Gonzales Rivas
  later_scores[later_scores$fullname == "Edward Gonzales",]$fullname <- "Edward Andre Gonzales Rivas"
  # Nicolas Andres Garfias Chan
  later_scores[later_scores$fullname == "Nicolas Garfias",]$fullname <- "Nicolas Andres Garfias Chan"
# PHI
  # Carlos Edriel Yulo
  later_scores[later_scores$fullname == "Carlos Yulo",]$fullname <- "Carlos Edriel Yulo"
  # Jan Gwynn Timbang
  later_scores[later_scores$fullname == "Jann Gwynn Timbang",]$fullname <- "Jan Gwynn Timbang"
  # Juancho Miguel Besana
  later_scores[later_scores$fullname == "Juancho Besana",]$fullname <- "Juancho Miguel Besana"
  # Kylee Ann Kvamme
  later_scores[later_scores$fullname == "Kylee Kvamme",]$fullname <- "Kylee Ann Kvamme"
# POL
  # Sebastian Norbert Gawronski
  later_scores[later_scores$fullname == "Sebastian Gawronski",]$fullname <- "Sebastian Norbert Gawronski"
# POR
  # Ana Filipa Martins
  later_scores[later_scores$fullname == "Filipa Martins",]$fullname <- "Ana Filipa Martins"
  # Jose Pedro Mendes Nogueira
  later_scores[later_scores$fullname == "Jose Nogueira",]$fullname <- "Jose Pedro Mendes Nogueira"
# PRK
  # N/A
# PUR
  # Ainhoa Sofia Herrero Lugo
  later_scores[later_scores$fullname == "Ainhoa Herrero Lugo",]$fullname <- "Ainhoa Sofia Herrero Lugo"
  # Alejandra Sofia Alvarez Diaz
  later_scores[later_scores$fullname == "Alejandra Alvarez Diaz",]$fullname <- "Alejandra Sofia Alvarez Diaz"
  # Andres Josue Perez Gines
  later_scores[later_scores$fullname == "Andres Josue Perez Ginez"|
               later_scores$fullname == "Andres Perez",]$fullname <- "Andres Josue Perez Gines"
  # Jose Antonio Lopez Martinez
  later_scores[later_scores$fullname == "Jose Lopez",]$fullname <- "Jose Antonio Lopez Martinez"
  # Karelys Diaz Davila
  later_scores[later_scores$fullname == "Karelys Diaz",]$fullname <- "Karelys Diaz Davila"
  # Katyna Kamila Alicea Cardona
  later_scores[later_scores$fullname == "Katyna Alicea",]$fullname <- "Katyna Kamila Alicea Cardona"
  # Miguel Angel Aquino III
  later_scores[later_scores$fullname == "Miguel Angel Aquino Iii"|
               later_scores$fullname == "Miguel Aquino"|
               later_scores$fullname == "Miguel Aquino Iii",]$fullname <- "Miguel Angel Aquino III"
  # Natalia Gabriela Delgado Lopez
  later_scores[later_scores$fullname == "Natalia Delgado",]$fullname <- "Natalia Gabriela Delgado Lopez"
  # Nelson Alberto Guilbe Morales
  later_scores[later_scores$fullname == "Nelson Guilbe Morales"|
               later_scores$fullname == "Nelson Guilbe",]$fullname <- "Nelson Alberto Guilbe Morales"
  # Pablo Jose Perez Martinez
  later_scores[later_scores$fullname == "Pablo Perez",]$fullname <- "Pablo Jose Perez Martinez"
  # Stella Loren Diaz Muniz
  later_scores[later_scores$fullname == "Stella Loren Diaz"|
               later_scores$fullname == "Stella Diaz Muniz"|
               later_scores$fullname == "Stella Diaz",]$fullname <- "Stella Loren Diaz Muniz"
  # Sydney Tatiana Barros
  later_scores[later_scores$fullname == "Sydney Barros",]$fullname <- "Sydney Tatiana Barros"
# QAT
  # Al-Harith Rakan
  later_scores[later_scores$fullname == "Al Harith Rakan"|
               later_scores$fullname == "Rakah Al Harithi"|
               later_scores$fullname == "Rakan Al Harithi"|
               later_scores$fullname == "Rakan Alharith",]$fullname <- "Al-Harith Rakan"
# ROU
  # Andrei Vasile Muntean
  later_scores[later_scores$fullname == "Andrei Muntean",]$fullname <- "Andrei Vasile Muntean"
  # Razvan-Denis Marc
  later_scores[later_scores$fullname == "Razvan Denis Marc",]$fullname <- "Razvan-Denis Marc"
  # Toma Roland Modoianu-Zseder
  later_scores[later_scores$fullname == "Toma Roland Modoianu Zseder"|
               later_scores$fullname == "Toma Modoianu-Zseder"|
               later_scores$fullname == "Toma Modoianu Zseder",]$fullname <- "Toma Roland Modoianu-Zseder"
# RSA
  # Caitlin Rooskrantz
  later_scores[later_scores$fullname == "Caitlin Rooskrantz",]$gender <- "w"
  # Naveen Daries
  later_scores[later_scores$fullname == "Naveen Daries",]$gender <- "w"
  # Shante Koti
  later_scores[later_scores$fullname == "Shanté Koti",]$fullname <- "Shante Koti"
# RUS
  # N/A
# SGP
  # Emma En Lin Yap
  later_scores[later_scores$fullname == "Emma Yap",]$fullname <- "Emma En Lin Yap"
  later_scores[later_scores$fullname == "Emma En Lin Yap",]$gender <- "w"
  # Nadine Joy Nathan
  later_scores[later_scores$fullname == "Nadine Joy Nathan",]$gender <- "w"
  # Wei An Terry Tay
  later_scores[later_scores$fullname == "Terry Tay Wei An",]$fullname <- "Wei An Terry Tay"
# SLO
  later_scores[later_scores$fullname == "Teja Belak",]$country <- "SLO"
# SRB
  # N/A
# SRI
  # Amaya Sithumini Kalukottage
  later_scores[later_scores$fullname == "Amaya Kalukottage",]$fullname <- "Amaya Sithumini Kalukottage"
  # Milka Gehani Elpitiya Badalge Dona
  later_scores[later_scores$fullname == "Milka Gehani",]$fullname <- "Milka Gehani Elpitiya Badalge Dona"
# SUI
  # Dominic Daniel Tamsel
  later_scores[later_scores$fullname == "Dominic Tamsel",]$fullname <- "Dominic Daniel Tamsel"
  # Lilli Leanne Habisreutinger
  later_scores[later_scores$fullname == "Lilli Habisreutinger",]$fullname <- "Lilli Leanne Habisreutinger"
  # Noe Samuel Seifert
  later_scores[later_scores$fullname == "Noe Seifert",]$fullname <- "Noe Samuel Seifert"
# SVK
  # N/A
# SWE
  # Karl Idesjoe
  later_scores[later_scores$fullname == "Karl Idesjö",]$fullname <- "Karl Idesjoe"
  # Kim Vanstrom
  later_scores[later_scores$fullname == "Kim Vanstroem"|
               later_scores$fullname == "Kim Wanström",]$fullname <- "Kim Vanstrom"
# SYR
  # Mohamad Khalil
  later_scores[later_scores$fullname == "Mohamed Khalil",]$fullname <- "Mohamad Khalil"
# THA
  # Ananya Belle Patanakul
  later_scores[later_scores$fullname == "Ananya Patanakul",]$fullname <- "Ananya Belle Patanakul"
  # Sasiwimon Mueangphuan
  later_scores[later_scores$fullname == "Sasiwimion Mueangphuan",]$fullname <- "Sasiwimon Mueangphuan"
# TPE
  # Chia-Hung Tang
  later_scores[later_scores$fullname == "Chia Hung Tang",]$fullname <- "Chia-Hung Tang"
  # Chih-Kai Lee
  later_scores[later_scores$fullname == "Chih Kai Lee"|
               later_scores$fullname == "Chih Lee",]$fullname <- "Chih-Kai Lee"
  # Guan-Yi Lin
  later_scores[later_scores$fullname == "Guan Yi Lin"|
               later_scores$fullname == "Guan Lin",]$fullname <- "Guan-Yi Lin"
  # Hua-Tien Ting
  later_scores[later_scores$fullname == "Hua Tien Ting"|
               later_scores$fullname == "Hua Ting",]$fullname <- "Hua-Tien Ting"
  # Hsiang Han Mai Liu
  later_scores[later_scores$fullname == "Liu Hsiang-Han Mai",]$fullname <- "Hsiang Han Mai Liu"
  # Pin-Ju Lai
  later_scores[later_scores$fullname == "Pin Lai"|
               later_scores$fullname == "Pin Ju Lai",]$fullname <- "Pin-Ju Lai"
  # Sing Fen Wu
  later_scores[later_scores$fullname == "Sing Wu",]$fullname <- "Sing Fen Wu"
  # Wei-Sheng Tseng
  later_scores[later_scores$fullname == "Wei Sheng Tseng"|
               later_scores$fullname == "Wei Tseng",]$fullname <- "Wei-Sheng Tseng"
  # Yen-Chang Huang
  later_scores[later_scores$fullname == "Yen Chang Huang",]$fullname <- "Yen-Chang Huang"
  # Yi-Chen Lin
  later_scores[later_scores$fullname == "Yi Chen Lin"|
               later_scores$fullname == "Yi Lin",]$fullname <- "Yi-Chen Lin"
  # Yi-Chun Liao
  later_scores[later_scores$fullname == "Yi Chun Liao",]$fullname <- "Yi-Chun Liao"
  # Yu-Jan Shiao
  later_scores[later_scores$fullname == "Yu Jan Shiao"|
               later_scores$fullname == "Yu Shiao",]$fullname <- "Yu-Jan Shiao"
  # Yuan-Hsi Hung
  later_scores[later_scores$fullname == "Yuan Hsi Hung",]$fullname <- "Yuan-Hsi Hung"
# TTO
  # Annalise Becca Newman Achee
  later_scores[later_scores$fullname == "Annalise Newman Achee",]$fullname <- "Annalise Becca Newman Achee"
# TUR
  # Ahmet Onder
  later_scores[later_scores$fullname == "Ahmet Önder",]$fullname <- "Ahmet Onder"
  # Derin Tanriyasukur
  later_scores[later_scores$fullname == "Derin Tanriyasükür",]$fullname <- "Derin Tanriyasukur"
  # Goksu Uctas Sanli
  later_scores[later_scores$fullname == "Göksu Üctas Sanli",]$fullname <- "Goksu Uctas Sanli"
  # Mehmet Ayberk Kosak
  later_scores[later_scores$fullname == "Mehmet Ayberk"|
               later_scores$fullname == "Mehmet Kosak",]$fullname <- "Mehmet Ayberk Kosak"
  # Yunus Emre Gundogdu
  later_scores[later_scores$fullname == "Yunus Gundogdu"|
               later_scores$fullname == "Yunus Gündogdu",]$fullname <- "Yunus Emre Gundogdu"
# UKR
  # N/A
# USA
  # Curran Michael Phillips
  later_scores[later_scores$fullname == "Curran Phillips",]$fullname <- "Curran Michael Phillips"
  # Frederick Nathaniel Richard
  later_scores[later_scores$fullname == "Frederick Richard"|
               later_scores$fullname == "Fred Richard",]$fullname <- "Frederick Nathaniel Richard"
  # Ian Hunter Skirkey
  later_scores[later_scores$fullname == "Ian Skirkey",]$fullname <- "Ian Hunter Skirkey"
  # Joscelyn Michelle Roberson
  later_scores[later_scores$fullname == "Joscelyn Roberson",]$fullname <- "Joscelyn Michelle Roberson"
  # Joshua Andrew Karnes
  later_scores[later_scores$fullname == "Joshua Karnes",]$fullname <- "Joshua Andrew Karnes"
  # Khoi Alexander Young
  later_scores[later_scores$fullname == "Khoi Young",]$fullname <- "Khoi Alexander Young"
  # Matthew Cormier
  later_scores[later_scores$fullname == "Matt Cormier",]$fullname <- "Matthew Cormier"
  # Nola Rhianne Matthews
  later_scores[later_scores$fullname == "Nola Matthews",]$fullname <- "Nola Rhianne Matthews"
  # Shane Michael Wiskus
  later_scores[later_scores$fullname == "Shane Wiskus",]$fullname <- "Shane Michael Wiskus"
  # Taylor Troy Christopulos
  later_scores[later_scores$fullname == "Taylor Christopulos",]$fullname <- "Taylor Troy Christopulos"
  # Yul Kyung Tae Moldauer
  later_scores[later_scores$fullname == "Yul Moldauer",]$fullname <- "Yul Kyung Tae Moldauer"
# UZB
  # N/A
# VEN
  # Adickxon Gabriel Trejo Basalo
  later_scores[later_scores$fullname == "Adickxon Trejo Basalo"|
               later_scores$fullname == "Adickxon Trejo",]$fullname <- "Adickxon Gabriel Trejo Basalo"
  # Deborah Mersedes Salmina Arroyo
  later_scores[later_scores$fullname == "Deborah Salmina",]$fullname <- "Deborah Mersedes Salmina Arroyo"
  # Edward Rafael Rolin Carrasco
  later_scores[later_scores$fullname == "Edward Rolin",]$fullname <- "Edward Rafael Rolin Carrasco"
  # Getsemary Auxiliadora Martinez Figuera
  later_scores[later_scores$fullname == "Getsemary Martinez",]$fullname <- "Getsemary Auxiliadora Martinez Figuera"
  # Milca Andreina Leon Andrade
  later_scores[later_scores$fullname == "Milca Leon",]$fullname <- "Milca Andreina Leon Andrade"
  # Valeria Daniela Arraiz Reyes
  later_scores[later_scores$fullname == "Valeria Arraiz",]$fullname <- "Valeria Daniela Arraiz Reyes"
  # Victor Manuel Betancourt Quintana
  later_scores[later_scores$fullname == "Victor Betancourt",]$fullname <- "Victor Manuel Betancourt Quintana"
  # Yefferson Gregorio Anton Yeguez
  later_scores[later_scores$fullname == "Yefferson Anton",]$fullname <- "Yefferson Gregorio Anton Yeguez"
# VIE
  # Thanh Tung Le
  later_scores[later_scores$fullname == "Thanh Tùng Lê",]$fullname <- "Thanh Tung Le"
  

# df <- later_scores[later_scores$country == sort(unique(later_scores$country))[##],]
# sort(unique(df$fullname))  

# for (j in seq_along(unique(later_scores$country))) {
#   df <- later_scores[later_scores$country == sort(unique(later_scores$country))[j],]
#   for (i in seq_along(unique(df$fullname))) {
#     df_2 <- df[df$fullname == sort(unique(df$fullname))[i],]
#     
#     if (length(unique(df_2$gender)) > 1) {
#       print(sort(unique(df$fullname))[i])
#     }
#     if (length(unique(df_2$country)) > 1) {
#       print(sort(unique(df$fullname))[i])
#     }
#   }  
# }
