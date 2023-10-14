## This file includes the data cleaning for each qualifying individual gymnast
## whose country did not qualify

# Clean up qualifying men's data
  # Milad Karimi
    # no changes needed
  # Artem Dolgopyat
    # no changes needed
  # Artur Davtyan
    later_scores[grepl("Artur Davtyan", later_scores$fullname), ]$country <- "ARM"
  # Krisztofer Meszaros
    # no changes needed
  # Lee Jun-ho
    # no data available before 2023 World Championships
  # Diogo Soares
    # no changes needed
  # Luka Van den Keybus
    later_scores$firstname <- gsub(".*Luka.*", "Luka", later_scores$firstname)
    later_scores$lastname <- gsub(".*Keybus.*", "Van Den Keybus", later_scores$lastname)
    later_scores$fullname <- gsub(".*Keybus.*", "Luka Van Den Keybus", later_scores$fullname)
  # Andrei Vasile Muntean
    # no changes needed
  # Rhys McClenaghan
    later_scores$firstname <- gsub(".*Rhys.*", "Rhys", later_scores$firstname)
    later_scores$lastname <- gsub(".*Clenaghan.*", "McClenaghan", later_scores$lastname)
    later_scores$fullname <- gsub(".*Clenaghan.*", "Rhys McClenaghan", later_scores$fullname)
    later_scores[grepl("Rhys McClenaghan", later_scores$fullname), ]$country <- "IRL"
  # Eleftherios Petrounias
    # no changes needed
  # Kevin Penev
    later_scores[later_scores$lastname == "Penev",]$country <- "BUL"
  # Noah Kuavita
    # no changes needed
  # Tin Srbic
    # no changes needed
    
# Clean up qualifying women's data
  # Kaylia Nemour
    # no data available before 2023 World Championships
  # Pauline Schaefer Betz
    # no changes needed
  # Alexa Moreno
    # no changes needed
  # Filipa Martins
    # no changes needed
  # Aleah Finnegan
    # no data available before 2023 World Championships
  # Bettina Lili Czifra
    # no changes needed
  # Alba Petisco
    # no changes needed
  # Anna Lashchevska
    # no changes needed
  # Lena Bickel
    # no changes needed
  # Hillary Heron
    later_scores$lastname <- gsub(".*Heron.*", "Heron Soto", later_scores$lastname)
    later_scores$fullname <- gsub(".*Hillary Heron.*", "Hillary Heron Soto", later_scores$fullname)
  # Caitlin Rooskrantz
    later_scores[later_scores$lastname == "Rooskrantz",]$gender <- "w"
  # Sona Artamonova
    # no changes needed
  # Lihie Raz
    #.no changes needed
  # Lucija Hribar
    # no changes neeeded
  # Csenge Maria Bacskay
    later_scores$firstname <- gsub(".*Csenge.*", "Csenge Maria", later_scores$firstname)
    later_scores$fullname <- gsub(".*Csenge.*", "Csenge Maria Bacskay", later_scores$fullname)
  # Ahtziri Sandoval
    # no changes needed
  # Ana Perez
    # no changes needed
  # Sarah Voss
    # no changes needed

# Search later_scores for cases that match    
# later_scores[grepl("Rooskrantz", later_scores$fullname), ]
