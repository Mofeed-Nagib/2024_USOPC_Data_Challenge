## This file includes the data cleaning for each qualifying individual gymnast
## whose country did not qualify

# Clean up qualifying men's data
  # Milad Karimi
    # no changes needed
  # Artem Dolgopyat
    # no changes needed
  # Artur Davtyan
    # no changes needed
  # Krisztofer Meszaros
    # no changes needed
  # Junho Lee
    # no changes needed
  # Diogo Soares
    # no changes needed
  # Luka Van den Keybus
    # no changes needed
  # Andrei Vasile Muntean
    # no changes needed
  # Rhys McClenaghan
    # no changes needed
  # Eleftherios Petrounias
    # no changes needed
  # Kevin Penev
    # no changes needed
  # Noah Kuavita
    # no changes needed
  # Tin Srbic
    # no changes needed
  # Audrys Nin Reyes
    # no changes needed
  # Carlos Yulo
    # no changes needed
    
# Clean up qualifying women's data
  # Kaylia Nemour
    # no changes needed
  # Pauline Schaefer Betz
    # no changes needed
  # Alexa Citlali Moreno Medina
    # no changes needed
  # Filipa Martins
    # no changes needed
  # Aleah Finnegan
    # no changes needed
  # Bettina Lili Czifra
    # no changes needed
  # Alba Petisco
    # no changes needed
  # Anna Lashchevska
    # no changes needed
  # Lena Bickel
    # no changes needed
  # Hillary Alexandra Heron Soto
    # no changes needed
  # Caitlin Rooskrantz
    later_scores[later_scores$lastname == "Rooskrantz",]$gender <- "w"
  # Sona Artamonova
    # no changes needed
  # Lihie Raz
    # no changes needed
  # Lucija Hribar
    # no changes needed
  # Csenge Maria Bacskay
    # no changes needed
  # Ahtziri Viridiana Sandoval
    # no changes needed
  # Ana Perez
    # no changes needed
  # Sarah Voss
    # no changes needed
  # Luisa Blanco
    # no changes needed
  # Rifda Irfanaluthfi
    # no changes needed


# Search later_scores for cases that match    
# later_scores[grepl("Csenge", later_scores$fullname), ]
