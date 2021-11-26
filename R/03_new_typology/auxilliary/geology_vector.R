# lookup table to transform highly resolved geology or soil data into calcareous, siliceous or mixed. 

calc <- c(
        #- bedrock 
        "Limestones and marls", 
        "Limestones",
        "Limestones and clays",
        "Limestones and sands",
        "Marlstones and clays",
        "Marls",
        "Marlstones",
        "Marlstones and sands",
        "Clays",
        "Claystones and clays",
        "Conglomerates",
        "Conglomerates and clays",
        "Conglomerates and sands",
        "Marbles",
        "Sandstones",
        "Sandstones and clays",
        "Sandstones and marls",
        "Sandstones and sands",
        #- soil 
        "?-Calcaric Arenosol", 
        "?-Calcaric Fluvisol", 
        "?-Calcic Chernozem", 
        "Calcaric Cambisol",
        "Calcaric Fluvisol",
        "Calcaric Regosol"
)

sil <- c(
        "Gneisses",
        "Quartzites", 
        "Quartzites",
        "Shales",
        "Volcanic rocks",
        "Dystric Cambisol",
        "?-Dystric Cambisol"
)

unknown <- c(
        "Gravels","Plutonic rocks","Sands", "Alisol", "Rendzic Leptosol", NA, "?-Chromic Luvisol", "?-Eutric Cambisol", "?-Luvic Phaeozem", "Alisol", "", 
        "Cambic Podzol", "Chromic Luvisol", "?-Eutric Fluvisol", "Eutric Cambisol","Eutric Fluvisol","Eutric Podzoluvisol" ,"Eutric Regosol"  ,    "Eutric Vertisol", 
        "Haplic Calcisol", "Haplic Greyzem", "Haplic Phaeozem", "Haplic Solonchak", "Humic Cambisol", "Lithic Leptosol", 
        "Luvic Phaeozem", "Mollic Leptosol", "Petric Calcisol", "Stagnic Luvisol", "Stagnic Phaeozem", "Umbric Andosol", "Umbric Regosol","Water body"
)

mixed <- c(
        "Haplic Luvisol"
)

geology.vector <- list()
geology.vector$calcareous <- calc
geology.vector$siliceous <- sil
geology.vector$unknown <- unknown
geology.vector$mixed <- mixed
