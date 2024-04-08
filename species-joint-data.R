source("functions.R")

#Overall data
data = read.csv(paste0(getwd(),"/files/Data.csv"))
dataArray = makeArray(data)

#Data filtered to species
humanData  = data %>% filter(Species == "homo")
baboonData = data %>% filter(Species == "papio")
chimpData  = data %>% filter(Species == "pan")

humanArray  = makeArray(humanData)
baboonArray = makeArray(baboonData)
chimpArray  = makeArray(chimpData)

#Data filtered to species+joint
  #Human
  human.CubCal   = humanData %>% filter(Joint == " CubCal") %>% makeArray()
  human.Mt5CubAP = humanData %>% filter(Joint == " Mt5CubAP") %>% makeArray()
  human.NavCub   = humanData %>% filter(Joint == " NavCub") %>% makeArray()
  human.NavTal   = humanData %>% filter(Joint == " NavTal") %>% makeArray()
  human.TalCal   = humanData %>% filter(Joint == " TalCal") %>% makeArray()

  #Baboon
  baboon.CubCal   = baboonData %>% filter(Joint == " CubCal") %>% makeArray() 
  #baboon.Mt5CubAP = baboonData %>% filter(Joint == " Mt5CubAP") #Observations do not exist
  baboon.NavCub   = baboonData %>% filter(Joint == " NavCub") %>% makeArray()
  baboon.NavTal   = baboonData %>% filter(Joint == " NavTal") %>% makeArray()
  baboon.TalCal   = baboonData %>% filter(Joint == " TalCal") %>% makeArray()
  
  #Chimp
  chimp.CubCal   = chimpData %>% filter(Joint == " CubCal") %>% makeArray()
  chimp.Mt5CubAP = chimpData %>% filter(Joint == " Mt5CubAP") %>% makeArray()
  chimp.NavCub   = chimpData %>% filter(Joint == " NavCub") %>% makeArray()
  chimp.NavTal   = chimpData %>% filter(Joint == " NavTal") %>% makeArray()
  chimp.TalCal   = chimpData %>% filter(Joint == " TalCal") %>% makeArray()

  