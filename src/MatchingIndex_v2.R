library(stringr)
library(tidyverse)
library(plotly)
library(gridExtra)

#~~~SET PARAMETERS BEFORE RUNNING~~~
# !!! specify working directory (top directory of this repository)
setwd("/Users/amf198/Documents/GitHub/MatchingIndex/")

# this uses bash to replace the default google doc headers with the correct headers for this analysis. The only strict requirements are that
# the question weights are labled identically to the questions but with "weight_" as the very first characters. For instance
# the question "gender" would have the weight data labled "weight_gender"
# !!! specify survey data file on line 15
x <- 'cat data/MenteePairingSurveyHeaders.txt > data/2022MentorMenteePairingData.csv'
y <-'tail -n+2 data/MenteePairingSurvey2022.csv >> data/2022MentorMenteePairingData.csv'
system(x)
system(y)

# !!! specify year of analysis
year <- 2022

# !!! specify the group analyzed
group <- "MCDB" 

# !!! specify the relative degree to which it matters whether a mentee matches mentor preferences and vice versa
fractionMentorScore <- 0.2
fractionMenteeScore <- 1 - fractionMentorScore 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# import data frames
df <- as.data.frame(read.csv("data/2022MentorMenteePairingData.csv", header = T, sep = ",",row.names = NULL))

# remove timestamp and comment columns
df <- df[,-1]
df <- df[,-length(colnames(df))]

# ensures the dataframe is ordered by surveyee name to ensure consistency in analysis later
df_original <- df[order(df$name),]
df <- df[order(df$name),]

# this line is important to set the groups of compatible mentor/mentees 
df <- df_original[df_original$program == group,]

# ensures anyone that specified that they are a first year is lableled as a mentee and others labled as mentors
df$mentorshipStatus <- ifelse(df$year == "Incoming first year", "mentee", "mentor")

# sorts the names of mentors and mentees and collects them in their respective vectors
mentors <- sort(df[df$mentorshipStatus == "mentor",]$name)
mentees <-  sort(df[df$mentorshipStatus == "mentee",]$name)

# pulls out the columns with the "weight_" prefix for the weights 
dfWeights <- cbind(name = df[,1],df[,colnames(df)[str_detect(colnames(df), "weight_")]])
# lack of a preference response counts as 0 in question weight
dfWeights[is.na(dfWeights)] <- 0
dfWeightSums <- as.data.frame(cbind(name = dfWeights[,1], weightSum = (rowSums(dfWeights[,-1]))))
dfMenteeWeightSums <- dfWeightSums[dfWeightSums$name%in%mentees,]
dfMenteeWeightSums$weightSum <- as.numeric(dfMenteeWeightSums$weightSum)/10
dfMentorWeightSums <- dfWeightSums[!dfWeightSums$name%in%mentees,]
dfMentorWeightSums$weightSum <- as.numeric(dfMentorWeightSums$weightSum)/10

# pulls out the columns that dont have the "weight_" prefix
dfResponses <-  df[,colnames(df)[!str_detect(colnames(df), "weight_")]]

# expands the responses dataframe to make each persons response to a question it's own line
dfResponses <- pivot_longer(dfResponses, !name, names_to = "question", values_to = "response" )
# separates the mentee and mentor responses my comparing the entries to the mentee/mentor lists collected earlier
dfMenteeResponses <- dfResponses[dfResponses$name%in%mentees,]
dfMentorResponses <- dfResponses[!dfResponses$name%in%mentees,]

# does the same thing with weights as was done with the responses
dfWeights <- pivot_longer(dfWeights, !name, names_to = "question", values_to = "weight" )

# change 1-10 sliding scale to a fraction where 1 = most important and 0 = unimportant
dfWeights$weight <- dfWeights$weight/10


# removes the "weight_" prefixes so the responses and weights have the same name 
# to make them easier to combine later
dfWeights$question <- str_remove(dfWeights$question,"weight_")
dfMenteeWeights <- dfWeights[dfWeights$name%in%mentees,]
dfMentorWeights <- dfWeights[!dfWeights$name%in%mentees,]


# gets a list of the questions asked each mentor/mentee
questions <- sort(unique(dfResponses$question))

# removes questions that won't be used to calculate the matching index
questions <- questions[!questions%in%c("mentorshipStatus","year","program", "email")]

# makes an empty 3d array that will hold mentor and mentee similarity scores for each question
# fills it with zero's as a place holder
similarityMatrix <- array(0, c(length(mentors), length(mentees), length(questions)),
                          dimnames = list(mentors,mentees,
                                          questions))
# copies this empty array to later hold mentee and mentor preference weights
menteeWeightMatrix <- similarityMatrix
mentorWeightMatrix <- similarityMatrix

# Sets up a list to hold similarity matrix plots
plot_list = list()

# loops through each question to calculate similarity scores for each pair of mentors and mentees
for (q in questions){
  # pulls out mentor and mentee responses to question q into separate tibbles
  menteeResponsesToQ <- dfMenteeResponses[dfMenteeResponses$question == q,]
  mentorResponsesToQ <- dfMentorResponses[dfMentorResponses$question == q,]
  # loops through each mentee to examine their responses
  for (mentee in menteeResponsesToQ$name) {
    # pulls the response value for mentee "mentee" 
    menteeResponse <- menteeResponsesToQ[menteeResponsesToQ$name == mentee,]$response
    # separates mentee responses (if applicable) by the semicolon separating them
    menteeResponse <- unlist(str_split(menteeResponse, ";"))
    # pulls and breaks up all mentor responses for question "q"
    mentorResponsesSplit <- str_split(mentorResponsesToQ$response, ";")
    # names the mentor responses appropriately
    names(mentorResponsesSplit) <- mentorResponsesToQ$name
    # finds the cardinality for each set of mentor responses, i.e. how many responses did the mentor
    # give for question "q" 
    mentorResponseSizes <- lengths(mentorResponsesSplit)
    # breaks up the responses so I can compare mentor responses to mentee responses
    mentorResponsesSplit <- unlist(mentorResponsesSplit)
    # removes the # at the end of mentor names so I can combine them later
    names(mentorResponsesSplit) <- sub('[0-9]','', names(mentorResponsesSplit))
    # identify mentors that matched a least 1x with a "mentee" response  
    matches <- names(mentorResponsesSplit[which(mentorResponsesSplit%in%menteeResponse)])
    # provides a denominator for the similarity score so the maximum value a question an have is 1
    responseSizeNormalization <- ((length(menteeResponse) + mentorResponseSizes)/2)
    # makes an empty data frame to hold similarity scores and fills it with 0s
    similarityScores <- data.frame(mentors)
    similarityScores$count <- rep(0, length(mentors))
    # now we finally count the number of matches with the same mentor and assigns that value to the appropriate
    # mentor in the similarity scores dataframe
    similarityScores$count[which(mentors%in%names(table(matches)))] <- table(matches)
    # places the similarity score (adjusted by the max weight) into the similarity 
    # score matrix at the proper mentee and question location
    ## !! NOTE, IF THERE ARE 3+ MATCHES WITH RESPONSES, IT DEFAULTS TO A "PERFECT" MATCH SCORE
    similarityMatrix[,mentee,q] <- ifelse(similarityScores$count >= 3, 1, (similarityScores$count / responseSizeNormalization))
  }
  similarityMatrixSlice <- as.data.frame(similarityMatrix[,,q])
  similarityMatrixSlice$mentor <- rownames(similarityMatrixSlice)
  similarityMatrixSlice_Long <- pivot_longer(similarityMatrixSlice, !mentor, names_to = "mentee", values_to = "similarityScore")
  similarityMatrixSlice_Long <- similarityMatrixSlice_Long %>% mutate(text = paste0("Mentor: ", mentor, "\n", "Mentee: ", mentee, "\n", "similarityScore: ", round(similarityScore,2)))
  p <- ggplot(similarityMatrixSlice_Long, aes(mentor, mentee, fill=similarityScore, text=text)) + labs(title=paste0("Similarity Scores for Question: ", q)) +
    geom_tile() +
    geom_text(aes(label = round(similarityScore, 1)), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_fixed(ratio = 1)
  plot_list[[q]] = p
}

# saves a series of plots showing mentor/mentee similarty per question
pdf(paste0("res/SimilarityMatrices_", year, "_", group, ".pdf"))
for (q in questions) {
  print(plot_list[[q]])
}
dev.off()

# normalizes the similarity matrix according to the max possible contribution of mentee and mentor repsonses to final score
# this ensure the max score is 100% if mentor and mentee match each others preferences exactly and both strongly prefer the other to have
# similar responses to each question 

# builds an array  of weights mentees place on each question
for (q in questions){
  menteeWeightsForQ <- dfMenteeWeights[dfMenteeWeights$question == q,]
  for (Mo in mentors){
    menteeWeightMatrix[Mo,,q] <- menteeWeightsForQ$weight / as.numeric(dfMenteeWeightSums$weightSum)
  }
}

# builds an array  of weights mentors place on each question
for (q in questions){
  mentorWeightsForQ <- dfMentorWeights[dfMentorWeights$question == q,]
  for (Me in mentees){
    mentorWeightMatrix[,Me,q] <- mentorWeightsForQ$weight / as.numeric(dfMentorWeightSums$weightSum)
  }
}



# scales similarity matrix by weights and dampening factors for mentees/mentors per question
menteeWeightedSimilarityMatrix <- similarityMatrix * menteeWeightMatrix * fractionMenteeScore * 100
mentorWeightedSimilarityMatrix <- similarityMatrix * mentorWeightMatrix * fractionMentorScore * 100

# compresses the slices of the array by adding all of the weighted similarity scores for each mentor/mentee pair
# across all questions
# the outputs here could be considered the one-sided matching indices for mentors to mentee expectations
# in the "menteeWeightedSimilarityMatrix" and vice versa for the "mentorWeightedSimilarityMatrix"
menteeMatchingIndexMatrix <- apply(menteeWeightedSimilarityMatrix, c(1,2), FUN=sum)
mentorMatchingIndexMatrix <- apply(mentorWeightedSimilarityMatrix, c(1,2), FUN=sum)

# generates the full matching index matrix
matchingIndexMatrix <- menteeMatchingIndexMatrix + mentorMatchingIndexMatrix


# reformats matrices describing the overall matching indicies as well as the 
# mentor-sided and mentee-sided matching indicies
matchingIndexMatrix <- as.data.frame(matchingIndexMatrix)
matchingIndexMatrix$mentor <- rownames(matchingIndexMatrix)
matchingIndexMatrix_Long <- pivot_longer(matchingIndexMatrix, !mentor, names_to = "mentee", values_to = "MatchingIndex")
matchingIndexMatrix_Long <- matchingIndexMatrix_Long %>% mutate(text = paste0("Mentor: ", mentor, "\n", "Mentee: ", mentee, "\n", "MatchingIndex: ", round(MatchingIndex,2)))

mentorMatchingIndexMatrix <- as.data.frame(mentorMatchingIndexMatrix)
mentorMatchingIndexMatrix$mentor <- rownames(mentorMatchingIndexMatrix)
mentorMatchingIndexMatrix_Long <- pivot_longer(mentorMatchingIndexMatrix, !mentor, names_to = "mentee", values_to = "mentorMatchingIndex")
mentorMatchingIndexMatrix_Long <- mentorMatchingIndexMatrix_Long %>% mutate(text = paste0("Mentor: ", mentor, "\n", "Mentee: ", mentee, "\n", "mentorMatchingIndex: ", round(mentorMatchingIndex,2)))

menteeMatchingIndexMatrix <- as.data.frame(menteeMatchingIndexMatrix)
menteeMatchingIndexMatrix$mentor <- rownames(menteeMatchingIndexMatrix)
menteeMatchingIndexMatrix_Long <- pivot_longer(menteeMatchingIndexMatrix, !mentor, names_to = "mentee", values_to = "menteeMatchingIndex")
menteeMatchingIndexMatrix_Long <- menteeMatchingIndexMatrix_Long %>% mutate(text = paste0("Mentor: ", mentor, "\n", "Mentee: ", mentee, "\n", "menteeMatchingIndex: ", round(menteeMatchingIndex,2)))

# plotting summary matching index metric
p <- ggplot(matchingIndexMatrix_Long, aes(mentor, mentee, fill=MatchingIndex, text=text)) +
  geom_tile() +
  geom_text(aes(label = round(MatchingIndex, 1)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_fixed(ratio = 1)
pdf(paste0("res/MatchingIndices", "_", year, "_", group, ".pdf"))
p
dev.off()

# plotting mentee matching index (i.e. how mentors match mentee preferences)
menteeMatchingIndexMatrix_Long$menteeMatchingIndex <- menteeMatchingIndexMatrix_Long$menteeMatchingIndex/fractionMenteeScore
p <- ggplot(menteeMatchingIndexMatrix_Long, aes(mentor, mentee, fill=menteeMatchingIndex, text=text)) +
  geom_tile() +
  geom_text(aes(label = round(menteeMatchingIndex, 1)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_fixed(ratio = 1)

pdf(paste0("res/MatchingIndices_MenteePreferencesOnly", "_", year, "_", group, ".pdf"))
p
dev.off()

# plotting mentor matching index (i.e. how mentees match mentor preferences)
mentorMatchingIndexMatrix_Long$mentorMatchingIndex <- mentorMatchingIndexMatrix_Long$mentorMatchingIndex/fractionMentorScore
p <- ggplot(mentorMatchingIndexMatrix_Long, aes(mentor, mentee, fill=mentorMatchingIndex, text=text)) +
  geom_tile() +
  geom_text(aes(label = round(mentorMatchingIndex, 1)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1))  + coord_fixed(ratio = 1)
pdf(paste0("res/MatchingIndices_MentorPreferencesOnly", "_", year, "_", group, ".pdf"), height = length(mentors), width = length(mentees))
p
dev.off()


# We also rank the best-matching mentors for each mentee and export the summary by csv
matchingIndexMatrix_Long <- matchingIndexMatrix_Long[order(matchingIndexMatrix_Long$MatchingIndex, decreasing = TRUE),]
matchingIndexMatrix_Long <- matchingIndexMatrix_Long[order(matchingIndexMatrix_Long$mentee, decreasing = TRUE),]
matchingIndexMatrix_Long$mentorMatchRank <- rep(1:length(mentors), length(mentees))
matchingIndexMatrix_Long <- matchingIndexMatrix_Long[order(matchingIndexMatrix_Long$MatchingIndex, decreasing = TRUE),]
write.csv(matchingIndexMatrix_Long[, colnames(matchingIndexMatrix_Long) != "text"], file = "res/matchingIndices.csv")


