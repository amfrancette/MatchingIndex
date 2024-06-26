#!/usr/bin/env Rscript
# args = commandArgs(trailingOnly=TRUE)

# if needed, uncomment below to install prerequisite packages
# BiocManager::install("stringr") # used for some string manipulations
# BiocManager::install("tidyverse") # essential
# BiocManager::install("plotly") 
# BiocManager::install("gridExtra") # great for combining mutliple plots
# BiocManager::install("pheatmap") # great for hierarchical clustering
# BiocManager::install("clue") # this is used to implement the hungarian algorithm to calculate the best possible matches minimizing cost
# loads packages used in analysis
library(stringr)
library(tidyverse)
library(plotly)
library(gridExtra)
library(pheatmap)
library(clue)

#~~~SET PARAMETERS BEFORE RUNNING~~~
# !!! specify working directory (top directory of this repository)
setwd("/Users/amf198/Documents/ExtraCurriculars/GSO/PeerMentoringCommittee/Fall2024_MentorMenteePairing/")

# just iterate this up each year as long as the rest of the survey name is the same
year <- "2023"
surveyData <- paste0("Fall", year, "_PeerMentorMenteePairingSurvey.csv")
dir.create("res/other/")
dir.create("res/csv/")
dir.create("res/summary/")

# This loop will run on EE and MCDB pairing separately
group <- "MCDB"
for (group in c("E&E", "MCDB")){
  print(group)
  # group <- "MCDB"
  # this uses bash to replace the default google doc headers with the correct headers for this analysis. The only strict requirements are that
  # the question weights are labeled identically to the questions but with "weight_" as the very first characters. For instance
  # the question "gender" would have the weight data labeled "weight_gender"
  # !!! specify survey data file on line 15
  system(paste0("cat data/MenteePairingSurveyHeaders.txt > data/", year, "_SurveyResults.csv"))
  system(paste0("tail -n+2 data/", surveyData, " >> data/", year , "_SurveyResults.csv"))
  
  
  # !!! specify the relative degree to which it matters whether a mentee matches mentor preferences and vice versa
  # fractionMenteeScore = 0.8 means their scores contribute a 80% to the final score, 
  fractionMentorScore <- 0.2
  fractionMenteeScore <- 1 - fractionMentorScore 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # import data frame
  df <- as.data.frame(read.csv(paste0("data/", year, "_SurveyResults.csv"), header = T, sep = ",",row.names = NULL))
  
  # remove timestamp and comment columns
  df <- df[,-1]
  df <- df[,-length(colnames(df))]
  
  # ensures the dataframe is ordered by surveyee name to ensure consistency in analysis later
  df_original <- df[order(df$name),]
  df <- df[order(df$name),]
  
  # this line is important to set the groups of compatible mentor/mentees 
  df <- df_original[df_original$program == group,]
  
  # ensures anyone that specified that they are a first year is labeled as a mentee and others labeled as mentors
  df$mentorshipStatus <- ifelse(df$year == "Incoming first year", "mentee", "mentor")
  
  # sorts the names of mentors and mentees and collects them in their respective vectors
  mentors <- sort(df[df$mentorshipStatus == "mentor",]$name)
  mentees <- sort(df[df$mentorshipStatus == "mentee",]$name)
  
  # print the number of mentees and mentors
  print(paste0("For ", group, " there are ", length(mentees), " registered mentees"))
  print(paste0("For ", group, " there are ", length(mentors), " registered mentors"))
  
  # pulls out the columns with the "weight_" prefix for the weights 
  dfWeights <- cbind(name = df[,1],df[,colnames(df)[str_detect(colnames(df), "weight_")]])

  # lack of a preference response counts as default 0 in question weight
  dfWeights[is.na(dfWeights)] <- 0
  # Weight sums refers to the total of all of the weights provided by each surveyee
  # This is used later to normalize the relative value of each question in the context of other responses
  # for instance if someone only thinks gender is somewhat important (5/10) and parental status is very imporant (9/10)
  # but thinks nothing else is important (0/10 for all else), the total weight for this surveyee is 5+9=14. This means that 
  # 9/10 response holds 9/14=64% of all of the "importance" for this surveyee. 
  # If they think everything is exactly as important, then there is no preference, but if they think some categories as unimportant
  # it increases the value of those that they think *are* important. 
  dfWeightSums <- as.data.frame(cbind(name = dfWeights[,1], weightSum = (rowSums(dfWeights[,-1]))))
  dfMenteeWeightSums <- dfWeightSums[dfWeightSums$name%in%mentees,]
  dfMenteeWeightSums$weightSum <- as.numeric(dfMenteeWeightSums$weightSum)/10
  dfMentorWeightSums <- dfWeightSums[!dfWeightSums$name%in%mentees,]
  dfMentorWeightSums$weightSum <- as.numeric(dfMentorWeightSums$weightSum)/10
  
  # pulls out the columns that don't have the "weight_" prefix
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
  
  # gets a list of the questions asked to each mentor/mentee
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
      # pulls the response value for the mentee specified in the loop 
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
      # provides a denominator for the similarity score so the maximum value a question can have is 1
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
  pdf(paste0("res/other/SimilarityMatrices_", year, "_", group, ".pdf"))
  for (q in questions) {
    print(plot_list[[q]])
  }
  dev.off()
  
  # normalizes the similarity matrix according to the max possible contribution of mentee and mentor repsonses to final score
  # this ensures the max score is 100% if mentor and mentee match each others preferences exactly and both strongly prefer the other to have
  # similar responses to each question 

  # builds an array  of weights mentees place on each question
  for (q in questions){
    menteeWeightsForQ <- dfMenteeWeights[dfMenteeWeights$question == q,]
    for (Mo in mentors){
      menteeWeightMatrix[Mo,,q] <- menteeWeightsForQ$weight / as.numeric(dfMenteeWeightSums$weightSum)
    }
  }
  
  
  
  # builds an array of weights that mentors place on each question
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
  
  # needed to prevent 0/0 from mentees or mentors that score 0 for everything, defaults to max  
  # compatibility for all questions 
  menteeMatchingIndexMatrix[is.nan(menteeMatchingIndexMatrix)] <- fractionMenteeScore * 100
  mentorMatchingIndexMatrix[is.nan(mentorMatchingIndexMatrix)] <- fractionMentorScore * 100
  
  
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

  ggplot(matchingIndexMatrix_Long, aes(mentor, mentee, fill=MatchingIndex, text=text)) +
    geom_tile() +
    geom_text(aes(label = round(MatchingIndex, 1)), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_fixed(ratio = 1)
  ggsave(paste0("res/other/MatchingIndices", "_", year, "_", group, ".pdf"))
  
  # plotting mentee matching index (i.e. how mentors match mentee preferences)
  menteeMatchingIndexMatrix_Long$menteeMatchingIndex <- menteeMatchingIndexMatrix_Long$menteeMatchingIndex/fractionMenteeScore
  ggplot(menteeMatchingIndexMatrix_Long, aes(mentor, mentee, fill=menteeMatchingIndex, text=text)) +
    geom_tile() +
    geom_text(aes(label = round(menteeMatchingIndex, 1)), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_fixed(ratio = 1)
  ggsave(paste0("res/other/MatchingIndices_MenteePreferencesOnly", "_", year, "_", group, ".pdf"))

  
  # plotting mentor matching index (i.e. how mentees match mentor preferences)
  mentorMatchingIndexMatrix_Long$mentorMatchingIndex <- mentorMatchingIndexMatrix_Long$mentorMatchingIndex/fractionMentorScore
  ggplot(mentorMatchingIndexMatrix_Long, aes(mentor, mentee, fill=mentorMatchingIndex, text=text)) +
    geom_tile() +
    geom_text(aes(label = round(mentorMatchingIndex, 1)), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1))  + coord_fixed(ratio = 1)
  
  ggsave(paste0("res/other/MatchingIndices_MentorPreferencesOnly", "_", year, "_", group, ".pdf"), height = length(mentors), width = length(mentees))
  
  
  # We also rank the best-matching mentors for each mentee and export the summary by csv
  matchingIndexMatrix_Long <- matchingIndexMatrix_Long[order(matchingIndexMatrix_Long$MatchingIndex, decreasing = TRUE),]
  matchingIndexMatrix_Long <- matchingIndexMatrix_Long[order(matchingIndexMatrix_Long$mentee, decreasing = TRUE),]
  matchingIndexMatrix_Long$mentorMatchRank <- rep(1:length(mentors), length(mentees))
  matchingIndexMatrix_Long <- matchingIndexMatrix_Long[order(matchingIndexMatrix_Long$MatchingIndex, decreasing = TRUE),]
  write.csv(matchingIndexMatrix_Long[, colnames(matchingIndexMatrix_Long) != "text"], file = paste0("res/csv/MatchingIndices", "_", year, "_", group, ".csv"))
  
  
  # clustering analysis
  matchingIndexMatrixT <- matchingIndexMatrix[,-ncol(matchingIndexMatrix)] # trims off mentor column
  # rounds up matching indices to the nearest integer
  matchingIndexMatrixT[,1:ncol(matchingIndexMatrixT)] <- apply(matchingIndexMatrixT, 2, as.numeric) %>% as.integer()
  # prints clustered heatmap
  pdf(paste0("res/summary/ClusteredMatchingIndex", "_", year, "_", group, ".pdf"))
  p <- pheatmap(matchingIndexMatrixT, display_numbers = T, 
                number_color = "black", fontsize = 10, 
                cellheight = 20, cellwidth = 20, number_format = "%.0f",
                main = paste0("MatchingIndices ", year, " ", group))  
  print(p)
  dev.off()
  
  
  pdf(paste0("res/other/QuestionWeights", "_", year, "_", group, ".pdf"))
  p1 <- ggplot(dfMenteeWeights, aes(question, name, fill=weight, text=weight)) + 
    # labs(title=paste0("Similarity Scores for Question: ", q)) +
    geom_tile() +
    geom_text(aes(label = weight), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_fixed(ratio = 1)
  p2 <- ggplot(dfMentorWeights, aes(question, name, fill=weight, text=weight)) + 
    # labs(title=paste0("Similarity Scores for Question: ", q)) +
    geom_tile() +
    geom_text(aes(label = weight), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_fixed(ratio = 1)
  grid.arrange(p1,p2)
  dev.off()

  # here is the magic part. We apply the hungarian algorithm which needs to operate on a
  # square matrix of matches and "costs" associated with each match and calculates
  # the optimal parings such that cost is minimized. To permit unequal numbers of mentors and
  # mentees, I apply the "rectangular" hungarian algorithm which provides dummy rows or columns just 
  # to make the math workout. The output is a row order such that the diagonal of the matrix 
  # when re-ordered in that order are the optimal mentor/mentee matches. 
  hungarian_rectangular_algorithm <- function(scores_matrix) {
    scores_matrix <- t(scores_matrix[,-dim(scores_matrix)[2]]) 
    # Determine the size of the matrix
    n <- nrow(scores_matrix)
    m <- ncol(scores_matrix)
    
    # Flip the score so lower = better therefore the algorithm can find the "lowest cost"
    # otherwise it will give you the absolute worst pairings
    scores_matrix <-  100 - scores_matrix
    
    # Add extra rows or columns to make the matrix square. These extra rows have an
    # exceedingly large cost so no match would benefit from choosing them. But they are
    # needed for the math to workout. 
    if (n < m) {
      scores_matrix <- rbind(scores_matrix, matrix(1e+10, nrow = m-n, ncol = m))
    } else if (m < n) {
      scores_matrix <- cbind(scores_matrix, matrix(1e+10, nrow = n, ncol = n-m))
    }
    
    # Solve the square matrix using the Hungarian algorithm
    solution <- solve_LSAP(as.matrix(scores_matrix))
    
    # Remove the extra rows or columns
    if (n < m) {
      solution <- solution[1:n]
    } else if (m < n) {
      solution <- solution[(n+1):(n+m)]
    }
    
    # Calculate the total cost of the solution
    cost <- sum(scores_matrix[cbind(1:n, solution)])
    
    list(solution = solution, cost = cost)
  }
  
  # the output is a list of the rows corresponding to the best mentor/mentee matches 
  # and a metric of the cost of having to assign a mentor to a mentee.
  bestMatches <- hungarian_rectangular_algorithm(matchingIndexMatrix)

  # the diagonal of this matrix are the best solutions
  bestMatchMatchingIndexMatrix <- as.data.frame(matchingIndexMatrix[c(bestMatches$solution),seq(1:length(mentees))])
  bestMatchMatchingIndexMatrix$mentor <- row.names(bestMatchMatchingIndexMatrix)
  # factoring is needed so ggplot doesn't override the order when plotting. 
  bestMatchMatchingIndexMatrix$mentor <- factor(bestMatchMatchingIndexMatrix$mentor, 
                                                levels = bestMatchMatchingIndexMatrix$mentor)
  bestMatchMatchingIndexMatrix_Long <- pivot_longer(bestMatchMatchingIndexMatrix, !mentor, names_to = "mentee", values_to = "MatchingIndex")
  bestMatchMatchingIndexMatrix_Long <- bestMatchMatchingIndexMatrix_Long %>% mutate(text = paste0("Mentor: ", mentor, "\n", "Mentee: ", mentee, "\n", "MatchingIndex: ", round(MatchingIndex,2)))
  bestMatchMatchingIndexMatrix_Long$mentee <- factor(bestMatchMatchingIndexMatrix_Long$mentee, 
                                                levels = colnames(bestMatchMatchingIndexMatrix))
  df_original$name
  mentorMatches <- df_original[match(row.names(bestMatchMatchingIndexMatrix),df_original$name),]
  menteeMatches <- df_original[match(colnames(bestMatchMatchingIndexMatrix[-dim(bestMatchMatchingIndexMatrix)[2]],),df_original$name),]
  emptyRow <- df_original[1,]
  emptyRow[1,] <- ""
  remainingMentors <- df_original[(match(c(row.names(bestMatchMatchingIndexMatrix),colnames(bestMatchMatchingIndexMatrix[-dim(bestMatchMatchingIndexMatrix)[2]],))
                                    ,df_original$name))*-1,]
  remainingMentors <- filter(remainingMentors, program == group)
  df_restructured <-  rbind(mentorMatches[1,], menteeMatches[1,])
  for (n in 2:dim(mentorMatches)[1]) {
    df_restructured <-  rbind(df_restructured, emptyRow, mentorMatches[n,], menteeMatches[n,])
  }
  df_restructured <- rbind(df_restructured, emptyRow, emptyRow, filter(remainingMentors, program == group))
  
  write.csv(df_restructured, file = paste0("res/summary/restructuredByBestMatches_", year,"_",group, ".csv"), row.names = F)
  # plotting mentee matching index (i.e. how mentors match mentee preferences)
  ggplot(bestMatchMatchingIndexMatrix_Long, aes(mentor, mentee, fill=MatchingIndex, text=text)) +
    geom_tile() +
    geom_text(aes(label = round(MatchingIndex, 1)), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "red") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_fixed(ratio = 1)
  ggsave(paste0("res/summary/bestMatchMatchingIndexMatrix", "_", year, "_", group, ".pdf"))
  
}

