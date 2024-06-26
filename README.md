To use the Matching Index, follow the following steps:
1) Clone this repository
 
    ```
    git clone https://github.com/amfrancette/MatchingIndex.git
    ```
2)  Have mentors and mentees fill out a single >10 minute survey regarding their personality traits/interests as well as how important to them it is that their mentor/mentee shares similar responses to each question. 
3) Export survey results and move to "data" folder for analysis
4) Run R script setting parameters of the working directory, data filepath, year, group to be analyzed (MCDB or EE), and fraction of matching index dictated by mentee preferences (as opposed to mentor preferences)
5) Use resulting data (saved to "res") to best pair mentors and mentees in conjunction with whatever extraneous  information that may inform the decision.

It is key that additional survey question headers are appropriately added to the data/MenteePairingSurveyHeaders.txt file  if there are changes to the number or composition of survey questions. 
 
See info/MatchingIndexDescription.docx for a breakdown of the matching index calcualtion

Eq1: Full matching index equation

![plot](/info/Equation1.png)

Eq2: Moderately simplified equation

![plot](/info/Equation2.png)

Eq3: Very simplified equation

![plot](/info/Equation3.png)


M=Overall degree of matching for mentor-mentee pair

Me=match for a mentor to mentee prefrences

Mo=match for a mentee to mentor prefrences

Wie=weight of preference of mentee for mentor to have similar response to question (i)

Wio=weight of preference of mentor for mentee to have similar response to a question (i)

Wadjio=weight of preference of mentor for mentee to have similar response to a question (i)  
normalized to total sum of that mentee' s weight preferences

Wadjie=weight of preference of mentee for mentor to have similar response to a question (i)  
normalized to total sum of that mentee's weight preferences

Si=Similarity of mentor and mentee responses for question (i)  i.e.the number of overlaps in responses 
divided by the average number of responses given by mentr and mentee 

δe=Fraction of final summary statistic dictated by mentor compatibility with mentee preferences. 
Current default is 0.8 meaning if a mentor fits the mentees expectation exactly,the matching index can be no lower than 80.

