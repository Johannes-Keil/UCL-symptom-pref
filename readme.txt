We collected data from 130 adults who had indicated in a previous survey that they “Struggle with depression”  via CloudResearch. They were first given a list of 52 items gathered from a few different depression measures and asked to indicate “Which of these problems have bothered you in the previous 6 months?” The items are listed in revised_many_symptoms.csv. 5 of these items are related to anhedonia or low mood, the core symptoms of depression, these are designated with a 1 in the “core_item” column.

Participant demographics are in demo_res_wave1.csv. Additionally, scores from the CES-D and an indicator if the score is equal to above the threshold for identifying “individuals at greater risk for depression” (which is 16). The columns there are:
id: int
     The participant’s id in our system, used to link their results in all tables
duration: int
     The time in seconds between starting the task and completing it
sex: string
      f for female, m for male
age: int
      age in years
CESD: float
      Total score on the CESD
above_threshold: int
      0 if the total score is below 16, 1 if it is greater than 16

For each of the items they endorsed, plus the 5 core symptoms regardless of endorsement, we collected the following Likert responses:
(Frequency Question) How frequently has this problem bothered you in the last 6 months?
[‘0 - Never’,‘1 - Only once’,‘2 - Sometimes’,‘3 - Weekly’,‘4 - Every few days’,‘5 - Every day’]  
(Severity Question) When this problem bothers you, how severe is it?
[‘0 - No bother’,‘1 - Minimal’,‘2 - Mild’,‘3 - Moderate’,‘4 - High’,‘5 - Most severe’]  
(Impact Question) How much does this problem impact your life overall?  
[‘0 - No impact’,‘1 - Minor annoyance’,‘2 - Mild’,‘3 - Troublesome’,‘4 - High impact’,‘5 - Prevents me from going about my life’]
Responses to the initial symptom filtering and these likert questions are in filter_res_wave1.csv, which has these columns:
id: int
     The participant’s id in our system, used to link their results in all tables
item_id: int
     The item number from revised_many_symptoms.csv
revised_item: string
     The text of the item
filter_response: int
      1 if they had the symptom, 2 if they did not have the symptom
filter_rt: int
      Reaction time in miliseconds
filter_order: int
      The order in which the symptoms were presented to each participant
frequency_response: int
      The Likert response to the frequency question
frequency_rt: int
      Reaction time to frequency question in miliseconds
frequency_order: int
      The order in which that symptom was presented in the frequency ratings
severity_response: int
      The Likert response to the severity question
severity_rt: int
      Reaction time to severity question in miliseconds
severity_order: int
      The order in which that symptom was presented in the severity ratings
impact_response: int
      The Likert response to the impact question
impact_rt: int
      Reaction time to impact question in miliseconds
impact_order: int
      The order in which that symptom was presented in the impact ratings

After the likert questions, participants were shown up to 190 pairings of the symptoms they had endorsed + the 5 core symptoms. If the total of endorsed + core items for the participant was greater than 20, then a random subset of all of the possible pairs was presented. Pairs were selected so that each item was presented approximately the same number of times. Results for the pairwise ratings are in pairwise_res_wave1_nonull.csv, which has these columns:
id: int
     The participant’s id in our system, used to link their results in all tables
item_0: int
     The item number from revised_many_symptoms.csv for one of the presented symptoms in that pair
item_1: int
     The item number from revised_many_symptoms.csv for the other the presented symptoms in that pair
choice: int
      Did the participant choose item 0 or item 1
winner: int
      The item number from revised_many_symptoms.csv for the chosen symptom
rt: int
      Reaction time to pairwise rating in milliseconds
order: int
      The order in which that pair was presented in the pairwise choices
log_rt: float
      log base 10 of the reaction time

Following the pairwise choices, participants filled out the CES-D, a frequently used measure of depression. Several items from this measure were used in the bank of items for this task. The item level responses are in CESD_res_wave1.csv, which has these columns:
id: int
     The participant’s id in our system, used to link their results in all tables
item_id: int
     The CESD item number
item: string
      The text of the CESD item
positive: int
       An indicator of the items which are reverse coded (1 if reverse coded, 0 otherwise)
CESD_response: int
      The score of the response taking into account reverse scoring
CESD_rt: int
      The reaction time in milliseconds for that item
orig_respones: int
      The original response before reverse coding

preference.26:42