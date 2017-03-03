narrow<- read.csv("/Users/annazhu/PayWhatYouWant/Spring2016/AI_Thomascombined/mturkclose_multiplelines.csv", stringsAsFactors = FALSE)%>%
  select(-X)
prepare <- wide %>% select(Group) %>% mutate(Q1 = "", A1a = "", A1b = "", 
                                             Q2 = "", A2a ="", A2b ="", 
                                             Q3 ="", A3a ="", A3b ="", 
                                             Q4 ="", A4a ="", A4b ="", 
                                             Q5 ="", A5a ="", A5b ="", 
                                             Q6 ="", A6a ="", A6b ="", 
                                             Q7 ="", A7a ="", A7b ="", 
                                             Q8 ="", A8a ="", A8b ="", 
                                             Q9 ="", A9a ="", A9b ="", 
                                             Q10 ="", A10a ="", A10b ="", 
                                             Q11 ="", A11a ="", A11b ="", 
                                             Q12 ="", A12a ="", A12b ="", 
                                             Q13 ="", A13a ="", A13b ="", 
                                             Q14 ="", A14a ="", A14b ="", 
                                             Q15 ="", A15a ="", A15b = "") %>% mutate(`Talker1ID` = wide$Talker1.ID)

r = 1
i = 0
q0 = FALSE
q1 = FALSE
q2 = FALSE
q3 = FALSE
q4 = FALSE
q5 = FALSE
q6 = FALSE
q7 = FALSE
q8 = FALSE
q9 = FALSE
q10 = FALSE
q11 = FALSE
q12 = FALSE
q13 = FALSE
q14 = FALSE
currgroup = "group 14"
Talker1 = "User 1"
for (count in 1:nrow(narrow)) {
  if (narrow$Group[count] != currgroup) {
  r = r + 1
  i = 0
  q0 = FALSE
  q1 = FALSE
  q2 = FALSE
  q3 = FALSE
  q4 = FALSE
  q5 = FALSE
  q6 = FALSE
  q7 = FALSE
  q8 = FALSE
  q9 = FALSE
  q10 = FALSE
  q11 = FALSE
  q12 = FALSE
  q13 = FALSE
  q14 = FALSE
  Talker1 = narrow$User[count]
  currgroup = narrow$Group[count]
  }
    if ( (!q0) & grepl("anyone.*in.*the.*world.*dinner.*guest",narrow$Message[count], ignore.case = TRUE)) {
      i = 0
      prepare[r, i*3+2] = narrow$Message[count]
      i = 1
      q0 = TRUE
    }
    else if ((!q1) & grepl("constitute.*a.*perfect.*day",narrow$Message[count], ignore.case = TRUE)) {
      i = 1
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 2
      q1 = TRUE
    }
    else if ((!q2) & grepl("the.*age.*of.*90.*retain.*either.*the.*mind.*or.*body",narrow$Message[count], ignore.case = TRUE)) {
      i = 2
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 3
      q2 = TRUE
    }
    else if ((!q3) & grepl("change.*anything.*about.*you.*raised,",narrow$Message[count], ignore.case = TRUE)) {
      i = 3
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 4
      q3 = TRUE
    }
    else if ((!q4) & grepl("wake.*up.*tomorrow.*gain.*quality.*or.*ability,",narrow$Message[count], ignore.case = TRUE)) {
      i = 4
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 5
      q4 = TRUE
    }
    else if ((!q5) & grepl("crystal.*ball.*tell.*you.*truth.*about.*life.*future.*anything.*else",narrow$Message[count], ignore.case = TRUE)) {
      i = 5
      prepare[r,i * 3 + 2] =narrow$Message[count]
      i = 6
      q5 = TRUE
    }
    else if ((!q6) & grepl("greatest.*accomplishment.*of.*your.*life",narrow$Message[count], ignore.case = TRUE)) {
      i = 6
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 7
      q6 = TRUE
    }
    else if ((!q7) & grepl("most.*treasured.*memory",narrow$Message[count], ignore.case = TRUE)) {
      i = 7
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 8
      q7 = TRUE
    }
    else if ((!q8) & grepl("die.*suddenly.*change.*anything.*about.*the.*way",narrow$Message[count], ignore.case = TRUE)) {
      i = 8
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 9
      q8 = TRUE
    }
    else if ((!q9) & grepl("relationship.*with.*your.*mother",narrow$Message[count], ignore.case = TRUE)) {
      i = 9
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 10
      q9 = TRUE
    }
    else if ((!q10) & grepl("an.*embarrassing.*moment.*in.*your.*life",narrow$Message[count], ignore.case = TRUE)) {
      i = 10
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 11
      q10 = TRUE
    }
    else if ((!q11) & grepl("last.*cry.*in.*front.*of.*another.*person",narrow$Message[count], ignore.case = TRUE)) {
      i = 11
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 12
      q11 = TRUE
    }
    else if ((!q12) & grepl("opportunity.*to.*communicate.*with.*anyone.*regret",narrow$Message[count], ignore.case = TRUE)) {
      i = 12
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 13
      q12 = TRUE
    }
    else if ((!q13) & grepl("catch.*fire.*a.*final.*dash.*to.*save.*any.*one.*item",narrow$Message[count], ignore.case = TRUE)) {
      i = 13
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 14
      q13 = TRUE
    }
    else if ((!q14) & grepl("family.*whose.*death.*most.*disturbing",narrow$Message[count], ignore.case = TRUE)) {
      i = 14
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 15
      q14 = TRUE
    }
    else {
      if (i == 0) {
        if (narrow$User[count]== Talker1) {
          prepare[r,i * 3 + 3] = paste(prepare[r,i * 3 + 3], narrow$Message[count], sep = " ")
        } else {
          prepare[r,i * 3 + 4] = paste(prepare[r,i * 3 + 4], narrow$Message[count], sep = " ")
        }
      }else{
      if (narrow$User[count] == Talker1) {
        prepare[r,(i-1) * 3 + 3] = paste(prepare[r,(i-1) * 3 + 3], narrow$Message[count], sep = " ")
      } else {
        prepare[r,(i-1) * 3 + 4] = paste(prepare[r,(i-1) * 3 + 4], narrow$Message[count], sep = " ")
      }
      }
    
  }
}


write.csv(prepare, "/Users/annazhu/PayWhatYouWant/Spring2016/AI_Thomascombined/mturk_close_XiaoPrepare2.csv")
