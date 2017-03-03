narrow<- read.csv("/Users/annazhu/PayWhatYouWant/Spring2016/AI_Thomascombined/mturksmall_multiplelines.csv", stringsAsFactors = FALSE)%>%
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
currgroup = "group 3"
Talker1 = "User 2"
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
    if ( (!q0) & grepl("walk.*for.*more.*than.*an.*hour",narrow$Message[count], ignore.case = TRUE)) {
      i = 0
      prepare[r, i*3+2] = narrow$Message[count]
      i = 1
      q0 = TRUE
    }
    else if ((!q1) & grepl("how.*celebrate.*last.*halloween",narrow$Message[count], ignore.case = TRUE)) {
      i = 1
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 2
      q1 = TRUE
    }
    else if ((!q2) & grepl("a.*new.*flavor.*of.*ice.*cream",narrow$Message[count], ignore.case = TRUE)) {
      i = 2
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 3
      q2 = TRUE
    }
    else if ((!q3) & grepl("best.*gift.*you.*ever.*received,)|(what.*is.*your.*favorite.*holiday)",narrow$Message[count], ignore.case = TRUE)) {
      i = 3
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 4
      q3 = TRUE
    }
    else if ((!q4) & grepl("gift.*receive.*last.*birthday",narrow$Message[count], ignore.case = TRUE)) {
      i = 4
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 5
      q4 = TRUE
    }
    else if ((!q5) & grepl("last.*time.*went.*to.*the.*zoo",narrow$Message[count], ignore.case = TRUE)) {
      i = 5
      prepare[r,i * 3 + 2] =narrow$Message[count]
      i = 6
      q5 = TRUE
    }
    else if ((!q6) & grepl("get.*up.*early.*stay.*up.*late",narrow$Message[count], ignore.case = TRUE)) {
      i = 6
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 7
      q6 = TRUE
    }
    else if ((!q7) & grepl("what.*did.*you.*do.*this.*summer",narrow$Message[count], ignore.case = TRUE)) {
      i = 7
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 8
      q7 = TRUE
    }
    else if ((!q8) & grepl("favorite.*actor.*of.*your.*gender",narrow$Message[count], ignore.case = TRUE)) {
      i = 8
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 9
      q8 = TRUE
    }
    else if ((!q9) & grepl("what.*is.*your.*favorite.*holiday",narrow$Message[count], ignore.case = TRUE)) {
      i = 9
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 10
      q9 = TRUE
    }
    else if ((!q10) & grepl("foreign.*country.*most.*like.*to.*visit",narrow$Message[count], ignore.case = TRUE)) {
      i = 10
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 11
      q10 = TRUE
    }
    else if ((!q11) & grepl("Do.*you.*prefer.*digital.*watches.*and.*clocks.*or.*the.*kind.*with.*hands",narrow$Message[count], ignore.case = TRUE)) {
      i = 11
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 12
      q11 = TRUE
    }
    else if ((!q12) & grepl("your.*mother.*best.*friend",narrow$Message[count], ignore.case = TRUE)) {
      i = 12
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 13
      q12 = TRUE
    }
    else if ((!q13) & grepl("how.*often.*get.*your.*hair.*cut",narrow$Message[count], ignore.case = TRUE)) {
      i = 13
      prepare[r,i * 3 + 2] = narrow$Message[count]
      i = 14
      q13 = TRUE
    }
    else if ((!q14) & grepl("what.*is.*the.*last.*concert.*you.*saw",narrow$Message[count], ignore.case = TRUE)) {
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


write.csv(prepare, "/Users/annazhu/PayWhatYouWant/Spring2016/AI_Thomascombined/mturk_small_XiaoPrepare2.csv")
