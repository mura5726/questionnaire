# count up answer each questions
cuans = function(q, d){
  switch(q,
         # q1 : count up answer 1, 2, 3
         sum(d <= 3),
         # q2 : count up answer 9, 10, 11, 12, 13
         sum(d >= 9),
         # q3 : count up answer 1, 2
         sum(d <= 2),
         # q4 : count up answer 1, 2
         sum(d <= 2),
         # q5 : count up answer 1, 2
         sum(d <= 2),
         # q6 : count up answer 1, 2
         sum(d <= 2)
  )
}
