#Creating a variable called n_stay and assigning the value 0 to the variable n_stay(which is the number of 
#contestants that chose to stay at the chosen door).
n_stay <- 0

#Creating a variable called n_switch and assigning the value 0 to the variable n_switch(which is the number of 
#contestants that chose to switch to the other available door).
n_switch <- 0

#Creating a variable called n_switch and assigning the value 0 to the variable n_contestantwins(which is the number of 
#times the contestant has chosen the door which had a car behind it)
n_contestantwins <- 0

#The for loop runs through the code inside a 100 times(hence playing the game a 100 times). The for loop starts 
#as i= 1 and increments each time till 100. 
for ( i in 1:100) {
  
  #Creating a vector named door that contains numbers 1,2 and 3(which is the different 
  #number of doors that have goats or a car).
  door <- c(1,2,3) 
  
  #Randomly selects one of the doors to have the car behind and assigns that to the variable cardoor. Cardoor now
  #contains randomly selected value from those in the vector door.
  cardoor <- sample(door,1)
  
  #Randomly selects a door(Out of the 3 doors) chosen by the contestant and assigns that to the variable choice.
  choice <- sample(door,1) 
  
  #Creating a new vector called goatdoors that holds the values corressponding to the doors that have a goat. The 
  #following code sets the the other two doors(other than the cardoor which was already chosen at random shown in the 
  #above code) of the three to have a goat. 
  goatdoors <- setdiff(door, cardoor) 
  
  #Creating a new vactor that holds the optional door to reveal. The reveal_option has to be a goat, therefore the
  #following code reveals a door that has the goat. If the contestant picks the door with the car then the
  #reveal_options will have two possible options. However if the contestant picks the door with the goat there is only
  #one other door that the host can reveal that will have the goat.
  reveal_options <- setdiff(goatdoors, choice) 
  
  #If the door chosen by the contestant is the same as the door that has a car then....
  if (choice == cardoor) {
    #Creating a new variable called 'reveal' that holds the value of the door to reveal. The following code randomly 
    #picks one of the 'reveal_options'. If the contestant picks the cardoor then 'reveal_options' will have two 
    #options to randomly pick from. 
    reveal <- sample(reveal_options,1)  
  #If the door chosen by the contestant isn't the same as the door with then car then....
  }else {
    #Creating a new variable and assigning the value of the 'reveal_options' to that variable. If the contestant
    #picks a goat door(i.e that isn't the cardoor) then 'reveal_option' will only have a single element and that
    #value is assigned to reveal.
    reveal <- reveal_options 
  }
  
  #Creating a new vector called 'remaining_doors' which identifies the two remaining doors. The following code
  #identifies the remaining two doors(i.e Out of the three doors identifies the two doors that are left over 
  #after the host revealed the other door) 
  remaining_doors <-setdiff(door, reveal)
  
  #Creating a new variable recording the final choice of door if the contestant swtitches. The 'newchoice' differs 
  #depending on the 'remaining_doors' and the 'choice' of the contestant.
  newchoice <- setdiff(remaining_doors, choice)   
  
  #Creates a new variable and assigns a random value depending on the 'remaining_doors'. 
  randomContestantsChoice <- sample(remaining_doors,1)
  
  #Checks if the contestants 'choice' is the same as the door with the car then...
  if (choice == cardoor) {
    #Creating a new variable called 'n_stay' which is the number of contestants that chose to stay with their original
    #door choice a. The varialbe n_stay is incremented each time and it records the number of contestants that chose to 
    #stay and got it right(i.e there is a car behing the door).
    n_stay <- n_stay + 1
  }
  
  #Checks if the contestants 'newchoice'(i.e the contestant chose to switch doors) is the same as the door with 
  #the car then....
  if (newchoice == cardoor) {
    #Creating a new variable called 'n_switch' which is the number of contestants that chose to switch with their 
    #their original door choice. The varialbe n_stay is incremented each time and it records the number of contestants 
    #that chose to switch doors and got it right(i.e there is a car behing the door).
    n_switch <- n_switch + 1
  }
  #If the 'randomContestantChoice' was the same as the 'cardoor' then....
  if (randomContestantsChoice == cardoor){
    #Incrementing the variable 'n_contestantwins' which contains the number of times that the contestant wins the car
    #when they randomly decide to switch or stay(i.e the 'randomContestantChoice' being the 'cardoor')
    n_contestantwins = n_contestantwins + 1
  }
}

#Print out the variable 'n_stay' divided by 100.This turns the integer into a proportion(as the simulation was done 
#100 times) to show what proportion of the time did staying at the original door chosen resulted in the 
#contestant winning the car.This proportion is an average of 0.33(i.e one third of the time staying with the 
#original 'choice' results in winning the car.
print(n_stay/100)

#Print out the Variable 'n_switch' divided by 100.This turns the integer into a proportion(as the simulation was done 
#100 times) to show what proportion of the time did switching door from 'choice' to 'newchoice' result in the 
#contestant winning the car.This proportion is an average of 0.66(i.e two thirds of the time switching results 
#in winning the car)
print(n_switch/100)

#Printing string sentence explaining what the variable n_contestantwins represents while also including this variable 
#value in the printing. The'%s'gets replaced by the value of 'n_contestantwins' when the code is run.the result is an 
#average of 50 wins as the contestant has a 50% chance of choosing the car when only two doors remain and they choose
#to switch or stay randomly.
sprintf("For a player who, in each game, randomly chooses to switch or stay they win the car %s times", n_contestantwins)

 






