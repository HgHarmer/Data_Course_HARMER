










library(gganimate)
library(plotly)

library(ggrepel)












library(tidyverse)
library(jpeg)
library(ggbreak)

library(ggpubr)
library(ggimage)
#####
set.seed(18)#small blue big color

set.seed(32)#med blue med-large color
set.seed(34)#small small blue med- small color
set.seed(39)# large color, faded blue

set.seed(69)#nice, itty bitty blue big color
set.seed(76)# tiny all
set.seed(102)#300 blue
set.seed(26)#or28 all blue

set.seed(17)#super fadded
set.seed(14)# nice blue lines


######important objects


data(starwars)
jarjarpallet <-c( '#D321F0','#F8D6F5','#0AC5FB','#1331E2','#D70B0B',
'#FBEF0C','#CF76F8','#098FFB','#F78D0F','#7E2AED','#1166FB','#FAEA8D','#8BC3FB')
breaks = sample(c(0:160,1200:1358),25)
a <-  c(rep(0,5),1:20,1:20,15:35,45:50,70:80,300)
img <- readJPEG('jar jar me thinks_5.jpg')
attach(starwars)
#####favorite seeds
set.seed(87)###no

set.seed(111)####big

set.seed(7)# big poin big color light blue

set.seed(12)#pretty average

set.seed(15)#just right

#rules of good graph design 
#data should be arranged in a way that makes sense 
#points should be clearly distinguishable 
#graph should be easily replicated 
#x and y axes should be readable 
  ggplot(starwars, aes(y=mass,x=hair_color))+
  #####
  background_image(img)+
  geom_abline(x=birth_year)+
   
  geom_point(aes(x=skin_color,
                 color=hair_color),alpha=runif(n=1, min = 0, max = .75),
             size=sample(a,1))+
   
   scale_color_manual(values = jarjarpallet,
                      name='protein filament pigment')+
   
  geom_point(color='#8BC3FB', alpha=runif(n=1, min = 0, max = .75),
             size=sample(a,1))+
  
   theme(axis.text.x=element_text(angle = -42,
                                   vjust = -20,
                                   hjust=-1,
                                   size = 6),
        axis.text.y = element_text(size = 20,
                                   angle = 4,
                                   color = 'limegreen'),
        legend.background = element_rect('#FF6700'),
        legend.title = element_text(color = '#FF00F9',
                                    angle = 269,
                                    vjust = 2,
                                    hjust = -.25),
        plot.background = element_rect(fill = 'hotpink',colour ='#9AAE07' ),
        plot.title = element_text(size = 5,
                                  angle = 5,
                                  vjust=-5,
                                  hjust = .6),
        legend.text = element_text(color = '#9AAE07',
                                   angle = 95),
        axis.title.x = element_text(vjust = 4))+	
   labs(x='this x axis is kind of complicated, it technically shows two variables, the first is skin color, which is represented by the colors in the key, the other varia
        ble is hair color which is represented by the blue dots of indeterminate size, unfortinately these dots can sometimes cover up the other points if they happen to be too big so 
        i decided to also annotate each point with a label that says that there is a poin here, originaly it was suposed to say there is a point (with a T) but i mispelled it on the first att
        empt and i havent gone back to fix it and really who cares if its misspelled? the only one that big enough to read is the one that labels the most massive character, oh i forgot to mention each 
        anotation is scalled to be equal to the mass of the character it points to devided by 100, thats why so many of them are so small because their mass is like 50 or something but that gets 
        divided by 100 so the font size ends up being like .5, i should probably be more brief with these lables, theyre kind of shrinking my graph... but before i stop its probably 
        important to say where the x lables are theyre those diagonal words that kind of get covered up by the key',
        y="this y axis shows the mass variable, i felt that the range of points from 300-500 was particularly important so i included every whole number in that range
        the mass of 900.519846518416 was also notable so it was included. did you know that the mass column for this dataset doesnt inculude the units? Ihats weird, I kind of assume that the mass is measured in kilograms because most of the mass measurments are from 15-50 and those measur
        emnts wouldnt make much sense for a grown man if it was pounds, but whats realy crazy is the mass of the most massive character, that character is Jabba Desilijic Tiure, im not really sure how to pronounce that but he has a mass of 1358, thats massive! if it is measured in kilograms then he would weigh 2994 pounds, thats nearly two smart cars! (kg?)
        actualy scratch all that stuff i said about y axis lables all those were trash so i removed them most of them entirely from the graph, who cared about 300-500 anyway? now i have a much better way of placing my y axis lables, 
        i have the range of points from 0-160, and 1200-1358 set as a vector and i just sample 25 of those each time, its much easier if you dont have to think about what points matter.
        you know what i take it back i miss 300-500 so i put it back in its the middle y scale break, i decided not to lable it because i feel like thats self explanitory. actualy i still like 300-500 but 350-400 is anoying me so im removing those points from the graph, also every time i split the graph a samller jarjar png is generated, thats weird.... ")+
  
   annotate('text',x=skin_color,
          y=mass-50,
          label="theres a poin here",
          angle=42,size=mass/sample(c(25:100),1),
          color='#Fffda0')+
 geom_image(aes(x="orange",
                  y=66,image='jar_2.png' ,size=10,), size=.1)+  
 ggtitle('i didnt want this title to get in the way of any data so i moved it down here to this little gap between 650 and 790,but then it kind of covered the x axis titles so i also had to turn it a bit to avoid those, but thats not as easy as you would think
         the hjust and vjust comands affect text more the longer that text strand is, its honsetly a real pain, i tried to get the title into position first and then move it but then when i 
         tried to run the graph the text just dissapeared, very annoying, anyway this graph shows the reationship between the mass of a starwars character and their hair/eye color, now that im experiemnting a bit more how hjust and vjust change also depends on the size of the window, thats kind of dumb to be honest, how are you suposed to get consistant results?
         some of the stuff ive said in the past on this axis is kind of a mute point now, i got rid of the empty space between like 200 and 1200 mass units so the title is kind of in a random spot now, oh well
         ive decided im going to move this title again i feel like it could be better placed at the bottom of this chart. yeah i think i like that better but ill let you know if i decide to move it agian')+
  scale_y_reverse(breaks=sample(c(0:160,1200:1358),25))+
    scale_y_break(c(160,300))+
    scale_y_break(c(350,400))+
   scale_y_break(c(500,1200))

  
  
  
  
  
############################################################################# 
#############################################################################
#############################################################################
#############################################################################
#############################################################################


for (i in 1:3) {
  print(p)
  
}
 iris %>%
 mutate(blink=Sepal.Width<3) %>% 
 ggplot(aes(x=Sepal.Length,y=Sepal.Width, color=Species))+
 geom_point()+
gganimate::transition_states(blink,state_length = .5)+
gganimate::enter_appear()
 anim_save()#how to save
 
 
 
 ggplot(starwars, aes(y=mass,x=hair_color))+
   background_image(img)+
   geom_abline(x=birth_year)+
   
   geom_point(aes(x=skin_color,
                  color=hair_color),alpha=runif(n=1, min = 0, max = .5),
              size=sample(a,1))+
   
   scale_color_manual(values = jarjarpallet,
                      name='protein filament pigment')+
   scale_y_break(c(200,1300))
 
 
 
 
 ggplot(starwars, aes(y=mass,x=hair_color))+
   
   geom_point(aes(x=skin_color,
                  color=hair_color),alpha=runif(n=1, min = 0, max = .5),
              size=1)+
   
   scale_color_manual(values = jarjarpallet,
                      name='protein filament pigment')+
   
   geom_point(color='#8BC3FB', alpha=runif(n=1, min = 0, max = .5),
              size=1)+
   
   theme(axis.text.x=element_text(angle = -42,
                                  vjust = -20,
                                  hjust=-1,
                                  size = 6),
         axis.text.y = element_text(size = 20,
                                    angle = 1,
                                    color = '#DFFF70'),
         legend.background = element_rect('#FDEE00'),
         legend.title = element_text(color = '#FF00F9',
                                     angle = 269,
                                     vjust = 2,
                                     hjust = 0),
         plot.background = element_rect(fill = 'hotpink',colour ='#9AAE07' ),
         plot.title = element_text(size = 5,
                                   angle = 5,
                                   vjust=-2.3,
                                   hjust = .6),
         legend.text = element_text(color = '#9AAE07',
                                    angle = 95)
   )
a
ggplotly()
GGally::ggpairs(iris) 