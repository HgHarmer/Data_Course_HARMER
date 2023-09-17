
library(tidyverse)
library(ggpubr)
library(jpeg)
library(ggimage)
attach(starwars)
data("starwars")


breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,300,500,550,600,601,602,603,604,605,606,607,608,609,650,790,847,848,852,900.5198465184154876648464628468465468465486516846156,
           1000,1356,1357,1358)


img <- readJPEG('jar jar me thinks_5.jpg')
?install.packages


ggplot(starwars, aes(y=mass,x=hair_color))+
 
  background_image(img)+
  
  geom_abline(x=birth_year)+
  geom_point(aes(x=skin_color,color=hair_color))+
  geom_point(color='#00ff00')+
  aes(color=name)+
  theme(axis.text.x=element_text(angle = 42,
                                   vjust = 1,
                                   hjust=-5,
                                   size = 6))+
  annotate('text',x=skin_color,y=mass-20,label="theres a poin here",angle=-42,size=mass/100)+
 annotate('text', x='fair' ,y=1000,label='Natalie Portman is the reason I work out. I have this fantasy where we start talking at the Vanity Fair Oscars party bar. We exchange a few pleasantries. She asks what I do. I say I loved her in New Girl. She laughs. I get my drink.
           
           Well, see ya, I say and walk away. Ive got her attention now. How many guys voluntarily leave a conversation with Natalie Portman? She touches her neck as she watches me leave.
           
           Later, as the nights dragged on and the coterie of gorgeous narcissists grows increasingly loose, she finds me on the balcony, my bowtie undone, smoking a cigarette.
           
           Got a spare? she asks.
           
           Whats in it for me? I say as I hand her one of my little white ladies. She smiles.
           
           Conversation with me, duh.
           
           I laugh.
           
           Whats so funny? she protests.
           
           Nothing, nothing... Its just... dont you grow tired of the egos?
             
             You get used to it, she says, lighting her cigarette and handing me back the lighter.
           
           What would you do if you werent an actress? I ask.
           
           Teaching, I think.
           
           And if I was your student, what would I be learning?
             
             Discipline, she says quickly, looking up into my eyes, before changing the subject. Where are you from?
             
             Bermuda, I say.
           
           Oh wow. Thats lovely.
           
           Its ok, I admit. Not everything is to my liking.
           
           What could possibly be not to your liking in Bermuda? she inquires.
           
           I dont like sand, I tell her. Its coarse and rough and irritating and it gets everywhere.',
           size=.5,
           angle = 142)+
  ggtitle('ɿɘʇʇuƨ oƚ ylno ɘɿɘʜ lliƚƨ ɘw ɘɿɒ yʜw')+
  theme(plot.title = element_text(size = 5,angle = 5))+
  scale_y_reverse(breaks=breaks, labels=breaks)
 
 
 scale
  