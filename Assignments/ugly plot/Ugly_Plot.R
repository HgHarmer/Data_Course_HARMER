
library(tidyverse)
library(ggpubr)
library(jpeg)
library(ggimage)
attach(starwars)
library(ggrepel)
data("starwars")
breaks = c(0:100,200:300,500:600,800:900,1100:1200)
breaks = c(15:1358)
breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,300:500,550,600,601,602,603,604,605,606,607,608,609,650,790,847,848,852,900.5198465184154876648464628468465468465486516846156,
           1000,1356,1357,1358)
jarjarpallet <-c( '#D321F0','#F8D6F5','#0AC5FB','#1331E2','#D70B0B',
'#FBEF0C','#CF76F8','#098FFB','#F78D0F','#7E2AED','#1166FB','#FAEA8D','#8BC3FB')
jarjar<- scale_color_manual(jarjarpallet)
img <- readJPEG('jar jar me thinks_5.jpg')



ggplot(starwars, aes(y=mass,x=hair_color))+
  background_image(img)+
  geom_abline(x=birth_year)+
  geom_point(aes(x=skin_color,color=hair_color))+
  geom_point(color='#00ff00')+
  aes(color=name)+
  theme(axis.text.x=element_text(angle = -42,
                                   vjust = -20,
                                   hjust=-1,
                                   size = 6))+
  theme(axis.text.y = element_text(size = 20,
                                   angle = 1))+

 annotate('text',x=skin_color,y=mass-50,label="theres a poin here",angle=42,size=mass/100)+
 
 annotate('text', x='fair' ,y=500,label='Natalie Portman is the reason I work out. I have this fantasy where we start talking at the Vanity Fair Oscars party bar. We exchange a few pleasantries. She asks what I do. I say I loved her in New Girl. She laughs. I get my drink.
           
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
 













geom_label_repel(aes(x=skin_color,y=mass),
                 label="theres a poin here",
                 min.segment.length=unit(0, 'lines'),
                 nudge_y=6,
                 size=mass/100)


annotate('text',x=skin_color,y=mass-20,label="theres a poin here",angle=-30,size=mass/100)


ggplot(data = starwars) + 
  geom_point(mapping = aes(x = skin_color, y = mass))+
  geom_text_repel(mapping = aes(x = skin_color, y = mass, label = 'theres a poin here'),
                  max.overlaps = 500,
                  size=mass/100)









geom_text_repel(mapping = aes(x = skin_color, y = mass, label = 'theres a poin here'),
                  max.overlaps = 500,
                  size=mass/100)