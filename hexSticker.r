### mcmcHammer hex sticker
###
### source('E:/Ecology/Drive/R/airUpThere/working/hexSticker.r')

library(hexSticker)
library(magick)
library(sysfonts)

img <- image_read('E:/Ecology/Drive/R/airUpThere/working/windmill_stripped_white.png')

sticker(
	subplot = img,
	package='airUpThere',
	p_size=18,
	p_color='white',
	p_y=1.4,
	s_x=1,
	s_y=0.7,
	s_width=0.97,
	s_height=1.05,
	h_color = 'white',
	h_fill = 'cornflowerblue',
	white_around_sticker = FALSE,
	filename='E:/Ecology/Drive/R/airUpThere/working/airUpThere.png'
)


		