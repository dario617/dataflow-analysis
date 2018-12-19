import tkinter
import cv2
import PIL.Image, PIL.ImageTk

from graphviz import Digraph

window = tkinter.Tk()
window.title("Test")

dot = Digraph(comment='a test graph')

dot.node('1','x:= 1')
dot.node('2','y:= x + 2')
dot.node('3','x:= y')

dot.edges(['12','23'])

dot.render('testim.png', format='png')
cv_img = cv2.cvtColor(cv2.imread("testim.png.png"),cv2.COLOR_BGR2RGB)


height, width, no_channels = cv_img.shape

canvas = tkinter.Canvas(window, width = width, height = height)
canvas.pack()

photo = PIL.ImageTk.PhotoImage(image = PIL.Image.fromarray(cv_img))
canvas.create_image(0,0,image = photo, anchor=tkinter.NW)

window.mainloop()