
from tkinter import Tk, Text, BOTH, W, N, E, S, Canvas, NW, RAISED, RIGHT
from tkinter.ttk import Frame, Button, Label, Style
import cv2
import PIL.Image, PIL.ImageTk

class Example(Frame):
  
    def __init__(self):
        super().__init__()   
         
        self.initUI()
        
    def putMessage(self):
        print("Mensaje"+self.area.get("1.0",'end-1c'))
        
    def initUI(self):
      
        self.master.title("Data Visualizer")
        self.pack(fill=BOTH, expand=True)

        self.columnconfigure(1, weight=1)
        self.columnconfigure(3, pad=7)
        self.columnconfigure(4, pad=7)
        self.rowconfigure(3, weight=1)
        self.rowconfigure(5, pad=7)
        
        lbl = Label(self, text="Code input")
        lbl.grid(sticky=W, pady=4, padx=5)
        
        self.area = Text(self)
        self.area.grid(row=1, column=0, columnspan=2, rowspan=4,
            padx=5, sticky=E+W+S+N)
        
        abtn = Button(self, text="Connect to racket", command=self.putMessage)
        abtn.grid(row=1, column=3)

        cbtn = Button(self, text="Close connection")
        cbtn.grid(row=2, column=3, pady=4)
        
        hbtn = Button(self, text="Help")
        hbtn.grid(row=5, column=0, padx=5)

        obtn = Button(self, text="OK")
        obtn.grid(row=5, column=2)    

        quitB = Button(self, text="Quit", command=self.quit)
        quitB.grid(row=5, column=3)

        cv_img = cv2.cvtColor(cv2.imread("testim.jpg"),cv2.COLOR_BGR2RGB)
        height, width, no_channels = cv_img.shape

        canvas = Canvas(self, width = width, height = height)
        canvas.grid(column=4,row=0, columnspan=4)

        photo = PIL.ImageTk.PhotoImage(image = PIL.Image.fromarray(cv_img))
        canvas.create_image(0,0,image = photo, anchor=NW)

def main():
  
    root = Tk()
    root.geometry("600x500+100+100")
    app = Example()
    root.mainloop()  
    

if __name__ == '__main__':
    main() 