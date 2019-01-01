import tkinter
import os
import socket
import sys
import time
from PIL import Image, ImageTk
import cv2
import json
from graphviz import Digraph
from tkinter import messagebox

BASE_STMT = '((:= x 4) \n\
(:= y 4) \n\
(:= x 2) \n\
(if (> 3 2) \n\
(:= z 4) \n\
(:= z (* y x)) \n\
) \n\
(:= x z))'

# Display Concrete Syntax in graph
def replaceAST(s):
    copy = s.replace("Assign",":=").replace("Greater",">").replace("NoOp","Skip")
    copy = copy.replace("Div","/").replace("Mult","*").replace("Equal","==")
    copy = copy.replace("Plus","+").replace("Minus","-").replace("(","[").replace(")","]")
    return copy

class ConnectionHandler:
    def __init__(self,host,port):
        self.host = host
        self.port = port

    def connect(self):
        # Restart connection
        try:
            if self.sock != None:
                self.closeCon()
        except Exception:
            print("Nothing to close")
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        print("==== Connecting ==== ")
        self.sock.connect((self.host,self.port))
        print("====   Success  ==== ")

    def sendAndReceive(self,msg):
        print("Sending "+msg)
        self.sock.sendall(msg.encode())
        print("Sent and waiting for response")
        rcv =  self.sock.recv(2048)
        print("Got response from racket")
        return rcv

    def closeCon(self):
        print("== Disconnecting == ")
        self.sock.close()

class GraphMaker:
    def __init__(self,dir):
        try:
            self.dir = dir
            os.system('mkdir '+dir)
        except Exception:
            print("Error on mkdir")

    def makeGraph(self,g,fileName):
        print("Initialize")
        dot = Digraph(comment="Data Graph")
        # Parse JSON to non string format
        localCopy = g.replace("'","").replace("\\","")[1:-1]
        localCopy = json.loads(localCopy)

        # Get nodes and add them to the graph
        nodes = localCopy['nodes']
        for i in range(len(nodes)):
            dot.node(str(nodes[i]['label']),str(nodes[i]['value'])+str(nodes[i]['label']))
        
        # Get nodes and add them to the graph
        edges = localCopy['edges']
        edgeList = list()
        for e in range(len(edges)):
            edgeList.append(str(edges[e]['l1'])+str(edges[e]['l2']))
        dot.edges(edgeList)
        print("About to render")
        dot.render(self.dir+"/"+fileName,format='gif')
        print("Render successful")
        return self.dir+"/"+fileName+".gif", localCopy

    def makeEdges(self,g,inh,outh,fileName,current):
        print("Initialize edge maker")
        dot = Digraph(comment="New Data Graph")
        # Graph sould be ok by now

        # Parse the JSONs
        localCopyIN = inh.replace("'","").replace("\\","")[1:]
        localCopyOUT = outh.replace("'","").replace("\\","")[:-1]
        localCopyIN = json.loads(localCopyIN)
        localCopyOUT = json.loads(localCopyOUT)

        print(localCopyIN)
        print(localCopyOUT)

        nodes = g['nodes']
        for i in range(len(nodes)):
            dot.node(str(nodes[i]['label']),str(nodes[i]['value'])+str(nodes[i]['label']))


        edges = g['edges']
        if current == "Live Variables":
            # use OUT
            for e in range(len(edges)):
                dot.edge(str(edges[e]['l1']),str(edges[e]['l2']),
                label=self.parseEdgeLabel(localCopyOUT[str(edges[e]['l2'])]))
        else:
            # use IN
            for e in range(len(edges)):
                dot.edge(str(edges[e]['l1']),str(edges[e]['l2']),
                label=self.parseEdgeLabel(localCopyIN[str(edges[e]['l2'])]))

        print("About to render image with edges")
        dot.render(self.dir+"/"+fileName,format='gif')
        print("Render successful")
        return self.dir+"/"+fileName+".gif"
    
    def parseEdgeLabel(self,edge):
        localCopy = replaceAST(edge)
        localCopy = localCopy.replace("list","")
        return localCopy

class DViz(tkinter.Frame):
    def __init__(self, host, port):

        super().__init__()

        self.host = host
        self.port = port
        self.con = ConnectionHandler(host,port)
        self.connected = False
        self.gm = GraphMaker("graphs")
        self.images = list()
        self.counter = 0

    def startRacketServer(self):
        os.system('racket tcpAnalisis.rkt &')
        print("Called Racket")
        self.pageStateVar.set("")

    def handShake(self):
        self.con.connect()
        self.connected = True
        self.stateVar.set("State: Connected")
        print(str(self.canvas.winfo_width())+" "+str(self.canvas.winfo_height()))
        self.images = list()
        self.counter = 0
    
    def makeGraph(self):
        codeToSend = self.codeArea.get("1.0","end-1c")
        if len(codeToSend) == 0:
            return
        try:
            a = self.con.sendAndReceive("getGraph\n")
            
            self.simpleGraph = replaceAST(self.con.sendAndReceive(codeToSend).decode())
            print(json.loads(self.simpleGraph))
            try: 
                image, self.originalGraph = self.gm.makeGraph(self.simpleGraph,"graphTest")
                self.images.append(tkinter.PhotoImage(file=image))
                self.canvas.itemconfig(self.image_on_canvas, image=self.images[-1])
            except Exception as e:
                print(e)
                self.stateVar.set("State: Error on server, please restart")
        except Exception:
            self.stateVar.set("State: Error on server, please restart")

    def setOptions(self):
        print(self.execVar.get())
        # Set work mode
        mssg = ""
        if self.execVar.get() == "Worklist":
            mssg = "worklist\n"
        else:
            mssg = "chaotic\n"
        print(self.con.sendAndReceive(mssg).decode())
        # Set analysis
        mssg = ""
        if self.analysisVar.get() == "Live Variables":
            mssg = "liveVar\n"
        else:
            mssg = 'avExpr\n'
        print(self.con.sendAndReceive(mssg).decode())

        varsToSend = self.variablesArea.get("1.0","end-1c")
        if varsToSend != "":
            varsToSend = "("+ varsToSend +")\n"
            print(self.con.sendAndReceive(varsToSend).decode())

        self.stateVar.set("State: Settings Sent!")

    def doAnalysis(self):
        codeToSend = self.codeArea.get("1.0","end-1c")
        if len(codeToSend) == 0:
            return
        print("Going for analysis")
        mssg = "doAnalysis\n"
        response = self.con.sendAndReceive(mssg).decode()
        print(response)
        time.sleep(1)
        self.simpleGraph = self.con.sendAndReceive(codeToSend).decode()
        print(str(self.simpleGraph))
        self.counter = 1
        self.images = [self.images[0]]

    def setPageNumber(self,page):
        self.pageStateVar.set(str(page)+"/"+str(len(self.images)))

    def doStep(self):
        if self.counter < len(self.images):
            self.counter = self.counter + 1
            self.canvas.itemconfig(self.image_on_canvas, image=self.images[self.counter - 1])
            self.setPageNumber(self.counter)
            return
        result = ""
        try:
            result = self.con.sendAndReceive("goAhead\n").decode()
            inhash , outhash = result.split(";")
            image = self.gm.makeEdges(self.originalGraph,inhash,outhash,"newFile"+str(len(self.images)),self.analysisVar.get())

            self.counter = self.counter + 1
            self.images.append(tkinter.PhotoImage(file=image))
            self.setPageNumber(self.counter)
            self.canvas.itemconfig(self.image_on_canvas, image=self.images[self.counter - 1])
        except Exception as e:
            if "RKT: Done" not in result:
                print("Error? "+str(e))
                print("Racket gave : "+str(result))
            else:
                messagebox.showinfo("Analysis", "Analysis is done!")
                self.stateVar.set("State: Please Restart Racket")
            

    def doBack(self):
        if self.counter > 1:
            self.counter = self.counter - 1
            self.setPageNumber(self.counter)
            self.canvas.itemconfig(self.image_on_canvas, image=self.images[self.counter - 1])

    def resetRacket(self):
        if self.connected:
            self.connected = False
            self.stateVar.set("State: Disconnected")
            self.con.closeCon()
        os.system('fuser 9876/tcp')
        time.sleep(2)
        self.startRacketServer()

    def initUI(self):

        self.master.title("Data Visualizer")

        # Settings for app

        Rcktbtn = tkinter.Button(self.master, text="Connect to racket", command=self.handShake)
        Rcktbtn.grid(row = 0, column = 1)

        Rcktbtn = tkinter.Button(self.master, text="Start racket", command=self.resetRacket)
        Rcktbtn.grid(row = 0, column = 2)

        self.execVar = tkinter.StringVar(self.master)
        self.execVar.set("Worklist") # default value

        wExec = tkinter.OptionMenu(self.master, self.execVar, "Chaotic Iteration", "Worklist")
        wExec.grid(row = 0,column = 3)

        self.analysisVar = tkinter.StringVar(self.master)
        self.analysisVar.set("Live Variables") # default value

        wA = tkinter.OptionMenu(self.master, self.analysisVar, "Live Variables", "Available Expressions")
        wA.grid(row = 0,column = 4)

        self.stateVar = tkinter.StringVar()
        self.stateVar.set("State: Disconnected")
        lbl = tkinter.Label(self.master,textvariable=self.stateVar)
        lbl.grid(row = 0, column = 5, pady=4, padx=5)

        # WHILE Code input

        lbl = tkinter.Label(self.master,text="WHILE code input")
        lbl.grid(sticky=tkinter.W, pady=4, padx=5, row=0, column= 0)

        lblvar = tkinter.Label(self.master,text="Live Vars at exit")
        lblvar.grid(sticky=tkinter.W, pady=4, padx=5, row=1, column= 0)

        self.codeArea = tkinter.Text(self.master, width=50, height=20)
        self.codeArea.grid(row=2, column=0, columnspan=2, rowspan=1, padx=5, sticky=tkinter.E+tkinter.W+tkinter.S+tkinter.N)
        self.codeArea.insert(tkinter.END, BASE_STMT)

        self.variablesArea = tkinter.Text(self.master, width=20, height=1)
        self.variablesArea.grid(row = 1, column = 1, sticky=tkinter.E+tkinter.W+tkinter.S+tkinter.N)
        #self.codeArea.insert(tkinter.)

        SendBtn = tkinter.Button(self.master, text="PreProcess Code", command=self.makeGraph)
        SendBtn.grid(row = 4, column = 0)

        setOptionsBtn = tkinter.Button(self.master, text="Set Options", command=self.setOptions)
        setOptionsBtn.grid(row = 4, column = 1)

        DoAnalysisBtn = tkinter.Button(self.master, text="Do Analysis", command=self.doAnalysis)
        DoAnalysisBtn.grid(row = 4, column = 2)
        
        # Scroll bar for text input
        
        sbarText = tkinter.Scrollbar(self.master)
        sbarText.grid(row=2, column=2,sticky=tkinter.N+tkinter.S+tkinter.W)

        sbarText.configure(orient="vertical",command=self.codeArea.yview)
        self.codeArea.configure(yscrollcommand=sbarText.set)

        # Graph image canvas

        lbl = tkinter.Label(self.master,text="Graph Preview")
        lbl.grid(sticky=tkinter.W, pady=4, padx=5, row=1, column=3)

        self.pageStateVar = tkinter.StringVar()
        codePageLabel = tkinter.Label(self.master,textvariable=self.pageStateVar)
        codePageLabel.grid(sticky=tkinter.W, pady=4, padx=5, row=1, column=5)

        PreviousBtn = tkinter.Button(self.master, text="Previous Step", command=self.doBack)
        PreviousBtn.grid(row=4, column=4)

        NextBtn = tkinter.Button(self.master, text="Next Step",command=self.doStep)
        NextBtn.grid(row=4, column=5)

        self.canvas = tkinter.Canvas(self.master)
        self.canvas.grid(row=2, columnspan=4, column= 3)

        # Scroll bar for canvas
    
        sbar = tkinter.Scrollbar(self.master)
        sbar.grid(row=2, column=7,sticky=tkinter.N+tkinter.S+tkinter.W)
        xscrollbar = tkinter.Scrollbar(self.master, orient=tkinter.HORIZONTAL)
        xscrollbar.grid(row=3, column=3, sticky=tkinter.E+tkinter.W, columnspan = 4)

        sbar.configure(orient="vertical",command=self.canvas.yview)
        xscrollbar.configure(orient="horizontal", command=self.canvas.xview)
        self.canvas.configure(yscrollcommand=sbar.set, xscrollcommand=xscrollbar.set)

        # Put sample image on a canvas

        self.cv_img = cv2.cvtColor(cv2.imread("testim.jpg"),cv2.COLOR_BGR2RGB)

        height, width, no_channels = self.cv_img.shape

        self.photo = ImageTk.PhotoImage(image = Image.fromarray(self.cv_img))
        self.image_on_canvas = self.canvas.create_image(0,0,image = self.photo, anchor=tkinter.NW)

    def runApp(self,r):
        # Start
        self.initUI()
        # Run
        r.mainloop()
        # Close sockets
        os.system('fuser 9876/tcp')

if __name__ == "__main__":
    root = tkinter.Tk()
    m = DViz("",9876)
    m.runApp(root)