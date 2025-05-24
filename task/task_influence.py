# -*- coding: utf-8 -*-
"""
Created on Sun Feb  5 12:26:56 2023

@author: Admin
"""
import os
from psychopy import core, visual, event, gui
import csv, random, time
import winsound
import serial
import os.path
import tkinter as tk
import urllib.request as urllib2
import json
from threading import Thread
import numpy as np


root = tk.Tk()

def scan():
    available = []
    for i in range(256) : 
        try : 
            s= serial.Serial("COM"+str(i))
            available.append((s.portstr))
            s.close()
        except serial.SerialException :
            pass
    return 

def send_trigger(trigger):
    try : 
         port.write(trigger)
    except:
        pass

# screen_width = root.winfo_screenwidth()
# screen_height = root.winfo_screenheight()
screen_width = 2560
screen_height = 1600

# port = serial.Serial('COM14', baudrate = 115200) #uncomment or comment to do with EEG
speed = 0 # speed = 1 when fast experiment to test / speed = 0 when normal
full_screen = False # to do full_screen or small screen

my_directory ="C:\\Users\\mfbpe\\OneDrive\\Cognitive_Science\\experiments\\2023_cambodia\\code\\task\\"
save_path = "C:\\Users\\mfbpe\\OneDrive\\Cognitive_Science\\experiments\\2023_cambodia\\beh\\"
materials = "C:\\Users\\mfbpe\\OneDrive\\Cognitive_Science\\experiments\\2023_cambodia\\code\\task\\materials\\"

#------- TRIGGER NEW INFLUENCE TASK ----------------------------------------------
# list_duo= [[0,0],[1,1],[2,2],[3,3],[4,4],[5,5],[0,2],[0,3], [0,4],[0,5], [1,2],[1,3], [1,4],[1,5],[2,4],[2,5],[3,4],[3,5]] 
## 0 auth yes / 1 auth No / 2 Group yes / 3 Group No / 4 Individu Yes / 5 Individu No

# 1 to 18
trigger_visu_duo = [b'\x01', b'\x02', b'\x03', b'\x04', b'\x05', b'\x06', b'\x07', b'\x08', b'\x09', b'\x0A', b'\x0B', b'\x0C', b'\x0D', b'\x0E', b'\x0F', b'\x10', b'\x11', b'\x12']


#choice can be 0 - 1 -2 - 3 - 4 - 5 ///0 auth yes / 1 auth No / 2 Group yes / 3 Group No / 4 Individu Yes / 5 Individu No
forced_choice_trigger = [b'\x13',b'\x14',b'\x15',b'\x16',b'\x17',b'\x18'] #19 to 24
free_choice_trigger = [b'\x19',b'\x1A',b'\x1B',b'\x1C',b'\x1D',b'\x1E'] #25 to 30

#--- trigger pause unpause -------
pause_off = b'\xFD'#253
pause_on = b'\xFE'#254

okfile = 0
while okfile != 1 : 
    exp_info = {"1-Participant":"","2-Age":"", "3-Gender":""} # open a box and ask information partcipant and age and store in a dictionnary {}
    dlg = gui.DlgFromDict(dictionary = exp_info)
    if dlg.OK == False:
        core.quit()


    dataname = [save_path+"Social_influence_Participant_"+exp_info["1-Participant"]+".csv"]
    if speed == 0 :
        if os.path.isfile(dataname[0]):
            myDlg = gui.Dlg(title="The file already exist")
            myDlg.addField("Do you want to erase the existing file?", choices=["Yes","No"])
            Exp_asw = myDlg.show() ; # show dialog and wait for OK or Cancel
            if myDlg.OK ==False:  # or if ok_data is not None
                core.quit()
            
            if Exp_asw == ["Yes"] : # show dialog and wait for OK or Cancel
                okfile = 1
            elif Exp_asw == ["No"]: 
                okfile = 0
        else:
            okfile = 1
    else:
        okfile = 1
        
############################################################################################
######################## NEW INFLUENCE TASK ########################################
############################################################################################

dataname = [save_path+"New_Influence_P"+exp_info["1-Participant"]+".csv"]


#-------- Création du CSV --------------------------------------------------------------------------

datafile=open(dataname[0],"w", newline="")
writer=csv.writer(datafile, delimiter=";");
writer.writerow(["Participant", "Age", "Gender", "Scenario", "Trial", "Scenario", "Duo_presented", "Forced_free", "choice_individu", "RT"])



################################################
###        Setting the variables     ###########
################################################

win = visual.Window([screen_width,screen_height], color=[-1,-1,-1], fullscr=full_screen) #create a windows of 800 800 if fullscr off / individu -1 black 1 white
stim = visual.TextStim(win, wrapWidth=(1.8), height=.15, color='white')


instruction_influence = visual.ImageStim(win,units='norm',size=(2,1.6), image = materials+"Instruction_New_SocialInfluence.jpg")
call_assistant = visual.ImageStim(win,units='norm', size=(1.6,.5), image = materials+"Call_Assistant.jpg")

call_assistant.draw()
win.flip()

image_scenarios = [visual.ImageStim(win,units='norm',size=(1.7,1.7), image = materials+"IS_Scenario"+str(imagi)+".jpg") for imagi in range(1,4)]
image_stories = [visual.ImageStim(win,units='norm',pos=(0,-.5),size=(2,.9), image = materials+"Story_"+imagi+"_IS.jpg") for imagi in ["Authority","Group","Individu"]]
text_yes = [visual.ImageStim(win,units='norm',pos = (0,.2),size=.5, image = materials+imagi+".jpg") for imagi in ['Yes_Left', 'Yes_Right']]
text_no = [visual.ImageStim(win,units='norm',pos = (0,.2),size=.5, image = materials+imagi+".jpg") for imagi in ['No_Left', 'No_Right']]
character = [visual.ImageStim(win,units='norm',pos = (0,.2), image = materials+imagi+".jpg") for imagi in ['Authority', 'Group', 'Individu']]
individuals = [visual.ImageStim(win,units='norm',pos = (0,.2), image = materials+"I"+str(imagi)+".jpg") for imagi in range(1,6)]

n_training = 12

pos_answer = [(0,.2), (0,-.2)] #mapping of the answer yes no for the participant
pos_character = [-.55,.55] #position for the authority and group
pos_yes_no = [(-.4,-.75),(.4,-.75)] # position for the answer yes/no
pos_answer_key = ['w','x']
answ_keys = pos_answer_key


#--------- creating pseudo_random (e.g trials and blocks) ------------------------------------

list_pairs = [] #different pairs of individuals. 0 = Victim / 1 = Perpetrator / 2 = Neutral
role_IPT = ['Victim','Perpetrator','Neutral']

fixation_cross = np.random.normal(1.65, .1, 1000)
fixation_cross = [ i for i in fixation_cross if i>1.5 and i<1.8]

list_pairs = [] #different pairs of group and authority saying yes or no. left authority, right group, 0 no, 1 yes. e.g [0,1] authority no and group yes

list_pairs_raw = [[0,0],[1,1],[2,2],[3,3],[4,4],[5,5],[0,2],[0,3], [0,4],[0,5], [1,2],[1,3], [1,4],[1,5],[2,4],[2,5],[3,4],[3,5]] 
list_duo= [[0,0],[1,1],[2,2],[3,3],[4,4],[5,5],[0,2],[0,3], [0,4],[0,5], [1,2],[1,3], [1,4],[1,5],[2,4],[2,5],[3,4],[3,5]] 
## 0 auth yes / 1 auth No / 2 Group yes / 3 Group No / 4 Individu Yes / 5 Individu No
i=0

while i != 25 :

    random.shuffle(list_pairs_raw)
    
    if i>0:
        if list_pairs[-1] != list_pairs_raw[0]: # to avoid having twice the same pairs
            list_pairs.extend(list_pairs_raw)
            i+=1
    else:
        list_pairs.extend(list_pairs_raw)
        i+=1


rdm_scenarios=[ a for i in [random.sample(range(3),3) for k in range(20)] for a in i] # to generate x random scenario (0,1,2)

answ_keys = ["w","x"]

n_trial_per_scenario = 30

#----- to do a speed and normal version of the experiment -------------------------
if speed == 1 :
    list_pairs = list_pairs[0:4]
    n_trial_per_scenario = 2
    n_training = 1

event.waitKeys(keyList = "return")


#------ present the instruction text + audio ----------------------
instruction_influence.draw()
win.flip()
event.waitKeys(keyList="return")

rdm_pos_stories = random.sample(range(3),3)
for indi in rdm_pos_stories:
    if indi == 0 or indi == 2:
        character[indi].setSize((.3,1))
    else :
        character[indi].setSize((.92,1.5))

    character[indi].setPos((0,.5))
    character[indi].draw()
    image_stories[indi].draw()
    win.flip()
    event.waitKeys()



#------ message training ----------------------

image_training = visual.ImageStim(win,units='norm',size=(1,.5), image = materials+"Entrainement.jpg")
image_training.draw()
win.flip()
event.waitKeys(keyList = "return")

#-----------------------------------------------
#----- training  -------------------------------
#-----------------------------------------------

trial_training = 0
trial_scenario = 0 #to record iteration 
show_scenario = 0 #to show scenario when ==1 and change scenario when achieving x trials 
rdm_scenario_training = random.sample(range(3),3)

for basi in range(6):

    triali = list_pairs[random.sample(range(len(list_pairs)),1)[0]]
    
    if show_scenario == 2 : #to reset show_scenario when achieving x trials 
        show_scenario=0

    #------      display the scenario and stories   ---------------      
    if show_scenario==0:

        image_scenarios[rdm_scenario_training[trial_scenario]].draw()
        win.flip()
        # playsound('IS_scenario'+str(rdm_scenario_training[trial_scenario]+1)+'.wav')
        winsound.PlaySound(materials+'IS_scenario'+str(rdm_scenario_training[trial_scenario]+1)+'.wav', winsound.SND_FILENAME)

        event.waitKeys()
        trial_scenario+=1


        #------    display the Fixation Cross  ----------------

        stim.setHeight(.15)
        stim.setText("+")
        stim.setPos((0,0))
        stim.draw()
        win.flip()
        time.sleep(fixation_cross[trial_training])

    pos_rdm = random.sample(range(2),2) #to randomly display individuals left and right

    if triali[0] == 0 :
        character[0].setSize((.3,1))
        character[0].setPos((pos_character[pos_rdm[0]],.2))
        character[0].draw()
        text_yes[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_yes[pos_rdm[0]].draw()
        authority_answer = 1
    
    if  triali[0] == 1 :
        character[0].setSize((.3,1))
        character[0].setPos((pos_character[pos_rdm[0]],.2))
        character[0].draw()
        text_no[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_no[pos_rdm[0]].draw()
        authority_answer = 0
    
    if triali[0] == 2 :
        character[1].setSize((.92,1.5))
        character[1].setPos((pos_character[pos_rdm[0]],.2))
        character[1].draw()
        text_yes[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_yes[pos_rdm[0]].draw()
        group_answer = 1
    
    if  triali[0] == 3 :
        character[1].setSize((.92,1.5))
        character[1].setPos((pos_character[pos_rdm[0]],.2))
        character[1].draw()
        text_no[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_no[pos_rdm[0]].draw()
        group_answer = 0
        
    if triali[0] == 4 :
        character[2] = individuals[random.sample(range(5),1)[0]]
        character[2].setSize((.3,1))
        character[2].setPos((pos_character[pos_rdm[0]],.2))
        character[2].draw()
        text_yes[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_yes[pos_rdm[0]].draw()
        individu_answer = 1
    
    if  triali[0] == 5 :
        character[2] = individuals[random.sample(range(5),1)[0]]
        character[2].setSize((.3,1))
        character[2].setPos((pos_character[pos_rdm[0]],.2))
        character[2].draw()
        text_no[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_no[pos_rdm[0]].draw()
        individu_answer = 0
        
    if  triali[1] == 0:
        character[0].setSize((.3,1))
        character[0].setPos((pos_character[pos_rdm[1]],.2))
        character[0].draw()
        text_no[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_no[pos_rdm[1]].draw()
        authority_answer = 1
        
    if  triali[1] == 1 :
        character[0].setSize((.3,1))
        character[0].setPos((pos_character[pos_rdm[1]],.2))
        character[0].draw()
        text_no[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_no[pos_rdm[1]].draw()
        authority_answer = 0
    
    if triali[1] == 2 :
        character[1].setSize((.92,1.5))
        character[1].setPos((pos_character[pos_rdm[1]],.2))
        character[1].draw()
        text_yes[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_yes[pos_rdm[1]].draw()
        group_answer = 1
    
    if  triali[1] == 3 :
        character[1].setSize((.92,1.5))
        character[1].setPos((pos_character[pos_rdm[1]],.2))
        character[1].draw()
        text_no[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_no[pos_rdm[1]].draw()
        group_answer = 0
        
    if triali[1] == 4 :
        character[2] = individuals[random.sample(range(5),1)[0]]
        character[2].setSize((.3,1))
        character[2].setPos((pos_character[pos_rdm[1]],.2))
        character[2].draw()
        text_yes[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_yes[pos_rdm[1]].draw()
        individu_answer = 1
    
    if  triali[1] == 5 :
        character[2] = individuals[random.sample(range(5),1)[0]]
        character[2].setSize((.3,1))
        character[2].setPos((pos_character[pos_rdm[1]],.2))
        character[2].draw()
        text_no[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_no[pos_rdm[1]].draw()
        individu_answer = 0

    win.flip()
    #------- record answer of the participant and get the RT --------------------
    start = time.time()
    keys = event.waitKeys(keyList = answ_keys)
    end=time.time()

    #------    display the Fixation Cross  ----------------

    stim.setHeight(.15)
    stim.setText("+")
    stim.setPos((0,0))
    stim.draw()
    win.flip()
    time.sleep(fixation_cross[trial_training])

    show_scenario+=1
    trial_training+=1

#------------------------------------------------------------------------------------------------------------------------------------------
                                            #STARTING OF THE TASK
#------------------------------------------------------------------------------------------------------------------------------------------
trial = 0
trial_scenario = 0 #to record iteration 
show_scenario = 0#to show scenario when ==1 and change scenario when achieving x trials 


#------ message start experiment ----------------------

image_start = visual.ImageStim(win,units='norm',size=(1,.5), image = materials+"Debut_Experience.jpg")
image_start.draw()
win.flip()
event.waitKeys(keyList = "return")

#----- to start the recording ------------------
send_trigger(pause_off)


for triali in list_pairs:
    
    if triali in list_duo[0:6]:  #meaning the trial is a forced choice trial 
        forced_free = 0 # 0 for forced / 1 for free
    else : 
        forced_free = 1

    if show_scenario == n_trial_per_scenario : #to reset show_scenario when achieving x trials 
        show_scenario=0


    triali = list_pairs[random.sample(range(len(list_pairs)),1)[0]]

    #------      display the scenario and stories   ---------------      
    if show_scenario==0:

        image_scenarios[rdm_scenario_training[trial_scenario]].draw()
        win.flip()
        # playsound('IS_scenario'+str(rdm_scenario_training[trial_scenario]+1)+'.wav')
        winsound.PlaySound(materials+'IS_scenario'+str(rdm_scenario_training[trial_scenario]+1)+'.wav', winsound.SND_FILENAME)

        event.waitKeys()
        trial_scenario+=1


        #------    display the Fixation Cross  ----------------

        stim.setHeight(.15)
        stim.setText("+")
        stim.setPos((0,0))
        stim.draw()
        win.flip()
        time.sleep(fixation_cross[trial_training])

    pos_rdm = random.sample(range(2),2) #to randomly display individuals left and right



    if triali[0] == 0 :
        character[0].setSize((.3,1))
        character[0].setPos((pos_character[pos_rdm[0]],.2))
        character[0].draw()
        text_yes[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_yes[pos_rdm[0]].draw()
        authority_answer = 1
    
    if  triali[0] == 1 :
        character[0].setSize((.3,1))
        character[0].setPos((pos_character[pos_rdm[0]],.2))
        character[0].draw()
        text_no[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_no[pos_rdm[0]].draw()
        authority_answer = 0
    
    if triali[0] == 2 :
        character[1].setSize((.92,1.5))
        character[1].setPos((pos_character[pos_rdm[0]],.2))
        character[1].draw()
        text_yes[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_yes[pos_rdm[0]].draw()
        group_answer = 1
    
    if  triali[0] == 3 :
        character[1].setSize((.92,1.5))
        character[1].setPos((pos_character[pos_rdm[0]],.2))
        character[1].draw()
        text_no[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_no[pos_rdm[0]].draw()
        group_answer = 0
        
    if triali[0] == 4 :
        character[2] = individuals[random.sample(range(5),1)[0]]
        character[2].setSize((.3,1))
        character[2].setPos((pos_character[pos_rdm[0]],.2))
        character[2].draw()
        text_yes[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_yes[pos_rdm[0]].draw()
        individu_answer = 1
    
    if  triali[0] == 5 :
        character[2] = individuals[random.sample(range(5),1)[0]]
        character[2].setSize((.3,1))
        character[2].setPos((pos_character[pos_rdm[0]],.2))
        character[2].draw()
        text_no[pos_rdm[0]].setPos(pos_yes_no[pos_rdm[0]])
        text_no[pos_rdm[0]].draw()
        individu_answer = 0
        
    if  triali[1] == 0:
        character[0].setSize((.3,1))
        character[0].setPos((pos_character[pos_rdm[1]],.2))
        character[0].draw()
        text_no[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_no[pos_rdm[1]].draw()
        authority_answer = 1
        
    if  triali[1] == 1 :
        character[0].setSize((.3,1))
        character[0].setPos((pos_character[pos_rdm[1]],.2))
        character[0].draw()
        text_no[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_no[pos_rdm[1]].draw()
        authority_answer = 0
    
    if triali[1] == 2 :
        character[1].setSize((.92,1.5))
        character[1].setPos((pos_character[pos_rdm[1]],.2))
        character[1].draw()
        text_yes[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_yes[pos_rdm[1]].draw()
        group_answer = 1
    
    if  triali[1] == 3 :
        character[1].setSize((.92,1.5))
        character[1].setPos((pos_character[pos_rdm[1]],.2))
        character[1].draw()
        text_no[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_no[pos_rdm[1]].draw()
        group_answer = 0
        
    if triali[1] == 4 :
        character[2] = individuals[random.sample(range(5),1)[0]]
        character[2].setSize((.3,1))
        character[2].setPos((pos_character[pos_rdm[1]],.2))
        character[2].draw()
        text_yes[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_yes[pos_rdm[1]].draw()
        individu_answer = 1
    
    if  triali[1] == 5 :
        character[2] = individuals[random.sample(range(5),1)[0]]
        character[2].setSize((.3,1))
        character[2].setPos((pos_character[pos_rdm[1]],.2))
        character[2].draw()
        text_no[pos_rdm[1]].setPos(pos_yes_no[pos_rdm[1]])
        text_no[pos_rdm[1]].draw()
        individu_answer = 0

    win.flip()
    duo_presented = list_duo.index(triali)
    send_trigger(trigger_visu_duo[duo_presented])
        

    start = time.time()
    keys = event.waitKeys(keyList = answ_keys)
    end=time.time()
    RT = round(end-start,3)

    
    
    ################################################
    ###           AFTER RESPONSE         ###########
    ################################################

    #------- record the answer of the participant using pos_rdm because we switch the position of the duo. For instance the pair 0-1 (individu 1 and 2) can be displayed 0-1 or 1-0

    if keys[0]== answ_keys[0]:
        prt_answer = triali[pos_rdm[0]]
    elif keys[0]== answ_keys[1]:
        prt_answer = triali[pos_rdm[1]]

    #------ send the trigger choice depending on the condition and answer

    if forced_free ==0:  #meaning forced choice condition
        send_trigger(forced_choice_trigger[prt_answer])
    elif forced_free ==  1: #meaning free choice condition
        send_trigger(free_choice_trigger[prt_answer])

    writer.writerow([exp_info["1-Participant"], exp_info["2-Age"],exp_info["3-Gender"], rdm_scenarios[trial_scenario-1], trial, duo_presented, forced_free, prt_answer, RT])


    ################################################
    ###      display the Fixation Cross  ###########
    ################################################

    stim.setHeight(.15)
    stim.setText("+")
    stim.setPos((0,0))
    stim.draw()
    win.flip()
    time.sleep(fixation_cross[trial])
    trial+=1
    show_scenario+=1



#----- to pause the recording ------------------
send_trigger(pause_on)


win.close()
datafile.close()