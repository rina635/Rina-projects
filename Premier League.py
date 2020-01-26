#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 21 22:47:53 2019

@author: Rina
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 20 18:10:10 2019

@author: Rina
"""

import pandas as pd
import numpy as np

stats = pd.read_csv('/Users/Rina/Downloads/premier-league/stats.csv')

stats.info()
stats.head()

TRC = stats['total_red_card']
Mainseason = stats['season']

manu_stats = stats[stats.team=='Manchester United']
gkp = ['saves', 'punches','season']
manu_gkp = manu_stats[gkp]

import matplotlib.pyplot as plt

plt.plot(manu_gkp.season, manu_gkp.saves, label = 'Saves', marker='o')

x = manu_stats.season
y = manu_stats.saves
plt.bar(x,y)
plt.xticks(rotation=60) #rotates x axis text

Season = manu_stats.season
Punches = manu_stats.punches
plt.bar(Season,Punches)
plt.xticks(rotation=60) #rotates x axis text

plt.plot(Mainseason,TRC)


import seaborn as sns
sns.set(rc={'figure.figsize':(11.7,8.27)})
pd.pivot_table(stats, index = 'team', values = ['total_red_card', 'total_yel_card']).plot(kind= 'bar')

pd.pivot_table(stats, index = 'team', values = 'total_yel_card').plot(kind= 'bar')
#values by itself will give you the AVG of yellow cards

pd.pivot_table(stats, index = 'team', values = 'total_red_card', aggfunc= [np.sum, max])
#by using the agg func we're telling it to sum the total red cards over all seasons
#Chelsea has had the most red cards in all seasons
#Sunderland and Queens Park Rangers have had most red cards in a season

#how many wins and losses manu had in each season
pd.pivot_table(manu_stats, index = 'season', values = ['wins', 'losses'])
pd.pivot_table(manu_stats, index = 'season', values = ['total_red_card', 'total_yel_card'])
pd.pivot_table(manu_stats, index = 'season', values = ['wins', 'total_red_card'])

MURC = manu_stats.total_red_card
MUYC = manu_stats.total_yel_card
MUW = manu_stats.wins
MUSzn = manu_stats.season

import matplotlib.pyplot as plt
plt.bar(MUSzn, MURC)
plt.xticks(rotation=60)

plt.scatter(MUSzn, MUW)

plt.rcParams['figure.figsize'] = [12, 8] #creates a graph of 10x7 inches
plt.plot(MUSzn, MUW, label = 'Wins', marker='o', color = 'green') #plot Wins v Season
plt.plot(MUSzn, MURC, label = 'Red cards', marker='o', color = 'red')
plt.plot(MUSzn, MUYC, label = 'Yellow cards', marker = 'o', color = 'yellow')
plt.xticks(rotation=90)
plt.xlabel('Season')
plt.title('Man United Wins and Total red cards /n Each season')
plt.legend()

Stats_Season = stats['season']
Teams = stats['team']
Wins = stats['wins']
Losses = stats['losses']
pd.pivot_table(stats, index = 'team', values = ['total_red_card'], aggfunc = [np.sum, max])
ARS_stats = stats[stats.team == 'Arsenal']#Team Arsenal stats
CHL_stats = stats[stats.team == 'Chelsea']#Team Chelsea's stats
SUN_stats = stats[stats.team == 'Sunderland'] #Team Sunderland stats
NewCutd_stats = stats[stats.team == 'Newcastle United']# Team Newcastle United stats
ManCity_stats = stats[stats.team == 'Manchester City'] #Team Man city stats

ARS_wins = ARS_stats.wins
ARS_TRC = ARS_stats.total_red_card

plt.plot(MUSzn, ARS_wins, label = 'Arsenal Wins', marker='o', color = 'magenta') #plot Wins v Season
plt.plot(MUSzn, ARS_TRC, label = 'Arsenal Red cards', marker='o', color = 'red')
plt.xticks(rotation=90)
plt.xlabel('Season')
plt.title('Arsenal Total red cards /n Each season')
plt.legend()
