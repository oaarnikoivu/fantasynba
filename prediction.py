import pandas as pd
import numpy as np
import warnings
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import KFold
from sklearn.metrics import f1_score, jaccard_score
from player import Player

warnings.filterwarnings('ignore')

df = pd.read_csv('/Users/olive/github/fantasynba/data/nba_data.csv')
df.drop(['PLAYER', 'POS', ], axis=1, inplace=True)

X = df[df.columns[0:10]]
y = df[df.columns[10:18]]

CATEGORIES = ['PUNT_FG', 'PUNT_FT', 'PUNT_3PM', 'PUNT_PTS',
              'PUNT_AST', 'PUNT_STL', 'PUNT_BLK', 'PUNT_TO']

clf = DecisionTreeClassifier(
    random_state=43, criterion='gini', max_depth=5)

cv = KFold(n_splits=20, random_state=43)

jaccard_scores = []
f1_micro_scores = []

for train_index, test_index in cv.split(X):
    X_train, X_test, y_train, y_test = X.iloc[train_index], X.iloc[
        test_index], y.iloc[train_index], y.iloc[test_index]
    clf.fit(X_train, y_train)
    predictions = clf.predict(X_test)
    jaccard_scores.append(jaccard_score(y_test, predictions, average="weighted",
                                        labels=np.unique(predictions)))
    f1_micro_scores.append(f1_score(y_test, predictions, average='micro'))


print(f'Jaccard: {np.mean(jaccard_scores):.2f}')
print(f'F1 Micro: {np.mean(f1_micro_scores):.2f}')

ad = Player(name="Anthony Davis", mpg=34.3, fg=0.51, ft=0.84, tpm=1.2, pts=26.7,
            treb=9.4, ast=3.1, stl=1.5, blk=2.4, to=2.5)
vucevic = Player(name="Nikola Vucevic", mpg=32.6, fg=0.47, ft=0.78,
                 tpm=1.5, pts=19.5, treb=11.0, ast=3.7, stl=0.9, blk=0.9, to=1.4)
giannis = Player(name="Giannis", mpg=30.9, fg=0.55, ft=0.63,
                 tpm=1.5, pts=29.6, treb=13.7, ast=5.8, stl=1.1, blk=1.0, to=3.7)
zion = Player(name="Zion", mpg=29.7, fg=0.59, ft=0.65,
              tpm=0.3, pts=23.6, treb=6.8, ast=2.2, stl=0.8, blk=0.5, to=2.7)
lebron = Player(name="LeBron James", mpg=34.9, fg=0.50, ft=0.70,
                tpm=2.2, pts=25.7, treb=7.8, ast=10.6, stl=1.2, blk=0.5, to=4.0)
graham = Player(name="Devonte' Graham", mpg=35.1, fg=0.38, ft=0.81,
                tpm=3.5, pts=18.2, treb=3.3, ast=7.5, stl=1.0, blk=0.2, to=2.9)
simmons = Player(name="Ben Simmons", mpg=35.7, fg=0.59, ft=0.62,
                 tpm=0.0, pts=16.7, treb=7.9, ast=8.2, stl=2.1, blk=0.6, to=3.6)
randle = Player(name="Julius Randle", mpg=32.5, fg=0.46, ft=0.73,
                tpm=1.0, pts=19.5, treb=9.7, ast=3.1, stl=0.8, blk=0.3, to=3.0)
sabonis = Player(name="Domantas Sabonis", mpg=34.8, fg=0.54, ft=0.73,
                 tpm=0.3, pts=18.5, treb=12.4, ast=5.0, stl=0.7, blk=0.5, to=2.8)

players = [ad, vucevic, giannis, zion,
           lebron, graham, simmons, randle, sabonis]


def do_predict(player):
    print(f'Predicted categories for {player.name}:')
    predictions = clf.predict(player.get_stats())
    for i in range(len(predictions[0])):
        if predictions[0][i] == 1:
            print(f'{CATEGORIES[i]}')
    print('')


print('')
for player in players:
    do_predict(player)
