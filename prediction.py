import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn import tree
from sklearn.metrics import f1_score, roc_auc_score, classification_report
from player import Player

df = pd.read_csv('/Users/olive/github/fantasynba/data/nba_data.csv')
df.drop(['PLAYER', 'POS', ], axis=1, inplace=True)

X = df[df.columns[0:10]]
y = df[df.columns[10:18]]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.3, random_state=43)

CATEGORIES = ['PUNT_FG', 'PUNT_FT', 'PUNT_3PM', 'PUNT_PTS',
              'PUNT_AST', 'PUNT_STL', 'PUNT_BLK', 'PUNT_TO']


classifier = tree.DecisionTreeClassifier(random_state=43, max_depth=5)
classifier.fit(X_train, y_train)
predictions = classifier.predict(X_test)

ad = Player(name="Anthony Davis", mpg=34.3, fg=0.51, ft=0.84, tpm=1.2, pts=26.7,
            treb=9.4, ast=3.1, stl=1.5, blk=2.4, to=2.5)
vucevic = Player(name="Nikola Vucevic", mpg=32.6, fg=0.47, ft=0.78,
                 tpm=1.5, pts=19.5, treb=11.0, ast=3.7, stl=0.9, blk=0.9, to=1.4)
giannis = Player(name="Giannis", mpg=30.9, fg=0.55, ft=0.63,
                 tpm=1.5, pts=29.6, treb=13.7, ast=5.8, stl=1.1, blk=1.0, to=3.7)
zion = Player(name="Zion", mpg=29.7, fg=0.59, ft=0.65,
              tpm=0.3, pts=23.6, treb=6.8, ast=2.2, stl=0.8, blk=0.5, to=2.7)


def do_predict(player):
    print(f'Predicted categories for {player.name}:')
    predictions = classifier.predict(player.get_stats())
    for i in range(len(predictions[0])):
        if predictions[0][i] == 1:
            print(f'{CATEGORIES[i]}')
    print('')


do_predict(ad)
do_predict(vucevic)
do_predict(giannis)
do_predict(zion)

tree.plot_tree(classifier)
plt.show()
