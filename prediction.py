import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.svm import LinearSVC
from sklearn.multiclass import OneVsRestClassifier
from sklearn.metrics import f1_score, roc_auc_score, classification_report

df = pd.read_csv('/Users/olive/github/fantasynba/data/test.csv')

df.drop(['PLAYER', 'POS'], axis=1, inplace=True)

X = df[df.columns[0:10]]
y = df[df.columns[10:18]]

ben_simmons = [[35.7, 0.59, 0.62, 0, 15.7, 7.9, 8.2, 2.1, 0.6, 3.6]]
giannis = [[33.0, 0.53, 0.58, 0.1, 17.7, 15.2, 2.7, 1.9, 1.6, 3.6]]
drummond = [[33.0, 0.53, 0.58, 0.1, 17.7, 15.2, 2.7, 1.9, 1.6, 3.6]]
john_collins = [[33.2, 0.58, 0.78, 1.4, 21.6, 10.1, 1.5, 0.8, 1.6, 1.8]]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=43)

CATEGORIES = ['PUNT_FG', 'PUNT_TO', 'PUNT_AST', 'PUNT_3PM',
              'PUNT_FT', 'PUNT_BLK', 'PUNT_TREB', 'PUNT_STL']

classifier = OneVsRestClassifier(
    LogisticRegression(solver="lbfgs", n_jobs=1, max_iter=5000))

classifier.fit(X_train, y_train)
predictions = classifier.predict(X_test)
print(f'{roc_auc_score(y_test, predictions):.2f}')


def do_predict(player):
    predictions = classifier.predict(player)
    for i in range(len(predictions[0])):
        if predictions[0][i] == 1:
            print(f'{CATEGORIES[i]}')


do_predict(john_collins)
