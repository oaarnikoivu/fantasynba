import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.svm import LinearSVC
from sklearn.naive_bayes import MultinomialNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.multiclass import OneVsRestClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import f1_score, roc_auc_score, classification_report

df = pd.read_csv('/Users/olive/github/fantasynba/data/nba_data.csv')
df.drop(['PLAYER', 'POS'], axis=1, inplace=True)

X = df[df.columns[0:10]]
y = df[df.columns[10:18]]

kawhi = [[32.2, 0.47, 0.88, 2.1, 26.9, 7.3, 5.0, 1.8, 0.6, 2.7]]
westbrook = [[35.9, 0.47, 0.78, 1.0, 27.5, 8.1, 7.0, 1.7, 0.3, 4.5]]
lavine = [[34.8, 0.45, 0.80, 3.1, 25.5, 4.8, 4.2, 1.5, 0.5, 3.4]]
kristaps = [[31.3, 0.42, 0.78, 2.5, 19.2, 9.5, 1.7, 0.7, 2.1, 1.6]]
ad = [[34.3, 0.51, 0.84, 1.2, 26.7, 9.4, 3.1, 1.5, 2.4, 2.5]]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.3, random_state=43)

CATEGORIES = ['PUNT_FG', 'PUNT_FT', 'PUNT_3PM', 'PUNT_PTS',
              'PUNT_AST', 'PUNT_STL', 'PUNT_BLK', 'PUNT_TO']

# classifier = OneVsRestClassifier(
#     LogisticRegression(solver="lbfgs", n_jobs=1, max_iter=5000))

# classifier = KNeighborsClassifier(n_neighbors=5)

classifier = MLPClassifier(max_iter=5000)

classifier.fit(X_train, y_train)
predictions = classifier.predict(X_test)
# print(f'{roc_auc_score(y_test, predictions):.2f}')


def do_predict(player):
    predictions = classifier.predict(player)
    for i in range(len(predictions[0])):
        if predictions[0][i] == 1:
            print(f'{CATEGORIES[i]}')


do_predict(kristaps)
