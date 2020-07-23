import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.multiclass import OneVsRestClassifier
from sklearn.metrics import f1_score, roc_auc_score

df = pd.read_csv('/Users/olive/github/fantasynba/data/test.csv')
df.drop(['PLAYER', 'POS'], axis=1, inplace=True)

X = df[df.columns[0:10]]
y = df[df.columns[10:18]]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.1, random_state=43)


CATEGORIES = ['PUNT_FG', 'PUNT_TO', 'PUNT_AST', 'PUNT_3PM',
              'PUNT_FT', 'PUNT_BLK', 'PUNT_TREB', 'PUNT_STL']

classifier = OneVsRestClassifier(
    LogisticRegression(solver="sag", n_jobs=1, max_iter=5000))

preds_list = []
labels_list = []

for cat in CATEGORIES:
    print('\n... Processing {}'.format(cat))

    tr_y = y_train[cat]
    ts_y = y_test[cat]

    labels_list.append(ts_y)

    classifier.fit(X_train, tr_y)
    y_pred_X = classifier.predict(X_train)

    test_y = classifier.predict(X_test)
    preds_list.append(test_y)

    print(f'F1 Score: {f1_score(ts_y, test_y):.2f}')
    print(f'Accuracy: {roc_auc_score(ts_y, test_y):.2f}')

print('')
print(f'F1 Micro: {f1_score(labels_list, preds_list, average="micro")}')
print(f'F1 Macro: {f1_score(labels_list, preds_list, average="macro")}')
print(f'ROC AUC: {roc_auc_score(labels_list, preds_list):.2f}')
