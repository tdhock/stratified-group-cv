import numpy as np
import pandas as pd
from collections import Counter, defaultdict

def stratified_group_k_fold(y, groups, k=5):
    labels_num = np.max(y) + 1
    y_counts_per_group = defaultdict(lambda: np.zeros(labels_num))
    y_distr = Counter()
    for label, g in zip(y, groups):
        y_counts_per_group[g][label] += 1
        y_distr[label] += 1

    y_counts_per_fold = defaultdict(lambda: np.zeros(labels_num))
    groups_per_fold = defaultdict(set)

    def eval_y_counts_per_fold(y_counts, fold):
        y_counts_per_fold[fold] += y_counts
        std_per_label = []
        for label in range(labels_num):
            label_std = np.std([y_counts_per_fold[i][label] / y_distr[label] for i in range(k)])
            std_per_label.append(label_std)
        y_counts_per_fold[fold] -= y_counts
        return np.mean(std_per_label)
    
    groups_and_y_counts = list(y_counts_per_group.items())
    print(len(y), len(groups_and_y_counts))
    for g, y_counts in sorted(groups_and_y_counts, key=lambda x: -np.std(x[1])):
        best_fold = None
        min_eval = None
        for i in range(k):
            fold_eval = eval_y_counts_per_fold(y_counts, i)
            if min_eval is None or fold_eval < min_eval:
                min_eval = fold_eval
                best_fold = i
        y_counts_per_fold[best_fold] += y_counts
        groups_per_fold[best_fold].add(g)
    return groups_per_fold


train_df = pd.read_csv('train.csv')
train_y = train_df.AdoptionSpeed.values
groups = train_df.RescuerID.values
out = stratified_group_k_fold(train_y, groups)

N = 29000
N = 10000
def getN(arr, N):
    times=int(N / len(arr))+1
    return np.tile(arr, times)[:N]
def getargs(N):
    return (getN(train_y, N), getN(groups, N))

Nargs = getargs(N)
[len(j) for j in Nargs]
out = stratified_group_k_fold(*Nargs)
