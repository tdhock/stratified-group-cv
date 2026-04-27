import numpy as np
import pandas as pd
from sklearn.model_selection import StratifiedGroupKFold
from collections import Counter
sgkf = StratifiedGroupKFold()
train_x = pd.read_csv('train.csv')
train_y = train_x.AdoptionSpeed.values
groups = np.array(train_x.RescuerID.values)

def get_distribution(y_vals):
        y_distr = Counter(y_vals)
        y_vals_sum = sum(y_distr.values())
        return [f'{y_distr[i] / y_vals_sum:.2%}' for i in range(np.max(y_vals) + 1)]

distrs = [get_distribution(train_y)]
index = ['training set']

train_x["fold"] = -1
for fold_ind, (dev_ind, val_ind) in enumerate(sgkf.split(train_x, train_y, groups)):
    train_x.loc[val_ind, "fold"] = fold_ind
    dev_y, val_y = train_y[dev_ind], train_y[val_ind]
    dev_groups, val_groups = groups[dev_ind], groups[val_ind]
    
    assert len(set(dev_groups) & set(val_groups)) == 0
    
    distrs.append(get_distribution(dev_y))
    index.append(f'development set - fold {fold_ind}')
    distrs.append(get_distribution(val_y))
    index.append(f'validation set - fold {fold_ind}')

train_x.to_csv("train_fold_sklearn.csv")

display('Distribution per class:')
pd.DataFrame(distrs, index=index, columns=[f'Label {l}' for l in range(np.max(train_y) + 1)])

